(* Copyright (C) 1992, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)

(* File: Scanner.m3                                              *)
(* Last modified on Tue Jan 31 08:12:36 PST 1995 by kalsow       *)
(*      modified on Sat Mar 16 00:25:08 1991 by muller           *)
(*      modified on Fri Oct 19 10:52:56 1990 by nr@princeton.edu *)

UNSAFE MODULE Scanner;

IMPORT Text, File, OSError, TextIntTbl;
IMPORT M3, M3ID, Error, M3String, Token;
IMPORT Target, TInt, TWord, TFloat, Host, M3Buf;

CONST
  MaxStack  = 40;
  MaxLines  = 100000;
  MaxString = 4095;
  MaxBuffer = 4096;
  UndoPad   = 4;
  EOFChar   = '\000';
  MaxRsrvd  = 250;  (* => only the first few ids may be reserved *)

TYPE
  (* CharSet = SET OF CHAR; *)
  TK = Token.T;
  IDList = UNTRACED REF RECORD id: M3ID.T;  next: IDList END;
  CharMap = ARRAY CHAR OF BOOLEAN;

VAR (* CONST *)
  WhiteSpace    : CharMap := CharMap { FALSE, .. };
  AlphaNumerics : CharMap := CharMap { FALSE, .. };
  Digits        : CharMap := CharMap { FALSE, .. };
  OctalDigits   : CharMap := CharMap { FALSE, .. };
  HexDigits     : CharMap := CharMap { FALSE, .. };
  CommentAlert  : CharMap := CharMap { FALSE, .. };
  missing       : M3ID.T;
  LINE          : M3ID.T;
  NOWARN        : M3ID.T;
  PRAGMA        : M3ID.T;

TYPE
  InputBufferIndex = [-UndoPad .. MaxBuffer+1];
  InputBuffer = REF ARRAY InputBufferIndex OF File.Byte;
  (* Note: to avoid a range check in GetCh (and ScanComment), we don't
     use "MaxBuffer-1" as the upper bound of the input buffer. *)

TYPE
  StringBufferIndex =  [0..MaxString];
  StringBuffer =  ARRAY StringBufferIndex OF CHAR;

TYPE
  FileState = RECORD
    ch      : CHAR;
    offs    : INTEGER;	(* fileno * MaxLines + lineno *)
    input   : File.T;
    buf     : InputBuffer;
    buf_ptr : InputBufferIndex;
    buf_len : INTEGER;
    sym     : Symbol;
    ignore  : IDList;  (* pragmas to ignore *)
    accepted: INTEGER;
    is_main : BOOLEAN;
  END;

TYPE
  FileNames = REF ARRAY OF TEXT;

VAR (* never reset *)
  nFiles      : INTEGER;
  file_map    := NEW (TextIntTbl.Default).init ();
  files       := NEW (FileNames, 200);
  local_files := NEW (FileNames, 200);
  reserved    : ARRAY [0..MaxRsrvd] OF M3.Value;

VAR (* explicitly reset *)
  input       : File.T;
  input_buf   : InputBuffer;
  input_ptr   : InputBufferIndex;
  input_len   : INTEGER;
  ch          : CHAR;
  ignore      : IDList;
  accepted    : INTEGER := 2;
  tos         : INTEGER;
  stack       : ARRAY [0..MaxStack] OF FileState;
  buf         : StringBuffer;
  

PROCEDURE Initialize () =
  BEGIN
    missing    := M3ID.Add ("<missing id>");
    LINE       := M3ID.Add ("LINE");
    NOWARN     := M3ID.Add ("NOWARN");
    PRAGMA     := M3ID.Add ("PRAGMA");

    nFiles     := 0;
    tos        := 0;

    WhiteSpace [' ']  := TRUE;
    WhiteSpace ['\n'] := TRUE;
    WhiteSpace ['\t'] := TRUE;
    WhiteSpace ['\r'] := TRUE;
    WhiteSpace ['\f'] := TRUE;

    AlphaNumerics ['_'] := TRUE;
    FOR c := 'a' TO 'z' DO AlphaNumerics [c] := TRUE END;
    FOR c := 'A' TO 'Z' DO AlphaNumerics [c] := TRUE END;
    FOR c := '0' TO '9' DO AlphaNumerics [c] := TRUE END;

    FOR c := '0' TO '9' DO Digits [c] := TRUE END;

    FOR c := '0' TO '7' DO OctalDigits [c] := TRUE END;

    FOR c := '0' TO '9' DO HexDigits [c] := TRUE END;
    FOR c := 'a' TO 'f' DO HexDigits [c] := TRUE END;
    FOR c := 'A' TO 'F' DO HexDigits [c] := TRUE END;

    CommentAlert ['*'] := TRUE;
    CommentAlert ['('] := TRUE;
    CommentAlert [EOFChar] := TRUE;
    CommentAlert ['\n'] := TRUE;
    CommentAlert ['@'] := TRUE;
  END Initialize;

PROCEDURE Reset () =
  BEGIN
    WHILE (tos > 0) DO
      Pop ();
      Host.CloseFile (input);
    END;
    input_buf  := NIL;
    input_len  := -1; (* not 0 ==> EOF *)
    input_ptr  := 0;
    ch         := ' ';
    ignore     := NIL;
    accepted   := 2;

    (* interface variables *)
    offset     := 0;
    nLines     := 0;
    nPushed    := 0;
    cur.token  := TK.tEOF;
  END Reset;

PROCEDURE Push (name: TEXT;  file: File.T;  is_main: BOOLEAN) =
  BEGIN
    INC (nPushed);
    WITH z = stack[tos] DO
      z.ch      := ch;
      z.offs    := offset;
      z.sym     := cur;
      z.input   := input;
      z.buf     := input_buf;
      z.buf_len := input_len;
      z.buf_ptr := input_ptr;
      z.ignore  := ignore;
      z.accepted:= accepted;
      z.is_main := in_main;
    END;
    INC (tos);
    in_main   := is_main;
    offset    := FileNumber (name) * MaxLines + 1;
    ch        := ' ';
    ignore    := NIL;
    accepted  := 2;
    input     := file;
    input_ptr := 0;
    input_len := -1;  (* not 0 ==> EOF *)
    input_buf := stack[tos].buf;
    IF (input_buf = NIL) THEN
      input_buf := NEW (InputBuffer);
      stack[tos].buf := input_buf;
    END;
    IF (file # NIL) THEN GetToken (); (* prime the input stream *) END;
  END Push;

PROCEDURE Pop () =
  BEGIN
    DEC (tos);
    WITH z = stack[tos] DO
      ch        := z.ch;
      offset    := z.offs;
      cur       := z.sym;
      input     := z.input;
      input_buf := z.buf;
      input_ptr := z.buf_ptr;
      input_len := z.buf_len;
      ignore    := z.ignore;
      accepted  := z.accepted;
      in_main   := z.is_main;
    END;
  END Pop;

PROCEDURE FileNumber (filename: TEXT): INTEGER =
   (* returns index into files of filename, adding it if it doesn't exist *)
  VAR index := offset DIV MaxLines;
  BEGIN
    (* often we'll hit the current file *)
    IF (index < NUMBER (files^))
      AND (files[index] # NIL)
      AND Text.Equal (files[index], filename) THEN
      RETURN index;
    END;

    IF file_map.get (filename, index) THEN RETURN index; END;

    (* add a new one *)
    IF (nFiles >= NUMBER (files^)) THEN ExpandFiles(); END;
    EVAL file_map.put (filename, nFiles);
    files[nFiles] := filename;
    local_files[nFiles] := NIL;
    INC(nFiles);
    RETURN nFiles-1;
  END FileNumber;

PROCEDURE ExpandFiles () =
  VAR
    n := NUMBER (files^);
    new_global := NEW (FileNames, n + n);
    new_local  := NEW (FileNames, n + n);
  BEGIN
    SUBARRAY (new_global^, 0, n) := files^;
    SUBARRAY (new_local^, 0, n)  := local_files^;
    files       := new_global;
    local_files := new_local;
  END ExpandFiles;

PROCEDURE Here (VAR file: TEXT;  VAR line: INTEGER) =
  BEGIN
    file := files [offset DIV MaxLines];
    line := offset MOD MaxLines;
  END Here;

PROCEDURE LocalHere (VAR file: TEXT;  VAR line: INTEGER) =
  VAR fnum := offset DIV MaxLines;
  BEGIN
    IF (local_files[fnum] = NIL) THEN
      local_files[fnum] := Host.FileTail (files[fnum]);
    END;
    file := local_files [fnum];
    line := offset MOD MaxLines;
  END LocalHere;

PROCEDURE SameFile (a, b: INTEGER): BOOLEAN =
  BEGIN
    RETURN (a DIV MaxLines) = (b DIV MaxLines);
  END SameFile;

PROCEDURE Match (t: Token.T) =
  BEGIN
    IF (cur.token = t) THEN
      GetToken ();
    ELSE
      DoFail ("missing \'" & M3ID.ToText (Token.name [t]) & "\'", t);
      IF (cur.token = t) THEN GetToken () END;
    END;
  END Match;

PROCEDURE MatchID (): M3ID.T =
  VAR id: M3ID.T;
  BEGIN
    IF (cur.token = TK.tIDENT) THEN
      id := cur.id;
      GetToken ();
    ELSE
      DoFail ("missing identifier", TK.tIDENT);
      IF (cur.token = TK.tIDENT)
        THEN  id := cur.id;  GetToken ();
        ELSE  id := missing;
      END;
    END;
    RETURN id;
  END MatchID;

PROCEDURE Fail (msg: TEXT) =
  BEGIN
    DoFail (msg, TK.tEOF);
  END Fail;

PROCEDURE DoFail (msg: TEXT;  stop: TK) =
  VAR t: TEXT;  i: INTEGER;
  BEGIN
    IF (accepted > 1) THEN
      t := "syntax error: " & msg;
      CASE cur.token OF
      | TK.tIDENT =>
          Error.ID (cur.id, t);
      | TK.tTEXTCONST =>
          Error.Txt (M3String.ToText (cur.str), t);
      | TK.tREALCONST, TK.tLONGREALCONST, TK.tEXTENDEDCONST =>
          Error.Txt ("<float>", t);
      | TK.tCARDCONST, TK.tCHARCONST =>
          IF TInt.ToInt (cur.int, i)
            THEN Error.Int (i, t);
            ELSE Error.Txt ("<integer>", t);
          END;
      ELSE (* no extra info *)
          Error.Msg (t);
      END;
    END;
    (* skip forward to a major token... *)
    WHILE (cur.token # stop) AND NOT (cur.token IN Restart) DO
      GetToken ();
    END;
    accepted := 0;
    IF (cur.token = stop) THEN accepted := 1; END;
  END DoFail;

CONST
  Restart = Token.Set {
    TK.tEOF, TK.tSEMI, TK.tINLINE, TK.tEXTERNAL, TK.tASSERT,
    TK.tUNUSED, TK.tOBSOLETE, TK.tTRACE, TK.tCALLCONV,
    TK.tFATAL,  TK.tBEGIN, TK.tCASE, TK.tCONST,
    TK.tELSE, TK.tELSIF, TK.tEVAL, TK.tEXCEPT, TK.tEXCEPTION,
    TK.tEXIT, TK.tEXPORTS, TK.tFINALLY, TK.tFOR, TK.tFROM,
    TK.tGENERIC, TK.tIF, TK.tIMPORT, TK.tINTERFACE,
    TK.tLOCK, TK.tLOOP, TK.tMODULE, TK.tPROCEDURE,
    TK.tRAISE, TK.tREADONLY, TK.tREPEAT, TK.tRETURN, TK.tREVEAL,
    TK.tTHEN, TK.tTRY, TK.tTYPE, TK.tTYPECASE, TK.tUNSAFE, TK.tUNTIL,
    TK.tVALUE, TK.tVAR, TK.tWHILE, TK.tWITH };

PROCEDURE NoteReserved (name: M3ID.T;  value: M3.Value) =
  BEGIN
    <* ASSERT M3ID.GetClass (name) = 0 *>
    <* ASSERT reserved[name] = NIL *>
    reserved [name] := value;
  END NoteReserved;

<*INLINE*> PROCEDURE GetCh () =
  <*FATAL OSError.E*>
  BEGIN
    LOOP
      IF (input_ptr < input_len) THEN
        ch := VAL (input_buf[input_ptr], CHAR);
        INC (input_ptr);
        RETURN;
      ELSIF (input_len = 0) THEN
        ch := EOFChar;
        RETURN;
      ELSE
        input_len := input.read (SUBARRAY (input_buf^, UndoPad, MaxBuffer));
        input_ptr := 0;
        input_buf[input_len] := ORD('@'); (* => in CommentAlert *)
        (* loop around and try again *)
      END;
    END;
  END GetCh;

PROCEDURE GetToken () =
  VAR len: StringBufferIndex;
  BEGIN
    INC (accepted);
    LOOP
      (* skip white space *)
      WHILE (WhiteSpace[ch]) DO
        IF (ch = '\n') THEN INC (offset);  INC (nLines)  END;
        GetCh ();
      END;
      (* remember where this token starts *)
      cur.offset := offset;

      CASE ch OF

      | 'a'..'z', 'A'..'Z' =>
          (* scan an identifier *)
          len := 0;
          WHILE (AlphaNumerics[ch]) DO
            buf [len] := ch;  INC (len);
	    GetCh ();
          END;
          cur.id    := M3ID.FromStr (buf, len);
          cur.token := TK.tIDENT;
          cur.defn  := NIL;
          VAR i := M3ID.GetClass (cur.id); BEGIN
            IF (ORD (Token.First_Keyword) <= i)
              AND (i <= ORD (Token.Last_Keyword)) THEN
              cur.token := VAL (i, TK);
            END;
          END;
          IF (cur.id <= LAST(reserved)) THEN
            cur.defn := reserved[cur.id];
          END;
          RETURN;

      | '0'..'9' => ScanNumber ();                             RETURN;
      | '\''     => ScanChar ();                               RETURN;
      | '\"'     => ScanText ();                               RETURN;
      | '+'      => cur.token := TK.tPLUS;       GetCh ();  RETURN;
      | '-'      => cur.token := TK.tMINUS;      GetCh ();  RETURN;
      | '/'      => cur.token := TK.tSLASH;      GetCh ();  RETURN;
      | '&'      => cur.token := TK.tAMPERSAND;  GetCh ();  RETURN;
      | ','      => cur.token := TK.tCOMMA;      GetCh ();  RETURN;
      | ';'      => cur.token := TK.tSEMI;       GetCh ();  RETURN;
      | '['      => cur.token := TK.tLBRACKET;   GetCh ();  RETURN;
      | '{'      => cur.token := TK.tLBRACE;     GetCh ();  RETURN;
      | '^'      => cur.token := TK.tARROW;      GetCh ();  RETURN;
      | '#'      => cur.token := TK.tSHARP;      GetCh ();  RETURN;
      | ')'      => cur.token := TK.tRPAREN;     GetCh ();  RETURN;
      | ']'      => cur.token := TK.tRBRACKET;   GetCh ();  RETURN;
      | '}'      => cur.token := TK.tRBRACE;     GetCh ();  RETURN;
      | '|'      => cur.token := TK.tBAR;        GetCh ();  RETURN;
      | EOFChar  => cur.token := TK.tEOF;                   RETURN;

      | '*' => (* '*>' '*' *)
	    GetCh ();
            IF (ch = '>')
	      THEN  cur.token := TK.tENDPRAGMA;  GetCh ();
              ELSE  cur.token := TK.tASTERISK;
            END;
            RETURN;
      | '=' => (*  '='  '=>'  *)
            GetCh ();
            IF (ch = '>')
	      THEN  cur.token := TK.tIMPLIES;  GetCh ();
              ELSE  cur.token := TK.tEQUAL;
            END;
            RETURN;
      | ':' => (*  ':'  ':='  *)
            GetCh ();
            IF (ch = '=')
	      THEN  cur.token := TK.tASSIGN;  GetCh ();
              ELSE  cur.token := TK.tCOLON;
            END;
            RETURN;
      | '.' => (*  '.'  '..'  *)
            GetCh ();
            IF (ch = '.')
	      THEN  cur.token := TK.tDOTDOT;  GetCh ();
              ELSE  cur.token := TK.tDOT;
            END;
            RETURN;
      | '(' => (*  '('*'  '('  *)
            GetCh ();
            IF (ch = '*')
	      THEN  ScanComment ();
              ELSE  cur.token := TK.tLPAREN;  RETURN;
            END;
      | '>' => (*  '>'  '>='  *)
            GetCh ();
            IF (ch = '=')
	      THEN  cur.token := TK.tGREQUAL;  GetCh ();
              ELSE  cur.token := TK.tGREATER;
            END;
            RETURN;
      | '<' => (*  '<'  '<='  '<:'  '<*' *)
            GetCh ();
            IF    (ch = '=') THEN  cur.token := TK.tLSEQUAL;  GetCh ();
            ELSIF (ch = ':') THEN  cur.token := TK.tSUBTYPE;  GetCh ();
            ELSIF (ch = '*') THEN  ScanPragma ();
            ELSE                   cur.token := TK.tLESS;
            END;
            RETURN;

      ELSE
        Error.Int (ORD (ch), "Illegal character");
        GetCh ();

      END; (*case*)
    END; (*loop*)
  END GetToken;

PROCEDURE ScanNumber () =
  VAR
    base: INTEGER;
    val: Target.Int;
    pre: Target.Precision;
    len: StringBufferIndex;
  BEGIN
    (* scan the decimal digits *)
    len := 0;
    WHILE (Digits[ch]) DO
      buf[len] := ch;  INC (len);
      GetCh ();
    END;

    IF (ch = '_') THEN
      (* scan a based integer *)
      IF    NOT TInt.New (SUBARRAY (buf, 0, len), val)
         OR NOT TInt.ToInt (val, base)
         OR (base < 2)
         OR (16 < base) THEN
        Error.Int (base, "illegal base for based literal, 10 used");
        base := 10;
      END;
      len := 0;
      LOOP
        GetCh ();
        IF NOT (HexDigits[ch]) THEN EXIT END;
        buf [len] := ch;  INC (len);
      END;
      IF (len = 0) OR NOT TWord.New (SUBARRAY (buf, 0, len), base, val) THEN
        Error.Msg ("illegal based integer literal, zero used");
        val := TInt.Zero;
      END;
      cur.token := TK.tCARDCONST;
      cur.int   := val;

    ELSIF (ch = '.') THEN
      (* scan a floating point number *)
      buf[len] := '.';  INC (len);
      GetCh (); (* eat the '.' *)
      IF (ch = '.') THEN
        (* we saw  "dddd.." *)

	(*****  Rd.UnGetChar (input);  *****)
        DEC (input_ptr);  input_buf[input_ptr] := ORD ('.');

        IF NOT TInt.New (SUBARRAY (buf, 0, len-1), val) THEN
          Error.Msg ("illegal integer literal, zero used");
          val := TInt.Zero;
        END;
        cur.token := TK.tCARDCONST;
        cur.int   := val;
        RETURN;
      END;

      (* scan the fractional digits *)
      IF NOT (Digits[ch]) THEN
        Error.Msg ("missing digits in real fraction");
        buf[len] := '0';  INC (len);
      END;
      WHILE (Digits[ch]) DO  buf[len] := ch; INC (len); GetCh ()  END;

      (* check for the exponent *)
      pre := Target.Precision.Short;
      IF (ch = 'e') OR (ch = 'E') THEN
        buf[len] := 'e';  INC (len);
        cur.token := TK.tREALCONST;
        pre := Target.Precision.Short;
      ELSIF (ch = 'd') OR (ch = 'D') THEN
        buf[len] := 'e';  INC (len);
        cur.token := TK.tLONGREALCONST;
        pre := Target.Precision.Long;
      ELSIF (ch = 'x') OR (ch = 'X') THEN
        buf[len] := 'e';  INC (len);
        cur.token := TK.tEXTENDEDCONST;
        pre := Target.Precision.Extended;
      ELSE (* real constant with no exponent *)
        IF NOT TFloat.New (SUBARRAY (buf, 0, len), pre, cur.float) THEN
          Error.Msg ("illegal floating-point literal");
        END;
        cur.token := TK.tREALCONST;
        RETURN;
      END;
      GetCh (); (* eat the exponent entry char *)

      (* get the exponent sign *)
      IF (ch = '+') THEN
        buf[len] := '+';  INC (len);
        GetCh ();
      ELSIF (ch = '-') THEN
        buf[len] := '-';  INC (len);
        GetCh ();
      ELSE
        buf[len] := '+';
      END;

      (* finally, get the exponent digits *)
      IF NOT (Digits[ch]) THEN
        Error.Msg ("missing digits in real exponent");
        buf[len] := '0';  INC (len);
      END;
      WHILE (Digits[ch]) DO  buf[len] := ch; INC (len); GetCh ();  END;

      IF NOT TFloat.New (SUBARRAY (buf, 0, len), pre, cur.float) THEN
        Error.Msg ("illegal floating-point literal");
      END;

    ELSE
      (* already scanned a decimal integer *)
      IF NOT TInt.New (SUBARRAY (buf, 0, len), val) THEN
        Error.Msg ("illegal integer literal, zero used");
        val := TInt.Zero;
      END;
      cur.token := TK.tCARDCONST;
      cur.int   := val;
    END;

  END ScanNumber;

PROCEDURE ScanChar () =
  VAR val := 0;
  BEGIN
    cur.token := TK.tCHARCONST;
    cur.int   := TInt.Zero;
    GetCh ();
    IF (ch = '\'') THEN
      Error.Msg ("missing character in character literal");
      GetCh ();
      RETURN;
    ELSIF (ch = '\n') OR (ch = '\r') OR (ch = '\f') THEN
      Error.Msg ("end-of-line encountered in character literal");
      RETURN;
    ELSIF (ch = '\\') THEN
      GetCh ();
      IF    (ch = 'n')  THEN  val := ORD ('\n');   GetCh ();
      ELSIF (ch = 't')  THEN  val := ORD ('\t');   GetCh ();
      ELSIF (ch = 'r')  THEN  val := ORD ('\r');   GetCh ();
      ELSIF (ch = 'f')  THEN  val := ORD ('\f');   GetCh ();
      ELSIF (ch = '\\') THEN  val := ORD ('\\');   GetCh ();
      ELSIF (ch = '\'') THEN  val := ORD ('\'');   GetCh ();
      ELSIF (ch = '\"') THEN  val := ORD ('\"');   GetCh ();
      ELSIF (OctalDigits[ch]) THEN  val := GetOctalChar ();
      ELSE  Error.Msg ("unknown escape sequence in character literal");
      END;
    ELSIF (ch = EOFChar) THEN
      Error.Msg ("EOF encountered in character literal");
      RETURN ;
    ELSE (* a simple character literal *)
      val := ORD (ch);
      GetCh ();
    END;
    IF (ch # '\'')
      THEN Error.Msg ("missing closing quote on character literal");
      ELSE GetCh ();
    END;
    IF NOT TInt.FromInt (val, cur.int) THEN
      Error.Msg ("illegal character literal");
    END;
  END ScanChar;

PROCEDURE ScanText () =
  VAR i: INTEGER;  mbuf: M3Buf.T := NIL;
  PROCEDURE Stuff (c: CHAR) =
    BEGIN
      IF (i < NUMBER (buf)) THEN
        buf [i] := c;  INC (i);
      ELSIF (i = NUMBER (buf)) THEN
        mbuf := M3Buf.New ();
        M3Buf.PutSub (mbuf, buf);
        M3Buf.PutChar (mbuf, c);
        INC (i);
      ELSE
        M3Buf.PutChar (mbuf, c);
        INC (i);
      END;
    END Stuff;
  BEGIN
    i := 0;
    cur.token := TK.tTEXTCONST;
    GetCh ();
    LOOP
      IF (ch = '\"') THEN
        GetCh ();
        EXIT;
      ELSIF (ch = '\n') OR (ch = '\r') OR (ch = '\f') THEN
        Error.Msg ("end-of-line encountered in text literal");
        EXIT;
      ELSIF (ch = '\\') THEN
        GetCh ();
        IF    (ch = 'n') THEN  Stuff ('\n');  GetCh ();
        ELSIF (ch = 't') THEN  Stuff ('\t');  GetCh ();
        ELSIF (ch = 'r') THEN  Stuff ('\r');  GetCh ();
        ELSIF (ch = 'f') THEN  Stuff ('\f');  GetCh ();
        ELSIF (ch = '\\') THEN Stuff ('\\');  GetCh ();
        ELSIF (ch = '\'') THEN Stuff ('\'');  GetCh ();
        ELSIF (ch = '\"') THEN Stuff ('\"');  GetCh ();
        ELSIF (OctalDigits[ch]) THEN Stuff (VAL (GetOctalChar (), CHAR));
        ELSE  Error.Msg ("unknown escape sequence in text literal");
        END;
      ELSIF (ch = EOFChar) THEN
        Error.Msg ("EOF encountered in text literal");
        EXIT;
      ELSE (* a simple character *)
        Stuff (ch);
        GetCh ();
      END;
    END;

    IF (mbuf = NIL)
      THEN cur.str := M3String.FromStr (buf, i);
      ELSE cur.str := M3String.Add (M3Buf.ToText (mbuf));
    END;
  END ScanText;

PROCEDURE GetOctalChar (): INTEGER =
  VAR value: INTEGER;
  BEGIN
    <* ASSERT OctalDigits[ch] *>
    value := ORD (ch) - ORD ('0');
    GetCh ();
    IF  NOT (OctalDigits[ch]) THEN BadOctal (); RETURN value END;
    value := value * 8 + ORD (ch) - ORD ('0');
    GetCh ();
    IF  NOT (OctalDigits[ch]) THEN BadOctal (); RETURN value END;
    value := value * 8 + ORD (ch) - ORD ('0');
    GetCh ();
    RETURN value;
  END GetOctalChar;

PROCEDURE BadOctal () =
  BEGIN
    Error.Msg ("octal character constant must have 3 digits");
  END BadOctal;

PROCEDURE ScanComment () =
  VAR nest, save: INTEGER; start: INTEGER;
  BEGIN
    start := cur.offset;
    GetCh ();
    nest := 1;
    WHILE (nest > 0) DO
      WHILE (NOT CommentAlert[ch]) DO
        (* INLINE GetCh (); *)
        ch := VAL (input_buf[input_ptr], CHAR);
        INC (input_ptr);
      END;
      IF (ch = '*') THEN
        GetCh ();  IF (ch = ')') THEN DEC (nest); GetCh ();  END;
      ELSIF (ch = '(') THEN
        GetCh ();  IF (ch = '*') THEN INC (nest); GetCh ();  END;
      ELSIF (ch = EOFChar) THEN
        save := offset;
	offset := start;
        Error.Msg ("EOF encountered in comment");
	offset := save;
        nest := 0;
      ELSIF (ch = '\n') THEN
        INC (offset);  INC (nLines);
        GetCh ();
      ELSE
        GetCh ();
      END;
    END;
  END ScanComment;

PROCEDURE ScanPragma () =
  VAR nest, save, start, i, lineno, fileno: INTEGER;  ss: IDList;
  BEGIN
    start := cur.offset;
    GetCh();  (* '*' *)

    (* skip white space *)
    WHILE (WhiteSpace[ch]) DO
      IF (ch = '\n') THEN INC (offset);  INC (nLines);  END;
      GetCh();
    END;

    (* scan an identifier *)
    i := 0;
    WHILE (AlphaNumerics[ch]) DO
      buf [i] := ch;  INC (i);
      GetCh ();
    END;
    cur.id    := M3ID.FromStr (buf, i);
    cur.token := VAL (M3ID.GetClass (cur.id), TK);

    IF (Token.First_Pragma<=cur.token) AND (cur.token<=Token.Last_Pragma) THEN
      RETURN;
    END;

    IF (cur.id = LINE) THEN
      GetToken (); (* LINE *)
      IF (cur.token # TK.tCARDCONST) THEN
        Error.Msg ("missing line number on LINE pragma; skipping to \'*>\'");
        WHILE (cur.token # TK.tENDPRAGMA) AND (cur.token # TK.tEOF) DO
          GetToken ();
        END;
        IF (cur.token = TK.tENDPRAGMA) THEN GetToken () END;
        RETURN;
      END;
      IF NOT TInt.ToInt (cur.int, lineno) THEN
        Error.Msg ("illegal line number, ignored");
        lineno := offset MOD MaxLines;
      END;
      fileno := offset DIV MaxLines;
      GetToken (); (* CARD "line number" *)
      IF (cur.token = TK.tTEXTCONST) THEN
        fileno := FileNumber (M3String.ToText (cur.str));
        GetToken(); (* TEXT "filename" *)
      END;
      offset := fileno * MaxLines + lineno - 1;
      IF (cur.token # TK.tENDPRAGMA)
        THEN Error.Msg ("missing \'*>\' on LINE pragma");
        ELSE GetToken (); (* fetch the next one *)
      END;
      RETURN;
    ELSIF (cur.id = NOWARN) THEN
      Error.IgnoreWarning (cur.offset);
      GetToken ();  (* NOWARN *)
      IF (cur.token # TK.tENDPRAGMA)
        THEN Error.Msg ("missing \'*>\' on NOWARN pragma");
        ELSE GetToken (); (* fetch the next one *)
      END;
      RETURN;
    ELSIF (cur.id = PRAGMA) THEN
      GetToken (); (* PRAGMA *)
      WHILE (cur.token = TK.tIDENT) 
      OR ((Token.First_Pragma<=cur.token) AND (cur.token<=Token.Last_Pragma))
      OR ((Token.First_Keyword<=cur.token) AND (cur.token<=Token.Last_Keyword))
      DO
        ignore := NEW (IDList, id := cur.id, next := ignore);
        GetToken ();  (* IDENT *)
        IF (cur.token # TK.tCOMMA) THEN EXIT END;
        GetToken ();  (* COMMA *)
      END;
      IF (cur.token # TK.tENDPRAGMA)
        THEN Error.Msg ("missing \'*>\' on PRAGMA pragma");
        ELSE GetToken ();  (* fetch the next real token *)
      END;
      RETURN;
    ELSIF Target.FindConvention (M3ID.ToText (cur.id)) # NIL THEN
      cur.token := TK.tCALLCONV;
      RETURN;
    ELSE (* scan and ignore the list *)
      ss := ignore;
      WHILE (ss # NIL) AND (ss.id # cur.id) DO  ss := ss.next  END;
      IF (ss = NIL) THEN
        Error.WarnID (2, cur.id, "unrecognized pragma (ignored)");
      END;
    END;


    (* scan over and ignore the offending pragma *)
    nest := 1;
    WHILE (nest > 0) DO
      IF (ch = '*') THEN
        GetCh();  IF (ch = '>') THEN DEC(nest); GetCh(); END;
      ELSIF (ch = '<') THEN
        GetCh();  IF (ch = '*') THEN INC(nest); GetCh(); END;
      ELSIF (ch = EOFChar) THEN
        save := offset;
	offset := start;
        Error.Msg ("EOF encountered in pragma");
	offset := save;
	nest := 0;
      ELSIF (ch = '\n') THEN
        INC (offset);  INC (nLines);
	GetCh();
      ELSE
        GetCh();
      END;
    END;

    GetToken (); (* get the next token *)
  END ScanPragma;

BEGIN
END Scanner.
