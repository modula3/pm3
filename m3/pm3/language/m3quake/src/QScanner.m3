(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Feb 21 11:20:03 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

MODULE QScanner;

IMPORT File, OSError;
IMPORT M3ID, M3File, QToken;
(** IMPORT Stdio, Wr, Fmt; **)

TYPE
  TK = QToken.T;

REVEAL
  T = T_ BRANDED OBJECT
    char    : CHAR;
    curLine : INTEGER := 0;
    cursor  : INTEGER := 0;
    length  : INTEGER := 0;
    buffer  : REF ARRAY OF CHAR := NIL;
  OVERRIDES
    init := Init;
    next := Next;
  END;

CONST
  EOFChar = '\000';

VAR
  init_done    := FALSE;
  min_keyword  : M3ID.T;
  max_keyword  : M3ID.T;
  keywords     : ARRAY [0..31] OF TK;
  AlphaNumeric := ARRAY CHAR OF BOOLEAN { FALSE, .. };

PROCEDURE Init (t: T;  f: File.T): T =
  VAR status: File.Status;
  BEGIN
    IF NOT init_done THEN InitTables () END;

    (* slurp the source into memory *)
    TRY
      status := f.status ();
      t.buffer := NEW (REF ARRAY OF CHAR, MAX (0, status.size) + 1);
      t.length := M3File.Read (f, t.buffer^, status.size);
      IF (t.length # status.size) THEN RETURN NIL; END;
      t.buffer [t.length] := EOFChar;
    EXCEPT OSError.E => RETURN NIL;
    END;

    t.token   := TK.Unknown;
    t.line    := 0;
    t.char    := ' ';
    t.curLine := 1;
    t.cursor  := 0;
    t.length  := NUMBER (t.buffer^);
    RETURN t;
  END Init;

PROCEDURE InitTables () =
  VAR id: M3ID.T;  ids: ARRAY [TK.And .. TK.Return] OF M3ID.T;
  BEGIN
    WhiteSpace [' ']  := TRUE;
    WhiteSpace ['\n'] := TRUE;
    WhiteSpace ['\t'] := TRUE;
    WhiteSpace ['\r'] := TRUE;
    WhiteSpace ['\f'] := TRUE;

    AlphaNumeric ['_'] := TRUE;
    FOR c := 'a' TO 'z' DO AlphaNumeric [c] := TRUE END;
    FOR c := 'A' TO 'Z' DO AlphaNumeric [c] := TRUE END;
    FOR c := '0' TO '9' DO AlphaNumeric [c] := TRUE END;

    (* build the keyword map *)
    min_keyword := LAST (M3ID.T);
    max_keyword := FIRST (M3ID.T);
    FOR tk := TK.And TO TK.Return DO
      id := M3ID.Add (QToken.Name [tk]);
      ids [tk] := id;
      min_keyword := MIN (min_keyword, id);
      max_keyword := MAX (min_keyword, id);
    END;
    <*ASSERT max_keyword - min_keyword < NUMBER (keywords)*>
    FOR i := FIRST (keywords) TO LAST (keywords) DO keywords[i] := TK.Name END;
    FOR tk := TK.And TO TK.Return DO
      keywords [ids[tk] - min_keyword] := tk;
    END;

    init_done := TRUE;
  END InitTables;

(**
PROCEDURE Next (t: T) =
  <*FATAL ANY*>
  BEGIN
    NextXX (t);
    Wr.PutText (Stdio.stdout, "tok ");
    Wr.PutText (Stdio.stdout, Fmt.Int (t.line));
    Wr.PutText (Stdio.stdout, " => ");
    Wr.PutText (Stdio.stdout, QToken.Name [t.token]);
    Wr.PutText (Stdio.stdout, "\n");
  END Next;
**)

PROCEDURE Next (t: T) =
  BEGIN
    LOOP
      (* skip white space *)
      WHILE WhiteSpace [t.char] DO NextChar (t) END;

      (* remember where this token starts *)
      t.line := t.curLine;

      CASE t.char OF
      | '%' => (* Single-line comment *)
          NextChar (t);
          WHILE (t.char # '\n') AND (t.char # EOFChar) DO NextChar (t); END;

      | '/' => (* C-style comment *)
          NextChar (t);
          IF (t.char # '*') THEN t.token := TK.Unknown; RETURN; END;
          NextChar (t);
          SkipComment (t);

      | '"'                         => ReadString (t);    RETURN;
      | '0' .. '9'                  => ReadCardinal (t);  RETURN;
      | 'a' .. 'z', 'A' .. 'Z', '_' => ReadName (t);      RETURN;

        (* It's punctuation *)
      | '$'     => t.token := TK.Dollar;     NextChar (t);  RETURN;
      | '&'     => t.token := TK.Ampersand;  NextChar (t);  RETURN;
      | '('     => t.token := TK.LParen;     NextChar (t);  RETURN;
      | ')'     => t.token := TK.RParen;     NextChar (t);  RETURN;
      | '+'     => t.token := TK.Plus;       NextChar (t);  RETURN;
      | ','     => t.token := TK.Comma;      NextChar (t);  RETURN;
      | ':'     => t.token := TK.Colon;      NextChar (t);  RETURN;
      | '<'     => t.token := TK.Less;       NextChar (t);  RETURN;
      | '='     => t.token := TK.Equal;      NextChar (t);  RETURN;
      | '>'     => t.token := TK.Greater;    NextChar (t);  RETURN;
      | '@'     => t.token := TK.At;         NextChar (t);  RETURN;
      | '['     => t.token := TK.LSquare;    NextChar (t);  RETURN;
      | ']'     => t.token := TK.RSquare;    NextChar (t);  RETURN;
      | '{'     => t.token := TK.LBrace;     NextChar (t);  RETURN;
      | '}'     => t.token := TK.RBrace;     NextChar (t);  RETURN;
      | EOFChar => t.token := TK.EOF;                       RETURN;

      ELSE  t.token := TK.Unknown; RETURN;
      END;
    END;
  END Next;

PROCEDURE NextChar (t: T) =
  BEGIN
    IF (t.cursor <= t.length) THEN
      t.char := t.buffer [t.cursor];
      INC (t.cursor);
      IF (t.char = '\n') THEN INC (t.curLine) END;
    ELSE
      t.char := EOFChar;
    END;
  END NextChar;

PROCEDURE SkipComment (t: T) =
  VAR c0 := ' ';  c1 := t.char;
  BEGIN
    WHILE (c1 # EOFChar) AND ((c1 # '/') OR (c0 # '*')) DO
      c0 := c1;
      NextChar (t);
      c1 := t.char;
    END;
    NextChar (t);
  END SkipComment;

PROCEDURE ReadCardinal (t: T) =
  VAR i: CARDINAL := 0;
  BEGIN
    WHILE ('0' <= t.char) AND (t.char <= '9') DO
      i := i * 10 + ORD(t.char) - ORD('0');
      NextChar(t);
    END;
    t.cardinal := i;
    t.token    := TK.Cardinal;
  END ReadCardinal;

PROCEDURE ReadString (t: T) =
  VAR start, next: INTEGER;
  BEGIN
    start := t.cursor; (* first character after quote *)
    next  := t.cursor;
    NextChar (t);
    LOOP
      CASE t.char OF
      | EOFChar => EXIT;
      | '"'     => NextChar (t); EXIT;
      | '\\'    =>
        NextChar (t);
        CASE t.char OF
        | '\n' => (* ignore quoted new-line in strings *)
        | '\\' => t.buffer[next] := '\\';  INC (next);     
        | 'n'  => t.buffer[next] := '\n';  INC (next);
        | 'r'  => t.buffer[next] := '\r';  INC (next);
        | 't'  => t.buffer[next] := '\t';  INC (next);
        | 'f'  => t.buffer[next] := '\f';  INC (next);
        | '"'  => t.buffer[next] := '"';   INC (next);
        | EOFChar => EXIT;
        ELSE t.buffer[next] := t.char;  INC (next);
        END;
        NextChar (t);
      ELSE
        t.buffer[next] := t.char;  INC (next);
        NextChar (t);
      END;
    END;

    t.string := M3ID.FromStr (SUBARRAY (t.buffer^, start,
                                        NUMBER(t.buffer^)-start),
                               next - start);
    t.token  := TK.String;
  END ReadString;

PROCEDURE ReadName (t: T) =
  VAR start := t.cursor;  id: M3ID.T;
  BEGIN
    WHILE AlphaNumeric [t.char] DO NextChar (t); END;
    id := M3ID.FromStr (SUBARRAY (t.buffer^, start-1, t.cursor-start));
    t.string := id;
    t.token  := TK.Name;
    IF (min_keyword <= id) AND (id <= max_keyword) THEN
      t.token := keywords [id - min_keyword];
    END;
  END ReadName;

BEGIN
END QScanner.
