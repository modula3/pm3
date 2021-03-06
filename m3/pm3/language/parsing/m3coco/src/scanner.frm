MODULE -->Scanner EXPORTS -->Grammar ;

(* Scanner generated by m3coco *)

IMPORT ASCII, Text, Rd, Thread ;

CONST
  StartState = ARRAY CHAR OF CARDINAL {
    -->StartTable
  } ;

  IgnoreCase = -->IgnoreCase ;

REVEAL
  Scanner = PublicS BRANDED OBJECT
              err         : ErrHandler ;
              lastCh      : CHAR ;              (* last input char *)
              ch          : CHAR ;              (* current input char *)
              offset      : CARDINAL ;          (* offset into stream *)
              currentLine : CARDINAL ;          (* current input line *)
              currentCol  : CARDINAL ;          (* current input column *)
              lastLineLen : CARDINAL ;          (* length of previous line *)
              apx         : CARDINAL ;          (* length of appendix *)
                                                (* (for CONTEXT) *)
              oldEols     : CARDINAL ;          (* number of EOLs in comment *)
              rd          : Rd.T ;              (* the input stream *)
              buf         : REF ARRAY OF CHAR ; (* the input buffer *)
              bufLen      : CARDINAL            (* length of the input buffer *)
            OVERRIDES
              init       := Init ;
              get        := Get
            END ;

PROCEDURE NextCh(s : Scanner ; record := FALSE) RAISES { Rd.Failure,
                                                         Rd.EndOfFile,
                                                         Thread.Alerted } =
(* Updates global variable s.ch *)
VAR newBuf : REF ARRAY OF CHAR ;
BEGIN
  s.lastCh := s.ch ;
  s.ch := Rd.GetChar(s.rd) ;
  INC(s.offset) ;
  IF (s.ch = '\n') THEN
    INC(s.currentLine) ;
    s.lastLineLen := s.currentCol ;
    s.currentCol := 0
  ELSE
    INC(s.currentCol)
  END ;
  IF (record) THEN
    IF (s.bufLen = LAST(s.buf^)) THEN
    (* double the buffer size *)
      newBuf := NEW(REF ARRAY OF CHAR, NUMBER(s.buf^) * 2) ;
      SUBARRAY(newBuf^, 0, NUMBER(s.buf^)) := s.buf^ ;
      s.buf := newBuf
    END ;
    INC(s.bufLen) ;
    s.buf[s.bufLen] := s.ch
  END ;
  IF (IgnoreCase) THEN
    s.ch := ASCII.Upper[s.ch]
  END
END NextCh ;

PROCEDURE PrevCh(s : Scanner) =
BEGIN
  Rd.UnGetChar(s.rd) ;
  s.ch := s.lastCh ;
  DEC(s.offset) ;
  IF (s.bufLen > 0) THEN
    DEC(s.bufLen)
  END ;
  IF (s.currentCol = 0) THEN
    s.currentCol := s.lastLineLen ;
    DEC(s.currentLine)
  ELSE
    DEC(s.currentCol)
  END
END PrevCh ;

PROCEDURE Comment(s : Scanner) : BOOLEAN RAISES { Rd.Failure,
                                                  Rd.EndOfFile,
                                                  Thread.Alerted } =
VAR
  level                : CARDINAL := 1 ;
  <*NOWARN*> startLine : CARDINAL := s.currentLine ;
BEGIN
  -->Comment
  RETURN FALSE
END Comment ;

PROCEDURE GetSym(s : Scanner) : Symbol RAISES { Rd.Failure,
                                                Rd.EndOfFile,
                                                Thread.Alerted } =
VAR
  state  : CARDINAL ;

  PROCEDURE Equal(text : TEXT) : BOOLEAN =
  VAR tmp : ARRAY [0 .. -->MaxLiteralWidth] OF CHAR ;
      i   : CARDINAL ;
  BEGIN
    Text.SetChars(tmp, text) ;
    i := 0 ;
    WHILE ((i < s.bufLen) AND (s.buf[i] = tmp[i])) DO
      INC(i)
    END ;
    RETURN (i = s.bufLen)
  END Equal ;

  -->CheckLiteral

BEGIN
  s.buf[0]  := s.ch ;
  s.bufLen  := 0 ;
  s.apx     := 0 ;
  state     := StartState[s.ch] ;
  LOOP
    NextCh(s, TRUE) ;
    CASE state OF
    -->GetCore
    ELSE
      RETURN -->UnknownSym (* NextCh already done *)
    END
  END
END GetSym ;

PROCEDURE Get(s : Scanner ; VAR ss : ScanSymbol) RAISES { Rd.Failure,
                                                          Thread.Alerted } =
BEGIN
  TRY
    -->GetPrelude
    ss.offset := s.offset - 1 ;
    ss.line   := s.currentLine ;
    ss.column := s.currentCol ;
    ss.sym    := GetSym(s) ;
    ss.length := s.bufLen ;
    ss.string := Text.FromChars(SUBARRAY(s.buf^, 0, s.bufLen)) ;
    IF (IgnoreCase) THEN
      VAR tmp := NEW(REF ARRAY OF CHAR, s.bufLen) ;
      BEGIN
        FOR i := 0 TO s.bufLen - 1 DO
          tmp[i] := ASCII.Upper[s.buf[i]]
        END ;
        ss.name := Text.FromChars(tmp^)
      END
    ELSE
      ss.name := ss.string
    END
  EXCEPT
    Rd.EndOfFile =>
      ss.line := s.currentLine ;
      ss.column := s.currentCol ;
      ss.sym    := -->EofSym ;
      ss.string := NIL ;
      ss.name   := NIL
  END
END Get ;

PROCEDURE Init(s : Scanner ; rd : Rd.T ; e : ErrHandler) : Scanner =
BEGIN
  s.rd          := rd ;
  s.err         := e ;
  s.offset      := 0 ;
  s.currentLine := 1 ;
  s.currentCol  := 0 ;
  s.lastLineLen := 0 ;
  s.oldEols     := 0 ;
  s.apx         := 0 ;
  s.lastCh      := ASCII.NUL ;
  s.buf         := NEW(REF ARRAY OF CHAR, 128) ;
  s.bufLen      := 0 ;
  TRY
    NextCh(s)
  EXCEPT ELSE END ;
  RETURN s
END Init ;

BEGIN
END -->Scanner.
