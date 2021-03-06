MODULE CRS EXPORTS CR ;

(* Scanner generated by m3coco *)

IMPORT ASCII, Text, Rd, Thread ;

CONST
  StartState = ARRAY CHAR OF CARDINAL {
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24,  2, 24,  6, 24, 24,  4, 12, 13, 24,  9, 24, 10,  8, 24,
      5,  5,  5,  5,  5,  5,  5,  5,  5,  5, 24, 24, 19,  7, 20, 24,
     24,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
      1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 15, 24, 16, 24, 24,
     24,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
      1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 17, 14, 18, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24
  } ;

  IgnoreCase = FALSE ;

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
  IF (s.ch = '(') THEN
    NextCh(s) ;
    IF (s.ch = '*') THEN
      NextCh(s) ;
      LOOP
        IF (s.ch = '*') THEN
          NextCh(s) ;
          IF (s.ch = ')') THEN
            DEC(level) ;
            NextCh(s) ;
            IF (level = 0) THEN
              RETURN TRUE
            END
          END ;
        ELSIF (s.ch = '(') THEN
          NextCh(s) ;
          IF (s.ch = '*') THEN
            INC(level) ;
            NextCh(s)
          END ;
        ELSIF (s.ch = ASCII.NUL) THEN
          RETURN FALSE
        ELSE
          NextCh(s)
        END
      END ;
    ELSE
      IF (s.ch = '\n') THEN
        DEC(s.currentLine)
      END ;
      PrevCh(s)
    END
  END ;
  RETURN FALSE
END Comment ;

PROCEDURE GetSym(s : Scanner) : Symbol RAISES { Rd.Failure,
                                                Rd.EndOfFile,
                                                Thread.Alerted } =
VAR
  state  : CARDINAL ;

  PROCEDURE Equal(text : TEXT) : BOOLEAN =
  VAR tmp : ARRAY [0 .. 11] OF CHAR ;
      i   : CARDINAL ;
  BEGIN
    Text.SetChars(tmp, text) ;
    i := 0 ;
    WHILE ((i < s.bufLen) AND (s.buf[i] = tmp[i])) DO
      INC(i)
    END ;
    RETURN (i = s.bufLen)
  END Equal ;

  PROCEDURE CheckLiteral(nomatch : Symbol) : Symbol =
  BEGIN
    CASE s.buf[0] OF
      'A' => IF ((s.bufLen = 3) AND Equal("ANY")) THEN
               RETURN Symbol.sANY
             END
    | 'C' => IF ((s.bufLen = 4) AND Equal("CASE")) THEN
               RETURN Symbol.sCASE
           ELSIF ((s.bufLen = 10) AND Equal("CHARACTERS")) THEN
               RETURN Symbol.sCHARACTERS
           ELSIF ((s.bufLen = 3) AND Equal("CHR")) THEN
               RETURN Symbol.sCHR
           ELSIF ((s.bufLen = 8) AND Equal("COMMENTS")) THEN
               RETURN Symbol.sCOMMENTS
           ELSIF ((s.bufLen = 8) AND Equal("COMPILER")) THEN
               RETURN Symbol.sCOMPILER
           ELSIF ((s.bufLen = 7) AND Equal("CONTEXT")) THEN
               RETURN Symbol.sCONTEXT
             END
    | 'E' => IF ((s.bufLen = 3) AND Equal("END")) THEN
               RETURN Symbol.sEND
             END
    | 'F' => IF ((s.bufLen = 4) AND Equal("FROM")) THEN
               RETURN Symbol.sFROM
             END
    | 'I' => IF ((s.bufLen = 6) AND Equal("IGNORE")) THEN
               RETURN Symbol.sIGNORE
             END
    | 'N' => IF ((s.bufLen = 5) AND Equal("NAMES")) THEN
               RETURN Symbol.sNAMES
           ELSIF ((s.bufLen = 6) AND Equal("NESTED")) THEN
               RETURN Symbol.sNESTED
             END
    | 'P' => IF ((s.bufLen = 7) AND Equal("PRAGMAS")) THEN
               RETURN Symbol.sPRAGMAS
           ELSIF ((s.bufLen = 11) AND Equal("PRODUCTIONS")) THEN
               RETURN Symbol.sPRODUCTIONS
             END
    | 'S' => IF ((s.bufLen = 4) AND Equal("SYNC")) THEN
               RETURN Symbol.sSYNC
             END
    | 'T' => IF ((s.bufLen = 2) AND Equal("TO")) THEN
               RETURN Symbol.sTO
           ELSIF ((s.bufLen = 6) AND Equal("TOKENS")) THEN
               RETURN Symbol.sTOKENS
             END
    | 'W' => IF ((s.bufLen = 4) AND Equal("WEAK")) THEN
               RETURN Symbol.sWEAK
             END
    ELSE
    END ;
    RETURN nomatch
  END CheckLiteral ;

BEGIN
  s.buf[0]  := s.ch ;
  s.bufLen  := 0 ;
  s.apx     := 0 ;
  state     := StartState[s.ch] ;
  LOOP
    NextCh(s, TRUE) ;
    CASE state OF
        1 => IF ((s.ch >= '0') AND (s.ch <= '9')) OR
                ((s.ch >= 'A') AND (s.ch <= 'Z')) OR
                ((s.ch >= 'a') AND (s.ch <= 'z')) THEN
             ELSE
               RETURN CheckLiteral(Symbol.ident)
             END
    |   2 => IF (s.ch = '\000') OR
                ((s.ch >= ' ') AND (s.ch <= '!')) OR
                (s.ch >= '#') THEN
             ELSIF (s.ch = '"') THEN
               state := 3 ;
             ELSE
               RETURN Symbol.Undef
             END
    |   3 => RETURN Symbol.string
    |   4 => IF (s.ch = '\000') OR
                ((s.ch >= ' ') AND (s.ch <= '&')) OR
                (s.ch >= '(') THEN
             ELSIF (s.ch = '\'') THEN
               state := 3 ;
             ELSE
               RETURN Symbol.Undef
             END
    |   5 => IF ((s.ch >= '0') AND (s.ch <= '9')) THEN
             ELSE
               RETURN Symbol.number
             END
    |   6 => IF ((s.ch >= 'A') AND (s.ch <= 'Z')) OR
                ((s.ch >= 'a') AND (s.ch <= 'z')) THEN
             ELSE
               RETURN Symbol.Options
             END
    |   7 => RETURN Symbol.Equal
    |   8 => IF (s.ch = '.') THEN
               state := 11 ;
             ELSIF (s.ch = ')') THEN
               state := 22 ;
             ELSE
               RETURN Symbol.Dot
             END
    |   9 => RETURN Symbol.Plus
    |  10 => RETURN Symbol.Dash
    |  11 => RETURN Symbol.sDotDot
    |  12 => IF (s.ch = '.') THEN
               state := 21 ;
             ELSE
               RETURN Symbol.Lbr
             END
    |  13 => RETURN Symbol.Rbr
    |  14 => RETURN Symbol.Bar
    |  15 => RETURN Symbol.Lsqu
    |  16 => RETURN Symbol.Rsqu
    |  17 => RETURN Symbol.Lcurl
    |  18 => RETURN Symbol.Rcurl
    |  19 => RETURN Symbol.Less
    |  20 => RETURN Symbol.Gtr
    |  21 => RETURN Symbol.sLbrDot
    |  22 => RETURN Symbol.sDotRbr
    ELSE
      RETURN Symbol.Undef (* NextCh already done *)
    END
  END
END GetSym ;

PROCEDURE Get(s : Scanner ; VAR ss : ScanSymbol) RAISES { Rd.Failure,
                                                          Thread.Alerted } =
BEGIN
  TRY
    WHILE ((s.ch = ' ') OR ((s.ch >= '\t') AND (s.ch <= '\n')) OR
           (s.ch = '\r')) DO
      NextCh(s)
    END ;
    IF (((s.ch = '(')) AND Comment(s)) THEN
      Get(s, ss) ;
      RETURN
    END ;
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
      ss.sym    := Symbol.Eof ;
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
END CRS.
