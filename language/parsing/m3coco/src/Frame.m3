MODULE Frame ;

IMPORT Rd, Wr, Text, TextRd, Stdio, Process, Fmt, ASCII, TextTextTbl, Thread ;
IMPORT FileRd, FileWr, Bundle, CocoFrames, Pathname ;

VAR
  tbl : TextTextTbl.T ;

CONST
  Pattern    = ARRAY OF CHAR { '-', '-', '>' } ;
  MaxNameLen = 32 ;

PROCEDURE Put(name : TEXT ; content : TEXT) =
BEGIN
  IF (Text.Length(name) > MaxNameLen) THEN
    TRY
      Wr.PutText(Stdio.stderr, "internal error : illegal frame name '"
                                                              & name & "'\n") ;
    EXCEPT ELSE END ;
    Process.Exit(1)
  END ;
  IF (tbl.put(name, content)) THEN
    TRY
      Wr.PutText(Stdio.stderr, "internal warning : replacing '"
                                                          & name & "' frame\n")
    EXCEPT ELSE END
  END
END Put ;

PROCEDURE Get(name : TEXT) : TEXT =
VAR content : TEXT ;
BEGIN
  IF (tbl.get(name, content)) THEN
    RETURN content
  ELSE
    TRY
      Wr.PutText(Stdio.stderr, "Frame element '" & name & "' undefined!\n")
    EXCEPT ELSE END ;
    Process.Exit(1) ;
    RETURN NIL  (* needed because it doesn't know Process.Exit never returns *)
  END
END Get ;

PROCEDURE Create(srcDir : Pathname.T ; src : TEXT ; altSrc : TEXT ; dest : TEXT) =
VAR rd : Rd.T ;
    wr : Wr.T ;
BEGIN
(* try altSrc then src as filenames, finally src as a bundle element *)
  IF (altSrc # NIL) THEN
    TRY
      rd := FileRd.Open(Pathname.Join(srcDir, altSrc, NIL))
    EXCEPT
    ELSE
      rd := NIL
    END
  END ;

  IF (rd = NIL) THEN
    TRY
      rd := FileRd.Open(Pathname.Join(srcDir, src, NIL))
    EXCEPT
    ELSE
      rd := TextRd.New(Bundle.Get(CocoFrames.Get(), src))
    END
  END ;

  TRY
    wr := FileWr.Open(dest)
  EXCEPT
  ELSE
    TRY
      Wr.PutText(Stdio.stderr, "Cannot create '" & dest & "'!\n")
    EXCEPT ELSE END ;
    Process.Exit(1)
  END ;

  TRY
    TRY
      Copy(rd, wr) ;
    FINALLY
      Rd.Close(rd) ;
      Wr.Close(wr)
    END
  EXCEPT
  ELSE
    TRY
      Wr.PutText(Stdio.stderr, "Failed to create '" & dest & "'!\n")
    EXCEPT ELSE END ;
    Process.Exit(1)
  END
END Create ;

PROCEDURE Copy(src : Rd.T ; dest : Wr.T) RAISES { Rd.Failure, Wr.Failure,
                                                  Thread.Alerted } =
VAR temp : ARRAY [0 .. MaxNameLen - 1] OF CHAR ;
    i    : CARDINAL ;
    ch   : CHAR ;
    pos  : CARDINAL ;
BEGIN
  TRY
    LOOP
      ch := Rd.GetChar(src) ;
      IF (ch = '\n') THEN
        pos := 0
      ELSE
        INC(pos)
      END ;
      IF (ch = Pattern[0]) THEN
        i := 0 ;
        WHILE ((i < LAST(Pattern)) AND (ch = Pattern[i])) DO
          INC(i) ;
          ch := Rd.GetChar(src)
        END ;
        IF (i = LAST(Pattern)) THEN
          i := 0 ;
          ch := Rd.GetChar(src) ;
          WHILE ((i < MaxNameLen) AND (ch IN ASCII.AlphaNumerics)) DO
            temp[i] := ch ;
            INC(i) ;
            ch := Rd.GetChar(src)
          END ;
          IF (i = 0) THEN
            Wr.PutString(dest, Pattern) ;
            INC(pos, NUMBER(Pattern))
          ELSE
            CopyContent(dest, Get(Text.FromChars(SUBARRAY(temp, 0, i))),
                              pos - 1) ;
            Wr.PutChar(dest, ch)
          END
        ELSE
          Wr.PutString(dest, SUBARRAY(Pattern, 0, i)) ;
          Wr.PutChar(dest, ch) ;
          INC(pos, i)
        END
      ELSE
        Wr.PutChar(dest, ch)
      END
    END
  EXCEPT
    Rd.EndOfFile =>
  END
END Copy ;

PROCEDURE CopyContent(dest : Wr.T ; src : TEXT ;
                      offset : CARDINAL) RAISES { Rd.Failure, Wr.Failure,
                                                  Thread.Alerted } =
VAR rd    := TextRd.New(src) ;
    first := TRUE ;
    pad   : TEXT ;
    line  : TEXT ;
BEGIN
  IF (offset > 0) THEN
    pad := Fmt.Pad("", offset)
  END ;
  TRY
    LOOP
      line := Rd.GetLine(rd) ;
      IF (NOT first) THEN
        Wr.PutChar(dest, '\n') ;
        IF (offset > 0) THEN
          Wr.PutText(dest, pad)
        END
      END ;
      Wr.PutText(dest, line) ;
      first := FALSE
    END
  EXCEPT
    Rd.EndOfFile =>
  END
END CopyContent ;

BEGIN
  tbl := NEW(TextTextTbl.Default).init()
END Frame.
