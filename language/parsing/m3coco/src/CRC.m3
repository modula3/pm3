MODULE CRC ;

IMPORT CRA, CRS, CRT ;
IMPORT Rd, Wr, Fmt, Pathname, FileRd, FileWr, Process, Stdio ;
IMPORT TextRd, Bundle, CocoFrames ;

<* FATAL ANY *>

PROCEDURE WriteDriver() =
VAR
  leftMargin : CARDINAL ;
  gn         : CRT.GraphNode ;
  sn         : CRT.SymbolNode ;
  err        : Rd.T ; (* error message texts *)
  fram       : Rd.T ; (* parser frame parser.frm *)
  com        : Wr.T ; (* generated parser *)
  path       : Pathname.T ;
  line       : TEXT ;
BEGIN
  CRT.GetNode(CRT.root, gn) ;
  CRT.GetSym(gn.p1, sn) ;

(* search for the compiler frame file *)

  fram := NIL ;
  IF (NOT CRT.ddt['B']) THEN
    path := Pathname.Join(CRS.directory, sn.name, "frm") ;
    TRY
      fram := FileRd.Open(path)
    EXCEPT
    ELSE
      path := Pathname.Join(CRS.directory, "compiler", "frm") ;
      TRY
        fram := FileRd.Open(path)
      EXCEPT
      ELSE
        fram := NIL
      END
    END
  END ;
  IF (fram = NIL) THEN
    fram := TextRd.New(Bundle.Get(CocoFrames.Get(), "compiler.frm"))
  END ;

  leftMargin := 0;

  path := Pathname.Join(NIL, sn.name & "e", "txt") ;
  TRY
    err := FileRd.Open(path) ;
  EXCEPT
  ELSE
    Wr.PutText(Stdio.stdout, "\nCannot open " & path & " - Aborted.\n") ;
    Process.Exit(1)
  END ;

  path := Pathname.Join(NIL, sn.name, "m3") ;
  TRY
    com := FileWr.Open(path)
  EXCEPT
  ELSE
    Wr.PutText(Stdio.stdout, "\nCannot open " & path & " - Aborted.\n") ;
    Process.Exit(1)
  END ;

  CRA.CopyFramePart("-->Grammar", leftMargin, fram, com);
  Wr.PutText(com, sn.name) ;

  CRA.CopyFramePart("-->Scanner", leftMargin, fram, com);
  Wr.PutText(com, sn.name) ;
  Wr.PutChar(com, 'S') ;

  CRA.CopyFramePart("-->Parser", leftMargin, fram, com);
  Wr.PutText(com, sn.name) ;
  Wr.PutChar(com, 'P') ;

  CRA.CopyFramePart("-->Errors", leftMargin, fram, com);

  TRY
    LOOP
      line := Rd.GetLine(err) ;
      Wr.PutText(com, Fmt.Pad("", leftMargin)) ;
      Wr.PutText(com, line) ;
      Wr.PutChar(com, '\n')
    END
  EXCEPT
    Rd.EndOfFile =>
  ELSE
    Wr.PutText(Stdio.stdout, "error copying errors - Aborted.\n")
  END ;

  CRA.CopyFramePart("-->Grammar", leftMargin, fram, com);
  Wr.PutText(com, sn.name) ;

  CRA.CopyFramePart("-->$$$", leftMargin, fram, com);
  Wr.Close(com) ;
  Rd.Close(err) ;
  Rd.Close(fram) ;
END WriteDriver;

BEGIN
END CRC.
