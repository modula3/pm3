MODULE xUtils;
(*Copyright (c) 1996, m3na project
  
Abstract: m3na Utilities.

12/13/95  Harry George    Initial version
1/27/96   Harry George    Comverted to m3na
*)
IMPORT Word,IO,Stdio,Fmt,TextWr,Wr;

CONST Module = "xUtils.";

(*---debug---*)
PROCEDURE debug(level:[0..3]; ftn,str:TEXT)=
BEGIN
  IF verbosity >= level THEN
    Wr.PutText(Stdio.stdout,"m3na." & ftn & ":" & str & "\n");
    Wr.Flush(Stdio.stdout);
  END;
END debug;

(*---errors-----------*)
PROCEDURE err(ftn:TEXT; code:Err; errmsg :TEXT:=NIL) RAISES {Error}=
BEGIN
  IO.Put("m3na error:" & ftn & ":" & errmsg & "\n");
  RAISE Error(code);
END err;

(*==========================*)
BEGIN
  verbosity:=3;
END xUtils.
