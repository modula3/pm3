
MODULE Main;

IMPORT SGML, SGMLPrintStructure, Stdio, Params;

VAR
  options: SGML.ParserOptions;
  parser := NEW(SGML.Parser);
  files := NEW(REF ARRAY OF TEXT,Params.Count - 1);

BEGIN
  FOR i := 0 TO LAST(files^) DO
    files[i] := Params.Get(i + 1);
  END;

  (* Create a parser with the default options and the command line arguments
     as list of files to parse *)

  EVAL parser.init(options,Params.Get(0),files);

  (* Parse the files specifying a PrintStructure SGML.Application as callback
     object. The annotated normalized output goes to standard I/O. *)

  EVAL parser.run(NEW(SGMLPrintStructure.T, wr := Stdio.stdout).init());
END Main.
