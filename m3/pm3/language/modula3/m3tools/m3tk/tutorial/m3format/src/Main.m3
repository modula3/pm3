(* Michel Dagenais, Dept. of Electrical and Computer Engineering,
   Ecole Polytechnique de Montreal (dagenais@vlsi.polymtl.ca),
   15 April 1997. 
*)

(* This program traverses the Modula-3 abstract syntax tree (ast) for
   the requested units, checks for adherence to programming conventions
   (indentation, upper/lower case in identifiers), and produces a formatted
   printout of the code described by the ast. It is intended as an example
   of how to use the Modula-3 toolkit to analyze Modula-3 source code,
   eventually modifying the code, and finally printing out the source code.

   The main body initializes the command line arguments and starts
   the compiler front-end tool, specifying "DoRun" as callback.
   The "DoRun" procedure then performs the compilation and subsequently
   the analysis and printout of the generated ast.

   The M3Conv module performs the formatting and programming conventions
   checking. *)

MODULE Main;

IMPORT M3ToolFrame, M3AST_all, M3Context, M3CFETool, M3AST_FE,
    Stdio, Wr, M3Args, Pathname, M3CUnit, M3AST_AS, Text, M3Conv, 
    M3ASTDisplay, FileWr, OSError, Thread;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>
<*FATAL OSError.E*>

(* Convert the filename into a unit name and unit type *)

PROCEDURE UnitName(path: TEXT; VAR name: TEXT; VAR type: M3CUnit.Type) =
  VAR
    ext: TEXT;
  BEGIN
    name := Pathname.LastBase(path);
    ext := Pathname.LastExt(path);
    IF Text.Equal(ext,"i3") THEN type := M3CUnit.Type.Interface;
    ELSIF Text.Equal(ext,"ig") THEN type := M3CUnit.Type.Interface_gen_def;
    ELSIF Text.Equal(ext,"m3") THEN type := M3CUnit.Type.Module;
    ELSIF Text.Equal(ext,"mg") THEN type := M3CUnit.Type.Module_gen_def;
    ELSE <*ASSERT FALSE *>
    END;
  END UnitName;

(* This procedure is called back by the compiler front-end once the
   compilation context is setup. It then performs the core of the
   work: parse the specified files, analyse the generated ast, and
   printout the result. *)

PROCEDURE DoRun(<*UNUSED*> w: M3ToolFrame.Worker; c: M3Context.T;
    <*UNUSED*> compileResult: INTEGER): INTEGER RAISES {}=
  VAR returnCode: INTEGER;
    args: REF ARRAY OF TEXT;
    name: TEXT;
    type: M3CUnit.Type;
    unit: M3AST_AS.Compilation_Unit;
    wr: Wr.T;
  BEGIN
    (* Parse the files. The import/export resolution and semantic analysis
       are not needed for the "formatting" work performed here. *)

    returnCode := M3CFETool.CompileInContext(c, 
        phases := M3AST_FE.Unit_status{M3AST_FE.Unit_state.Parsed});

    (* Read the value of the command line arguments *)

    args := M3Args.GetStringList(M3CFETool.GetTool(),M3CFETool.PathNames_Arg);
    formatExtension := M3Args.GetString(formatTool,FormatArg);
    check := M3Args.GetFlag(formatTool,CheckFormatArg);
    verbose := M3Args.GetFlag(formatTool,VerboseArg);
    underscore := M3Args.GetFlag(formatTool,UnderscoreArg);
    IF formatExtension = NIL THEN check := TRUE; END;

    (* Print the list of processed files *)

    IF verbose THEN
      Wr.PutText(Stdio.stdout,
          "Compilation of the following files was requested\n");
      FOR i := 0 TO LAST(args^) DO
        Wr.PutText(Stdio.stdout,args[i] & "\n");
      END;
      Wr.PutText(Stdio.stdout,"\n\n");
    END;

    (* For each requested file name, find the corresponding unit in the
       compilation context. Then perform the operations on this unit. *)

    FOR i := 0 TO LAST(args^) DO
      UnitName(args[i],name,type);
      IF NOT M3Context.FindExact(c, name, type, unit) THEN <*ASSERT FALSE*>
      END;

      (* Process the ast and generate a pretty printed output in file
         name & formatExtension, as requested. *)

      IF formatExtension # NIL THEN
        wr := FileWr.Open(args[i] & formatExtension);
        M3Conv.Set(unit, checkFormat := check, underscore := underscore);
        M3ASTDisplay.Nodes(unit,wr);
        Wr.Close(wr);

      (* Only checking the programming conventions is requested, no pretty
         printed output. *)
      ELSE
        M3Conv.Set(unit, checkFormat := check, underscore := underscore);
      END;
    END;

    RETURN returnCode;
  END DoRun;

(* The Modula-3 toolkit allows defining several tools, each with its set of
   command line arguments. Define a few arguments for m3format. The arguments
   for specifying the files to process and their location are managed by the
   compiler front end tool. *)

VAR
  formatTool := M3Args.New("m3format",
      "Formatter/checker for Modula-3 source code","1.0");
  check, verbose, underscore: BOOLEAN := FALSE;
  formatExtension: TEXT := NIL;

CONST
  FormatArg = "format";
  CheckFormatArg = "checkFormat";
  VerboseArg = "verbose";
  UnderscoreArg = "underscore";

BEGIN
  (* Register the command line arguments for this tool *)

  M3Args.RegisterString(formatTool,FormatArg,
      "Extension for formatted units (e.g. -format .new -> Unit.m3.new)");
  M3Args.RegisterFlag(formatTool,CheckFormatArg, 
      "Check the formatting of the files even if formatted files are\n" &
      "requested with option -format");
  M3Args.RegisterFlag(formatTool,VerboseArg, 
      "Display all the files processed");
  M3Args.RegisterFlag(formatTool,UnderscoreArg, 
      "Do not complain for underscore in identifiers");

  (* Start the compiler tools, which will call back the DoRun procedure *)

  <*NOWARN*>EVAL M3ToolFrame.Startup(NEW(M3ToolFrame.Worker, work := DoRun),
      compile := FALSE);
END Main.
