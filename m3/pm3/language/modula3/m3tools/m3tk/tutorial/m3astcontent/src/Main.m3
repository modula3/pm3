(* Michel Dagenais, Dept. of Electrical and Computer Engineering,
   Ecole Polytechnique de Montreal (dagenais@vlsi.polymtl.ca),
   15 April 1997. 
*)

(* This program prints the content of a Modula-3 Abstract syntax tree
   produced by the Modula-3 toolkit "m3tk". It is intended to be useful
   both to see how the abstract syntax tree (ast) is stored, and how to use
   the Modula-3 toolkit. 

   The first few procedures implement support functions such as printing
   the units processed and the content of the abstract syntax tree. Then,
   the procedure "DoRun" performs the main task (compiling and then printing
   the content of the abstract syntax tree). The main body simply registers
   the command line arguments and calls the m3tk tool framework which calls
   back "DoRun". *)

MODULE Main;

IMPORT M3ToolFrame, M3AST_all, M3Context, M3CFETool,
       SeqM3AST_LX_SRC_NODE, M3AST_AS_F, M3CId, M3CLiteral, M3CWhitespace,
       Stdio, Wr, M3Args, Pathname, M3CUnit, M3AST_AS, Text, 
       M3ASTDisplay, ASTWalk, AST, M3CSrcPos, Fmt,
       M3AST_LX, M3CComment, M3AST_FE, Thread;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>

(* Convert a file name into a unit name and unit type: Main.m3
   is a "M3CUnit.Type.Module" named "Main" *)

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

(* This procedure is called once for each unit in the compilation "context".
   The unit received as argument is printed. *)

PROCEDURE PrintUnit(<*UNUSED*>cl: M3Context.Closure; 
    <*UNUSED*>ut: M3CUnit.Type; name: TEXT; 
    <*UNUSED*>cu: M3AST_AS.Compilation_Unit) =
  BEGIN
    Wr.PutText(Stdio.stdout,name & "\n");
  END PrintUnit;

(* The DisplayClosure has its "callback" method called for each node in
   the abstract syntax tree. It stores information which needs to be carried
   between calls, such as the current level of indentation, and the comments
   which are stored separately from the syntax tree. *)

TYPE
  DisplayClosure = M3ASTDisplay.Closure OBJECT 
      wr: Wr.T;
      comments: M3CComment.Iter;
      comment: M3CComment.T;
      indent := 0;
    OVERRIDES
      callback := DisplayCallback;
    END;

(* Print all the comments which are before the position of the current 
   abstract syntax tree node. *)

PROCEDURE DisplayComments(self: DisplayClosure; line, char: CARDINAL) =
  VAR
    cline, cchar: CARDINAL;
  BEGIN
    WHILE self.comment # NIL DO
      (* Extract the character and line position of the comment *)
      cline := M3CSrcPos.Unpack(M3CComment.Position(self.comment),cchar);

      (* There is a comment before the current ast node, print it *)
      IF (cline < line) OR ((cline = line) AND (cchar < char)) THEN
        Indentation(self.indent,self.wr);
        Wr.PutText(self.wr,"Comment at position(" & Fmt.Int(cline) & "," &
        Fmt.Int(cchar) & "): " & M3CComment.Body(self.comment) & "\n");
        IF M3CComment.Next(self.comments, self.comment) THEN
        ELSE self.comment := NIL;
        END;

      (* no comment left before the current ast node, exit *)
      ELSE
        EXIT;
      END;
    END;
  END DisplayComments;

(* This procedure is called upon entering and exiting each ast node *)

PROCEDURE DisplayCallback(self: DisplayClosure; n: AST.NODE; 
    vm := ASTWalk.VisitMode.Entry) RAISES ANY =
  VAR
    src_node: M3AST_LX.SRC_NODE := n;
    line, char: CARDINAL;
  BEGIN
    line := M3CSrcPos.Unpack(src_node.lx_srcpos,char);

    (* Entering the node. Print the comments, increase the indentation,
       display a message "entering". *)

    IF vm = ASTWalk.VisitMode.Entry THEN
      DisplayComments(self,line,char);
      Indentation(self.indent,self.wr);
      INC(self.indent);
      Wr.PutText(self.wr,"Entering ");

    (* Leaving the node, decrement the indentation. *)
    ELSE
      DEC(self.indent);
      Indentation(self.indent,self.wr);
      Wr.PutText(self.wr,"Exiting ");
    END;

    (* Print the "type" of node encountered *)

    Wr.PutText(self.wr,n.name() & " at position(" & Fmt.Int(line) & "," &
        Fmt.Int(char) & ")\n");

    IF NOT vm = ASTWalk.VisitMode.Entry THEN RETURN; END;

    (* Upon entry, try to print information about the lexical elements
       contained within the node. This typically is "identifiers". *)

    IF NOT ISTYPE(src_node,M3AST_LX.SRC_NODE_C) THEN 
      Indentation(self.indent,self.wr);
      Display(self.wr,src_node);
      RETURN;
    END;

    (* While leaves may contain an identifier, internal nodes of the tree
       usually contain no lexical element. *)

    VAR
      src_node_c := NARROW(n,M3AST_LX.SRC_NODE_C);
      iter := SeqM3AST_LX_SRC_NODE.NewIter(src_node_c.lx_node_s);
    BEGIN
      WHILE SeqM3AST_LX_SRC_NODE.Next(iter, src_node) DO
        Indentation(self.indent,self.wr);
        Display(self.wr,src_node,TRUE);
      END;
    END;
  END DisplayCallback;

(* This procedure prints the lexical element contained within a node *)

PROCEDURE Display(wr: Wr.T; node: M3AST_LX.SRC_NODE; inSrcNodeList := FALSE) =
  BEGIN
    TYPECASE node OF
    | M3AST_AS_F.ID(n) => Wr.PutText(wr, "ID " &
          M3CId.ToText(n.lx_symrep));
    | M3AST_AS_F.LITERAL(n) => Wr.PutText(wr, "Literal " &
          M3CLiteral.ToText(n.lx_litrep));
    | M3AST_AS_F.Whitespace(n) =>Wr.PutText(wr,"WhiteSpace " & 
          M3CWhitespace.ToText(n.lx_whitespace_rep));
    | M3AST_AS_F.Comment => Wr.PutText(wr,"Comment");
    | M3AST_AS_F.Pragma => Wr.PutText(wr,"Pragma");
    | M3AST_LX.SRC_NODE_C => Wr.PutText(wr,"SRC NODE C!!");
    ELSE
      Wr.PutText(wr,"Unknown lexical token");
    END;
    IF inSrcNodeList THEN 
      Wr.PutText(wr," (In lexical SRC node list)\n"); 
    ELSE
      Wr.PutText(wr,"\n");
    END;
  END Display;

(* Output the correct number of blanks for the current indentation level. *)

PROCEDURE Indentation(indent: CARDINAL; wr: Wr.T) =
  BEGIN
    FOR i := 0 TO (indent - 1) DO Wr.PutText(wr,"  "); END;
  END Indentation;

(* This is where the main decisions are performed. This procedure is
   called by the m3tk tool framework. *)

PROCEDURE DoRun(<*UNUSED*> w: M3ToolFrame.Worker; c: M3Context.T;
                <*UNUSED*> compileResult: INTEGER): INTEGER RAISES {}=
  VAR returnCode: INTEGER;
    args: REF ARRAY OF TEXT;
    name: TEXT;
    type: M3CUnit.Type;
    unit: M3AST_AS.Compilation_Unit;
    cl := NEW(DisplayClosure);
  BEGIN
    (* Get the list of units to process. Return if empty. *)

    args := M3Args.GetStringList(M3CFETool.GetTool(),M3CFETool.PathNames_Arg);
    IF args = NIL THEN RETURN 0; END;

    (* Check the options determining which compilation phases to perform
       and the amount of output to produce. *)

    IF M3Args.GetFlag(astTool,ParseArg) THEN phases := ParsePhase; END;
    IF M3Args.GetFlag(astTool,ImportArg) THEN phases := ImportPhase; END;
    verbose := M3Args.GetFlag(astTool,VerboseArg);

    (* Perform the compilation *)

    returnCode := M3CFETool.CompileInContext(c, phases := phases);

    (* Print the list of units processed, noting those included once the
       import/export are resolved. *)

    IF verbose THEN
      Wr.PutText(Stdio.stdout,
          "Compilation of the following files was requested\n");
      FOR i := 0 TO LAST(args^) DO
        Wr.PutText(Stdio.stdout,args[i] & "\n");
      END;

      Wr.PutText(Stdio.stdout,
          "\n\nAfter imports, the following were processed\n");
      <*NOWARN*>M3Context.Apply(c, NEW(M3Context.Closure, 
          callback := PrintUnit), findStandard := FALSE);
      Wr.PutText(Stdio.stdout,"\n\n");
    END;

    (* For each unit requested, print the content of the abstract syntax
       tree. *)

    FOR i := 0 TO LAST(args^) DO

      (* Find the unit corresponding to the user specified file name *)
      UnitName(args[i],name,type);
      IF NOT M3Context.FindExact(c, name, type, unit) THEN <*ASSERT FALSE*>
      END;

      (* Prepare the Closure used to visit all the nodes in the ast. *)
      cl.wr := Stdio.stdout;
      cl.indent := 0;

      (* initialize the list of comments with the next comment always
         stored in cl.comment and NIL if no comments are left. *)
      cl.comments := M3CComment.NewIter(unit.lx_comments);
      EVAL M3CComment.Next(cl.comments, cl.comment);
      <*NOWARN*>ASTWalk.ModeVisitNodes(unit,cl,ASTWalk.OnEntryAndExit);

      (* Process any comment occuring after the last node in the ast *)
      DisplayComments(cl,LAST(INTEGER),0);
    END;

    RETURN returnCode;
  END DoRun;

CONST
  ParsePhase = M3AST_FE.Unit_status{M3AST_FE.Unit_state.Parsed};
  ImportPhase = M3AST_FE.Unit_status{M3AST_FE.Unit_state.Parsed, 
      M3AST_FE.Unit_state.ImportsResolved};
  SemanticPhase = M3AST_FE.Unit_AllPhases;
  ParseArg = "parse";
  ImportArg = "resolve";
  VerboseArg = "verbose";

(* The Modula-3 toolkit assumes that several tools are connected to the
   compiler. Each tool upon initialization registers its command line
   arguments along with information for the on-line help. Then, during
   the compilation, each tool gets a chance to "DoRun", query the
   command line arguments and act upon the abstract syntax tree. 

   The "m3astcontent" tool is defined and its command line
   arguments "parse", "resolve" and "verbose" are registered. *)
   
VAR
  astTool := M3Args.New("m3astcontent",
      "Print the content of the AST for Modula-3 source code","1.0");
  phases := SemanticPhase;
  verbose := FALSE;

BEGIN
  M3Args.RegisterFlag(astTool,ParseArg, 
      "Stop the processing after the lexical analysis");
  M3Args.RegisterFlag(astTool,ImportArg, 
      "Stop the processing after resolving the imports");
  M3Args.RegisterFlag(astTool,VerboseArg, 
      "Display the list of files processed");

  (* Start the tool framework adding a "worker" to act upon the produced
     ast. The "worker" does not need the compilation to take place
     (compile := false) as its method "DoRun" will explicitly call
     the compiler and specify the desired compilation phases. *)

  <*NOWARN*>EVAL M3ToolFrame.Startup(NEW(M3ToolFrame.Worker, work := DoRun),
      compile := FALSE);
END Main.
