
(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObliqParser;
IMPORT Thread, Rd, TextRd, Bundle, ObliqBdl, SynWr, SynScan, SynParse, MetaParser, ObParseFrame, ObTree, ObValue, ObParseTree, ObFrame, Obliq, SynLocation, ObErr, Process;
FROM ObValue IMPORT Error, Exception;

  VAR metaParserMutex: Thread.Mutex;
      obliqClauseList: MetaParser.ClauseList;

  VAR setupDone := FALSE;

  PROCEDURE PackageSetup() =
  BEGIN
    SynParse.PackageSetup();
    MetaParser.PackageSetup(); <* NOWARN *>
    Obliq.PackageSetup();
    
    IF NOT setupDone THEN
      setupDone := TRUE;
      ObFrame.Setup();
      ObParseTree.Setup();
      ObParseFrame.Setup();
      metaParserMutex := NEW(Thread.Mutex);
      obliqClauseList := NIL;
    END;
  END PackageSetup;

  PROCEDURE New(swr: SynWr.T): SynParse.T =
  VAR parser: SynParse.T; actions: MetaParser.ActionTable;
  BEGIN
    LOCK metaParserMutex DO
      IF obliqClauseList = NIL THEN
        actions := MetaParser.NewActionTable();
        ObParseFrame.RegisterActions(actions);
        ObParseTree.RegisterActions(actions); <* NOWARN *>
        obliqClauseList := MetaParser.NewClauseList(actions, "obliq.gr",
          TextRd.New(Bundle.Get(ObliqBdl.Get(), "ObliqGram")));
      END;
      parser := SynParse.New(swr, SynParse.NewEnv());
      MetaParser.AddClauseList(obliqClauseList, parser);
    END;
    SynScan.SetPrompt(parser.Scanner(), "- ", "  ");
    RETURN parser;
  END New;

  PROCEDURE ReadFrom(p: T; rdName: TEXT; rd: Rd.T; closeRd: BOOLEAN;
    generateEOF: BOOLEAN := TRUE) =
  BEGIN
    SynScan.PushInput(p.Scanner(), rdName, rd, closeRd, generateEOF);    
  END ReadFrom;
  
  PROCEDURE ParseTerm(p: SynParse.T): Obliq.Term RAISES {Error, Eof} =
  BEGIN
      TRY
        TYPECASE p.ReadNonTerminal("phrase") OF
        | ObTree.PhraseTerm(node) =>
          RETURN node.term;
        ELSE
          SynParse.Fault(p,
            "ObliqParser.ParseTerm: parsed a phrase that is not a term");
        END;
      EXCEPT
      | SynScan.NoReader =>
          RAISE Eof;    
      | SynParse.Fail, SynScan.Fail => 
          Obliq.RaiseError("Static Error");
      END;
  END ParseTerm;

  PROCEDURE ParsePhrase(p: SynParse.T): Obliq.Phrase RAISES {Error, Eof} =
  BEGIN
      TRY
        RETURN p.ReadNonTerminal("phrase");
      EXCEPT
      | SynScan.NoReader =>
          RAISE Eof;    
      | SynParse.Fail, SynScan.Fail => 
          Obliq.RaiseError("Static Error");
      END;
  END ParsePhrase;

  PROCEDURE EvalPhrase(p: SynParse.T; phrase: Phrase; VAR (*in-out*) env: Env; 
    loc: Location:=NIL): Obliq.Val(*or NIL*) RAISES {Error, Exception} =
  VAR val: ObValue.Val;
  BEGIN
    IF loc=NIL THEN loc:=SourceLocation("Obliq.EvalPhrase") END;
    val := NIL;
    TRY
      TYPECASE phrase OF
      | NULL =>
      | ObFrame.Quit => Process.Exit();
      | ObFrame.Load(node) =>
	    ObFrame.LoadFile(p.Scanner(), node.name);
      | ObFrame.Import(node) =>
	    ObFrame.ImportFrame(p.Scanner(), node.name, env);
      | ObFrame.Module(node) =>
	    ObFrame.ModuleFrame(p.Scanner(), node.name, node.for,
	      node.imports, env);
      | ObFrame.EndModule =>
	    ObFrame.ModuleEnd(p.Scanner());
      | ObFrame.Establish(node) =>
	    env := ObFrame.EstablishFrame(node.name, node.for, env);
      | ObFrame.Save(node) =>
	    env := ObFrame.SaveFrame(node.name, node.name, env);
      | ObFrame.Delete(node) =>
	    env := ObFrame.DeleteFrame(node.name, env);
      | ObFrame.Qualify =>
	    env := ObFrame.QualifyFrame(env);
      | ObTree.PhraseCommand, 
        ObTree.PhraseTerm =>
          val := Obliq.EvalPhrase(phrase, (*in-out*) env, loc);
      END;
      RETURN val;
    EXCEPT
    | ObErr.Fail => 
        Obliq.RaiseError("Static Error", loc);
    END;
  END EvalPhrase;

  PROCEDURE SourceLocation(where: TEXT): SynLocation.T =
  BEGIN
    RETURN
      SynLocation.NewLineLocation(
        SynLocation.Info{fileName:=where, line:=0, lineChar:=0, char:=0});
  END SourceLocation;

BEGIN
END ObliqParser.
