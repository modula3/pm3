(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE ObParseTree;
IMPORT SynLocation, SynScan, Text, SynParse, ObLib, ObTree, MetaParser;

  PROCEDURE SelectText(p: SynParse.T; index: INTEGER): TEXT =
    BEGIN
      RETURN NARROW(p.stack[index], MetaParser.TextTemp).text;
    END SelectText;

  PROCEDURE SelectInt(p: SynParse.T; index: INTEGER): INTEGER =
    BEGIN
      RETURN NARROW(p.stack[index], MetaParser.IntegerTemp).int;
    END SelectInt;

  PROCEDURE SelectReal(p: SynParse.T; index: INTEGER): LONGREAL =
    BEGIN
      RETURN NARROW(p.stack[index], MetaParser.RealTemp).real;
    END SelectReal;

  PROCEDURE Select1(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+1];
    END Select1;

  PROCEDURE Select2(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+2];
    END Select2;

  PROCEDURE Select3(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+3];
    END Select3;

  PROCEDURE Select4(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+4];
    END Select4;

  PROCEDURE Select5(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+5];
    END Select5;

  PROCEDURE Select6(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN p.stack[base+6];
    END Select6;

  PROCEDURE BuildIdeName(p: SynParse.T; index: INTEGER): ObTree.IdeName =
    BEGIN
      RETURN NEW(ObTree.IdeName, text:=SelectText(p, index), variant:=0);
    END BuildIdeName;

  PROCEDURE BuildPhraseEmpty(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree =
    BEGIN
      RETURN NIL;
    END BuildPhraseEmpty;

  PROCEDURE BuildPhraseFlag(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
  VAR name, arg: TEXT;
  BEGIN
    IF p.stack[base+1]=NIL THEN name:="?"; arg:="?";
    ELSE
      name := SelectText(p, base+1);
      IF p.stack[base+2]=NIL THEN arg:="?";
      ELSE
	arg:=SelectText(p, base+2);
      END;
    END;
    RETURN 
      NEW(ObTree.PhraseCommand, location:=SynLocation.NewLineLocation(info),
        set:=ObTree.doCommandSet, name:=name, arg:=arg);
  END BuildPhraseFlag;

  PROCEDURE BuildPhraseHelp(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
  VAR name, arg: TEXT;
  BEGIN
    IF p.stack[base+1]=NIL THEN name:="?"; arg:="?";
    ELSE
      name := SelectText(p, base+1);
      IF p.stack[base+2]=NIL THEN arg:="?";
      ELSE
	arg:=SelectText(p, base+2);
      END;
    END;
    RETURN 
      NEW(ObTree.PhraseCommand, location:=SynLocation.NewLineLocation(info),
        set:=ObLib.helpCommandSet, name:=name, arg:=arg);
  END BuildPhraseHelp;

  PROCEDURE BuildPhraseTerm(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN
	NEW(ObTree.PhraseTerm, location:=SynLocation.NewLineLocation(info),
	  term:=p.stack[base+1], printDepth:=-1);
    END BuildPhraseTerm;

  PROCEDURE BuildPhraseTermDeep(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN
	NEW(ObTree.PhraseTerm, location:=SynLocation.NewLineLocation(info),
	  term:=p.stack[base+1], printDepth:=40);
    END BuildPhraseTermDeep;

  PROCEDURE BuildPhraseTermDepth(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN
	NEW(ObTree.PhraseTerm, location:=SynLocation.NewLineLocation(info),
	  term:=p.stack[base+1],
	  printDepth:=SelectInt(p, base+2));       
    END BuildPhraseTermDepth;

  PROCEDURE BuildTermBinding(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
	NEW(ObTree.TermBinding, location:=SynLocation.NewLineLocation(info),
	  binder:=BuildIdeName(p, base+1), 
	  term:=p.stack[base+2],
	  rest:=p.stack[base+3]);
    END BuildTermBinding;

  PROCEDURE BuildTermBindingSingle(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
	NEW(ObTree.TermBinding, location:=SynLocation.NewLineLocation(info),
	  binder:=BuildIdeName(p, base+1), 
	  term:=p.stack[base+2],
	  rest:=NIL);
    END BuildTermBindingSingle;

  PROCEDURE BuildTermBindingNil(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NIL;
    END BuildTermBindingNil;

  PROCEDURE BuildTermIde(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermIde, location:=SynLocation.NewLineLocation(info),
	name:=BuildIdeName(p, base+1), place:=NIL);
    END BuildTermIde;

  PROCEDURE BuildTermOk(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermOk, location:=SynLocation.NewLineLocation(info));
    END BuildTermOk;

  PROCEDURE BuildTermBoolTrue(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermBool, location:=SynLocation.NewLineLocation(info),
	bool:=TRUE);
    END BuildTermBoolTrue;

  PROCEDURE BuildTermBoolFalse(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermBool, location:=SynLocation.NewLineLocation(info),
	bool:=FALSE);
    END BuildTermBoolFalse;

  PROCEDURE BuildTermChar(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermChar, location:=SynLocation.NewLineLocation(info),
	char:=Text.GetChar(SelectText(p, base+1),0));
    END BuildTermChar;

  PROCEDURE BuildTermText(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermText, location:=SynLocation.NewLineLocation(info),
	text:=SelectText(p, base+1));
    END BuildTermText;

  PROCEDURE BuildTermInt(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermInt, location:=SynLocation.NewLineLocation(info),
	int:=SelectInt(p, base+1));
    END BuildTermInt;

  PROCEDURE BuildTermReal(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermReal, location:=SynLocation.NewLineLocation(info),
	real:=SelectReal(p, base+1));
    END BuildTermReal;

  PROCEDURE BuildTermArray(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermArray, location:=SynLocation.NewLineLocation(info),
	elems:=p.stack[base+1]);
    END BuildTermArray;

  PROCEDURE BuildTermOption(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermOption, location:=SynLocation.NewLineLocation(info),
	tag := BuildIdeName(p, base+1), term :=p.stack[base+2]);
    END BuildTermOption;

  PROCEDURE BuildTermAlias(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermAlias, location:=SynLocation.NewLineLocation(info),
	label := BuildIdeName(p, base+1), term :=p.stack[base+2]);
    END BuildTermAlias;

  PROCEDURE BuildTermOp(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
    VAR pkg: ObTree.IdeName;
    BEGIN
      TYPECASE p.stack[base+1] OF
      | ObTree.TermIde(node) => pkg:=node.name;
      ELSE 
        SynScan.SyntaxMsg(p.Scanner(), "Identifier expected before '_'","");
        RAISE SynParse.Fail;
      END;
      RETURN NEW(ObTree.TermOp, location:=SynLocation.NewLineLocation(info),
        pkg:=pkg,
        op:=BuildIdeName(p, base+2),
	args:=p.stack[base+3],
	(* the rest is setup in Scope.Term *)
	argsNo:=0, package:=NIL, opCode:=NIL);
    END BuildTermOp;

  PROCEDURE BuildTermOpConst(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
    VAR pkg: ObTree.IdeName;
    BEGIN
      TYPECASE p.stack[base+1] OF
      | ObTree.TermIde(node) => pkg:=node.name;
      ELSE 
        SynScan.SyntaxMsg(p.Scanner(), "Identifier expected before '_'","");
        RAISE SynParse.Fail;
      END;
      RETURN NEW(ObTree.TermOp, location:=SynLocation.NewLineLocation(info),
        pkg:=pkg,
        op:=BuildIdeName(p, base+2),
	args:=NIL, argsNo:=-1,
	(* the rest is setup in Scope.Term *)
	package:=NIL, opCode:=NIL);
    END BuildTermOpConst;

  PROCEDURE BuildIdeListNil(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NIL;
    END BuildIdeListNil;

  PROCEDURE BuildIdeListSingle(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.IdeList, location:=SynLocation.NewLineLocation(info),
        first:=BuildIdeName(p, base+1), rest:=NIL);
    END BuildIdeListSingle;

  PROCEDURE BuildIdeListCons(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.IdeList, location:=SynLocation.NewLineLocation(info),
        first:=BuildIdeName(p, base+1),
        rest:=p.stack[base+2]);
    END BuildIdeListCons;

  PROCEDURE BuildTermListNil(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NIL;
    END BuildTermListNil;

  PROCEDURE BuildTermListSingle(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermList, location:=SynLocation.NewLineLocation(info),
        first:=p.stack[base+1], rest:=NIL);
    END BuildTermListSingle;

  PROCEDURE BuildTermListCons(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermList, location:=SynLocation.NewLineLocation(info),
        first:=p.stack[base+1],
        rest:=p.stack[base+2]);
    END BuildTermListCons;

 PROCEDURE BuildTermProc(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermFun,location:=SynLocation.NewLineLocation(info),
	  binders:=p.stack[base+1],
	  bindersNo:=-1,
	  body:=p.stack[base+2],
	  globals:=NIL,
	  globalsNo:=-1);
    END BuildTermProc;

  PROCEDURE BuildTermAppl(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
    VAR fun: ObTree.Term; args: ObTree.TermList; 
      loc: SynLocation.T; pkgName: TEXT;
    BEGIN
      fun := p.stack[base+1];
      args := p.stack[base+2];
      loc := SynLocation.NewLineLocation(info);
      TYPECASE fun OF
      | ObTree.TermIde(ide) =>
          CASE ObLib.LookupFixity(ide.name.text, ObLib.libraries, 
                                  (*out*)pkgName) OF
          | ObLib.OpFixity.Undefined, ObLib.OpFixity.Qualified =>
              RETURN NEW(ObTree.TermAppl, location:=loc, fun:=fun, args:=args);
          | ObLib.OpFixity.Prefix, ObLib.OpFixity.Infix => 
            RETURN
              NEW(ObTree.TermOp, location:=loc,
                pkg:=NEW(ObTree.IdeName, text:=pkgName, variant:=0),
                op:=ide.name, args:=args, package:=NIL, opCode:=NIL);
          END;
      ELSE RETURN NEW(ObTree.TermAppl, location:=loc, fun:=fun, args:=args);
      END;
    END BuildTermAppl;

  PROCEDURE BuildTermInfix(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
    VAR opName: ObTree.IdeName; pkgName: TEXT; args: ObTree.TermList; 
      loc: SynLocation.T;
    BEGIN
      opName := BuildIdeName(p, base+2);
      loc := SynLocation.NewLineLocation(info);
      args :=
        NEW(ObTree.TermList, location:=loc,
           first:=p.stack[base+1],
           rest:=
             NEW(ObTree.TermList, location:=loc,
                 first:=p.stack[base+3],
                 rest:=NIL));
      CASE ObLib.LookupFixity(opName.text, ObLib.libraries, (*out*)pkgName) OF
      | ObLib.OpFixity.Infix, ObLib.OpFixity.Prefix(*will give an error leater*) =>
        RETURN
          NEW(ObTree.TermOp, location:=loc,
            pkg:=NEW(ObTree.IdeName, text:=pkgName, variant:=0),
            op:=opName, args:=args,
	    package:=NIL, opCode:=NIL);
      | ObLib.OpFixity.Undefined, ObLib.OpFixity.Qualified =>
        RETURN 
          NEW(ObTree.TermAppl, location:=loc,
	    fun:=NEW(ObTree.TermIde, location:=loc, name:=opName, place:=NIL),
	    args:=args);
      END;
    END BuildTermInfix;

  PROCEDURE BuildTermSeq(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(ObTree.TermSeq, location:=SynLocation.NewLineLocation(info),
	before:=p.stack[base+1], 
	after:=p.stack[base+2]);
    END BuildTermSeq;

 PROCEDURE BuildTermObj(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail} =
    VAR protected: BOOLEAN; serialized: ObTree.Sync;
    BEGIN
      protected := NARROW(p.stack[base+1], BoolTemp).bool;
      IF NARROW(p.stack[base+2], BoolTemp).bool THEN
        serialized := ObTree.Sync.Monitored;
      ELSE serialized := ObTree.Sync.None;
      END;
      RETURN 
        NEW(ObTree.TermObj, location:=SynLocation.NewLineLocation(info),
	  protected := protected, 
          sync := serialized,
          fields:=p.stack[base+3]);
    END BuildTermObj;

  TYPE BoolTemp = 
    SynParse.Tree BRANDED OBJECT bool: BOOLEAN END;

  PROCEDURE BuildOptionYes(self: SynParse.Action; p: SynParse.T; 
      base:INTEGER; READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(BoolTemp, location:=NIL, bool:=TRUE);
    END BuildOptionYes;

  PROCEDURE BuildOptionNo(self: SynParse.Action; p: SynParse.T; 
      base:INTEGER; READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NEW(BoolTemp, location:=NIL, bool:=FALSE);
    END BuildOptionNo;

 PROCEDURE BuildTermObjFieldNil(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NIL;
    END BuildTermObjFieldNil;

 PROCEDURE BuildTermObjFieldSingle(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermObjFields,location:=SynLocation.NewLineLocation(info),
          label:=BuildIdeName(p, base+1),
          term:=p.stack[base+2],
	  rest:=NIL);
    END BuildTermObjFieldSingle;

 PROCEDURE BuildTermObjField(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermObjFields,location:=SynLocation.NewLineLocation(info),
          label:=BuildIdeName(p, base+1),
          term:=p.stack[base+2],
	  rest:=p.stack[base+3]);
    END BuildTermObjField;

 PROCEDURE BuildTermMeth(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermMeth,location:=SynLocation.NewLineLocation(info),
	  binders:=p.stack[base+1],
	  bindersNo := -1,
	  body:=p.stack[base+2],
	  globals := NIL,
	  globalsNo := -1);
    END BuildTermMeth;

 PROCEDURE BuildTermClone(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermClone,location:=SynLocation.NewLineLocation(info),
	  objs:=p.stack[base+1]);
    END BuildTermClone;

 PROCEDURE BuildTermRedirect(self: SynParse.Action; p: SynParse.T;
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermRedirect,location:=SynLocation.NewLineLocation(info),
	  obj:=p.stack[base+1], toObj:=p.stack[base+2]);
    END BuildTermRedirect;

 PROCEDURE BuildTermSelect(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermSelect,location:=SynLocation.NewLineLocation(info),
	  obj:=p.stack[base+1],
	  label:=BuildIdeName(p, base+2), labelIndexHint:=-1,
	  invoke:=FALSE, argsNo:=0, args:=NIL);
    END BuildTermSelect;

 PROCEDURE BuildTermInvoke(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermSelect,location:=SynLocation.NewLineLocation(info),
	  obj:=p.stack[base+1],
	  label:=BuildIdeName(p, base+2), labelIndexHint:=-1,
	  invoke:=TRUE, argsNo:=0, args:=p.stack[base+3]);
    END BuildTermInvoke;

 PROCEDURE BuildTermUpdate(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermUpdate,location:=SynLocation.NewLineLocation(info),
	  obj:=p.stack[base+1],
	  label:=BuildIdeName(p, base+2), labelIndexHint:=-1,
	  term:=p.stack[base+3]);
    END BuildTermUpdate;

 PROCEDURE BuildTermArrayGet(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    VAR loc: SynLocation.T;
    BEGIN
      loc := SynLocation.NewLineLocation(info);
      RETURN 
        NEW(ObTree.TermOp,  location:=loc,
          pkg:=NEW(ObTree.IdeName, text:="array", variant:=0),
          op:=NEW(ObTree.IdeName, text:="get", variant:=0),
          args:=NEW(ObTree.TermList, location:=loc,
            first:=p.stack[base+1],
            rest:=NEW(ObTree.TermList, location:=loc,
              first:=p.stack[base+2],
              rest:=NIL)),
	(* the rest is setup in Scope.Term *)
	package:=NIL, opCode:=NIL);
    END BuildTermArrayGet;

 PROCEDURE BuildTermArraySub(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    VAR loc: SynLocation.T;
    BEGIN
      loc := SynLocation.NewLineLocation(info);
      RETURN 
        NEW(ObTree.TermOp,  location:=loc,
          pkg:=NEW(ObTree.IdeName, text:="array", variant:=0),
          op:=NEW(ObTree.IdeName, text:="sub", variant:=0),
          args:=NEW(ObTree.TermList, location:=loc,
            first:=p.stack[base+1],
            rest:=NEW(ObTree.TermList, location:=loc,
              first:=p.stack[base+2],
              rest:=NEW(ObTree.TermList, location:=loc,
                first:=p.stack[base+3],
                rest:=NIL))),
	(* the rest is setup in Scope.Term *)
	package:=NIL, opCode:=NIL);
    END BuildTermArraySub;

 PROCEDURE BuildTermArraySet(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    VAR loc: SynLocation.T;
    BEGIN
      loc := SynLocation.NewLineLocation(info);
      RETURN 
        NEW(ObTree.TermOp,  location:=loc,
          pkg:=NEW(ObTree.IdeName, text:="array", variant:=0),
          op:=NEW(ObTree.IdeName, text:="set", variant:=0),
          args:=NEW(ObTree.TermList, location:=loc,
            first:=p.stack[base+1],
            rest:=NEW(ObTree.TermList, location:=loc,
              first:=p.stack[base+2],
              rest:=NEW(ObTree.TermList, location:=loc,
                first:=p.stack[base+3],
                rest:=NIL))),
	(* the rest is setup in Scope.Term *)
	package:=NIL, opCode:=NIL);
    END BuildTermArraySet;

 PROCEDURE BuildTermArrayUpd(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    VAR loc: SynLocation.T;
    BEGIN
      loc := SynLocation.NewLineLocation(info);
      RETURN 
        NEW(ObTree.TermOp,  location:=loc,
          pkg:=NEW(ObTree.IdeName, text:="array", variant:=0),
          op:=NEW(ObTree.IdeName, text:="upd", variant:=0),
          args:=NEW(ObTree.TermList, location:=loc,
            first:=p.stack[base+1],
            rest:=NEW(ObTree.TermList, location:=loc,
              first:=p.stack[base+2],
              rest:=NEW(ObTree.TermList, location:=loc,
                first:=p.stack[base+3],
                rest:=NEW(ObTree.TermList, location:=loc,
                  first:=p.stack[base+4],
                rest:=NIL)))),
	(* the rest is setup in Scope.Term *)
	package:=NIL, opCode:=NIL);
    END BuildTermArrayUpd;

  PROCEDURE BuildTermMinus(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    VAR loc: SynLocation.T;
    BEGIN
      loc := SynLocation.NewLineLocation(info);
      RETURN 
        NEW(ObTree.TermOp,  location:=loc,
          pkg:=NEW(ObTree.IdeName, text:="real", variant:=0),
          op:=NEW(ObTree.IdeName, text:="minus", variant:=0),
          args:=NEW(ObTree.TermList, location:=loc,
            first:=p.stack[base+1],
            rest:=NIL),
	(* the rest is setup in Scope.Term *)
	package:=NIL, opCode:=NIL);
    END BuildTermMinus;

 PROCEDURE BuildTermLet(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermLet,location:=SynLocation.NewLineLocation(info),
	  var:=FALSE, rec:=FALSE,
	  binding:=p.stack[base+1]);
    END BuildTermLet;

 PROCEDURE BuildTermVar(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermLet,location:=SynLocation.NewLineLocation(info),
	  var:=TRUE, rec:=FALSE,
	  binding:=p.stack[base+1]);
    END BuildTermVar;

 PROCEDURE BuildTermLetRec(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermLet,location:=SynLocation.NewLineLocation(info),
	  var:=FALSE, rec:=TRUE,
	  binding:=p.stack[base+1]);
    END BuildTermLetRec;

 PROCEDURE BuildTermVarRec(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermLet,location:=SynLocation.NewLineLocation(info),
	  var:=TRUE, rec:=TRUE,
	  binding:=p.stack[base+1]);
    END BuildTermVarRec;

 PROCEDURE BuildTermAssign(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree RAISES {SynParse.Fail}  =
    VAR name: ObTree.IdeName;
    BEGIN
      TYPECASE p.stack[base+1] OF
      | ObTree.TermIde(node) => name:=node.name;
      ELSE 
        SynScan.SyntaxMsg(p.Scanner(), "Identifier expected before ':='","");
        RAISE SynParse.Fail;
      END;
      RETURN 
        NEW(ObTree.TermAssign,location:=SynLocation.NewLineLocation(info),
	  name:=name, place:=NIL, val:=p.stack[base+2]);
    END BuildTermAssign;

  PROCEDURE BuildTermIf(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermIf,location:=SynLocation.NewLineLocation(info),
	  test:=p.stack[base+1],
	  ifTrue:=p.stack[base+2],
	  ifFalse:=p.stack[base+3]);
    END BuildTermIf;

  PROCEDURE BuildTermIfEnd(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NIL;
    END BuildTermIfEnd;

  PROCEDURE BuildTermAndif(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermIf,location:=SynLocation.NewLineLocation(info),
	  test:=p.stack[base+1],
	  ifTrue:=p.stack[base+2],
	  ifFalse:= 
              NEW(ObTree.TermBool, location:=SynLocation.NewLineLocation(info),
                  bool:=FALSE));
    END BuildTermAndif;

  PROCEDURE BuildTermOrif(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermIf,location:=SynLocation.NewLineLocation(info),
	  test:=p.stack[base+1],
	  ifTrue:=
              NEW(ObTree.TermBool, location:=SynLocation.NewLineLocation(info),
                  bool:=TRUE),
	  ifFalse:=p.stack[base+2]);
    END BuildTermOrif;

  PROCEDURE BuildTermCase(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermCase, location:=SynLocation.NewLineLocation(info),
	  option:=p.stack[base+1],
	  caseList:=p.stack[base+2]);
    END BuildTermCase;

 PROCEDURE BuildTermLoop(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermLoop,location:=SynLocation.NewLineLocation(info),
	  loop:=p.stack[base+1]);
    END BuildTermLoop;

 PROCEDURE BuildTermExit(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermExit,location:=SynLocation.NewLineLocation(info));
    END BuildTermExit;

  PROCEDURE BuildTermFor(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN
        NEW(ObTree.TermFor, location:=SynLocation.NewLineLocation(info),
          binder := BuildIdeName(p, base+1), lb := p.stack[base+2],
          ub := p.stack[base+3], body := p.stack[base+4]);
    END BuildTermFor;

  PROCEDURE BuildTermForeachDo(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN
        NEW(ObTree.TermForeach, location:=SynLocation.NewLineLocation(info),
          binder := BuildIdeName(p, base+1), range := p.stack[base+2],
          body := p.stack[base+3], map:=FALSE);
    END BuildTermForeachDo;

  PROCEDURE BuildTermForeachMap(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN
        NEW(ObTree.TermForeach, location:=SynLocation.NewLineLocation(info),
          binder := BuildIdeName(p, base+1), range := p.stack[base+2],
          body := p.stack[base+3], map:=TRUE);
    END BuildTermForeachMap;

 PROCEDURE BuildTermException(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermException,location:=SynLocation.NewLineLocation(info),
            name:=p.stack[base+1]);
    END BuildTermException;

 PROCEDURE BuildTermRaise(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermRaise,location:=SynLocation.NewLineLocation(info),
          exception:=p.stack[base+1]);
    END BuildTermRaise;

 PROCEDURE BuildTermTry(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermTry,location:=SynLocation.NewLineLocation(info),
          body:=p.stack[base+1],
          tryList:=p.stack[base+2]);
    END BuildTermTry;

 PROCEDURE BuildTermTryElse(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermTry,location:=SynLocation.NewLineLocation(info),
          body:=p.stack[base+1],
          tryList:=
            NEW(ObTree.TermTryList,location:=SynLocation.NewLineLocation(info),
              exception:=NIL,
              recover:=p.stack[base+2],
              rest:=NIL));
    END BuildTermTryElse;

 PROCEDURE BuildTermTryFinally(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermTryFinally,location:=SynLocation.NewLineLocation(info),
          body:=p.stack[base+1],
          finally:=p.stack[base+2]);
    END BuildTermTryFinally;

 PROCEDURE BuildCaseListCons(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    VAR bind: ObTree.IdeName;
    BEGIN
      IF p.stack[base+2] = NIL THEN bind := NIL;
      ELSE bind := BuildIdeName(p, base+2);
      END;
      RETURN 
        NEW(ObTree.TermCaseList, location:=SynLocation.NewLineLocation(info),
          tag:=BuildIdeName(p, base+1),
          binder:=bind,
          body:=p.stack[base+3],
          rest:=p.stack[base+4]);
    END BuildCaseListCons;

 PROCEDURE BuildCaseListElse(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermCaseList, location:=SynLocation.NewLineLocation(info),
          tag:=NIL, binder:=NIL, body:=p.stack[base+1], rest:=NIL);
    END BuildCaseListElse;

 PROCEDURE BuildCaseListNil(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NIL;
    END BuildCaseListNil;

 PROCEDURE BuildTryListCons(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermTryList,location:=SynLocation.NewLineLocation(info),
          exception:=p.stack[base+1],
          recover:=p.stack[base+2],
          rest:=p.stack[base+3]);
    END BuildTryListCons;

 PROCEDURE BuildTryListConsElse(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermTryList,location:=SynLocation.NewLineLocation(info),
          exception:=p.stack[base+1],
          recover:=p.stack[base+2],
          rest:=
            NEW(ObTree.TermTryList,location:=SynLocation.NewLineLocation(info),
                exception:=NIL,
                recover:=p.stack[base+3],
                rest:=NIL));
    END BuildTryListConsElse;

 PROCEDURE BuildTryListSingle(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermTryList,location:=SynLocation.NewLineLocation(info),
          exception:=p.stack[base+1],
          recover:=p.stack[base+2],
          rest:=NIL);
    END BuildTryListSingle;

 PROCEDURE BuildTryListElse(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN 
        NEW(ObTree.TermTryList,location:=SynLocation.NewLineLocation(info),
          exception:=NIL,
          recover:=p.stack[base+1],
          rest:=NIL);
    END BuildTryListElse;

 PROCEDURE BuildTryListNil(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN NIL;
    END BuildTryListNil;

  PROCEDURE BuildTermLock(self: SynParse.Action; p: SynParse.T; base: INTEGER;
      READONLY info: SynLocation.Info): SynParse.Tree  =
    VAR loc: SynLocation.T;
    BEGIN
      loc := SynLocation.NewLineLocation(info);
      RETURN
        NEW(ObTree.TermOp,  location:=loc,
          pkg:=NEW(ObTree.IdeName, text:="thread", variant:=0),
          op:=NEW(ObTree.IdeName, text:="lock", variant:=0),
          args:=NEW(ObTree.TermList, location:=loc,
            first:=p.stack[base+1],
            rest:=NEW(ObTree.TermList, location:=loc,
              first:=
                NEW(ObTree.TermFun,location:=loc,
	          binders:=NIL, bindersNo:=-1,
	          body:=p.stack[base+2],
	          globals:=NIL, globalsNo:=-1),
              rest:=NIL)),
	  (* the rest is setup in Scope.Term *)
	  package:=NIL, opCode:=NIL);
      END BuildTermLock;

  PROCEDURE BuildTermWatch(self: SynParse.Action; p: SynParse.T; 
      base: INTEGER; READONLY info: SynLocation.Info): SynParse.Tree  =
    BEGIN
      RETURN
        NEW(ObTree.TermWatch,location:=SynLocation.NewLineLocation(info),
          condition:=p.stack[base+1],
          guard:=p.stack[base+2]);
      END BuildTermWatch;
      
  PROCEDURE RegisterActions(actions: MetaParser.ActionTable) 
    RAISES {SynParse.Fail} =
  BEGIN
    MetaParser.Register("Select1", Select1, actions);
    MetaParser.Register("Select2", Select2, actions);
    MetaParser.Register("Select3", Select3, actions);
    MetaParser.Register("Select4", Select4, actions);
    MetaParser.Register("Select5", Select5, actions);
    MetaParser.Register("Select6", Select6, actions);
    MetaParser.Register("BuildPhraseEmpty", BuildPhraseEmpty, actions);
    MetaParser.Register("BuildPhraseFlag", BuildPhraseFlag, actions);
    MetaParser.Register("BuildPhraseHelp", BuildPhraseHelp, actions);
    MetaParser.Register("BuildPhraseTerm", BuildPhraseTerm, actions);
    MetaParser.Register("BuildPhraseTermDepth", BuildPhraseTermDepth, actions);
    MetaParser.Register("BuildPhraseTermDeep", BuildPhraseTermDeep, actions);
    MetaParser.Register("BuildTermBinding", BuildTermBinding, actions);
    MetaParser.Register("BuildTermBindingSingle", BuildTermBindingSingle, actions);
    MetaParser.Register("BuildTermBindingNil", BuildTermBindingNil, actions);
    MetaParser.Register("BuildTermIde", BuildTermIde, actions);
    MetaParser.Register("BuildTermOk", BuildTermOk, actions);
    MetaParser.Register("BuildTermBoolTrue", BuildTermBoolTrue, actions);
    MetaParser.Register("BuildTermBoolFalse", BuildTermBoolFalse, actions);
    MetaParser.Register("BuildTermChar", BuildTermChar, actions);
    MetaParser.Register("BuildTermString", BuildTermText, actions);
    MetaParser.Register("BuildTermInt", BuildTermInt, actions);
    MetaParser.Register("BuildTermReal", BuildTermReal, actions);
    MetaParser.Register("BuildTermArray", BuildTermArray, actions);
    MetaParser.Register("BuildTermOption", BuildTermOption, actions);
    MetaParser.Register("BuildTermAlias", BuildTermAlias, actions);
    MetaParser.Register("BuildTermOp", BuildTermOp, actions);
    MetaParser.Register("BuildTermOpConst", BuildTermOpConst, actions);
    MetaParser.Register("BuildTermAppl", BuildTermAppl, actions);
    MetaParser.Register("BuildTermInfix", BuildTermInfix, actions);
    MetaParser.Register("BuildTermSeq", BuildTermSeq, actions);
    MetaParser.Register("BuildTermLet", BuildTermLet, actions);
    MetaParser.Register("BuildTermVar", BuildTermVar, actions);
    MetaParser.Register("BuildTermLetRec", BuildTermLetRec, actions);
    MetaParser.Register("BuildTermVarRec", BuildTermVarRec, actions);
    MetaParser.Register("BuildTermAssign", BuildTermAssign, actions);
    MetaParser.Register("BuildTermIf", BuildTermIf, actions);
    MetaParser.Register("BuildTermIfEnd", BuildTermIfEnd, actions);
    MetaParser.Register("BuildTermAndif", BuildTermAndif, actions);
    MetaParser.Register("BuildTermOrif", BuildTermOrif, actions);
    MetaParser.Register("BuildTermCase", BuildTermCase, actions);
    MetaParser.Register("BuildTermUpdate", BuildTermUpdate, actions);
    MetaParser.Register("BuildTermSelect", BuildTermSelect, actions);
    MetaParser.Register("BuildTermInvoke", BuildTermInvoke, actions);
    MetaParser.Register("BuildTermArrayGet", BuildTermArrayGet, actions);
    MetaParser.Register("BuildTermArraySet", BuildTermArraySet, actions);
    MetaParser.Register("BuildTermArraySub", BuildTermArraySub, actions);
    MetaParser.Register("BuildTermArrayUpd", BuildTermArrayUpd, actions);
    MetaParser.Register("BuildTermMinus", BuildTermMinus, actions);
    MetaParser.Register("BuildTermObj", BuildTermObj, actions);
    MetaParser.Register("BuildOptionYes", BuildOptionYes, actions);
    MetaParser.Register("BuildOptionNo", BuildOptionNo, actions);
    MetaParser.Register("BuildTermObjField", BuildTermObjField, actions);
    MetaParser.Register("BuildTermObjFieldSingle", BuildTermObjFieldSingle, actions);
    MetaParser.Register("BuildTermObjFieldNil", BuildTermObjFieldNil, actions);
    MetaParser.Register("BuildTermClone", BuildTermClone, actions);
    MetaParser.Register("BuildTermRedirect", BuildTermRedirect, actions);
    MetaParser.Register("BuildTermProc", BuildTermProc, actions);
    MetaParser.Register("BuildTermMeth", BuildTermMeth, actions);
    MetaParser.Register("BuildTermLoop", BuildTermLoop, actions);
    MetaParser.Register("BuildTermExit", BuildTermExit, actions);
    MetaParser.Register("BuildTermFor", BuildTermFor, actions);
    MetaParser.Register("BuildTermForeachDo", BuildTermForeachDo, actions);
    MetaParser.Register("BuildTermForeachMap", BuildTermForeachMap, actions);
    MetaParser.Register("BuildTermException", BuildTermException, actions);
    MetaParser.Register("BuildTermRaise", BuildTermRaise, actions);
    MetaParser.Register("BuildTermTry", BuildTermTry, actions);
    MetaParser.Register("BuildTermTryElse", BuildTermTryElse, actions);
    MetaParser.Register("BuildTermTryFinally", BuildTermTryFinally, actions);
    MetaParser.Register("BuildTermLock", BuildTermLock, actions);
    MetaParser.Register("BuildTermWatch", BuildTermWatch, actions);
    MetaParser.Register("BuildCaseListCons", BuildCaseListCons, actions);
    MetaParser.Register("BuildCaseListElse", BuildCaseListElse, actions);
    MetaParser.Register("BuildCaseListNil", BuildCaseListNil, actions);
    MetaParser.Register("BuildTryListCons", BuildTryListCons, actions);
    MetaParser.Register("BuildTryListConsElse", BuildTryListConsElse, actions);
    MetaParser.Register("BuildTryListSingle", BuildTryListSingle, actions);
    MetaParser.Register("BuildTryListElse", BuildTryListElse, actions);
    MetaParser.Register("BuildTryListNil", BuildTryListNil, actions);
    MetaParser.Register("BuildIdeListNil", BuildIdeListNil, actions);
    MetaParser.Register("BuildIdeListSingle", BuildIdeListSingle, actions);
    MetaParser.Register("BuildIdeListCons", BuildIdeListCons, actions);
    MetaParser.Register("BuildTermListNil", BuildTermListNil, actions);
    MetaParser.Register("BuildTermListSingle", BuildTermListSingle, actions);
    MetaParser.Register("BuildTermListCons", BuildTermListCons, actions);
  END RegisterActions;

  PROCEDURE Setup()  =
  BEGIN
  END Setup;

BEGIN
END ObParseTree.
