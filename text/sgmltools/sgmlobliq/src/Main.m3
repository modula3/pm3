(*  SGML obliq interpreter                                                 *)
(*  Copyright (C) 1997 Michel Dagenais                                     *)
(*                                                                         *)
(*  This library is free software; you can redistribute it and/or          *)
(*  modify it under the terms of the GNU Library General Public            *)
(*  License as published by the Free Software Foundation; either           *)
(*  version 2 of the License, or (at your option) any later version.       *)
(*                                                                         *)
(*  This library is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      *)
(*  Library General Public License for more details.                       *)
(*                                                                         *)
(*  You should have received a copy of the GNU Library General Public      *)
(*  License along with this library; if not, write to the Free             *)
(*  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.     *)
(*                                                                         *)
(*  For more information on this program, contact Michel Dagenais at       *)
(*  dagenais@vlsi.polymtl.ca or Electrical Eng. Dept., Ecole Polytechnique *)
(*  P.O. Box 6079, Station A, Montreal, Quebec, Canada, H3C 3A7.           *)

MODULE Main;
IMPORT ObliqOnline;

IMPORT ObLib, ObLibM3, ObLibM3Help, Text, SynWr, SynLocation,
    ObCommand, ObValue, SGML, TextRd, Bundle, SGMLBundle, ObLibTbl, NetObj,
    AtomList, Atom;

(* Obliq objects are defined for the Database objects and commands *)

TYPE 
  OpCodes = ARRAY OF ObLib.OpCode;

  SGMLCode = {Error, Parse};

  SGMLOpCode = ObLib.OpCode OBJECT
        code: SGMLCode;
      END;
    
  PackageSGML = ObLib.T OBJECT
      OVERRIDES
        Eval:=EvalSGML;
      END;

(* Support for Help messages *)

CONST 
  Greetings = 
      "  obliq-sgml (obliq with sgml parser) (say \'help;\' for help)";
  HelpSummary =
      "  sgml                (the built-in sgml parser)\n";
  HelpText = 
      "  parse(catalogs, parameters, searchdirs, files: [TEXT]; cl: Cl);\n" &
      "    where cl is an object with the following methods being\n" &
      "    called back during the parsing:\n" &
      "      appInfo(string: TEXT);\n" &
      "      startDtd(name: TEXT; externalId: ExternalId);\n" &
      "      endDtd(name: TEXT);\n" &
      "      endProlog();\n" &
      "      startElement(name: TEXT; type: INTEGER; included: BOOLEAN; \n" &
      "          attributes: [Attribute]);\n" &
      "      endElement(name: TEXT);\n" &
      "      data(string: TEXT);\n" &
      "      sdata(string, entityName: TEXT);\n" &
      "      pi(string, entityName: TEXT);\n" &
      "      externalDataEntityRef(entity: Entity);\n" &
      "      subdocEntityRef(entity: Entity);\n" &
      "      nonSgmlChar(c: CHAR);\n" &
      "      commentDecl(comments, seps: ARRAY OF TEXT);\n" &
      "      markedSectionStart(status: INTEGER; types: [INTEGER]; \n" &
      "          entityNames: [TEXT]);\n" &
      "      markedSectionEnd(status: INTEGER);\n" &
      "      ignoredChars(string: TEXT);\n" &
      "      generalEntity(entity: Entity);\n" &
      "      error(type, line, column, entityOffset: INTEGER; entityName, \n" &
      "          filename, message: TEXT);\n" &
      "\n" &
      "    Entity, ExternalId, DataChunk and Attribute are objects with \n" &
      "    the same fields as their Modula-3 counterpart in SGML.i3. \n" &
      "    Constants are defined for dataType, declType, attributeType, \n" &
      "    attributeDefaulted, contentType, errorType, sectionStatus,\n" &
      "    and sectionType. They use the enumerated types as suffix\n" &
      "    (e.g. dataTypeSgml, dataTypeCData... dataTypePi, \n" &
      "    declTypeGeneral...).\n";

VAR sgmlException: ObValue.ValException;

PROCEDURE HelpSGML(self: ObCommand.T; arg: TEXT; 
      <*UNUSED*>data: REFANY:=NIL)  =
  BEGIN
    IF Text.Equal(arg, "!") THEN
      SynWr.Text(SynWr.out,HelpSummary);
    ELSIF Text.Equal(arg, "?") THEN
      SynWr.Text(SynWr.out, HelpText);
      SynWr.NewLine(SynWr.out);
    ELSE
      SynWr.Text(SynWr.out, "Command " & self.name & ": bad argument: " & arg);
      SynWr.NewLine(SynWr.out);
    END;
  END HelpSGML;

PROCEDURE NewSGMLOC(name: TEXT; arity: INTEGER; code: SGMLCode)
      : SGMLOpCode =
  BEGIN
    RETURN NEW(SGMLOpCode, name:=name, arity:=arity, code:=code);
  END NewSGMLOC;

(* Define and register the new commands in the db package *)

PROCEDURE PackageSetup() =
  VAR opCodes: REF OpCodes;
  BEGIN
    opCodes := NEW(REF OpCodes, NUMBER(SGMLCode));
    opCodes^ :=
      OpCodes{
        NewSGMLOC("failure", -1, SGMLCode.Error),
        NewSGMLOC("parse", 5, SGMLCode.Parse)
      };
    ObLib.Register(
      NEW(PackageSGML, name:="sgml", opCodes:=opCodes));
    sgmlException := NEW(ObValue.ValException, name:="sgml_failure");
    ObLib.RegisterHelp("sgml", HelpSGML);
  END PackageSetup;

(* For each database command, check the arguments, call the
   relevant procedure and wrap the result in Obliq objects. *)

PROCEDURE EvalSGML(self: PackageSGML; opCode: ObLib.OpCode; 
    <*UNUSED*>arity: ObLib.OpArity; READONLY args: ObValue.ArgArray; 
    <*UNUSED*>temp: BOOLEAN; loc: SynLocation.T)
    : ObValue.Val RAISES {ObValue.Error, ObValue.Exception} =
  VAR
    array1, array2, array3, array4: ObValue.RemArray;
    obj1: ObValue.RemObj;
    options: SGML.ParserOptions;
    parser: SGML.Parser;
    files: REF ARRAY OF TEXT;
    app: T;
  BEGIN
    CASE NARROW(opCode, SGMLOpCode).code OF
    | SGMLCode.Error => 
        RETURN sgmlException;

    | SGMLCode.Parse =>
        TYPECASE args[1] OF | ObValue.ValArray(node) => array1:=node.remote;
        ELSE ObValue.BadArgType(1, "array", self.name, opCode.name, loc); END;
        TYPECASE args[2] OF | ObValue.ValArray(node) => array2:=node.remote;
        ELSE ObValue.BadArgType(2, "array", self.name, opCode.name, loc); END;
        TYPECASE args[3] OF | ObValue.ValArray(node) => array3:=node.remote;
        ELSE ObValue.BadArgType(3, "array", self.name, opCode.name, loc); END;
        TYPECASE args[4] OF | ObValue.ValArray(node) => array4:=node.remote;
        ELSE ObValue.BadArgType(4, "array", self.name, opCode.name, loc); END;
        TYPECASE args[5] OF | ObValue.ValObj(node) => obj1:=node.remote;
        ELSE ObValue.BadArgType(5, "object", self.name, opCode.name, loc); END;

        TRY
          options.addCatalog := CopyTextArray(self,loc,opCode,1,array1);
          options.includeParam := CopyTextArray(self,loc,opCode,2,array2);
          options.addSearchDir := CopyTextArray(self,loc,opCode,3,array3);
          files := CopyTextArray(self,loc,opCode,4,array4);
          parser := NEW(SGML.Parser).init(options,"obliq-sgml",files);
          app := NEW(T, peer := obj1, parser := parser).init();
          EVAL parser.run(app);
          IF app.netobj # NIL THEN RAISE NetObj.Error(app.netobj);
          ELSIF app.obError # NIL THEN RAISE ObValue.Error(app.obError);
          ELSIF app.obException # NIL THEN 
            RAISE ObValue.Exception(app.obException);
          ELSIF app.obServerError # NIL THEN 
            RAISE ObValue.ServerError(app.obServerError);
          END;
        EXCEPT
        | NetObj.Error(e) => ObValue.RaiseError(AtomListToText(e),loc);
        | ObValue.ServerError(e) => ObValue.RaiseError(e,loc);
        END;
        RETURN ObValue.valOk;
    ELSE
      ObValue.BadOp(self.name, opCode.name, loc);
      RETURN ObValue.valOk;
    END;
  END EvalSGML;

PROCEDURE AtomListToText(l: AtomList.T): TEXT =
  VAR
    t := "";
  BEGIN
    WHILE l # NIL DO 
      t := t & " " & Atom.ToText(l.head);
      l := l.tail;
    END;
    RETURN t;
  END AtomListToText;

PROCEDURE CopyTextArray(self: PackageSGML; loc: SynLocation.T; 
    opCode: ObLib.OpCode; argRank: CARDINAL; array: ObValue.RemArray): 
    REF ARRAY OF TEXT RAISES{NetObj.Error, ObValue.Error, 
    ObValue.ServerError} =
  VAR
    textArray: REF ARRAY OF TEXT;
    value: ObValue.Val;
  BEGIN
    IF array = NIL OR array.Size() = 0 THEN RETURN NIL; END;
    textArray := NEW(REF ARRAY OF TEXT,array.Size());
    FOR i := 0 TO LAST(textArray^) DO
      value := array.Get(i);
      TYPECASE value OF | ObValue.ValText(node) => textArray[i]:=node.text;
      ELSE ObValue.BadArgType(argRank, "[TEXT]", self.name, opCode.name, loc);
      END;
    END;
    RETURN textArray;
  END CopyTextArray;

TYPE
  T = SGML.Application OBJECT 
      peer: ObValue.RemObj;
      netobj: AtomList.T := NIL;
      obException: ObValue.ExceptionPacket := NIL;
      obError: ObValue.ErrorPacket := NIL;
      obServerError: TEXT := NIL;
      parser: SGML.Parser;
    OVERRIDES
      init := Init;
      appInfo := AppInfo;
      startDtd := StartDtd;
      endDtd := EndDtd;
      endProlog := EndProlog;
      startElement := StartElement;
      endElement := EndElement;
      data := Data;
      sdata := SData;
      pi := Pi;
      externalDataEntityRef := ExternalDataEntityRef;
      subdocEntityRef := SubdocEntityRef;
      nonSgmlChar := NonSgmlChar;
      commentDecl := CommentDecl;
      markedSectionStart := MarkedSectionStart;
      markedSectionEnd := MarkedSectionEnd;
      ignoredChars := IgnoredChars;
      generalEntity := GeneralEntity;
      error := Error;
      openEntityChange := OpenEntityChange;
    END;

VAR
  appInfoHint, startDtdHint, endDtdHint, endPrologHint, startElementHint,
  endElementHint, dataHint, sdataHint, piHint, externalDataEntityRefHint,
  subdocEntityRefHint, nonSgmlCharHint, commentDeclHint, 
  markedSectionStartHint, markedSectionEndHint, ignoredCharsHint,
  generalEntityHint, errorHint: INTEGER := 0;

PROCEDURE AppInfo(self: T; READONLY e: SGML.AppinfoEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.string);
    EVAL Invoke(self,"appInfo",1,args,FALSE,appInfoHint);
  END AppInfo;

PROCEDURE StartDtd(self: T; 
    READONLY e: SGML.StartDtdEvent) =
  VAR
    args: ARRAY [0..1] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.name);
    args[1] := CopyExternalId(e.externalId);
    EVAL Invoke(self,"startDtd",2,args,FALSE,startDtdHint);
  END StartDtd;

PROCEDURE EndDtd(self: T; READONLY e: SGML.EndDtdEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.name);
    EVAL Invoke(self,"endDtd",1,args,FALSE,endDtdHint);
  END EndDtd;

PROCEDURE EndProlog(self: T; <*UNUSED*>READONLY e: SGML.EndPrologEvent) =
  VAR
    args: ARRAY [0..-1] OF ObValue.Val;
  BEGIN
    EVAL Invoke(self,"endProlog",0,args,FALSE,endPrologHint);
  END EndProlog;

PROCEDURE StartElement(self: T; READONLY e: SGML.StartElementEvent) =
  VAR
    args: ARRAY [0..3] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.gi);
    args[1] := NewInteger(ORD(e.contentType));
    args[2] := NewBool(e.included);
    args[3] := CopyAttributes(e.attributes);
    EVAL Invoke(self,"startElement",4,args,FALSE,startElementHint);
  END StartElement;

PROCEDURE EndElement(self: T; READONLY e: SGML.EndElementEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.gi);
    EVAL Invoke(self,"endElement",1,args,FALSE,endElementHint);
  END EndElement;

PROCEDURE Data(self: T; READONLY e: SGML.DataEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.data);
    EVAL Invoke(self,"data",1,args,FALSE,dataHint);
  END Data;

PROCEDURE SData(self: T; READONLY e: SGML.SdataEvent) =
  VAR
    args: ARRAY [0..1] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.text);
    args[1] := NewText(e.entityName);
    EVAL Invoke(self,"sdata",2,args,FALSE,sdataHint);
  END SData;

PROCEDURE Pi(self: T; READONLY e: SGML.PiEvent) =
  VAR
    args: ARRAY [0..1] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.data);
    args[1] := NewText(e.entityName);
    EVAL Invoke(self,"pi",2,args,FALSE,piHint);
  END Pi;

PROCEDURE ExternalDataEntityRef(self: T; 
    READONLY e: SGML.ExternalDataEntityRefEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := CopyEntity(e.entity);
    EVAL Invoke(self,"externalDataEntityRef",1,args,FALSE,
        externalDataEntityRefHint);
  END ExternalDataEntityRef;

PROCEDURE SubdocEntityRef(self: T; READONLY e: SGML.SubdocEntityRefEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := CopyEntity(e.entity);
    EVAL Invoke(self,"subdocEntityRef",1,args,FALSE,
        subdocEntityRefHint);
  END SubdocEntityRef;

PROCEDURE NonSgmlChar(self: T; READONLY e: SGML.NonSgmlCharEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := NewChar(e.c);
    EVAL Invoke(self,"nonSgmlChar",1,args,FALSE,nonSgmlCharHint);
  END NonSgmlChar;

PROCEDURE CommentDecl(self: T; READONLY e: SGML.CommentDeclEvent) =
  VAR
    args: ARRAY [0..1] OF ObValue.Val;
  BEGIN
    args[0] := NewTextArray(e.comments);
    args[1] := NewTextArray(e.seps);
    EVAL Invoke(self,"commentDecl",2,args,FALSE,commentDeclHint);
  END CommentDecl;

PROCEDURE MarkedSectionStart(self: T; 
      READONLY e: SGML.MarkedSectionStartEvent) =
  VAR
    args: ARRAY [0..2] OF ObValue.Val;
    integerArray := NEW(REF ARRAY OF INTEGER, NUMBER(e.params^));
    textArray := NEW(REF ARRAY OF TEXT, NUMBER(e.params^));
  BEGIN
    FOR i := 0 TO LAST(integerArray^) DO
      integerArray[i] := ORD(e.params[i].type);
      textArray[i] := e.params[i].entityName;
    END;

    args[0] := NewInteger(ORD(e.status));
    args[1] := NewIntegerArray(integerArray);
    args[2] := NewTextArray(textArray);
    EVAL Invoke(self,"markedSectionStart",3,args,FALSE,
        markedSectionStartHint);
  END MarkedSectionStart;

PROCEDURE MarkedSectionEnd(self: T; READONLY e: SGML.MarkedSectionEndEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := NewInteger(ORD(e.status));
    EVAL Invoke(self,"markedSectionEnd",1,args,FALSE,
        markedSectionEndHint);
  END MarkedSectionEnd;

PROCEDURE IgnoredChars(self: T; READONLY e: SGML.IgnoredCharsEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := NewText(e.data);
    EVAL Invoke(self,"ignoredChars",1,args,FALSE,ignoredCharsHint);
  END IgnoredChars;

PROCEDURE GeneralEntity(self: T; READONLY e: SGML.GeneralEntityEvent) =
  VAR
    args: ARRAY [0..0] OF ObValue.Val;
  BEGIN
    args[0] := CopyEntity(e.entity);
    EVAL Invoke(self,"generalEntity",1,args,FALSE,generalEntityHint);
  END GeneralEntity;

PROCEDURE Error(self: T; READONLY e: SGML.ErrorEvent) =
  VAR
    position := self.getDetailedLocation(e.pos);
    args: ARRAY [0..6] OF ObValue.Val;
  BEGIN
    args[0] := NewInteger(ORD(e.type));
    args[1] := NewInteger(position.lineNumber);
    args[2] := NewInteger(position.columnNumber);
    args[3] := NewInteger(position.entityOffset);
    args[4] := NewText(position.entityName);
    args[5] := NewText(position.filename);
    args[6] := NewText(e.message);
    EVAL Invoke(self,"error",7,args,FALSE,errorHint);
  END Error;

PROCEDURE OpenEntityChange(<*UNUSED*>self: T) =
  VAR
  BEGIN
  END OpenEntityChange;

PROCEDURE Init(self: T): SGML.Application =
  VAR
  BEGIN
    EVAL SGML.Application.init(self);
    RETURN self;
  END Init;

PROCEDURE Invoke(self: T; label: TEXT; argNo: INTEGER; READONLY args: 
    ObValue.Vals; internal: BOOLEAN; VAR hint: INTEGER): ObValue.Val =
  BEGIN
    TRY
      RETURN self.peer.Invoke(label, argNo, args, internal, hint);
    EXCEPT
    | NetObj.Error(e) => self.netobj := e;
    | ObValue.Error(e) => self.obError := e;
    | ObValue.Exception(e) => self.obException := e;
    | ObValue.ServerError(e) => self.obServerError := e;
    END;
    self.parser.halt();
    RETURN ObValue.valOk;
  END Invoke;

PROCEDURE NewText(t: TEXT): ObValue.Val =
  BEGIN
    IF t = NIL THEN RETURN ObValue.valOk; END;
    RETURN ObValue.NewText(t);
  END NewText;

PROCEDURE NewInteger(i: INTEGER): ObValue.ValInt =
  BEGIN
    RETURN NEW(ObValue.ValInt, int := i);
  END NewInteger;

PROCEDURE NewBool(b: BOOLEAN): ObValue.ValBool =
  BEGIN
    RETURN NEW(ObValue.ValBool, bool := b);
  END NewBool;

PROCEDURE NewChar(c: CHAR): ObValue.ValChar =
  BEGIN
    RETURN NEW(ObValue.ValChar, char := c);
  END NewChar;

PROCEDURE NewTextArray(a: REF ARRAY OF TEXT): ObValue.ValArray =
  VAR
    vals := NEW(REF ARRAY OF ObValue.Val, NUMBER(a^));
  BEGIN
    FOR i := 0 TO LAST(vals^) DO
      vals[i] := NewText(a[i]);
    END;
    RETURN ObValue.NewArrayFromVals(vals);
  END NewTextArray;

PROCEDURE NewIntegerArray(a: REF ARRAY OF INTEGER): ObValue.ValArray =
  VAR
    vals := NEW(REF ARRAY OF ObValue.Val, NUMBER(a^));
  BEGIN
    FOR i := 0 TO LAST(vals^) DO
      vals[i] := NewInteger(a[i]);
    END;
    RETURN ObValue.NewArrayFromVals(vals);
  END NewIntegerArray;

PROCEDURE CopyExternalId(READONLY e: SGML.ExternalId): ObValue.ValObj =
  VAR
    fields := NEW(REF ObValue.ObjFields,3);
  BEGIN
    fields[0].label := "systemId";
    fields[0].field := NewText(e.systemId);
    fields[1].label := "publicId";
    fields[1].field := NewText(e.publicId);
    fields[2].label := "generatedSystemId";
    fields[2].field := NewText(e.generatedSystemId);
    RETURN ObValue.NewObjectFromFields(fields,"",FALSE,NIL);
  END CopyExternalId;

PROCEDURE CopyEntity(READONLY e: SGML.Entity): ObValue.ValObj =
  VAR
    fields := NEW(REF ObValue.ObjFields,7);
  BEGIN
    fields[0].label := "name";
    fields[0].field := NewText(e.name);
    fields[1].label := "dataType";
    fields[1].field := NewInteger(ORD(e.dataType));
    fields[2].label := "declType";
    fields[2].field := NewInteger(ORD(e.declType));
    fields[3].label := "internalText";
    fields[3].field := NewText(e.internalText);
    fields[4].label := "externalId";
    fields[4].field := CopyExternalId(e.externalId);
    fields[5].label := "attributes";
    fields[5].field := CopyAttributes(e.attributes);
    fields[6].label := "notation";
    fields[6].field := CopyNotation(e.notation);
    RETURN ObValue.NewObjectFromFields(fields,"",FALSE,NIL);
  END CopyEntity;

PROCEDURE CopyAttributes(READONLY a: REF ARRAY OF SGML.Attribute): 
    ObValue.Val =
  VAR
    fields := NEW(REF ObValue.ObjFields,9);
    attrs: REF ARRAY OF ObValue.Val;
    chunks: REF ARRAY OF ObValue.Val;
    entities: REF ARRAY OF ObValue.Val;
  BEGIN
    IF a = NIL THEN RETURN ObValue.valOk; END;
    fields[0].label := "name";
    fields[1].label := "type";
    fields[2].label := "defaulted";
    fields[3].label := "cdataChunks";
    fields[4].label := "tokens";
    fields[5].label := "isId";
    fields[6].label := "isGroup";
    fields[7].label := "entities";
    fields[8].label := "notation";
    attrs := NEW(REF ARRAY OF ObValue.Val, NUMBER(a^));
    FOR i := 0 TO LAST(a^) DO
      fields[0].field := NewText(a[i].name);
      fields[1].field := NewInteger(ORD(a[i].type));
      fields[2].field := NewInteger(ORD(a[i].defaulted));
      IF a[i].cdataChunks = NIL THEN
        fields[3].field := ObValue.valOk;
      ELSE
        chunks := NEW(REF ARRAY OF ObValue.Val,NUMBER(a[i].cdataChunks^));
        FOR j := 0 TO LAST(chunks^) DO 
          chunks[j] := CopyDataChunk(a[i].cdataChunks[j]);
        END;
        fields[3].field := ObValue.NewArrayFromVals(chunks);
      END;
      fields[4].field := NewText(a[i].tokens);
      fields[5].field := NewBool(a[i].isId);
      fields[6].field := NewBool(a[i].isGroup);
      IF a[i].entities = NIL THEN
        fields[7].field := ObValue.valOk;
      ELSE
        entities := NEW(REF ARRAY OF ObValue.Val, NUMBER(a[i].entities^));
        FOR j := 0 TO LAST(entities^) DO
          entities[j] := CopyEntity(a[i].entities[j]);
        END;
        fields[7].field := ObValue.NewArrayFromVals(entities);
      END;
      fields[8].field := CopyNotation(a[i].notation);
      attrs[i] := ObValue.NewObject(fields^);
    END;
    RETURN ObValue.NewArrayFromVals(attrs);
  END CopyAttributes;

PROCEDURE CopyNotation(READONLY n: SGML.Notation): ObValue.ValObj =
  VAR
    fields := NEW(REF ObValue.ObjFields,2);
  BEGIN
    fields[0].label := "name";
    fields[0].field := NewText(n.name);
    fields[1].label := "externalId";
    fields[1].field := CopyExternalId(n.externalId);
    RETURN ObValue.NewObjectFromFields(fields,"",FALSE,NIL);
  END CopyNotation;

PROCEDURE CopyDataChunk(READONLY d: SGML.CdataChunk): ObValue.ValObj =
  VAR
    fields := NEW(REF ObValue.ObjFields,3);
  BEGIN
    fields[0].label := "nonSgmlChar";
    fields[0].field := NewChar(d.nonSgmlChar);
    fields[1].label := "data";
    fields[1].field := NewText(d.data);
    fields[2].label := "entityName";
    fields[2].field := NewText(d.entityName);
    RETURN ObValue.NewObjectFromFields(fields,"",FALSE,NIL);
  END CopyDataChunk;

(* TODO: upon error, stop parser, raise obliq error when returning from
   "parse" with such an error. *)

VAR
  interpreter: ObliqOnline.T;
  rd := TextRd.New(Bundle.Get(SGMLBundle.Get(),"startup"));

BEGIN
  ObliqOnline.Setup();
  ObLibM3.PackageSetup();
  ObLibM3Help.Setup();
  ObLibTbl.PackageSetup();
  PackageSetup();

  interpreter := ObliqOnline.New(Greetings);
  ObliqOnline.Interact(interpreter,"sgml-startup",rd,TRUE,TRUE);
  ObliqOnline.Interact(interpreter);
END Main.

