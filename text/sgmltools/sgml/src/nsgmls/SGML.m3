(*  SGML parser library                                                    *)
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


UNSAFE MODULE SGML;

IMPORT Rd, NSGMLS, WeakRef, Ctypes, M3toC, Text, TextF, Cstring;

REVEAL

  Parser = ParserPublic BRANDED OBJECT
      parser: NSGMLS.EventGenerator := NIL;
      parserKit: NSGMLS.ParserEventGeneratorKit := NIL;
    OVERRIDES
      init := InitParser;
      run := RunParser;
      halt := HaltParser;
      inhibitMessages := InhibitMessages;
      subdocumentParser := SubdocumentParser;
    END;

  Application = ApplicationPublic BRANDED OBJECT
      peer: NSGMLS.SGMLApplication := NIL;
    OVERRIDES
      getDetailedLocation := GetDetailedLocation;
      init := ApplicationInit;
    END;

TYPE
  c_bool = NSGMLS.c_bool;

(* The nsgmls library has an EventGeneratorKit and an EventGenerator
   class. Both of these are wrapped in the Parser Modula-3 object.
   It is simpler but the ability to generate several parsers
   while keeping the same options is lost. *)

EXCEPTION Unimplemented;

PROCEDURE InitParser(self: Parser; options: ParserOptions; programName: TEXT; 
    files: REF ARRAY OF TEXT; rds: REF ARRAY OF Rd.T := NIL): Parser =
  VAR
    nfiles: Ctypes.int := NUMBER(files^);
    theFiles := NEW(REF ARRAY OF Ctypes.char_star,NUMBER(files^));
    (* Array of files as needed by the C function *)
  <*FATAL Unimplemented*>
  BEGIN
    IF rds # NIL THEN RAISE Unimplemented; END;

    IF self.parser = NIL THEN
      (* First time around, register for cleanup *)
      EVAL WeakRef.FromRef(self,CleanUpParser);
    ELSE
      (* just cleanup before continuing *)
      NSGMLS.DeleteM3SGMLEventGenerator(self.parser);
      NSGMLS.DeleteM3SGMLParserEventGeneratorKit(self.parserKit);
    END;
    self.parserKit := NSGMLS.CreateM3SGMLParserEventGeneratorKit();

    (* Set all the specified options *)

    IF options.showOpenEntities THEN
      NSGMLS.SetOptionM3SGML(self.parserKit,
          ORD(NSGMLS.Option.ShowOpenEntities));
    END;

    IF options.showOpenElements THEN
      NSGMLS.SetOptionM3SGML(self.parserKit,
          ORD(NSGMLS.Option.ShowOpenElements));
    END;

    IF options.outputCommentDecls THEN
      NSGMLS.SetOptionM3SGML(self.parserKit,
          ORD(NSGMLS.Option.OutputCommentDecls));
    END;

    IF options.outputMarkedSections THEN
      NSGMLS.SetOptionM3SGML(self.parserKit,
          ORD(NSGMLS.Option.OutputMarkedSections));
    END;

    IF options.outputGeneralEntities THEN
      NSGMLS.SetOptionM3SGML(self.parserKit,
          ORD(NSGMLS.Option.OutputGeneralEntities));
    END;

    IF options.mapCatalogDocument THEN
      NSGMLS.SetOptionM3SGML(self.parserKit,
          ORD(NSGMLS.Option.MapCatalogDocument));
    END;

    (* Set all the options expecting an argument *)

    SendOptionWithArg(self.parserKit,options.addCatalog,
        NSGMLS.OptionWithArg.AddCatalog);

    SendOptionWithArg(self.parserKit,options.includeParam,
        NSGMLS.OptionWithArg.IncludeParam);

    SendOptionWithArg(self.parserKit,options.enableWarning,
        NSGMLS.OptionWithArg.EnableWarning);

    SendOptionWithArg(self.parserKit,options.addSearchDir,
        NSGMLS.OptionWithArg.AddSearchDir);

    SendOptionWithArg(self.parserKit,options.activateLink,
        NSGMLS.OptionWithArg.ActivateLink);

    SendOptionWithArg(self.parserKit,options.architecture,
        NSGMLS.OptionWithArg.Architecture);

    NSGMLS.SetProgramNameM3SGML(self.parserKit,M3toC.TtoS(programName));

    FOR i := 0 TO LAST(files^) DO
      theFiles[i] := M3toC.TtoS(files[i]);
    END;

    (* Create the EventGenerator *)

    self.parser := NSGMLS.CreateM3SGMLEventGenerator(self.parserKit,
        nfiles, ADR(theFiles[0]));
    RETURN self;
  END InitParser;

(* When the Modula-3 Parser object is garbage collected, insure that the
   associated EventGenerator and EventGeneratorKit objects are deleted *)

PROCEDURE CleanUpParser(<*UNUSED*>READONLY w: WeakRef.T; r: REFANY) =
  VAR
    self: Parser := r;
  BEGIN
    NSGMLS.DeleteM3SGMLEventGenerator(self.parser);
    NSGMLS.DeleteM3SGMLParserEventGeneratorKit(self.parserKit);
  END CleanUpParser;

PROCEDURE SendOptionWithArg(parserKit: NSGMLS.ParserEventGeneratorKit; 
    array: REF ARRAY OF TEXT; option: NSGMLS.OptionWithArg) =
  BEGIN
    IF array = NIL THEN RETURN; END;

    FOR i := 0 TO LAST(array^) DO
      NSGMLS.SetOptionWithArgM3SGML(parserKit,ORD(option),
          M3toC.TtoS(array[i]));
    END;
  END SendOptionWithArg;

(* These Modula-3 methods just relay the arguments to the underlying C++
   object. *)

PROCEDURE RunParser(self: Parser; a: Application): CARDINAL =
  VAR
    nbErrors: CARDINAL;
  BEGIN
    nbErrors := NSGMLS.RunM3SGMLEventGenerator(self.parser, a.peer, a);
    RETURN nbErrors;
  END RunParser;

PROCEDURE HaltParser(self: Parser) =
  BEGIN
    NSGMLS.HaltM3SGMLEventGenerator(self.parser);
  END HaltParser;

PROCEDURE InhibitMessages(self: Parser; inhibit: BOOLEAN) =
  VAR
    b: c_bool;
  BEGIN
    IF inhibit THEN b := 1; ELSE b := 0; END;
    NSGMLS.InhibitMessagesM3SGMLEventGenerator(self.parser,b);
  END InhibitMessages;

(* This is a different way to create a parser. Insure that the C++
   EventGenerator object gets deleted when this parser is collected. *)

PROCEDURE SubdocumentParser(self: Parser; systemId: TEXT): Parser =
  VAR
    parser := NEW(Parser);
    systemIdS := M3toC.TtoS(systemId);
    systemIdLength := Text.Length(systemId);
  BEGIN
    parser.parser := NSGMLS.MakeSubDocM3SGMLEventGenerator(self.parser,
        systemIdS, systemIdLength);
    EVAL WeakRef.FromRef(parser,CleanUpSubParser);
    RETURN parser;
  END SubdocumentParser;

PROCEDURE CleanUpSubParser(<*UNUSED*>READONLY w: WeakRef.T; r: REFANY) =
  VAR
    self: Parser := r;
  BEGIN
    NSGMLS.DeleteM3SGMLEventGenerator(self.parser);
  END CleanUpSubParser;

(* Create an associated C++ object for the parser Application object.
   Insure that it gets deleted when it gets garbage collected. *)

PROCEDURE ApplicationInit(self: Application): Application =
  VAR
  BEGIN
    IF self.peer = NIL THEN
      EVAL WeakRef.FromRef(self,CleanUpApplication);
    ELSE
      NSGMLS.DeleteM3SGMLApplication(self.peer);
    END;
    self.peer := NSGMLS.CreateM3SGMLApplication(self);
    RETURN self;
  END ApplicationInit;

PROCEDURE CleanUpApplication(<*UNUSED*>READONLY w: WeakRef.T; r: REFANY) =
  VAR
    self: Application := r;
  BEGIN
    NSGMLS.DeleteM3SGMLApplication(self.peer);
  END CleanUpApplication;

(* Each method of the SGMLApplication C++ peer object calls a corresponding 
   Modula-3 procedure. The procedure converts the "event" structure
   containing the relevant information and calls the Application Modula-3
   object. In each structure, booleans are extracted from 0/#0 in C,
   enumerations are extracted from C integers, TEXT are extracted from
   character strings, REF ARRAY are extracted from C pointers to structures.
   Booleans indicating if REF values (TEXT or REF ARRAY) are available 
   are removed since it is indicated by a NIL REF. *)

PROCEDURE AppInfoProc(self: Application; e: NSGMLS.AppinfoEvent) =
  VAR
    ne: AppinfoEvent;
  BEGIN
    ne.pos := e.pos;
    IF e.none # 0 THEN ne.string := NIL;
    ELSE ne.string := CopyStringToT(e.string);
    END;
    self.appInfo(ne);
  END AppInfoProc;

PROCEDURE StartDtdProc(self: Application; e: NSGMLS.StartDtdEvent) =
  VAR
    ne: StartDtdEvent;
  BEGIN
    ne.pos := e.pos;
    ne.name := CopyStringToT(e.name);
    ne.externalId.systemId := NIL;
    ne.externalId.publicId := NIL;
    ne.externalId.generatedSystemId := NIL;
    IF e.haveExternalId # 0 THEN
      IF e.externalId.haveSystemId # 0 THEN
        ne.externalId.systemId := CopyStringToT(e.externalId.systemId);
      END;

      IF e.externalId.havePublicId # 0 THEN
        ne.externalId.publicId := CopyStringToT(e.externalId.publicId);
      END;

      IF e.externalId.haveGeneratedSystemId # 0 THEN
        ne.externalId.generatedSystemId := 
            CopyStringToT(e.externalId.generatedSystemId);
      END;
    END;
    self.startDtd(ne);
  END StartDtdProc;

PROCEDURE EndDtdProc(self: Application; e: NSGMLS.EndDtdEvent) =
  VAR
    ne: EndDtdEvent;
  BEGIN
    ne.pos := e.pos;
    ne.name := CopyStringToT(e.name);
    self.endDtd(ne);
  END EndDtdProc;

PROCEDURE EndPrologProc(self: Application; e: NSGMLS.EndPrologEvent) =
  VAR
    ne: EndPrologEvent;
  BEGIN
    ne.pos := e.pos;
    self.endProlog(ne);
  END EndPrologProc;

PROCEDURE StartElementProc(self: Application; e: NSGMLS.StartElementEvent) =
  VAR
    ne: StartElementEvent;
  BEGIN
    ne.pos := e.pos;
    ne.gi := CopyStringToT(e.gi);
    ne.contentType := VAL(e.contentType,ElementContentType);
    ne.included := e.included # 0;
    ne.attributes := CopyAttributes(e.nAttributes,e.attributes);
    self.startElement(ne);
  END StartElementProc;

PROCEDURE EndElementProc(self: Application; e: NSGMLS.EndElementEvent) =
  VAR
    ne: EndElementEvent;
  BEGIN
    ne.pos := e.pos;
    ne.gi := CopyStringToT(e.gi);
    self.endElement(ne);
  END EndElementProc;

PROCEDURE DataProc(self: Application; e: NSGMLS.DataEvent) =
  VAR
    ne: DataEvent;
  BEGIN
    ne.pos := e.pos;
    ne.data := CopyStringToT(e.data);
    self.data(ne);
  END DataProc;

PROCEDURE SDataProc(self: Application; e: NSGMLS.SdataEvent) =
  VAR
    ne: SdataEvent;
  BEGIN
    ne.pos := e.pos;
    ne.text := CopyStringToT(e.text);
    ne.entityName := CopyStringToT(e.entityName);
    self.sdata(ne);
  END SDataProc;

PROCEDURE PIProc(self: Application; e: NSGMLS.PiEvent) =
  VAR
    ne: PiEvent;
  BEGIN
    ne.pos := e.pos;
    ne.data := CopyStringToT(e.data);
    ne.entityName := CopyStringToT(e.entityName);
    self.pi(ne);
  END PIProc;

PROCEDURE ExternalDataEntityRefProc(self: Application; 
      e: NSGMLS.ExternalDataEntityRefEvent) =
  VAR
    ne: ExternalDataEntityRefEvent;
  BEGIN
    ne.pos := e.pos;
    CopyEntity(ne.entity, e.entity);
    self.externalDataEntityRef(ne);
  END ExternalDataEntityRefProc;

PROCEDURE SubdocEntityRefProc(self: Application; 
    e: NSGMLS.SubdocEntityRefEvent) =
  VAR
    ne: SubdocEntityRefEvent;
  BEGIN
    ne.pos := e.pos;
    CopyEntity(ne.entity, e.entity);
    self.subdocEntityRef(ne);
  END SubdocEntityRefProc;

PROCEDURE NonSgmlCharProc(self: Application; e: NSGMLS.NonSgmlCharEvent) =
  VAR
    ne: NonSgmlCharEvent;
  BEGIN
    ne.pos := e.pos;
    ne.c := VAL(e.c,CHAR);
    self.nonSgmlChar(ne);
  END NonSgmlCharProc;

PROCEDURE CommentDeclProc(self: Application; e: NSGMLS.CommentDeclEvent) =
  VAR
    ne: CommentDeclEvent;
  BEGIN
    ne.pos := e.pos;
    ne.comments := CopyStringArray(e.nComments, e.comments);
    ne.seps := CopyStringArray(e.nComments, e.seps);
    self.commentDecl(ne);
  END CommentDeclProc;

PROCEDURE MarkedSectionStartProc(self: Application; 
      e: NSGMLS.MarkedSectionStartEvent) =
  VAR
    ne: MarkedSectionStartEvent;
  BEGIN
    ne.pos := e.pos;
    ne.status := VAL(e.status, MarkedSectionStatus);
    ne.params := CopyParams(e.nParams, e.params);
    self.markedSectionStart(ne);
  END MarkedSectionStartProc;

PROCEDURE MarkedSectionEndProc(self: Application; 
      e: NSGMLS.MarkedSectionEndEvent) =
  VAR
    ne: MarkedSectionEndEvent;
  BEGIN
    ne.pos := e.pos;
    ne.status := e.status;
    self.markedSectionEnd(ne);
  END MarkedSectionEndProc;

PROCEDURE IgnoredCharsProc(self: Application; e: NSGMLS.IgnoredCharsEvent) =
  VAR
    ne: IgnoredCharsEvent;
  BEGIN
    ne.pos := e.pos;
    ne.data := CopyStringToT(e.data);
    self.ignoredChars(ne);
  END IgnoredCharsProc;

PROCEDURE GeneralEntityProc(self: Application; e: NSGMLS.GeneralEntityEvent) =
  VAR
    ne: GeneralEntityEvent;
  BEGIN
    CopyEntity(ne.entity, e.entity);
    self.generalEntity(ne);
  END GeneralEntityProc;

PROCEDURE ErrorProc(self: Application; e: NSGMLS.ErrorEvent) =
  VAR
    ne: ErrorEvent;
  BEGIN
    ne.pos := e.pos;
    ne.type := VAL(e.type, ErrorType);
    ne.message := CopyStringToT(e.message);
    self.error(ne);
  END ErrorProc;

PROCEDURE OpenEntityChangeProc(self: Application) =
  BEGIN
    self.openEntityChange();
  END OpenEntityChangeProc;

PROCEDURE CopyAttributes(n: CARDINAL; array: UNTRACED REF
    NSGMLS.Attribute): REF ARRAY OF Attribute =
  VAR
    newArray := NEW(REF ARRAY OF Attribute,n);
  BEGIN
    FOR i := 0 TO LAST(newArray^) DO
      newArray[i].name := CopyStringToT(array.name);
      newArray[i].type := VAL(array.type, AttributeType);
      IF newArray[i].type = AttributeType.CData OR
          newArray[i].type = AttributeType.Tokenized THEN
        newArray[i].defaulted := VAL(array.defaulted, AttributeDefaulted);
        IF newArray[i].type = AttributeType.CData THEN
          newArray[i].cdataChunks := CopyDataChunks(array.nCdataChunks,
              array.cdataChunks);
          newArray[i].tokens := NIL;
          newArray[i].isId := FALSE;
          newArray[i].isGroup := FALSE;
          newArray[i].entities := NIL;
          newArray[i].notation.name := NIL;
        ELSE
          newArray[i].tokens := CopyStringToT(array.tokens);
          newArray[i].isId := array.isId # 0;
          newArray[i].isGroup := array.isGroup # 0;
          newArray[i].entities := CopyEntities(array.nEntities,
              array.entities);
          newArray[i].notation.name := CopyStringToT(array.notation.name);
          IF Text.Length(newArray[i].notation.name) > 0 THEN
            CopyExternalId(newArray[i].notation.externalId,
                array.notation.externalId);
          END;
          newArray[i].cdataChunks := NIL;
        END;
      END;
      array := array + ADRSIZE(array^);
    END;
    RETURN newArray;
  END CopyAttributes;

PROCEDURE CopyDataChunk(VAR ne: CdataChunk; 
    READONLY e: NSGMLS.AttributeCdataChunk) =
  BEGIN
    ne.data := NIL;
    ne.entityName := NIL;

    IF e.isSdata # 0 THEN ne.entityName := CopyStringToT(e.entityName); END;

    IF e.isNonSgml # 0 THEN ne.nonSgmlChar := VAL(e.nonSgmlChar,CHAR);
    ELSE ne.data := CopyStringToT(e.data);
    END;
  END CopyDataChunk;

PROCEDURE CopyDataChunks(n: CARDINAL; array: UNTRACED REF 
    NSGMLS.AttributeCdataChunk): REF ARRAY OF CdataChunk =
  VAR
    newArray := NEW(REF ARRAY OF CdataChunk, n);
  BEGIN
    FOR i := 0 TO LAST(newArray^) DO
      CopyDataChunk(newArray[i],array^);
      array := array + ADRSIZE(array^);
    END;
    RETURN newArray;
  END CopyDataChunks;

PROCEDURE CopyEntity(VAR ne: Entity; READONLY e: NSGMLS.Entity) =
  BEGIN
    ne.name := CopyStringToT(e.name);
    ne.dataType := e.dataType;
    ne.declType := e.declType;

    IF e.isInternal # 0 THEN 
      ne.internalText := CopyStringToT(e.text); 
      ne.externalId.systemId := NIL;
      ne.externalId.publicId := NIL;
      ne.externalId.generatedSystemId := NIL;
      ne.attributes := NIL;
      ne.notation.name := NIL;
    ELSE 
      CopyExternalId(ne.externalId,e.externalId);
      ne.internalText := NIL;
      ne.attributes := CopyAttributes(e.nAttributes,e.attributes);
      ne.notation.name := CopyStringToT(e.notation.name);
    END;
    CopyExternalId(ne.notation.externalId,e.notation.externalId);
  END CopyEntity;

PROCEDURE CopyEntities(n: CARDINAL; array: UNTRACED REF NSGMLS.Entity): 
    REF ARRAY OF Entity =
  VAR
    newArray := NEW(REF ARRAY OF Entity, n);
  BEGIN
    FOR i := 0 TO LAST(newArray^) DO
      CopyEntity(newArray[i],array^);
      array := array + ADRSIZE(array^);
    END;
    RETURN newArray;
  END CopyEntities;

PROCEDURE CopyExternalId(VAR ne: ExternalId; READONLY e: NSGMLS.ExternalId) =
  BEGIN
    IF e.haveSystemId  # 0 THEN
      ne.systemId := CopyStringToT(e.systemId);
    ELSE
      ne.systemId := NIL;
    END;

    IF e.havePublicId # 0 THEN
      ne.publicId := CopyStringToT(e.publicId);
    ELSE
      ne.publicId := NIL;
    END;

    IF e.haveGeneratedSystemId # 0 THEN
      ne.generatedSystemId := CopyStringToT(e.generatedSystemId);
    ELSE
      ne.generatedSystemId := NIL;
    END;
  END CopyExternalId;

PROCEDURE CopyStringArray(n: CARDINAL; array: UNTRACED REF NSGMLS.CharString):
      REF ARRAY OF TEXT =
  VAR
    newArray := NEW(REF ARRAY OF TEXT, n);
  BEGIN
    FOR i := 0 TO LAST(newArray^) DO
      newArray[i] := CopyStringToT(array^);
      array := array + ADRSIZE(array^);
    END;
    RETURN newArray;
  END CopyStringArray;

PROCEDURE CopyParams(n: CARDINAL; array: UNTRACED REF 
      NSGMLS.MarkedSectionParam): REF ARRAY OF MarkedSectionParam =
  VAR
    newArray := NEW(REF ARRAY OF MarkedSectionParam, n);
  BEGIN
    FOR i := 0 TO LAST(newArray^) DO
      newArray[i].type := VAL(array.type, MarkedSectionParamType);
      newArray[i].entityName := CopyStringToT(array.entityName);
      array := array + ADRSIZE(array^);
    END;
    RETURN newArray;
  END CopyParams;

PROCEDURE CopyStringToT(s: NSGMLS.CharString): TEXT =
  VAR 
    t := NEW (TEXT, s.len + 1);
  BEGIN
    EVAL Cstring.memcpy (ADR (t[0]), s.ptr, s.len);
    t[s.len] := '\000';
    RETURN t;
  END CopyStringToT;

PROCEDURE GetDetailedLocation(self: Application; pos: Position): 
    DetailedLocation =
  VAR
    l: NSGMLS.DetailedLocation;
    nl: DetailedLocation;
    p: NSGMLS.Position := pos;
  BEGIN
    NSGMLS.EntityPtrLocateM3SGMLPosition(self.peer,p,ADR(l));
    nl.lineNumber := l.lineNumber;
    nl.columnNumber := l.columnNumber;
    nl.byteOffset := l.byteOffset;
    nl.entityOffset := l.entityOffset;
    nl.entityName := CopyStringToT(l.entityName);
    nl.filename := CopyStringToT(l.filename);
    RETURN nl;
  END GetDetailedLocation;

(* Register the pointers to the Modula-3 procedures to be called by
   the C++ library when calling back to signify parsing events. *)

BEGIN
  NSGMLS.SetupM3SGMLProcedures(AppInfoProc, StartDtdProc, EndDtdProc, 
    EndPrologProc, StartElementProc, EndElementProc, 
    DataProc, SDataProc, PIProc, ExternalDataEntityRefProc,
    SubdocEntityRefProc, NonSgmlCharProc, CommentDeclProc,
    MarkedSectionStartProc, MarkedSectionEndProc, IgnoredCharsProc,
    GeneralEntityProc, ErrorProc, OpenEntityChangeProc);
END SGML.
