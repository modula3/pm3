(*  SGML parsing library                                                   *)
(*  Copyright (C) 1997  Michel Dagenais                                    *)
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

(* The nsgmls C++ library may be accessed through m3sgml.cxx as C calls.
   This interface defines all the Modula-3 RECORD and PROCEDURE which
   correspond to the C functions and structures accessed from nsgmls
   and m3sgml.cxx. *)

INTERFACE NSGMLS;

IMPORT Ctypes, Cstring, SGML;

TYPE
  c_bool = Ctypes.unsigned_char;
  (* Some C compilers have a boolean type which appears to correspond to
     unsigned_char. This may be added to Ctypes and properly customized
     per platform. *)

(* Define the type of all the procedure pointers to be passed to m3sgml.cxx *)

  AppInfoFP = PROCEDURE(self: Application; e: AppinfoEvent);
  StartDtdFP = PROCEDURE(self: Application; e: StartDtdEvent);
  EndDtdFP = PROCEDURE(self: Application; e: EndDtdEvent);
  EndPrologFP = PROCEDURE(self: Application; e: EndPrologEvent);
  StartElementFP = PROCEDURE(self: Application; e: StartElementEvent);
  EndElementFP = PROCEDURE(self: Application; e: EndElementEvent);
  DataFP = PROCEDURE(self: Application; e: DataEvent);
  SDataFP = PROCEDURE(self: Application; e: SdataEvent);
  PIFP = PROCEDURE(self: Application; e: PiEvent);
  ExternalDataEntityRefFP = PROCEDURE(self: Application; 
      e: ExternalDataEntityRefEvent);
  SubdocEntityRefFP = PROCEDURE(self: Application; e: SubdocEntityRefEvent);
  NonSgmlCharFP = PROCEDURE(self: Application; e: NonSgmlCharEvent);
  CommentDeclFP = PROCEDURE(self: Application; e: CommentDeclEvent);
  MarkedSectionStartFP = PROCEDURE(self: Application; 
      e: MarkedSectionStartEvent);
  MarkedSectionEndFP = PROCEDURE(self: Application; 
      e: MarkedSectionEndEvent);
  IgnoredCharsFP = PROCEDURE(self: Application; e: IgnoredCharsEvent);
  GeneralEntityFP = PROCEDURE(self: Application; e: GeneralEntityEvent);
  ErrorFP = PROCEDURE(self: Application; e: ErrorEvent);
  OpenEntityChangeFP = PROCEDURE(self: Application);

(* The C++ objects are treated as opaque untraced references *)

  ParserEventGeneratorKit = UNTRACED BRANDED REF RECORD END;

  EventGenerator = UNTRACED BRANDED REF RECORD END;

  SGMLApplication = UNTRACED BRANDED REF RECORD END;

(* The Modula-3 object to be called back during the C++ SGML parsing. *)

  Application = SGML.Application;

(* The nsgmls library defines a large number of C structures to convey
   all the information needed to initialize the parser, and generated
   by the parser thereafter. SGML is, to say the least, flexible, and
   thus somewhat complex. *)

  Option = {ShowOpenEntities, ShowOpenElements, OutputCommentDecls,
      OutputMarkedSections, OutputGeneralEntities, MapCatalogDocument};

  OptionWithArg = { AddCatalog, IncludeParam, EnableWarning, AddSearchDir,
      ActivateLink, Architecture };

  Char = Ctypes.unsigned_char;

  Position = Ctypes.unsigned_long;

  CharString = RECORD 
      ptr: Ctypes.char_star;
      len: Cstring.size_t;
    END;

  ExternalId = RECORD
      haveSystemId: c_bool;
      havePublicId: c_bool;
      haveGeneratedSystemId: c_bool;
      systemId: CharString;
      publicId: CharString;
      generatedSystemId: CharString;
    END;

  Notation = RECORD
      name: CharString;
      externalId: ExternalId;
    END;

  EntityDataType = SGML.EntityDataType;

  EntityDeclType = SGML.EntityDeclType;

  Entity = RECORD
      name: CharString;
      dataType: EntityDataType;
      declType: EntityDeclType;
      isInternal: c_bool;
      (* Following valid if isInternal is true *)
      text: CharString;
      (* Following valid if isInternal is false *)
      externalId: ExternalId;
      nAttributes: Cstring.size_t;
      attributes: UNTRACED REF Attribute;
      notation: Notation;
    END;

  AttributeType = SGML.AttributeType;

  AttributeDefaulted = SGML.AttributeDefaulted;

  AttributeCdataChunk = RECORD
      isSdata: c_bool;
      isNonSgml: c_bool;
      nonSgmlChar: Char;
      data: CharString;
      entityName: CharString;
    END;

  Attribute = RECORD
      name: CharString;
      type: CARDINAL; (* AttributeType *)
      defaulted: CARDINAL; (* AttributeDefaulted *)
      nCdataChunks: Cstring.size_t;
      cdataChunks: UNTRACED REF AttributeCdataChunk;
      tokens: CharString;
      isId: c_bool;
      isGroup: c_bool;
      nEntities: Cstring.size_t;
      entities: UNTRACED REF Entity;
      notation: Notation;
    END;

  PiEvent = UNTRACED REF RECORD
      pos: Position;
      data: CharString;
      entityName: CharString;
    END;

  ElementContentType = SGML.ElementContentType;

  StartElementEvent = UNTRACED REF RECORD
      pos: Position;
      gi: CharString;
      contentType: CARDINAL; (* ElementContentType *)
      included: c_bool;
      nAttributes: Cstring.size_t;
      attributes: UNTRACED REF Attribute;
    END;
      
  EndElementEvent = UNTRACED REF RECORD
      pos: Position;
      gi: CharString;
    END;

  DataEvent = UNTRACED REF RECORD
      pos: Position;
      data: CharString;
    END;

  SdataEvent = UNTRACED REF RECORD
      pos: Position;
      text: CharString;
      entityName: CharString;
    END;

  ExternalDataEntityRefEvent = UNTRACED REF RECORD
      pos: Position;
      entity: Entity;
    END;

  SubdocEntityRefEvent = UNTRACED REF RECORD
      pos: Position;
      entity: Entity;
    END;

  NonSgmlCharEvent = UNTRACED REF RECORD
      pos: Position;
      c: Char;
    END;

  ErrorType = SGML.ErrorType;

  ErrorEvent = UNTRACED REF RECORD
      pos: Position;
      type: CARDINAL; (* ErrorType *)
      message: CharString;
    END;

  AppinfoEvent = UNTRACED REF RECORD
      pos: Position;
      none: c_bool;
      string: CharString;
    END;

  StartDtdEvent = UNTRACED REF RECORD
      pos: Position;
      name: CharString;
      haveExternalId: c_bool;
      externalId: ExternalId;
    END;

  EndDtdEvent = UNTRACED REF RECORD
      pos: Position;
      name: CharString;
    END;

  EndPrologEvent = UNTRACED REF RECORD
      pos: Position;
    END;

  GeneralEntityEvent = UNTRACED REF RECORD
      entity: Entity;
    END;

  CommentDeclEvent = UNTRACED REF RECORD
      pos: Position;
      nComments: Cstring.size_t;
      comments: UNTRACED REF CharString;
      seps: UNTRACED REF CharString;
    END;

  MarkedSectionStatus = SGML.MarkedSectionStatus;

  MarkedSectionParamType = SGML.MarkedSectionParamType;

  MarkedSectionParam = RECORD
      type: CARDINAL; (* MarkedSectionParamType *)
      entityName: CharString;
    END;

  MarkedSectionStartEvent = UNTRACED REF RECORD
      pos: Position;
      status: CARDINAL; (* MarkedSectionStatus *)
      nParams: Cstring.size_t;
      params: UNTRACED REF MarkedSectionParam;
    END;

  MarkedSectionEndEvent = UNTRACED REF RECORD
      pos: Position;
      status: MarkedSectionStatus;
    END;

  IgnoredCharsEvent = UNTRACED REF RECORD
      pos: Position;
      data: CharString;
    END;

  DetailedLocation = RECORD
      lineNumber: Ctypes.unsigned_long;
      columnNumber: Ctypes.unsigned_long;
      byteOffset: Ctypes.unsigned_long;
      entityOffset: Ctypes.unsigned_long;
      entityName: CharString;
      filename: CharString;
    END;

(* C procedures needed to access nsgmls and m3sgml. *)

(* Initialize the C pointers to Modula-3 procedures *)

<*EXTERNAL*>
PROCEDURE SetupM3SGMLProcedures(fp1: AppInfoFP; fp2: StartDtdFP; fp3: EndDtdFP;
    fp4: EndPrologFP; fp5: StartElementFP; fp6: EndElementFP;
    fp7: DataFP; fp8: SDataFP; fp9: PIFP; fp10: ExternalDataEntityRefFP;
    fp11: SubdocEntityRefFP; fp12: NonSgmlCharFP; fp13: CommentDeclFP;
    fp14: MarkedSectionStartFP; fp15: MarkedSectionEndFP; fp16: IgnoredCharsFP;
    fp17: GeneralEntityFP; fp18: ErrorFP; fp19: OpenEntityChangeFP);

(* Creation/Deletion procedures and access to methods for the SGMLApplication,
   EventGenerator and EventGeneratorKit C++ objects. *)

<*EXTERNAL*>
PROCEDURE CreateM3SGMLApplication(self: Application): SGMLApplication;

<*EXTERNAL*>
PROCEDURE DeleteM3SGMLApplication(sa: SGMLApplication);

<*EXTERNAL*>
PROCEDURE DeleteM3SGMLEventGenerator(eg: EventGenerator);

<*EXTERNAL*>
PROCEDURE RunM3SGMLEventGenerator(eg: EventGenerator; sa: SGMLApplication;
    a: Application):
    Ctypes.unsigned_int;

<*EXTERNAL*>
PROCEDURE InhibitMessagesM3SGMLEventGenerator(eg: EventGenerator; 
    b: c_bool);

<*EXTERNAL*>
PROCEDURE HaltM3SGMLEventGenerator(eg: EventGenerator);

<*EXTERNAL*>
PROCEDURE MakeSubDocM3SGMLEventGenerator(eg: EventGenerator;
    systemId: Ctypes.char_star; systemIdLength: Cstring.size_t): 
    EventGenerator;

<*EXTERNAL*>
PROCEDURE CreateM3SGMLParserEventGeneratorKit(): ParserEventGeneratorKit;

<*EXTERNAL*>
PROCEDURE DeleteM3SGMLParserEventGeneratorKit(pe: ParserEventGeneratorKit);

<*EXTERNAL*>
PROCEDURE SetOptionM3SGML(pe: ParserEventGeneratorKit; option: CARDINAL
    (* Option *));

<*EXTERNAL*>
PROCEDURE SetProgramNameM3SGML(pe: ParserEventGeneratorKit; 
    name: Ctypes.char_star);

<*EXTERNAL*>
PROCEDURE SetOptionWithArgM3SGML(pe: ParserEventGeneratorKit;
    option: CARDINAL (* OptionWithArg*); value: Ctypes.char_star);

<*EXTERNAL*>
PROCEDURE CreateM3SGMLEventGenerator(pe: ParserEventGeneratorKit;
    nFiles: Ctypes.int; files: Ctypes.char_star_star): EventGenerator;

<*EXTERNAL*>
PROCEDURE EntityPtrLocateM3SGMLPosition(sa: SGMLApplication;
    position: Position; dl: UNTRACED REF DetailedLocation);

END NSGMLS.
