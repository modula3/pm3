MODULE DumpScheme EXPORTS Main;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.12  1998/09/14 08:15:44  roland
    Modified code to remove compiler warnings.

    Revision 1.11  1998/03/18 13:41:27  roland
    New command line option '-local' enables the tool to work with the
    client part of a pool.

    Revision 1.10  1998/01/14 17:50:42  roland
    Dependencies now printed after all attribute definitions for '-b'
    switch.

    Revision 1.9  1997/12/15 16:40:21  roland
    New man pages and unified command lines options.

    Revision 1.8  1997/11/21 15:18:17  roland
    Bugfix: Node types do not need to have a super class.

    Revision 1.7  1997/11/10 10:02:54  roland
    Bugfix: classes were visited more than once when using option -b.

    Revision 1.6  1997/07/21 10:57:41  roland
    Adapted to new set implementation.

    Revision 1.5  1997/05/05 10:56:58  roland
    Adapted to HiGRAS.

    Revision 1.4  1997/03/25 17:08:16  roland
    GrasServerId-Parameter added.

    Revision 1.3  1997/02/28 14:28:22  roland
    Bugfix: use TypeGraphSystem instead of ChgMgmtGraphSystem

    Revision 1.2  1997/02/28 13:30:23  roland
    Both scheme tools now use GrasParams to parse GRAS3 command-line
    parameters.

    Revision 1.1  1997/02/27 15:55:34  roland
    This program opens a GRAS scheme and prints its contents to stdout.

*)
(***************************************************************************)

(* Print a text form of a scheme graph *)

IMPORT ParseParams, IO, Fmt, Text, Process, Stdio, Pathname;
IMPORT Scheme, TypedGraphSystem, Access, ErrorSupport, AttributeValue,
       PageFile, TypedGraphPool, NodeSet;
IMPORT GrasParams;

VAR
  rootPath, schemeName: TEXT;
  poolname            : TEXT;
  nameserver, serverid: TEXT;
  cachesize           : CARDINAL;
  scheme              : Scheme.T;
  pool                : TypedGraphPool.T;
  forBuildScheme      : BOOLEAN          := FALSE;
  local               : BOOLEAN          := FALSE;

PROCEDURE ReadParams () =
  CONST USAGE = "<pool> <scheme> [-b] [-local] -root <rootPath>";

  PROCEDURE ParseError (prog, err: TEXT) =
    BEGIN
      IO.Put(err & "\n");
      IO.Put(prog & USAGE & "\n");
      Process.Exit(1);
    END ParseError;
  VAR valid: BOOLEAN;
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        poolname := pp.getNext();
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Pool name is mandatory first parameter");
      END;
      IF Text.Equal("-root", poolname) THEN
        ParseError(Pathname.Last(pp.arg[0]),
                   "Pool name is mandatory first parameter");
      END;

      TRY
        schemeName := pp.getNext();
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Scheme name is mandatory second parameter");
      END;
      IF Text.Equal("-root", schemeName) THEN
        ParseError(Pathname.Last(pp.arg[0]),
                   "Scheme name is mandatory second parameter");
      END;

      IF pp.keywordPresent("-b") THEN forBuildScheme := TRUE END;
      IF pp.keywordPresent("-local") THEN local := TRUE END;

      GrasParams.ParseComandLine(
        rootPath, valid, cachesize, serverid, nameserver);
      IF NOT valid THEN
        ParseError(Pathname.Last(pp.arg[0]), "Need root path.");
      END;
    END;
  END ReadParams;

PROCEDURE ErrorAbort () =
  BEGIN
    TRY pool.abortTransaction(); EXCEPT ELSE END;
  END ErrorAbort;

PROCEDURE IDToText (id: Scheme.ID): TEXT =
  BEGIN
    RETURN "(" & Fmt.Int(id.graph) & ", " & Fmt.Int(id.entity) & ")";
  END IDToText;

PROCEDURE CardToText (c: Scheme.Cardinality): TEXT =
  VAR max: TEXT;
  BEGIN
    IF c.max = LAST(CARDINAL) THEN
      max := "n"
    ELSE
      max := Fmt.Int(c.max)
    END;
    RETURN "[" & Fmt.Int(c.min) & ":" & max & "]";
  END CardToText;

PROCEDURE CardToTextForBuildScheme (c: Scheme.Cardinality): TEXT =
  VAR max: TEXT;
  BEGIN
    IF c.max = LAST(CARDINAL) THEN
      max := "n"
    ELSE
      max := Fmt.Int(c.max)
    END;
    RETURN Fmt.Int(c.min) & " " & max;
  END CardToTextForBuildScheme;

PROCEDURE KindToText (kind: Scheme.AttributeKind): TEXT =
  BEGIN
    CASE kind OF
      Scheme.AttributeKind.Intrinsic => RETURN "intrinsic"
    | Scheme.AttributeKind.Derived => RETURN "derived"
    | Scheme.AttributeKind.Dummy => RETURN "dummy"
    | Scheme.AttributeKind.Meta => RETURN "meta"
    END;
  END KindToText;

PROCEDURE IndexPropsToText (prop: Scheme.IndexProperties): TEXT =
  BEGIN
    CASE prop OF
      Scheme.IndexProperties.Normal => RETURN "normal"
    | Scheme.IndexProperties.Index => RETURN "index"
    | Scheme.IndexProperties.Key => RETURN "key"
    END;
  END IndexPropsToText;

PROCEDURE TypeCodeToText (type: CARDINAL): TEXT =
  BEGIN
    CASE type OF
      AttributeValue.DummyTypeCode => RETURN "dummy"
    | AttributeValue.BooleanTypeCode => RETURN "boolean"
    | AttributeValue.IntegerTypeCode => RETURN "integer"
    | AttributeValue.CardinalTypeCode => RETURN "cardinal"
    | AttributeValue.RealTypeCode => RETURN "real"
    | AttributeValue.CharTypeCode => RETURN "char"
    | AttributeValue.TextTypeCode => RETURN "text"
    ELSE
      RETURN "type " & Fmt.Int(type);
    END;
  END TypeCodeToText;


PROCEDURE DumpAttributes (scheme: Scheme.T; CoT: Scheme.ID)
  RAISES {Scheme.InternalError, Scheme.NotDeclared} =
  VAR
    attributes, deps                      : Scheme.IDSet;
    attr, defCOT                          : Scheme.ID;
    len                                   : CARDINAL;
    kind                                  : Scheme.AttributeKind;
    indexP                                : Scheme.IndexProperties;
    type                                  : CARDINAL;
    card                                  : Scheme.Cardinality;
    constLen, found                       : BOOLEAN;
    name                                  : TEXT;
    dep, depOnA, depA, depOnC, depC, etype: Scheme.ID;
    dfound                                : BOOLEAN;
    depKind                               : Scheme.DependencyKind;
  BEGIN
    attributes := scheme.getAllAttributesOfNodeClassOrType(CoT);
    IF attributes.isEmpty() THEN
      IO.Put("  no attributes\n");
    ELSE
      attributes.loop();
      attr := attributes.get(found);
      WHILE found DO
        scheme.getAttributeNameAndClass(attr, defCOT, name);
        IF defCOT # CoT THEN
          (* this class/type only inherits attr *)
          IO.Put("  attribute " & name & " " & IDToText(attr)
                   & " inherited from class " & IDToText(defCOT) & "\n");
        ELSE
          IO.Put("  attribute " & name & " " & IDToText(attr));
          scheme.showAttributeProps(
            attr, kind, indexP, type, card, constLen, len);
          IO.Put(" " & KindToText(kind) & " " & IndexPropsToText(indexP)
                   & " " & TypeCodeToText(type) & " " & CardToText(card));
          IF constLen THEN
            IO.Put(" length " & Fmt.Int(len) & "\n");
          ELSE
            IO.Put(" variable length\n");
          END;
        END;
        (* print evaluation function *)
        name := scheme.getEvaluationFunction(CoT, attr);
        IF name # NIL THEN
          IO.Put("    evaluation function " & name & "\n");
        END;
        (* print dependencies *)
        deps := scheme.getDefiningDependencies(CoT, attr);
        IF deps.isEmpty() THEN
          IO.Put("    no dependencies\n");
        ELSE
          IO.Put("    depends on\n");
          deps.loop();
          dep := deps.get(dfound);
          WHILE dfound DO
            scheme.showDependencyInfo(
              dep, depA, depOnA, depC, depOnC, depKind, etype);
            IO.Put("      attribute " & IDToText(depOnA));
            IF depKind = Scheme.DependencyKind.Incoming THEN
              IO.Put(" via incoming edge of type " & IDToText(etype));
            ELSIF depKind = Scheme.DependencyKind.Outgoing THEN
              IO.Put(" via outgoing edge of type " & IDToText(etype));
            END;
            IO.Put("\n");
            dep := deps.get(dfound);
          END;
        END;
        attr := attributes.get(found);
      END;
    END;
  END DumpAttributes;

PROCEDURE DumpAttributesForBuildScheme (scheme    : Scheme.T;
                                        attributes: Scheme.IDSet)
  RAISES {Scheme.InternalError, Scheme.NotDeclared} =
  VAR
    deps, subclasses            : Scheme.IDSet;
    attr, defCOT                : Scheme.ID;
    len                         : CARDINAL;
    kind                        : Scheme.AttributeKind;
    indexP                      : Scheme.IndexProperties;
    type                        : CARDINAL;
    subc                        : Scheme.ID;
    card                        : Scheme.Cardinality;
    constLen, found             : BOOLEAN;
    evalname, attrname, cname   : TEXT;
    defattrname, defcname, ename: TEXT;
    dep, depOnA, depA           : Scheme.ID;
    depOnC, depC, etype         : Scheme.ID;
    dfound, sfound              : BOOLEAN;
    depKind                     : Scheme.DependencyKind;
  BEGIN
    (* we perform two loops over the found attributes to ensure every
       attribute is defined before it is refernced in a dependency *)
    attributes.loop();
    attr := attributes.get(found);
    WHILE found DO
      scheme.getAttributeNameAndClass(attr, defCOT, attrname);
      IF scheme.existsNodeClassByNumber(defCOT) THEN
        cname := scheme.getNodeClassName(defCOT);
      ELSE
        cname := scheme.getNodeTypeName(defCOT);
      END;
      IO.Put("attribute " & cname & " " & attrname);
      scheme.showAttributeProps(
        attr, kind, indexP, type, card, constLen, len);
      IO.Put(
        " " & KindToText(kind) & " " & IndexPropsToText(indexP) & " "
          & Fmt.Int(type) & " " & CardToTextForBuildScheme(card) & " ");
      IF constLen THEN
        IO.Put(" TRUE " & Fmt.Int(len) & "\n");
      ELSE
        IO.Put(" FALSE 0\n");
      END;
      attr := attributes.get(found);
    END;

    attributes.loop();
    attr := attributes.get(found);
    WHILE found DO
      scheme.getAttributeNameAndClass(attr, defCOT, attrname);
      IF scheme.existsNodeClassByNumber(defCOT) THEN
        (* get all subclasses *)
        subclasses := scheme.getAllSubClasses(defCOT);
        subclasses.union(scheme.getAllNodeTypesOfClass(defCOT));
      ELSE
        subclasses := NodeSet.New();
      END;
      subclasses.insert(defCOT);

      subclasses.loop();
      subc := subclasses.get(sfound);
      WHILE sfound DO
        IF scheme.existsNodeClassByNumber(subc) THEN
          cname := scheme.getNodeClassName(subc);
        ELSE
          cname := scheme.getNodeTypeName(subc);
        END;
        (* print evaluation functions *)
        IF scheme.definesEvaluationFunction(subc, attr) THEN
          evalname := scheme.getEvaluationFunction(subc, attr);
          IO.Put("eval " & cname & " " & attrname & " " & evalname & "\n");
        END;
        (* print dependencies *)
        IF scheme.definesDependency(subc, attr) THEN
          deps := scheme.getDefiningDependencies(subc, attr);
          deps.loop();
          dep := deps.get(dfound);
          WHILE dfound DO
            scheme.showDependencyInfo(
              dep, depA, depOnA, depC, depOnC, depKind, etype);

            IF scheme.existsNodeClassByNumber(depOnC) THEN
              defcname := scheme.getNodeClassName(depOnC);
            ELSE
              defcname := scheme.getNodeTypeName(depOnC);
            END;
            scheme.getAttributeNameAndClass(depOnA, defCOT, defattrname);

            IO.Put("dependency " & cname & " " & attrname & " " & defcname
                     & " " & defattrname & " ");
            IF depKind = Scheme.DependencyKind.Incoming THEN
              ename := scheme.getEdgeTypeName(etype);
              IO.Put("incoming " & ename);
            ELSIF depKind = Scheme.DependencyKind.Outgoing THEN
              ename := scheme.getEdgeTypeName(etype);
              IO.Put("outgoing " & ename);
            ELSE
              IO.Put("self");
            END;
            IO.Put("\n");
            dep := deps.get(dfound);
          END;
        END;
        subc := subclasses.get(sfound);
      END;
      attr := attributes.get(found);
    END;
  END DumpAttributesForBuildScheme;


PROCEDURE Dump (scheme: Scheme.T)
  RAISES {Scheme.InternalError, Scheme.NotDeclared} =
  VAR
    classes, superCs, types: Scheme.IDSet;
    edgeTypes              : Scheme.IDSet;
    found, sfound          : BOOLEAN;
    type                   : Scheme.ID;
    class, etype, super    : Scheme.ID;
    sourceCT, targetCT     : Scheme.ID;
    sourceCard, targetCard : Scheme.Cardinality;
  BEGIN
    (* Get all classes and print them out *)
    classes := scheme.getAllClasses();
    classes.loop();
    class := classes.get(found);
    WHILE found DO
      IO.Put("class " & scheme.getNodeClassName(class) & " "
               & IDToText(class) & "\n");
      (* find super classes *)
      superCs := scheme.getDirectSuperClasses(class);
      IF superCs.isEmpty() THEN
        IO.Put("  no super classes\n");
      ELSE
        IO.Put("  super classes :");
        superCs.loop();
        super := superCs.get(sfound);
        WHILE sfound DO
          IO.Put(" " & IDToText(super));
          super := superCs.get(sfound);
        END;
        IO.Put("\n");
      END;
      (* find all attributes *)
      DumpAttributes(scheme, class);
      IO.Put("end\n\n");
      class := classes.get(found);
    END;

    (* Get all node types and print them out *)
    types := scheme.getAllNodeTypes();
    types.loop();
    type := types.get(found);
    WHILE found DO
      IO.Put("type " & scheme.getNodeTypeName(type) & " " & IDToText(type)
               & "\n");
      (* find defining class *)
      IF scheme.typeHasSuperClass(type) THEN
        super := scheme.getSuperClassOfNodeType(type);
        IO.Put("  super class : " & IDToText(super) & "\n");
      ELSE
        IO.Put("  no super class\n");
      END;
      (* find all attributes *)
      DumpAttributes(scheme, type);
      IO.Put("end\n\n");
      type := types.get(found);
    END;

    (* Get all edge types and print them out *)
    edgeTypes := scheme.getAllEdgeTypes();
    edgeTypes.loop();
    etype := edgeTypes.get(found);
    WHILE found DO
      (* get properties *)
      scheme.showEdgeTypeProps(
        etype, sourceCT, targetCT, sourceCard, targetCard);
      IO.Put("edge type " & scheme.getEdgeTypeName(etype) & " "
               & IDToText(etype) & "\n");
      IO.Put(
        " " & IDToText(sourceCT) & " " & CardToText(sourceCard) & " --> "
          & IDToText(targetCT) & " " & CardToText(targetCard) & "\n");
      IO.Put("end\n\n");
      etype := edgeTypes.get(found);
    END;

  END Dump;

PROCEDURE DumpForBuildScheme (scheme: Scheme.T)
  RAISES {Scheme.InternalError, Scheme.NotDeclared} =
  VAR
    classes, superCs, types  : Scheme.IDSet;
    attributes, edgeTypes    : Scheme.IDSet;
    subClasses, visited      : Scheme.IDSet;
    found, sfound            : BOOLEAN;
    class, type, etype, super: Scheme.ID;
    sourceCT, targetCT       : Scheme.ID;
    sourceCard, targetCard   : Scheme.Cardinality;
  BEGIN
    attributes := NEW(Scheme.IDSet).init();
    visited := NEW(Scheme.IDSet).init();
    (* Get all classes and print them out *)
    classes := scheme.getAllRootClasses();
    WHILE NOT classes.isEmpty() DO
      subClasses := NEW(Scheme.IDSet).init();
      classes.loop();
      class := classes.get(found);
      WHILE found DO
        IF NOT visited.in(class) THEN
          IO.Put("class " & scheme.getNodeClassName(class) & "\n");
          (* find super classes *)
          superCs := scheme.getDirectSuperClasses(class);
          superCs.loop();
          super := superCs.get(sfound);
          WHILE sfound DO
            IO.Put("append " & scheme.getNodeClassName(class) & " "
                     & scheme.getNodeClassName(super) & "\n");
            super := superCs.get(sfound);
          END;
          subClasses.union(scheme.getDirectSubClasses(class));
          attributes.union(scheme.getAllAttributesOfNodeClassOrType(class));
          visited.insert(class);
        END;
        class := classes.get(found);
      END;
      classes := subClasses;
    END;

    (* Get all node types and print them out *)
    types := scheme.getAllNodeTypes();
    types.loop();
    type := types.get(found);
    WHILE found DO
      IO.Put("type " & scheme.getNodeTypeName(type) & "\n");
      (* find defining class *)
      IF scheme.typeHasSuperClass(type) THEN
        super := scheme.getSuperClassOfNodeType(type);
        IO.Put("append " & scheme.getNodeTypeName(type) & " "
                 & scheme.getNodeClassName(super) & "\n");
      END;
      attributes.union(scheme.getAllAttributesOfNodeClassOrType(type));
      type := types.get(found);
    END;

    (* Get all edge types and print them out *)
    edgeTypes := scheme.getAllEdgeTypes();
    edgeTypes.loop();
    etype := edgeTypes.get(found);
    WHILE found DO
      (* get properties *)
      scheme.showEdgeTypeProps(
        etype, sourceCT, targetCT, sourceCard, targetCard);
      IO.Put("edge " & scheme.getEdgeTypeName(etype) & " ");
      IF scheme.existsNodeClassByNumber(sourceCT) THEN
        IO.Put(scheme.getNodeClassName(sourceCT));
      ELSE
        IO.Put(scheme.getNodeTypeName(sourceCT));
      END;
      IO.Put(" " & CardToTextForBuildScheme(sourceCard) & " ");
      IF scheme.existsNodeClassByNumber(targetCT) THEN
        IO.Put(scheme.getNodeClassName(targetCT));
      ELSE
        IO.Put(scheme.getNodeTypeName(targetCT));
      END;
      IO.Put(" " & CardToTextForBuildScheme(targetCard) & "\n");
      etype := edgeTypes.get(found);
    END;

    DumpAttributesForBuildScheme(scheme, attributes);
  END DumpForBuildScheme;

BEGIN
  ReadParams();
  TRY
    TypedGraphSystem.Login(rootPath, cachesize, serverid, nameserver);
    pool := NEW(TypedGraphPool.T).open(
              poolname, Access.Mode.ReadWriteShared, new := FALSE);
    scheme := NEW(Scheme.T).open(
                pool, schemeName, local := local,
                access := Scheme.AccessMode.ReadOnlyShared, new := FALSE);
    pool.beginTransaction();
    IF forBuildScheme THEN
      DumpForBuildScheme(scheme);
    ELSE
      Dump(scheme);
    END;
    pool.commitTransaction();
    scheme.close();
    pool.close();
  EXCEPT
  | TypedGraphPool.InternalError (info) =>
      IO.Put(ErrorSupport.ToText(info));
  | Scheme.InternalError (info) =>
      ErrorAbort();
      IO.Put(ErrorSupport.ToText(info));
  | Scheme.Existent => ErrorAbort(); IO.Put("Scheme existent!\n");
  | Scheme.InUse => ErrorAbort(); IO.Put("Scheme in use!\n");
  | Scheme.NotExistent => ErrorAbort(); IO.Put("Scheme not existent!\n");
  | TypedGraphPool.NotInTransaction =>
      ErrorAbort();
      IO.Put("Not in transaction!\n");
  | Scheme.NotDeclared => ErrorAbort(); IO.Put("Not declared!\n");
  | Access.Denied (msg) => IO.Put("Access denied: " & msg & "\n");
  | Access.Locked => IO.Put("Access locked!\n");
  | PageFile.NoAccess (msg) => IO.Put("No access: " & msg & "\n");
  | Scheme.NoValidScheme => IO.Put("Invalid scheme!\n");
  | TypedGraphPool.CardinalityError => IO.Put("Cardinality Error?!\n");
  END
END DumpScheme.
