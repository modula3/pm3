MODULE SchemeView EXPORTS Main;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.2  1998/09/14 08:15:53  roland
    Modified code to remove compiler warnings.

    Revision 1.1  1998/08/06 10:49:02  roland
    Displays GRAS schemas with daVinci.

*)
(***************************************************************************)

(* Vie the contents of a scheme graph with daVinci *)

IMPORT ParseParams, IO, Fmt, Text, Process, Stdio, Pathname, Thread;
IMPORT Scheme, TypedGraphSystem, Access, ErrorSupport, AttributeValue,
       PageFile, TypedGraphPool;
IMPORT GrasParams;
IMPORT DaVinci, DaVinciMsg;

TYPE
  SchemeComponent = {NodeClass, NodeType, EdgeType, Attribute, Dependency};

CONST
  SchemeComponentParam = ARRAY SchemeComponent OF
                           TEXT{"classes", "types", "edges", "attributes",
                                "dependencies"};

VAR
  rootPath, schemeName: TEXT;
  poolname            : TEXT;
  nameserver, serverid: TEXT;
  cachesize           : CARDINAL;
  scheme              : Scheme.T;
  pool                : TypedGraphPool.T;
  local               : BOOLEAN          := FALSE;
  debug               : BOOLEAN          := FALSE;
  visibleComponents := SET OF
                         SchemeComponent{SchemeComponent.NodeClass,
                                         SchemeComponent.NodeType,
                                         SchemeComponent.EdgeType,
                                         SchemeComponent.Attribute,
                                         SchemeComponent.Dependency};

PROCEDURE ReadParams () =
  CONST
    USAGE = "<pool> <scheme> {-hide <schemecomp>} {-show <schemecomp>} \n"
              & "\t\t[-local] -root <rootPath>";

  PROCEDURE ParseError (prog, err: TEXT) =
    BEGIN
      IO.Put(err & "\n");
      IO.Put(prog & USAGE & "\n");
      Process.Exit(1);
    END ParseError;

  PROCEDURE SingleComponent (comp: TEXT): SET OF SchemeComponent
    RAISES {ParseParams.Error} =
    BEGIN
      FOR i := FIRST(SchemeComponent) TO LAST(SchemeComponent) DO
        IF Text.Equal(comp, SchemeComponentParam[i]) THEN
          RETURN SET OF SchemeComponent{i};
        END;
      END;
      RAISE ParseParams.Error;
    END SingleComponent;

  VAR
    valid   : BOOLEAN;
    complist: TEXT    := "";
    comp    : TEXT;
  BEGIN
    FOR i := FIRST(SchemeComponent) TO LAST(SchemeComponent) DO
      complist := complist & SchemeComponentParam[i] & " ";
    END;
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

      IF pp.keywordPresent("-local") THEN local := TRUE END;
      IF pp.keywordPresent("-debug") THEN debug := TRUE END;

      WHILE pp.keywordPresent("-hide") DO
        TRY
          comp := pp.getNext();
          visibleComponents := visibleComponents - SingleComponent(comp);
        EXCEPT
          ParseParams.Error =>
            ParseError(Pathname.Last(pp.arg[0]),
                       "-hide must be followed by one of " & complist);
        END;
      END;

      WHILE pp.keywordPresent("-show") DO
        TRY
          comp := pp.getNext();
          visibleComponents := visibleComponents + SingleComponent(comp);
        EXCEPT
          ParseParams.Error =>
            ParseError(Pathname.Last(pp.arg[0]),
                       "-hide must be followed by one of " & complist);
        END;
      END;

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


PROCEDURE ShowAttributes (scheme: Scheme.T;
                          CoT   : Scheme.ID;
                          msg: DaVinciMsg.T  )
  RAISES {Scheme.InternalError, Scheme.NotDeclared} =
  VAR
    attributes                      : Scheme.IDSet;
    attr, defCOT                          : Scheme.ID;
    len                                   : CARDINAL;
    kind                                  : Scheme.AttributeKind;
    indexP                                : Scheme.IndexProperties;
    type                                  : CARDINAL;
    card                                  : Scheme.Cardinality;
    constLen, found                       : BOOLEAN;
    name                                  : TEXT;
  BEGIN
    attributes := scheme.getAllAttributesOfNodeClassOrType(CoT);
    attributes.loop();
    attr := attributes.get(found);
    WHILE found DO
      scheme.getAttributeNameAndClass(attr, defCOT, name);
      IF defCOT = CoT THEN
        scheme.showAttributeProps(
          attr, kind, indexP, type, card, constLen, len);
        msg.declareNode(IDToText(attr), "attribute",
                        label := name & ": " & TypeCodeToText(type),
                        shape := DaVinciMsg.NodeShape.Text,
                        fontstyle := DaVinciMsg.FontStyle.Italic);
        msg.declareEdge(IDToText(attr) & " attr " & IDToText(CoT),
                        IDToText(CoT), type := "toAttr",
                        direction := DaVinciMsg.EdgeDirection.None,
                        first := TRUE);
        msg.endDeclareNode();
      END;
      (**
      (* print evaluation function *)
      name := scheme.getEvaluationFunction(CoT, attr);
      IF name # NIL THEN
        IO.Put("    evaluation function " & name & "\n");
      END;
      *)
      (**
      IF SchemeComponent.Dependency IN visibleComponents THEN
        (* print dependencies *)
	deps := scheme.getDefiningDependencies(CoT, attr);
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
      *)
      attr := attributes.get(found);
    END;
  END ShowAttributes;


PROCEDURE CheckOutgoingEdgeTypes (    scheme   : Scheme.T;
                                      CoT      : Scheme.ID;
                                      edgeTypes: Scheme.IDSet;
                                      msg      : DaVinciMsg.T;
                                  VAR firstEdge: BOOLEAN       )
  RAISES {Scheme.InternalError, Scheme.NotDeclared} =
  VAR
    sourceCT, targetCT    : Scheme.ID;
    sourceCard, targetCard: Scheme.Cardinality;
    targetName            : TEXT;
    etype                 : Scheme.ID;
    found                 : BOOLEAN;
  BEGIN
    edgeTypes.loop();
    etype := edgeTypes.get(found);
    WHILE found DO
      (* get properties *)
      scheme.showEdgeTypeProps(
        etype, sourceCT, targetCT, sourceCard, targetCard);
      IF sourceCT = CoT THEN
        IF scheme.existsNodeClassByNumber(targetCT) THEN
          targetName := scheme.getNodeClassName(targetCT);
        ELSE
          targetName := scheme.getNodeTypeName(targetCT);
        END;
        msg.declareEdge(
          scheme.getEdgeTypeName(etype), IDToText(targetCT),
          type := "edge_type", label := scheme.getEdgeTypeName(etype),
          first := firstEdge);
        firstEdge := FALSE;
      END;
      etype := edgeTypes.get(found);
    END;
  END CheckOutgoingEdgeTypes;

PROCEDURE View (scheme: Scheme.T; viewer: DaVinci.T)
  RAISES {Scheme.InternalError, Scheme.NotDeclared, DaVinci.Error} =
  VAR
    classes, superCs, types: Scheme.IDSet;
    edgeTypes              : Scheme.IDSet;
    found, sfound          : BOOLEAN;
    type                   : Scheme.ID;
    class, super           : Scheme.ID;
    msg                    : DaVinciMsg.T;
    firstNode, firstEdge   : BOOLEAN;
  BEGIN
    (* Get all edge types *)
    edgeTypes := scheme.getAllEdgeTypes();

    msg := NEW(DaVinciMsg.T).init();
    msg.beginNewGraph();
    firstNode := TRUE;
    IF SchemeComponent.NodeClass IN visibleComponents THEN
      (* Get all classes and print them out *)
      classes := scheme.getAllClasses();
      classes.loop();
      class := classes.get(found);
      WHILE found DO
        msg.declareNode(
          IDToText(class), type := "class",
          label := scheme.getNodeClassName(class), first := firstNode);
        firstNode := FALSE;
        (* find super classes *)
        superCs := scheme.getDirectSuperClasses(class);
        firstEdge := TRUE;
        superCs.loop();
        super := superCs.get(sfound);
        WHILE sfound DO
          msg.declareEdge(
            IDToText(class) & " is_a " & IDToText(super), IDToText(super),
            type := "is_a", pattern := DaVinciMsg.EdgePattern.Dashed,
            first := firstEdge);
          firstEdge := FALSE;
          super := superCs.get(sfound);
        END;
        IF SchemeComponent.EdgeType IN visibleComponents THEN
          CheckOutgoingEdgeTypes(scheme, class, edgeTypes, msg, firstEdge);
        END;
        msg.endDeclareNode();
        IF SchemeComponent.Attribute IN visibleComponents THEN
          (* find all attributes *)
          ShowAttributes(scheme, class, msg);
        END;
        class := classes.get(found);
      END;
    END;

    IF SchemeComponent.NodeType IN visibleComponents THEN
      (* Get all node types and print them out *)
      types := scheme.getAllNodeTypes();
      types.loop();
      type := types.get(found);
      WHILE found DO
        msg.declareNode(
          IDToText(type), type := "type",
          label := scheme.getNodeTypeName(type),
          shape := DaVinciMsg.NodeShape.Ellipse, first := firstNode);
        firstNode := FALSE;
        (* find defining class *)
        firstEdge := TRUE;
        IF scheme.typeHasSuperClass(type) THEN
          super := scheme.getSuperClassOfNodeType(type);
          msg.declareEdge(
            IDToText(type) & " is_a " & IDToText(super), IDToText(super),
            type := "is_a", pattern := DaVinciMsg.EdgePattern.Dashed,
            first := firstEdge);
          firstEdge := FALSE;
        END;
        IF SchemeComponent.EdgeType IN visibleComponents THEN
          CheckOutgoingEdgeTypes(scheme, type, edgeTypes, msg, firstEdge);
        END;
        msg.endDeclareNode();
        IF SchemeComponent.Attribute IN visibleComponents THEN
          (* find all attributes *)
          ShowAttributes(scheme, type, msg);
        END;
        type := types.get(found);
      END;
    END;
    msg.endNewGraph();
    WITH t = msg.toText() DO
      IF debug THEN IO.Put(t); END;
      viewer.send(t);
    END;
  END View;

PROCEDURE Quit (<* UNUSED *> handler: DaVinci.EventHandler;
                <* UNUSED *> type   : DaVinci.MsgType;
                <* UNUSED *> msg    : TEXT                  ) =
  BEGIN
    viewer.quit();
    Thread.Signal(Terminate);
  END Quit;

PROCEDURE MsgPrinter (<* UNUSED *> handler: DaVinci.EventHandler;
                      <* UNUSED *> type   : DaVinci.MsgType;
                                   msg    : TEXT                  ) =
  BEGIN
    IO.Put(msg & "\n");
  END MsgPrinter;

VAR
  viewer   : DaVinci.T;
  Terminate            := NEW(Thread.Condition);
  Lock                 := NEW(MUTEX);
  quitter              := NEW(DaVinci.EventHandler, notify := Quit);
  printer              := NEW(DaVinci.EventHandler, notify := MsgPrinter);

<* FATAL TypedGraphPool.CardinalityError *>
BEGIN
  ReadParams();
  IF debug THEN
    IO.Put("Displaying scheme " & poolname & "/" & schemeName & "\n");
    IO.Put("The following components will be displayed:\n");
    FOR i := FIRST(SchemeComponent) TO LAST(SchemeComponent) DO
      IF i IN visibleComponents THEN
        IO.Put(SchemeComponentParam[i] & " ");
      END;
    END;
    IO.Put("\n");
  END;
  TRY
    TypedGraphSystem.Login(rootPath, cachesize, serverid, nameserver);
    pool := NEW(TypedGraphPool.T).open(
              poolname, Access.Mode.ReadWriteShared, new := FALSE);
    scheme := NEW(Scheme.T).open(
                pool, schemeName, local := local,
                access := Scheme.AccessMode.ReadOnlyShared, new := FALSE);

    viewer := NEW(DaVinci.T).init();
    (* we want root classes on top *)
    viewer.send("menu(layout(orientation(bottom_up)))\n");
    viewer.registerHandler(DaVinci.MsgType.Quit, quitter);
    IF debug THEN
      FOR t := FIRST(DaVinci.MsgType) TO LAST(DaVinci.MsgType) DO
        viewer.registerHandler(t, printer);
      END;
    END;

    pool.beginTransaction();
    View(scheme, viewer);
    pool.commitTransaction();
    scheme.close();
    pool.close();
    LOCK Lock DO Thread.Wait(Lock, Terminate); END;
  EXCEPT
  | DaVinci.Error (msg) => IO.Put(msg & "\n");
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
  END;
END SchemeView.
