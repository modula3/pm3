MODULE Scheme EXPORTS Scheme, InternScheme;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:35  hosking
    Initial revision

    Revision 1.17  1998/03/18 09:27:23  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.16  1998/03/17 14:14:28  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.15  1998/01/21 12:36:50  roland
    New method baseName to determine filename.

    Revision 1.14  1998/01/14 17:46:56  roland
    Bugfixes in DeclareAttribute: Guard and physical length.

    Revision 1.13  1997/12/08 17:26:01  roland
    Bugfix in TextToAttributeState: converting length information to
    integer now.

    Revision 1.12  1997/12/08 11:52:22  roland
    New feature attribute placement. An attribute placement defines a
    physical location for a logical attribute.

    Revision 1.11  1997/11/21 15:17:02  roland
    New method typeHasSuperClass. Also: All temporary sets are disposed
    after use.

    Revision 1.10  1997/11/21 09:39:51  roland
    Caches switched on for read only access.

    Revision 1.9  1997/09/22 18:41:42  roland
    Scheme must not use cache when opened for writing.

    Revision 1.8  1997/09/03 13:07:00  roland
    Method closeAndFlush added to ensure real close 'inside' transactions.

    Revision 1.7  1997/07/21 10:48:07  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.6  1997/07/07 15:40:20  roland
    Added caches for relations in a Scheme and for node types in a typed
    graph.

    Revision 1.5  1997/06/04 10:26:44  roland
    Bugfix: Use AssertNodeClassOrTypeDeclared in attribute methods.

    Revision 1.4  1997/06/03 17:02:20  roland
    Bugfix in Open: Logmode in None if access is read-only.

    Revision 1.3  1997/06/02 15:54:32  roland
    Added two methods to convert between numbers of edge-types,
    node-types, and node-classes and their respective names.

    Revision 1.2  1997/05/05 10:50:42  roland
    Bugfixes in open routines for schemes.
    Dependency information moved from intern to public interface.

    Revision 1.1  1997/05/01 13:23:16  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.2  1997/02/04 11:16:01  roland
    It is now possible to disable logging in ChgMgmtGraph completely.

    Revision 1.1  1997/01/31 10:33:19  roland
    First version of new scheme layer for GRAS_3. Schemes are stored in
    separate graphs. Caches are used for accessing scheme.

*)
(***************************************************************************)

IMPORT PersistentGraph, ChgMgmtGraph, Access, PageFile;
IMPORT AttributeValue, TypedGraphPool, InternTypedGraphPool;
IMPORT ErrorSupport;
IMPORT Text, Pathname;
IMPORT NodeCard, CardText, NameCache, IdCache, AttributeCache, LabelCache,
       SourceCache, TargetCache, Node, NodeSet, Transaction;

REVEAL
  T =
    Intern BRANDED OBJECT
      pool : TypedGraphPool.T;
      graph: ChgMgmtGraph.T;
      root : Node.T;

      useCaches  : BOOLEAN            := FALSE;
      readOnly   : BOOLEAN            := FALSE;
      idCache    : IdCache.T;
      nameCache  : NameCache.T;
      labelCache : LabelCache.T;
      attrCache  : AttributeCache.T;
      sourceCache: SourceCache.T;
      targetCache: TargetCache.T;
    OVERRIDES
      open          := Open;
      close         := Close;
      closeAndFlush := CloseAndFlush;

      baseName := BaseName;

      (* Methods for managing node classes *)

      declareNodeClass        := DeclareNodeClass;
      existsNodeClassByName   := ExistsNodeClassByName;
      existsNodeClassByNumber := ExistsNodeClassByNumber;
      getNodeClassName        := GetNodeClassName;
      getNodeClassNumber      := GetNodeClassNumber;
      appendNodeClass         := AppendNodeClass;
      getAllRootClasses       := GetAllRootClasses;
      getDirectSubClasses     := GetDirectSubClasses;
      getAllSubClasses        := GetAllSubClasses;
      getDirectSuperClasses   := GetDirectSuperClasses;
      getAllSuperClasses      := GetAllSuperClasses;
      getAllClasses           := GetAllClasses;

      (* Methods for managing node types *)

      declareNodeType           := DeclareNodeType;
      existsNodeTypeByName      := ExistsNodeTypeByName;
      existsNodeTypeByNumber    := ExistsNodeTypeByNumber;
      getNodeTypeName           := GetNodeTypeName;
      getNodeTypeNumber         := GetNodeTypeNumber;
      appendNodeType            := AppendNodeType;
      getDirectNodeTypesOfClass := GetDirectNodeTypesOfClass;
      getAllNodeTypesOfClass    := GetAllNodeTypesOfClass;
      typeHasSuperClass         := TypeHasSuperClass;
      getSuperClassOfNodeType   := GetSuperClassOfNodeType;
      getAllNodeTypes           := GetAllNodeTypes;
      isSubClassOrType          := IsSubClassOrType;

      (* Methods for managing edge types *)

      declareEdgeType        := DeclareEdgeType;
      existsEdgeTypeByName   := ExistsEdgeTypeByName;
      existsEdgeTypeByNumber := ExistsEdgeTypeByNumber;
      getEdgeTypeName        := GetEdgeTypeName;
      getEdgeTypeNumber      := GetEdgeTypeNumber;
      showEdgeTypeProps      := ShowEdgeTypeProps;
      getIncomingEdgeTypes   := GetIncomingEdgeTypes;
      getOutgoingEdgeTypes   := GetOutgoingEdgeTypes;
      getAllEdgeTypes        := GetAllEdgeTypes;

      (* For convenience *)
      getTypeName   := GetTypeName;
      getTypeNumber := GetTypeNumber;

      (* Methods for managing attributes *)

      declareAttribute           := DeclareAttribute;
      existsAttributeByName      := ExistsAttributeByName;
      existsAttributeByNumber    := ExistsAttributeByNumber;
      getAttributeNameAndClass   := GetAttributeNameAndClass;
      getAttributeNumber         := GetAttributeNumber;
      showAttributeProps         := ShowAttributeProps;
      getGuard                   := GetGuard;
      isGuardedAttribute         := IsGuarded;
      getGuardsOfNodeClassOrType := AllGuardsOfNodeClassOrType;
      putMetaAttribute           := PutMetaAttribute;
      getMetaAttribute           := GetMetaAttribute;
      setEvaluationFunction      := SetEvaluationFunction;
      getEvaluationFunction      := GetEvaluationFunction;
      definesEvaluationFunction  := DefinesEvaluationFunction;
      definesDependency          := DefinesDependency;
      declareDependency          := DeclareDependency;
      getDependingOnAttribute    := GetDependingOnAttribute;
      getDependingOnEdgeType     := GetDependingOnEdgeType;
      getDependingOnClassOrType  := GetDependingOnClassOrType;
      getDefiningDependencies    := GetDefiningDependencies;
      dependsOn                  := DependsOn;
      getAllDependencies         := GetAllDependencies;
      showDependencyInfo         := ShowDependencyInfo;
      deleteDependencies         := DeleteDependencies;
      getAllAttributesOfNodeClassOrType := GetAllAttributesOfNodeClassOrType;

      showAttributePlacement   := ShowAttributePlacement;
      defineAttributePlacement := DefineAttributePlacement;

    END;

CONST
  RootName = "Gras-Scheme-1.0";

  (* The index attribute numbers we use in the scheme graph *)
  RootNameIndex      = 0;        (* the name of the root node *)
  ClassNameIndex     = 1;        (* the name of a class node *)
  TypeNameIndex      = 2;        (* the name of a type node *)
  EdgeNameIndex      = 3;        (* the name of an edge type node *)
  AttributeNameIndex = 4;        (* the name an attribute node *)

  (* the attribute numbers we use in the scheme graph *)
  PropsAttr = 0;                 (* Attribute, edge types, and dependencies
                                    have properties.  These are stored in
                                    attribute 0 of the defining node. *)

  (* the node labels we use in the scheme graph *)
  RootLabel       = 100;
  ClassLabel      = 0;           (* a node class *)
  TypeLabel       = 1;           (* a node type *)
  AttributeLabel  = 2;           (* an attribute *)
  EdgeTypeLabel   = 3;           (* an edge type *)
  DependencyLabel = 4;           (* an attribute dependency *)
  GuardLabel      = 5;           (* an attribute guard *)

  (* the edge labels we use in the scheme graph *)
  ToSubclass = 0;
  (* from a class to a sub class *)
  ToSubClassOrTypeTC = 1;        (* from a class to all sub classes
                                    (including self) *)
  ToSubtype = 2;                 (*
                from a class to a sub type *)
  AttributeDef = 3;              (* from a class/type to the attributes it
                                    defines *)
  ToAttribute = 4;               (*
              from a class/type to all of its attributes *)
  ToSource = 5;                  (*
                 from an edge type to its source node class *)
  ToTarget = 6;                  (*
                 from an edge type to its target node class *)
  ToDependent = 8;               (*
              From a dependency to its dependent attribute *)
  ToDefining = 9;                (*
               From a dependency to its defining attribute *)
  ToDependentCOT = 10;           (* From a dependency to the depending
                                    class/type *)
  ToDefiningCOT = 11;            (* From a dependency to the defining
                                    class/type *)
  IncomingEdgeDep = 12;          (* From a dependency to the edge
                                    definition *)
  OutgoingEdgeDep = 13;

  IncomingEdgeType = 14;         (* From a node class/type to all *)
  OutgoingEdgeType = 15;         (* potentially incident edge types *)

  ToGuard = 16;                  (* From an attribute declaration to the
                                    node defining its guard attribute *)
  ToAttributeGuard = 17;         (* From a node class/type to all guards of
                                    its attributes *)

  ToClassNode = 20;              (* From root to every class definition *)
  ToTypeNode = 21;               (* From root to every node type
                                    definition *)
  ToEdgeNode = 22;               (* From root to every edge type
                                    definition *)
  ToDependencyNode = 23;         (* From root to every declared
                                    dependency *)

TYPE
  Conflict = {None, Naming, Placement};

  State = {Undefined, Inherited, Defining};

  AttributeState =
    RECORD
      plcSt: State;              (* placement for this attribute? *)
      plcDefCOT: ID;             (* which class defined the placement, if
                                    plcSt = Inherited *)
      plcPhysNo: CARDINAL;       (* placement: physical attribute no *)
      plcStart : CARDINAL;       (* placement: start position *)
      plcLen   : INTEGER;        (* placement: max.  length *)
      depSt    : State;          (* dependency for this attribute? *)
      depDefClass: ID;           (* which class defined the dependency, if
                                    depSt = Inherited *)
      evalFuncSt: State;         (* evaluation function for this attribute
                                    ? *)
      evalDefClass: ID;          (* which class defined the evaluattion
                                    function, if evalFundSt = Inherited *)
      evalFuncName: TEXT;        (* the name of the function *)
    END;

CONST Unlimited = -1;

PROCEDURE Open (scheme    : T;
                pool      : TypedGraphPool.T;
                schemeName: Pathname.T;
                local     : BOOLEAN            := FALSE;
                access: AccessMode := AccessMode.ReadWriteExclusive;
                new   : BOOLEAN    := TRUE                           ): T

  RAISES {Access.Locked, Access.Denied, NoValidScheme, InternalError,
          InUse, NotExistent} =

  PROCEDURE Mode (access: AccessMode): ChgMgmtGraph.AccessMode =
    BEGIN
      IF access = AccessMode.ReadWriteExclusive THEN
        RETURN ChgMgmtGraph.AccessMode.ReadWriteExclusive;
      ELSE
        RETURN ChgMgmtGraph.AccessMode.ReadOnlyShared;
      END;
    END Mode;

  VAR
    ok     : BOOLEAN;
    logmode: ChgMgmtGraph.LogMode;
  BEGIN
    TRY
      scheme.pool := pool;
      pool.beginTransaction();
      IF new THEN
        IF pool.existsScheme(schemeName, local) THEN
          IF NOT pool.getGraphsWithScheme(schemeName, local).isEmpty()
               OR pool.inUse(schemeName, local) THEN
            RAISE InUse;
          ELSE
            pool.deleteScheme(schemeName, local);
          END;
        END;
      ELSIF NOT pool.existsScheme(schemeName, local) THEN
        RAISE NotExistent;
      END;

      IF access = AccessMode.ReadWriteExclusive THEN
        logmode := ChgMgmtGraph.LogMode.Linear;
      ELSE
        logmode := ChgMgmtGraph.LogMode.None;
        scheme.useCaches := TRUE;
      END;
      scheme.graph := NEW(ChgMgmtGraph.T);
      pool.openCGForScheme(
        scheme.graph, schemeName, local, Mode(access), errorChecks := TRUE,
        log := logmode, idCache := scheme.idCache,
        nameCache := scheme.nameCache, attrCache := scheme.attrCache,
        labelCache := scheme.labelCache, sourceCache := scheme.sourceCache,
        targetCache := scheme.targetCache);
      pool.commitTransaction();

      IF new THEN
        pool.beginTransaction();
        scheme.root := scheme.graph.createNodeNumber(
                         Node.T{scheme.graph.number(), RootLabel});
        scheme.root := scheme.graph.createNode(RootLabel);
        scheme.graph.putIndex(scheme.root, RootNameIndex, RootName);
        pool.commitTransaction();
      ELSE
        pool.beginTransaction();
        scheme.root := scheme.graph.getNodesWithIndex(
                         RootNameIndex, RootName).extractAnyElement(ok);
        pool.commitTransaction();
        IF NOT ok THEN RAISE NoValidScheme END;
      END;

      IF access = AccessMode.ReadOnlyShared THEN
        scheme.readOnly := TRUE
      END;
    EXCEPT
      ChgMgmtGraph.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.Open", "ChgMgmtGraph.InternalError", info));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.Open", "PersistentGraph.InternalError", info));
    | TypedGraphPool.CardinalityError =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.Mode", "TypedGraphPool.CardinalityError"));
    | TypedGraphPool.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.Open", "TypedGraphPool.InternalError", info));
    | TypedGraphPool.NoScheme =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.Open", "TypedGraphPool.NoScheme"));
    | TypedGraphPool.NotExistent =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "Scheme.Open", "TypedGraphPool.NotExistent"));
    | TypedGraphPool.InUse =>
        RAISE
          InternalError(
            ErrorSupport.Create("Scheme.Open", "TypedGraphPool.InUse"));
    | PageFile.NoAccess (msg) =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.Open", "PageFile.NoAccess: " & msg));
    | PersistentGraph.NotOwner =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.Open", "PersistentGraph.NotOwner"));
    | TypedGraphPool.NotInTransaction =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.Open", "TypedGraphPool.NotInTransaction"));
    | ChgMgmtGraph.LogError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.Open", "ChgMgmtGraph.LogError", info));
    | PersistentGraph.IndexUsed =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.Open", "PersistentGraph.IndexUsed"));
    | PersistentGraph.NodeNotFound =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "Scheme.Open", "PersistentGraph.NodeNotFound"));
    END;
    RETURN scheme;
  END Open;

PROCEDURE Close (scheme: T) RAISES {InternalError} =
  BEGIN
    TRY
      scheme.pool.closeCGForScheme(scheme.graph);
    EXCEPT
      TypedGraphPool.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.Close", "ChgMgmtGraph.InternalError", info));
    END;
  END Close;

PROCEDURE CloseAndFlush (scheme: T) RAISES {InternalError} =
  VAR depth: CARDINAL;
  <* FATAL TypedGraphPool.NotInTransaction *>
  BEGIN
    Close(scheme);
    depth := 0;
    TRY
      WHILE scheme.pool.getTransactionLevel() >= Transaction.TopLevel DO
        scheme.pool.commitTransaction();
        INC(depth);
      END;
      FOR d := 1 TO depth DO scheme.pool.beginTransaction(); END;
    EXCEPT
      TypedGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.CloseAndFlush",
                              "TypedGraphPool.InternalError", info));
    | TypedGraphPool.CardinalityError =>
        RAISE InternalError(
                ErrorSupport.Create("Scheme.CloseAndFlush",
                                    "TypedGraphPool.CardinalityError"));
    END;
  END CloseAndFlush;

PROCEDURE BaseName (scheme: T): Pathname.T =
  BEGIN
    RETURN scheme.graph.baseName();
  END BaseName;

(* Methods for managing node classes. *)

PROCEDURE DeclareNodeClass (scheme: T; name: TEXT): ID
  RAISES {AlreadyDeclared, InternalError} =
  VAR node: ID;
  BEGIN
    AssertNotDeclared(scheme, name, ClassNameIndex);
    node := CreateNode(scheme, ClassLabel, name, ClassNameIndex);
    CreateEdge(scheme, scheme.root, node, ToClassNode);
    RETURN node;
  END DeclareNodeClass;

PROCEDURE ExistsNodeClassByName (scheme: T; name: TEXT): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    RETURN ExistsNamedNode(scheme, ClassLabel, name, ClassNameIndex);
  END ExistsNodeClassByName;

PROCEDURE ExistsNodeClassByNumber (scheme: T; class: ID): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    RETURN ExistsNodeWithLabel(scheme, class, ClassLabel);
  END ExistsNodeClassByNumber;

PROCEDURE GetNodeClassName (scheme: T; class: ID): TEXT
  RAISES {NotDeclared, InternalError} =
  BEGIN
    RETURN GetNodeName(scheme, class, ClassLabel, ClassNameIndex);
  END GetNodeClassName;

PROCEDURE GetNodeClassNumber (scheme: T; name: TEXT): ID
  RAISES {NotDeclared, InternalError} =
  BEGIN
    RETURN GetNamedNode(scheme, ClassLabel, name, ClassNameIndex);
  END GetNodeClassNumber;

PROCEDURE AppendNodeClass (scheme: T; class, super: ID)
  RAISES {NotDeclared, Cyclic, AttributeNameClash, AttributePlacementClash,
          InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, class, ClassLabel);
    AssertDeclaredByNumber(scheme, super, ClassLabel);
    MakeSubClassOrType(scheme, class, super, type := FALSE);
  END AppendNodeClass;

PROCEDURE GetAllRootClasses (scheme: T): IDSet RAISES {InternalError} =
  VAR
    classes, inc, roots: IDSet;
    class              : ID;
    found              : BOOLEAN;
  BEGIN
    roots := NodeSet.New();
    classes := GetTargets(scheme, scheme.root, ToClassNode);
    classes.loop();
    class := classes.get(found);
    WHILE found DO
      inc := GetSources(scheme, class, ToSubclass);
      IF inc.isEmpty() THEN roots.insert(class); END;
      inc.dispose();
      class := classes.get(found);
    END;
    classes.dispose();
    RETURN roots;
  END GetAllRootClasses;

PROCEDURE GetDirectSubClasses (scheme: T; class: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, class, ClassLabel);
    RETURN GetTargets(scheme, class, ToSubclass);
  END GetDirectSubClasses;

PROCEDURE GetAllSubClasses (scheme: T; class: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, class, ClassLabel);
    RETURN GetTargets(scheme, class, ToSubClassOrTypeTC);
  END GetAllSubClasses;

PROCEDURE GetDirectSuperClasses (scheme: T; class: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, class, ClassLabel);
    RETURN GetSources(scheme, class, ToSubclass);
  END GetDirectSuperClasses;

PROCEDURE GetAllSuperClasses (scheme: T; class: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, class, ClassLabel);
    RETURN GetSources(scheme, class, ToSubClassOrTypeTC);
  END GetAllSuperClasses;

PROCEDURE GetAllClasses (scheme: T): IDSet RAISES {InternalError} =
  BEGIN
    RETURN GetTargets(scheme, scheme.root, ToClassNode);
  END GetAllClasses;

(* Methods for managing node types *)

PROCEDURE DeclareNodeType (scheme: T; name: TEXT): ID
  RAISES {AlreadyDeclared, InternalError} =
  VAR node: ID;
  BEGIN
    AssertNotDeclared(scheme, name, TypeNameIndex);
    node := CreateNode(scheme, TypeLabel, name, TypeNameIndex);
    CreateEdge(scheme, scheme.root, node, ToTypeNode);
    RETURN node;
  END DeclareNodeType;

PROCEDURE ExistsNodeTypeByName (scheme: T; name: TEXT): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    RETURN ExistsNamedNode(scheme, TypeLabel, name, TypeNameIndex);
  END ExistsNodeTypeByName;

PROCEDURE ExistsNodeTypeByNumber (scheme: T; type: ID): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    RETURN ExistsNodeWithLabel(scheme, type, TypeLabel);
  END ExistsNodeTypeByNumber;

PROCEDURE GetNodeTypeName (scheme: T; type: ID): TEXT
  RAISES {NotDeclared, InternalError} =
  BEGIN
    RETURN GetNodeName(scheme, type, TypeLabel, TypeNameIndex);
  END GetNodeTypeName;

PROCEDURE GetNodeTypeNumber (scheme: T; name: TEXT): ID
  RAISES {NotDeclared, InternalError} =
  BEGIN
    RETURN GetNamedNode(scheme, TypeLabel, name, TypeNameIndex);
  END GetNodeTypeNumber;

PROCEDURE AppendNodeType (scheme: T; type, class: ID)
  RAISES {AlreadyDeclared, AttributePlacementClash, NotDeclared,
          InternalError} =
  BEGIN
    TRY
      AssertDeclaredByNumber(scheme, type, TypeLabel);
      AssertDeclaredByNumber(scheme, class, ClassLabel);
      WITH super = GetSources(scheme, type, ToSubtype) DO
        IF NOT super.isEmpty() THEN
          super.dispose();
          RAISE AlreadyDeclared;
        END;
        super.dispose();
      END;
      MakeSubClassOrType(scheme, type, class, type := TRUE);
    EXCEPT
    | Cyclic =>
        RAISE InternalError(
                ErrorSupport.Create("Scheme.AppendNodeType", "Cyclic"));
    | AttributeNameClash =>
        RAISE InternalError(ErrorSupport.Create("Scheme.AppendNodeType",
                                                "AttributeNameClash"));
    END;
  END AppendNodeType;

PROCEDURE GetDirectNodeTypesOfClass (scheme: T; class: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, class, ClassLabel);
    RETURN GetTargets(scheme, class, ToSubtype);
  END GetDirectNodeTypesOfClass;

PROCEDURE GetAllNodeTypesOfClass (scheme: T; class: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  VAR types: IDSet;
  BEGIN
    AssertDeclaredByNumber(scheme, class, ClassLabel);
    types := GetTargets(scheme, scheme.root, ToTypeNode);
    WITH trans = GetTargets(scheme, class, ToSubClassOrTypeTC) DO
      types.intersection(trans);
      trans.dispose();
    END;
    RETURN types;
  END GetAllNodeTypesOfClass;

PROCEDURE IsSubClassOrType (scheme: T; sub, super: ID): BOOLEAN
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, sub);
    AssertNodeClassOrTypeDeclared(scheme, super);
    RETURN
      sub = super OR ExistsEdge(scheme, super, sub, ToSubClassOrTypeTC);
  END IsSubClassOrType;

PROCEDURE TypeHasSuperClass (scheme: T; type: ID): BOOLEAN
  RAISES {NotDeclared, InternalError} =
  VAR res: BOOLEAN;
  BEGIN
    AssertDeclaredByNumber(scheme, type, TypeLabel);
    WITH supers = GetSources(scheme, type, ToSubtype) DO
      res := NOT supers.isEmpty();
      supers.dispose();
    END;
    RETURN res;
  END TypeHasSuperClass;

PROCEDURE GetSuperClassOfNodeType (scheme: T; type: ID): ID
  RAISES {NotDeclared, InternalError} =
  VAR
    res: ID;
    ok : BOOLEAN;
  BEGIN
    AssertDeclaredByNumber(scheme, type, TypeLabel);
    WITH supers = GetSources(scheme, type, ToSubtype) DO
      supers.loop();
      res := supers.get(ok);
      IF NOT ok THEN
        supers.dispose();
        RAISE NotDeclared;
      ELSIF supers.card() # 1 THEN
        supers.dispose();
        RAISE InternalError(
                ErrorSupport.Create("Scheme.GetSuperClassOfNodeType",
                                    "More than one super class."));
      END;
      supers.dispose();
    END;
    RETURN res;
  END GetSuperClassOfNodeType;

PROCEDURE GetAllNodeTypes (scheme: T): IDSet RAISES {InternalError} =
  BEGIN
    RETURN GetTargets(scheme, scheme.root, ToTypeNode);
  END GetAllNodeTypes;

(* Methods for managing edge types *)

PROCEDURE DeclareEdgeType (scheme    : T;
                           name      : TEXT;
                           sourceCOT : ID;
                           targetCOT : ID;
                           sourceCard: Cardinality := ArbitraryCard;
                           targetCard: Cardinality := ArbitraryCard  ): ID
  RAISES {NotDeclared, AlreadyDeclared, InternalError} =
  VAR
    edgedef     : ID;
    subs        : IDSet;
    DrawEdgeClos: DrawEdgeClosure;
  BEGIN
    AssertNotDeclared(scheme, name, EdgeNameIndex);
    AssertNodeClassOrTypeDeclared(scheme, sourceCOT);
    AssertNodeClassOrTypeDeclared(scheme, targetCOT);
    edgedef := CreateNode(scheme, EdgeTypeLabel, name, EdgeNameIndex);
    CreateEdge(scheme, scheme.root, edgedef, ToEdgeNode);
    (* Connect edge definition with source and target node class *)
    CreateEdge(scheme, edgedef, sourceCOT, ToSource);
    CreateEdge(scheme, edgedef, targetCOT, ToTarget);
    CreateEdge(scheme, sourceCOT, edgedef, OutgoingEdgeType);
    CreateEdge(scheme, targetCOT, edgedef, IncomingEdgeType);
    (* store edge properties *)
    PutAttribute(
      scheme, edgedef, PropsAttr, EdgeCardsToText(sourceCard, targetCard));
    (* connect all subclasses of sourceCOT and targetCOT to edge type *)
    subs := GetTargets(scheme, sourceCOT, ToSubClassOrTypeTC);
    DrawEdgeClos :=
      NEW(DrawEdgeClosure, scheme := scheme, type := OutgoingEdgeType,
          node := edgedef, isSource := FALSE);
    Map(DrawEdgeClos, subs);
    subs.dispose();
    subs := GetTargets(scheme, targetCOT, ToSubClassOrTypeTC);
    DrawEdgeClos.type := IncomingEdgeType;
    Map(DrawEdgeClos, subs);
    subs.dispose();
    RETURN edgedef;
  END DeclareEdgeType;

PROCEDURE ExistsEdgeTypeByName (scheme: T; name: TEXT): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    RETURN ExistsNamedNode(scheme, EdgeTypeLabel, name, EdgeNameIndex);
  END ExistsEdgeTypeByName;

PROCEDURE ExistsEdgeTypeByNumber (scheme: T; edge: ID): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    RETURN ExistsNodeWithLabel(scheme, edge, EdgeTypeLabel);
  END ExistsEdgeTypeByNumber;

PROCEDURE GetEdgeTypeName (scheme: T; edge: ID): TEXT
  RAISES {NotDeclared, InternalError} =
  BEGIN
    RETURN GetNodeName(scheme, edge, EdgeTypeLabel, EdgeNameIndex);
  END GetEdgeTypeName;

PROCEDURE GetEdgeTypeNumber (scheme: T; name: TEXT): ID
  RAISES {NotDeclared, InternalError} =
  BEGIN
    RETURN GetNamedNode(scheme, EdgeTypeLabel, name, EdgeNameIndex);
  END GetEdgeTypeNumber;

PROCEDURE ShowEdgeTypeProps (    scheme    : T;
                                 edgeType  : ID;
                             VAR sourceCT  : ID;
                             VAR targetCT  : ID;
                             VAR sourceCard: Cardinality;
                             VAR targetCard: Cardinality  )
  RAISES {NotDeclared, InternalError} =
  VAR cardtext: TEXT;
  BEGIN
    AssertDeclaredByNumber(scheme, edgeType, EdgeTypeLabel);
    cardtext := GetAttribute(scheme, edgeType, PropsAttr);
    TextToEdgeCards(cardtext, sourceCard, targetCard);
    sourceCT := GetSingleTarget(scheme, edgeType, ToSource);
    targetCT := GetSingleTarget(scheme, edgeType, ToTarget);
  END ShowEdgeTypeProps;

PROCEDURE GetIncomingEdgeTypes (scheme: T; CoT: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, CoT);
    RETURN GetTargets(scheme, CoT, IncomingEdgeType);
  END GetIncomingEdgeTypes;

PROCEDURE GetOutgoingEdgeTypes (scheme: T; CoT: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, CoT);
    RETURN GetTargets(scheme, CoT, OutgoingEdgeType);
  END GetOutgoingEdgeTypes;

PROCEDURE GetAllEdgeTypes (scheme: T): IDSet RAISES {InternalError} =
  BEGIN
    RETURN GetTargets(scheme, scheme.root, ToEdgeNode);
  END GetAllEdgeTypes;

(* For convenience *)

PROCEDURE GetTypeName (scheme: T; type: ID): TEXT
  RAISES {NotDeclared, InternalError} =
  VAR
    res, index: CARDINAL;
    found     : BOOLEAN;
    val       : TEXT;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.labelCache.get(type, res, found);
        IF NOT found THEN RAISE NotDeclared; END;
      ELSE
        res := scheme.graph.getNodeLabel(type);
      END;

      IF res = ClassLabel THEN
        index := ClassNameIndex;
      ELSIF res = TypeLabel THEN
        index := TypeNameIndex;
      ELSIF res = EdgeTypeLabel THEN
        index := EdgeNameIndex;
      ELSE
        RAISE NotDeclared
      END;

      IF scheme.useCaches THEN
        scheme.idCache.get(NodeCard.T{type, index}, val, found);
      ELSE
        val := scheme.graph.getIndex(type, index, found);
      END;
      IF NOT found THEN
        RAISE
          InternalError(
            ErrorSupport.Create("Scheme.GetTypeName", "CorruptScheme"));
      END;
      RETURN val;
    EXCEPT
      PersistentGraph.NodeNotFound => RAISE NotDeclared;
    | PersistentGraph.NotOwner => RAISE NotDeclared;
    | LabelCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.GetTypeName", "LabelCache.Error", info));
    | Access.Locked =>
        RAISE
          InternalError(
            ErrorSupport.Create("Scheme.GetTypeName", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetTypeName",
                              "PersistentGraph.InternalError", info));
    | IdCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.GetTypeName", "IdCache.Error", info));
    END;
  END GetTypeName;

PROCEDURE GetTypeNumber (scheme: T; name: TEXT): ID
  RAISES {NotDeclared, InternalError} =
  VAR
    res: Node.T;
    set: IDSet;
    ok : BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.nameCache.get(CardText.T{ClassNameIndex, name}, res, ok);
        IF NOT ok THEN
          scheme.nameCache.get(CardText.T{TypeNameIndex, name}, res, ok);
          IF NOT ok THEN
            scheme.nameCache.get(CardText.T{EdgeNameIndex, name}, res, ok);
            IF NOT ok THEN RAISE NotDeclared; END;
          END;
        END;
      ELSE
        set := scheme.graph.getNodesWithIndex(ClassNameIndex, name);
        res := set.extractAnyElement(ok);
        IF NOT ok THEN
          set := scheme.graph.getNodesWithIndex(TypeNameIndex, name);
          res := set.extractAnyElement(ok);
          IF NOT ok THEN
            set := scheme.graph.getNodesWithIndex(EdgeNameIndex, name);
            res := set.extractAnyElement(ok);
            IF NOT ok THEN RAISE NotDeclared; END;
          END;
        END;
        IF set.card() > 0 THEN
          RAISE InternalError(ErrorSupport.Create(
                                "Scheme.GetTypeNumber", "CorruptScheme"))
        END;
      END;
      RETURN res;
    EXCEPT
    | NameCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.GetTypeNumber", "NameCache.Error", info));
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetTypeNumber", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetTypeNumber",
                              "PersistentGraph.InternalError", info));
    END;
  END GetTypeNumber;

(* Methods for managing attributes *)

PROCEDURE DeclareAttribute (scheme     : T;
                            classOrType: ID;
                            name       : TEXT;
                            kind       : AttributeKind;
                            props      : IndexProperties;
                            type       : CARDINAL;
                            card       : Cardinality;
                            constLen   : BOOLEAN;
                            len        : CARDINAL         ): ID
  RAISES {NotDeclared, AlreadyDeclared, AttributeNameClash,
          AttributePlacementClash, InternalError} =
  VAR
    attdef, oldattr : ID;
    propagateClosure: PropagateAttributeClosure;
    ok              : BOOLEAN                   := FALSE;
    stopSet         : IDSet                     := NIL;
    guarded         : BOOLEAN                   := FALSE;
    guard           : ID;
    plcmtlen        : INTEGER;
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, classOrType);
    IF HasAttributeByName(scheme, classOrType, name, oldattr) THEN
      RAISE AlreadyDeclared;
    END;

    attdef := CreateNode(scheme, AttributeLabel, name, AttributeNameIndex);
    (* Connect attribute definition with defining node class *)
    CreateEdge(scheme, classOrType, attdef, AttributeDef);
    (* store attribute properties *)
    PutAttribute(scheme, attdef, PropsAttr,
                 AttrPropsToText(kind, props, type, card, constLen, len));
    IF props = IndexProperties.Index OR (props = IndexProperties.Key
                                           AND kind
                                                 # AttributeKind.Intrinsic) THEN
      (* This is a guarded attribute.  Define a guard for it *)
      guarded := TRUE;
      guard := CreateNode(scheme, GuardLabel);
      CreateEdge(scheme, attdef, guard, ToGuard);
    END;

    (* plcmtlen is always set to unlimited initially to guarantee
       truncation for derived attributes during invalidation. *)
    plcmtlen := Unlimited;

    (* Propagate attribute to sub classes. *)
    propagateClosure := NEW(PropagateAttributeClosure);
    propagateClosure.attr := attdef;
    propagateClosure.attrWithSameName :=
      GetNodesWithIndex(scheme, AttributeNameIndex, name);
    propagateClosure.attrWithSameName.deleteElement(attdef, ok);
    propagateClosure.state :=
      AttributeState{
        plcSt := State.Undefined, plcDefCOT := classOrType, plcPhysNo :=
        attdef.entity, plcStart := 0, plcLen := plcmtlen, depSt :=
        State.Undefined, depDefClass := ID{0, 0}, evalFuncSt :=
        State.Undefined, evalDefClass := ID{0, 0}, evalFuncName := ""};
    propagateClosure.guarded := guarded;
    propagateClosure.guard := guard;
    Propagate(propagateClosure, scheme, classOrType, stopSet);
    IF NOT stopSet.isEmpty() THEN
      CASE propagateClosure.conflict OF
      | Conflict.Naming => RAISE AttributeNameClash;
      | Conflict.Placement => RAISE AttributePlacementClash;
      ELSE
      END;
    END;
    RETURN attdef;
  END DeclareAttribute;

PROCEDURE ExistsAttributeByName (scheme: T; classOrType: ID; name: TEXT):
  BOOLEAN RAISES {NotDeclared, InternalError} =
  VAR attr: ID;
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, classOrType);
    RETURN HasAttributeByName(scheme, classOrType, name, attr);
  END ExistsAttributeByName;

PROCEDURE ExistsAttributeByNumber (scheme: T; attr: ID): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    RETURN ExistsNodeWithLabel(scheme, attr, AttributeLabel);
  END ExistsAttributeByNumber;

PROCEDURE GetAttributeNameAndClass (    scheme     : T;
                                        attr       : ID;
                                    VAR classOrType: ID;
                                    VAR name       : TEXT)
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    classOrType := GetSingleSource(scheme, attr, AttributeDef);
    name := GetNodeName(scheme, attr, AttributeLabel, AttributeNameIndex);
  END GetAttributeNameAndClass;

PROCEDURE GetAttributeNumber (scheme: T; classOrType: ID; name: TEXT): ID
  RAISES {NotDeclared, InternalError} =
  VAR num: ID;
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, classOrType);
    IF NOT HasAttributeByName(scheme, classOrType, name, num) THEN
      RAISE NotDeclared;
    END;
    RETURN num;
  END GetAttributeNumber;

PROCEDURE ShowAttributeProps (    scheme   : T;
                                  attribute: ID;
                              VAR kind     : AttributeKind;
                              VAR props    : IndexProperties;
                              VAR type     : CARDINAL;
                              VAR card     : Cardinality;
                              VAR constLen : BOOLEAN;
                              VAR len      : CARDINAL         )
  RAISES {NotDeclared, InternalError} =
  VAR proptext: TEXT;
  BEGIN
    AssertDeclaredByNumber(scheme, attribute, AttributeLabel);
    proptext := GetAttribute(scheme, attribute, PropsAttr);
    TextToAttrProps(proptext, kind, props, type, card, constLen, len);
  END ShowAttributeProps;

PROCEDURE DefineAttributePlacement (scheme         : T;
                                    cot            : ID;
                                    attr           : ID;
                                    physAttributeNo: CARDINAL;
                                    physStart      : CARDINAL;
                                    physLength     : INTEGER   )
  RAISES {NotDeclared, AttributePlacementClash, InternalError} =
  VAR
    propagateClosure: PropagatePlacementClosure;
    stopSet         : IDSet;
    kind            : AttributeKind;
    props           : IndexProperties;
    type            : CARDINAL;
    card            : Cardinality;
    constLen        : BOOLEAN;
    len             : CARDINAL;
    proptext        : TEXT;
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    AssertNodeClassOrTypeDeclared(scheme, cot);
    IF NOT ExistsEdge(scheme, cot, attr, ToAttribute) THEN
      RAISE NotDeclared;
    END;

    IF physLength < 0 THEN physLength := Unlimited END;
    (* check attribute properties *)
    proptext := GetAttribute(scheme, attr, PropsAttr);
    TextToAttrProps(proptext, kind, props, type, card, constLen, len);
    IF props # IndexProperties.Normal OR kind = AttributeKind.Meta
         OR kind = AttributeKind.Dummy THEN
      (* no index/key attributes and no meta or dummy attributes *)
      RAISE AttributePlacementClash;
    END;
    IF kind = AttributeKind.Derived AND physLength # Unlimited THEN
      (* derived attributes only if truncation is possible *)
      RAISE AttributePlacementClash;
    END;
    IF constLen AND physLength # Unlimited AND len > physLength THEN
      (* physical placement needs at least as much space as logical *)
      RAISE AttributePlacementClash;
    END;

    propagateClosure := NEW(PropagatePlacementClosure);
    propagateClosure.attr := attr;
    propagateClosure.defCOT := cot;
    propagateClosure.phys := physAttributeNo;
    propagateClosure.start := physStart;
    propagateClosure.len := physLength;
    propagateClosure.conflict := Conflict.None;
    Propagate(propagateClosure, scheme, cot, stopSet);
    IF propagateClosure.conflict # Conflict.None THEN
      RAISE AttributePlacementClash;
    END;
  END DefineAttributePlacement;

PROCEDURE ShowAttributePlacement (    scheme     : T;
                                      classOrType: ID;
                                      attribute  : ID;
                                  VAR physNo     : CARDINAL;
                                  VAR physStart  : CARDINAL;
                                  VAR physMaxLen : INTEGER   )
  RAISES { (* NotDeclared, *)InternalError} =
  VAR state: AttributeState;
  BEGIN
    (* AssertDeclaredByNumber(scheme, attribute, AttributeLabel); *)
    (* AssertNodeClassOrTypeDeclared(scheme, classOrType); *)
    (* IF NOT ExistsEdge(scheme, cot, attr, ToAttribute) THEN *)
    (* RAISE NotDeclared; *)
    (* END; *)

    TextToAttributeState(
      GetAttribute(scheme, attribute, classOrType.entity), state);
    physNo := state.plcPhysNo;
    physStart := state.plcStart;
    physMaxLen := state.plcLen;
  END ShowAttributePlacement;

PROCEDURE IsGuarded (scheme: T; attr: ID): BOOLEAN
  RAISES {NotDeclared, InternalError} =
  VAR res: BOOLEAN;
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    WITH guards = GetTargets(scheme, attr, ToGuard) DO
      res := NOT guards.isEmpty();
      guards.dispose();
    END;
    RETURN res;
  END IsGuarded;

PROCEDURE GetGuard (scheme: T; attr: ID): ID
  RAISES {NotDeclared, InternalError} =
  VAR
    guards: IDSet;
    guard : ID;
    ok    : BOOLEAN;
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    guards := GetTargets(scheme, attr, ToGuard);
    guards.loop();
    guard := guards.get(ok);
    IF NOT ok THEN
      guards.dispose();
      RAISE NotDeclared;
    ELSIF NOT guards.card() = 1 THEN
      guards.dispose();
      RAISE InternalError(ErrorSupport.Create(
                            "Scheme.GetGuard", "More than one guard!"));
    END;
    guards.dispose();
    RETURN guard;
  END GetGuard;

PROCEDURE AllGuardsOfNodeClassOrType (scheme: T; CoT: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, CoT);
    RETURN GetTargets(scheme, CoT, ToAttributeGuard);
  END AllGuardsOfNodeClassOrType;

PROCEDURE PutMetaAttribute (scheme           : T;
                            classOrType, attr: ID;
                            begin            : CARDINAL;
                            value            : TEXT      )
  RAISES {NotDeclared, InternalError} =
  VAR
    proptext   : TEXT;
    kind       : AttributeKind;
    props      : IndexProperties;
    type       : CARDINAL;
    card       : Cardinality;
    constLen   : BOOLEAN;
    len        : CARDINAL;
    subs       : IDSet;
    PutAttrClos: PutAttributeClosure;
  BEGIN
    (* exists class and attribute ? *)
    AssertNodeClassOrTypeDeclared(scheme, classOrType);
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    (* is attribute an attribute of class ? *)
    IF NOT ExistsEdge(scheme, classOrType, attr, ToAttribute) THEN
      RAISE NotDeclared;
    END;
    (* is attribute a meta attribute ? *)
    proptext := GetAttribute(scheme, attr, PropsAttr);
    TextToAttrProps(proptext, kind, props, type, card, constLen, len);
    IF kind # AttributeKind.Meta THEN RAISE NotDeclared; END;
    (* write value to classOrType and all of its sub classes and types *)
    subs := GetTargets(scheme, classOrType, ToSubClassOrTypeTC);
    subs.insert(classOrType);
    PutAttrClos :=
      NEW(PutAttributeClosure, scheme := scheme, number := attr.entity,
          begin := begin, value := value);
    Map(PutAttrClos, subs);
    subs.dispose();
  END PutMetaAttribute;

PROCEDURE GetMetaAttribute (scheme           : T;
                            classOrType, attr: ID;
                            begin, length    : CARDINAL): TEXT
  RAISES {NotDeclared, InternalError} =
  VAR
    proptext: TEXT;
    kind    : AttributeKind;
    props   : IndexProperties;
    type    : CARDINAL;
    card    : Cardinality;
    constLen: BOOLEAN;
    len     : CARDINAL;
  BEGIN
    (* exists class and attribute ? *)
    AssertNodeClassOrTypeDeclared(scheme, classOrType);
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    (* is attribute an attribute of class ? *)
    IF NOT ExistsEdge(scheme, classOrType, attr, ToAttribute) THEN
      RAISE NotDeclared;
    END;
    (* is attribute a meta attribute ? *)
    proptext := GetAttribute(scheme, attr, PropsAttr);
    TextToAttrProps(proptext, kind, props, type, card, constLen, len);
    IF kind # AttributeKind.Meta THEN RAISE NotDeclared; END;
    TRY
      RETURN
        scheme.graph.getAttribute(classOrType, attr.entity, begin, length);
    EXCEPT
      PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.GetAttribute", "PersistentGraph.NodeNotFound"));
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetAttribute", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetAttribute",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("Scheme.GetMetaAttribute",
                                            "PersistentGraph.NotOwner"));
    END
  END GetMetaAttribute;

PROCEDURE SetEvaluationFunction (scheme: T; cot, attr: ID; name: TEXT)
  RAISES {NotDeclared, InternalError} =
  VAR
    propEvalClosure := NEW(PropagateEvalFuncClosure, func := name,
                           attr := attr, from := cot);
    stopSet: IDSet := NIL;
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    AssertNodeClassOrTypeDeclared(scheme, cot);
    IF NOT ExistsEdge(scheme, cot, attr, ToAttribute) THEN
      RAISE NotDeclared;
    END;

    Propagate(propEvalClosure, scheme, cot, stopSet);
  END SetEvaluationFunction;

PROCEDURE DefinesEvaluationFunction (scheme: T; cot, attr: ID): BOOLEAN
  RAISES {NotDeclared, InternalError} =
  VAR state: AttributeState;
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    AssertNodeClassOrTypeDeclared(scheme, cot);
    IF NOT ExistsEdge(scheme, cot, attr, ToAttribute) THEN
      RAISE NotDeclared;
    END;

    TextToAttributeState(GetAttribute(scheme, attr, cot.entity), state);
    RETURN state.evalFuncSt = State.Defining;
  END DefinesEvaluationFunction;

PROCEDURE DefinesDependency (scheme: T; cot, attr: ID): BOOLEAN
  RAISES {NotDeclared, InternalError} =
  VAR state: AttributeState;
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    AssertNodeClassOrTypeDeclared(scheme, cot);
    IF NOT ExistsEdge(scheme, cot, attr, ToAttribute) THEN
      RAISE NotDeclared;
    END;

    TextToAttributeState(GetAttribute(scheme, attr, cot.entity), state);
    RETURN state.depSt = State.Defining;
  END DefinesDependency;

PROCEDURE GetEvaluationFunction (scheme: T; cot, attr: ID): TEXT
  RAISES {NotDeclared, InternalError} =
  VAR state: AttributeState;
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    AssertNodeClassOrTypeDeclared(scheme, cot);
    IF NOT ExistsEdge(scheme, cot, attr, ToAttribute) THEN
      RAISE NotDeclared;
    END;

    TextToAttributeState(GetAttribute(scheme, attr, cot.entity), state);
    IF state.evalFuncSt # State.Undefined THEN
      RETURN state.evalFuncName;
    ELSE
      RETURN NIL;
    END;
  END GetEvaluationFunction;

PROCEDURE DeclareDependency (scheme       : T;
                             CoT          : ID;
                             dependentAttr: ID;
                             dependsOnAttr: ID;
                             kind         : DependencyKind;
                             edgeType     : ID              )
  RAISES {NotDeclared, InternalError} =

  PROCEDURE InsertDependency (scheme                     : T;
                              dependentAttr, dependentCOT: ID;
                              dependsOnAttr, dependsOnCOT: ID;
                              edgeType                   : ID;
                              kind                       : DependencyKind)
    RAISES {InternalError} =
    VAR
      depNode: ID;
      props  : TEXT;
    BEGIN
      depNode := CreateNode(scheme, DependencyLabel);
      CreateEdge(scheme, scheme.root, depNode, ToDependencyNode);
      props := DependencyPropsToText(edgeType, kind);
      PutAttribute(scheme, depNode, PropsAttr, props);
      CreateEdge(scheme, depNode, dependentAttr, ToDependent);
      CreateEdge(scheme, depNode, dependsOnAttr, ToDefining);
      CreateEdge(scheme, depNode, dependentCOT, ToDependentCOT);
      CreateEdge(scheme, depNode, dependsOnCOT, ToDefiningCOT);
      IF kind = DependencyKind.Outgoing THEN
        CreateEdge(scheme, depNode, edgeType, OutgoingEdgeDep);
      ELSIF kind = DependencyKind.Incoming THEN
        CreateEdge(scheme, depNode, edgeType, IncomingEdgeDep);
      END;
    END InsertDependency;

  VAR
    depsOnCOT     : ID;
    propDepClosure: PropagateDependencyClosure;
    stopSet       : IDSet                      := NIL;
  BEGIN
    (* Are class/type, attributes, and edge type declared ? *)
    AssertNodeClassOrTypeDeclared(scheme, CoT);
    AssertDeclaredByNumber(scheme, dependentAttr, AttributeLabel);
    AssertDeclaredByNumber(scheme, dependsOnAttr, AttributeLabel);
    IF kind # DependencyKind.Self THEN
      AssertDeclaredByNumber(scheme, edgeType, EdgeTypeLabel);
    END;
    IF NOT ExistsEdge(scheme, CoT, dependentAttr, ToAttribute) THEN
      RAISE NotDeclared
    END;

    CASE kind OF
      DependencyKind.Self =>
        IF NOT ExistsEdge(scheme, CoT, dependsOnAttr, ToAttribute) THEN
          RAISE NotDeclared;
        END;
        InsertDependency(
          scheme, dependentAttr, CoT, dependsOnAttr, CoT, edgeType, kind);
    | DependencyKind.Incoming =>
        depsOnCOT := GetSingleSource(scheme, dependsOnAttr, AttributeDef);
        IF NOT EdgeTypeConnects(scheme, depsOnCOT, CoT, edgeType) THEN
          RAISE NotDeclared;
        END;
        InsertDependency(scheme, dependentAttr, CoT, dependsOnAttr,
                         depsOnCOT, edgeType, kind);
    | DependencyKind.Outgoing =>
        depsOnCOT := GetSingleSource(scheme, dependsOnAttr, AttributeDef);
        IF NOT EdgeTypeConnects(scheme, CoT, depsOnCOT, edgeType) THEN
          RAISE NotDeclared;
        END;
        InsertDependency(scheme, dependentAttr, CoT, dependsOnAttr,
                         depsOnCOT, edgeType, kind);
    END;

    propDepClosure := NEW(PropagateDependencyClosure,
                          attr := dependentAttr, depCOT := CoT);
    Propagate(propDepClosure, scheme, CoT, stopSet);
  END DeclareDependency;

PROCEDURE GetDependingOnAttribute (scheme: T; attr: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    RETURN GetSources(scheme, attr, ToDefining);
  END GetDependingOnAttribute;

PROCEDURE GetDependingOnEdgeType (scheme  : T;
                                  edgeType: ID;
                                  incoming: BOOLEAN): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, edgeType, EdgeTypeLabel);
    IF incoming THEN
      RETURN GetSources(scheme, edgeType, IncomingEdgeDep);
    ELSE
      RETURN GetSources(scheme, edgeType, OutgoingEdgeDep);
    END;
  END GetDependingOnEdgeType;

PROCEDURE GetDependingOnClassOrType (scheme: T; CoT: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, CoT);
    RETURN GetSources(scheme, CoT, ToDefiningCOT);
  END GetDependingOnClassOrType;

PROCEDURE GetDefiningDependencies (scheme: T; CoT, dependentAttr: ID):
  IDSet RAISES {NotDeclared, InternalError} =
  VAR
    state            : AttributeState;
    super            : ID;
    classDep, attrDep: IDSet;
  BEGIN
    (* Are class/type and attribute declared ? *)
    AssertNodeClassOrTypeDeclared(scheme, CoT);
    AssertDeclaredByNumber(scheme, dependentAttr, AttributeLabel);
    WITH attrs = GetTargets(scheme, CoT, ToAttribute) DO
      IF NOT attrs.in(dependentAttr) THEN
        attrs.dispose();
        RAISE NotDeclared
      END;
      attrs.dispose();
    END;

    (* read attribute state *)
    TextToAttributeState(
      GetAttribute(scheme, dependentAttr, CoT.entity), state);
    IF state.depSt = State.Undefined THEN
      RETURN NodeSet.New();
    ELSE
      super := state.depDefClass;
      classDep := GetSources(scheme, super, ToDependentCOT);
      attrDep := GetSources(scheme, dependentAttr, ToDependent);
      attrDep.intersection(classDep);
      classDep.dispose();
      RETURN attrDep;
    END;
  END GetDefiningDependencies;

PROCEDURE GetAllDependencies (scheme: T): IDSet RAISES {InternalError} =
  BEGIN
    RETURN GetTargets(scheme, scheme.root, ToDependencyNode);
  END GetAllDependencies;

PROCEDURE DependsOn (scheme: T; cot, attr, dep: ID): BOOLEAN
  RAISES {NotDeclared, InternalError} =
  VAR state: AttributeState;
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, cot);
    AssertDeclaredByNumber(scheme, dep, DependencyLabel);
    AssertDeclaredByNumber(scheme, attr, AttributeLabel);
    IF NOT ExistsEdge(scheme, cot, attr, ToAttribute) THEN
      RAISE NotDeclared;
    END;

    TextToAttributeState(GetAttribute(scheme, attr, cot.entity), state);
    IF state.depSt = State.Undefined THEN
      RETURN FALSE;
    ELSE
      RETURN
        GetSingleTarget(scheme, dep, ToDependentCOT) = state.depDefClass;
    END;
  END DependsOn;

PROCEDURE ShowDependencyInfo (    scheme                      : T;
                                  dep                         : ID;
                              VAR dependentAttr, dependsOnAttr: ID;
                              VAR dependentCOT, dependsOnCOT  : ID;
                              VAR kind    : DependencyKind;
                              VAR edgeType: ID              )
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertDeclaredByNumber(scheme, dep, DependencyLabel);
    TextToDependencyProps(
      GetAttribute(scheme, dep, PropsAttr), edgeType, kind);
    dependentAttr := GetSingleTarget(scheme, dep, ToDependent);
    dependsOnAttr := GetSingleTarget(scheme, dep, ToDefining);
    dependentCOT := GetSingleTarget(scheme, dep, ToDependentCOT);
    dependsOnCOT := GetSingleTarget(scheme, dep, ToDefiningCOT);
  END ShowDependencyInfo;

PROCEDURE DeleteDependencies (scheme: T; CoT, dependentAttr: ID)
  RAISES {NotDeclared, InternalError} =
  BEGIN
    (* Are class/type and attribute declared ? *)
    AssertNodeClassOrTypeDeclared(scheme, CoT);
    AssertDeclaredByNumber(scheme, dependentAttr, AttributeLabel);
    WITH attrs = GetTargets(scheme, CoT, ToAttribute) DO
      IF NOT attrs.in(dependentAttr) THEN
        attrs.dispose();
        RAISE NotDeclared
      END;
      attrs.dispose();
    END;
  END DeleteDependencies;

PROCEDURE GetAllAttributesOfNodeClassOrType (scheme: T; cot: ID): IDSet
  RAISES {NotDeclared, InternalError} =
  BEGIN
    AssertNodeClassOrTypeDeclared(scheme, cot);
    RETURN GetTargets(scheme, cot, ToAttribute);
  END GetAllAttributesOfNodeClassOrType;


(* Auxiliary procedures *)

PROCEDURE AssertNotDeclared (scheme: T; name: TEXT; index: CARDINAL)
  RAISES {AlreadyDeclared, InternalError} =
  VAR
    found: BOOLEAN;
    node : Node.T;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.nameCache.get(CardText.T{index, name}, node, found);
        IF found THEN RAISE AlreadyDeclared END;
      ELSE
        IF NOT scheme.graph.getNodesWithIndex(index, name).isEmpty() THEN
          RAISE AlreadyDeclared;
        END;
      END;
    EXCEPT
    | NameCache.Error (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.Write", "NameCache.Error", info));
    | Access.Locked =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "Scheme.AssertNotDeclared", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.AssertNotDeclared",
                              "PersistentGraph.InternalError", info));
    END;
  END AssertNotDeclared;

PROCEDURE AssertNodeClassOrTypeDeclared (scheme: T; cot: ID)
  RAISES {InternalError, NotDeclared} =
  VAR
    res  : CARDINAL;
    found: BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.labelCache.get(cot, res, found);
        IF NOT found THEN RAISE NotDeclared; END;
      ELSE
        res := scheme.graph.getNodeLabel(cot);
      END;
      IF res # ClassLabel AND res # TypeLabel THEN RAISE NotDeclared END;
    EXCEPT
      PersistentGraph.NodeNotFound => RAISE NotDeclared;
    | PersistentGraph.NotOwner => RAISE NotDeclared;
    | LabelCache.Error (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.AssertNodeClassOrTypeDeclared",
                              "LabelCache.Error", info));
    | Access.Locked =>
        RAISE
          InternalError(
            ErrorSupport.Create(
              "Scheme.AssertNodeClassOrTypeDeclared", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.AssertNodeClassOrTypeDeclared",
                              "PersistentGraph.InternalError", info));
    END;
  END AssertNodeClassOrTypeDeclared;

PROCEDURE AssertDeclaredByNumber (scheme: T; node: ID; label: CARDINAL)
  RAISES {NotDeclared, InternalError} =
  VAR
    res  : CARDINAL;
    found: BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.labelCache.get(node, res, found);
        IF NOT found THEN RAISE NotDeclared; END;
      ELSE
        res := scheme.graph.getNodeLabel(node);
      END;
      IF res # label THEN RAISE NotDeclared END;
    EXCEPT
      PersistentGraph.NodeNotFound => RAISE NotDeclared;
    | PersistentGraph.NotOwner => RAISE NotDeclared;
    | LabelCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Scheme.AssertDeclaredByNumber",
                                       "LabelCache.Error", info));
    | Access.Locked =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.AssertDeclaredByNumber", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.AssertDeclaredByNumber",
                              "PersistentGraph.InternalError", info));
    END;
  END AssertDeclaredByNumber;

PROCEDURE CreateNode (scheme: T;
                      label : CARDINAL;
                      name  : TEXT       := NIL;
                      index : CARDINAL   := 0    ): ID
  RAISES {InternalError} =
  VAR node: ID;
  BEGIN
    TRY
      node := scheme.graph.createNodeNumber(scheme.root);
      node := scheme.graph.createNode(label);
      IF scheme.useCaches THEN
        scheme.labelCache.put(node, label, writeThrough := FALSE);
      END;
      IF name # NIL THEN
        scheme.graph.putIndex(node, index, name);
        IF scheme.useCaches THEN
          scheme.idCache.put(
            NodeCard.T{node, index}, name, writeThrough := FALSE);
          scheme.nameCache.put(
            CardText.T{index, name}, node, writeThrough := FALSE);
        END;
      END;
      RETURN node;
    EXCEPT
      ChgMgmtGraph.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "Scheme.CreateNode", "ChgMgmtGraph.InternalError", info));
    | PersistentGraph.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.CreateNode", "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE InternalError(
                ErrorSupport.Create("Scheme.CreateNode", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.CreateNode",
                              "PersistentGraph.InternalError", info));
    | ChgMgmtGraph.LogError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.CreateNode", "ChgMgmtGraph.LogError", info));
    | PersistentGraph.IndexUsed =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.CreateNode", "PersistentGraph.IndexUsed"));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.CreateNode", "PersistentGraph.NodeNotFound"));
    | LabelCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.CreateNode", "LabelCache.Error", info));
    | NameCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.CreateNode", "NameCache.Error", info));
    | IdCache.Error (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.CreateNode", "IdCache.Error", info));
    END;
  END CreateNode;

PROCEDURE ExistsNamedNode (scheme: T;
                           label : CARDINAL;
                           name  : TEXT;
                           index : CARDINAL  ): BOOLEAN
  RAISES {InternalError} =
  VAR
    res: Node.T;
    lab: CARDINAL;
    set: IDSet;
    ok : BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.nameCache.get(CardText.T{index, name}, res, ok);
        IF ok THEN
          scheme.labelCache.get(res, lab, ok);
          IF NOT ok OR lab # label THEN RETURN FALSE END;
          RETURN TRUE;
        ELSE
          RETURN FALSE;
        END;
      ELSE
        set := scheme.graph.getNodesWithIndex(index, name);
        res := set.extractAnyElement(ok);
        IF NOT ok THEN RETURN FALSE END;
        IF set.card() > 0 THEN
          RAISE InternalError(ErrorSupport.Create(
                                "Scheme.ExistsNamedNode", "CorruptScheme"))
        END;
        IF scheme.graph.getNodeLabel(res) # label THEN RETURN FALSE END;
        RETURN TRUE;
      END;
    EXCEPT
    | NameCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate("Scheme.ExistsNamedNode",
                                               "NameCache.Error", info));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("Scheme.ExistsNamedNode",
                                            "PersistentGraph.NotOwner"));
    | LabelCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate("Scheme.ExistsNamedNode",
                                               "LabelCache.Error", info));
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.ExistsNamedNode", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.ExistsNamedNode",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.ExistsNamedNode",
                              "PersistentGraph.NodeNotFound"));
    END;
  END ExistsNamedNode;

PROCEDURE ExistsNodeWithLabel (scheme: T; number: ID; label: CARDINAL):
  BOOLEAN RAISES {InternalError} =
  VAR
    res  : CARDINAL;
    found: BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.labelCache.get(number, res, found);
        IF NOT found THEN RETURN FALSE; END;
      ELSE
        res := scheme.graph.getNodeLabel(number);
      END;
      RETURN label = res;
    EXCEPT
      PersistentGraph.NodeNotFound => RETURN FALSE;
    | PersistentGraph.NotOwner => RETURN FALSE;
    | Access.Locked =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "Scheme.ExistsNodeWithLabel", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.ExistsNodeWithLabel",
                              "PersistentGraph.InternalError", info));
    | LabelCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.ExistsNodeWithLabel", "LabelCache.Error", info));
    END;
  END ExistsNodeWithLabel;

PROCEDURE GetNamedNode (scheme: T;
                        label : CARDINAL;
                        name  : TEXT;
                        index : CARDINAL  ): ID
  RAISES {NotDeclared, InternalError} =
  VAR
    res: Node.T;
    lab: CARDINAL;
    set: IDSet;
    ok : BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.nameCache.get(CardText.T{index, name}, res, ok);
        IF NOT ok THEN RAISE NotDeclared END;
        scheme.labelCache.get(res, lab, ok);
        IF NOT ok THEN
          RAISE InternalError(ErrorSupport.Create(
                                "Scheme.GetNamedNode", "CorruptScheme"))
        END;
        IF lab # label THEN RAISE NotDeclared END;
      ELSE
        set := scheme.graph.getNodesWithIndex(index, name);
        res := set.extractAnyElement(ok);
        IF NOT ok THEN RAISE NotDeclared END;
        IF set.card() > 0 THEN
          RAISE InternalError(ErrorSupport.Create(
                                "Scheme.GetNamedNode", "CorruptScheme"))
        END;
        IF scheme.graph.getNodeLabel(res) # label THEN
          RAISE NotDeclared;
        END;
      END;
      RETURN res;
    EXCEPT
    | LabelCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.GetNamedNode", "LabelCache.Error", info));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("Scheme.GetNamedNode",
                                            "PersistentGraph.NotOwner"));
    | NameCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.GetNamedNode", "NameCache.Error", info));
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetNamedNode", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetNamedNode",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.GetNamedNode", "PersistentGraph.NodeNotFound"));
    END;
  END GetNamedNode;

PROCEDURE GetNodesWithIndex (scheme: T; index: CARDINAL; name: TEXT): IDSet
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN scheme.graph.getNodesWithIndex(index, name);
    EXCEPT
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetNamedNode", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetNamedNode",
                              "PersistentGraph.InternalError", info));
    END;
  END GetNodesWithIndex;


PROCEDURE GetNodeName (scheme: T; node: ID; label, index: CARDINAL): TEXT
  RAISES {NotDeclared, InternalError} =
  VAR
    res  : CARDINAL;
    found: BOOLEAN;
    val  : TEXT;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.labelCache.get(node, res, found);
        IF NOT found OR res # label THEN RAISE NotDeclared END;
        scheme.idCache.get(NodeCard.T{node, index}, val, found);
      ELSE
        res := scheme.graph.getNodeLabel(node);
        IF res # label THEN RAISE NotDeclared END;
        val := scheme.graph.getIndex(node, index, found);
      END;
      IF NOT found THEN
        RAISE
          InternalError(
            ErrorSupport.Create("Scheme.GetNodeName", "CorruptScheme"));
      END;
      RETURN val;
    EXCEPT
      PersistentGraph.NodeNotFound => RAISE NotDeclared;
    | PersistentGraph.NotOwner => RAISE NotDeclared;
    | Access.Locked =>
        RAISE
          InternalError(
            ErrorSupport.Create("Scheme.GetNodeName", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetNodeName",
                              "PersistentGraph.InternalError", info));
    | LabelCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.GetNodeName", "LabelCache.Error", info));
    | IdCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.GetNodeName", "IdCache.Error", info));
    END;
  END GetNodeName;

(* Make class a sub class of super.  Propagates attributes from super to
   class and its sub classes/types. *)
PROCEDURE MakeSubClassOrType (scheme: T; class, super: ID; type: BOOLEAN)
  RAISES {AttributeNameClash, AttributePlacementClash, Cyclic,
          InternalError} =
  VAR
    subClasses     : IDSet;      (* all sub classes of class *)
    superClasses   : IDSet;      (* all super classes of super *)
    attributes     : IDSet;      (* all attributes of super *)
    attr           : ID;         (* one of supers attributes *)
    afound, ok     : BOOLEAN;
    stopSet, etypes: IDSet;
    attrname       : TEXT;
    drawToSubclassClosure: DrawEdgesClosure;
    edgeTypesClosure     : DrawEdgesClosure;
    propagateClosure     : PropagateAttributeClosure;
  BEGIN
    TRY
      (* check for cycle in inheritance relationship *)
      subClasses := GetTargets(scheme, class, ToSubClassOrTypeTC);
      subClasses.insert(class);
      IF (subClasses.in(super)) THEN RAISE Cyclic; END;
      (* draw a ToSubclass/ToSubtype edge *)
      IF type THEN
        CreateEdge(scheme, super, class, ToSubtype);
      ELSE
        CreateEdge(scheme, super, class, ToSubclass);
      END;

      (* propagate ToSubClassOrTypeTC edges from all super types to class
         and its sub classes/types *)
      superClasses := GetSources(scheme, super, ToSubClassOrTypeTC);
      superClasses.insert(super);
      drawToSubclassClosure :=
        NEW(DrawEdgesClosure, scheme := scheme, type := ToSubClassOrTypeTC,
            nodes := superClasses, isSource := TRUE);
      Map(drawToSubclassClosure, subClasses);
      superClasses.dispose();

      (* propagate Incoming/OutgoingEdgeType edges from all edges of super
         to class and its sub classes/types *)
      etypes := GetTargets(scheme, super, IncomingEdgeType);
      edgeTypesClosure :=
        NEW(DrawEdgesClosure, scheme := scheme, type := IncomingEdgeType,
            nodes := subClasses, isSource := TRUE);
      Map(edgeTypesClosure, etypes);
      etypes.dispose();

      etypes := GetTargets(scheme, super, OutgoingEdgeType);
      edgeTypesClosure.type := OutgoingEdgeType;
      Map(edgeTypesClosure, etypes);
      subClasses.dispose();
      etypes.dispose();

      (* Propagate all attributes of super to class and its sub classes *)
      propagateClosure := NEW(PropagateAttributeClosure);
      attributes := GetTargets(scheme, super, ToAttribute);
      attributes.loop();
      attr := attributes.get(afound);
      WHILE afound DO
        (* initialize closure *)
        propagateClosure.attr := attr;
        TextToAttributeState(
          GetAttribute(scheme, attr, super.entity), propagateClosure.state);
        IF propagateClosure.state.plcSt = State.Defining THEN
          propagateClosure.state.plcSt := State.Inherited;
        END;
        IF propagateClosure.state.depSt = State.Defining THEN
          propagateClosure.state.depSt := State.Inherited;
        END;
        IF propagateClosure.state.evalFuncSt = State.Defining THEN
          propagateClosure.state.evalFuncSt := State.Inherited;
        END;
        (* find all attributedefinitions with the same name, except attr
           itself *)
        attrname :=
          GetNodeName(scheme, attr, AttributeLabel, AttributeNameIndex);
        propagateClosure.attrWithSameName :=
          GetNodesWithIndex(scheme, AttributeNameIndex, attrname);
        propagateClosure.attrWithSameName.deleteElement(attr, ok);
        (* find guardian if any *)
        WITH guards = GetTargets(scheme, attr, ToGuard) DO
          guards.loop();
          propagateClosure.guard := guards.get(propagateClosure.guarded);
          guards.dispose();
        END;
        Propagate(propagateClosure, scheme, class, stopSet);
        IF NOT stopSet.isEmpty() THEN
          CASE propagateClosure.conflict OF
          | Conflict.Naming => RAISE AttributeNameClash;
          | Conflict.Placement => RAISE AttributePlacementClash;
          ELSE
          END;
        END;
        attr := attributes.get(afound);
      END;
      attributes.dispose();

    EXCEPT
    | NotDeclared =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.MakeSubClassOrType", "NotDeclared"));
    END;
  END MakeSubClassOrType;

PROCEDURE EdgeTypeConnects (scheme: T; sourceCOT, targetCOT, edgeType: ID):
  BOOLEAN RAISES {InternalError} =
  VAR
    defSource, defTarget        : ID;
    defSourceSubs, defTargetSubs: IDSet;
    res                         : BOOLEAN;
  BEGIN
    defSource := GetSingleTarget(scheme, edgeType, ToSource);
    defTarget := GetSingleTarget(scheme, edgeType, ToTarget);
    defSourceSubs := GetTargets(scheme, defSource, ToSubClassOrTypeTC);
    defSourceSubs.insert(defSource);
    defTargetSubs := GetTargets(scheme, defTarget, ToSubClassOrTypeTC);
    defTargetSubs.insert(defTarget);
    res := defSourceSubs.in(sourceCOT) AND defTargetSubs.in(targetCOT);
    defSourceSubs.dispose();
    defTargetSubs.dispose();
    RETURN res;
  END EdgeTypeConnects;

PROCEDURE CreateEdge (scheme: T; source, target: ID; label: CARDINAL)
  RAISES {InternalError} =
  BEGIN
    TRY
      IF NOT scheme.graph.existsEdge(source, target, label) THEN
        scheme.graph.createEdge(source, target, label);
      END;
    EXCEPT
      PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.CreateEdge", "Corrupt scheme information."));
    | PersistentGraph.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.CreateEdge", "PersistentGraph.NotOwner"));
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "Scheme.CreateEdge", "ChgMgmtGraph.InternalError", info));
    | Access.Locked =>
        RAISE InternalError(
                ErrorSupport.Create("Scheme.CreateEdge", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.CreateEdge",
                              "PersistentGraph.InternalError", info));
    | ChgMgmtGraph.LogError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.CreateEdge", "ChgMgmtGraph.LogError", info));
    END;
  END CreateEdge;

PROCEDURE ExistsEdge (scheme: T; source, target: ID; label: CARDINAL):
  BOOLEAN RAISES {InternalError} =
  BEGIN
    TRY
      RETURN scheme.graph.existsEdge(source, target, label);
    EXCEPT
    | PersistentGraph.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.ExistsEdge", "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE InternalError(
                ErrorSupport.Create("Scheme.ExistsEdge", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.ExistsEdge",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.ExistsEdge", "PersistentGraph.NodeNotFound"));
    END
  END ExistsEdge;

PROCEDURE GetTargets (scheme: T; source: ID; label: CARDINAL): IDSet
  RAISES {InternalError} =
  VAR
    res  : IDSet;
    found: BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.targetCache.get(NodeCard.T{source, label}, res, found);
        IF NOT found THEN
          RAISE InternalError(
                  ErrorSupport.Create(
                    "Scheme.GetTargets", "PersistentGraph.NodeNotFound"));
        END;
        RETURN res.copy();
      ELSE
        RETURN scheme.graph.getTargets(source, label);
      END;
    EXCEPT
    | PersistentGraph.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.GetTargets", "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE InternalError(
                ErrorSupport.Create("Scheme.GetTargets", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetTargets",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.GetTargets", "PersistentGraph.NodeNotFound"));
    | TargetCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.GetTargets", "TargetCache.Error", info));
    END
  END GetTargets;

PROCEDURE GetSources (scheme: T; target: ID; label: CARDINAL): IDSet
  RAISES {InternalError} =
  VAR
    res  : IDSet;
    found: BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.sourceCache.get(NodeCard.T{target, label}, res, found);
        IF NOT found THEN
          RAISE InternalError(
                  ErrorSupport.Create(
                    "Scheme.GetSources", "PersistentGraph.NodeNotFound"));
        END;
        RETURN res.copy();
      ELSE
        RETURN scheme.graph.getSources(target, label);
      END;
    EXCEPT
    | PersistentGraph.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.GetSources", "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE InternalError(
                ErrorSupport.Create("Scheme.GetSources", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetSources",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.GetSources", "PersistentGraph.NodeNotFound"));
    | SourceCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Scheme.GetSources", "SourceCache.Error", info));
    END
  END GetSources;

PROCEDURE GetSingleSource (scheme: T; target: ID; label: CARDINAL): ID
  RAISES {InternalError} =
  VAR
    sources: IDSet;
    res    : ID;
    ok     : BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.sourceCache.get(NodeCard.T{target, label}, sources, ok);
        IF NOT ok THEN
          RAISE InternalError(
                  ErrorSupport.Create("Scheme.GetSingleSource",
                                      "None or more than one edge."));
        END;
      ELSE
        sources := scheme.graph.getSources(target, label);
      END;
      (* set shouldn't be changed when cached *)
      sources.loop();
      res := sources.get(ok);
      IF NOT ok OR sources.card() # 1 THEN
        RAISE
          InternalError(ErrorSupport.Create("Scheme.GetSingleSource",
                                            "None or more than one edge."));
      END;
      RETURN res;
    EXCEPT
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("Scheme.GetSingleSource",
                                            "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetSingleSource", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetSingleSource",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetSingleSource",
                              "PersistentGraph.NodeNotFound"));
    | SourceCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate("Scheme.GetSingleSource",
                                               "SourceCache.Error", info));
    END
  END GetSingleSource;

PROCEDURE GetSingleTarget (scheme: T; source: ID; label: CARDINAL): ID
  RAISES {InternalError} =
  VAR
    targets: IDSet;
    res    : ID;
    ok     : BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.targetCache.get(NodeCard.T{source, label}, targets, ok);
        IF NOT ok THEN
          RAISE InternalError(
                  ErrorSupport.Create("Scheme.GetSingleTarget",
                                      "None or more than one edge."));
        END;
      ELSE
        targets := scheme.graph.getTargets(source, label);
      END;
      (* set shouldn't be changed when cached *)
      targets.loop();
      res := targets.get(ok);
      IF NOT ok OR targets.card() # 1 THEN
        RAISE
          InternalError(ErrorSupport.Create("Scheme.GetSingleTarget",
                                            "None or more than one edge."));
      END;
      RETURN res;
    EXCEPT
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("Scheme.GetSingleTarget",
                                            "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetSingleTarget", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetSingleTarget",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetSingleTarget",
                              "PersistentGraph.NodeNotFound"));
    | TargetCache.Error (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate("Scheme.GetSingleTarget",
                                               "TargetCache.Error", info));
    END
  END GetSingleTarget;


PROCEDURE HasAttributeByName (    scheme     : T;
                                  classOrType: ID;
                                  name       : TEXT;
                              VAR attr       : ID    ): BOOLEAN
  RAISES {InternalError} =
  VAR
    Attribs: IDSet;
    ok     : BOOLEAN;
  BEGIN
    TRY
      Attribs := scheme.graph.getNodesWithIndex(AttributeNameIndex, name);
      WITH cotattrs = GetTargets(scheme, classOrType, ToAttribute) DO
        Attribs.intersection(cotattrs);
        cotattrs.dispose();
      END;
      IF Attribs.isEmpty() THEN
        RETURN FALSE;
      ELSE
        attr := Attribs.extractAnyElement(ok);
        IF NOT Attribs.isEmpty() THEN
          RAISE InternalError(
                  ErrorSupport.Create("Scheme.HasAttributeByName",
                                      "Corrupt scheme information"));
        END;
        RETURN TRUE;
      END;
    EXCEPT
    | Access.Locked =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "Scheme.HasAttributeByName", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.HasAttributeByName",
                              "PersistentGraph.InternalError", info));
    END;
  END HasAttributeByName;

PROCEDURE PutAttribute (scheme: T; node: ID; attrno: CARDINAL; value: TEXT)
  RAISES {InternalError} =
  BEGIN
    TRY
      scheme.graph.putAttribute(node, attrno, 0, value);
      IF scheme.useCaches THEN
        scheme.attrCache.put(
          NodeCard.T{node, attrno}, value, writeThrough := FALSE);
      END;
    EXCEPT
      PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.PutAttribute", "PersistentGraph.NodeNotFound"));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("Scheme.PutAttribute",
                                            "PersistentGraph.NotOwner"));
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "Scheme.PutAttribute", "ChgMgmtGraph.InternalError", info));
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.PutAttribute", "Access.Locked"));
    | ChgMgmtGraph.LogError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.PutAttribute", "ChgMgmtGraph.LogError", info));
    | AttributeCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.PutAttribute", "AttributeCache.Error", info));
    END;
  END PutAttribute;

PROCEDURE GetAttribute (scheme: T; node: ID; attrno: CARDINAL): TEXT
  RAISES {InternalError} =
  VAR
    res  : TEXT;
    found: BOOLEAN;
  BEGIN
    TRY
      IF scheme.useCaches THEN
        scheme.attrCache.get(NodeCard.T{node, attrno}, res, found);
        IF NOT found THEN
          RAISE InternalError(ErrorSupport.Create(
                                "Scheme.GetAttribute", "NodeNotFound"));
        END;
      ELSE
        res := scheme.graph.getAttribute(node, attrno, 0, LAST(CARDINAL));
      END;
      RETURN res;
    EXCEPT
      PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.GetAttribute", "PersistentGraph.NodeNotFound"));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("Scheme.GetAttribute",
                                            "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE InternalError(ErrorSupport.Create(
                              "Scheme.GetAttribute", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Scheme.GetAttribute",
                              "PersistentGraph.InternalError", info));
    | AttributeCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.GetAttribute", "AttributeCache.Error", info));
    END
  END GetAttribute;

PROCEDURE EdgeCardsToText (sc, tc: Cardinality): TEXT =
  VAR res: TEXT;
  BEGIN
    res := AttributeValue.CardToText(sc.min);
    res := res & AttributeValue.CardToText(sc.max);
    res := res & AttributeValue.CardToText(tc.min);
    res := res & AttributeValue.CardToText(tc.max);
    RETURN res;
  END EdgeCardsToText;

PROCEDURE TextToEdgeCards (t: TEXT; VAR sc, tc: Cardinality)
  RAISES {InternalError} =
  BEGIN
    IF Text.Length(t) # 2 * BYTESIZE(Cardinality) THEN
      RAISE InternalError(ErrorSupport.Create("Scheme.TextToEdgeCards",
                                              "Text has wrong length."));
    END;
    sc.min := AttributeValue.TextToCard(Text.Sub(t, 0, 4));
    sc.max := AttributeValue.TextToCard(Text.Sub(t, 4, 4));
    tc.min := AttributeValue.TextToCard(Text.Sub(t, 8, 4));
    tc.max := AttributeValue.TextToCard(Text.Sub(t, 12, 4));
  END TextToEdgeCards;

PROCEDURE AttrPropsToText (kind    : AttributeKind;
                           props   : IndexProperties;
                           type    : CARDINAL;
                           card    : Cardinality;
                           constLen: BOOLEAN;
                           len     : CARDINAL         ): TEXT =
  VAR res: TEXT;
  BEGIN
    res := AttributeValue.IntToText(ORD(kind));
    res := res & AttributeValue.IntToText(ORD(props));
    res := res & AttributeValue.CardToText(type);
    res := res & AttributeValue.CardToText(card.min);
    res := res & AttributeValue.CardToText(card.max);
    res := res & AttributeValue.IntToText(ORD(constLen));
    res := res & AttributeValue.CardToText(len);
    RETURN res;
  END AttrPropsToText;

PROCEDURE TextToAttrProps (    t       : TEXT;
                           VAR kind    : AttributeKind;
                           VAR props   : IndexProperties;
                           VAR type    : CARDINAL;
                           VAR card    : Cardinality;
                           VAR constLen: BOOLEAN;
                           VAR len     : CARDINAL         )
  RAISES {InternalError} =
  BEGIN
    IF Text.Length(t) # 7 * BYTESIZE(INTEGER) THEN
      RAISE InternalError(ErrorSupport.Create("Scheme.TextToAttrProps",
                                              "Text has wrong length."));
    END;
    kind :=
      VAL(AttributeValue.TextToInt(Text.Sub(t, 0, 4)), AttributeKind);
    props :=
      VAL(AttributeValue.TextToInt(Text.Sub(t, 4, 4)), IndexProperties);
    type := AttributeValue.TextToCard(Text.Sub(t, 8, 4));
    card.min := AttributeValue.TextToCard(Text.Sub(t, 12, 4));
    card.max := AttributeValue.TextToCard(Text.Sub(t, 16, 4));
    constLen := VAL(AttributeValue.TextToInt(Text.Sub(t, 20, 4)), BOOLEAN);
    len := AttributeValue.TextToCard(Text.Sub(t, 24, 4));
  END TextToAttrProps;


PROCEDURE AttributeStateToText (READONLY state: AttributeState): TEXT =
  VAR res: TEXT;
  BEGIN
    res := AttributeValue.IntToText(ORD(state.plcSt));
    res := res & AttributeValue.CardToText(state.plcDefCOT.graph);
    res := res & AttributeValue.CardToText(state.plcDefCOT.entity);
    res := res & AttributeValue.CardToText(state.plcPhysNo);
    res := res & AttributeValue.CardToText(state.plcStart);
    res := res & AttributeValue.IntToText(state.plcLen);
    res := res & AttributeValue.IntToText(ORD(state.depSt));
    res := res & AttributeValue.CardToText(state.depDefClass.graph);
    res := res & AttributeValue.CardToText(state.depDefClass.entity);
    res := res & AttributeValue.IntToText(ORD(state.evalFuncSt));
    res := res & AttributeValue.CardToText(state.evalDefClass.graph);
    res := res & AttributeValue.CardToText(state.evalDefClass.entity);
    res := res & state.evalFuncName;
    RETURN res;
  END AttributeStateToText;

PROCEDURE TextToAttributeState (t: TEXT; VAR state: AttributeState) =
  BEGIN
    IF Text.Length(t) >= 12 * BYTESIZE(INTEGER) THEN
      state.plcSt :=
        VAL(AttributeValue.TextToInt(Text.Sub(t, 0, 4)), State);
      state.plcDefCOT.graph :=
        AttributeValue.TextToCard(Text.Sub(t, 4, 4));
      state.plcDefCOT.entity :=
        AttributeValue.TextToCard(Text.Sub(t, 8, 4));
      state.plcPhysNo := AttributeValue.TextToCard(Text.Sub(t, 12, 4));
      state.plcStart := AttributeValue.TextToCard(Text.Sub(t, 16, 4));
      state.plcLen := AttributeValue.TextToInt(Text.Sub(t, 20, 4));
      state.depSt :=
        VAL(AttributeValue.TextToInt(Text.Sub(t, 24, 4)), State);
      state.depDefClass.graph :=
        AttributeValue.TextToCard(Text.Sub(t, 28, 4));
      state.depDefClass.entity :=
        AttributeValue.TextToCard(Text.Sub(t, 32, 4));
      state.evalFuncSt :=
        VAL(AttributeValue.TextToInt(Text.Sub(t, 36, 4)), State);
      state.evalDefClass.graph :=
        AttributeValue.TextToCard(Text.Sub(t, 40, 4));
      state.evalDefClass.entity :=
        AttributeValue.TextToCard(Text.Sub(t, 44, 4));
      state.evalFuncName := Text.Sub(t, 48);
    ELSE
      state.depSt := State.Undefined;
      state.depDefClass := ID{0, 0};
      state.evalFuncSt := State.Undefined;
      state.evalDefClass := ID{0, 0};
      state.evalFuncName := "";
    END;
  END TextToAttributeState;

PROCEDURE DependencyPropsToText (edgeType: ID; kind: DependencyKind):
  TEXT =
  VAR res: TEXT;
  BEGIN
    res := AttributeValue.CardToText(edgeType.graph);
    res := res & AttributeValue.CardToText(edgeType.entity);
    res := res & AttributeValue.IntToText(ORD(kind));
    RETURN res;
  END DependencyPropsToText;

PROCEDURE TextToDependencyProps (    t       : TEXT;
                                 VAR edgeType: ID;
                                 VAR kind    : DependencyKind)
  RAISES {InternalError} =
  BEGIN
    IF Text.Length(t) # 3 * BYTESIZE(INTEGER) THEN
      RAISE
        InternalError(ErrorSupport.Create("Scheme.TextToDependencyProps",
                                          "Text has wrong length."));
    ELSE
      edgeType.graph := AttributeValue.TextToCard(Text.Sub(t, 0, 4));
      edgeType.entity := AttributeValue.TextToCard(Text.Sub(t, 4, 4));
      kind :=
        VAL(AttributeValue.TextToInt(Text.Sub(t, 8, 4)), DependencyKind);
    END
  END TextToDependencyProps;

TYPE
  MapClosure = OBJECT METHODS do (arg: ID) RAISES {InternalError} END;

  (* With DrawEdgeClosure it is possible to draw an edge between a node and
     a set of nodes *)
  DrawEdgeClosure = MapClosure OBJECT
                      scheme  : T;
                      node    : ID;
                      type    : CARDINAL;
                      isSource: BOOLEAN;
                    OVERRIDES
                      do := DoDrawEdge;
                    END;

PROCEDURE DoDrawEdge (clos: DrawEdgeClosure; arg: ID)
  RAISES {InternalError} =
  BEGIN
    IF clos.isSource THEN
      CreateEdge(clos.scheme, clos.node, arg, clos.type);
    ELSE
      CreateEdge(clos.scheme, arg, clos.node, clos.type);
    END;
  END DoDrawEdge;

(* With DrawEdgeClosure it is possible to draw an edge between a node and a
   set of nodes *)
TYPE
  DrawEdgesClosure = MapClosure OBJECT
                       scheme  : T;
                       type    : CARDINAL;
                       nodes   : IDSet;
                       isSource: BOOLEAN;
                     OVERRIDES
                       do := DoConnect;
                     END;

PROCEDURE DoConnect (clos: DrawEdgesClosure; arg: ID)
  RAISES {InternalError} =
  VAR connect1N: DrawEdgeClosure;
  BEGIN
    connect1N :=
      NEW(DrawEdgeClosure, scheme := clos.scheme, type := clos.type,
          node := arg, isSource := NOT clos.isSource);
    Map(connect1N, clos.nodes);
  END DoConnect;


(* A closure that puts an attribute *)
TYPE
  PutAttributeClosure = MapClosure OBJECT
                          scheme: T;
                          number: CARDINAL;
                          begin : CARDINAL;
                          value : TEXT;
                        OVERRIDES
                          do := DoPutAttr;
                        END;

PROCEDURE DoPutAttr (clos: PutAttributeClosure; arg: ID)
  RAISES {InternalError} =
  BEGIN
    TRY
      clos.scheme.graph.putAttribute(
        arg, clos.number, clos.begin, clos.value);
    EXCEPT
      PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "Scheme.PutAttribute", "PersistentGraph.NodeNotFound"));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "Scheme.DoPutAttr", "PersistentGraph.NotOwner"));
    | ChgMgmtGraph.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.DoPutAttr", "ChgMgmtGraph.InternalError", info));
    | Access.Locked =>
        RAISE InternalError(
                ErrorSupport.Create("Scheme.DoPutAttr", "Access.Locked"));
    | ChgMgmtGraph.LogError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Scheme.DoPutAttr", "ChgMgmtGraph.LogError", info));
    END;
  END DoPutAttr;


(* Apply a MapClosure to a set of cardinal values *)
PROCEDURE Map (clos: MapClosure; set: IDSet) RAISES {InternalError} =
  VAR
    ok : BOOLEAN;
    arg: ID;
  BEGIN
    set.loop();
    arg := set.get(ok);
    WHILE ok DO clos.do(arg); arg := set.get(ok); END;
  END Map;


TYPE
  PropagateClosure =
    OBJECT
    METHODS
      do (scheme: T; cot: ID; VAR stop: BOOLEAN) RAISES {InternalError};
    END;

TYPE
  PropagateAttributeClosure =
    PropagateClosure OBJECT
      attr: ID;
      (* The attribute (node) number to propagate *)
      attrWithSameName: IDSet;   (* all declared attributes with the same
                                    name - {attr} *)
      state   : AttributeState;
      guarded : BOOLEAN;         (* The attribute is guarded *)
      guard   : ID;              (* and this is its guard *)
      conflict: Conflict;
    OVERRIDES
      do := DoPropagateAttribute;
    END;

PROCEDURE DoPropagateAttribute (    clos  : PropagateAttributeClosure;
                                    scheme: T;
                                    cot   : ID;
                                VAR stop  : BOOLEAN                    )
  RAISES {InternalError} =
  VAR
    attribs : IDSet;
    oldattr : ID;
    ok      : BOOLEAN;
    oldstate: AttributeState;
  BEGIN
    attribs := GetTargets(scheme, cot, ToAttribute);
    attribs.intersection(clos.attrWithSameName);
    IF NOT attribs.isEmpty() THEN
      (* This is an attribute name clash and must be handled by the calling
         routine *)
      attribs.dispose();
      stop := TRUE;
      clos.conflict := Conflict.Naming;
    ELSE
      attribs.dispose();
      (* Check for placement conflicts.  We have to do this every time for
         all attributes, because any attribute might define/inherit
         placement information for its attributes separately. *)
      attribs := GetTargets(scheme, cot, ToAttribute);
      attribs.loop();
      oldattr := attribs.get(ok);
      WHILE ok DO
        TextToAttributeState(
          GetAttribute(scheme, oldattr, cot.entity), oldstate);
        IF oldattr # clos.attr
             AND NOT DisjunctPlacements(clos.state, oldstate) THEN
          stop := TRUE;
          clos.conflict := Conflict.Placement;
          RETURN;
        END;
        oldattr := attribs.get(ok);
      END;
      attribs.dispose();

      stop := FALSE;
      CreateEdge(scheme, cot, clos.attr, ToAttribute);
      PutAttribute(
        scheme, clos.attr, cot.entity, AttributeStateToText(clos.state));
      IF clos.guarded THEN
        CreateEdge(scheme, cot, clos.guard, ToAttributeGuard);
      END;
    END;
  END DoPropagateAttribute;

TYPE
  PropagateEvalFuncClosure =
    PropagateClosure OBJECT
      func: TEXT;                (* The name of the evaluation function *)
      attr: ID;                  (* for which attribute? *)
      from: ID;                  (* The class from which we inherit *)
    OVERRIDES
      do := DoPropagateEvalFunc;
    END;

PROCEDURE DoPropagateEvalFunc (    clos  : PropagateEvalFuncClosure;
                                   scheme: T;
                                   cot   : ID;
                               VAR stop  : BOOLEAN                   )
  RAISES {InternalError} =
  VAR
    atval: TEXT;
    state: AttributeState;
  BEGIN
    atval := GetAttribute(scheme, clos.attr, cot.entity);
    TextToAttributeState(atval, state);
    IF state.evalFuncSt = State.Undefined
         OR (state.evalFuncSt = State.Inherited
               AND ExistsEdge(scheme, state.evalDefClass, clos.from,
                              ToSubClassOrTypeTC))
         OR state.evalDefClass = clos.from THEN
      (* eihter the class had no evaluation status for this attribute, it
         inherits the status from a super class of clos.from, or it defines
         the status and is the start class; anyway, we must write the new
         status and continue with propagation *)
      IF cot = clos.from THEN
        state.evalFuncSt := State.Defining;
      ELSE
        state.evalFuncSt := State.Inherited;
      END;
      state.evalDefClass := clos.from;
      state.evalFuncName := clos.func;
      PutAttribute(
        scheme, clos.attr, cot.entity, AttributeStateToText(state));
      stop := FALSE;
    ELSE
      (* this class defines an evaluation status for this attribute or
         already inherits one from a subclass of clos.from *)
      stop := TRUE;
    END;
  END DoPropagateEvalFunc;

TYPE
  PropagateDependencyClosure =
    PropagateClosure OBJECT
      attr  : ID;                (* for which attribute? *)
      depCOT: ID;                (* The class from which we inherit *)
    OVERRIDES
      do := DoPropagateDependency;
    END;

PROCEDURE DoPropagateDependency (    clos  : PropagateDependencyClosure;
                                     scheme: T;
                                     cot   : ID;
                                 VAR stop  : BOOLEAN                     )
  RAISES {InternalError} =
  VAR
    atval: TEXT;
    state: AttributeState;
  BEGIN
    atval := GetAttribute(scheme, clos.attr, cot.entity);
    TextToAttributeState(atval, state);
    IF state.depSt = State.Undefined
         OR (state.depSt = State.Inherited
               AND ExistsEdge(scheme, state.depDefClass, clos.depCOT,
                              ToSubClassOrTypeTC))
         OR state.depDefClass = clos.depCOT THEN
      (* eihter the class had no dependency status for this attribute, it
         inherits the status from a super class of depCOT, or it defines
         the status and is the start class; anyway, we must write the new
         status and continue with propagation *)
      IF cot = clos.depCOT THEN
        state.depSt := State.Defining;
      ELSE
        state.depSt := State.Inherited;
      END;
      state.depDefClass := clos.depCOT;
      PutAttribute(
        scheme, clos.attr, cot.entity, AttributeStateToText(state));
      stop := FALSE;
    ELSE
      (* this class defines a dependency status for this attribute or
         already inherits one from a sub class of clos.depCOT *)
      stop := TRUE;
    END;
  END DoPropagateDependency;


TYPE
  PropagatePlacementClosure =
    PropagateClosure OBJECT
      attr  : ID;                (* for which attribute? *)
      defCOT: ID;                (* The class from which we inherit *)
      phys, start: CARDINAL;     (* placement *)
      len        : INTEGER;
      conflict   : Conflict;
    OVERRIDES
      do := DoPropagatePlacement;
    END;

PROCEDURE DoPropagatePlacement (    clos  : PropagatePlacementClosure;
                                    scheme: T;
                                    cot   : ID;
                                VAR stop  : BOOLEAN                    )
  RAISES {InternalError} =
  VAR
    atval   : TEXT;
    state   : AttributeState;
    attribs : IDSet;
    oldattr : ID;
    ok      : BOOLEAN;
    oldstate: AttributeState;

  BEGIN
    atval := GetAttribute(scheme, clos.attr, cot.entity);
    TextToAttributeState(atval, state);

    IF state.plcSt = State.Undefined
         OR (state.plcSt = State.Inherited
               AND ExistsEdge(scheme, state.plcDefCOT, clos.defCOT,
                              ToSubClassOrTypeTC))
         OR state.plcDefCOT = clos.defCOT THEN
      (* eihter the class had no dependency status for this attribute, it
         inherits the status from a super class of defCOT, or it defines
         the status and is the start class; anyway, we must write the new
         status and continue with propagation *)

      (* Check for conflicts *)
      attribs := GetTargets(scheme, cot, ToAttribute);
      attribs.loop();
      oldattr := attribs.get(ok);
      WHILE ok DO
        TextToAttributeState(
          GetAttribute(scheme, oldattr, cot.entity), oldstate);
        IF clos.attr # oldattr AND NOT DisjunctPlacements(state, oldstate) THEN
          stop := TRUE;
          clos.conflict := Conflict.Placement;
          attribs.dispose();
          RETURN;
        END;
        oldattr := attribs.get(ok);
      END;
      attribs.dispose();

      IF cot = clos.defCOT THEN
        state.plcSt := State.Defining;
      ELSE
        state.plcSt := State.Inherited;
      END;
      state.plcDefCOT := clos.defCOT;
      state.plcPhysNo := clos.phys;
      state.plcStart := clos.start;
      state.plcLen := clos.len;
      PutAttribute(
        scheme, clos.attr, cot.entity, AttributeStateToText(state));
      stop := FALSE;
    ELSE
      (* this class defines a dependency status for this attribute or
         already inherits one from a sub class of clos.depCOT.  No need to
         propagate further or check for conflicts. *)
      clos.conflict := Conflict.None;
      stop := TRUE;
    END;
  END DoPropagatePlacement;


(* Apply a PropagateClosure to a class and all of its sub classes and
   types. *)
PROCEDURE Propagate (    clos   : PropagateClosure;
                         scheme : T;
                         class  : ID;
                     VAR StopSet: IDSet             )
  RAISES {InternalError} =
  VAR
    ok, stop                    : BOOLEAN;
    actClass                    : ID;
    directSucc, applyTo, doneSet: IDSet;
  BEGIN
    (* Only create stopSet, if it wasn't *)
    IF StopSet = NIL THEN StopSet := NodeSet.New(); END;
    (* The first class to handle is the argument class *)
    applyTo := NodeSet.New();
    applyTo.insert(class);
    (* We haven't done anything so far, doneSet is empty *)
    doneSet := NodeSet.New();

    (* main loop *)
    actClass := applyTo.extractAnyElement(ok);
    WHILE ok DO
      doneSet.insert(actClass);
      clos.do(scheme, actClass, stop);
      IF stop THEN
        StopSet.insert(actClass);
      ELSE
        (* add direct successors to applyTo *)
        directSucc := GetTargets(scheme, actClass, ToSubclass);
        directSucc.union(GetTargets(scheme, actClass, ToSubtype));
        directSucc.difference(doneSet);
        applyTo.union(directSucc);
        directSucc.dispose();
      END;
      actClass := applyTo.extractAnyElement(ok);
    END;
  END Propagate;

PROCEDURE DisjunctPlacements (READONLY st1, st2: AttributeState): BOOLEAN =
  BEGIN
    IF st1.plcPhysNo # st2.plcPhysNo THEN
      (* mapped to different physical attributes *)
      RETURN TRUE;
    ELSE
      IF st1.plcLen # Unlimited
           AND st1.plcStart + st1.plcLen < st2.plcStart THEN
        (* placement of st1 is entirely before placement of st2 *)
        RETURN TRUE;
      ELSIF st2.plcLen # Unlimited
              AND st2.plcStart + st2.plcLen < st1.plcStart THEN
        (* placement of st2 is entirely before placement of st1 *)
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;
  END DisjunctPlacements;

BEGIN
END Scheme.
