MODULE TypedGraph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:35  hosking
    Initial revision

    Revision 1.16  1998/06/10 10:57:06  kluck
    Definition of type LogMode from ChgMgmtGraph repeated.

    Revision 1.15  1998/03/18 09:27:26  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.14  1998/03/17 14:14:31  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.13  1998/02/16 12:04:20  roland
    Introduced a two-phase commit to coordinate graph commits.

    Revision 1.12  1998/01/21 12:38:39  roland
    New exception to signal undefined attribute value for getTypedAttribute.

    Revision 1.11  1998/01/14 17:49:01  roland
    Bugfix: Dependent attributes should only be invalidated when defining
    attribute was valid before write.

    Revision 1.10  1997/12/08 17:26:02  roland
    Bugfix in TextToAttributeState: converting length information to
    integer now.

    Revision 1.9  1997/12/08 11:52:24  roland
    New feature attribute placement. An attribute placement defines a
    physical location for a logical attribute.

    Revision 1.8  1997/12/02 17:50:54  roland
    Bugfix: Relations for CardinalityChecks were disposed twice.

    Revision 1.7  1997/11/26 08:38:22  roland
    Disposal of temporary sets, especially from schema queries.

    Revision 1.6  1997/09/09 10:37:07  roland
    closeAndFlush now also for graphs.

    Revision 1.5  1997/07/21 10:48:09  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.4  1997/07/07 15:40:22  roland
    Added caches for relations in a Scheme and for node types in a typed
    graph.

    Revision 1.3  1997/06/27 07:09:35  roland
    Transactions added so that a client might open a graph outside without
    a transaction.

    Revision 1.2  1997/05/06 13:21:14  roland
    Bugfix: consider transaction depth when opening.

    Revision 1.1  1997/05/01 13:23:19  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.4  1997/02/26 17:04:24  roland
    Type EvaluationFunction replaced with type Evaluator. This should
    ease inheritance as well as redefinition in modules using TypedGraph.

    Revision 1.3  1997/02/13 12:16:11  roland
    Type information of nodes is now also stored in the node
    labels. Therefore, getNodeLabel and putNodeLabel removed.

    Revision 1.2  1997/02/04 11:16:05  roland
    It is now possible to disable logging in ChgMgmtGraph completely.

    Revision 1.1  1997/01/31 10:33:26  roland
    First version of new scheme layer for GRAS_3. Schemes are stored in
    separate graphs. Caches are used for accessing scheme.

*)
(***************************************************************************)

IMPORT Pathname, PageFile, Access, NodeSet, TaggedNameSet,
       AttributeDescriptionSet, CardStack, NodeLabelRelation, Node,
       NodeTypeRelation, CardSet;
IMPORT ErrorSupport;
IMPORT ChgMgmtGraph AS Super;
IMPORT PersistentGraph, NodeTypeRelationStack;
IMPORT Scheme, InternScheme, AttributeValue, TypedGraphPool,
       InternTypedGraphPool, Transaction;
IMPORT Word, Text;
IMPORT TypeCache;

REVEAL
  T =
    Public BRANDED OBJECT
      pool        : TypedGraphPool.T;
      scheme      : Scheme.T;          (* The graphs scheme *)
      schemeNumber: CARDINAL;          (* the schemes number in the pool *)
      actChecks: ErrorCheckSet;  (* The checks enabled for the current
                                    transaction *)
      checks: CardStack.T;       (* This stack stores the ErrorCheckSets of
                                    parent transactions *)
      inChecks, outChecks: NodeTypeRelation.T;  (* Pairs (n, l) of nodes
                                                   and labels of
                                                   incoming/outgoing edges,
                                                   that have to be checked
                                                   on cardinality at the
                                                   end of the
                                                   transaction *)
      checkRels: NodeTypeRelationStack.T;  (* For each parent transaction
                                              this stack contains the
                                              relations for incoming and
                                              outgoing edge cardinality
                                              checks. *)
      doCardChecks: BOOLEAN;     (* TRUE iff top-level error checks contain
                                    Cardinality checks.  Used for
                                    efficiency. *)
      evaluatingAttribute: BOOLEAN;  (* TRUE iff getAttribute triggered an
                                        attribute evaluation *)
      pendingEvaluations: NodeLabelRelation.T;  (* Pairs (n, a) of nodes
                                                   and attributes already
                                                   touched in this
                                                   evaluation cycle.
                                                   evaluatingAttributes and
                                                   pendingEvaluations are
                                                   used to detect cycles in
                                                   the dynamic attribute
                                                   dependencies. *)
      evaluationTable: EFTable;  (* stores the functions for attribute
                                    reevaluation *)
      typeCache: TypeCache.T;
    OVERRIDES
      open                  := Open;
      close                 := Close;
      closeAndFlush         := CloseAndFlush;
      setErrorChecks        := SetErrorChecks;
      getErrorChecks        := GetErrorChecks;
      getScheme             := GetScheme;
      createNodeNumber      := CreateNodeNumber;
      createNode            := CreateNode;
      deleteNode            := DeleteNode;
      deleteNodeNoInfo      := DeleteNodeNoInfo;
      existsNode            := ExistsNode;
      getNodeType           := GetNodeType;
      nodeIsInstanceOf      := NodeIsInstanceOf;
      getAllNodesOfType     := GetAllNodesOfType;
      putAttribute          := PutAttribute;
      putTypedAttribute     := PutTypedAttribute;
      getAttribute          := GetAttribute;
      getTypedAttribute     := GetTypedAttribute;
      truncateAttribute     := TruncateAttribute;
      bindEvaluator         := BindEvaluator;
      getNodesWithAttribute := GetNodesWithAttribute;
      createEdge            := CreateEdge;
      deleteEdge            := DeleteEdge;
      existsEdge            := ExistsEdge;
      validEdge             := ValidEdge;
      getTargets            := GetTargets;
      getSources            := GetSources;
      getTargetsTo          := GetTargetsTo;
      getSourcesFrom        := GetSourcesFrom;
      getAllTargets         := GetAllTargets;
      getAllSources         := GetAllSources;
      getAllOutTypes        := GetAllOutTypes;
      getAllInTypes         := GetAllInTypes;
      getAllOutEdges        := GetAllOutEdges;
      getAllInEdges         := GetAllInEdges;
    END;

CONST
  TypeIndex   = 0;
  InvalidFlag = "1";

PROCEDURE ErrAbort (pool: TypedGraphPool.T) =
  BEGIN
    TRY
      pool.abortTransaction();
    EXCEPT
      TypedGraphPool.NotInTransaction => (* ignore *)
    | TypedGraphPool.InternalError => (* ignore *)
    END;
  END ErrAbort;

PROCEDURE Open (graph      : T;
                pool       : TypedGraphPool.T;
                baseName   : Pathname.T;
                access     : AccessMode;
                new        : BOOLEAN;
                errorChecks: ErrorCheckSet;
                local      : BOOLEAN            := FALSE;
                log        : LogMode            := LogMode.Linear;
                scheme     : Pathname.T         := NIL             ): T
  RAISES {NoScheme, PageFile.NoAccess, Access.Locked, Access.Denied,
          InternalError, NotExistent, InUse} =

  <* FATAL CardStack.Full, CardStack.Undefined *>
  <* FATAL NodeTypeRelationStack.Full, NodeTypeRelationStack.Undefined *>
  <* FATAL TypedGraphPool.CardinalityError, TypedGraphPool.NotInTransaction *>
  PROCEDURE Mode (access: AccessMode): Super.AccessMode =
    BEGIN
      RETURN VAL(ORD(access), Super.AccessMode);
    END Mode;

  BEGIN
    TRY
      pool.beginTransaction();
      graph.pool := pool;
      IF new THEN
        IF scheme = NIL THEN RAISE NoScheme; END;
        IF NOT pool.existsScheme(scheme, local) THEN RAISE NoScheme END;
        IF pool.existsGraph(baseName, local) THEN
          pool.deleteGraph(baseName, local);
        END;
      ELSE
        IF NOT pool.existsGraph(baseName, local) THEN
          RAISE NotExistent;
        END;
        IF pool.hasScheme(baseName, local) THEN
          scheme := pool.getScheme(baseName, local);
        ELSE
          pool.commitTransaction();
          RAISE NoScheme;
        END;
      END;
      graph.scheme :=
        NEW(Scheme.T).open(
          pool, scheme, local, access := Scheme.AccessMode.ReadOnlyShared,
          new := FALSE);

      WITH cb = InternTypedGraphPool.CallbackSuite{
                  CBBeginTransaction, CBPrepareCommitTransaction,
                  CBCommitTransaction, CBAbortTransaction} DO
        graph :=
          pool.openCG(graph, baseName, Mode(access), errorChecks := FALSE,
                      local := local, log := log, scheme := scheme,
                      callbacks := cb, schemeNumber := graph.schemeNumber);
      END;

      graph.actChecks := errorChecks;
      graph.checks := NEW(CardStack.T).init();
      graph.inChecks := NodeTypeRelation.New();
      graph.outChecks := NodeTypeRelation.New();
      graph.checkRels := NEW(NodeTypeRelationStack.T).init();
      (* adjust to transaction depth *)
      FOR l := 1 TO pool.getTransactionLevel() DO
        graph.checks.push(ErrorCheckSetToCard(errorChecks));
        graph.checkRels.push(graph.inChecks);
        graph.checkRels.push(graph.outChecks);
        graph.inChecks := NodeTypeRelation.New();
        graph.outChecks := NodeTypeRelation.New();
      END;
      graph.doCardChecks := ErrorCheck.Cardinality IN errorChecks;
      graph.pendingEvaluations := NodeLabelRelation.New();
      graph.evaluatingAttribute := FALSE;
      EFTInit(graph.evaluationTable);

      graph.typeCache := NEW(TypeCache.T).init(graph.pool);
      pool.commitTransaction();
      RETURN graph;
    EXCEPT
    | TypedGraphPool.InUse => ErrAbort(pool); RAISE InUse;
    | Scheme.InUse =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.Mode", "Scheme.InUse"));
    | Scheme.Existent =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.Mode", "Scheme.Existent"));
    | Scheme.NotExistent =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Create(
                              "TypedGraph.Mode", "Scheme.NotExistent"));
    | TypedGraphPool.NotExistent => ErrAbort(pool); RAISE NotExistent;
    | TypedGraphPool.NoGraph => ErrAbort(pool); RAISE NotExistent;
    | TypedGraphPool.NoScheme => ErrAbort(pool); RAISE NoScheme;
    | Scheme.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.Open", "Scheme.InternalError", info));
    | Scheme.NoValidScheme => ErrAbort(pool); RAISE NoScheme;
    | Access.Locked =>
        ErrAbort(pool);
        ErrorCloseScheme(graph);
        RAISE Access.Locked;
    | Access.Denied (msg) =>
        ErrAbort(pool);
        ErrorCloseScheme(graph);
        RAISE Access.Denied(msg);
    | PageFile.NoAccess (msg) =>
        ErrAbort(pool);
        ErrorCloseScheme(graph);
        RAISE PageFile.NoAccess(msg);
    | TypedGraphPool.InternalError (info) =>
        ErrAbort(pool);
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "TypedGraph.Mode", "TypedGraphPool.InternalError", info));
    END;
  END Open;

PROCEDURE Close (graph: T; keepLog: BOOLEAN) RAISES {InternalError} =
  BEGIN
    TRY
      graph.pool.closeCG(graph, keepLog);
      graph.scheme.close();
    EXCEPT
      TypedGraphPool.InternalError (info) =>
        ErrorCloseScheme(graph);
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.Close",
                              "TypedGraphPool.InternalError", info));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.Close", "Scheme.InternalError", info));
    END;
  END Close;

PROCEDURE CloseAndFlush (graph: T; keepLog: BOOLEAN)
  RAISES {InternalError} =
  VAR depth: CARDINAL;
  <* FATAL TypedGraphPool.NotInTransaction *>
  BEGIN
    depth := 0;
    TRY
      WHILE graph.pool.getTransactionLevel() >= Transaction.TopLevel DO
        graph.pool.commitTransaction();
        INC(depth);
      END;
      Close(graph, keepLog);
      FOR d := 1 TO depth DO graph.pool.beginTransaction(); END;
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

PROCEDURE CBBeginTransaction (g: Super.T) RAISES {} =
  <* FATAL CardStack.Full, CardStack.Undefined, NodeTypeRelationStack.Undefined,
  NodeTypeRelationStack.Full *>
  BEGIN
    WITH graph = NARROW(g, T) DO
      graph.checks.push(ErrorCheckSetToCard(graph.actChecks));
      graph.checkRels.push(graph.inChecks);
      graph.checkRels.push(graph.outChecks);
      graph.inChecks := NodeTypeRelation.New();
      graph.outChecks := NodeTypeRelation.New();
    END;
  END CBBeginTransaction;

PROCEDURE CBPrepareCommitTransaction (g: Super.T)
  RAISES {TypedGraphPool.CardinalityError, Super.InternalError} =
  VAR checks: ErrorCheckSet;
  <* FATAL CardStack.Undefined *>
  BEGIN
    TRY
      WITH graph = NARROW(g, T) DO
        IF graph.pool.getTransactionLevel() > 0 THEN
          checks := CardToErrorCheckSet(graph.checks.top());
          IF ErrorCheck.Cardinality IN checks
               AND NOT (graph.inChecks.isEmpty()
                          AND graph.outChecks.isEmpty()) THEN
            CardinalityChecks(graph, graph.inChecks, incoming := TRUE);
            CardinalityChecks(graph, graph.outChecks, incoming := FALSE);
          END;
        END;
      END;
    EXCEPT
    | Access.Locked =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "TypedGraph.CommitTransaction", "Access.Locked"));
    | CardStack.Empty =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "TypedGraph.CommitTransaction", "CardStack.Empty"));
    END;
  END CBPrepareCommitTransaction;

PROCEDURE CBCommitTransaction (g: Super.T) RAISES {} =
  <* FATAL CardStack.Empty, NodeTypeRelationStack.Empty *>
  <* FATAL CardStack.Undefined, NodeTypeRelationStack.Undefined *>
  BEGIN
    WITH graph = NARROW(g, T) DO
      IF graph.pool.getTransactionLevel() > 0 THEN
        graph.actChecks := CardToErrorCheckSet(graph.checks.pop());
        graph.outChecks.dispose();
        graph.inChecks.dispose();
        graph.outChecks := graph.checkRels.pop();
        graph.inChecks := graph.checkRels.pop();
      END;
    END;
  END CBCommitTransaction;

PROCEDURE CBAbortTransaction (g: Super.T) RAISES {Super.InternalError} =
  <* FATAL CardStack.Undefined, NodeTypeRelationStack.Undefined *>
  BEGIN
    TRY
      WITH graph = NARROW(g, T) DO
        IF graph.pool.getTransactionLevel() > 0 THEN
          graph.actChecks := CardToErrorCheckSet(graph.checks.pop());
          graph.outChecks := graph.checkRels.pop();
          graph.inChecks := graph.checkRels.pop();
        END;
      END;
    EXCEPT
    | CardStack.Empty =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "TypedGraph.CBAbortTransaction", "CardStack.Empty"));
    | NodeTypeRelationStack.Empty =>
        RAISE Super.InternalError(
                ErrorSupport.Create("TypedGraph.CBAbortTransaction",
                                    "NodeTypeRelationStack.Empty"));
    END;
  END CBAbortTransaction;

PROCEDURE SetErrorChecks (graph: T; checks: ErrorCheckSet)
  RAISES {InternalError} =
  <* FATAL CardStack.Undefined *>
  BEGIN
    TRY
      IF graph.pool.getTransactionLevel() > 0 THEN
        graph.actChecks :=
          CardToErrorCheckSet(graph.checks.top()) * checks;
      ELSE
        graph.actChecks := graph.actChecks * checks;
      END;
    EXCEPT
      CardStack.Empty =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "TypedGraph.SetErrorChecks", "CardStack.Empty"));
    END;
  END SetErrorChecks;

PROCEDURE GetErrorChecks (graph: T): ErrorCheckSet =
  BEGIN
    RETURN graph.actChecks;
  END GetErrorChecks;

PROCEDURE GetScheme (graph: T): Scheme.T =
  BEGIN
    RETURN graph.scheme;
  END GetScheme;

PROCEDURE CreateNodeNumber (graph: T; neighbour: Node.T): Node.T
  RAISES {NotOwner, Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN Super.T.createNodeNumber(graph, neighbour);
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.CreateNodeNumber",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    END;
  END CreateNodeNumber;

PROCEDURE CreateNode (graph: T; type: Scheme.ID): Node.T
  RAISES {Unknown, Access.Locked, InternalError, LogError} =
  VAR
    node  : Node.T;
    etypes: Scheme.IDSet;
    tmp   : NodeTypeRelation.T;
    guards: Scheme.IDSet;
    guard : Scheme.ID;
    found : BOOLEAN;
  BEGIN
    IF ErrorCheck.Type IN graph.actChecks THEN
      AssertNodeTypeKnown(graph, type);
    END;
    TRY
      (* create node *)
      node := Super.T.createNode(graph, type.entity);
      (* write its type *)
      InternPutNodeType(graph, node, type);
      (* mark all its guarded attributes invalid *)
      guards := graph.scheme.getGuardsOfNodeClassOrType(type);
      guards.loop();
      guard := guards.get(found);
      WHILE found DO
        MarkInvalid(graph, node, guard);
        guard := guards.get(found);
      END;
      guards.dispose();
      IF graph.doCardChecks THEN
        (* collect all incident edge types for later cardinality check *)
        etypes := graph.scheme.getIncomingEdgeTypes(type);
        tmp := NodeTypeRelation.New();
        tmp := tmp.fromElem2Set(etypes, node);
        graph.inChecks.union(tmp);
        tmp.dispose();
        etypes.dispose();

        etypes := graph.scheme.getOutgoingEdgeTypes(type);
        tmp := NodeTypeRelation.New();
        tmp := tmp.fromElem2Set(etypes, node);
        graph.outChecks.union(tmp);
        tmp.dispose();
        etypes.dispose();
      END;
      RETURN node;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.CreateNode", "Super.InternalError", info));
    | Super.LogError (info) =>
        RAISE LogError(ErrorSupport.Propagate(
                         "TypedGraph.CreateNode", "Super.LogError", info));
    | Scheme.NotDeclared => RAISE Unknown;
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.CreateNode", "Scheme.InternalError", info));
    END;
  END CreateNode;

PROCEDURE DeleteNode (    graph     : T;
                          node      : Node.T;
                      VAR attributes: AttributeDescriptionSet.T;
                      VAR indexes   : TaggedNameSet.T;
                      VAR outEdges  : NodeTypeRelation.T;
                      VAR inEdges   : NodeTypeRelation.T         )
  RAISES {NotOwner, Access.Locked, InternalError, LogError} =
  VAR
    needInvalidation   : NodeLabelRelation.T;
    target, source     : Node.T;
    cInEdges, cOutEdges: NodeLabelRelation.T;
    etype              : Scheme.ID;
    found              : BOOLEAN;
  BEGIN
    TRY
      IF NOT Super.T.existsNode(graph, node) THEN RETURN END;
      (* delete node *)
      Super.T.deleteNode(
        graph, node, attributes, indexes, cOutEdges, cInEdges);
      inEdges := NodeLabelToNodeTypeRelation(graph, cInEdges);
      outEdges := NodeLabelToNodeTypeRelation(graph, cOutEdges);
      cInEdges.dispose();
      cOutEdges.dispose();

      (* Collect all depending attributes.  Dependencies on a deleted node
         can only invalidate attributes of other nodes.  Therefore it is
         sufficient if we loop through the edges, not through the
         attrbutes. *)
      needInvalidation := NodeLabelRelation.New();
      outEdges.loop();
      outEdges.get(target, etype, found);
      WHILE found DO
        IF target # node AND graph.scheme.existsEdgeTypeByNumber(etype) THEN
          WITH tmp = CollectDependingOnEdge(
                       graph, target, etype, incoming := TRUE) DO
            needInvalidation.union(tmp);
            tmp.dispose();
          END;
        END;
        outEdges.get(target, etype, found);
      END;
      inEdges.loop();
      inEdges.get(source, etype, found);
      WHILE found DO
        IF source # node AND graph.scheme.existsEdgeTypeByNumber(etype) THEN
          WITH tmp = CollectDependingOnEdge(
                       graph, source, etype, incoming := FALSE) DO
            needInvalidation.union(tmp);
            tmp.dispose();
          END;
        END;
        inEdges.get(source, etype, found);
      END;
      PropagateNullity(graph, needInvalidation);
      needInvalidation.dispose();

      IF ErrorCheck.Cardinality IN graph.actChecks THEN
        (* collect its neighbours for cardinality checks *)
        graph.inChecks.union(outEdges);
        graph.outChecks.union(inEdges);
      END;
      (* remove cache entry *)
      graph.typeCache.delete(node);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.DeleteNode", "Super.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | Super.LogError (info) =>
        RAISE LogError(ErrorSupport.Propagate(
                         "TypedGraph.DeleteNode", "Super.LogError", info));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.DeleteNode",
                              "PersistentGraph.InternalError", info));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.DeleteNode", "Scheme.InternalError", info));
    END;
  END DeleteNode;

PROCEDURE DeleteNodeNoInfo (graph: T; node: Node.T)
  RAISES {NotOwner, Access.Locked, InternalError, LogError} =
  VAR
    attributes: AttributeDescriptionSet.T;
    indexes   : TaggedNameSet.T;
    outEdges  : NodeTypeRelation.T;
    inEdges   : NodeTypeRelation.T;
  BEGIN
    (* delete node *)
    DeleteNode(graph, node, attributes, indexes, outEdges, inEdges);
  END DeleteNodeNoInfo;

PROCEDURE ExistsNode (graph: T; node: Node.T): BOOLEAN
  RAISES {NotOwner, Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN Super.T.existsNode(graph, node);
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.ExistsNode",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    END;
  END ExistsNode;

PROCEDURE GetNodeType (graph: T; node: Node.T): Scheme.ID
  RAISES {NotOwner, Unknown, NodeNotFound, Access.Locked, InternalError} =
  VAR
    found: BOOLEAN;
    type : Scheme.ID;
  BEGIN
    IF ErrorCheck.Node IN graph.actChecks THEN
      AssertNodeExists(graph, node);
    END;
    type := InternGetNodeType(graph, node, found);
    IF NOT found THEN RAISE Unknown; END;
    RETURN type;
  END GetNodeType;

PROCEDURE NodeIsInstanceOf (graph: T; node, ct: Scheme.ID): BOOLEAN
  RAISES {NotOwner, Unknown, NodeNotFound, Access.Locked, InternalError} =
  VAR
    type : Scheme.ID;
    found: BOOLEAN;
  BEGIN
    TRY
      IF ErrorCheck.Node IN graph.actChecks THEN
        AssertNodeExists(graph, node);
      END;
      type := InternGetNodeType(graph, node, found);
      IF NOT found THEN RAISE Unknown END;
      RETURN graph.scheme.isSubClassOrType(type, ct);
    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.NodeIsInstanceOf",
                                       "Scheme.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.NodeIsInstanceOf",
                                            "Scheme.NotDeclared"));
    END;
  END NodeIsInstanceOf;


PROCEDURE GetAllNodesOfType (graph: T; type: Scheme.ID): NodeSet.T
  RAISES {Access.Locked, InternalError} =
  VAR text: TEXT;
  BEGIN
    TRY
      text := AttributeValue.CardToText(type.graph)
                & AttributeValue.CardToText(type.entity);
      RETURN Super.T.getNodesWithIndex(graph, TypeIndex, text);
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.NodeIsInstanceOf",
                              "PersistentGraph.InternalError", info));
    END;
  END GetAllNodesOfType;

PROCEDURE PutAttribute (graph      : T;
                        node       : Node.T;
                        attributeNo: Scheme.ID;
                        start      : CARDINAL;
                        attribute  : TEXT       )
  RAISES {NodeNotFound, Unknown, WrongType, Access.Locked, InternalError,
          LogError, NotOwner} =
  BEGIN
    InternPutAttribute(graph, node, attributeNo, start := start,
                       val := attribute, typeCheck := FALSE, type := 0);
  END PutAttribute;

PROCEDURE PutTypedAttribute (graph: T;
                             node : Node.T;
                             attr : Scheme.ID;
                             val  : AttributeValue.T)
  RAISES {NodeNotFound, Unknown, WrongType, (*CardinalityError,*)
          Access.Locked, InternalError, LogError, NotOwner} =
  VAR len: CARDINAL;
  BEGIN
    InternPutAttribute(
      graph, node, attr, start := 0, val := val.toText(len),
      typeCheck := TRUE, type := val.type());
  END PutTypedAttribute;


PROCEDURE GetAttribute (graph      : T;
                        node       : Node.T;
                        attributeNo: Scheme.ID;
                        start      : CARDINAL;
                        length     : CARDINAL   ): TEXT
  RAISES {Access.Locked, Unknown, WrongType, CyclicEvaluation,
          NodeNotFound, InternalError, LogError, NotOwner} =
  BEGIN
    RETURN InternGetAttribute(graph, node, attributeNo, start, length);
  END GetAttribute;

PROCEDURE GetTypedAttribute (graph: T;
                             node : Node.T;
                             attr : Scheme.ID;
                             val  : AttributeValue.T)
  RAISES {NodeNotFound, Unknown, WrongType, (*CardinalityError,*) NotOwner,
          CyclicEvaluation, Access.Locked, InternalError, LogError,
          ValueUndefined} =
  VAR attrib: TEXT;
  BEGIN
    attrib := InternGetAttribute(graph, node, attr, 0, LAST(CARDINAL));
    TRY
      val.fromText(attrib);
    EXCEPT
      AttributeValue.Invalid =>
        IF Text.Length(attrib) > 0 THEN
          RAISE WrongType;
        ELSE
          RAISE ValueUndefined;
        END;
    END;
  END GetTypedAttribute;

PROCEDURE TruncateAttribute (graph: T;
                             node : Node.T;
                             attr : Scheme.ID;
                             size : CARDINAL   )
  RAISES {Access.Locked, Unknown, NodeNotFound, WrongType, InternalError,
          LogError, NotOwner} =
  VAR
    ntype           : Scheme.ID;
    kind            : Scheme.AttributeKind;
    indexprops      : Scheme.IndexProperties;
    valuetype       : CARDINAL;
    attCard         : Scheme.Cardinality;
    constLength     : BOOLEAN;
    length          : CARDINAL;
    old, new        : TEXT;
    found           : BOOLEAN;
    needInvalidation: NodeLabelRelation.T;
    physNo          : CARDINAL;
    physStart       : CARDINAL;
    physMaxLen      : INTEGER;

  BEGIN
    TRY
      IF NOT CheckAttribute(graph, node, attr, ntype) THEN
        Super.T.truncateAttribute(graph, node, attr.entity, size);
        RETURN;
      END;

      (* length check ?? *)
      graph.scheme.showAttributeProps(
        attr, kind, indexprops, valuetype, attCard, constLength, length);
      graph.scheme.showAttributePlacement(
        ntype, attr, physNo, physStart, physMaxLen);
      CASE kind OF
        Scheme.AttributeKind.Intrinsic, Scheme.AttributeKind.Derived =>
          IF indexprops = Scheme.IndexProperties.Normal THEN
            IF physMaxLen < 0 (* Unlimited *) THEN
              (* the last logical attribute on this physical attribute *)
              Super.T.truncateAttribute(
                graph, node, physNo, physStart + size);
            ELSIF size < physMaxLen THEN
              (* Fill with \000 *)
              Super.T.putAttribute(graph, node, physNo, physStart + size,
                                   Zeroes(physMaxLen - size));
            END;
          ELSE                   (* key or index *)
            old := Super.T.getIndex(graph, node, physNo, found);
            IF found AND Text.Length(old) > size THEN
              Super.T.deleteIndex(graph, node, physNo, old);
              new := Text.Sub(old, 0, size);
              Super.T.putIndex(graph, node, physNo, new);
            END;
          END;
      | Scheme.AttributeKind.Meta, Scheme.AttributeKind.Dummy =>
          RAISE WrongType;
      END;
      (* if the attribute is now invalid *)
      IF size = 0 THEN
        (* invalidate depending attributes *)
        needInvalidation := CollectDependingOnAttribute(graph, node, attr);
        PropagateNullity(graph, needInvalidation);
        needInvalidation.dispose();
        (* if attr is guarded then it must now be set to invalid *)
        IF graph.scheme.isGuardedAttribute(attr) THEN
          MarkInvalid(graph, node, graph.scheme.getGuard(attr));
        END;
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.TruncateAttribute",
                                       "Super.InternalError", info));
    | Super.LogError (info) =>
        RAISE
          LogError(ErrorSupport.Propagate("TypedGraph.TruncateAttribute",
                                          "Super.LogError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.TruncateAttribute",
                                    "PersistentGraph.NodeNotFound"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.TruncateAttribute",
                                       "Scheme.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.TruncateAttribute",
                                            "Scheme.NotDeclared"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.TruncateAttribute",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.IndexUnused =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.TruncateAttribute",
                                    "PersistentGraph.IndexUnused"));
    | PersistentGraph.IndexUsed =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.TruncateAttribute",
                                            "PersistentGraph.IndexUsed"));
    END;
  END TruncateAttribute;


PROCEDURE BindEvaluator (graph: T; name: TEXT; eval: Evaluator)
  RAISES {AlreadyBound} =
  VAR old: Evaluator;
  BEGIN
    IF EFTRetrieve(graph.evaluationTable, name, old) THEN
      IF eval # old THEN RAISE AlreadyBound; END;
    ELSE
      EFTInsert(graph.evaluationTable, name, eval);
    END;
  END BindEvaluator;

PROCEDURE GetNodesWithAttribute (graph: T; attr: Scheme.ID; val: TEXT):
  NodeSet.T RAISES {Unknown, NoIndex, CyclicEvaluation, Access.Locked,
                    InternalError, LogError} =
  VAR
    declared   : BOOLEAN;
    kind       : Scheme.AttributeKind;
    indexprops : Scheme.IndexProperties;
    valuetype  : CARDINAL;
    attCard    : Scheme.Cardinality;
    constLength: BOOLEAN;
    length     : CARDINAL;
  BEGIN
    TRY
      declared := graph.scheme.existsAttributeByNumber(attr);
      IF ErrorCheck.Attribute IN graph.actChecks AND NOT declared THEN
        RAISE Unknown;
      ELSIF NOT declared THEN
        RETURN Super.T.getNodesWithIndex(graph, attr.entity, val);
      END;

      graph.scheme.showAttributeProps(
        attr, kind, indexprops, valuetype, attCard, constLength, length);
      IF indexprops = Scheme.IndexProperties.Normal THEN
        RAISE NoIndex;
      END;

      IF graph.scheme.isGuardedAttribute(attr) THEN
        EvaluateGuardedAttributes(graph, attr);
      END;
      RETURN Super.T.getNodesWithIndex(graph, attr.entity, val);

    EXCEPT
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetNodesWithAttribute",
                              "PersistentGraph.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE InternalError(ErrorSupport.Create(
                              "TypedGraph.GetNodesWithAttribute",
                              "Scheme.NotDeclared"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.GetNodesWithAttribute",
                                       "Scheme.InternalError", info));
    END;
  END GetNodesWithAttribute;

PROCEDURE CreateEdge (graph : T;
                      source: Node.T;
                      target: Node.T;
                      type  : Scheme.ID)
  RAISES {NotOwner, Unknown, NodeNotFound,
          WrongType,             (* CardinalityError, *)
          Access.Locked, InternalError, LogError} =
  VAR
    sourceCT, targetCT    : Scheme.ID;
    sourceCard, targetCard: Scheme.Cardinality;
    stype, ttype          : Scheme.ID;
    sfound, tfound, exists: BOOLEAN;
    needInvalidation      : NodeLabelRelation.T;
  BEGIN
    IF ErrorCheck.Node IN graph.actChecks THEN
      AssertNodePair(graph, source, target);
    END;
    TRY
      IF NOT graph.scheme.existsEdgeTypeByNumber(type) THEN
        IF ErrorCheck.Edge IN graph.actChecks THEN RAISE Unknown; END;
        exists := FALSE;
      ELSE
        exists := TRUE;
      END;

      IF exists THEN
        graph.scheme.showEdgeTypeProps(
          type, sourceCT, targetCT, sourceCard, targetCard);
        IF ErrorCheck.Edge IN graph.actChecks THEN
          stype := InternGetNodeType(graph, source, sfound);
          ttype := InternGetNodeType(graph, target, tfound);
          IF ErrorCheck.Type IN graph.actChecks AND NOT (sfound AND tfound) THEN
            RAISE Unknown;
          END;
          (* Have source and target correct types? *)
          IF NOT (((stype = sourceCT)
                     OR graph.scheme.isSubClassOrType(stype, sourceCT))
                    AND ((ttype = targetCT)
                           OR graph.scheme.isSubClassOrType(
                                ttype, targetCT))) THEN
            RAISE WrongType;
          END;
        END;
      END;

      Super.T.createEdge(graph, source, target, type.entity);

      (* invalidate dependent attributes *)
      IF exists THEN
        needInvalidation :=
          CollectDependingOnEdge(graph, source, type, incoming := FALSE);
        WITH tmp = CollectDependingOnEdge(
                     graph, target, type, incoming := TRUE) DO
          needInvalidation.union(tmp);
          tmp.dispose();
        END;
        PropagateNullity(graph, needInvalidation);
        needInvalidation.dispose();
        (* prepare cardinality checks *)
        IF graph.doCardChecks THEN
          IF sourceCard # Scheme.ArbitraryCard THEN
            graph.outChecks.insert(source, type);
          END;
          IF targetCard # Scheme.ArbitraryCard THEN
            graph.inChecks.insert(target, type);
          END;
        END;

      END;

    EXCEPT
      Scheme.NotDeclared =>
        RAISE InternalError(ErrorSupport.Create("TypedGraph.CreateEdge",
                                                "Scheme.NotDeclared"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.CreateEdge", "Scheme.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.CreateEdge", "Super.InternalError", info));
    | Super.LogError (info) =>
        RAISE LogError(ErrorSupport.Propagate(
                         "TypedGraph.CreateEdge", "Super.LogError", info));
    END;
  END CreateEdge;

PROCEDURE DeleteEdge (graph : T;
                      source: Node.T;
                      target: Node.T;
                      type  : Scheme.ID)
  RAISES {NotOwner, NodeNotFound, Unknown,
          WrongType,             (* CardinalityError, *)
          Access.Locked, InternalError, LogError} =
  VAR
    sourceCT, targetCT    : Scheme.ID;
    sourceCard, targetCard: Scheme.Cardinality;
    stype, ttype          : Scheme.ID;
    sfound, tfound, exists: BOOLEAN;
    needInvalidation      : NodeLabelRelation.T;
  BEGIN
    IF ErrorCheck.Node IN graph.actChecks THEN
      AssertNodePair(graph, source, target);
    END;
    TRY
      IF NOT graph.scheme.existsEdgeTypeByNumber(type) THEN
        IF ErrorCheck.Edge IN graph.actChecks THEN RAISE Unknown; END;
        exists := FALSE;
      ELSE
        exists := TRUE;
      END;

      IF exists THEN
        graph.scheme.showEdgeTypeProps(
          type, sourceCT, targetCT, sourceCard, targetCard);
        IF ErrorCheck.Edge IN graph.actChecks THEN
          stype := InternGetNodeType(graph, source, sfound);
          ttype := InternGetNodeType(graph, target, tfound);
          IF ErrorCheck.Type IN graph.actChecks AND NOT (sfound AND tfound) THEN
            RAISE Unknown;
          END;
          (* Have source and target correct types? *)
          IF NOT (((stype = sourceCT)
                     OR graph.scheme.isSubClassOrType(stype, sourceCT))
                    AND ((ttype = targetCT)
                           OR graph.scheme.isSubClassOrType(
                                ttype, targetCT))) THEN
            RAISE WrongType;
          END;
        END;
      END;

      Super.T.deleteEdge(graph, source, target, type.entity);

      IF exists THEN
        (* invalidate dependent attributes *)
        needInvalidation :=
          CollectDependingOnEdge(graph, source, type, incoming := FALSE);
        WITH tmp = CollectDependingOnEdge(
                     graph, target, type, incoming := TRUE) DO
          needInvalidation.union(tmp);
          tmp.dispose();
        END;
        PropagateNullity(graph, needInvalidation);
        needInvalidation.dispose();
        (* prepare cardinality checks *)
        IF graph.doCardChecks THEN
          IF sourceCard # Scheme.ArbitraryCard THEN
            graph.outChecks.insert(source, type);
          END;
          IF targetCard # Scheme.ArbitraryCard THEN
            graph.inChecks.insert(target, type);
          END;
        END;
      END;

    EXCEPT
      Scheme.NotDeclared =>
        RAISE InternalError(ErrorSupport.Create("TypedGraph.DeleteEdge",
                                                "Scheme.NotDeclared"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.DeleteEdge", "Scheme.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.DeleteEdge", "Super.InternalError", info));
    | Super.LogError (info) =>
        RAISE LogError(ErrorSupport.Propagate(
                         "TypedGraph.DeleteEdge", "Super.LogError", info));
    END;
  END DeleteEdge;

PROCEDURE ExistsEdge (graph : T;
                      source: Node.T;
                      target: Node.T;
                      type  : Scheme.ID): BOOLEAN
  RAISES {NotOwner, NodeNotFound, Unknown, Access.Locked, InternalError} =
  BEGIN
    TRY
      IF ErrorCheck.Edge IN graph.actChecks
           AND NOT graph.scheme.existsEdgeTypeByNumber(type) THEN
        RAISE Unknown;
      END;

      RETURN Super.T.existsEdge(graph, source, target, type.entity);

    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.ExistsEdge", "Scheme.InternalError", info));
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.ExistsEdge",
                              "PersistentGraph.InternalError", info));
    END;
  END ExistsEdge;

PROCEDURE ValidEdge (graph    : T;
                     source   : Node.T;
                     target   : Node.T;
                     type     : Scheme.ID;
                     checkCard: BOOLEAN     := FALSE): BOOLEAN
  RAISES {NotOwner, NodeNotFound, Access.Locked, InternalError} =
  VAR
    sourceCT, targetCT    : Scheme.ID;
    sourceCard, targetCard: Scheme.Cardinality;
    stype, ttype          : Scheme.ID;
    card                  : CARDINAL;
    sfound, tfound        : BOOLEAN;
    inc                   : NodeSet.T;
  BEGIN
    IF ErrorCheck.Node IN graph.actChecks THEN
      AssertNodeExists(graph, source);
      AssertNodeExists(graph, target);
    END;
    TRY
      IF NOT graph.scheme.existsEdgeTypeByNumber(type) THEN
        RETURN FALSE
      END;

      graph.scheme.showEdgeTypeProps(
        type, sourceCT, targetCT, sourceCard, targetCard);
      stype := InternGetNodeType(graph, source, sfound);
      ttype := InternGetNodeType(graph, target, tfound);
      IF NOT (sfound AND tfound) THEN RETURN FALSE; END;
      (* Have source and target correct types? *)
      IF NOT (((stype = sourceCT)
                 OR graph.scheme.isSubClassOrType(stype, sourceCT))
                AND ((ttype = targetCT)
                       OR graph.scheme.isSubClassOrType(ttype, targetCT))) THEN
        RETURN FALSE;
      END;

      IF Super.T.existsEdge(graph, source, target, type.entity) THEN
        RETURN FALSE;
      END;

      IF checkCard THEN
        inc := Super.T.getTargets(graph, source, type.entity);
        card := inc.card() + 1;
        IF card < sourceCard.min OR card > sourceCard.max THEN
          RETURN FALSE;
        END;
        inc.dispose();
        inc := Super.T.getSources(graph, target, type.entity);
        card := inc.card() + 1;
        IF card < targetCard.min OR card > targetCard.max THEN
          RETURN FALSE;
        END;
        inc.dispose();
      END;
      RETURN TRUE;
    EXCEPT
      Scheme.NotDeclared =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "TypedGraph.ValidEdge", "Scheme.NotDeclared"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.ValidEdge", "Scheme.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.ValidEdge",
                              "PersistentGraph.InternalError", info));
    END;
  END ValidEdge;

PROCEDURE GetTargets (graph: T; source: Node.T; type: Scheme.ID): NodeSet.T
  RAISES {NotOwner, NodeNotFound, Unknown, Access.Locked, InternalError} =
  BEGIN
    TRY
      IF ErrorCheck.Edge IN graph.actChecks
           AND NOT graph.scheme.existsEdgeTypeByNumber(type) THEN
        RAISE Unknown;
      END;

      RETURN Super.T.getTargets(graph, source, type.entity);

    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.GetTargets", "Scheme.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetTargets",
                              "PersistentGraph.InternalError", info));
    END;
  END GetTargets;

PROCEDURE GetSources (graph: T; target: Node.T; type: Scheme.ID): NodeSet.T
  RAISES {NotOwner, NodeNotFound, Unknown, Access.Locked, InternalError} =
  BEGIN
    TRY
      IF ErrorCheck.Edge IN graph.actChecks
           AND NOT graph.scheme.existsEdgeTypeByNumber(type) THEN
        RAISE Unknown;
      END;

      RETURN Super.T.getSources(graph, target, type.entity);

    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.GetSources", "Scheme.InternalError", info));
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetSources",
                              "PersistentGraph.InternalError", info));
    END;
  END GetSources;

PROCEDURE GetTargetsTo (graph    : T;
                        source   : Node.T;
                        type     : Scheme.ID;
                        neighbour: CARDINAL    := 0): NodeSet.T
  RAISES {NotOwner, NodeNotFound, Unknown, Access.Locked, InternalError} =
  BEGIN
    TRY
      IF ErrorCheck.Edge IN graph.actChecks
           AND NOT graph.scheme.existsEdgeTypeByNumber(type) THEN
        RAISE Unknown;
      END;

      RETURN Super.T.getTargetsTo(graph, source, type.entity, neighbour);

    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "TypedGraph.GetTargetsTo", "Scheme.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetTargetsTo",
                              "PersistentGraph.InternalError", info));
    END;
  END GetTargetsTo;

PROCEDURE GetSourcesFrom (graph    : T;
                          target   : Node.T;
                          type     : Scheme.ID;
                          neighbour: CARDINAL    := 0): NodeSet.T
  RAISES {NotOwner, NodeNotFound, Unknown, Access.Locked, InternalError} =
  BEGIN
    TRY
      IF ErrorCheck.Edge IN graph.actChecks
           AND NOT graph.scheme.existsEdgeTypeByNumber(type) THEN
        RAISE Unknown;
      END;

      RETURN Super.T.getSourcesFrom(graph, target, type.entity, neighbour);

    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.GetSourcesFrom",
                                       "Scheme.InternalError", info));
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetSourcesFrom",
                              "PersistentGraph.InternalError", info));
    END;
  END GetSourcesFrom;

PROCEDURE GetAllTargets (graph: T; source: Node.T): NodeSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    TRY
      RETURN Super.T.getAllTargets(graph, source);
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetAllTargets",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetAllTargets;

PROCEDURE GetAllSources (graph: T; target: Node.T): NodeSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    TRY
      RETURN Super.T.getAllSources(graph, target);
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetAllSources",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetAllSources;

PROCEDURE GetAllOutTypes (graph: T; source: Node.T): Scheme.IDSet
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    TRY
      WITH set = Super.T.getAllOutLabels(graph, source),
           res = CardSetToIDSet(graph, set)              DO
        set.dispose();
        RETURN res;
      END;
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetAllOutTypes",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetAllOutTypes;

PROCEDURE GetAllInTypes (graph: T; target: Node.T): Scheme.IDSet
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    TRY
      WITH set = Super.T.getAllInLabels(graph, target),
           res = CardSetToIDSet(graph, set)             DO
        set.dispose();
        RETURN res;
      END;
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetAllOutTypes",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    END;

  END GetAllInTypes;

PROCEDURE GetAllOutEdges (graph: T; source: Node.T): NodeTypeRelation.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    TRY
      WITH set = Super.T.getAllOutEdges(graph, source),
           res = NodeLabelToNodeTypeRelation(graph, set) DO
        set.dispose();
        RETURN res;
      END;
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetAllOutTypes",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetAllOutEdges;

PROCEDURE GetAllInEdges (graph: T; target: Node.T): NodeTypeRelation.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    TRY
      WITH set = Super.T.getAllInEdges(graph, target),
           res = NodeLabelToNodeTypeRelation(graph, set) DO
        set.dispose();
        RETURN res;
      END;
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.GetAllOutTypes",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    END;

  END GetAllInEdges;

(* auxiliary procedures *)

PROCEDURE ErrorCloseScheme (graph: T) =
  BEGIN
    TRY
      IF graph.scheme # NIL THEN graph.scheme.close(); END;
    EXCEPT
      Scheme.InternalError =>    (* ignore *)
    END;
  END ErrorCloseScheme;

PROCEDURE Zeroes (len: CARDINAL): TEXT =
  CONST ZZZZ = ARRAY [0 .. 99] OF CHAR{'\000', ..};
  VAR res: TEXT;
  BEGIN
    res := Text.FromChars(SUBARRAY(ZZZZ, 0, len MOD NUMBER(ZZZZ)));
    FOR i := 1 TO len DIV NUMBER(ZZZZ) DO
      res := res & Text.FromChars(ZZZZ);
    END;
    RETURN res;
  END Zeroes;

PROCEDURE AssertNodePair (graph: T; node1, node2: Node.T)
  RAISES {NotOwner, NodeNotFound, InternalError, Access.Locked} =
  BEGIN
    TRY
      WITH gnumber = graph.number() DO
        IF node1.graph # gnumber AND node2.graph # gnumber THEN
          RAISE NotOwner;
        END;
        IF node1.graph = gnumber THEN
          IF NOT Super.T.existsNode(graph, node1) THEN
            RAISE NodeNotFound;
          END;
        END;
        IF node2.graph = gnumber THEN
          IF NOT Super.T.existsNode(graph, node2) THEN
            RAISE NodeNotFound;
          END;
        END;
      END;
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.AssertNodeExists",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    END;
  END AssertNodePair;

PROCEDURE AssertNodeExists (graph: T; node: Node.T)
  RAISES {NotOwner, NodeNotFound, InternalError, Access.Locked} =
  BEGIN
    TRY
      IF NOT Super.T.existsNode(graph, node) THEN RAISE NodeNotFound; END;
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.AssertNodeExists",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner => RAISE NotOwner;
    END;
  END AssertNodeExists;

PROCEDURE AssertNodeTypeKnown (graph: T; type: Scheme.ID)
  RAISES {Unknown, InternalError} =
  BEGIN
    TRY
      IF NOT graph.scheme.existsNodeTypeByNumber(type) THEN
        RAISE Unknown;
      END;
    EXCEPT
      Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.AssertNodeTypeKnown",
                                       "Scheme.InternalError", info));
    END;
  END AssertNodeTypeKnown;

PROCEDURE ErrorCheckSetToCard (set: ErrorCheckSet): CARDINAL =
  VAR
    res: CARDINAL := 0;
    exp: CARDINAL := 1;
  BEGIN
    FOR i := FIRST(ErrorCheck) TO LAST(ErrorCheck) DO
      IF i IN set THEN res := Word.Or(res, exp) END;
      exp := exp * 2;
    END;
    RETURN res;
  END ErrorCheckSetToCard;

PROCEDURE CardToErrorCheckSet (x: CARDINAL): ErrorCheckSet =
  VAR
    res           := ErrorCheckSet{};
    exp: CARDINAL := 1;
  BEGIN
    FOR i := FIRST(ErrorCheck) TO LAST(ErrorCheck) DO
      IF Word.And(x, exp) > 0 THEN res := res + ErrorCheckSet{i} END;
      exp := exp * 2;
    END;
    RETURN res;
  END CardToErrorCheckSet;

PROCEDURE CardinalityChecks (graph   : T;
                             checks  : NodeTypeRelation.T;
                             incoming: BOOLEAN             )
  RAISES {TypedGraphPool.CardinalityError, Super.InternalError,
          Access.Locked} =
  VAR
    node                  : Node.T;
    etype                 : Scheme.ID;
    found, tfound         : BOOLEAN;
    type                  : Scheme.ID;
    card                  : CARDINAL;
    incident              : NodeSet.T;
    sourceCT, targetCT    : Scheme.ID;
    sourceCard, targetCard: Scheme.Cardinality;
  BEGIN
    TRY
      checks.loop();
      checks.get(node, etype, found);
      WHILE found DO
        (* Each pair (node,etype) in checks denotes a node and an edge type
           for which checks on incoming/outgoing cardinality have to be
           done. *)
        type := InternGetNodeType(graph, node, tfound);
        IF tfound AND graph.scheme.existsEdgeTypeByNumber(etype) THEN
          graph.scheme.showEdgeTypeProps(
            etype, sourceCT, targetCT, sourceCard, targetCard);
          IF incoming THEN
            incident := Super.T.getSources(graph, node, etype.entity);
            card := incident.card();
            IF card < targetCard.min OR card > targetCard.max THEN
              RAISE TypedGraphPool.CardinalityError(graph.number());
            END;
          ELSE
            incident := Super.T.getTargets(graph, node, etype.entity);
            card := incident.card();
            IF card < sourceCard.min OR card > sourceCard.max THEN
              RAISE TypedGraphPool.CardinalityError(graph.number());
            END;
          END;
          incident.dispose();
        END;
        checks.get(node, etype, found);
      END;
    EXCEPT
      Scheme.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("TypedGraph.CardinalityChecks",
                                       "Scheme.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "TypedGraph.CardinalityChecks", "Scheme.NotDeclared"));
    | PersistentGraph.NodeNotFound =>
        RAISE Super.InternalError(
                ErrorSupport.Create("TypedGraph.CardinalityChecks",
                                    "PersistentGraph.NodeNotFound"));
    | PersistentGraph.NotOwner =>
        RAISE Super.InternalError(
                ErrorSupport.Create("TypedGraph.CardinalityChecks",
                                    "PersistentGraph.NotOwner"));
    | PersistentGraph.InternalError (info) =>
        RAISE
          Super.InternalError(
            ErrorSupport.Propagate("TypedGraph.CardinalityChecks",
                                   "PersistentGraph.InternalError", info));
    | InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.CardinalityChecks", "InternalError", info));
    END
  END CardinalityChecks;

PROCEDURE InternGetNodeType (graph: T; node: Node.T; VAR found: BOOLEAN):
  Scheme.ID RAISES {Access.Locked, InternalError} =
  VAR
    res : Scheme.ID;
    attr: TEXT;
  BEGIN
    TRY
      (* lookup in cache *)
      found := graph.typeCache.get(node, attr);
      IF NOT found THEN
        (* index wasn't found in cache, search in graph *)
        attr := Super.T.getIndex(graph, node, TypeIndex, found);
        IF found THEN
          (* if index exists, store in cache *)
          graph.typeCache.put(node, attr);
        END;
      END;
      IF found THEN
        res.graph := AttributeValue.TextToCard(Text.Sub(attr, 0, 4));
        res.entity := AttributeValue.TextToCard(Text.Sub(attr, 4, 4));
        RETURN res;
      ELSE
        RETURN Scheme.ID{graph.number(), 0};
      END;
    EXCEPT
      PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.InternGetNodeType",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.InternGetNodeType",
                                            "PersistentGraph.NotOwner"));
    | PersistentGraph.NodeNotFound => (* This should have been checked by
                                         caller *)
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.InternGetNodeType",
                                    "PersistentGraph.NodeNotFound"));
    END;
  END InternGetNodeType;

PROCEDURE InternPutNodeType (graph: T; node: Node.T; type: Scheme.ID)
  RAISES {InternalError, Access.Locked} =
  VAR text: TEXT;
  BEGIN
    TRY
      text := AttributeValue.CardToText(type.graph)
                & AttributeValue.CardToText(type.entity);
      (* update graph and cache *)
      Super.T.putIndex(graph, node, TypeIndex, text);
      graph.typeCache.put(node, text);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.InternPutNodeType",
                              "PersistentGraph.InternalError", info));
    | Super.LogError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.InternPutNodeType", "Super.LogError", info));
    | PersistentGraph.IndexUsed =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.InternPutNodeType",
                                            "PersistentGraph.IndexUsed"));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.InternPutNodeType",
                                            "PersistentGraph.NotOwner"));
    | PersistentGraph.NodeNotFound => (* This should have been checked by
                                         caller *)
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.InternPutNodeType",
                                    "PersistentGraph.NodeNotFound"));
    END;
  END InternPutNodeType;

PROCEDURE CheckAttribute (    graph: T;
                              node : Node.T;
                              attr : Scheme.ID;
                          VAR ntype: Scheme.ID  ): BOOLEAN
  RAISES {NotOwner, NodeNotFound, Unknown, InternalError, Access.Locked} =
  (* Return value determines if an attr is declared for node type *)
  VAR
    declared, ntfound: BOOLEAN;
    cot              : Scheme.ID;
    aname            : TEXT;
  BEGIN
    TRY
      IF ErrorCheck.Node IN graph.actChecks THEN
        AssertNodeExists(graph, node);
      END;

      declared := graph.scheme.existsAttributeByNumber(attr);
      IF ErrorCheck.Attribute IN graph.actChecks AND NOT declared THEN
        RAISE Unknown;
      ELSIF NOT declared THEN
        RETURN FALSE;
      END;

      ntype := InternGetNodeType(graph, node, ntfound);
      IF (ErrorCheck.Type IN graph.actChecks
            OR ErrorCheck.Attribute IN graph.actChecks) AND NOT ntfound THEN
        RAISE Unknown;
      ELSIF NOT ntfound THEN
        RETURN FALSE;
      END;

      graph.scheme.getAttributeNameAndClass(attr, cot, aname);
      IF ErrorCheck.Attribute IN graph.actChecks THEN
        IF NOT (cot = ntype OR graph.scheme.isSubClassOrType(ntype, cot)) THEN
          RAISE Unknown;
        END;
      END;
      RETURN TRUE;
    EXCEPT
    | Scheme.NotDeclared =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.CheckAttribute",
                                            "Scheme.NotDeclared"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.CheckAttribute",
                                       "Scheme.InternalError", info));
    END;
  END CheckAttribute;

PROCEDURE InternPutAttribute (graph    : T;
                              node     : Node.T;
                              attr     : Scheme.ID;
                              start    : CARDINAL;
                              val      : TEXT;
                              typeCheck: BOOLEAN;
                              type     : CARDINAL   )
  RAISES {NodeNotFound, Unknown, WrongType, Access.Locked, InternalError,
          LogError, NotOwner} =
  VAR
    ntype           : Scheme.ID;
    kind            : Scheme.AttributeKind;
    indexprops      : Scheme.IndexProperties;
    valuetype       : CARDINAL;
    attCard         : Scheme.Cardinality;
    constLength     : BOOLEAN;
    length, vlen    : CARDINAL;
    old, new        : TEXT;
    found           : BOOLEAN;
    needInvalidation: NodeLabelRelation.T;
    physNo          : CARDINAL;
    physStart       : CARDINAL;
    physMaxLen      : INTEGER;
    wasValid        : BOOLEAN;
  BEGIN
    TRY
      IF NOT CheckAttribute(graph, node, attr, ntype) THEN
        Super.T.putAttribute(graph, node, attr.entity, start, val);
        RETURN;
      END;

      vlen := Text.Length(val);
      graph.scheme.showAttributeProps(
        attr, kind, indexprops, valuetype, attCard, constLength, length);
      IF constLength AND vlen + start > length THEN RAISE WrongType END;
      IF typeCheck AND type # valuetype THEN RAISE WrongType END;

      (* check physical placement restritions *)
      graph.scheme.showAttributePlacement(
        ntype, attr, physNo, physStart, physMaxLen);
      IF NOT constLength AND physMaxLen >= 0 THEN
        IF vlen + start > physMaxLen THEN RAISE WrongType END;
        (* If constLength, then physMaxLen = length -> no check
           necessary *)
      END;

      CASE kind OF
        Scheme.AttributeKind.Intrinsic, Scheme.AttributeKind.Derived =>
          IF indexprops = Scheme.IndexProperties.Normal THEN
            old := Super.T.getAttribute(graph, node, physNo, physStart, 1);
            wasValid := old # NIL AND Text.Length(old) > 0;
            Super.T.putAttribute(
              graph, node, physNo, physStart + start, val);
          ELSE                   (* key or index *)
            old := Super.T.getIndex(graph, node, physNo, found);
            IF found THEN
              Super.T.deleteIndex(graph, node, physNo, old);
              new := Text.Sub(old, 0, start) & val
                       & Text.Sub(old, start + vlen);
              wasValid := TRUE;
            ELSE
              new := val;
            END;
            Super.T.putIndex(graph, node, physNo, new);
          END;
      | Scheme.AttributeKind.Meta =>
          graph.scheme.putMetaAttribute(ntype, attr, start, val);
      | Scheme.AttributeKind.Dummy => RAISE WrongType;
      END;
      (* if something was written *)
      IF start + vlen > 0 THEN
        IF wasValid THEN
          (* invalidate depending attributes *)
          needInvalidation :=
            CollectDependingOnAttribute(graph, node, attr);
          PropagateNullity(graph, needInvalidation);
          needInvalidation.dispose();
        ELSE
          (* set defining dummies to valid, in order to be able to
             propagate invalidity next time one of the defining attributes
             in the n-context is written. *)
          PropagateValidDummies(graph, node, attr);
        END;
        (* if attr is guarded then it can now be set to valid *)
        IF graph.scheme.isGuardedAttribute(attr) THEN
          MarkValid(graph, node, graph.scheme.getGuard(attr));
        END;
      END;
    EXCEPT
      PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.InternPutAttribute",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.IndexUsed =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.InternPutAttribute",
                                    "PersistentGraph.IndexUsed"));
    | PersistentGraph.IndexUnused =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.InternPutAttribute",
                                    "PersistentGraph.IndexUnused"));
    | Super.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("TypedGraph.InternPutAttribute",
                                   "ChgMgmtGraph.InternalError", info));
    | Super.LogError (info) =>
        RAISE
          LogError(ErrorSupport.Propagate("TypedGraph.InternPutAttribute",
                                          "ChgMgmtGraph.LogError", info));
    | Scheme.NotDeclared =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "TypedGraph.InternPutAttribute", "Scheme.NotDeclared"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.InternPutAttribute",
                                       "Scheme.InternalError", info));
    END;
  END InternPutAttribute;

PROCEDURE InternGetAttribute (graph : T;
                              node  : Node.T;
                              attr  : Scheme.ID;
                              start : CARDINAL;
                              length: CARDINAL   ): TEXT
  RAISES {Access.Locked, Unknown, WrongType, CyclicEvaluation,
          NodeNotFound, InternalError, LogError, NotOwner} =
  VAR
    ntype      : Scheme.ID;
    kind       : Scheme.AttributeKind;
    indexprops : Scheme.IndexProperties;
    valuetype  : CARDINAL;
    attCard    : Scheme.Cardinality;
    constLength: BOOLEAN;
    plength    : CARDINAL;
    value, test: TEXT;
    found      : BOOLEAN;
    physNo     : CARDINAL;
    physStart  : CARDINAL;
    physMaxLen : INTEGER;

  PROCEDURE ReadAttribute (VAR value: TEXT): BOOLEAN
    RAISES {WrongType, InternalError, Access.Locked,
            PersistentGraph.NotOwner} =
    VAR valid: BOOLEAN;
    BEGIN
      TRY
        CASE kind OF
          Scheme.AttributeKind.Intrinsic, Scheme.AttributeKind.Derived =>
            IF indexprops = Scheme.IndexProperties.Normal THEN
              value := Super.T.getAttribute(
                         graph, node, physNo, physStart + start, length);
              IF Text.Length(value) = 0 THEN
                IF start > 0 THEN
                  (* This attribute might be valid, but too short *)
                  test :=
                    Super.T.getAttribute(graph, node, physStart, 0, 1);
                  valid :=
                    Text.Length(test) > 0; (* no need to recompute *)
                ELSE
                  valid := FALSE;
                END;
              ELSE               (* Length(value) > 0 *)
                valid := TRUE;
              END;
            ELSE                 (* key or index *)
              test := Super.T.getIndex(graph, node, physNo, found);
              IF found THEN
                value := Text.Sub(test, start, length);
                valid := TRUE;
              ELSE
                value := "";
                valid := FALSE
              END;
            END;
        | Scheme.AttributeKind.Meta =>
            value :=
              graph.scheme.getMetaAttribute(ntype, attr, start, length);
            IF Text.Length(value) = 0 THEN
              IF start > 0 THEN
                (* This attribute might be valid, but too short *)
                test := graph.scheme.getMetaAttribute(ntype, attr, 0, 1);
                valid := Text.Length(test) > 0; (* no need to recompute *)
              ELSE
                valid := FALSE;
              END;
            ELSE                 (* Length(value) > 0 *)
              valid := TRUE;
            END;
        | Scheme.AttributeKind.Dummy => RAISE WrongType;
        END;
        RETURN valid;
      EXCEPT
        PersistentGraph.NodeNotFound =>
          RAISE InternalError(
                  ErrorSupport.Create("TypedGraph.ReadAttribute",
                                      "PersistentGraph.NodeNotFound"));
      | PersistentGraph.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "TypedGraph.ReadAttribute",
                                "PersistentGraph.InternalError", info));
      | Scheme.InternalError (info) =>
          RAISE InternalError(
                  ErrorSupport.Propagate("TypedGraph.ReadAttribute",
                                         "Scheme.InternalError", info));
      | Scheme.NotDeclared =>
          RAISE
            InternalError(ErrorSupport.Create("TypedGraph.ReadAttribute",
                                              "Scheme.NotDeclared"));
      END;
    END ReadAttribute;

  BEGIN
    TRY
      IF NOT CheckAttribute(graph, node, attr, ntype) THEN
        RETURN
          Super.T.getAttribute(graph, node, attr.entity, start, length);
      END;

      graph.scheme.showAttributeProps(
        attr, kind, indexprops, valuetype, attCard, constLength, plength);
      graph.scheme.showAttributePlacement(
        ntype, attr, physNo, physStart, physMaxLen);
      IF ErrorCheck.Attribute IN graph.actChecks AND physMaxLen >= 0 THEN
        (* check not to read beyond placement *)
        IF start + length > physMaxLen THEN RAISE WrongType; END;
      END;

      IF ReadAttribute(value) THEN
        RETURN value;
      ELSE
        IF EvaluateAttribute(graph, node, ntype, attr) THEN
          EVAL ReadAttribute(value);
          RETURN value;
        ELSE
          RETURN "";
        END;

      END;

    EXCEPT
      PersistentGraph.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraph.NotOwner => RAISE NotOwner;
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.InternGetAttribute",
                              "PersistentGraph.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "TypedGraph.InternGetAttribute", "Scheme.NotDeclared"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.InternGetAttribute",
                                       "Scheme.InternalError", info));
    END;
  END InternGetAttribute;

PROCEDURE EvaluateAttribute (graph: T; node, ntype: Node.T; attr: Scheme.ID):
  BOOLEAN
  RAISES {CyclicEvaluation, LogError, InternalError, Access.Locked} =
  VAR
    wasEvaluating: BOOLEAN;
    evaluate     : Evaluator;
    functionname : TEXT;
  BEGIN
    TRY
      wasEvaluating := graph.evaluatingAttribute;
      (* Can we recompute the attribute ? *)
      IF wasEvaluating AND graph.pendingEvaluations.in(node, attr.entity) THEN
        (* we already started reevaluation for this attribute, starting it
           again would result in an infinitive loop. *)
        RAISE CyclicEvaluation;
      END;

      (* Find evaluation function *)
      functionname := graph.scheme.getEvaluationFunction(ntype, attr);
      IF functionname = NIL THEN
        (* no evaluation function -> no recomputation *)
        RETURN FALSE;
      END;
      IF NOT EFTRetrieve(graph.evaluationTable, functionname, evaluate) THEN
        (* no evaluation function -> no recomputation *)
        RETURN FALSE;
      END;

      (* Finally we are ready to call the evaluation function.  Just note
         that we are in a recomputation for (node, attr). *)
      graph.evaluatingAttribute := TRUE;
      graph.pendingEvaluations.insert(node, attr.entity);

      evaluate.apply(graph, node, attr);

      (* everything fine *)
      graph.pendingEvaluations.deleteElement(node, attr.entity);
      graph.evaluatingAttribute := wasEvaluating;
      RETURN TRUE;
    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.EvaluateAttribute",
                                       "Scheme.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.EvaluateAttribute",
                                            "Scheme.NotDeclared"));
    END;
  END EvaluateAttribute;

PROCEDURE EvaluateGuardedAttributes (graph: T; attr: Scheme.ID)
  RAISES {Access.Locked, LogError, InternalError, CyclicEvaluation} =
  VAR
    marked      : NodeSet.T;
    guard       : Scheme.ID;
    node        : Node.T;
    found, valid: BOOLEAN;
    ntype       : Scheme.ID;
    ntfound     : BOOLEAN;
  BEGIN
    TRY
      guard := graph.scheme.getGuard(attr);
      marked :=
        Super.T.getNodesWithIndex(graph, guard.entity, InvalidFlag);
      marked.loop();
      node := marked.get(found);
      WHILE found DO
        MarkValid(graph, node, guard);
        ntype := InternGetNodeType(graph, node, ntfound);
        EVAL Super.T.getIndex(graph, node, attr.entity, valid);
        IF NOT valid THEN
          EVAL EvaluateAttribute(graph, node, ntype, attr);
        END;
        node := marked.get(found);
      END;
      marked.dispose();
    EXCEPT
      Scheme.NotDeclared =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.EvaluateGuardedAttributes",
                                    "Scheme.NotDeclared"));
    | Scheme.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.EvaluateGuardedAttributes",
                              "Scheme.InternalError", info));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.EvaluateGuardedAttributes",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.EvaluateGuardedAttributes",
                                    "PersistentGraph.NotOwner"));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.EvaluateGuardedAttributes",
                                    "PersistentGraph.NodeNotFound"));
    END;
  END EvaluateGuardedAttributes;

PROCEDURE MarkInvalid (graph: T; node: Node.T; guardno: Scheme.ID)
  RAISES {Access.Locked, InternalError, LogError} =
  VAR
    old  : TEXT;
    found: BOOLEAN;
  BEGIN
    TRY
      old := Super.T.getIndex(graph, node, guardno.entity, found);
      IF found THEN
        IF NOT Text.Equal(old, InvalidFlag) THEN
          Super.T.deleteIndex(graph, node, guardno.entity, old);
          Super.T.putIndex(graph, node, guardno.entity, InvalidFlag);
        END;
      ELSE
        Super.T.putIndex(graph, node, guardno.entity, InvalidFlag);
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.MarkInvalid", "Super.InternalError", info));
    | Super.LogError (info) =>
        RAISE
          LogError(ErrorSupport.Propagate(
                     "TypedGraph.MarkInvalid", "Super.LogError", info));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.MarkInvalid",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound => (* should have been checked by
                                         calling procedure *)
        RAISE InternalError(ErrorSupport.Create(
                              "TypedGraph.MarkInvalid",
                              "PersistentGraph.NodeNotFound"));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.MarkInvalid",
                                            "PersistentGraph.NotOwner"));
    | PersistentGraph.IndexUsed => (* Should never occur here *)
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.MarkInvalid",
                                            "PersistentGraph.IndexUsed"));
    | PersistentGraph.IndexUnused => (* should never occur here *)
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.MarkInvalid",
                                            "PersistentGraph.IndexUnused"));
    END;
  END MarkInvalid;

PROCEDURE MarkValid (graph: T; node: Node.T; guardno: Scheme.ID)
  RAISES {Access.Locked, LogError, InternalError} =
  VAR
    old  : TEXT;
    found: BOOLEAN;
  BEGIN
    TRY
      old := Super.T.getIndex(graph, node, guardno.entity, found);
      IF found THEN
        Super.T.deleteIndex(graph, node, guardno.entity, old);
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraph.MarkValid", "Super.InternalError", info));
    | Super.LogError (info) =>
        RAISE LogError(ErrorSupport.Propagate(
                         "TypedGraph.MarkValid", "Super.LogError", info));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.MarkValid",
                                            "PersistentGraph.NotOwner"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.MarkValid",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound => (* should have been checked by
                                         calling procedure *)
        RAISE InternalError(
                ErrorSupport.Create(
                  "TypedGraph.MarkValid", "PersistentGraph.NodeNotFound"));
    | PersistentGraph.IndexUnused => (* should never occur here *)
        RAISE InternalError(
                ErrorSupport.Create(
                  "TypedGraph.MarkValid", "PersistentGraph.IndexUnused"));
    END;
  END MarkValid;

PROCEDURE PropagateValidDummies (graph: T; node: Node.T; attr: Scheme.ID)
  RAISES {InternalError, Access.Locked, LogError} =
  VAR
    valids                      : NodeLabelRelation.T;
    vnode                       : Node.T;
    vattr, vntype               : Scheme.ID;
    vfound, vntfound            : BOOLEAN;
    dependencies                : Scheme.IDSet;
    totest                      : NodeSet.T;
    ok, dfound, nfound          : BOOLEAN;
    testnode                    : Node.T;
    dep, ntype                  : Scheme.ID;
    dependentAttr, dependsOnAttr: Scheme.ID;
    dependentCOT, dependsOnCOT  : Scheme.ID;
    dkind                       : Scheme.DependencyKind;
    etype                       : Scheme.ID;
    kind                        : Scheme.AttributeKind;
  BEGIN
    TRY
      valids := NodeLabelRelation.New();
      valids.insert(node, attr.entity);

      vnode := node;
      vattr := attr;
      vfound := TRUE;
      WHILE vfound DO
        vntype := InternGetNodeType(graph, vnode, vntfound);
        (* get all dependencies of vattr *)
        dependencies :=
          graph.scheme.getDefiningDependencies(vntype, vattr);
        dependencies.loop();
        dep := dependencies.get(dfound);
        WHILE dfound DO
          graph.scheme.showDependencyInfo(
            dep, dependentAttr, dependsOnAttr, dependentCOT, dependsOnCOT,
            dkind, etype);
          (* is dependsOnAttr a Dummy ? *)
          VAR
            indexprops : Scheme.IndexProperties;
            valuetype  : CARDINAL;
            attCard    : Scheme.Cardinality;
            constLength: BOOLEAN;
            plength    : CARDINAL;
          BEGIN
            graph.scheme.showAttributeProps(
              dependsOnAttr, kind, indexprops, valuetype, attCard,
              constLength, plength);
          END;
          IF kind = Scheme.AttributeKind.Dummy THEN
            CASE dkind OF
              Scheme.DependencyKind.Self =>
                totest := NodeSet.New();
                totest.insert(vnode);
            | Scheme.DependencyKind.Incoming =>
                (* incoming relative to dependent *)
                totest := Super.T.getSources(graph, vnode, etype.entity);
            | Scheme.DependencyKind.Outgoing =>
                (* outgoing relative to dependent *)
                totest := Super.T.getTargets(graph, vnode, etype.entity);
            END;
            totest.loop();
            testnode := totest.get(nfound);
            WHILE nfound DO
              ntype := InternGetNodeType(graph, testnode, ok);
              IF (ntype = dependsOnCOT)
                   OR graph.scheme.isSubClassOrType(ntype, dependsOnCOT) THEN
                (* We only need to propagate if this attribute is
                   invalid *)
                IF Text.Length(
                     Super.T.getAttribute(
                       graph, testnode, dependsOnAttr.entity, 0, 1)) = 0 THEN
                  Super.T.putAttribute(
                    graph, testnode, dependsOnAttr.entity, 0, "@");
                  valids.insert(testnode, dependsOnAttr.entity);
                END;
              END;
              testnode := totest.get(nfound);
            END;
            totest.dispose();
          END;
          dep := dependencies.get(dfound);
        END;
        dependencies.dispose();
        valids.extractAnyElement(vnode, vattr.entity, vfound);
      END;
      valids.dispose();
    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.PropagateValidDummies",
                                       "Scheme.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE InternalError(ErrorSupport.Create(
                              "TypedGraph.PropagateValidDummies",
                              "Scheme.NotDeclared"));
    | Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.PropagateValidDummies",
                                       "Super.InternalError", info));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.PropagateValidDummies",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.PropagateValidDummies",
                                    "PersistentGraph.NotOwner"));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.PropagateValidDummies",
                                    "PersistentGraph.NodeNotFound"));
    | Super.LogError (info) =>
        RAISE LogError(
                ErrorSupport.Propagate("TypedGraph.PropagateValidDummies",
                                       "ChgMgmtGraph.LogError", info));
    END;
  END PropagateValidDummies;

PROCEDURE CollectDependingOnAttribute (graph: T;
                                       node : Node.T;
                                       attr : Scheme.ID):
  NodeLabelRelation.T RAISES {InternalError, Access.Locked} =
  (* collect all attribute instances that depend on node.attr *)
  VAR
    dependencies                : Scheme.IDSet;
    totest                      : NodeSet.T;
    ok, dfound, nfound          : BOOLEAN;
    testnode                    : Node.T;
    dep, ntype                  : Scheme.ID;
    dependentAttr, dependsOnAttr: Scheme.ID;
    dependentCOT, dependsOnCOT  : Scheme.ID;
    dkind                       : Scheme.DependencyKind;
    etype                       : Scheme.ID;
    res                         : NodeLabelRelation.T;
  BEGIN
    TRY
      res := NodeLabelRelation.New();
      dependencies := graph.scheme.getDependingOnAttribute(attr);
      dependencies.loop();
      dep := dependencies.get(dfound);
      WHILE dfound DO
        graph.scheme.showDependencyInfo(
          dep, dependentAttr, dependsOnAttr, dependentCOT, dependsOnCOT,
          dkind, etype);
        CASE dkind OF
          Scheme.DependencyKind.Self =>
            totest := NodeSet.New();
            totest.insert(node);
        | Scheme.DependencyKind.Incoming =>
            (* incoming relative to dependent *)
            totest := Super.T.getTargets(graph, node, etype.entity);
        | Scheme.DependencyKind.Outgoing =>
            (* outgoing relative to dependent *)
            totest := Super.T.getSources(graph, node, etype.entity);
        END;
        totest.loop();
        testnode := totest.get(nfound);
        WHILE nfound DO
          ntype := InternGetNodeType(graph, testnode, ok);
          IF (ntype = dependentCOT)
               OR (graph.scheme.isSubClassOrType(ntype, dependentCOT)
                     AND graph.scheme.dependsOn(ntype, dependentAttr, dep)) THEN
            res.insert(testnode, dependentAttr.entity);
          END;
          testnode := totest.get(nfound);
        END;
        totest.dispose();
        dep := dependencies.get(dfound);
      END;
      dependencies.dispose();
      RETURN res;
    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.CollectDependingOnAttribute",
                              "Scheme.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE InternalError(ErrorSupport.Create(
                              "TypedGraph.CollectDependingOnAttribute",
                              "Scheme.NotDeclared"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.CollectDependingOnAttribute",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner =>
        RAISE InternalError(ErrorSupport.Create(
                              "TypedGraph.CollectDependingOnAttribute",
                              "PersistentGraph.NotOwner"));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "TypedGraph.CollectDependingOnAttribute",
                              "PersistentGraph.NodeNotFound"));
    END;
  END CollectDependingOnAttribute;

PROCEDURE CollectDependingOnEdge (graph   : T;
                                  node    : Node.T;
                                  etype   : Scheme.ID;
                                  incoming: BOOLEAN    ):
  NodeLabelRelation.T RAISES {InternalError, Access.Locked} =
  (* collect all attribute instances of node that depend on an
     incoming/outgoing edge of type etype *)
  VAR
    dependencies                : Scheme.IDSet;
    ok, dfound                  : BOOLEAN;
    dep, ntype                  : Scheme.ID;
    dependentAttr, dependsOnAttr: Scheme.ID;
    dependentCOT, dependsOnCOT  : Scheme.ID;
    dkind                       : Scheme.DependencyKind;
    detype                      : Scheme.ID;
    res                         : NodeLabelRelation.T;
  BEGIN
    TRY
      res := NodeLabelRelation.New();
      (* dependencies on etype *)
      ntype := InternGetNodeType(graph, node, ok);
      dependencies := graph.scheme.getDependingOnEdgeType(etype, incoming);
      dependencies.loop();
      dep := dependencies.get(dfound);
      WHILE dfound DO
        graph.scheme.showDependencyInfo(
          dep, dependentAttr, dependsOnAttr, dependentCOT, dependsOnCOT,
          dkind, detype);

        IF (ntype = dependentCOT)
             OR (graph.scheme.isSubClassOrType(ntype, dependentCOT)
                   AND graph.scheme.dependsOn(ntype, dependentAttr, dep)) THEN
          res.insert(node, dependentAttr.entity);
        END;
        dep := dependencies.get(dfound);
      END;
      dependencies.dispose();
      RETURN res;
    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.CollectDependingOnEdge",
                                       "Scheme.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.CollectDependingOnEdge",
                                    "Scheme.NotDeclared"));
    END;
  END CollectDependingOnEdge;


PROCEDURE PropagateNullity (graph: T; invalids: NodeLabelRelation.T)
  RAISES {Access.Locked, InternalError, LogError} =
  VAR
    found, ifound: BOOLEAN;
    node         : Node.T;
    attr         : Scheme.ID;
    kind         : Scheme.AttributeKind;
    indexprops   : Scheme.IndexProperties;
    valuetype    : CARDINAL;
    attCard      : Scheme.Cardinality;
    constLength  : BOOLEAN;
    plength      : CARDINAL;
    newInvalids  : NodeLabelRelation.T;
    val          : TEXT;
    ntype        : Scheme.ID;
    ntfound      : BOOLEAN;
    physNo       : CARDINAL;
    physMaxLen   : INTEGER;
    physStart    : CARDINAL;
  BEGIN
    TRY
      attr.graph := graph.schemeNumber;
      invalids.extractAnyElement(node, attr.entity, found);
      WHILE found DO
        graph.scheme.showAttributeProps(
          attr, kind, indexprops, valuetype, attCard, constLength, plength);
        ntype := InternGetNodeType(graph, node, ntfound);
        IF ntfound THEN
          graph.scheme.showAttributePlacement(
            ntype, attr, physNo, physStart, physMaxLen);
        ELSE
          (* set defaults *)
          physStart := 0;
          physMaxLen := -1;
          physNo := attr.entity;
        END;
        IF indexprops = Scheme.IndexProperties.Normal THEN
          IF physMaxLen >= 0 THEN
            val := Super.T.getAttribute(
                     graph, node, physNo, physStart, physMaxLen);
          ELSE
            val := Super.T.getAttribute(
                     graph, node, physNo, physStart, LAST(CARDINAL));
          END;
          IF Text.Length(val) > 0 THEN
            IF physMaxLen < 0 THEN
              Super.T.truncateAttribute(graph, node, physNo, physStart);
            ELSE
              Super.T.putAttribute(
                graph, node, physNo, physStart, Zeroes(physMaxLen));
            END;
            newInvalids := CollectDependingOnAttribute(graph, node, attr);
            invalids.union(newInvalids);
            newInvalids.dispose();
          END;
        ELSE
          (* key or index attribute *)
          val := Super.T.getIndex(graph, node, attr.entity, ifound);
          IF ifound THEN
            Super.T.deleteIndex(graph, node, attr.entity, val);
            newInvalids := CollectDependingOnAttribute(graph, node, attr);
            invalids.union(newInvalids);
            newInvalids.dispose();
            IF graph.scheme.isGuardedAttribute(attr) THEN
              MarkInvalid(graph, node, graph.scheme.getGuard(attr));
            END;
          END;
        END;
        invalids.extractAnyElement(node, attr.entity, found);
      END;
    EXCEPT
    | Scheme.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.PropagateNullity",
                                       "Scheme.InternalError", info));
    | Scheme.NotDeclared =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.PropagateNullity",
                                            "Scheme.NotDeclared"));
    | Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraph.PropagateNullity",
                                       "Super.InternalError", info));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraph.PropagateNullity",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraph.PropagateNullity",
                                            "PersistentGraph.NotOwner"));
    | PersistentGraph.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.PropagateNullity",
                                    "PersistentGraph.NodeNotFound"));
    | PersistentGraph.IndexUnused =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraph.PropagateNullity",
                                    "PersistentGraph.IndexUnused"));
    | Super.LogError (info) =>
        RAISE
          LogError(ErrorSupport.Propagate("TypedGraph.PropagateNullity",
                                          "ChgMgmtGraph.LogError", info));
    END;
  END PropagateNullity;

(* PROCEDURE AttributeNo (attrId: Scheme.ID): CARDINAL = *)
(* BEGIN *)
(* RETURN attrId.entity; *)
(* END AttributeNo; *)

(* PROCEDURE AttributeID (graph: T; attrNo: CARDINAL): Scheme.ID = *)
(* BEGIN *)
(* RETURN Scheme.ID{graph.schemeNumber, attrNo}; *)
(* END AttributeID; *)

PROCEDURE NodeLabelToNodeTypeRelation (graph: T; in: NodeLabelRelation.T):
  NodeTypeRelation.T =
  VAR
    res              := NodeTypeRelation.New();
    found: BOOLEAN;
    node : Node.T;
    type : Scheme.ID;
    label: CARDINAL;
  BEGIN
    type.graph := graph.schemeNumber;
    in.loop();
    in.get(node, label, found);
    WHILE found DO
      type.entity := label;
      NodeTypeRelation.T.insert(res, node, type);
      in.get(node, label, found);
    END;
    RETURN res;
  END NodeLabelToNodeTypeRelation;

PROCEDURE CardSetToIDSet (graph: T; in: CardSet.T): Scheme.IDSet =
  VAR
    res              := NodeSet.New();
    found: BOOLEAN;
    card : CARDINAL;
    type : Scheme.ID;
  BEGIN
    type.graph := graph.schemeNumber;
    in.loop();
    card := in.get(found);
    WHILE found DO
      type.entity := card;
      res.insert(type);
      card := in.get(found);
    END;
    RETURN res;
  END CardSetToIDSet;

(* Hash table implementation for evaluation functions *)
CONST EFTableSize = 53;

TYPE
  HashEntry = RECORD
                name     : TEXT;
                evaluator: Evaluator;
                next     : REF HashEntry;
              END;

  EFTable = ARRAY [0 .. EFTableSize - 1] OF REF HashEntry;

PROCEDURE EFTInit (VAR t: EFTable) =
  BEGIN
    t := ARRAY [0 .. EFTableSize - 1] OF REF HashEntry{NIL, ..};
  END EFTInit;

PROCEDURE EFTInsert (VAR t: EFTable; name: TEXT; eval: Evaluator) =
  VAR hash: CARDINAL;
  BEGIN
    hash := Text.Hash(name) MOD EFTableSize;
    (* Just insert as first element of the collision list.  If other
       entries with the same name exists - never mind. *)
    t[hash] :=
      NEW(REF HashEntry, name := name, evaluator := eval, next := t[hash]);
  END EFTInsert;

PROCEDURE EFTRetrieve (READONLY t: EFTable; name: TEXT; VAR eval: Evaluator):
  BOOLEAN =
  VAR
    hash: CARDINAL;
    h   : REF HashEntry;
  BEGIN
    hash := Text.Hash(name) MOD EFTableSize;
    h := t[hash];
    WHILE h # NIL AND NOT Text.Equal(name, h^.name) DO h := h^.next; END;
    IF h # NIL THEN eval := h^.evaluator; END;
    RETURN h # NIL;
  END EFTRetrieve;

BEGIN
END TypedGraph.
