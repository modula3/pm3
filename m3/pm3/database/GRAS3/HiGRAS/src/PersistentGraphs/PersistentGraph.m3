MODULE PersistentGraph;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.12  1998/03/18 12:13:22  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.11  1998/03/17 14:14:16  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.10  1998/03/09 14:56:40  roland
    New method local that returns true if the graph is a local graph.

    Revision 1.9  1998/01/21 12:35:43  roland
    New method baseName to determine filename.

    Revision 1.8  1997/11/21 09:37:03  roland
    New GraphEvents PutAttribute and TruncateAttribute replace ModifyAttribute

    Revision 1.7  1997/11/12 15:23:35  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

    Revision 1.6  1997/11/10 10:44:27  roland
    Added event signalling for (post) graph events.

    Revision 1.5  1997/07/21 10:43:02  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.4  1997/06/17 17:05:25  roland
    Bugfix: RETURN should not stand in FINALLY-Part.

    Revision 1.3  1997/04/24 14:32:45  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.2  1997/04/23 14:33:52  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:29  roland
    Subsystem PersistentGraph adapted to handle graph boundary crossing
    edges. This has consequences on the architecture of the subsystem as
    well as on the graph model and interface.

    Graphs are organized in pools. Every graph has a number in the
    pool. Pools are the units of transaction management. Two graphs might
    be related by one external relation storage storing the edges between
    nodes of them. Nodes are identified by pairs (graph, entity), where
    graph is the number of the graph in the pool and entity the node
    number within the graph. Graphs and external relation storages are
    administered by the pool in a separate graph.

    Revision 1.21  1997/02/20 16:19:05  roland
    PageCache must not be initialized prior to PersistentGraphSystem.Login.

    Revision 1.20  1997/02/06 13:44:14  roland
    Attributes of LongAttributeStorage are now cached in LongAttributeCache.

    Revision 1.19  1996/11/22 16:06:22  roland
    Exception-Handling in PersistentGraph changed so that no exceptions of
    VirtualResource are passed to calling procedures.

    Revision 1.18  1996/11/20 12:23:08  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.17  1996/11/14 14:17:44  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Access in mode ReadOnlyShared is now considered when opening graphs.

    Revision 1.16  1996/09/17 13:09:11  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.15  1996/09/09 11:50:26  rbnix
        In method close: check for delayed triggers added. (This is
        related to new resource event handling.)

    Revision 1.14  1996/08/23 14:51:11  rbnix
        Administration operations moved to a new module.

    Revision 1.13  1996/08/06 16:26:38  roland
    Merge of PAGESERVER and main branch.

    Revision 1.11.2.4  1996/07/25 09:20:56  rbnix
        Methods open and create merged.

    Revision 1.11.2.3  1996/07/24 13:48:37  rbnix
        Name of data field T.file changed to T.dataFile.

        Name of main data file changed to database.

    Revision 1.11.2.2  1996/07/24 09:21:16  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

        Bug fixed: the page cache is now initialized only once.

    Revision 1.11.2.1  1996/04/29 13:43:49  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

    Revision 1.11  1995/11/28 09:50:30  grover
    added getPath

# Revision 1.10  1995/07/05  16:03:18  alex
# Add the method deleteAttribute to PersistentGraph
#
# Revision 1.9  1995/06/08  14:50:38  grover
# removed some bugs about index - operations
#
# Revision 1.8  1994/08/15  13:06:21  pk
# Same bug removed from checkOut and open.
#
# Revision 1.7  1994/08/03  13:08:59  pk
# Serious bug removed: Create returned a NEW(T).
#
# Revision 1.6  1994/04/01  16:14:38  pk
# forward/backward parameters for deleteNode renamed to out/inEdges.
#
# Revision 1.5  1994/04/01  15:19:00  pk
# names parameter for deleteNode renamed to indexes.
#
# Revision 1.4  1994/04/01  15:10:00  pk
# New method errorCheckOn.
#
# Revision 1.3  1994/04/01  14:50:44  pk
# CreateEntity/Node returns the node number.
#
# Revision 1.2  1994/03/30  18:32:02  pk
# Adaptions for new Files subsystem.
#
# Revision 1.1  1994/01/20  18:41:39  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT PageFile, Access, NodeSet, Node, NodeLabelRelation, CardRelation,
       CardSet, TaggedNameSet, AttributeDescriptionSet,
       PersistentGraphPool, Database, InternPersistentGraphPool,
       ExtConnectorStorage;
IMPORT ErrorSupport, VirtualResource;
IMPORT Pathname;
IMPORT GraphEvents, RuleEngine, PersistentGraphEventHandler;

REVEAL
  T = Public BRANDED OBJECT
        db      : Database.T;             (* the graph *)
        num     : CARDINAL;               (* its number in the pool *)
        name    : Pathname.T;             (* its name in the pool *)
        pool    : PersistentGraphPool.T;  (* its pool *)
        isLocal : BOOLEAN;                (* file is local or remote *)
      OVERRIDES
        open  := Open;
        close := Close;

        errorCheckOn := ErrorCheckOn;
        number       := GraphNumber;
        local        := Local;
        baseName     := BaseName;

        createNodeNumber := CreateNodeNumber;
        createNode       := CreateNode;
        ownsNode         := OwnsNode;
        deleteNode       := DeleteNode;
        deleteNodeNoInfo := DeleteNodeNoInfo;
        existsNode       := ExistsNode;
        putNodeLabel     := PutNodeLabel;
        getNodeLabel     := GetNodeLabel;

        putAttribute           := PutAttribute;
        deleteAttribute        := DeleteAttribute;
        getAttribute           := GetAttribute;
        truncateAttribute      := TruncateAttribute;
        getAllAttributeNumbers := GetAllAttributeNumbers;

        putIndex           := PutIndex;
        deleteIndex        := DeleteIndex;
        getIndex           := GetIndex;
        getNodesWithIndex  := GetNodesWithIndex;
        getIndexNosForNode := GetIndexNosForNode;

        createEdge      := CreateEdge;
        deleteEdge      := DeleteEdge;
        existsEdge      := ExistsEdge;
        getTargets      := GetTargets;
        getSources      := GetSources;
        getTargetsTo    := GetTargetsTo;
        getSourcesFrom  := GetSourcesFrom;
        getAllTargets   := GetAllTargets;
        getAllSources   := GetAllSources;
        getAllOutLabels := GetAllOutLabels;
        getAllInLabels  := GetAllInLabels;
        getAllOutEdges  := GetAllOutEdges;
        getAllInEdges   := GetAllInEdges;
      END;


PROCEDURE EndTransaction (graph: T; abort: BOOLEAN) =
  BEGIN
    TRY
      IF abort THEN
        VirtualResource.T.abortTransaction(graph.pool);
      ELSE
        VirtualResource.T.commitTransaction(graph.pool);
      END;
    EXCEPT
      VirtualResource.FatalError,
          VirtualResource.NotInTransaction => (* ignore *)
    END;
  END EndTransaction;

(****************************************
 graph operations
 ****************************************)

PROCEDURE Open (graph       : T;
                pool        : PersistentGraphPool.T;
                baseName    : Pathname.T;
                access      : AccessMode;
                new         : BOOLEAN;
                local       : BOOLEAN;
                errorChecks : BOOLEAN                ): T
  RAISES {NotExistent, Access.Locked, InUse, PageFile.NoAccess,
          InternalError, Access.Denied} =

  PROCEDURE Mode (pool: PersistentGraphPool.T; access: AccessMode):
    Access.Mode =
    BEGIN
      CASE access OF
        AccessMode.Inherit => RETURN pool.getAccessMode();
      | AccessMode.ReadWriteShared => RETURN Access.Mode.ReadWriteShared;
      | AccessMode.ReadOnlyShared => RETURN Access.Mode.ReadOnlyShared;
      | AccessMode.ReadWriteExclusive =>
          RETURN Access.Mode.ReadWriteExclusive;
      END;
    END Mode;

  VAR abort: BOOLEAN := FALSE;
  BEGIN
    TRY
      graph.name    := baseName;
      graph.pool    := pool;
      graph.isLocal := local;

      TRY
        VirtualResource.T.beginTransaction(graph.pool);

        IF new THEN
          (* look if the database already exists locally *)
          IF pool.existsGraph(graph.name, graph.isLocal) THEN
            (* is someone using it? *)
            IF pool.graphInUse(graph.name, graph.isLocal) THEN
              abort := TRUE;
              RAISE InUse;
            END;
            pool.deleteGraph(graph.name, graph.isLocal);
          END;
        ELSE
          IF NOT pool.existsGraph(graph.name, graph.isLocal) THEN
            abort := TRUE;
            RAISE NotExistent;
          END;
        END;
        (*--- open database file ---*)
        pool.openDB(graph.name, local, Mode(pool, access), errorChecks,
                    graph.num, graph.db);
      EXCEPT
        PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "PersistentGraph.Open",
                            "PersistentGraphPool.InternalError", info));
      | VirtualResource.FatalError (info) =>
          abort := TRUE;
          RAISE InternalError(ErrorSupport.Propagate(
                                "PersistentGraph.Open",
                                "VirtualResource.FatalError", info));
      | PersistentGraphPool.InUse => abort := TRUE; RAISE InUse;
      | PersistentGraphPool.NotExistent =>
          abort := TRUE;
          RAISE InternalError(
                  ErrorSupport.Create("PersistentGraph.Open",
                                      "PersistentGraphPool.NotExistent"));
      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
    RETURN graph;
  END Open;


PROCEDURE Close (graph: T) RAISES {InternalError} =
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    TRY
      TRY
        VirtualResource.T.beginTransaction(graph.pool);
        graph.pool.closeDB(graph.num);
      EXCEPT
        PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "PersistentGraph.Close",
                            "PersistentGraphPool.InternalError", info));
      | VirtualResource.FatalError (info) =>
          abort := TRUE;
          RAISE InternalError(ErrorSupport.Propagate(
                                "PersistentGraph.Close",
                                "VirtualResource.FatalError", info));
(* exception never raised yet *)
(*      | Database.InternalError (info) => *)
(*          abort := TRUE; *)
(*          RAISE InternalError( *)
(*                  ErrorSupport.Propagate("PersistentGraph.Close", *)
(*                                         "Database.InternalError", info)); *)

      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
  END Close;

PROCEDURE ErrorCheckOn (graph: T): BOOLEAN =
  BEGIN
    RETURN graph.db.errorCheckOn();
  END ErrorCheckOn;

PROCEDURE GraphNumber (graph: T): CARDINAL =
  BEGIN
    RETURN graph.num;
  END GraphNumber;

PROCEDURE Local(graph: T): BOOLEAN =
  BEGIN
    RETURN graph.isLocal;
  END Local;
  
PROCEDURE BaseName (graph: T): Pathname.T =
  BEGIN
    RETURN graph.db.baseName();
  END BaseName;
  
(****************************************
 node operations
 ****************************************)

PROCEDURE CreateNodeNumber (graph: T; neighbour: Node.T): Node.T
  RAISES {NotOwner, Access.Locked, InternalError} =
  BEGIN
    AssertOwner(graph, neighbour);
    TRY
      RETURN
        Node.T{graph.num, graph.db.createNodeNumber(neighbour.entity)};
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.CreateNodeNumber",
                                       "Database.InternalError", info));
    END;
  END CreateNodeNumber;


PROCEDURE CreateNode (graph: T; label: CARDINAL): Node.T
  RAISES {Access.Locked, InternalError} =
  VAR node: Node.T;
  BEGIN
    TRY
      (* create node *)
      node := Node.T{graph.num, graph.db.createNode(label)};
      (* signal creation *)
      GraphEvents.SignalCreateNode(
        graph.pool.getRuleEngineID(), graph.pool.getBaseName(), graph.pool,
        graph.num, graph, FALSE, graph.pool.getTransactionLevel(), node, label);
      RETURN node;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.CreateNode",
                                       "Database.InternalError", info));
    END;
  END CreateNode;

PROCEDURE OwnsNode (graph: T; node: Node.T): BOOLEAN =
  BEGIN
    RETURN node.graph = graph.num;
  END OwnsNode;

PROCEDURE DeleteNode (    graph            : T;
                          node             : Node.T;
                      VAR attributes       : AttributeDescriptionSet.T;
                      VAR indexes          : TaggedNameSet.T;
                      VAR outEdges, inEdges: NodeLabelRelation.T        )
  RAISES {NotOwner, Access.Locked, InternalError} =
  VAR
    internInEdges : CardRelation.T;
    internOutEdges: CardRelation.T;
    hIn, hOut     : NodeLabelRelation.T;
    extRels       : ExtConnectorStorage.T;
    neighb        : CARDINAL;
    found         : BOOLEAN;
    target, source: Node.T;
    label, nlabel : CARDINAL;
  BEGIN
    AssertOwner(graph, node);
    TRY
      outEdges := NodeLabelRelation.New();
      inEdges := NodeLabelRelation.New();
      (* remove node and all internal relations *)
      nlabel := graph.db.getNodeLabel(node.entity);
      graph.db.deleteNode(
        node.entity, attributes, indexes, internOutEdges, internInEdges);
      CardRelToNodeLabRel(internOutEdges, graph.num, outEdges);
      internOutEdges.dispose();
      CardRelToNodeLabRel(internInEdges, graph.num, inEdges);
      internInEdges.dispose();

      (* remove all external relations *)
      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          extRels.deleteConnectorsByEntity(node, hOut, hIn);
          inEdges.union(hIn);
          outEdges.union(hOut);
          hIn.dispose();
          hOut.dispose();
        END;
      END;

      WITH poolName = graph.pool.getBaseName(),
           unit     = graph.pool.getRuleEngineID(),
           level    = graph.pool.getTransactionLevel() DO
        RuleEngine.DelayActionExecution(unit);
        (* send events for deleted outgoing edges *)
        outEdges.loop();
        outEdges.get(target, label, found);
        WHILE found DO
          GraphEvents.SignalDeleteEdge(
            unit, poolName, graph.pool, graph.num, graph, FALSE, level,
            node, target, label, sourceEx := FALSE, targetEx := TRUE);
          outEdges.get(target, label, found);
        END;

        (* send events for deleted incoming edges *)
        inEdges.loop();
        inEdges.get(source, label, found);
        WHILE found DO
          GraphEvents.SignalDeleteEdge(
            unit, poolName, graph.pool, graph.num, graph, FALSE, level,
            source, node, label, sourceEx := TRUE, targetEx := FALSE);
          inEdges.get(source, label, found);
        END;

        (* signal node deletion *)
        GraphEvents.SignalDeleteNode(unit, poolName, graph.pool, graph.num,
                                    graph, FALSE, level, node, nlabel);        
        PersistentGraphEventHandler.NotifyNodeDeletion(unit, level, node);
        RuleEngine.ReleaseActionExecution(unit);
      END;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.DeleteNode",
                                       "Database.InternalError", info));
    | Database.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.DeleteNode",
                                     "Database.NodeNotFound"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.DeleteNode",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.RelationNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "PersistentGraph.DeleteNode",
                              "ExtConnectorStorage.RelationNotFound"));
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.DeleteNode",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.DeleteNode",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.DeleteNode",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END DeleteNode;

PROCEDURE DeleteNodeNoInfo (graph: T; node: Node.T)
  RAISES {NotOwner, Access.Locked, InternalError} =
  VAR
    attrs            : AttributeDescriptionSet.T;
    indxs            : TaggedNameSet.T;
    inEdges, outEdges: NodeLabelRelation.T;
  BEGIN
    DeleteNode(graph, node, attrs, indxs, outEdges, inEdges);
    attrs.dispose();
    indxs.dispose();
    inEdges.dispose();
    outEdges.dispose();
  END DeleteNodeNoInfo;

PROCEDURE ExistsNode (graph: T; node: Node.T): BOOLEAN
  RAISES {NotOwner, Access.Locked, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      RETURN graph.db.existsNode(node.entity);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.ExistsNode",
                                       "Database.InternalError", info));
    END;
  END ExistsNode;


PROCEDURE PutNodeLabel (graph: T; node: Node.T; label: CARDINAL)
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      graph.db.putNodeLabel(node.entity, label);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.PutNodeLabel",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END PutNodeLabel;


PROCEDURE GetNodeLabel (graph: T; node: Node.T): CARDINAL
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      RETURN graph.db.getNodeLabel(node.entity);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetNodeLabel",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetNodeLabel;


(****************************************
 attribute operations
 ****************************************)

PROCEDURE PutAttribute (graph      : T;
                        node       : Node.T;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        attribute  : TEXT      )
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      graph.db.putAttribute(node.entity, attributeNo, start, attribute);
      GraphEvents.SignalPutAttribute(
        graph.pool.getRuleEngineID(), graph.pool.getBaseName(), graph.pool,
        graph.num, graph, FALSE, graph.pool.getTransactionLevel(), node,
        attributeNo, start, attribute, TRUE);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.PutAttribute",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END PutAttribute;

PROCEDURE DeleteAttribute (graph: T; node: Node.T; attributeNo: CARDINAL)
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      graph.db.deleteAttribute(node.entity, attributeNo);
      GraphEvents.SignalTruncateAttribute(
        graph.pool.getRuleEngineID(), graph.pool.getBaseName(), graph.pool,
        graph.num, graph, FALSE, graph.pool.getTransactionLevel(), node,
        attributeNo, 0, TRUE);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.DeleteAttribute",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END DeleteAttribute;


PROCEDURE GetAttribute (graph      : T;
                        node       : Node.T;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        length     : CARDINAL  ): TEXT
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      RETURN
        graph.db.getAttribute(node.entity, attributeNo, start, length);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetAttribute",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetAttribute;


PROCEDURE TruncateAttribute (graph      : T;
                             node       : Node.T;
                             attributeNo: CARDINAL;
                             size       : CARDINAL  )
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      graph.db.truncateAttribute(node.entity, attributeNo, size);
      GraphEvents.SignalTruncateAttribute(
        graph.pool.getRuleEngineID(), graph.pool.getBaseName(), graph.pool,
        graph.num, graph, FALSE, graph.pool.getTransactionLevel(), node,
        attributeNo, size, TRUE);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.TruncateAttribute",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END TruncateAttribute;


PROCEDURE GetAllAttributeNumbers (graph: T; node: Node.T): CardSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      RETURN graph.db.getAllAttributeNumbers(node.entity);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllAttributeNumbers",
                              "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetAllAttributeNumbers;


(****************************************
 index attribute operations
 ****************************************)

PROCEDURE PutIndex (graph: T; node: Node.T; indexNo: CARDINAL; index: TEXT)
  RAISES {NotOwner, Access.Locked, NodeNotFound, IndexUsed, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      graph.db.putIndex(node.entity, indexNo, index);
      GraphEvents.SignalPutIndex(
        graph.pool.getRuleEngineID(), graph.pool.getBaseName(), graph.pool,
        graph.num, graph, FALSE, graph.pool.getTransactionLevel(), node,
        indexNo, index, TRUE);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.PutIndex",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | Database.IndexUsed => RAISE IndexUsed;
    END;
  END PutIndex;


PROCEDURE DeleteIndex (graph  : T;
                       node   : Node.T;
                       indexNo: CARDINAL;
                       index  : TEXT      ) RAISES {NotOwner,
                                                    Access.Locked,
                                                    NodeNotFound,
                                                    IndexUnused,
                                                    InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      graph.db.deleteIndex(node.entity, indexNo, index);
      GraphEvents.SignalDeleteIndex(
        graph.pool.getRuleEngineID(), graph.pool.getBaseName(), graph.pool,
        graph.num, graph, FALSE, graph.pool.getTransactionLevel(), node,
        indexNo, index, TRUE);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.DeleteIndex",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | Database.IndexUnused => RAISE IndexUnused;
    END;
  END DeleteIndex;


PROCEDURE GetIndex (    graph  : T;
                        node   : Node.T;
                        indexNo: CARDINAL;
                    VAR found  : BOOLEAN   ): TEXT
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      RETURN graph.db.getIndex(node.entity, indexNo, found);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetIndex",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetIndex;


PROCEDURE GetNodesWithIndex (graph: T; indexNo: CARDINAL; index: TEXT):
  NodeSet.T RAISES {Access.Locked, InternalError} =
  VAR
    res               := NodeSet.New();
    intern: CardSet.T;
  BEGIN
    TRY
      intern := graph.db.getNodesWithIndex(indexNo, index);
      CardSetToNodeSet(intern, graph.num, res);
      intern.dispose();
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetNodesWithIndex",
                                       "Database.InternalError", info));
    END;
  END GetNodesWithIndex;


PROCEDURE GetIndexNosForNode (graph: T; node: Node.T): CardSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    AssertOwner(graph, node);
    TRY
      RETURN graph.db.getIndexNosForNode(node.entity);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetIndexNosForNode",
                              "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    END;
  END GetIndexNosForNode;


(****************************************
 edge operations
 ****************************************)

PROCEDURE CreateEdge (graph: T; source, target: Node.T; label: CARDINAL)
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR exSt: ExtConnectorStorage.T;
  BEGIN
    TRY
      IF source.graph # graph.num AND target.graph # graph.num THEN
        RAISE NotOwner;
      END;
      IF source.graph = target.graph THEN
        (* internal edge *)
        graph.db.createEdge(source.entity, target.entity, label);
      ELSE
        IF graph.isLocal THEN RAISE NotOwner END;
        (* external edge *)
        IF source.graph = graph.num THEN
          (* edge to another graph *)
          exSt := graph.pool.externalRelations(graph.num, target.graph, graph.isLocal);
        ELSE
          (* edge from another graph *)
          exSt := graph.pool.externalRelations(graph.num, source.graph, graph.isLocal);
        END;
        exSt.putConnector(source, target, label);
      END;
      GraphEvents.SignalCreateEdge(
        graph.pool.getRuleEngineID(), graph.pool.getBaseName(), graph.pool,
        graph.num, graph, FALSE, graph.pool.getTransactionLevel(), source,
        target, label, TRUE, TRUE);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.CreateEdge",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.CreateEdge",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.CreateEdge",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.CreateEdge",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.CreateEdge",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END CreateEdge;


PROCEDURE DeleteEdge (graph: T; source, target: Node.T; label: CARDINAL)
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR exSt: ExtConnectorStorage.T;
  BEGIN
    TRY
      IF source.graph # graph.num AND target.graph # graph.num THEN
        RAISE NotOwner;
      END;
      IF source.graph = target.graph THEN
        (* internal edge *)
        graph.db.deleteEdge(source.entity, target.entity, label);
      ELSE
        IF graph.isLocal THEN RAISE NotOwner END;
        (* external edge *)
        IF source.graph = graph.num THEN
          (* edge to another graph *)
          exSt := graph.pool.externalRelations(graph.num, target.graph, graph.isLocal);
        ELSE
          (* edge from another graph *)
          exSt := graph.pool.externalRelations(graph.num, source.graph, graph.isLocal);
        END;
        exSt.deleteConnector(source, target, label);
      END;
      GraphEvents.SignalDeleteEdge(
        graph.pool.getRuleEngineID(), graph.pool.getBaseName(), graph.pool,
        graph.num, graph, FALSE, graph.pool.getTransactionLevel(), source,
        target, label, TRUE, TRUE);
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.DeleteEdge",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.CreateEdge",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.CreateEdge",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.RelationNotFound => (* ignore *)
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.CreateEdge",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.CreateEdge",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END DeleteEdge;


PROCEDURE ExistsEdge (graph: T; source, target: Node.T; label: CARDINAL):
  BOOLEAN RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR exSt: ExtConnectorStorage.T;
  BEGIN
    TRY
      IF source.graph # graph.num AND target.graph # graph.num THEN
        RAISE NotOwner;
      END;
      IF source.graph = target.graph THEN
        (* internal edge *)
        RETURN graph.db.existsEdge(source.entity, target.entity, label);
      ELSE
        IF graph.isLocal THEN RAISE NotOwner END;
        (* external edge *)
        IF source.graph = graph.num THEN
          (* edge to another graph *)
          exSt := graph.pool.externalRelations(graph.num, target.graph, graph.isLocal);
        ELSE
          (* edge from another graph *)
          exSt := graph.pool.externalRelations(graph.num, source.graph, graph.isLocal);
        END;
        RETURN exSt.areConnected(source, target, label);
      END;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.ExistsEdge",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.ExistsEdge",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.ExistsEdge",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.ExistsEdge",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.ExistsEdge",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END ExistsEdge;


PROCEDURE GetTargets (graph: T; source: Node.T; label: CARDINAL): NodeSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res    : NodeSet.T;
    intern : CardSet.T;
    extRels: ExtConnectorStorage.T;
    neighb : CARDINAL;
  BEGIN
    TRY
      AssertOwner(graph, source);
      res := NodeSet.New();
      intern := graph.db.getTargets(source.entity, label);
      CardSetToNodeSet(intern, graph.num, res);
      intern.dispose();

      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          WITH d = extRels.getTargets(source, label) DO
            res.union(d);
            d.dispose();
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetTargets",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetTargets",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetTargets",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetTargets",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetTargets",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetTargets;


PROCEDURE GetSources (graph: T; target: Node.T; label: CARDINAL): NodeSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res    : NodeSet.T;
    intern : CardSet.T;
    extRels: ExtConnectorStorage.T;
    neighb : CARDINAL;
  BEGIN
    TRY
      AssertOwner(graph, target);
      res := NodeSet.New();
      intern := graph.db.getSources(target.entity, label);
      CardSetToNodeSet(intern, graph.num, res);
      intern.dispose();

      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          WITH d = extRels.getSources(target, label) DO
            res.union(d);
            d.dispose();
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetSources",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetSources",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetSources",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetSources",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetSources",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetSources;


PROCEDURE GetTargetsTo (graph    : T;
                        source   : Node.T;
                        label    : CARDINAL;
                        neighbour: CARDINAL   := 0): NodeSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res   : NodeSet.T;
    intern: CardSet.T;
  BEGIN
    TRY
      AssertOwner(graph, source);
      IF neighbour = 0 OR neighbour = graph.num THEN
        res := NodeSet.New();
        intern := graph.db.getTargets(source.entity, label);
        CardSetToNodeSet(intern, graph.num, res);
        intern.dispose();
      ELSE
        WITH extRels = graph.pool.externalRelations(graph.num, neighbour, graph.isLocal) DO
          res := extRels.getTargets(source, label);
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetTargets",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetTargets",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetTargets",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetTargets",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetTargets",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetTargetsTo;


PROCEDURE GetSourcesFrom (graph    : T;
                          target   : Node.T;
                          label    : CARDINAL;
                          neighbour: CARDINAL   := 0): NodeSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res   : NodeSet.T;
    intern: CardSet.T;
  BEGIN
    TRY
      AssertOwner(graph, target);
      IF neighbour = 0 OR neighbour = graph.num THEN
        res := NodeSet.New();
        intern := graph.db.getSources(target.entity, label);
        CardSetToNodeSet(intern, graph.num, res);
        intern.dispose();
      ELSE
        WITH extRels = graph.pool.externalRelations(graph.num, neighbour, graph.isLocal) DO
          res := extRels.getSources(target, label);
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetSources",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetSources",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetSources",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetSources",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetSources",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetSourcesFrom;


PROCEDURE GetAllTargets (graph: T; source: Node.T): NodeSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res    : NodeSet.T;
    intern : CardSet.T;
    extRels: ExtConnectorStorage.T;
    neighb : CARDINAL;
  BEGIN
    TRY
      AssertOwner(graph, source);
      res := NodeSet.New();
      intern := graph.db.getAllTargets(source.entity);
      CardSetToNodeSet(intern, graph.num, res);
      intern.dispose();

      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          WITH d = extRels.getAllTargets(source) DO
            res.union(d);
            d.dispose();
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetAllTargets",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllTargets",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllTargets",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllTargets",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllTargets",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetAllTargets;


PROCEDURE GetAllSources (graph: T; target: Node.T): NodeSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res    : NodeSet.T;
    intern : CardSet.T;
    extRels: ExtConnectorStorage.T;
    neighb : CARDINAL;
  BEGIN
    TRY
      AssertOwner(graph, target);
      res := NodeSet.New();
      intern := graph.db.getAllSources(target.entity);
      CardSetToNodeSet(intern, graph.num, res);
      intern.dispose();

      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          WITH d = extRels.getAllSources(target) DO
            res.union(d);
            d.dispose();
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetAllSources",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllSources",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllSources",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllSources",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllSources",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetAllSources;


PROCEDURE GetAllOutLabels (graph: T; source: Node.T): CardSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res    : CardSet.T;
    extRels: ExtConnectorStorage.T;
    neighb : CARDINAL;
  BEGIN
    TRY
      AssertOwner(graph, source);
      res := graph.db.getAllOutLabels(source.entity);

      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          WITH d = extRels.getAllOutTypes(source) DO
            res.union(d);
            d.dispose();
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetAllOutLabels",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllOutLabels",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllOutLabels",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllOutLabels",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllOutLabels",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetAllOutLabels;


PROCEDURE GetAllInLabels (graph: T; target: Node.T): CardSet.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res    : CardSet.T;
    extRels: ExtConnectorStorage.T;
    neighb : CARDINAL;
  BEGIN
    TRY
      AssertOwner(graph, target);
      res := graph.db.getAllInLabels(target.entity);

      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          WITH d = extRels.getAllInTypes(target) DO
            res.union(d);
            d.dispose();
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetAllInLabels",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllInLabels",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllInLabels",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllInLabels",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllInLabels",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetAllInLabels;


PROCEDURE GetAllOutEdges (graph: T; source: Node.T): NodeLabelRelation.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res    : NodeLabelRelation.T;
    intern : CardRelation.T;
    extRels: ExtConnectorStorage.T;
    neighb : CARDINAL;
  BEGIN
    TRY
      AssertOwner(graph, source);
      res := NodeLabelRelation.New();
      intern := graph.db.getAllOutEdges(source.entity);
      CardRelToNodeLabRel(intern, graph.num, res);
      intern.dispose();

      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          WITH d = extRels.getAllOutConnectors(source) DO
            res.union(d);
            d.dispose();
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetAllOutEdges",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllOutEdges",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllOutEdges",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllOutEdges",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllOutEdges",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetAllOutEdges;


PROCEDURE GetAllInEdges (graph: T; target: Node.T): NodeLabelRelation.T
  RAISES {NotOwner, Access.Locked, NodeNotFound, InternalError} =
  VAR
    res    : NodeLabelRelation.T;
    intern : CardRelation.T;
    extRels: ExtConnectorStorage.T;
    neighb : CARDINAL;
  BEGIN
    TRY
      AssertOwner(graph, target);
      res := NodeLabelRelation.New();
      intern := graph.db.getAllInEdges(target.entity);
      CardRelToNodeLabRel(intern, graph.num, res);
      intern.dispose();

      WITH it = graph.pool.iterateNeighbours(graph.num) DO
        WHILE it.get(neighb) DO
          extRels := graph.pool.externalRelations(graph.num, neighb, graph.isLocal);
          WITH d = extRels.getAllInConnectors(target) DO
            res.union(d);
            d.dispose();
          END;
        END;
      END;
      RETURN res;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraph.GetAllInEdges",
                                       "Database.InternalError", info));
    | Database.NodeNotFound => RAISE NodeNotFound;
    | PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllInEdges",
                              "PersistentGraphPool.InternalError", info));
    | PersistentGraphPool.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllInEdges",
                                    "PersistentGraphPool.NotExistent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraph.GetAllInEdges",
                              "ExtConnectorStorage.InternalError", info));
    | ExtConnectorStorage.NotOwner =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraph.GetAllInEdges",
                                    "ExtConnectorStorage.NotOwner"));
    END;
  END GetAllInEdges;


(* internal procedures*)

PROCEDURE CardSetToNodeSet (    cset: CardSet.T;
                                db  : CARDINAL;
                            VAR nset: NodeSet.T  ) =
  VAR
    found: BOOLEAN;
    n    : CARDINAL;
  BEGIN
    cset.loop();
    n := cset.get(found);
    WHILE found DO
      nset.insert(Node.T{graph := db, entity := n});
      n := cset.get(found);
    END;
  END CardSetToNodeSet;

PROCEDURE CardRelToNodeLabRel (    crel: CardRelation.T;
                                   db  : CARDINAL;
                               VAR nrel: NodeLabelRelation.T) =
  VAR
    found: BOOLEAN;
    n, m : CARDINAL;
  BEGIN
    crel.loop();
    crel.get(m, n, found);
    WHILE found DO
      nrel.insert(Node.T{graph := db, entity := n}, m);
      crel.get(m, n, found);
    END;
  END CardRelToNodeLabRel;

PROCEDURE AssertOwner (graph: T; node: Node.T) RAISES {NotOwner} =
  BEGIN
    IF graph.num # node.graph THEN RAISE NotOwner END;
  END AssertOwner;

BEGIN
END PersistentGraph.
