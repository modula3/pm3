MODULE Database;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.3  1998/01/21 12:35:41  roland
    New method baseName to determine filename.

    Revision 1.2  1997/04/24 14:32:16  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/03/26 11:39:03  roland
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

*)
(***************************************************************************)

IMPORT Pathname, PageFile, Access, ITPFile, EntityStorage, NameStorage,
       RelationStorage, AttributeStorage, LongAttributeStorage,
       MultiNameStorage, ConnectorStorage, CardSet, TaggedNameSet,
       AttributeDescriptionSet, CardRelation, LongAttributeCache;
IMPORT ErrorSupport, VirtualResource;

REVEAL
  T = Public BRANDED OBJECT
        file          : ITPFile.T;
        errorChecks   : BOOLEAN;
        attributeCache: LongAttributeCache.T;
      OVERRIDES
        open  := Open;
        close := Close;

        errorCheckOn := ErrorCheckOn;
        baseName     := BaseName;

        createNodeNumber := CreateNodeNumber;
        createNode       := CreateNode;
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
        getAllTargets   := GetAllTargets;
        getAllSources   := GetAllSources;
        getAllOutLabels := GetAllOutLabels;
        getAllInLabels  := GetAllInLabels;
        getAllOutEdges  := GetAllOutEdges;
        getAllInEdges   := GetAllInEdges;
      END;


(****************************************
 graph operations
 ****************************************)

CONST NoOfStorages = 4;

PROCEDURE Open (graph      : T;
                resource   : VirtualResource.T;
                name       : Pathname.T;
                mode       : Access.Mode;
                new        : BOOLEAN;
                local      : BOOLEAN;
                errorChecks: BOOLEAN            ): T
  RAISES {Access.Denied, PageFile.NoAccess, Access.Locked} =
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    graph.errorChecks := errorChecks;

    TRY
      (* open database file *)
      graph.file := NEW(ITPFile.T).open(resource, name, mode, Access.Kind.Data,
                                        new, local, NoOfStorages);
    EXCEPT
      ITPFile.TreeUnknown =>
        RAISE PageFile.NoAccess("Database.Open: Bad ITPFile.");
    END;

    (* initialize new graph database *)
    IF new THEN
      TRY
        TRY
          VirtualResource.T.beginTransaction(resource);
          EntityStorage.Init(graph.file);
          NameStorage.Init(graph.file);
          RelationStorage.Init(graph.file, ConnectorStorage.IndexTree);
          AttributeStorage.Init(graph.file);
        EXCEPT
          NameStorage.InternalError (info) =>
            abort := TRUE;
            RAISE PageFile.NoAccess("NameStorage.InternalError"
                                      & ErrorSupport.ToText(info));
        | EntityStorage.InternalError (info) =>
            abort := TRUE;
            RAISE PageFile.NoAccess("EntityStorage.InternalError"
                                      & ErrorSupport.ToText(info));
        | RelationStorage.InternalError (info) =>
            abort := TRUE;
            RAISE PageFile.NoAccess("RelationStorage.InternalError"
                                      & ErrorSupport.ToText(info));
        | AttributeStorage.InternalError (info) =>
            abort := TRUE;
            RAISE PageFile.NoAccess("AttributeStorage.InternalError"
                                      & ErrorSupport.ToText(info));
        | VirtualResource.FatalError (info) =>
            abort := TRUE;
            RAISE PageFile.NoAccess("VirtualResource.FatalError"
                                      & ErrorSupport.ToText(info));
        END;
      FINALLY
        TRY
          IF abort THEN
            VirtualResource.T.abortTransaction(resource);
          ELSE
            VirtualResource.T.commitTransaction(resource);
          END;
        EXCEPT
          VirtualResource.FatalError,
              VirtualResource.NotInTransaction => (* ignore *)
        END;
      END;
    END;
    (* initialize attribute cache *)
    graph.attributeCache :=
      NEW(LongAttributeCache.T).init(resource);

    RETURN graph;
  END Open;


PROCEDURE Close (graph: T) RAISES {InternalError} =
  BEGIN
    TRY
      IF graph.attributeCache # NIL THEN graph.attributeCache.close() END;
      IF graph.file # NIL THEN graph.file.close(); END;
    EXCEPT
      ITPFile.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Database.Close", "ITCFile.InternalError", info));
    END;
  END Close;


PROCEDURE ErrorCheckOn (graph: T): BOOLEAN =
  BEGIN
    RETURN graph.errorChecks;
  END ErrorCheckOn;

PROCEDURE BaseName(graph: T): Pathname.T =
  BEGIN
    RETURN graph.file.baseName();
  END BaseName;

(****************************************
 node operations
 ****************************************)

PROCEDURE CreateNodeNumber (graph: T; neighbour: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN EntityStorage.CreateEntityNumber(graph.file, neighbour);
    EXCEPT
      EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.CreateNodeNumber",
                              "EntityStorage.InternalError", info));
    END;
  END CreateNodeNumber;


PROCEDURE CreateNode (<* UNUSED *> graph: T; label: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN EntityStorage.CreateEntity(label);
    EXCEPT
      EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.CreateNode",
                              "EntityStorage.InternalError", info));
    END;
  END CreateNode;


PROCEDURE DeleteNode (    graph            : T;
                          node             : CARDINAL;
                      VAR attributes       : AttributeDescriptionSet.T;
                      VAR indexes          : TaggedNameSet.T;
                      VAR outEdges, inEdges: CardRelation.T                 )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      ConnectorStorage.DeleteConnectorsByEntity(
        graph.file, node, outEdges, inEdges);
      MultiNameStorage.DeleteAllNamesForEntity(graph.file, node, indexes);
      graph.attributeCache.removeEntity(node);
      attributes :=
        LongAttributeStorage.GetAllAttributes(graph.file, node);
      AttributeStorage.DeleteAllAttributes(graph.file, node);
      EntityStorage.DeleteEntity(graph.file, node);
    EXCEPT
      MultiNameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.DeleteNode",
                              "MultiNameStorage.InternalError", info));
    | ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.DeleteNode",
                              "ConnectorStorage.InternalError", info));
    | LongAttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.DeleteNode",
                              "LongAttributeStorage.InternalError", info));
    | AttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.DeleteNode",
                              "AttributeStorage.InternalError", info));
    | EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.DeleteNode",
                              "EntityStorage.InternalError", info));
    | EntityStorage.EntityNotFound, ConnectorStorage.RelationNotFound,
          LongAttributeStorage.EntityNotFound => (* ignore *)
    END;
  END DeleteNode;

PROCEDURE DeleteNodeNoInfo (graph: T; node: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR
    atr   : AttributeDescriptionSet.T;
    idx   : TaggedNameSet.T;
    ie, oe: CardRelation.T;
  BEGIN
    DeleteNode(graph, node, atr, idx, oe, ie);
  END DeleteNodeNoInfo;

PROCEDURE ExistsNode (graph: T; node: CARDINAL): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN EntityStorage.ExistsEntity(graph.file, node);
    EXCEPT
      EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.ExistsNode",
                              "EntityStorage.InternalError", info));
    END;
  END ExistsNode;


PROCEDURE PutNodeLabel (graph: T; node: CARDINAL; label: CARDINAL)
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    TRY
      EntityStorage.PutLabel(graph.file, node, label);
    EXCEPT
      EntityStorage.EntityNotFound => RAISE NodeNotFound;
    | EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.PutNodeLabel",
                              "EntityStorage.InternalError", info));
    END;
  END PutNodeLabel;


PROCEDURE GetNodeLabel (graph: T; node: CARDINAL): CARDINAL
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  VAR res: CARDINAL;
  BEGIN
    TRY
      res := EntityStorage.GetLabel(graph.file, node);
    EXCEPT
      EntityStorage.EntityNotFound => RAISE NodeNotFound;
    | EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetNodeLabel",
                              "EntityStorage.InternalError", info));
    END;
    RETURN res;
  END GetNodeLabel;


(****************************************
 attribute operations
 ****************************************)

PROCEDURE PutAttribute (graph      : T;
                        node       : CARDINAL;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        attribute  : TEXT      )
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, node);
    TRY
      IF graph.attributeCache.has(node, attributeNo) THEN
        graph.attributeCache.put(node, attributeNo, start, attribute);
      END;
      LongAttributeStorage.PutAttribute(
        graph.file, node, attributeNo, start, attribute);
    EXCEPT
      LongAttributeStorage.EntityNotFound =>
        RAISE
          InternalError(
            ErrorSupport.Create("Database.PutAttribute",
                                "LongAttributeStorage.EntityNotFound"));
    | LongAttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.PutAttribute",
                              "LongAttributeStorage.InternalError", info));
    END;
  END PutAttribute;

PROCEDURE DeleteAttribute (graph: T; node: CARDINAL; attributeNo: CARDINAL)
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, node);
    TRY
      IF graph.attributeCache.has(node, attributeNo) THEN
        graph.attributeCache.delete(node, attributeNo);
      END;
      LongAttributeStorage.DeleteAttribute(
        graph.file, node, attributeNo);
    EXCEPT
      LongAttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.DeleteAttribute",
                              "LongAttributeStorage.InternalError", info));
    END;
  END DeleteAttribute;


PROCEDURE GetAttribute (graph      : T;
                        node       : CARDINAL;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        length     : CARDINAL  ): TEXT
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  VAR
    res, all: TEXT;
    found   : BOOLEAN;
  BEGIN
    CheckNode(graph, node);
    TRY
      res :=
        graph.attributeCache.get(node, attributeNo, start, length, found);
      IF NOT found THEN
        all := LongAttributeStorage.GetAttribute(
                 graph.file, node, attributeNo, 0, LAST(CARDINAL));
        graph.attributeCache.store(node, attributeNo, all);
        res := graph.attributeCache.get(
                 node, attributeNo, start, length, found);
      END;
    EXCEPT
      LongAttributeStorage.EntityNotFound =>
        RAISE
          InternalError(
            ErrorSupport.Create("Database.GetAttribute",
                                "LongAttributeStorage.EntityNotFound"));
    | LongAttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetAttribute",
                              "LongAttributeStorage.InternalError", info));
    END;
    RETURN res;
  END GetAttribute;


PROCEDURE TruncateAttribute (graph      : T;
                             node       : CARDINAL;
                             attributeNo: CARDINAL;
                             size       : CARDINAL  )
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, node);
    TRY
      IF graph.attributeCache.has(node, attributeNo) THEN
        graph.attributeCache.truncate(node, attributeNo, size);
      END;
      LongAttributeStorage.TruncateAttribute(
        graph.file, node, attributeNo, size);
    EXCEPT
      LongAttributeStorage.EntityNotFound =>
        RAISE
          InternalError(
            ErrorSupport.Create("Database.TruncateAttribute",
                                "LongAttributeStorage.EntityNotFound"));
    | LongAttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.TruncateAttribute",
                              "LongAttributeStorage.InternalError", info));
    END;
  END TruncateAttribute;


PROCEDURE GetAllAttributeNumbers (graph: T; node: CARDINAL): CardSet.T
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  VAR res: CardSet.T;
  BEGIN
    CheckNode(graph, node);
    TRY
      res := LongAttributeStorage.GetAllAttributeNumbers(graph.file, node);
    EXCEPT
      LongAttributeStorage.EntityNotFound =>
        RAISE
          InternalError(
            ErrorSupport.Create("Database.GetAllAttributeNumbers",
                                "LongAttributeStorage.EntityNotFound"));
    | LongAttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetAllAttributeNumbers",
                              "LongAttributeStorage.InternalError", info));
    END;
    RETURN res;
  END GetAllAttributeNumbers;


(****************************************
 index attribute operations
 ****************************************)

PROCEDURE PutIndex (graph  : T;
                    node   : CARDINAL;
                    indexNo: CARDINAL;
                    index  : TEXT      )
  RAISES {Access.Locked, NodeNotFound, IndexUsed, InternalError} =
  BEGIN
    CheckNode(graph, node);
    TRY
      MultiNameStorage.PutName(graph.file, node, indexNo, index);
    EXCEPT
      MultiNameStorage.TagUsed => RAISE IndexUsed;
    | MultiNameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.PutIndex",
                              "MultiNameStorage.InternalError", info));
    END;
  END PutIndex;


PROCEDURE DeleteIndex (graph  : T;
                       node   : CARDINAL;
                       indexNo: CARDINAL;
                       index  : TEXT      )
  RAISES {Access.Locked, NodeNotFound, IndexUnused, InternalError} =
  BEGIN
    CheckNode(graph, node);
    TRY
      MultiNameStorage.DeleteName(graph.file, node, indexNo, index);
    EXCEPT
      MultiNameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.DeleteIndex",
                              "MultiNameStorage.InternalError", info));
    | MultiNameStorage.NameNotFound => RAISE IndexUnused;
    END;
  END DeleteIndex;


PROCEDURE GetIndex (    graph  : T;
                        node   : CARDINAL;
                        indexNo: CARDINAL;
                    VAR found  : BOOLEAN   ): TEXT
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, node);
    TRY
      RETURN MultiNameStorage.GetName(graph.file, node, indexNo, found);
    EXCEPT
      MultiNameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetIndex",
                              "MultiNameStorage.InternalError", info));
    END;
  END GetIndex;


PROCEDURE GetNodesWithIndex (graph: T; indexNo: CARDINAL; index: TEXT):
  CardSet.T RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN
        MultiNameStorage.FindEntitiesForName(graph.file, indexNo, index);
    EXCEPT
      MultiNameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetNodesWithIndex",
                              "MultiNameStorage.InternalError", info));
    END;
  END GetNodesWithIndex;


PROCEDURE GetIndexNosForNode (graph: T; node: CARDINAL): CardSet.T
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, node);
    TRY
      RETURN MultiNameStorage.FindTagsForEntity(graph.file, node);
    EXCEPT
      MultiNameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetIndexNosForNode",
                              "MultiNameStorage.InternalError", info));
    END;
  END GetIndexNosForNode;


(****************************************
 edge operations
 ****************************************)

PROCEDURE CreateEdge (graph: T; source, target: CARDINAL; label: CARDINAL)
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, source, target);
    TRY
      ConnectorStorage.PutConnector(graph.file, source, target, label);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.CreateEdge",
                              "ConnectorStorage.InternalError", info));
    END;
  END CreateEdge;


PROCEDURE DeleteEdge (graph: T; source, target: CARDINAL; label: CARDINAL)
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, source, target);
    TRY
      ConnectorStorage.DeleteConnector(graph.file, source, target, label);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.DeleteEdge",
                              "ConnectorStorage.InternalError", info));
    | ConnectorStorage.RelationNotFound => (* ignore *)
    END;
  END DeleteEdge;


PROCEDURE ExistsEdge (graph: T; source, target: CARDINAL; label: CARDINAL):
  BOOLEAN RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, source, target);
    TRY
      RETURN
        ConnectorStorage.AreConnected(graph.file, source, target, label);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.ExistsEdge",
                              "ConnectorStorage.InternalError", info));
    END;
  END ExistsEdge;


PROCEDURE GetTargets (graph: T; source: CARDINAL; label: CARDINAL):
  CardSet.T RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, source);
    TRY
      RETURN ConnectorStorage.GetTargets(graph.file, source, label);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetTargets",
                              "ConnectorStorage.InternalError", info));
    END;
  END GetTargets;


PROCEDURE GetSources (graph: T; target: CARDINAL; label: CARDINAL):
  CardSet.T RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, target);
    TRY
      RETURN ConnectorStorage.GetSources(graph.file, target, label);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetSources",
                              "ConnectorStorage.InternalError", info));
    END;
  END GetSources;


PROCEDURE GetAllTargets (graph: T; source: CARDINAL): CardSet.T
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, source);
    TRY
      RETURN ConnectorStorage.GetAllTargets(graph.file, source);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetAllTargets",
                              "ConnectorStorage.InternalError", info));
    END;
  END GetAllTargets;


PROCEDURE GetAllSources (graph: T; target: CARDINAL): CardSet.T
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, target);
    TRY
      RETURN ConnectorStorage.GetAllSources(graph.file, target);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetAllSources",
                              "ConnectorStorage.InternalError", info));
    END;
  END GetAllSources;


PROCEDURE GetAllOutLabels (graph: T; source: CARDINAL): CardSet.T
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, source);
    TRY
      RETURN ConnectorStorage.GetAllOutTypes(graph.file, source);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetAllOutLabels",
                              "ConnectorStorage.InternalError", info));
    END;
  END GetAllOutLabels;


PROCEDURE GetAllInLabels (graph: T; target: CARDINAL): CardSet.T
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, target);
    TRY
      RETURN ConnectorStorage.GetAllInTypes(graph.file, target);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetAllInLabels",
                              "ConnectorStorage.InternalError", info));
    END;
  END GetAllInLabels;


PROCEDURE GetAllOutEdges (graph: T; source: CARDINAL): CardRelation.T
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, source);
    TRY
      RETURN ConnectorStorage.GetAllOutConnectors(graph.file, source);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetAllOutEdges",
                              "ConnectorStorage.InternalError", info));
    END;
  END GetAllOutEdges;


PROCEDURE GetAllInEdges (graph: T; target: CARDINAL): CardRelation.T
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    CheckNode(graph, target);
    TRY
      RETURN ConnectorStorage.GetAllInConnectors(graph.file, target);
    EXCEPT
      ConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Database.GetAllInEdges",
                              "ConnectorStorage.InternalError", info));
    END;
  END GetAllInEdges;


(* Check for node existance if necessary. *)
PROCEDURE CheckNode (graph: T; node1, node2 := -1)
  RAISES {Access.Locked, NodeNotFound, InternalError} =
  BEGIN
    IF (graph.errorChecks) THEN
      TRY
        IF (node1 >= 0) THEN
          IF (NOT EntityStorage.ExistsEntity(graph.file, node1)) THEN
            RAISE NodeNotFound;
          END;
        END;
        IF (node2 >= 0) THEN
          IF (NOT EntityStorage.ExistsEntity(graph.file, node2)) THEN
            RAISE NodeNotFound;
          END;
        END;
      EXCEPT
        EntityStorage.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "Database.CheckNode",
                                "EntityStorage.InternalError", info));
      END;
    END;
  END CheckNode;


BEGIN
END Database.
