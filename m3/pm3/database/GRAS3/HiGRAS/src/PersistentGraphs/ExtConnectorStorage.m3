MODULE ExtConnectorStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.5  1997/09/18 08:25:41  roland
    Removed unnecessary creation of sets.

    Revision 1.4  1997/07/21 10:42:54  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.3  1997/04/24 14:32:22  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.2  1997/04/23 14:33:44  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:09  roland
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

IMPORT Pathname;
IMPORT Node, NodeSet, Access, ITPFile, NodeLabelRelation, VirtualResource,
       CardSet;
IMPORT PageFile, ErrorSupport, RelationStorage, CardRelation;

REVEAL
  T = Public BRANDED OBJECT
        file    : ITPFile.T;
        db1, db2: CARDINAL;
      OVERRIDES
        open                     := Open;
        close                    := Close;
        putConnector             := PutConnector;
        deleteConnector          := DeleteConnector;
        deleteConnectorsByEntity := DeleteConnectorsByEntity;
        areConnected             := AreConnected;
        getTargets               := GetTargets;
        getSources               := GetSources;
        getAllTargets            := GetAllTargets;
        getAllSources            := GetAllSources;
        getAllOutTypes           := GetAllOutTypes;
        getAllInTypes            := GetAllInTypes;
        getAllOutConnectors      := GetAllOutConnectors;
        getAllInConnectors       := GetAllInConnectors;
      END;

CONST
  Forward_1_2 = 5;               (* source in db1, target in db2,
                                    forward *)
  Backward_1_2 = 3;              (* source in db1, target in db2,
                                    backward *)
  Forward_2_1 = 4;               (* source in db2, target in db1,
                                    forward *)
  Backward_2_1 = 2;              (* source in db2, target in db1,
                                    backward *)

  NoOfStorages = 1;
  IndexTree    = 1;

PROCEDURE Open (storage : T;
                resource: VirtualResource.T;
                name    : Pathname.T;
                access  : Access.Mode;
                new     : BOOLEAN;
                local   : BOOLEAN;
                db1, db2: CARDINAL           ): T
  RAISES {PageFile.NoAccess, InternalError, Access.Locked, Access.Denied} =
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    <* ASSERT db1 # db2 *>
    IF db1 < db2 THEN
      storage.db1 := db1;
      storage.db2 := db2;
    ELSE
      storage.db1 := db2;
      storage.db2 := db1;
    END;
    TRY
      (* open file *)
      storage.file :=
        NEW(ITPFile.T).open(resource, name, access, Access.Kind.Data, new,
                            local, NoOfStorages);
    EXCEPT
      ITPFile.TreeUnknown =>
        RAISE InternalError(ErrorSupport.Create("ExtConnectorStorage.Open",
                                                "ITPFile.TreeUnknown"));
    END;

    (* initialize new storage *)
    IF new THEN
      TRY
        TRY
          VirtualResource.T.beginTransaction(resource);
          RelationStorage.Init(storage.file, IndexTree);
        EXCEPT
        | RelationStorage.InternalError (info) =>
            abort := TRUE;
            RAISE PageFile.NoAccess("RelationStorage.InternalError"
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

    RETURN storage;
  END Open;

PROCEDURE Close (storage: T) RAISES {InternalError} =
  BEGIN
    TRY
      storage.file.close();
    EXCEPT
      ITPFile.InternalError =>
        RAISE
          InternalError(ErrorSupport.Create("ExtConnectorStorage.Close",
                                            "ITPFile.InternalError"));
    END;
  END Close;

PROCEDURE PutConnector (storage: T; source, target: Node.T; type: CARDINAL)
  RAISES {NotOwner, Access.Locked, InternalError} =
  VAR forder, border: CARDINAL;
  BEGIN
    TRY
      Orders(storage, source, target, forder, border);
      RelationStorage.PutRelation(storage.file, IndexTree, forder,
                                  source.entity, type, target.entity);
      RelationStorage.PutRelation(storage.file, IndexTree, border,
                                  target.entity, type, source.entity);
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ExtConnectorStorage.PutConnector",
                              "RelationStorage.InternalError", info));
    END;
  END PutConnector;


PROCEDURE DeleteConnector (storage       : T;
                           source, target: Node.T;
                           type          : CARDINAL)
  RAISES {NotOwner, Access.Locked, RelationNotFound, InternalError} =
  VAR forder, border: CARDINAL;
  BEGIN
    TRY
      Orders(storage, source, target, forder, border);
      RelationStorage.DeleteRelation(storage.file, IndexTree, forder,
                                     source.entity, type, target.entity);
      RelationStorage.DeleteRelation(storage.file, IndexTree, border,
                                     target.entity, type, source.entity);
    EXCEPT
      RelationStorage.RelationNotFound => RAISE RelationNotFound;
    | RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ExtConnectorStorage.DeleteConnector",
                              "RelationStorage.InternalError", info));
    END;
  END DeleteConnector;


PROCEDURE DeleteConnectorsByEntity (storage: T;
                                    node   : Node.T;
                                    VAR forward, backward: NodeLabelRelation.T)
  RAISES {NotOwner, Access.Locked, RelationNotFound, InternalError} =
  VAR forw, backw: CardRelation.T;
  BEGIN
    IF node.graph = storage.db1 THEN
      (* Delete relations with source in db1 *)
      DeleteConnectorsOneDirection(
        storage, node.entity, forw, Forward_1_2, Backward_1_2);
      (* Delete relations with target in db1 *)
      DeleteConnectorsOneDirection(
        storage, node.entity, backw, Backward_2_1, Forward_2_1);
    ELSIF node.graph = storage.db1 THEN
      (* Delete relations with source in db2 *)
      DeleteConnectorsOneDirection(
        storage, node.entity, forw, Forward_2_1, Backward_2_1);
      (* Delete relations with target in db2 *)
      DeleteConnectorsOneDirection(
        storage, node.entity, backw, Backward_1_2, Forward_1_2);
    ELSE
      RAISE NotOwner;
    END;
    forward := NodeLabelRelation.New();
    backward := NodeLabelRelation.New();
    CardRelToNodeLabRel(forw, node.graph, forward);
    CardRelToNodeLabRel(backw, node.graph, backward);
    forw.dispose();
    backw.dispose();
  END DeleteConnectorsByEntity;


PROCEDURE AreConnected (storage: T; source, target: Node.T; type: CARDINAL):
  BOOLEAN RAISES {NotOwner, Access.Locked, InternalError} =
  VAR forder, border: CARDINAL;
  BEGIN
    TRY
      Orders(storage, source, target, forder, border);
      WITH forward = RelationStorage.IsARelation(
                       storage.file, IndexTree, forder, source.entity,
                       type, target.entity),
           backward = RelationStorage.IsARelation(
                        storage.file, IndexTree, border, target.entity,
                        type, source.entity) DO
        IF NOT (forward = backward) THEN
          RAISE InternalError(
                  ErrorSupport.Create("ExtConnectorStorage.AreConnected",
                                      "Half edge detected"))
        END;

        RETURN forward;
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ExtConnectorStorage.AreConnected",
                              "RelationStorage.InternalError", info));
    END;
  END AreConnected;


PROCEDURE GetTargets (storage: T; source: Node.T; type: CARDINAL):
  NodeSet.T RAISES {NotOwner, Access.Locked, InternalError} =
  VAR res := NodeSet.New();
  BEGIN
    TRY
      WITH Forward = ForwardOrder(storage, source.graph),
           set = RelationStorage.GetThirdComponents(
                   storage.file, IndexTree, Forward, source.entity, type) DO
        CardSetToNodeSet(set, OtherDB(storage, source.graph), res);
        set.dispose();
        RETURN res;
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ExtConnectorStorage.GetTargets",
                              "RelationStorage.InternalError", info));
    END;
  END GetTargets;


PROCEDURE GetSources (storage: T; target: Node.T; type: CARDINAL):
  NodeSet.T RAISES {NotOwner, Access.Locked, InternalError} =
  VAR res := NodeSet.New();
  BEGIN
    TRY
      WITH Backward = BackwardOrder(storage, target.graph),
           set = RelationStorage.GetThirdComponents(
                   storage.file, IndexTree, Backward, target.entity, type) DO
        CardSetToNodeSet(set, OtherDB(storage, target.graph), res);
        set.dispose();
        RETURN res;
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ExtConnectorStorage.GetSources",
                              "RelationStorage.InternalError", info));
    END;
  END GetSources;


PROCEDURE GetAllTargets (storage: T; source: Node.T): NodeSet.T
  RAISES {NotOwner, Access.Locked, InternalError} =
  VAR res := NodeSet.New();
  BEGIN
    WITH Forward = ForwardOrder(storage, source.graph),
         set = GetRelationComponents(
                 storage, entity := source.entity, direction := Forward,
                 componentNo := 2) DO
      CardSetToNodeSet(set, OtherDB(storage, source.graph), res);
      set.dispose();
      RETURN res;
    END;
  END GetAllTargets;


PROCEDURE GetAllSources (storage: T; target: Node.T): NodeSet.T
  RAISES {NotOwner, Access.Locked, InternalError} =
  VAR res := NodeSet.New();
  BEGIN
    WITH Backward = BackwardOrder(storage, target.graph),
         set = GetRelationComponents(
                 storage, entity := target.entity, direction := Backward,
                 componentNo := 2) DO
      CardSetToNodeSet(set, OtherDB(storage, target.graph), res);
      set.dispose();
      RETURN res;
    END;
  END GetAllSources;


PROCEDURE GetAllOutTypes (storage: T; source: Node.T): CardSet.T
  RAISES {NotOwner, Access.Locked, InternalError} =
  BEGIN
    WITH Forward = ForwardOrder(storage, source.graph) DO
      RETURN GetRelationComponents(storage, entity := source.entity,
                                   direction := Forward, componentNo := 1);
    END;
  END GetAllOutTypes;


PROCEDURE GetAllInTypes (storage: T; target: Node.T): CardSet.T
  RAISES {NotOwner, Access.Locked, InternalError} =
  BEGIN
    WITH Backward = BackwardOrder(storage, target.graph) DO
      RETURN
        GetRelationComponents(storage, entity := target.entity,
                              direction := Backward, componentNo := 1);
    END;
  END GetAllInTypes;


PROCEDURE GetAllOutConnectors (storage: T; source: Node.T):
  NodeLabelRelation.T RAISES {NotOwner, Access.Locked, InternalError} =
  VAR
    range: RelationStorage.RelationRange;
    crel : CardRelation.T;
    res  : NodeLabelRelation.T;
  BEGIN
    res := NodeLabelRelation.New();
    WITH Forward = ForwardOrder(storage, source.graph) DO
      TRY
        range := RelationStorage.InitRelationRange(
                   storage.file, IndexTree, Forward, source.entity);
        crel := RelationStorage.GetRelationRange(
                  storage.file, IndexTree, range, permute := FALSE);
        CardRelToNodeLabRel(crel, OtherDB(storage, source.graph), res);
        crel.dispose();
        RETURN res;
      EXCEPT
        RelationStorage.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "ExtConnectorStorage.GetAllOutConnectors",
                                "RelationStorage.InternalError", info));
      END;
    END;
  END GetAllOutConnectors;


PROCEDURE GetAllInConnectors (storage: T; target: Node.T):
  NodeLabelRelation.T RAISES {NotOwner, Access.Locked, InternalError} =
  VAR
    range: RelationStorage.RelationRange;
    crel : CardRelation.T;
    res  : NodeLabelRelation.T;
  BEGIN

    res := NodeLabelRelation.New();
    WITH Backward = BackwardOrder(storage, target.graph) DO
      TRY
        range := RelationStorage.InitRelationRange(
                   storage.file, IndexTree, Backward, target.entity);
        crel := RelationStorage.GetRelationRange(
                  storage.file, IndexTree, range, permute := FALSE);
        CardRelToNodeLabRel(crel, OtherDB(storage, target.graph), res);
        crel.dispose();
        RETURN res;
      EXCEPT
        RelationStorage.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "ExtConnectorStorage.GetAllInConnectors",
                                "RelationStorage.InternalError", info));
      END;
    END;
  END GetAllInConnectors;


(* Loop over the RelationRange defined by entity and direction and retrieve
   the first or second component according to componentNo. *)
PROCEDURE GetRelationComponents (storage    : T;
                                 entity     : CARDINAL;
                                 direction  : CARDINAL;
                                 componentNo: [1 .. 2]  ): CardSet.T
  RAISES {Access.Locked, InternalError} =
  VAR
    range    : RelationStorage.RelationRange;
    result   : CardSet.T;
    component: ARRAY [1 .. 2] OF CARDINAL;
    found    : BOOLEAN;
  BEGIN
    result := CardSet.New();
    TRY
      range := RelationStorage.InitRelationRange(
                 storage.file, IndexTree, direction, entity);
      RelationStorage.GetFromRelationRange(
        storage.file, IndexTree, range, found, component[1], component[2]);

      WHILE (found) DO
        result.insert(component[componentNo]);
        RelationStorage.GetFromRelationRange(storage.file, IndexTree,
                                             range, found, component[1],
                                             component[2]);
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ExtConnectorStorage.GetRelationComponents",
                              "RelationStorage.InternalError", info));
    END;

    RETURN result;
  END GetRelationComponents;


(* Delete the connectors connected to entity in the given direction. *)
PROCEDURE DeleteConnectorsOneDirection (    storage  : T;
                                            entity   : CARDINAL;
                                        VAR deleted  : CardRelation.T;
                                            direction: CARDINAL;
                                        counterDirection: CARDINAL)
  RAISES {Access.Locked, RelationNotFound, InternalError} =
  VAR
    type  : CARDINAL;
    target: CARDINAL;
    found : BOOLEAN;
  BEGIN
    TRY
      (* delete all relations in direction *)
      RelationStorage.DeleteRelationsByEntity(
        storage.file, IndexTree, direction, entity, deleted);

      (* now delete their counterparts in the other direction *)
      deleted.loop();
      deleted.get(type, target, found);
      WHILE (found) DO
        RelationStorage.DeleteRelation(
          storage.file, IndexTree, counterDirection, target, type, entity);
        deleted.get(type, target, found);
      END;
    EXCEPT
      RelationStorage.RelationNotFound => RAISE RelationNotFound;
    | RelationStorage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ExtConnectorStorage.DeleteConnectorsOneDirection",
                  "RelationStorage.InternalError", info));
    END;
  END DeleteConnectorsOneDirection;

PROCEDURE Orders (    storage       : T;
                      source, target: Node.T;
                  VAR forw, backw   : CARDINAL) RAISES {NotOwner} =
  BEGIN
    IF source.graph = storage.db1 AND target.graph = storage.db2 THEN
      forw := Forward_1_2;
      backw := Backward_1_2;
    ELSIF source.graph = storage.db2 AND target.graph = storage.db1 THEN
      forw := Forward_2_1;
      backw := Backward_2_1;
    ELSE
      RAISE NotOwner;
    END;
  END Orders;

PROCEDURE ForwardOrder (st: T; db: CARDINAL): CARDINAL RAISES {NotOwner} =
  BEGIN
    IF db = st.db1 THEN
      RETURN Forward_1_2;
    ELSIF db = st.db2 THEN
      RETURN Forward_2_1;
    ELSE
      RAISE NotOwner;
    END;
  END ForwardOrder;

PROCEDURE BackwardOrder (st: T; db: CARDINAL): CARDINAL RAISES {NotOwner} =
  (* Return the order for edges with target in db *)
  BEGIN
    IF db = st.db1 THEN
      RETURN Backward_2_1;
    ELSIF db = st.db2 THEN
      RETURN Backward_1_2;
    ELSE
      RAISE NotOwner;
    END;
  END BackwardOrder;

PROCEDURE OtherDB (storage: T; db: CARDINAL): CARDINAL =
  BEGIN
    IF db = storage.db1 THEN
      RETURN storage.db2;
    ELSE
      RETURN storage.db1;
    END;
  END OtherDB;

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

BEGIN
END ExtConnectorStorage.
