MODULE ConnectorStorage;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1997/07/21 10:42:52  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1  1997/03/26 11:39:00  roland
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

    Revision 1.4  1996/11/20 12:22:51  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/17 13:09:00  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.2  1996/08/06 16:26:25  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/07/24 09:19:54  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.1.2.1  1996/04/29 13:43:33  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.1  1994/01/20  18:41:19  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT CardSet, Access, ITPFile, RelationStorage, CardRelation;
IMPORT ErrorSupport;

CONST
  Forward   = 5;
  Backward  = 3;


PROCEDURE PutConnector (file          : ITPFile.T;
                        source, target: CARDINAL;
                        type          : CARDINAL   )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RelationStorage.PutRelation(
        file, IndexTree, Forward, source, type, target);
      RelationStorage.PutRelation(
        file, IndexTree, Backward, target, type, source);
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ConnectorStorage.PutConnector",
                              "RelationStorage.InternalError", info));
    END;
  END PutConnector;


PROCEDURE DeleteConnector (file          : ITPFile.T;
                           source, target: CARDINAL;
                           type          : CARDINAL   )
  RAISES {Access.Locked, RelationNotFound, InternalError} =
  BEGIN
    TRY
      RelationStorage.DeleteRelation(
        file, IndexTree, Forward, source, type, target);
      RelationStorage.DeleteRelation(
        file, IndexTree, Backward, target, type, source);
    EXCEPT
      RelationStorage.RelationNotFound => RAISE RelationNotFound;
    | RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ConnectorStorage.DeleteConnector",
                              "RelationStorage.InternalError", info));
    END;
  END DeleteConnector;


PROCEDURE DeleteConnectorsByEntity (    file             : ITPFile.T;
                                        entity           : CARDINAL;
                                    VAR forward, backward: CardRelation.T )
  RAISES {Access.Locked, RelationNotFound, InternalError} =
  BEGIN
    DeleteConnectorsOneDirection(file, entity, forward, Forward, Backward);
    DeleteConnectorsOneDirection(file, entity, backward, Backward, Forward);
  END DeleteConnectorsByEntity;


PROCEDURE AreConnected (file          : ITPFile.T;
                        source, target: CARDINAL;
                        type          : CARDINAL   ): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH forward = RelationStorage.IsARelation(
                       file, IndexTree, Forward, source, type, target),
           backward = RelationStorage.IsARelation(
                        file, IndexTree, Backward, target, type, source) DO
        IF NOT (forward = backward) THEN
          RAISE InternalError(
                  ErrorSupport.Create(
                    "ConnectorStorage.AreConnected", "Half edge detected"))
        END;

        RETURN forward;
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ConnectorStorage.AreConnected",
                              "RelationStorage.InternalError", info));
    END;
  END AreConnected;


PROCEDURE GetTargets (file: ITPFile.T; source: CARDINAL; type: CARDINAL):
  CardSet.T RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN RelationStorage.GetThirdComponents(
               file, IndexTree, Forward, source, type);
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ConnectorStorage.GetTargets",
                              "RelationStorage.InternalError", info));
    END;
  END GetTargets;


PROCEDURE GetSources (file: ITPFile.T; target: CARDINAL; type: CARDINAL):
  CardSet.T RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN RelationStorage.GetThirdComponents(
               file, IndexTree, Backward, target, type);
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ConnectorStorage.GetSources",
                              "RelationStorage.InternalError", info));
    END;
  END GetSources;


PROCEDURE GetAllTargets (file: ITPFile.T; source: CARDINAL): CardSet.T
  RAISES {Access.Locked, InternalError} =
  BEGIN
    RETURN GetRelationComponents(file, entity := source,
                                 direction := Forward, componentNo := 2);
  END GetAllTargets;


PROCEDURE GetAllSources (file: ITPFile.T; target: CARDINAL): CardSet.T
  RAISES {Access.Locked, InternalError} =
  BEGIN
    RETURN GetRelationComponents(file, entity := target,
                                 direction := Backward, componentNo := 2);
  END GetAllSources;


PROCEDURE GetAllOutTypes (file: ITPFile.T; source: CARDINAL): CardSet.T
  RAISES {Access.Locked, InternalError} =
  BEGIN
    RETURN GetRelationComponents(file, entity := source,
                                 direction := Forward, componentNo := 1);
  END GetAllOutTypes;


PROCEDURE GetAllInTypes (file: ITPFile.T; target: CARDINAL): CardSet.T
  RAISES {Access.Locked, InternalError} =
  BEGIN
    RETURN GetRelationComponents(file, entity := target,
                                 direction := Backward, componentNo := 1);
  END GetAllInTypes;


PROCEDURE GetAllOutConnectors (file: ITPFile.T; source: CARDINAL):
  CardRelation.T RAISES {Access.Locked, InternalError} =
  VAR range: RelationStorage.RelationRange;
  BEGIN
    TRY
      range := RelationStorage.InitRelationRange(
                 file, IndexTree, Forward, source);
      RETURN RelationStorage.GetRelationRange(
               file, IndexTree, range, permute := FALSE);
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ConnectorStorage.GetAllOutConnectors",
                              "RelationStorage.InternalError", info));
    END;
  END GetAllOutConnectors;


PROCEDURE GetAllInConnectors (file: ITPFile.T; target: CARDINAL):
  CardRelation.T RAISES {Access.Locked, InternalError} =
  VAR range: RelationStorage.RelationRange;
  BEGIN

    TRY
      range := RelationStorage.InitRelationRange(
                 file, IndexTree, Backward, target);
      RETURN RelationStorage.GetRelationRange(
               file, IndexTree, range, permute := FALSE);
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ConnectorStorage.GetAllInConnectors",
                              "RelationStorage.InternalError", info));
    END;
  END GetAllInConnectors;


(* Loop over the RelationRange defined by entity and direction and retrieve
   the first or second component according to componentNo. *)
PROCEDURE GetRelationComponents (file       : ITPFile.T;
                                 entity     : CARDINAL;
                                 direction  : CARDINAL;
                                 componentNo: [1 .. 2]   ): CardSet.T
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
                 file, IndexTree, direction, entity);
      RelationStorage.GetFromRelationRange(
        file, IndexTree, range, found, component[1], component[2]);

      WHILE (found) DO
        result.insert(component[componentNo]);
        RelationStorage.GetFromRelationRange(
          file, IndexTree, range, found, component[1], component[2]);
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ConnectorStorage.GetRelationComponents",
                              "RelationStorage.InternalError", info));
    END;

    RETURN result;
  END GetRelationComponents;


(* Delete the connectors connected to entity in the given direction. *)
PROCEDURE DeleteConnectorsOneDirection (    file            : ITPFile.T;
                                            entity          : CARDINAL;
                                        VAR deleted         : CardRelation.T;
                                            direction       : CARDINAL;
                                            counterDirection: CARDINAL    )
  RAISES {Access.Locked, RelationNotFound, InternalError} =
  VAR
    type  : CARDINAL;
    target: CARDINAL;
    found : BOOLEAN;
  BEGIN
    TRY
      (* delete all relations in direction *)
      RelationStorage.DeleteRelationsByEntity(
        file, IndexTree, direction, entity, deleted);

      (* now delete their counterparts in the other direction *)
      deleted.loop();
      deleted.get(type, target, found);
      WHILE (found) DO
        RelationStorage.DeleteRelation(
          file, IndexTree, counterDirection, target, type, entity);
        deleted.get(type, target, found);
      END;
    EXCEPT
      RelationStorage.RelationNotFound => RAISE RelationNotFound;
    | RelationStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "ConnectorStorage.DeleteConnectorsOneDirection",
                          "RelationStorage.InternalError", info));
    END;
  END DeleteConnectorsOneDirection;

BEGIN
END ConnectorStorage.
