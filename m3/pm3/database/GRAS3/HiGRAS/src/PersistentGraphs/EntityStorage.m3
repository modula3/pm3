MODULE EntityStorage;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.3  1998/03/17 14:14:13  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.2  1997/11/21 15:20:46  roland
    When 0 is given as neighbour, CreateEntityNumber selects an arbitrary
    neighbour derived from previously created numbers instead of always
    using LAST(Type.Short)+1 to avoid unballancing.

    Revision 1.1  1997/03/26 11:39:06  roland
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

    Revision 1.4  1996/11/20 12:22:54  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/08/06 16:26:27  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/07/24 09:19:57  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.2.2.1  1996/04/29 13:43:36  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.2  1994/04/01  14:50:40  pk
# CreateEntity/Node returns the node number.
#
# Revision 1.1  1994/01/20  18:41:23  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Access, ITPFile, EntityRecordParameter, EntityRecordStorage;
IMPORT ErrorSupport;

CONST IndexTree = 1;
VAR
  (* data for newly created number *)
  createKey1    : EntityRecordParameter.Key1;
  createKey2    : EntityRecordParameter.Key2;
  createLocation: EntityRecordStorage.RecordLocation;
  LastNeighbour : CARDINAL := 0;

PROCEDURE Init (file: ITPFile.T) RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      EntityRecordStorage.Init(file, IndexTree);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.Init",
                              "EntityRecordStorage.InternalError", info));
    | EntityRecordStorage.LevelError =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "EntityStorage.Init", "EntityRecordStorage.LevelError"));
    END;
  END Init;


PROCEDURE CreateEntityNumber (file: ITPFile.T; neighbour: CARDINAL):
  CARDINAL RAISES {Access.Locked, InternalError} =
  BEGIN
    IF (neighbour = 0) THEN
      (* neighbour := Type.MaxShort + 1; *)
      INC(LastNeighbour);
      neighbour := LastNeighbour;
    END;
    createKey1 := neighbour;
    createKey2 := 0;

    TRY
      createLocation := EntityRecordStorage.LocateNeighbourPosition(
                          file, IndexTree, createKey1, createKey2);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.CreateEntityNumber",
                              "EntityRecordStorage.InternalError", info));
    END;
    LastNeighbour := createKey1;
    
    RETURN createKey1;
  END CreateEntityNumber;


PROCEDURE CreateEntity (label: CARDINAL): CARDINAL
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      EntityRecordStorage.PutRecord(createLocation, createKey1, createKey2);
      EntityRecordStorage.PutData(createLocation, label);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.CreateEntity",
                              "EntityRecordStorage.InternalError", info));
    | EntityRecordStorage.NotFree =>
        RAISE InternalError(
                ErrorSupport.Create("EntityStorage.CreateEntity",
                                    "EntityRecordStorage.NotFree"));
    END;

    RETURN createKey1;
  END CreateEntity;


PROCEDURE DeleteEntity (file: ITPFile.T; entity: CARDINAL)
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    location: EntityRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    TRY
      location := EntityRecordStorage.FindRecord(
                    file, IndexTree, key1 := entity, key2 := 0, found := found);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.DeleteEntity",
                              "EntityRecordStorage.InternalError", info));
    END;
    IF NOT (found) THEN RAISE EntityNotFound END;

    TRY
      EntityRecordStorage.DeleteRecord(location);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.DeleteEntity",
                              "EntityRecordStorage.InternalError", info));
    END;
  END DeleteEntity;


PROCEDURE ExistsEntity (file: ITPFile.T; entity: CARDINAL): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  VAR
    location: EntityRecordStorage.RecordLocation;
    found   : BOOLEAN;
  BEGIN
    TRY
      location := EntityRecordStorage.FindRecord(
                    file, IndexTree, key1 := entity, key2 := 0, found := found);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.ExistsEntity",
                              "EntityRecordStorage.InternalError", info));
    END;

    RETURN found;
  END ExistsEntity;


PROCEDURE PutLabel (file: ITPFile.T; entity: CARDINAL; label: CARDINAL)
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    location: EntityRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    TRY
      location := EntityRecordStorage.FindRecord(
                    file, IndexTree, key1 := entity, key2 := 0, found := found);
      IF NOT (found) THEN RAISE EntityNotFound END;

      EntityRecordStorage.PutData(location, label);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.PutLabel",
                              "EntityRecordStorage.InternalError", info));
    END;
  END PutLabel;


PROCEDURE GetLabel (file: ITPFile.T; entity: CARDINAL): CARDINAL
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    location: EntityRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    TRY
      location := EntityRecordStorage.FindRecord(
                    file, IndexTree, key1 := entity, key2 := 0, found := found);
      IF NOT (found) THEN RAISE EntityNotFound END;

      RETURN EntityRecordStorage.GetData(location);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.GetLabel",
                              "EntityRecordStorage.InternalError", info));
    END;
  END GetLabel;


PROCEDURE PutAttribute (file: ITPFile.T; entity: CARDINAL; attribute: TEXT)
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    location: EntityRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    TRY
      location := EntityRecordStorage.FindRecord(
                    file, IndexTree, key1 := entity, key2 := 0, found := found);
      IF NOT (found) THEN RAISE EntityNotFound END;

      EntityRecordStorage.PutAttribute(location, attribute);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.PutAttribute",
                              "EntityRecordStorage.InternalError", info));
    END;
  END PutAttribute;


PROCEDURE GetAttribute (file: ITPFile.T; entity: CARDINAL): TEXT
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    location: EntityRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    TRY
      location := EntityRecordStorage.FindRecord(
                    file, IndexTree, key1 := entity, key2 := 0, found := found);
      IF NOT (found) THEN RAISE EntityNotFound END;

      RETURN EntityRecordStorage.GetAttribute(location);
    EXCEPT
      EntityRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "EntityStorage.GetAttribute",
                              "EntityRecordStorage.InternalError", info));
    END;
  END GetAttribute;

BEGIN
END EntityStorage.
