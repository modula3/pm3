MODULE AttributeStorage;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.3  1998/03/17 14:14:12  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.2  1997/07/21 10:42:50  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1  1997/03/26 11:38:56  roland
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

    Revision 1.5  1996/12/03 13:13:11  roland
    (Still) Replaced Type.Triple by CARDINAL, because of compiler error.

    Revision 1.4  1996/12/03 09:53:57  roland
    Type.Triple replaced by CARDINAL because of compiler bug.

    Revision 1.3  1996/11/20 12:22:47  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:26:22  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/07/24 09:19:50  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.1.2.1  1996/04/29 13:43:30  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.1  1994/01/20  18:41:15  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Access, ITPFile, AttributeRecordStorage,
       AttributeRecordParameter, CardRelation, ErrorSupport;

CONST IndexTree = 4;

PROCEDURE Init (file: ITPFile.T) RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      AttributeRecordStorage.Init(file, IndexTree);
    EXCEPT
      AttributeRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "AttributeStorage.Init",
                          "AttributeRecordStorage.InternalError", info));
    | AttributeRecordStorage.LevelError =>
        RAISE InternalError(
                ErrorSupport.Create("AttributeStorage.Init",
                                    "AttributeRecordStorage.LevelError"));
    END;
  END Init;


PROCEDURE PutAttribute (file       : ITPFile.T;
                        entity     : CARDINAL;
                        attributeNo: CARDINAL;
                        recordNo   : CARDINAL (* Type.Triple *);
                        attribute  : TEXT         )
  RAISES {Access.Locked, InternalError} =
  VAR
    key2    : AttributeRecordParameter.Key2;
    location: AttributeRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    key2 := AttributeRecordParameter.Key2{attributeNo, recordNo};
    TRY
      location :=
        AttributeRecordStorage.FindRecord(file, IndexTree, entity, key2, found);
      IF NOT found THEN
        AttributeRecordStorage.PutRecord(location, entity, key2);
      END;
      AttributeRecordStorage.PutAttribute(location, attribute);
    EXCEPT
      AttributeRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "AttributeStorage.PutAttribute",
                          "AttributeRecordStorage.InternalError", info));
    | AttributeRecordStorage.NotFree =>
        RAISE InternalError(
                ErrorSupport.Create("AttributeStorage.PutAttribute",
                                    "AttributeRecordStorage.NotFree"));
    END;
  END PutAttribute;


PROCEDURE GetAttribute (    file       : ITPFile.T;
                            entity     : CARDINAL;
                            attributeNo: CARDINAL;
                            recordNo   : CARDINAL (* Type.Triple *);
                        VAR found      : BOOLEAN      ): TEXT
  RAISES {Access.Locked, InternalError} =
  VAR
    location : AttributeRecordStorage.RecordLocation;
    attribute: TEXT;
  BEGIN
    TRY
      WITH key2 = AttributeRecordParameter.Key2{attributeNo, recordNo} DO
        location :=
            AttributeRecordStorage.FindRecord(file, IndexTree, entity, key2, found);
      END;

      IF (found) THEN
        attribute := AttributeRecordStorage.GetAttribute(location);
      ELSE
        attribute := "";
      END;
    EXCEPT
      AttributeRecordStorage.InternalError (info) =>
      RAISE
        InternalError(ErrorSupport.Propagate(
                          "AttributeStorage.GetAttribute",
                          "AttributeRecordStorage.InternalError", info));
    END;

    RETURN attribute;
  END GetAttribute;


PROCEDURE DeleteAttribute (    file       : ITPFile.T;
                               entity     : CARDINAL;
                               attributeNo: CARDINAL;
                               recordNo   : CARDINAL (* Type.Triple *);
                           VAR found      : BOOLEAN      )
  RAISES {Access.Locked, InternalError} =
  VAR location: AttributeRecordStorage.RecordLocation;
  BEGIN
    WITH key2 = AttributeRecordParameter.Key2{attributeNo, recordNo} DO
      TRY
        location :=
          AttributeRecordStorage.FindRecord(file, IndexTree, entity, key2, found);
      EXCEPT
        AttributeRecordStorage.InternalError (info) =>
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "AttributeStorage.DeleteAttribute",
                            "AttributeRecordStorage.InternalError", info));
      END;
      IF NOT found THEN RETURN; END;
    END;

    TRY
      AttributeRecordStorage.DeleteRecord(location);
    EXCEPT
      AttributeRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "AttributeStorage.DeleteAttribute",
                          "AttributeRecordStorage.InternalError", info));
    END;
  END DeleteAttribute;


PROCEDURE DeleteAllAttributes (file: ITPFile.T; entity: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  VAR
    range   : AttributeRecordStorage.Range;
    found   : BOOLEAN;
    location: AttributeRecordStorage.RecordLocation;
  BEGIN
    TRY
      LOOP
        (* range has to be reinitialized after every delete *)
        range := AttributeRecordStorage.InitRange(file, IndexTree, entity);
        AttributeRecordStorage.GetNextInRange(range, found);
        IF NOT found THEN EXIT; END;

        location := AttributeRecordStorage.GetCurrentFromRange(range);
        AttributeRecordStorage.DeleteRecord(location);
      END;
    EXCEPT
      AttributeRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "AttributeStorage.DeleteAllAttributes",
                          "AttributeRecordStorage.InternalError", info));
    END;
  END DeleteAllAttributes;


PROCEDURE GetAllAttributes (file: ITPFile.T; entity: CARDINAL): CardRelation.T
  RAISES {Access.Locked, InternalError} =
  VAR
    relation: CardRelation.T;
    range   : AttributeRecordStorage.Range;
    found   : BOOLEAN;
    location: AttributeRecordStorage.RecordLocation;
    key1    : AttributeRecordParameter.Key1;
    key2    : AttributeRecordParameter.Key2;
  BEGIN
    relation := NEW(CardRelation.T).init();

    TRY
      range := AttributeRecordStorage.InitRange(file, IndexTree, entity);
      AttributeRecordStorage.GetNextInRange(range, found);
      WHILE (found) DO
        location := AttributeRecordStorage.GetCurrentFromRange(range);
        AttributeRecordStorage.GetKeys(location, key1, key2);
        relation.insert(key2.attributeNo, key2.recordNo);

        AttributeRecordStorage.GetNextInRange(range, found);
      END;
    EXCEPT
      AttributeRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "AttributeStorage.GetAllAttributes",
                          "AttributeRecordStorage.InternalError", info));
    END;

    RETURN relation;
  END GetAllAttributes;

BEGIN
END AttributeStorage.

