MODULE NameStorage;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:39:21  roland
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

    Revision 1.4  1996/11/20 12:23:04  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/08/06 16:26:35  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/07/24 09:20:10  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.2.2.1  1996/04/29 13:43:46  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.2  1994/11/24  10:36:51  roland
# Small bugfix in procedure PutName. Multiple nodes (entities) with the
# same index attribute (tag) are now treated correctly.
#
# Revision 1.1  1994/01/20  18:41:35  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Text, Word, Access, ITPFile, NameRecordParameter, NameRecordStorage,
       ErrorSupport;

CONST IndexTree = 2;
CONST EmptyLocation = NameRecordStorage.RecordLocation{NIL, 0, 0, 0};


PROCEDURE Init (file: ITPFile.T) RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      NameRecordStorage.Init(file, IndexTree);
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.Init",
                              "NameRecordStorage.InternalError", info));
    | NameRecordStorage.LevelError =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "NameStorage.Init", "NameRecordStorage.LevelError"));
    END;
  END Init;


PROCEDURE PutName (    file       : ITPFile.T;
                       tag        : CARDINAL;
                       name       : TEXT;
                       entity     : CARDINAL;
                   VAR otherEntity: CARDINAL;
                   VAR key1, key2 : CARDINAL;
                   VAR done       : BOOLEAN    )
  RAISES {Access.Locked, InternalError} =
  VAR
    location: NameRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    done := FALSE;

    TRY
      location := GetLocation(file, tag, name, found, key1, key2);
      IF (NOT found) THEN
        (* location is free: store keys, data, and attribute *)
        location := NameRecordStorage.FindRecord(file, IndexTree, key1, key2, found);
        (* record expected if location found *)
        IF found THEN
          RAISE InternalError(ErrorSupport.Create(
                                "NameStorage.PutName", "Unexpected found"))
        END;

        NameRecordStorage.PutRecord(location, key1, key2);
        WITH data = NameRecordParameter.Data{entity, tag} DO
          NameRecordStorage.PutData(location, data);
        END;
        NameRecordStorage.PutAttribute(location, name);
        done := TRUE;
      ELSE
        (* name already in use: compute otherEntity *)
        otherEntity := NameRecordStorage.GetData(location).entity;
        IF (otherEntity # 0) THEN
          WITH emptyData = NameRecordParameter.Data{0, tag} DO
            NameRecordStorage.PutData(location, emptyData);
          END;
        END;
      END;
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.PutName",
                              "NameRecordStorage.InternalError", info));
    | NameRecordStorage.NotFree =>
        RAISE
          InternalError(ErrorSupport.Create("NameStorage.PutName",
                                            "NameRecordStorage.NotFree"));
    END;
  END PutName;


PROCEDURE PutEntity (file      : ITPFile.T;
                     key1, key2: CARDINAL;
                     entity    : CARDINAL   )
  RAISES {Access.Locked, NameNotFound, InternalError} =
  VAR
    location: NameRecordStorage.RecordLocation;
    found   : BOOLEAN;
    data    : NameRecordParameter.Data;
  BEGIN
    TRY
      location := NameRecordStorage.FindRecord(file, IndexTree, key1, key2, found);
      IF NOT (found) THEN RAISE NameNotFound END;

      data := NameRecordStorage.GetData(location);
      data.entity := entity;
      NameRecordStorage.PutData(location, data);
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.PutEntity",
                              "NameRecordStorage.InternalError", info));
    END;
  END PutEntity;


PROCEDURE DeleteName (    file      : ITPFile.T;
                          tag       : CARDINAL;
                          name      : TEXT;
                      VAR entity    : CARDINAL;
                      VAR key1, key2: CARDINAL;
                      VAR multiple  : BOOLEAN    )
  RAISES {Access.Locked, NameNotFound, InternalError} =
  VAR
    location: NameRecordStorage.RecordLocation;
    found   : BOOLEAN;
  BEGIN
    multiple := FALSE;

    TRY
      location := GetLocation(file, tag, name, found, key1, key2);
      (* NameNotFound *)
      IF NOT (found) THEN RAISE NameNotFound END;

      entity := NameRecordStorage.GetData(location).entity;
      IF (entity = 0) THEN
        multiple := TRUE;
      ELSE
        NameRecordStorage.DeleteRecord(location);
      END;
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.DeleteName",
                              "NameRecordStorage.InternalError", info));
    END;
  END DeleteName;


PROCEDURE DeleteNameByKeys (    file      : ITPFile.T;
                                key1, key2: CARDINAL;
                            VAR entity    : CARDINAL;
                            VAR multiple  : BOOLEAN    )
  RAISES {Access.Locked, NameNotFound, InternalError} =
  VAR
    location: NameRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    multiple := FALSE;

    TRY
      location := NameRecordStorage.FindRecord(file, IndexTree, key1, key2, found);
      (* NameNotFound? *)
      IF NOT (found) THEN RAISE NameNotFound END;

      entity := NameRecordStorage.GetData(location).entity;
      IF (entity = 0) THEN
        multiple := TRUE;
      ELSE
        NameRecordStorage.DeleteRecord(location);
      END;
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.DeleteNameByKeys",
                              "NameRecordStorage.InternalError", info));
    END;
  END DeleteNameByKeys;


PROCEDURE FindName (    file      : ITPFile.T;
                        tag       : CARDINAL;
                        name      : TEXT;
                    VAR entity    : CARDINAL;
                    VAR key1, key2: CARDINAL;
                    VAR found     : BOOLEAN;
                    VAR multiple  : BOOLEAN    )
  RAISES {Access.Locked, InternalError} =
  VAR location: NameRecordStorage.RecordLocation;
  BEGIN
    found := FALSE;
    multiple := FALSE;
    location := GetLocation(file, tag, name, found, key1, key2);
    IF (NOT found) THEN RETURN; END;

    TRY
      entity := NameRecordStorage.GetData(location).entity;
    EXCEPT
    | NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.FindName",
                              "NameRecordStorage.InternalError", info));
    END;

    IF (entity = 0) THEN multiple := TRUE; END;
  END FindName;


PROCEDURE GetNameFromKeys (file: ITPFile.T; key1, key2: CARDINAL): TEXT
  RAISES {Access.Locked, NameNotFound, InternalError} =
  VAR
    location: NameRecordStorage.RecordLocation;
    found   : BOOLEAN;
  BEGIN
    TRY
      location := NameRecordStorage.FindRecord(file, IndexTree, key1, key2, found);

      (* NameNotFound? *)
      IF NOT (found) THEN RAISE NameNotFound END;

      RETURN NameRecordStorage.GetAttribute(location);
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.GetNameFromKeys",
                              "NameRecordStorage.InternalError", info));
    END;
  END GetNameFromKeys;


PROCEDURE GetTagFromKeys (file: ITPFile.T; key1, key2: CARDINAL): CARDINAL
  RAISES {Access.Locked, NameNotFound, InternalError} =
  VAR
    location: NameRecordStorage.RecordLocation;
    found   : BOOLEAN;
  BEGIN
    TRY
      location := NameRecordStorage.FindRecord(file, IndexTree, key1, key2, found);

      (* NameNotFound? *)
      IF NOT (found) THEN RAISE NameNotFound END;

      RETURN NameRecordStorage.GetData(location).tag;
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.GetTagFromKeys",
                              "NameRecordStorage.InternalError", info));
    END;
  END GetTagFromKeys;


PROCEDURE GetNameAndTagFromKeys (    file      : ITPFile.T;
                                     key1, key2: CARDINAL;
                                 VAR name      : TEXT;
                                 VAR tag       : CARDINAL   )
  RAISES {Access.Locked, NameNotFound, InternalError} =
  VAR
    location: NameRecordStorage.RecordLocation;
    found   : BOOLEAN;

  BEGIN
    TRY
      location := NameRecordStorage.FindRecord(file, IndexTree, key1, key2, found);

      (* NameNotFound? *)
      IF NOT (found) THEN RAISE NameNotFound END;

      name := NameRecordStorage.GetAttribute(location);
      tag := NameRecordStorage.GetData(location).tag;
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.GetNameAndTagFromKeys",
                              "NameRecordStorage.InternalError", info));
    END;
  END GetNameAndTagFromKeys;


(* Try to find the location and keys for the given name.  If no such name
   exists, found returns FALSE and the keys are set to the values under
   which the name should be stored. *)
PROCEDURE GetLocation (    file      : ITPFile.T;
                           tag       : CARDINAL;
                           name      : TEXT;
                       VAR found     : BOOLEAN;
                       VAR key1, key2: CARDINAL   ):
  NameRecordStorage.RecordLocation RAISES {Access.Locked, InternalError} =
  VAR
    hashValue: Word.T;
    location : NameRecordStorage.RecordLocation;
    data     : NameRecordParameter.Data;
    length   : CARDINAL;
    maxKey2  : CARDINAL;
    range    : NameRecordStorage.Range;
    attribute: TEXT;
  BEGIN
    length := Text.Length(name);
    found := FALSE;

    (* compute key1: hash over all characters *)
    hashValue := tag;
    WITH MaxCard = LAST(CARDINAL) DO
      FOR i := 0 TO length - 1 DO
        WITH c = Text.GetChar(name, i) DO
          hashValue :=
            (((hashValue * 211) MOD MaxCard) + ORD(c)) MOD MaxCard;
        END;
      END;
      IF (hashValue = 0) THEN key1 := MaxCard; ELSE key1 := hashValue; END;
    END;

    (* compute key2: search name in all attributes with the given key1 *)
    TRY
      range := NameRecordStorage.InitRange(file, IndexTree, key1);
      maxKey2 := 0;

      LOOP
        NameRecordStorage.GetNextInRange(range, found);
        IF (NOT found) THEN EXIT; END;

        location := NameRecordStorage.GetCurrentFromRange(range);
        NameRecordStorage.GetKeys(location, key1, key2);
        data := NameRecordStorage.GetData(location);
        IF (tag = data.tag) THEN
          attribute := NameRecordStorage.GetAttribute(location);
          found := Text.Equal(attribute, name);
          IF (found) THEN EXIT; END;
        END;

        maxKey2 := MAX(maxKey2, key2);
      END;
    EXCEPT
      NameRecordStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "NameStorage.GetLocation",
                              "NameRecordStorage.InternalError", info));
    END;

    IF (NOT found) THEN
      key2 := maxKey2 + 1;
      location := EmptyLocation;
    END;

    RETURN location;
  END GetLocation;

BEGIN
END NameStorage.
