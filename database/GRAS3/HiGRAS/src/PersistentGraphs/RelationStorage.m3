MODULE RelationStorage;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.2  1997/07/21 10:43:08  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1  1997/03/26 11:39:46  roland
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

    Revision 1.4  1996/11/20 12:23:17  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/17 13:09:17  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.2  1996/08/06 16:26:45  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/07/24 09:20:16  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.1.2.1  1996/04/29 13:43:54  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.1  1994/01/20  18:41:53  pk
# Initial revision
#
*)
(***************************************************************************)
IMPORT CardSet, Access, ITPFile, BitCompareWord,
       RelationRecordStorage, RelationRecordParameter, CardRelation;
IMPORT ErrorSupport;

PROCEDURE Init (file: ITPFile.T; tree: CARDINAL)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RelationRecordStorage.Init(file, tree);
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.Init",
                          "RelationRecordStorage.InternalError", info));
    | RelationRecordStorage.LevelError =>
        RAISE InternalError(
                ErrorSupport.Create("RelationStorage.Init",
                                    "RelationRecordStorage.LevelError"));
    END;
  END Init;


PROCEDURE PutRelation (file                     : ITPFile.T;
                       tree                     : CARDINAL;
                       order                    : Order;
                       entity1, entity2, entity3: CARDINAL   )
  RAISES {Access.Locked, InternalError} =
  VAR
    key1     : RelationRecordParameter.Key1;
    location : RelationRecordStorage.RecordLocation;
    found    : BOOLEAN;
    displaced: BOOLEAN;
  BEGIN
    key1 := RelationRecordParameter.Key1{
              order := order, entity1 := entity1, overflowCounter := 0};
    TRY
      location :=
        RelationRecordStorage.FindRecord(file, tree, key1, entity2, found);
      IF NOT found THEN
        RelationRecordStorage.PutRecord(location, key1, entity2);
      END;

      (* store third component in the set *)
      RelationRecordStorage.PutSetElement(location, entity3, displaced);
      WHILE (displaced) DO
        INC(key1.overflowCounter);
        location := RelationRecordStorage.FindRecord(
                      file, tree, key1, entity2, found);
        IF NOT found THEN
          RelationRecordStorage.PutRecord(location, key1, entity2);
        END;
        RelationRecordStorage.PutSetElement(location, entity3, displaced);
      END;
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.PutRelation",
                          "RelationRecordStorage.InternalError", info));
    | RelationRecordStorage.NotFree =>
        RAISE InternalError(
                ErrorSupport.Create("RelationStorage.PutRelation",
                                    "RelationRecordStorage.NotFree"));
    END;
  END PutRelation;


PROCEDURE DeleteRelation (file                     : ITPFile.T;
                          tree                     : CARDINAL;
                          order                    : Order;
                          entity1, entity2, entity3: CARDINAL   )
  RAISES {Access.Locked, RelationNotFound, InternalError} =
  VAR
    key1, oldKey1         : RelationRecordParameter.Key1;
    oldKey2               : RelationRecordParameter.Key2;
    location, nextLocation: RelationRecordStorage.RecordLocation;
    found                 : BOOLEAN;
    displaced             : BOOLEAN;
    smallestElement       : CARDINAL;
  BEGIN
    key1 := RelationRecordParameter.Key1{
              order := order, entity1 := entity1, overflowCounter := 0};
    TRY
      location :=
        RelationRecordStorage.FindRecord(file, tree, key1, entity2, found);
      (* RelationNotFound? *)
      IF NOT (found) THEN RAISE RelationNotFound END;

      (* determine the record which has the entity in it's set *)
      LOOP
        IF (RelationRecordStorage.IsElementInSet(location, entity3)) THEN
          RelationRecordStorage.DeleteSetElement(location, entity3);
          EXIT;
        END;
        WITH card = RelationRecordStorage.SetCardinality(location) DO
          (* RelationNotFound: no such relation? *)
          IF NOT (card >= RelationRecordStorage.MaxElements) THEN
            RAISE RelationNotFound
          END;

          WITH greatestElement = RelationRecordStorage.GetSetElement(
                                   location, card) DO
            (* RelationNotFound: no such relation? *)
            IF NOT (NOT BitCompareWord.F(greatestElement, entity3)) THEN
              RAISE RelationNotFound
            END;
          END;
        END;
        INC(key1.overflowCounter);
        location := RelationRecordStorage.FindRecord(
                      file, tree, key1, entity2, found);
        (* RelationNotFound? *)
        IF NOT (found) THEN RAISE RelationNotFound END;
      END;

      (* clean up overflow sets *)
      LOOP
        WITH card = RelationRecordStorage.SetCardinality(location) DO
          IF (card = 0) THEN
            (* record not used anymore *)
            RelationRecordStorage.DeleteRecord(location);
            EXIT;
          END;
          IF (card < RelationRecordStorage.MaxElements - 1) THEN
            (* this is the last overflow record *)
            EXIT;
          END;
        END;
        oldKey1 := key1;
        oldKey2 := entity2;
        INC(key1.overflowCounter);
        nextLocation := RelationRecordStorage.FindRecord(
                          file, tree, key1, entity2, found);
        IF (NOT found) THEN EXIT; END;

        (* move smallest element from next overflow record to current *)
        smallestElement := RelationRecordStorage.GetSetElement(
                             location := nextLocation, elementNo := 1);
        RelationRecordStorage.DeleteSetElement(
          location := nextLocation, element := smallestElement);
        location := RelationRecordStorage.FindRecord(
                      file, tree, oldKey1, oldKey2, found);
        IF NOT (found) THEN
          RAISE
            InternalError(ErrorSupport.Create(
                            "RelationStorage.DeleteRelation", "not found"))
        END;
        RelationRecordStorage.PutSetElement(
          location, smallestElement, displaced);
        IF displaced THEN
          RAISE
            InternalError(ErrorSupport.Create(
                            "RelationStorage.DeleteRelation", "Displaced"))
        END;

        location := nextLocation;
      END;
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.DeleteRelation",
                          "RelationRecordStorage.InternalError", info));
    | RelationRecordStorage.NotFree =>
        RAISE InternalError(
                ErrorSupport.Create("RelationStorage.DeleteRelation",
                                    "RelationRecordStorage.NotFree"));
    | RelationRecordStorage.NotFound =>
        RAISE InternalError(
                ErrorSupport.Create("RelationStorage.DeleteRelation",
                                    "RelationRecordStorage.NotFound"));
    END;
  END DeleteRelation;


PROCEDURE DeleteRelationsByEntity (    file   : ITPFile.T;
                                       tree   : CARDINAL;
                                       order  : Order;
                                       entity : CARDINAL;
                                   VAR deleted: CardRelation.T )
  RAISES {Access.Locked, InternalError} =
  VAR
    found           : BOOLEAN;
    cont            : BOOLEAN;
    recordFound     : BOOLEAN;
    range           : RelationRecordStorage.Range;
    location        : RelationRecordStorage.RecordLocation;
    key1            : RelationRecordParameter.Key1;
    key2            : RelationRecordParameter.Key2;
    deleteRecordKeys: CardRelation.T;
    counter         : CARDINAL;
  BEGIN
    deleted := CardRelation.New();
    deleteRecordKeys := CardRelation.New();

    TRY
      (* set up range for given entity *)
      WITH initialKey = RelationRecordParameter.Key1{
                          order := order, entity1 := entity,
                          overflowCounter := 0} DO
        range := RelationRecordStorage.InitRange(file, tree, initialKey);
      END;

      (* loop through the range *)
      RelationRecordStorage.GetNextInRange(range, found);
      WHILE (found) DO
        location := RelationRecordStorage.GetCurrentFromRange(range);
        cont := TRUE;
        (* now search this record and all overflow records *)
        WHILE (cont) DO
          (* copy all elements to deleted and remember key2 values *)
          RelationRecordStorage.GetKeys(location, key1, key2);
          deleteRecordKeys.insert(key1.overflowCounter, key2);
          WITH card = RelationRecordStorage.SetCardinality(location) DO
            FOR elementNo := 1 TO card DO
              WITH element = RelationRecordStorage.GetSetElement(
                               location, elementNo) DO
                deleted.insert(key2, element);
              END;
            END;
          END;

          (* compute overflow record if existent *)
          INC(key1.overflowCounter);
          location :=
            RelationRecordStorage.FindRecord(file, tree, key1, key2, cont);
        END;
        RelationRecordStorage.GetNextInRange(range, found);
      END;

      (* now we may actually delete the records found *)
      key1 := RelationRecordParameter.Key1{
                order := order, entity1 := entity, overflowCounter := 0};
      deleteRecordKeys.loop();
      deleteRecordKeys.get(counter, key2, found);
      key1.overflowCounter := counter;
      WHILE (found) DO
        location := RelationRecordStorage.FindRecord(
                      file, tree, key1, key2, recordFound);
        IF NOT (recordFound) THEN
          RAISE InternalError(ErrorSupport.Create(
                                "RelationStorage.DeleteRelationsByEntity",
                                "Not found"))
        END;

        RelationRecordStorage.DeleteRecord(location);
        deleteRecordKeys.get(counter, key2, found);
        key1.overflowCounter := counter;
      END;
      deleteRecordKeys.dispose();
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.DeleteRelationsByEntity",
                          "RelationRecordStorage.InternalError", info));
    END;
  END DeleteRelationsByEntity;


PROCEDURE IsARelation (file                     : ITPFile.T;
                       tree                     : CARDINAL;
                       order                    : Order;
                       entity1, entity2, entity3: CARDINAL   ): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  VAR
    key1    : RelationRecordParameter.Key1;
    found   : BOOLEAN;
    location: RelationRecordStorage.RecordLocation;
  BEGIN
    TRY
      key1 := RelationRecordParameter.Key1{
                order := order, entity1 := entity1, overflowCounter := 0};

      (* loop through the overflow records *)
      LOOP
        location := RelationRecordStorage.FindRecord(
                      file, tree, key1, entity2, found);
        IF NOT found THEN RETURN FALSE; END;
        IF (RelationRecordStorage.IsElementInSet(location, entity3)) THEN
          RETURN TRUE;
        END;

        (* determine next overflow record if necessary *)
        WITH currentCard = RelationRecordStorage.SetCardinality(location) DO
          IF (currentCard < RelationRecordStorage.MaxElements) THEN
            RETURN FALSE;
          END;
          WITH greatestElement = RelationRecordStorage.GetSetElement(
                                   location,
                                   RelationRecordStorage.MaxElements) DO
            IF (BitCompareWord.F(greatestElement, entity3)) THEN
              RETURN FALSE;
            END;
          END;
          (* overflow might exist and entity3 might be there *)
          INC(key1.overflowCounter);
        END;
      END;
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.IsARelation",
                          "RelationRecordStorage.InternalError", info));
    END;
  END IsARelation;


PROCEDURE InitRelationRange (file  : ITPFile.T;
                             tree  : CARDINAL;
                             order : Order;
                             entity: CARDINAL   ): RelationRange
  RAISES {Access.Locked, InternalError} =
  VAR range: RelationRange;
  BEGIN
    WITH initialKey = RelationRecordParameter.Key1{
                        order := order, entity1 := entity,
                        overflowCounter := 0} DO
      TRY
        range.recordRange :=
          RelationRecordStorage.InitRange(file, tree, initialKey);
      EXCEPT
        RelationRecordStorage.InternalError (info) =>
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "RelationStorage.InitRelationRange",
                            "RelationRecordStorage.InternalError", info));
      END;
      range.valid := FALSE;

      RETURN range;
    END;
  END InitRelationRange;


PROCEDURE GetFromRelationRange (    file            : ITPFile.T;
                                    tree            : CARDINAL;
                                VAR range           : RelationRange;
                                VAR found           : BOOLEAN;
                                VAR entity2, entity3: CARDINAL       )
  RAISES {Access.Locked, InternalError} =
  VAR
    key1     : RelationRecordParameter.Key1;
    nextFound: BOOLEAN;

  BEGIN
    TRY
      (* initialize location if necessary *)
      IF (NOT range.valid) THEN
        RelationRecordStorage.GetNextInRange(range.recordRange, found);
        IF NOT found THEN RETURN; END;
        range.valid := TRUE;
        range.location :=
          RelationRecordStorage.GetCurrentFromRange(range.recordRange);
        range.elementNo := 0;
      END;

      (* retrieve the entities *)
      INC(range.elementNo);
      RelationRecordStorage.GetKeys(range.location, key1, entity2);
      entity3 := RelationRecordStorage.GetSetElement(
                   range.location, range.elementNo);

      (* set up new range *)
      WITH card = RelationRecordStorage.SetCardinality(range.location) DO
        IF (range.elementNo >= card) THEN
          (* next element is not in the same record: try to find overflow
             record *)
          IF (card = RelationRecordStorage.MaxElements) THEN
            INC(key1.overflowCounter);
            range.location := RelationRecordStorage.FindRecord(
                                file, tree, key1, entity2, nextFound);
            IF nextFound THEN
              range.elementNo := 0;
            ELSE
              (* no overflow record: try next from record range *)
              range.valid := FALSE;
            END;
          ELSE
            range.valid := FALSE;
          END;
        END;
      END;
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.GetFromRelationRange",
                          "RelationRecordStorage.InternalError", info));
    END;
  END GetFromRelationRange;


PROCEDURE GetRelationRange (         file   : ITPFile.T;
                                     tree   : CARDINAL;
                            READONLY range  : RelationRange;
                                     permute: BOOLEAN        ): CardRelation.T
  RAISES {Access.Locked, InternalError} =
  VAR
    relation                      : CardRelation.T;
    found                         : BOOLEAN;
    currentEntity1, currentEntity2: CARDINAL;
    entity1, entity2              : CARDINAL;
    localRange                    : RelationRange;
  BEGIN
    (* retrieve current element from range and reset recordRange *)
    localRange := range;
    GetFromRelationRange(
      file, tree, localRange, found, currentEntity1, currentEntity2);
    relation := CardRelation.New();
    TRY
      RelationRecordStorage.ResetRange(localRange.recordRange);
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.GetRelationRange",
                          "RelationRecordStorage.InternalError", info));
    END;
    localRange.valid := FALSE;

    (* copy range elements to relation *)
    REPEAT
      GetFromRelationRange(file, tree, localRange, found, entity1, entity2);
      IF (found) THEN
        IF (permute) THEN
          relation.insert(entity2, entity1);
        ELSE
          relation.insert(entity1, entity2);
        END;
      END;
    UNTIL (NOT found);

    (* move relation pointer to current element *)
    relation.gotoElement(currentEntity1, currentEntity2, found);

    RETURN relation;
  END GetRelationRange;


PROCEDURE NoOfThirdComponents (file            : ITPFile.T;
                               tree            : CARDINAL;
                               order           : Order;
                               entity1, entity2: CARDINAL   ): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    key1    : RelationRecordParameter.Key1;
    found   : BOOLEAN;
    card    : CARDINAL;
    location: RelationRecordStorage.RecordLocation;
  BEGIN
    TRY
      key1 := RelationRecordParameter.Key1{
                order := order, entity1 := entity1, overflowCounter := 0};
      card := 0;

      (* loop through all overflow records *)
      LOOP
        location := RelationRecordStorage.FindRecord(
                      file, tree, key1, entity2, found);
        IF NOT found THEN EXIT; END;
        WITH currentCard = RelationRecordStorage.SetCardinality(location) DO
          INC(card, currentCard);
          IF (currentCard < RelationRecordStorage.MaxElements) THEN
            EXIT;
          END;
          INC(key1.overflowCounter);
        END;
      END;
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.NoOfThirdComponents",
                          "RelationRecordStorage.InternalError", info));
    END;

    RETURN card;
  END NoOfThirdComponents;


PROCEDURE GetThirdComponents (file            : ITPFile.T;
                              tree            : CARDINAL;
                              order           : Order;
                              entity1, entity2: CARDINAL   ): CardSet.T
  RAISES {Access.Locked, InternalError} =
  VAR
    key1      : RelationRecordParameter.Key1;
    found     : BOOLEAN;
    location  : RelationRecordStorage.RecordLocation;
    components: CardSet.T;
  BEGIN
    TRY
      key1 := RelationRecordParameter.Key1{
                order := order, entity1 := entity1, overflowCounter := 0};
      components := CardSet.New();

      (* loop through all overflow records *)
      LOOP
        location := RelationRecordStorage.FindRecord(
                      file, tree, key1, entity2, found);
        IF NOT found THEN EXIT; END;
        WITH currentCard = RelationRecordStorage.SetCardinality(location) DO
          FOR elementNo := 1 TO currentCard DO
            WITH element = RelationRecordStorage.GetSetElement(
                             location, elementNo) DO
              components.insert(element);
            END;
          END;
          INC(key1.overflowCounter);
        END;
      END;
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.GetThirdComponents",
                          "RelationRecordStorage.InternalError", info));
    END;

    RETURN components;
  END GetThirdComponents;


PROCEDURE GetFirstThirdComponent (    file            : ITPFile.T;
                                      tree            : CARDINAL;
                                      order           : Order;
                                      entity1, entity2: CARDINAL;
                                  VAR found           : BOOLEAN    ):
  CARDINAL RAISES {Access.Locked, InternalError} =
  VAR
    key1    : RelationRecordParameter.Key1;
    location: RelationRecordStorage.RecordLocation;
  BEGIN
    TRY
      key1 := RelationRecordParameter.Key1{
                order := order, entity1 := entity1, overflowCounter := 0};
      location :=
        RelationRecordStorage.FindRecord(file, tree, key1, entity2, found);
      IF NOT found THEN RETURN 0; END;
      IF (RelationRecordStorage.SetCardinality(location) = 0) THEN
        found := FALSE;
        RETURN 0;
      END;

      RETURN RelationRecordStorage.GetSetElement(location, elementNo := 1);
    EXCEPT
      RelationRecordStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "RelationStorage.GetFirstThirdComponent",
                          "RelationRecordStorage.InternalError", info));
    END;
  END GetFirstThirdComponent;

BEGIN
END RelationStorage.
