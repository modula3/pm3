MODULE LongAttributeStorage;

(***************************************************************************)
(* The first 255 bytes of attribute 0 are stored at the entity, the rest is
   split into 255-bytes records and stored in AttributeStorage. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.2  1997/07/21 10:42:56  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1  1997/03/26 11:39:16  roland
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

    Revision 1.7  1996/11/20 12:22:58  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.6  1996/09/17 13:09:03  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.5  1996/08/06 16:26:30  roland
    Merge of PAGESERVER and main branch.

    Revision 1.4.2.2  1996/07/24 09:20:03  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.4.2.1  1996/04/29 13:43:42  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

    Revision 1.4  1996/01/17 17:57:09  roland
    PutRecord modified.

# Revision 1.3  1995/07/05  16:03:13  alex
# Add the method deleteAttribute to PersistentGraph
#
# Revision 1.2  1994/03/30  18:31:24  pk
# Adaptions for 3.1.
#
# Revision 1.1  1994/01/20  18:41:27  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Text, ASCII, Access, ITPFile, AttributeStorage,
       EntityStorage, CardSet, AttributeDescription,
       AttributeDescriptionSet, CardRelation, ErrorSupport;



CONST
  RecordSize       = 255;
  EmptyRecordArray = RecordArray{ASCII.NUL, ..};


TYPE RecordArray = ARRAY [1 .. RecordSize] OF CHAR;


VAR (* CONST *) EmptyRecord := Text.FromChars(EmptyRecordArray);


PROCEDURE PutAttribute (file       : ITPFile.T;
                        entity     : CARDINAL;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        attribute  : TEXT       )
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    currentRecordNo: CARDINAL;
    lastRecordNo   : CARDINAL;
    totalLength    : CARDINAL;
    recordNo       : CARDINAL;
    record         : TEXT;
  BEGIN
    WITH length = Text.Length(attribute) DO
      IF (length = 0) THEN RETURN; END;

      totalLength := start + length;
      lastRecordNo := (totalLength - 1) DIV RecordSize;
      currentRecordNo := start DIV RecordSize;
    END;

    (* pad all preceding records *)
    IF (currentRecordNo > 0) THEN
      recordNo := currentRecordNo - 1;
      LOOP
        record := GetRecord(file, entity, attributeNo, recordNo,
                            start := 0, length := RecordSize);
        IF (Text.Length(record) > 0) THEN
          IF (Text.Length(record) < RecordSize) THEN
            record := Text.Sub(record & EmptyRecord, 0, RecordSize);
            PutRecord(file, entity, attributeNo, recordNo, 0, record);
          END;
          EXIT;
        END;
        PutRecord(file, entity, attributeNo, recordNo, 0, EmptyRecord);
        IF (recordNo = 0) THEN EXIT; END;
        DEC(recordNo);
      END;
    END;

    (* write first record *)
    WITH relativeStart = start MOD RecordSize DO
      record := Text.Sub(attribute, 0, RecordSize - relativeStart);
      PutRecord(
        file, entity, attributeNo, currentRecordNo, relativeStart, record);
      IF (currentRecordNo = lastRecordNo) THEN RETURN; END;
      INC(currentRecordNo);
    END;

    (* write last record *)
    WITH relativeEnd = ((totalLength - 1) MOD RecordSize) + 1 DO
      record := Text.Sub(attribute, lastRecordNo * RecordSize - start,
                         relativeEnd);
      PutRecord(file, entity, attributeNo, lastRecordNo, 0, record);
    END;

    (* write remaining records *)
    WHILE (currentRecordNo < lastRecordNo) DO
      record := Text.Sub(attribute, currentRecordNo * RecordSize - start,
                         RecordSize);
      PutRecord(file, entity, attributeNo, currentRecordNo, 0, record);
      INC(currentRecordNo);
    END;
  END PutAttribute;


PROCEDURE GetAttribute (file       : ITPFile.T;
                        entity     : CARDINAL;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        length     : CARDINAL   ): TEXT
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    currentRecordNo: CARDINAL;
    lastRecordNo   : CARDINAL;
    relativeStart  : CARDINAL;
    relativeEnd    : CARDINAL;
    attribute      : TEXT;
    record         : TEXT;
    lengthToRead   : CARDINAL;

  BEGIN
    currentRecordNo := start DIV RecordSize;
    IF (length = 0) THEN RETURN ""; END;

    WITH totalLength = start + length - 1 DO
      lastRecordNo := totalLength DIV RecordSize;
      relativeEnd := (totalLength MOD RecordSize) + 1;
      relativeStart := start MOD RecordSize;
    END;

    IF (currentRecordNo = lastRecordNo) THEN
      lengthToRead := length;
    ELSE
      lengthToRead := RecordSize - relativeStart;
    END;
    record := GetRecord(file, entity, attributeNo, currentRecordNo,
                        relativeStart, lengthToRead);
    attribute := record;
    IF (Text.Length(record) < lengthToRead) THEN RETURN attribute; END;
    INC(currentRecordNo);

    WHILE (currentRecordNo <= lastRecordNo) DO
      IF (currentRecordNo = lastRecordNo) THEN
        lengthToRead := relativeEnd;
      ELSE
        lengthToRead := RecordSize;
      END;
      record := GetRecord(file, entity, attributeNo, currentRecordNo, 0,
                          lengthToRead);
      attribute := attribute & record;
      IF (Text.Length(record) < lengthToRead) THEN RETURN attribute; END;
      INC(currentRecordNo);
    END;
    RETURN attribute;
  END GetAttribute;


PROCEDURE TruncateAttribute (file       : ITPFile.T;
                             entity     : CARDINAL;
                             attributeNo: CARDINAL;
                             size       : CARDINAL   )
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    old     : TEXT;
    new     : TEXT;
    found              := TRUE;
    recordNo: CARDINAL;

  BEGIN
    (* truncate last used attribute *)
    WITH lastRecordNo = size DIV RecordSize,
         remaining    = size MOD RecordSize  DO
      old := GetRecord(file, entity, attributeNo, lastRecordNo, start := 0,
                       length := RecordSize);
      new := Text.Sub(old, 0, remaining);
      PutRecord(file, entity, attributeNo, lastRecordNo, 0, new,
                transparent := FALSE);
      recordNo := lastRecordNo + 1;
    END;

    TRY
      (* delete the rest *)
      WHILE (found) DO
        AttributeStorage.DeleteAttribute(
          file, entity, attributeNo, recordNo, found);
        INC(recordNo);
      END;
    EXCEPT
      AttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "LongAttributeStorage.TruncateAttribute",
                              "AttributeStorage.InternalError", info));
    END;
  END TruncateAttribute;

PROCEDURE DeleteAttribute (file       : ITPFile.T;
                           entity     : CARDINAL;
                           attributeNo: CARDINAL   )
  RAISES {Access.Locked, InternalError} =
  VAR
    found              := TRUE;
    recordNo: CARDINAL := 0;
  BEGIN
    TRY
      WHILE found DO
        AttributeStorage.DeleteAttribute(
          file, entity, attributeNo, recordNo, found);
        INC(recordNo);
      END;
    EXCEPT
      AttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "LongAttributeStorage.DeleteAttribute",
                              "AttributeStorage.InternalError", info));
    END;
  END DeleteAttribute;

PROCEDURE DeleteAllAttributes (file: ITPFile.T; entity: CARDINAL)
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  BEGIN
    TRY
      EntityStorage.PutAttribute(file, entity, "");
      AttributeStorage.DeleteAllAttributes(file, entity);
    EXCEPT
      AttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "LongAttributeStorage.DeleteAllAttributes",
                              "AttributeStorage.InternalError", info));
    | EntityStorage.EntityNotFound => RAISE EntityNotFound;
    | EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "LongAttributeStorage.DeleteAllAttributes",
                              "EntityStorage.InternalError", info));
    END;
  END DeleteAllAttributes;


PROCEDURE GetAllAttributeNumbers (file: ITPFile.T; entity: CARDINAL):
  CardSet.T RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    attributeNumbers: CardSet.T;
    attributeNo     : CARDINAL;
    recordNo        : CARDINAL;
    records         : CardRelation.T;
    found           : BOOLEAN;
  BEGIN
    attributeNumbers := CardSet.New();

    TRY
      records := AttributeStorage.GetAllAttributes(file, entity);
      records.loop();

      records.get(attributeNo, recordNo, found);
      WHILE (found) DO
        attributeNumbers.insert(attributeNo);
        records.get(attributeNo, recordNo, found);
      END;

      WITH firstRecord = EntityStorage.GetAttribute(file, entity) DO
        IF (Text.Length(firstRecord) > 0) THEN
          attributeNumbers.insert(0);
        END;
      END;
    EXCEPT
      EntityStorage.EntityNotFound => RAISE EntityNotFound;
    | EntityStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "LongAttributeStorage.GetAllAttributeNumbers",
                          "EntityStorage.InternalError", info));
    | AttributeStorage.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "LongAttributeStorage.GetAllAttributeNumbers",
                          "AttributeStorage.InternalError", info));
    END;

    RETURN attributeNumbers;
  END GetAllAttributeNumbers;


PROCEDURE GetAllAttributes (file: ITPFile.T; entity: CARDINAL):
  AttributeDescriptionSet.T
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    attribute       : AttributeDescription.T;
    attributes      : AttributeDescriptionSet.T;
    attributeNumbers: CardSet.T;
    attributeNo     : CARDINAL;
    record          : TEXT;
    recordNo        : CARDINAL;
    found           : BOOLEAN;
    recordFound     : BOOLEAN;
  BEGIN
    attributes := AttributeDescriptionSet.New();
    attributeNumbers := GetAllAttributeNumbers(file, entity);

    (* loop through all used attributes *)
    attributeNumbers.loop();
    attributeNo := attributeNumbers.get(found);
    WHILE (found) DO
      attribute.number := attributeNo;
      recordNo := 0;

      TRY
        IF (attributeNo = 0) THEN
          attribute.value := EntityStorage.GetAttribute(file, entity);
        ELSE
          attribute.value := "";
        END;
      EXCEPT
        EntityStorage.EntityNotFound => RAISE EntityNotFound;
      | EntityStorage.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "LongAttributeStorage.GetAllAttributes",
                                "EntityStorage.InternalError", info));
      END;

      TRY
        (* collect all attribute records *)
        recordFound := TRUE;
        WHILE (recordFound) DO
          record := AttributeStorage.GetAttribute(
                      file, entity, attributeNo, recordNo, recordFound);
          attribute.value := attribute.value & record;
          INC(recordNo);
        END;
      EXCEPT
        AttributeStorage.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "LongAttributeStorage.GetAllAttributes",
                                "AttributeStorage.InternalError", info));
      END;

      (* store attribute *)
      attributes.insert(attribute);
      attributeNo := attributeNumbers.get(found);
    END;
    attributeNumbers.dispose();
    
    RETURN attributes;
  END GetAllAttributes;


(* Store one attribute record.  If transparent is true, the attribute is
   stored transparent, i.e.  old parts of the record are copied unless
   overwritten by the new part.  Otherwise only the leading parts of the
   record are saved. *)
PROCEDURE PutRecord (file       : ITPFile.T;
                     entity     : CARDINAL;
                     attributeNo: CARDINAL;
                     recordNo   : CARDINAL;
                     start      : CARDINAL;
                     attribute  : TEXT;
                     transparent: BOOLEAN     := TRUE)
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    found : BOOLEAN;
    old              := "";
    new   : TEXT;
    tail             := "";
    length: CARDINAL;

  BEGIN
    length := Text.Length(attribute);

    TRY
      IF ((start > 0) OR (transparent AND (length < RecordSize))) THEN
        (* get old attribute value *)
        IF (attributeNo = 0) THEN
          IF (recordNo = 0) THEN
            old := EntityStorage.GetAttribute(file, entity);
          ELSE
            old := AttributeStorage.GetAttribute(
                     file, entity, attributeNo, recordNo - 1, found);
          END;
        ELSE
          old := AttributeStorage.GetAttribute(
                   file, entity, attributeNo, recordNo, found);
          IF (NOT found) THEN old := ""; END;
        END;

        (* pad or truncate old value *)
        WITH oldLength = Text.Length(old),
             behindNew = start + length    DO
          IF (oldLength > behindNew) THEN
            tail := Text.Sub(old, behindNew, oldLength - behindNew);
          END;
          IF (oldLength < start) THEN
            old := old & Text.Sub(EmptyRecord, 0, start - oldLength);
          ELSE
            old := Text.Sub(old, 0, start);
          END;
        END;
      END;

      (* construct new value & store it *)
      new := old & attribute & tail;
      IF (attributeNo = 0) THEN
        IF (recordNo = 0) THEN
          EntityStorage.PutAttribute(file, entity, new);
        ELSE
          AttributeStorage.PutAttribute(
            file, entity, attributeNo, recordNo - 1, new);
        END;
      ELSE
        AttributeStorage.PutAttribute(
          file, entity, attributeNo, recordNo, new);
      END;
    EXCEPT
      EntityStorage.EntityNotFound => RAISE EntityNotFound;
    | EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "LongAttributeStorage.PutRecord",
                              "EntityStorage.InternalError", info));
    | AttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "LongAttributeStorage.PutRecord",
                              "AttributeStorage.InternalError", info));
    END;
  END PutRecord;


(* Get one attribute record. *)
PROCEDURE GetRecord (file       : ITPFile.T;
                     entity     : CARDINAL;
                     attributeNo: CARDINAL;
                     recordNo   : CARDINAL;
                     start      : CARDINAL;
                     length     : CARDINAL   ): TEXT
  RAISES {Access.Locked, EntityNotFound, InternalError} =
  VAR
    found           := TRUE;
    attribute: TEXT;

  BEGIN
    TRY
      IF (attributeNo = 0) THEN
        IF (recordNo = 0) THEN
          attribute := EntityStorage.GetAttribute(file, entity);
        ELSE
          attribute := AttributeStorage.GetAttribute(
                         file, entity, attributeNo, recordNo - 1, found);
        END;
      ELSE
        attribute := AttributeStorage.GetAttribute(
                       file, entity, attributeNo, recordNo, found);
      END;
    EXCEPT
      EntityStorage.EntityNotFound => RAISE EntityNotFound;
    | EntityStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "LongAttributeStorage.GetRecord",
                              "EntityStorage.InternalError", info));
    | AttributeStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "LongAttributeStorage.GetRecord",
                              "AttributeStorage.InternalError", info));
    END;
    IF (NOT found) THEN RETURN ""; END;

    WITH actualLength = MIN(Text.Length(attribute) - start, length) DO
      IF (actualLength <= 0) THEN
        RETURN "";
      ELSE
        RETURN Text.Sub(attribute, start, actualLength);
      END;
    END;
  END GetRecord;

BEGIN
END LongAttributeStorage.
