MODULE Names;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.8  1998/06/10 10:54:52  kluck
    Bugfixes and prettyprinting.

    Revision 1.7  1998/05/19 10:20:38  roland
    Bugfixes for local graphs and pretty-printing.

    Revision 1.6  1998/03/18 09:27:15  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.5  1998/03/17 14:19:43  kluck
    Administration of local graphs implemented. (MK)

    Revision 1.4  1997/07/21 10:43:00  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.3  1997/04/24 14:32:30  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.2  1997/04/23 14:33:48  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:23  roland
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

IMPORT TextIdSet, TextId, TextCursorSet;
IMPORT Database, Access, CardSet, PageFile, VirtualResource;
IMPORT Word, Text, Pathname;
IMPORT ErrorSupport;

REVEAL
  T = Public BRANDED OBJECT
        graph: Database.T := NIL; (* graph storing all remote
                                     information *)
        localgraph: Database.T := NIL; (* graph storing all local
                                          information *)
        anchor: CARDINAL;        (* A node as entry point to everything
                                    else *)
        loggedIn: BOOLEAN := FALSE;
      OVERRIDES
        login                          := Login;
        logout                         := Logout;
        declareCollection              := DeclareCollection;
        declareCollectionAttribute     := DeclareCollectionAttribute;
        setCollectionAttribute         := SetCollectionAttribute;
        getCollectionAttribute         := GetCollectionAttribute;
        getAllCollections              := GetAllCollections;
        getAllDeclCollectionAttributes := GetAllDeclCollectionAttributes;
        insert                         := Insert;
        remove                         := Remove;
        change                         := Change;
        name                           := Name;
        id                             := Id;
        isRemoteId                     := IsRemoteId;
        contained                      := Contained;
        getAllEntries                  := GetAllEntries;
        declareAttribute               := DeclareAttribute;
        setAttribute                   := SetAttribute;
        getAttribute                   := GetAttribute;
        getAllDeclAttributes           := GetAllDeclAttributes;
        declareRelation                := DeclareRelation;
        getAllRelations                := GetAllRelations;
        insertInRelation               := InsertInRelation;
        removeFromRelation             := RemoveFromRelation;
        targets                        := Targets;
        sources                        := Sources;
        related                        := Related;
      END;

CONST
  (* Node labels (types) *)
  AnchorLabel = 1;
  EntryLabel  = 2;
  CollLabel   = 3;
  RelLabel    = 4;
  AttrLabel   = 5;
  CAttrLabel  = 6;

  (* With different index numbers we create different name spaces for
     collections, relations, and names. *)
  CollIdx  = 1;
  NameIdx  = 2;
  RelIdx   = 3;
  AttrIdx  = 4;
  CAttrIdx = 5;

  (* Edge labels *)
  CONTAINS       = 1;
  SUBCOLLECTION  = 2;
  RELATION       = 3;
  ATTRIBUTE      = 4;
  COLLATTRIBUTE  = 5;
  FirstFreeLabel = COLLATTRIBUTE + 1;

  (* Attribute parameters *)
  FreeRelAttr  = 1;
  FreeAttrAttr = 2;
  FreeCAttr    = 3;

  FirstFreeCAttr = FreeCAttr + 1;
  FirstFreeAttr  = 1;

  NumberLength = 4;
  NumberStart  = 0;
  ValueStart   = NumberStart + NumberLength;

  RelLabelAttr = 1;
  AttrNumAttr  = 1;
  CAttrNumAttr = 1;


(* Internal stuff. *)


PROCEDURE NumberToText (no: CARDINAL): TEXT =
  (* converts a 4 byte cardinal to a TEXT-string of length 4 *)
  (* FOR-loop unrolled for efficiency *)
  VAR byte: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    byte[0] := VAL(Word.Extract(no, 24, 8), CHAR);
    byte[1] := VAL(Word.Extract(no, 16, 8), CHAR);
    byte[2] := VAL(Word.Extract(no, 8, 8), CHAR);
    byte[3] := VAL(Word.Extract(no, 0, 8), CHAR);
    RETURN Text.FromChars(byte);
  END NumberToText;


PROCEDURE TextToNumber (string: TEXT): CARDINAL =
  (* converts a TEXT-string of length 4 to a 4-byte cardinal *)
  (* FOR-loop unrolled for efficiency *)
  VAR no: CARDINAL := 0;
  BEGIN
    IF string # NIL AND Text.Length(string) >= 4 THEN
      no := Word.Insert(no, ORD(Text.GetChar(string, 0)), 24, 8);
      no := Word.Insert(no, ORD(Text.GetChar(string, 1)), 16, 8);
      no := Word.Insert(no, ORD(Text.GetChar(string, 2)), 8, 8);
      no := Word.Insert(no, ORD(Text.GetChar(string, 3)), 0, 8);
    END;
    RETURN no;
  END TextToNumber;


PROCEDURE NewEntry (names: T; name: TEXT): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    set  : CardSet.T;
    ok   : BOOLEAN;
    entry: CARDINAL;
  <* FATAL Database.NodeNotFound, Database.IndexUsed *>
  BEGIN
    TRY
      set := names.graph.getNodesWithIndex(NameIdx, name);
      IF set.card() > 0 THEN
        entry := set.extractAnyElement(ok);
      ELSE
        entry := names.graph.createNodeNumber(names.anchor);
        entry := names.graph.createNode(EntryLabel);
        names.graph.putIndex(entry, NameIdx, name);
        names.graph.createEdge(names.anchor, entry, CONTAINS);
      END;
      RETURN entry;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.NewEntry", "Database.InternalError", info));
    END;
  END NewEntry;


PROCEDURE NewLocalEntry (names: T; name: TEXT): CARDINAL
  RAISES {Access.Locked, InternalError, Database.NodeNotFound,
          Database.IndexUsed} =
  VAR
    set   : CardSet.T;
    ok    : BOOLEAN;
    entry : CARDINAL;
    lentry: CARDINAL;
  BEGIN
    TRY
      set := names.localgraph.getNodesWithIndex(NameIdx, name);
      IF set.card() > 0 THEN
        entry := set.extractAnyElement(ok);
      ELSE
        (*--- get the NUMBER from remote graph to be sure that it is unique
           ---*)
        entry := names.graph.createNodeNumber(names.anchor);
        entry := names.graph.createNode(EntryLabel);
        (*--- insert the new NUMBER into local graph ---*)
        lentry := names.localgraph.createNodeNumber(entry);
        lentry := names.localgraph.createNode(EntryLabel);
        <* ASSERT lentry = entry *>
        names.localgraph.putIndex(entry, NameIdx, name);
        names.localgraph.createEdge(names.anchor, entry, CONTAINS);
      END;
      RETURN entry;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.NewLocalEntry", "Database.InternalError", info));
    END;
  END NewLocalEntry;


PROCEDURE GetEntry (    names: T;
                        name : TEXT;
                        local: BOOLEAN;
                    VAR found: BOOLEAN  ): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    set  : CardSet.T;
    entry: CARDINAL;
  BEGIN
    TRY
      IF local THEN
        set := names.localgraph.getNodesWithIndex(NameIdx, name);
      ELSE
        set := names.graph.getNodesWithIndex(NameIdx, name);
      END;
      IF set.card() > 0 THEN entry := set.extractAnyElement(found); END;
      RETURN entry;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.GetEntry", "Database.InternalError", info));
    END;
  END GetEntry;


PROCEDURE GetRelLabel (names: T; local: BOOLEAN := FALSE; rel: CARDINAL):
  CARDINAL RAISES {Undeclared, Access.Locked, InternalError} =
  VAR lab: CARDINAL;
  BEGIN
    TRY
      lab := ReadNumber(names, local, rel, RelLabelAttr);
    EXCEPT
      Database.NodeNotFound => RAISE Undeclared;
    END;
    RETURN lab;
  END GetRelLabel;


PROCEDURE ReadNumber (names : T;
                      local : BOOLEAN;
                      node  : CARDINAL;
                      attrno: CARDINAL  ): CARDINAL
  RAISES {Access.Locked, Database.NodeNotFound, InternalError} =
  BEGIN
    TRY
      IF local THEN
        RETURN TextToNumber(names.localgraph.getAttribute(
                              node, attrno, NumberStart, NumberLength));
      ELSE
        RETURN TextToNumber(names.graph.getAttribute(
                              node, attrno, NumberStart, NumberLength));
      END;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.ReadNumber", "Database.InternalError", info));
    END;
  END ReadNumber;


PROCEDURE WriteNumber (names : T;
                       local : BOOLEAN;
                       node  : CARDINAL;
                       attrno: CARDINAL;
                       number: CARDINAL  )
  RAISES {Access.Locked, Database.NodeNotFound, InternalError} =
  BEGIN
    TRY
      IF NOT local THEN
        names.graph.putAttribute(
          node, attrno, NumberStart, NumberToText(number));
      ELSE
        names.localgraph.putAttribute(
          node, attrno, NumberStart, NumberToText(number));
      END;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.WriteNumber", "Database.InternalError", info));
    END;
  END WriteNumber;


PROCEDURE GetAttrNum (names: T; local: BOOLEAN; attr: CARDINAL): CARDINAL
  RAISES {Undeclared, Access.Locked, InternalError} =
  VAR no: CARDINAL;
  BEGIN
    TRY
      no := ReadNumber(names, local, attr, AttrNumAttr);
    EXCEPT
      Database.NodeNotFound => RAISE Undeclared;
    END;
    RETURN no;
  END GetAttrNum;


PROCEDURE GetCAttrNum (names: T; attr: CARDINAL): CARDINAL
  RAISES {Undeclared, Access.Locked, InternalError} =
  VAR no: CARDINAL;
  BEGIN
    TRY
      no := ReadNumber(names, FALSE (*local*), attr, CAttrNumAttr);
    EXCEPT
      Database.NodeNotFound => RAISE Undeclared;
    END;
    RETURN no;
  END GetCAttrNum;


PROCEDURE ReadAttribute (names : T;
                         local : BOOLEAN;
                         entry : CARDINAL;
                         attrno: CARDINAL  ): TEXT
  RAISES {Access.Locked, Database.NodeNotFound, InternalError} =
  VAR len: CARDINAL;
  BEGIN
    len := ReadNumber(names, local, entry, attrno);
    TRY
      IF len > 0 THEN
        IF local THEN
          RETURN
            names.localgraph.getAttribute(entry, attrno, ValueStart, len);
        ELSE
          RETURN names.graph.getAttribute(entry, attrno, ValueStart, len);
        END;
      END;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.ReadAttribute", "Database.InternalError", info));
    END;
    RETURN NIL;
  END ReadAttribute;


PROCEDURE WriteAttribute (names : T;
                          local : BOOLEAN;
                          entry : CARDINAL;
                          attrno: CARDINAL;
                          val   : TEXT      )
  RAISES {Access.Locked, Database.NodeNotFound, InternalError} =
  VAR len: CARDINAL := 0;
  BEGIN
    IF val # NIL THEN len := Text.Length(val); END;
    WriteNumber(names, local, entry, attrno, len);
    TRY
      IF len > 0 THEN
        IF local THEN
          names.localgraph.putAttribute(entry, attrno, ValueStart, val);
        ELSE
          names.graph.putAttribute(entry, attrno, ValueStart, val);
        END;
      END;
    EXCEPT
      Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.WriteAttribute", "Database.InternalError", info));
    END;
  END WriteAttribute;


(*----- Interface Procedures
   ------------------------------------------------*)

PROCEDURE Insert (names     : T;
                  name      : TEXT;
                  local     : BOOLEAN;
                  collection: CARDINAL  := All)
  RAISES {Undeclared, Access.Locked, InternalError} =
  VAR entry: CARDINAL;
  BEGIN
    TRY
      IF local THEN
        entry := NewLocalEntry(names, name);
        IF collection # All AND NOT names.localgraph.existsEdge(
                                      collection, entry, CONTAINS) THEN
          names.localgraph.createEdge(collection, entry, CONTAINS);
        END;
      ELSE
        entry := NewEntry(names, name);
        IF collection # All
             AND NOT names.graph.existsEdge(collection, entry, CONTAINS) THEN
          names.graph.createEdge(collection, entry, CONTAINS);
        END;
      END;                       (* IF local... *)
    EXCEPT
    | Database.NodeNotFound => RAISE Undeclared;
    | Database.IndexUsed =>      (* not implemented yet *)
    | Database.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Names.Insert", "Database.InternalError", info));
    END;
  END Insert;


PROCEDURE Remove (names     : T;
                  name      : TEXT;
                  local     : BOOLEAN;
                  collection: CARDINAL  := All)
  RAISES {Access.Locked, InternalError, Undeclared} =
  VAR
    entry : CARDINAL;
    exists: BOOLEAN;
  BEGIN
    TRY
      entry := GetEntry(names, name, local, exists);
      IF exists THEN
        IF local THEN
          IF collection = All THEN
            names.localgraph.deleteNodeNoInfo(entry);
            (* the node has to be deleted from remote graph, too *)
            names.graph.deleteNodeNoInfo(entry);
          ELSE
            IF names.localgraph.existsEdge(collection, entry, CONTAINS) THEN
              names.localgraph.deleteEdge(collection, entry, CONTAINS);
            END;
          END;                   (* IF collection # All... *)
        ELSE
          IF collection = All THEN
            names.graph.deleteNodeNoInfo(entry);
          ELSE
            IF names.graph.existsEdge(collection, entry, CONTAINS) THEN
              names.graph.deleteEdge(collection, entry, CONTAINS);
            END;
          END;                   (* IF collection # All... *)
        END;                     (* IF local... *)
      END;                       (* IF exists... *)
    EXCEPT
    | Database.NodeNotFound => RAISE Undeclared;
    | Database.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Names.Remove", "Database.InternalError", info));
    END;
  END Remove;


PROCEDURE Change (names: T; local: BOOLEAN; name: TEXT; new: TEXT)
  RAISES {Unknown, Access.Locked, InternalError} =
  VAR
    entry   : CARDINAL;
    exists  : BOOLEAN;
    oldindex: TEXT;
  <* FATAL Database.NodeNotFound *>
  <* FATAL Database.IndexUnused, Database.IndexUsed *>
  BEGIN
    TRY
      entry := GetEntry(names, name, FALSE (*local*), exists);
      IF exists THEN
        IF local THEN
          (*--- change name in local graph ---*)
          oldindex := names.localgraph.getIndex(entry, NameIdx, exists);
          IF exists THEN
            names.localgraph.deleteIndex(entry, NameIdx, oldindex);
            names.localgraph.putIndex(entry, NameIdx, new);
          ELSE
            RAISE InternalError(ErrorSupport.Create(
                                  "Name.Change",
                                  "Name index not found in local graph"));
          END;
        ELSE
          (*--- change name in remote graph ---*)
          oldindex := names.graph.getIndex(entry, NameIdx, exists);
          IF exists THEN
            names.graph.deleteIndex(entry, NameIdx, oldindex);
            names.graph.putIndex(entry, NameIdx, new);
          ELSE
            RAISE InternalError(ErrorSupport.Create(
                                  "Name.Change",
                                  "Name index not found in remote graph"));
          END;
        END;                     (* IF local... *)
      ELSE
        RAISE Unknown;
      END;                       (* IF exists... *)
    EXCEPT
    | Database.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Names.Change", "Database.InternalError", info));
    END;
  END Change;


PROCEDURE Id (names     : T;
              name      : TEXT;
              local     : BOOLEAN;
              collection: CARDINAL  := All): CARDINAL
  RAISES {Access.Locked, InternalError, Unknown, Undeclared} =
  VAR
    entry : CARDINAL;
    exists: BOOLEAN;
  BEGIN
    TRY
      entry := GetEntry(names, name, local, exists);
      IF exists THEN
        IF collection # All THEN
          (*--(mk)-- lookup in correct graph ---*)
          IF local THEN
            exists :=
              names.localgraph.existsEdge(collection, entry, CONTAINS);
          ELSE
            exists := names.graph.existsEdge(collection, entry, CONTAINS);
          END;
        END;
      END;
    EXCEPT
    | Database.NodeNotFound => RAISE Undeclared;
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.Contained", "Database.InternalError", info));
    END;
    IF exists THEN RETURN entry ELSE RAISE Unknown END;
  END Id;


PROCEDURE IsRemoteId (names: T; id: CARDINAL): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  VAR exists: BOOLEAN;
  <* FATAL Database.NodeNotFound *>
  BEGIN
    TRY
      IF names.graph.existsNode(id) THEN
        EVAL names.localgraph.getIndex(id, NameIdx, exists);
        RETURN exists;
      ELSE
        RETURN FALSE;
      END;
    EXCEPT
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.Contained", "Database.InternalError", info));
    END;
  END IsRemoteId;


PROCEDURE Name (names     : T;
                id        : CARDINAL;
                local     : BOOLEAN;
                collection: CARDINAL   := All): TEXT
  RAISES {Access.Locked, InternalError, Unknown, Undeclared} =
  VAR
    name  : TEXT;
    exists: BOOLEAN;
  BEGIN
    TRY
      IF local THEN
        (* check if node exists in local graph and has a name *)
        IF names.localgraph.existsNode(id) THEN
          name := names.localgraph.getIndex(id, NameIdx, exists);
          IF NOT exists THEN RAISE Unknown; END;
          (* now look whether node is in the correct collection *)
          IF collection # All THEN
            IF NOT names.localgraph.existsEdge(collection, id, CONTAINS) THEN
              RAISE Unknown;
            END;
          END;
          RETURN name;
        ELSE
          RAISE Unknown;
        END;
      ELSE
        (* check if node exists in remote graph and has a name *)
        IF names.graph.existsNode(id) THEN
          name := names.graph.getIndex(id, NameIdx, exists);
          IF NOT exists THEN RAISE Unknown; END;
          (* now look whether node is in the correct collection *)
          IF collection # All THEN
            IF NOT names.graph.existsEdge(collection, id, CONTAINS) THEN
              RAISE Unknown;
            END;
          END;
          RETURN name;
        ELSE
          RAISE Unknown;
        END;
      END;                       (* IF local... *)
    EXCEPT
    | Database.NodeNotFound => RAISE Undeclared;
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.Contained", "Database.InternalError", info));
    END;
  END Name;


PROCEDURE Contained (names     : T;
                     name      : TEXT;
                     local     : BOOLEAN;
                     collection: CARDINAL  := All): BOOLEAN
  RAISES {Access.Locked, InternalError, Undeclared} =
  VAR
    entry : CARDINAL;
    exists: BOOLEAN;
  BEGIN
    TRY
      entry := GetEntry(names, name, local, exists);
      IF exists THEN
        IF collection # All THEN
          IF local THEN
            exists :=
              names.localgraph.existsEdge(collection, entry, CONTAINS);
          ELSE
            exists := names.graph.existsEdge(collection, entry, CONTAINS);
          END;
        END;
      END;
    EXCEPT
    | Database.NodeNotFound => RAISE Undeclared;
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.Contained", "Database.InternalError", info));
    END;
    RETURN exists;
  END Contained;


PROCEDURE GetAllEntries (names     : T;
                         local     : BOOLEAN;
                         collection: CARDINAL  := All): TextCursorSet.T
  RAISES {Access.Locked, InternalError} =
  VAR
    set  : CardSet.T;
    ok   : BOOLEAN;
    entry: CARDINAL;
    res              := TextCursorSet.New();
  BEGIN
    IF collection = All THEN collection := names.anchor; END;
    TRY
      IF local THEN
        (*--- stuff for entries of local graph ---*)
        set := names.localgraph.getTargets(collection, CONTAINS);
        set.loop();
        entry := set.get(ok);
        WHILE ok DO
          res.insert(names.localgraph.getIndex(entry, NameIdx, ok));
          entry := set.get(ok);
        END;
        set.dispose();
      ELSE
        (*--- stuff for entries of remote graph ---*)
        set := names.graph.getTargets(collection, CONTAINS);
        set.loop();
        entry := set.get(ok);
        WHILE ok DO
          res.insert(names.graph.getIndex(entry, NameIdx, ok));
          entry := set.get(ok);
        END;
        set.dispose();
      END;                       (* IF local... *)
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.GetAllEntries",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.GetAllEntries", "Database.InternalError", info));
    END;
    RETURN res;
  END GetAllEntries;


PROCEDURE DeclareCollection (names: T; cname: TEXT): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    subanchor : CARDINAL;
    lsubanchor: CARDINAL;
    set       : CardSet.T;
    ok        : BOOLEAN;
  <* FATAL Database.IndexUsed *>
  BEGIN
    TRY
      set := names.graph.getNodesWithIndex(CollIdx, cname);
      IF set.card() > 0 THEN
        subanchor := set.extractAnyElement(ok);
      ELSE
        (*--- code for remote graph ---*)
        subanchor := names.graph.createNodeNumber(names.anchor);
        subanchor := names.graph.createNode(CollLabel);
        names.graph.putIndex(subanchor, CollIdx, cname);
        names.graph.createEdge(names.anchor, subanchor, SUBCOLLECTION);

        (*--- code for local graph ---*)
        lsubanchor := names.localgraph.createNodeNumber(subanchor);
        lsubanchor := names.localgraph.createNode(CollLabel);
        <* ASSERT subanchor = lsubanchor *>
        names.localgraph.putIndex(subanchor, CollIdx, cname);
        names.localgraph.createEdge(names.anchor, subanchor, SUBCOLLECTION);
      END;
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.DeclareCollection",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Names.DeclareCollection",
                                       "Database.InternalError", info));
    END;
    RETURN subanchor;
  END DeclareCollection;


PROCEDURE GetAllCollections (names: T): TextIdSet.T
  RAISES {Access.Locked, InternalError} =
  VAR
    cset: CardSet.T;
    coll: CARDINAL;
    ok  : BOOLEAN;
    res : TextIdSet.T;
  BEGIN
    res := TextIdSet.New();
    TRY
      cset := names.graph.getTargets(names.anchor, SUBCOLLECTION);
      cset.loop();
      coll := cset.get(ok);
      WHILE ok DO
        res.insert(
          TextId.T{
            text := names.graph.getIndex(coll, CollIdx, ok), id := coll});
        coll := cset.get(ok);
      END;
      cset.dispose();
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.GetAllCollections",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Names.GetAllCollections",
                                       "Database.InternalError", info));
    END;
    RETURN res;
  END GetAllCollections;


PROCEDURE DeclareCollectionAttribute (names: T; attrname: TEXT): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    node, num: CARDINAL;
    lnode    : CARDINAL;
    set      : CardSet.T;
    ok       : BOOLEAN;
  <* FATAL Database.IndexUsed *>
  BEGIN
    TRY
      set := names.graph.getNodesWithIndex(CAttrIdx, attrname);
      IF set.card() > 0 THEN
        node := set.extractAnyElement(ok);
      ELSE
        (*--- put in remote graph ---*)
        node := names.graph.createNodeNumber(names.anchor);
        node := names.graph.createNode(CAttrLabel);
        names.graph.putIndex(node, CAttrIdx, attrname);
        names.graph.createEdge(names.anchor, node, COLLATTRIBUTE);

        (*--- put in local graph ---*)
        lnode := names.localgraph.createNodeNumber(node);
        lnode := names.localgraph.createNode(CAttrLabel);
        <* ASSERT lnode = node *>
        names.localgraph.putIndex(node, CAttrIdx, attrname);
        names.localgraph.createEdge(names.anchor, node, COLLATTRIBUTE);

        num := ReadNumber(names, FALSE (*local*), names.anchor, FreeCAttr);
        WriteNumber(names, FALSE (* local *), node, CAttrNumAttr, num);
        WriteNumber(
          names, FALSE (* local *), names.anchor, FreeCAttr, num + 1);

        num := ReadNumber(names, TRUE (*local*), names.anchor, FreeCAttr);
        WriteNumber(names, TRUE (* local *), node, CAttrNumAttr, num);
        WriteNumber(
          names, TRUE (* local *), names.anchor, FreeCAttr, num + 1);
      END;
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("Names.DeclareCollectionAttribute",
                                    "Databases.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Names.DeclareCollectionAttribute",
                                       "Database.InternalError", info));
    END;
    RETURN node;
  END DeclareCollectionAttribute;


PROCEDURE SetCollectionAttribute (names: T;
                                  attr : CARDINAL;
                                  value: TEXT;
                                  coll : CARDINAL   := All)
  RAISES {Access.Locked, InternalError, Undeclared} =
  VAR attrno: CARDINAL;
  BEGIN
    TRY
      IF coll = All THEN coll := names.anchor END;
      attrno := GetCAttrNum(names, attr);
      WriteAttribute(names, FALSE (* local *), coll, attrno, value);
      WriteAttribute(names, TRUE (* local *), coll, attrno, value);
    EXCEPT
      Database.NodeNotFound =>
        RAISE
          InternalError(ErrorSupport.Create("Names.SetCollectionAttribute",
                                            "Database.NodeNotFound"));
    END;
  END SetCollectionAttribute;


PROCEDURE GetCollectionAttribute (names: T;
                                  attr : CARDINAL;
                                  coll : CARDINAL   := All): TEXT
  RAISES {Access.Locked, InternalError, Undeclared} =
  VAR
    res   : TEXT;
    attrno: CARDINAL;
  BEGIN
    TRY
      IF coll = All THEN coll := names.anchor END;
      attrno := GetCAttrNum(names, attr);
      res := ReadAttribute(names, FALSE (*local*), coll, attrno);
    EXCEPT
      Database.NodeNotFound =>
        RAISE
          InternalError(ErrorSupport.Create("Names.GetCollectionAttribute",
                                            "Database.NodeNotFound"));
    END;
    RETURN res;
  END GetCollectionAttribute;


PROCEDURE GetAllDeclCollectionAttributes (names: T): TextIdSet.T
  RAISES {Access.Locked, InternalError} =
  VAR
    caset: CardSet.T;
    cattr: CARDINAL;
    ok   : BOOLEAN;
    res  : TextIdSet.T;
  BEGIN
    res := TextIdSet.New();
    TRY
      caset := names.graph.getTargets(names.anchor, COLLATTRIBUTE);
      caset.loop();
      cattr := caset.get(ok);
      WHILE ok DO
        res.insert(TextId.T{
                     text := names.graph.getIndex(cattr, CAttrIdx, ok),
                     id := cattr});
        cattr := caset.get(ok);
      END;
      caset.dispose();
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.GetAllCollections",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Names.GetAllCollections",
                                       "Database.InternalError", info));
    END;
    RETURN res;
  END GetAllDeclCollectionAttributes;


PROCEDURE DeclareAttribute (names: T; aname: TEXT): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    node, num: CARDINAL;
    lnode    : CARDINAL;
    set      : CardSet.T;
    ok       : BOOLEAN;
  <* FATAL Database.IndexUsed *>
  BEGIN
    TRY
      set := names.graph.getNodesWithIndex(AttrIdx, aname);
      IF set.card() > 0 THEN
        node := set.extractAnyElement(ok);
      ELSE
        (*--- code for remote graph ---*)
        node := names.graph.createNodeNumber(names.anchor);
        node := names.graph.createNode(AttrLabel);
        names.graph.putIndex(node, AttrIdx, aname);
        names.graph.createEdge(names.anchor, node, ATTRIBUTE);

        num :=
          ReadNumber(names, FALSE (*local*), names.anchor, FreeAttrAttr);
        WriteNumber(names, FALSE (* local *), node, AttrNumAttr, num);
        WriteNumber(
          names, FALSE (* local *), names.anchor, FreeAttrAttr, num + 1);

        (*--- code for local graph ---*)
        lnode := names.localgraph.createNodeNumber(node);
        lnode := names.localgraph.createNode(AttrLabel);
        <* ASSERT lnode = node *>
        names.localgraph.putIndex(node, AttrIdx, aname);
        names.localgraph.createEdge(names.anchor, node, ATTRIBUTE);

        num :=
          ReadNumber(names, TRUE (*local*), names.anchor, FreeAttrAttr);
        WriteNumber(names, TRUE (* local *), node, AttrNumAttr, num);
        WriteNumber(
          names, TRUE (* local *), names.anchor, FreeAttrAttr, num + 1);
      END;                       (* IF set.card()... *)
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.DeclareAttribute",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Names.DeclareAttribute",
                              "Database.InternalError", info));
    END;
    RETURN node;
  END DeclareAttribute;


PROCEDURE SetAttribute (names: T;
                        name : TEXT;
                        local: BOOLEAN;
                        attr : CARDINAL;
                        value: TEXT      )
  RAISES {Access.Locked, InternalError, Undeclared, Unknown} =
  VAR
    attrno: CARDINAL;
    entry : CARDINAL;
    exists: BOOLEAN;
  BEGIN
    TRY
      attrno := GetAttrNum(names, local, attr);
      entry := GetEntry(names, name, local, exists);
      IF exists THEN
        WriteAttribute(names, local, entry, attrno, value);
      ELSE
        RAISE Unknown;
      END;
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.SetAttribute",
                                                "Database.NodeNotFound"));
    END;
  END SetAttribute;


PROCEDURE GetAttribute (names: T;
                        name : TEXT;
                        local: BOOLEAN;
                        attr : CARDINAL ): TEXT
  RAISES {Access.Locked, InternalError, Undeclared, Unknown} =
  VAR
    attrno: CARDINAL;
    entry : CARDINAL;
    exists: BOOLEAN;
    res   : TEXT;
  BEGIN
    TRY
      attrno := GetAttrNum(names, local, attr);
      entry := GetEntry(names, name, local, exists);
      IF exists THEN
        res := ReadAttribute(names, local, entry, attrno);
      ELSE
        RAISE Unknown;
      END;
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.GetAttribute",
                                                "Database.NodeNotFound"));
    END;
    RETURN res;
  END GetAttribute;


PROCEDURE GetAllDeclAttributes (names: T): TextIdSet.T
  RAISES {Access.Locked, InternalError} =
  VAR
    aset: CardSet.T;
    attr: CARDINAL;
    ok  : BOOLEAN;
    res : TextIdSet.T;
  BEGIN
    res := TextIdSet.New();
    TRY
      aset := names.graph.getTargets(names.anchor, ATTRIBUTE);
      aset.loop();
      attr := aset.get(ok);
      WHILE ok DO
        res.insert(
          TextId.T{
            text := names.graph.getIndex(attr, AttrIdx, ok), id := attr});
        attr := aset.get(ok);
      END;
      aset.dispose();
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.GetAllCollections",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Names.GetAllCollections",
                                       "Database.InternalError", info));
    END;
    RETURN res;
  END GetAllDeclAttributes;


PROCEDURE DeclareRelation (names: T; rname: TEXT): CARDINAL
  RAISES {Access.Locked, InternalError} =
  VAR
    relation : CARDINAL;
    lrelation: CARDINAL;
    rellabel : CARDINAL;
    set      : CardSet.T;
    ok       : BOOLEAN;
  <* FATAL Database.IndexUsed *>
  BEGIN
    TRY
      set := names.graph.getNodesWithIndex(RelIdx, rname);
      IF set.card() > 0 THEN
        relation := set.extractAnyElement(ok);
        rellabel :=
          TextToNumber(
            names.graph.getAttribute(
              relation, RelLabelAttr, NumberStart, NumberLength));
      ELSE
        (*--- put in remote graph ---*)
        relation := names.graph.createNodeNumber(names.anchor);
        relation := names.graph.createNode(RelLabel);
        names.graph.putIndex(relation, RelIdx, rname);
        names.graph.createEdge(names.anchor, relation, RELATION);

        (*--- put in local graph ---*)
        lrelation := names.localgraph.createNodeNumber(relation);
        lrelation := names.localgraph.createNode(RelLabel);
        <* ASSERT lrelation = relation *>
        names.localgraph.putIndex(relation, RelIdx, rname);
        names.localgraph.createEdge(names.anchor, relation, RELATION);

        rellabel :=
          ReadNumber(names, FALSE (*local*), names.anchor, FreeRelAttr);
        WriteNumber(
          names, FALSE (*local*), relation, RelLabelAttr, rellabel);
        WriteNumber(
          names, TRUE (* local *), relation, RelLabelAttr, rellabel);
        WriteNumber(
          names, FALSE (*local*), names.anchor, FreeRelAttr, rellabel + 1);
      END;
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.DeclareRelation",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "Names.DeclareRelation", "Database.InternalError", info));
    END;
    RETURN relation;
  END DeclareRelation;


PROCEDURE GetAllRelations (names: T): TextIdSet.T
  RAISES {Access.Locked, InternalError} =
  VAR
    rset: CardSet.T;
    rel : CARDINAL;
    ok  : BOOLEAN;
    res : TextIdSet.T;
  BEGIN
    res := TextIdSet.New();
    TRY
      rset := names.graph.getTargets(names.anchor, RELATION);
      rset.loop();
      rel := rset.get(ok);
      WHILE ok DO
        res.insert(TextId.T{text := names.graph.getIndex(rel, RelIdx, ok),
                            id := rel});
        rel := rset.get(ok);
      END;
      rset.dispose();
    EXCEPT
      Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.GetAllRelations",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "Names.GetAllRelations", "Database.InternalError", info));
    END;
    RETURN res;
  END GetAllRelations;


PROCEDURE InsertInRelation (names: T;
                            n1   : TEXT;
                            n2   : TEXT;
                            local: BOOLEAN;
                            rel  : CARDINAL )
  RAISES {Access.Locked, Undeclared, Unknown, InternalError} =
  VAR
    lab, e1, e2: CARDINAL;
    ok1, ok2   : BOOLEAN;
  BEGIN
    TRY
      lab := GetRelLabel(names, local, rel);
      e1 := GetEntry(names, n1, local, ok1);
      e2 := GetEntry(names, n2, local, ok2);

      IF local THEN
        IF ok1 AND ok2 THEN
          IF NOT names.localgraph.existsEdge(e1, e2, lab) THEN
            names.localgraph.createEdge(e1, e2, lab);
          END;
        ELSE
          RAISE Unknown;
        END;
      ELSE
        IF ok1 AND ok2 THEN
          IF NOT names.graph.existsEdge(e1, e2, lab) THEN
            names.graph.createEdge(e1, e2, lab);
          END;
        ELSE
          RAISE Unknown;
        END;
      END;                       (* IF local *)
    EXCEPT
      Undeclared => RAISE Undeclared;
    | Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.InsertInRelation",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Names.InsertInRelation",
                              "Database.InternalError", info));
    END;
  END InsertInRelation;


PROCEDURE RemoveFromRelation (names: T;
                              n1   : TEXT;
                              n2   : TEXT;
                              local: BOOLEAN;
                              rel  : CARDINAL )
  RAISES {Access.Locked, InternalError, Undeclared} =
  VAR
    lab, e1, e2: CARDINAL;
    ok1, ok2   : BOOLEAN;
  BEGIN
    TRY
      lab := GetRelLabel(names, local, rel);
      e1 := GetEntry(names, n1, local, ok1);
      e2 := GetEntry(names, n2, local, ok2);

      IF local THEN
        IF ok1 AND ok2 AND names.localgraph.existsEdge(e1, e2, lab) THEN
          names.localgraph.deleteEdge(e1, e2, lab);
        END;
      ELSE
        IF ok1 AND ok2 AND names.graph.existsEdge(e1, e2, lab) THEN
          names.graph.deleteEdge(e1, e2, lab);
        END;
      END;                       (* IF local *)
    EXCEPT
      Undeclared => RAISE Undeclared;
    | Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create("Names.RemoveFromRelation",
                                                "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Names.RemoveFromRelation",
                                       "Database.InternalError", info));
    END;
  END RemoveFromRelation;


PROCEDURE Targets (names: T; n1: TEXT; local: BOOLEAN; rel: CARDINAL):
  TextCursorSet.T
  RAISES {Undeclared, Unknown, Access.Locked, InternalError} =
  VAR
    t                      := TextCursorSet.New();
    set        : CardSet.T;
    e1, e2, lab: CARDINAL;
    ok         : BOOLEAN;
  BEGIN
    TRY
      lab := GetRelLabel(names, local, rel);
      e1 := GetEntry(names, n1, local, ok);
      IF ok THEN
        IF local THEN
          set := names.localgraph.getTargets(e1, lab);
          set.loop();
          e2 := set.get(ok);
          WHILE ok DO
            t.insert(names.localgraph.getIndex(e2, NameIdx, ok));
            e2 := set.get(ok);
          END;
          set.dispose();
        ELSE
          set := names.graph.getTargets(e1, lab);
          set.loop();
          e2 := set.get(ok);
          WHILE ok DO
            t.insert(names.graph.getIndex(e2, NameIdx, ok));
            e2 := set.get(ok);
          END;
          set.dispose();
        END;                     (* IF local *)
      ELSE
        RAISE Unknown;
      END;                       (* IF ok *)
    EXCEPT
    | Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "Names.Targets", "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.Targets", "Database.InternalError", info));
    END;
    RETURN t;
  END Targets;


PROCEDURE Sources (names: T; n2: TEXT; local: BOOLEAN; rel: CARDINAL):
  TextCursorSet.T
  RAISES {Undeclared, Unknown, Access.Locked, InternalError} =
  VAR
    s                      := TextCursorSet.New();
    set        : CardSet.T;
    e1, e2, lab: CARDINAL;
    ok         : BOOLEAN;
  BEGIN
    TRY
      lab := GetRelLabel(names, local, rel);
      e2 := GetEntry(names, n2, local, ok);
      IF ok THEN
        IF local THEN
          set := names.localgraph.getSources(e2, lab);
          set.loop();
          e1 := set.get(ok);
          WHILE ok DO
            s.insert(names.localgraph.getIndex(e1, NameIdx, ok));
            e1 := set.get(ok);
          END;
          set.dispose();
        ELSE
          set := names.graph.getSources(e2, lab);
          set.loop();
          e1 := set.get(ok);
          WHILE ok DO
            s.insert(names.graph.getIndex(e1, NameIdx, ok));
            e1 := set.get(ok);
          END;
          set.dispose();
        END;                     (* IF local *)
      ELSE
        RAISE Unknown;
      END;                       (* IF ok *)
    EXCEPT
    | Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "Names.Sources", "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.Sources", "Database.InternalError", info));
    END;
    RETURN s;
  END Sources;


PROCEDURE Related (names: T;
                   local: BOOLEAN;
                   n1   : TEXT;
                   n2   : TEXT;
                   rel  : CARDINAL ): BOOLEAN
  RAISES {Undeclared, Unknown, Access.Locked, InternalError} =
  VAR
    lab, e1, e2  : CARDINAL;
    res, ok1, ok2: BOOLEAN;
  BEGIN
    TRY
      lab := GetRelLabel(names, local, rel);
      e1 := GetEntry(names, n1, local, ok1);
      e2 := GetEntry(names, n2, local, ok2);
      IF ok1 AND ok2 THEN
        IF local THEN
          res := names.localgraph.existsEdge(e1, e2, lab);
        ELSE
          res := names.graph.existsEdge(e1, e2, lab);
        END;
      ELSE
        RAISE Unknown;
      END;
    EXCEPT
    | Database.NodeNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "Names.Related", "Database.NodeNotFound"));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.Related", "Database.InternalError", info));
    END;
    RETURN res;
  END Related;


PROCEDURE Login (names: T; res: VirtualResource.T; name: Pathname.T)
  RAISES {Access.Locked, InternalError} =
  <* FATAL Database.IndexUsed, Database.NodeNotFound *>

  VAR lanchor: CARDINAL;

  BEGIN
    IF names.loggedIn THEN RETURN END;

    names.graph := NIL;
    names.localgraph := NIL;

    TRY
      (*--- first try opening for remote graph ------------------*)
      IF res.existsFile(name, FALSE (*local*)) THEN
        names.graph := NEW(Database.T).open(
                         res, name, res.getAccessMode(), FALSE (*new*),
                         FALSE (*local*), TRUE (*errorChecks*));
        VAR
          aset: CardSet.T;
          ok  : BOOLEAN;
        BEGIN
          aset := names.graph.getNodesWithIndex(CollIdx, name);
          IF NOT aset.card() = 1 THEN
            RAISE InternalError(ErrorSupport.Create(
                                  "Names.Login", "Not a collection."));
          END;
          names.anchor := aset.extractAnyElement(ok);
        END;
      ELSE
        names.graph := NEW(Database.T).open(
                         res, name, res.getAccessMode(), TRUE (*new*),
                         FALSE (*local*), TRUE (*errorChecks*));
        names.anchor := names.graph.createNodeNumber(1);
        names.anchor := names.graph.createNode(AnchorLabel);
        names.graph.putIndex(names.anchor, CollIdx, name);
        names.graph.putAttribute(names.anchor, FreeRelAttr, NumberStart,
                                 NumberToText(FirstFreeLabel));
        names.graph.putAttribute(names.anchor, FreeAttrAttr, NumberStart,
                                 NumberToText(FirstFreeAttr));
        names.graph.putAttribute(names.anchor, FreeCAttr, NumberStart,
                                 NumberToText(FirstFreeCAttr));
      END;

      (*--- then try opening for local graph ----------------*)
      IF res.existsFile(name, TRUE (*local*)) THEN
        names.localgraph :=
          NEW(Database.T).open(
            res, name, res.getAccessMode(), FALSE (*new*), TRUE (*local*),
            TRUE (*errorChecks*));
        VAR
          aset: CardSet.T;
          ok  : BOOLEAN;
        BEGIN
          aset := names.localgraph.getNodesWithIndex(CollIdx, name);
          IF NOT aset.card() = 1 THEN
            RAISE InternalError(ErrorSupport.Create(
                                  "Names.Login", "Not a collection."));
          END;
          names.anchor := aset.extractAnyElement(ok);
        END;
      ELSE
        names.localgraph := NEW(Database.T).open(
                              res, name, res.getAccessMode(), TRUE (*new*),
                              TRUE (*local*), TRUE (*errorChecks*));
        lanchor := names.localgraph.createNodeNumber(names.anchor);
        lanchor := names.localgraph.createNode(AnchorLabel);

        <* ASSERT lanchor = names.anchor *>

        names.localgraph.putIndex(names.anchor, CollIdx, name);
        names.localgraph.putAttribute(
          names.anchor, FreeRelAttr, NumberStart,
          NumberToText(FirstFreeLabel));
        names.localgraph.putAttribute(
          names.anchor, FreeAttrAttr, NumberStart,
          NumberToText(FirstFreeAttr));
        names.localgraph.putAttribute(names.anchor, FreeCAttr, NumberStart,
                                      NumberToText(FirstFreeCAttr));
      END;

      names.loggedIn := TRUE;

    EXCEPT
      PageFile.NoAccess (msg) =>
        RAISE InternalError(ErrorSupport.Create(
                              "Names.Login", "PageFile.NoAccess: " & msg));
    | VirtualResource.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Names.Login", "VirtualResource.FatalError", info));
    | Database.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Names.Login", "Database.InternalError", info));
    | Access.Denied (msg) =>
        RAISE InternalError(ErrorSupport.Create(
                              "Names.Login", "Access.Denied: " & msg));
    END;
  END Login;


PROCEDURE Logout (names: T) =
  BEGIN
    TRY
      IF names.loggedIn THEN
        names.graph.close();
        names.localgraph.close();
      END;
      names.loggedIn := FALSE;
    EXCEPT
      Database.InternalError =>  (* ignore *)
    END;
  END Logout;

BEGIN
END Names.
