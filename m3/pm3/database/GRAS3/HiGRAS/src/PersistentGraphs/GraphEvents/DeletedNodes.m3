MODULE DeletedNodes;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.1  1997/11/12 15:23:49  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

IMPORT Node, Transaction;

REVEAL
  T = Public BRANDED OBJECT
        nodes   : NodeTable;
        levels  : LevelTable;
        maxLevel: Transaction.Level;
        empty   : BOOLEAN;
      OVERRIDES
        init            := Init;
        insert          := Insert;
        clear           := Clear;
        killTransaction := KillTransaction;
        invalid         := Invalid;
      END;

PROCEDURE Init (dn: T): T =
  BEGIN
    dn.nodes := NodeTable{NIL, ..};
    dn.levels := LevelTable{NIL, ..};
    dn.maxLevel := 0;
    dn.empty := TRUE;
    RETURN dn;
  END Init;

PROCEDURE Insert (dn       : T;
                  node     : Node.T;
                  level    : Transaction.Level;
                  timestamp: CARDINAL           ) =
  VAR info: REF DeletedNodeInfo;
  BEGIN
    info := NewDeletedNodeInfo();
    info.node := node;
    info.level := level;
    info.timestamp := timestamp;
    NodeTableInsert(dn.nodes, info);
    LevelTableInsert(dn.levels, info);
    dn.maxLevel := MAX(dn.maxLevel, level);
    dn.empty := FALSE;
  END Insert;

PROCEDURE Clear (dn: T) =
  VAR info: REF DeletedNodeInfo;
  BEGIN
    IF NOT dn.empty THEN
      FOR l := FIRST(LevelTableIndex) TO LAST(LevelTableIndex) DO
        WHILE dn.levels[l] # NIL DO
          info := dn.levels[l];
          ListRemove(Key.Level, dn.levels[l], info);
          NodeTableRemove(dn.nodes, info);
          DisposeDeletedNodeInfo(info);
        END;
      END;
      dn.maxLevel := Transaction.EnvelopeLevel;
      dn.empty := TRUE;
    END;
  END Clear;

PROCEDURE KillTransaction (dn: T; level: Transaction.Level) =
  VAR info: REF DeletedNodeInfo;
  BEGIN
    FOR lev := dn.maxLevel TO level BY -1 DO
      WHILE LevelTableFind(dn.levels, lev, info) DO
        LevelTableRemove(dn.levels, info);
        NodeTableRemove(dn.nodes, info);
        DisposeDeletedNodeInfo(info);
      END;
    END;
    dn.maxLevel := MAX(Transaction.EnvelopeLevel, level - 1);
  END KillTransaction;

PROCEDURE Invalid (dn: T; node: Node.T; timestamp: CARDINAL): BOOLEAN =
  BEGIN
    RETURN NodeTableFind(dn.nodes, node, timestamp);
  END Invalid;


TYPE
  Key = {Node, Level};
  RefPair = ARRAY Key OF REF DeletedNodeInfo;

  DeletedNodeInfo = RECORD
                      node      : Node.T;
                      level     : Transaction.Level;
                      timestamp : CARDINAL;
                      next, prev: RefPair;
                    END;

CONST NodeTableSize = 53;

TYPE
  NodeTableIndex = [0 .. NodeTableSize - 1];
  NodeTable = ARRAY NodeTableIndex OF REF DeletedNodeInfo;

PROCEDURE NodeHash (node: Node.T): NodeTableIndex =
  BEGIN
    RETURN (node.graph * 11 + node.entity) MOD NodeTableSize;
  END NodeHash;

PROCEDURE NodeLess (a, b: REF DeletedNodeInfo): BOOLEAN =
  BEGIN
    WITH c = Node.Compare(a.node, b.node) DO
      IF c < 0 THEN
        RETURN TRUE;
      ELSIF c > 0 THEN
        RETURN FALSE;
      ELSE
        RETURN a.timestamp < b.timestamp;
      END;
    END;
  END NodeLess;

PROCEDURE NodeTableInsert (VAR tab: NodeTable; dni: REF DeletedNodeInfo) =
  VAR h: NodeTableIndex;
  BEGIN
    h := NodeHash(dni.node);
    ListInsert(Key.Node, tab[h], dni, NodeLess);
  END NodeTableInsert;

PROCEDURE NodeTableRemove (VAR tab: NodeTable; dni: REF DeletedNodeInfo) =
  VAR h: NodeTableIndex;
  BEGIN
    h := NodeHash(dni.node);
    ListRemove(Key.Node, tab[h], dni);
  END NodeTableRemove;

PROCEDURE NodeTableFind (READONLY tab : NodeTable;
                                  node: Node.T;
                                  time: CARDINAL   ): BOOLEAN =
  VAR
    h  : NodeTableIndex;
    act: REF DeletedNodeInfo;
  BEGIN
    h := NodeHash(node);
    act := tab[h];
    WHILE act # NIL AND Node.Compare(act.node, node) >= 0 DO
      IF act.node = node AND act.timestamp < time THEN RETURN TRUE; END;
    END;
    RETURN FALSE;
  END NodeTableFind;

CONST LevelTableSize = 53;

TYPE
  LevelTableIndex = [0 .. LevelTableSize - 1];
  LevelTable = ARRAY LevelTableIndex OF REF DeletedNodeInfo;

PROCEDURE LevelHash (level: Transaction.Level): LevelTableIndex =
  BEGIN
    RETURN level MOD LevelTableSize;
  END LevelHash;

PROCEDURE LevelLess (a, b: REF DeletedNodeInfo): BOOLEAN =
  BEGIN
    RETURN a.level < b.level;
  END LevelLess;

PROCEDURE LevelTableInsert (VAR tab: LevelTable; dni: REF DeletedNodeInfo) =
  VAR h: LevelTableIndex;
  BEGIN
    h := LevelHash(dni.level);
    ListInsert(Key.Level, tab[h], dni, LevelLess);
  END LevelTableInsert;

PROCEDURE LevelTableRemove (VAR tab: LevelTable; dni: REF DeletedNodeInfo) =
  VAR h: LevelTableIndex;
  BEGIN
    h := LevelHash(dni.level);
    ListRemove(Key.Level, tab[h], dni);
  END LevelTableRemove;

PROCEDURE LevelTableFind (READONLY tab  : LevelTable;
                                   level: Transaction.Level;
                          VAR      info : REF DeletedNodeInfo): BOOLEAN =
  VAR h: LevelTableIndex;
  BEGIN
    h := LevelHash(level);
    info := tab[h];
    WHILE info # NIL AND info.level >= level DO
      IF info.level = level THEN RETURN TRUE; END;
    END;
    RETURN FALSE;
  END LevelTableFind;


TYPE
  LessProc = PROCEDURE (a, b: REF DeletedNodeInfo): BOOLEAN;
  List = REF DeletedNodeInfo;

PROCEDURE ListInsert (    key : Key;
                      VAR list: List;
                          elem: REF DeletedNodeInfo;
                          less: LessProc             ) =
  VAR h, p: REF DeletedNodeInfo;
  BEGIN
    IF list = NIL OR less(elem, list) THEN
      (* insert as first element *)
      elem.next[key] := list;
      elem.prev[key] := NIL;
      IF list # NIL THEN list.prev[key] := elem; END;
    ELSE
      (* find position to insert *)
      p := list;
      h := list.next[key];
      WHILE h # NIL AND less(h, elem) DO p := h; h := h.next[key]; END;
      p.next[key] := elem;
      elem.next[key] := h;
      elem.prev[key] := p;
      IF h # NIL THEN h.prev[key] := elem; END;
    END;
  END ListInsert;

PROCEDURE ListRemove (key: Key; VAR list: List; elem: REF DeletedNodeInfo) =
  BEGIN
    IF elem.prev[key] = NIL THEN
      list := elem.next[key];
    ELSE
      elem.prev[key].next[key] := elem.next[key];
    END;
    IF elem.next[key] # NIL THEN
      elem.next[key].prev[key] := elem.prev[key];
    END;
  END ListRemove;

VAR FreeInfos: REF DeletedNodeInfo := NIL;

PROCEDURE NewDeletedNodeInfo (): REF DeletedNodeInfo =
  VAR x: REF DeletedNodeInfo;
  BEGIN
    IF FreeInfos = NIL THEN
      RETURN NEW(REF DeletedNodeInfo);
    ELSE
      x := FreeInfos;
      FreeInfos := FreeInfos.next[Key.Node];
      x.next[Key.Node] := NIL;
      RETURN x;
    END;
  END NewDeletedNodeInfo;

PROCEDURE DisposeDeletedNodeInfo (dni: REF DeletedNodeInfo) =
  BEGIN
    dni^ := DeletedNodeInfo{
              Node.T{0, 0}, 0, 0, RefPair{NIL, NIL}, RefPair{NIL, NIL}};
    dni.next[Key.Node] := FreeInfos;
    FreeInfos := dni;
  END DisposeDeletedNodeInfo;

BEGIN
END DeletedNodes.
