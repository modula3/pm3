MODULE RemoteTriggerStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:05:13  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT Action;

VAR
  Table  := TriggerTable{NIL, ..};
  Access := NEW(MUTEX);

PROCEDURE Store (id       : CARDINAL;
                 eventType: TEXT;
                 priority : CARDINAL;
                 action   : Action.T;
                 userdata : REFANY) =
  VAR
    elem: REF ListElement;
    hash: CARDINAL;
  BEGIN
    LOCK Access DO
      elem := NewListElement();
      elem^ := ListElement{id, eventType, priority, action, userdata, NIL, NIL};
      hash := Hash(id);
      ListInsert(Table[hash], elem);
    END;
  END Store;

PROCEDURE Remove (id: CARDINAL) =
  VAR
    found: BOOLEAN;
    elem : REF ListElement;
    hash : CARDINAL;
  BEGIN
    LOCK Access DO
      hash := Hash(id);
      ListFind(Table[hash], id, elem, found);
      IF found THEN
        ListRemove(Table[hash], elem);
        DisposeListElement(elem);
      END;
    END;
  END Remove;

PROCEDURE Find (    id      : CARDINAL;
                VAR type    : TEXT;
                VAR priority: CARDINAL;
                VAR action  : Action.T;
                VAR userdata: REFANY): BOOLEAN =
  VAR
    found: BOOLEAN;
    elem : REF ListElement;
    hash : CARDINAL;
  BEGIN
    LOCK Access DO
      hash := Hash(id);
      ListFind(Table[hash], id, elem, found);
      IF found THEN
        type := elem.type;
        priority := elem.priority;
        action := elem.action;
        userdata := elem.userdata;
      END;
    END;
    RETURN found;
  END Find;

CONST TableSize = 53;

TYPE
  ListElement = RECORD
                  id        : CARDINAL;
                  type      : TEXT;
                  priority  : CARDINAL;
                  action    : Action.T;
                  userdata  : REFANY;
                  next, prev: REF ListElement;
                END;

  TableIndex = [0 .. TableSize - 1];
  TriggerTable = ARRAY TableIndex OF REF ListElement;

PROCEDURE Hash (type: CARDINAL): TableIndex =
  BEGIN
    RETURN type MOD TableSize;
  END Hash;

VAR FreeListElements: REF ListElement := NIL;

PROCEDURE NewListElement (): REF ListElement =
  VAR new: REF ListElement;
  BEGIN
    IF FreeListElements = NIL THEN
      RETURN NEW(REF ListElement);
    ELSE
      new := FreeListElements;
      FreeListElements := new.next;
      RETURN new;
    END;
  END NewListElement;

PROCEDURE DisposeListElement (elem: REF ListElement) =
  BEGIN
    elem^ := ListElement{0, NIL, 0, NIL, NIL, NIL, NIL};
    elem.next := FreeListElements;
    FreeListElements := elem;
  END DisposeListElement;

PROCEDURE ListInsert (VAR list: REF ListElement; new: REF ListElement) =
  VAR p: REF ListElement;
  BEGIN
    IF list = NIL OR list.id >= new.id THEN
      (* empty list or trigger preceeds/equals first element *)
      new.next := list;
      list := new;
      IF new.next # NIL THEN new.next.prev := new; END;
    ELSE
      p := list;
      WHILE p.next # NIL AND p.next.id < new.id DO p := p.next; END;
      new.next := p.next;
      new.prev := p;
      p.next := new;
      IF new.next # NIL THEN new.next.prev := new; END;
    END;
  END ListInsert;

PROCEDURE ListRemove (VAR list: REF ListElement; elem: REF ListElement) =
  BEGIN
    IF elem.prev = NIL THEN
      list := elem.next;
    ELSE
      elem.prev.next := elem.next;
    END;
    IF elem.next # NIL THEN elem.next.prev := elem.prev; END;
    elem.prev := NIL;
    elem.next := NIL;
  END ListRemove;


PROCEDURE ListFind (    list : REF ListElement;
                        id   : CARDINAL;
                    VAR elem : REF ListElement;
                    VAR found: BOOLEAN          ) =
  BEGIN
    elem := list;
    found := FALSE;
    LOOP
      IF elem = NIL THEN EXIT END;
      IF elem.id = id THEN found := TRUE; EXIT; END;
      elem := elem.next;
    END;
  END ListFind;

BEGIN
END RemoteTriggerStorage.
