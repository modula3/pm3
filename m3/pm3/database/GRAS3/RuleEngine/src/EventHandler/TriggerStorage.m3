MODULE TriggerStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:50  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:40  hosking
    Import of GRAS3 1.1

    Revision 1.2  1998/08/12 11:05:03  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.1  1997/10/31 14:04:04  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The event handler
    subsystem implements the mapping between events and action via triggers.

*)
(***************************************************************************)

IMPORT Trigger, Event, Action, ContextSet, EventPattern;

TYPE Public = T OBJECT METHODS init (): Default; END;

REVEAL
  Default = Public BRANDED OBJECT
              table: TriggerTable;
              map  : TriggerMap;
            OVERRIDES
              init          := Init;
              storeTrigger  := StoreTrigger;
              removeTrigger := RemoveTrigger;
              notifyEvent   := NotifyEvent;
              getNextAction := GetNextAction;
            END;


PROCEDURE Init (ts: Default): Default =
  BEGIN
    ts.table := TriggerTable{NIL, ..};
    ts.map := TriggerMap{NIL, ..};
    RETURN ts;
  END Init;

PROCEDURE StoreTrigger (ts: Default; t: Trigger.T; userdata: <*TRANSIENT*> REFANY; id: CARDINAL) =
  VAR
    new      : ListElement;
    map, list: CARDINAL;
  BEGIN
    new := NewListElement();
    new^ := ListElementRec{trigger := t, userdata := userdata,
                           id := id, next := NIL, prev := NIL,
                           nextMap := NIL, prevMap := NIL};
    list := Hash(t.pattern().type());
    map := HashMap(id);
    ListInsert(ts.table[list], new);
    MapInsert(ts.map[map], new);
  END StoreTrigger;

PROCEDURE RemoveTrigger (ts: Default; id: CARDINAL; VAR type: CARDINAL) =
  VAR
    elem     : ListElement;
    map, list: CARDINAL;
  BEGIN
    map := HashMap(id);
    IF MapGet(ts.map[map], id, elem) THEN
      MapRemove(ts.map[map], elem);
      type := elem.trigger.pattern().type();
      list := Hash(type);
      ListRemove(ts.table[list], elem);
      DisposeListElement(elem);
    END;
  END RemoveTrigger;

(**
  PROCEDURE KillClient (ts: Default; cl: CARDINAL) =
  VAR
    elem: REF ListElement;
    list: CARDINAL;
  BEGIN
    FOR map := FIRST(TableIndex) TO LAST(TableIndex) DO
      WHILE MapFindClient(ts.map[map], elem) DO
        MapRemove(ts.map[map], elem);
        list := Hash(elem.trigger.pattern().type());
        ListRemove(ts.table[list], elem);
      END;
    END;
  END KillClient;
*)

PROCEDURE NotifyEvent (ts: Default; e: Event.T; context: ContextSet.T) =
  VAR
    list : CARDINAL;
    elem : ListElement;
    act  : FiredAction;
    found: BOOLEAN;
  BEGIN
    list := Hash(e.type());
    ListGetFirstMatch(ts.table[list], e, context, elem, found);
    WHILE found DO
      act := NewFiredAction();
      act^ := FiredActionRec{action := elem.trigger.action(), userdata := elem.userdata, coupling :=
                          elem.trigger.coupling(), priority :=
                          elem.trigger.priority(), next := NIL};
      act.next := actions;
      actions := act;
      ListGetNextMatch(ts.table[list], e, context, elem, found);
    END;
  END NotifyEvent;

PROCEDURE GetNextAction (<* UNUSED *>     ts      : Default;
                                      VAR act     : Action.T;
                                      VAR coupl   : Trigger.CouplingMode;
                                      VAR priority: CARDINAL;
                                      VAR userdata: <*TRANSIENT*> REFANY):
  BOOLEAN =
  VAR h: FiredAction;
  BEGIN
    IF actions # NIL THEN
      act := actions.action;
      coupl := actions.coupling;
      priority := actions.priority;
      userdata := actions.userdata;
      h := actions;
      actions := actions.next;
      DisposeFiredAction(h);
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END GetNextAction;


CONST TableSize = 229;

TYPE
  ListElementRec = RECORD
                  trigger         : Trigger.T;
                  userdata        : <*TRANSIENT*> REFANY;
                  id              : CARDINAL;
                  next, prev      : ListElement;
                  nextMap, prevMap: ListElement;
                END;
  ListElement = <*TRANSIENT*> REF ListElementRec;

  TableIndex = [0 .. TableSize - 1];
  TriggerTable = ARRAY TableIndex OF ListElement;
  TriggerMap = TriggerTable;

PROCEDURE Hash (type: CARDINAL): TableIndex =
  BEGIN
    RETURN type MOD TableSize;
  END Hash;

PROCEDURE HashMap (id: CARDINAL): TableIndex =
  BEGIN
    RETURN id MOD TableSize;
  END HashMap;

VAR FreeListElements: ListElement := NIL;

PROCEDURE NewListElement (): ListElement =
  VAR new: ListElement;
  BEGIN
    IF FreeListElements = NIL THEN
      RETURN NEW(ListElement);
    ELSE
      new := FreeListElements;
      FreeListElements := new.next;
      RETURN new;
    END;
  END NewListElement;

PROCEDURE DisposeListElement (elem: ListElement) =
  BEGIN
    elem^ := ListElementRec{NIL, NIL, 0, NIL, NIL, NIL, NIL};
    elem.next := FreeListElements;
    FreeListElements := elem;
  END DisposeListElement;

PROCEDURE ListInsert (VAR list: ListElement; new: ListElement) =
  VAR p: ListElement;
  BEGIN
    IF list = NIL OR NOT EventPattern.Less(
                           list.trigger.pattern(), new.trigger.pattern()) THEN
      (* empty list or trigger preceeds/equals first element *)
      new.next := list;
      list := new;
      IF new.next # NIL THEN new.next.prev := new; END;
    ELSE
      p := list;
      WHILE p.next # NIL AND EventPattern.Less(p.next.trigger.pattern(),
                                               new.trigger.pattern()) DO
        p := p.next;
      END;
      new.next := p.next;
      new.prev := p;
      p.next := new;
      IF new.next # NIL THEN new.next.prev := new; END;
    END;
  END ListInsert;

PROCEDURE ListRemove (VAR list: ListElement; elem: ListElement) =
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


PROCEDURE ListGetFirstMatch (    list   : ListElement;
                                 event  : Event.T;
                                 context: ContextSet.T;
                             VAR elem   : ListElement;
                             VAR found  : BOOLEAN          ) =
  BEGIN
    elem := list;
    found := FALSE;
    LOOP
      IF elem = NIL THEN EXIT END;
      IF elem.trigger.active(context)
           AND elem.trigger.pattern().match(event) THEN
        found := TRUE;
        EXIT;
      END;
      elem := elem.next;
    END;
  END ListGetFirstMatch;

PROCEDURE ListGetNextMatch (<* UNUSED *>     list   : ListElement;
                                             event  : Event.T;
                                             context: ContextSet.T;
                                         VAR elem   : ListElement;
                                         VAR found  : BOOLEAN          ) =
  BEGIN
    elem := elem.next;
    found := FALSE;
    LOOP
      IF elem = NIL THEN EXIT END;
      IF elem.trigger.active(context)
           AND elem.trigger.pattern().match(event) THEN
        found := TRUE;
        EXIT;
      END;
      elem := elem.next;
    END;
  END ListGetNextMatch;

PROCEDURE MapInsert (VAR map: ListElement; new: ListElement) =
  VAR p: ListElement;
  BEGIN
    IF map = NIL OR map.id >= new.id THEN
      (* empty map or trigger preceeds/equals first element *)
      new.nextMap := map;
      map := new;
      IF new.nextMap # NIL THEN new.nextMap.prevMap := new; END;
    ELSE
      p := map;
      WHILE p.nextMap # NIL AND p.nextMap.id < new.id DO
        p := p.nextMap;
      END;
      new.nextMap := p.nextMap;
      new.prevMap := p;
      p.nextMap := new;
      IF new.nextMap # NIL THEN new.nextMap.prevMap := new; END;
    END;
  END MapInsert;

PROCEDURE MapRemove (VAR map: ListElement; elem: ListElement) =
  BEGIN
    IF elem.prevMap = NIL THEN
      map := elem.nextMap;
    ELSE
      elem.prevMap.nextMap := elem.nextMap;
    END;
    IF elem.nextMap # NIL THEN elem.nextMap.prevMap := elem.prevMap; END;
    elem.prevMap := NIL;
    elem.nextMap := NIL;
  END MapRemove;

PROCEDURE MapGet (    map : ListElement;
                      id  : CARDINAL;
                  VAR elem: ListElement  ): BOOLEAN =
  BEGIN
    elem := map;
    WHILE elem # NIL AND elem.id < id DO elem := elem.next; END;
    RETURN elem # NIL AND elem.id = id;
  END MapGet;


TYPE
  FiredActionRec = RECORD
                  action  : Action.T;
                  userdata: <*TRANSIENT*> REFANY;
                  coupling: Trigger.CouplingMode;
                  priority: CARDINAL;
                  next    : FiredAction;
                END;
  FiredAction = <*TRANSIENT*> REF FiredActionRec;

VAR actions, FreeFiredActions: FiredAction;

PROCEDURE NewFiredAction (): FiredAction =
  VAR new: FiredAction;
  BEGIN
    IF FreeFiredActions = NIL THEN
      RETURN NEW(FiredAction);
    ELSE
      new := FreeFiredActions;
      FreeFiredActions := new.next;
      RETURN new;
    END;
  END NewFiredAction;

PROCEDURE DisposeFiredAction (act: FiredAction) =
  BEGIN
    act^ := FiredActionRec{NIL, NIL, Trigger.CouplingMode.Immediate, 0, NIL};
    act.next := FreeFiredActions;
    FreeFiredActions := act;
  END DisposeFiredAction;

BEGIN
END TriggerStorage.
