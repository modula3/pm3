MODULE GraphTriggerStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:46  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:32  hosking
    Import of GRAS3 1.1

    Revision 1.3  1998/08/12 11:04:17  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.2  1997/11/21 09:37:18  roland
    New GraphEvents PutAttribute and TruncateAttribute replace ModifyAttribute

    Revision 1.1  1997/11/12 15:24:03  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

IMPORT Trigger, Event, Action, ContextSet, EventPattern, EventType;
IMPORT GraphEvents, PrivateGraphEvents, GraphEventPattern;

REVEAL
  T = Public BRANDED OBJECT
        table: TriggerTable;
        map  : TriggerMap;
      OVERRIDES
        init          := Init;
        storeTrigger  := StoreTrigger;
        removeTrigger := RemoveTrigger;
        notifyEvent   := NotifyEvent;
        getNextAction := GetNextAction;
      END;


PROCEDURE Init (ts: T): T =
  BEGIN
    ts.table := TriggerTable{NIL, ..};
    ts.map := TriggerMap{NIL, ..};
    RETURN ts;
  END Init;

PROCEDURE StoreTrigger (ts: T; t: Trigger.T; userdata: <*TRANSIENT*> REFANY;
                        id: CARDINAL) =
  VAR
    new      : REF ListElement;
    map, list: CARDINAL;

  PROCEDURE DependsOnFirstNode (tr: Trigger.T): BOOLEAN =
    VAR op: INTEGER;
    <* FATAL EventType.Unknown, EventType.Mismatch *>
    BEGIN
      EVAL PrivateGraphEvents.TypeToOp.get(tr.pattern().type(), op);
      CASE VAL(op, GraphEvents.Operation) OF
        GraphEvents.Operation.CreateNode,
          GraphEvents.Operation.DeleteNode =>
          (* node events do not depend on nodes *)
          RETURN FALSE;
      | GraphEvents.Operation.CreateEdge,
          GraphEvents.Operation.DeleteEdge =>
          WITH pat = tr.pattern() DO
            (* RETURN flag is not wildcard and is set TRUE *)
            RETURN NOT pat.isWildcard(PrivateGraphEvents.FirstNodeExANo)
                     AND GraphEventPattern.GetSourceNodeExists(pat);
          END;
      | GraphEvents.Operation.PutIndex,
          GraphEvents.Operation.DeleteIndex,
          GraphEvents.Operation.PutAttribute,
          GraphEvents.Operation.TruncateAttribute =>
        WITH pat = tr.pattern() DO
          (* RETURN flag is not wildcard and is set TRUE *)
          RETURN NOT pat.isWildcard(PrivateGraphEvents.FirstNodeExANo)
                   AND GraphEventPattern.GetNodeExists(pat);
        END;
      END;
    END DependsOnFirstNode;

  PROCEDURE DependsOnTargetNode (tr: Trigger.T): BOOLEAN =
    VAR op: INTEGER;
    <* FATAL EventType.Unknown, EventType.Mismatch *>
    BEGIN
      EVAL PrivateGraphEvents.TypeToOp.get(tr.pattern().type(), op);
      CASE VAL(op, GraphEvents.Operation) OF
      | GraphEvents.Operation.CreateEdge,
          GraphEvents.Operation.DeleteEdge =>
          WITH pat = tr.pattern() DO
            (* RETURN flag is not wildcard and is set TRUE *)
            RETURN NOT pat.isWildcard(PrivateGraphEvents.TargetNodeExANo)
                     AND GraphEventPattern.GetTargetNodeExists(pat);
          END;
      ELSE
        (* only edge events can depend on target node *)
        RETURN FALSE;
      END;
    END DependsOnTargetNode;

  BEGIN
    CheckType(t.pattern().type());
    new := NewListElement();
    new^ := ListElement{
              trigger := t, depsFirst := DependsOnFirstNode(t), depsTarg :=
              DependsOnTargetNode(t), userdata := userdata, id := id,
              next := NIL, prev := NIL, nextMap := NIL, prevMap := NIL};
    list := Hash(t.pattern().type());
    map := HashMap(id);
    ListInsert(ts.table[list], new);
    MapInsert(ts.map[map], new);
  END StoreTrigger;

PROCEDURE RemoveTrigger (ts: T; id: CARDINAL; VAR type: CARDINAL) =
  VAR
    elem     : REF ListElement;
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

PROCEDURE NotifyEvent (ts: T; e: Event.T; context: ContextSet.T) =
  VAR
    list : CARDINAL;
    elem : REF ListElement;
    act  : REF FiredAction;
    found: BOOLEAN;

  BEGIN
    list := Hash(e.type());
    ListGetFirstMatch(ts.table[list], e, context, elem, found);
    WHILE found DO
      act := NewFiredAction();
      act^ :=
        FiredAction{action := elem.trigger.action(), depsFirst :=
                    elem.depsFirst, depsTarg := elem.depsTarg, userdata :=
                    elem.userdata, coupling := elem.trigger.coupling(),
                    priority := elem.trigger.priority(), next := NIL};
      act.next := actions;
      actions := act;
      ListGetNextMatch(ts.table[list], e, context, elem, found);
    END;
  END NotifyEvent;

PROCEDURE GetNextAction (<* UNUSED *>     ts       : T;
                                      VAR act      : Action.T;
                                      VAR depsFirst: BOOLEAN;
                                      VAR depsTarg : BOOLEAN;
                                      VAR coupl    : Trigger.CouplingMode;
                                      VAR priority : CARDINAL;
                                      VAR userdata : REFANY                ):
  BOOLEAN =
  VAR h: REF FiredAction;
  BEGIN
    IF actions # NIL THEN
      act := actions.action;
      depsFirst := actions.depsFirst;
      depsTarg := actions.depsTarg;
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


CONST TableSize = ORD(LAST(GraphEvents.Operation)) + 1;

TYPE
  ListElement = RECORD
                  trigger         : Trigger.T;
                  depsFirst       : BOOLEAN;
                  depsTarg        : BOOLEAN;
                  userdata        : REFANY;
                  id              : CARDINAL;
                  next, prev      : REF ListElement;
                  nextMap, prevMap: REF ListElement;
                END;

  TableIndex = [0 .. TableSize - 1];
  TriggerTable = ARRAY TableIndex OF REF ListElement;
  TriggerMap = TriggerTable;

PROCEDURE Hash (type: CARDINAL): TableIndex =
  BEGIN
    RETURN type MOD TableSize;
  END Hash;

PROCEDURE HashMap (id: CARDINAL): TableIndex =
  BEGIN
    RETURN id MOD TableSize;
  END HashMap;

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
    elem^ := ListElement{NIL, FALSE, FALSE, NIL, 0, NIL, NIL, NIL, NIL};
    elem.next := FreeListElements;
    FreeListElements := elem;
  END DisposeListElement;

PROCEDURE ListInsert (VAR list: REF ListElement; new: REF ListElement) =
  VAR p: REF ListElement;
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


PROCEDURE ListGetFirstMatch (    list   : REF ListElement;
                                 event  : Event.T;
                                 context: ContextSet.T;
                             VAR elem   : REF ListElement;
                             VAR found  : BOOLEAN          ) =
  BEGIN
    elem := list;
    found := FALSE;
    LOOP
      IF elem = NIL THEN EXIT END;
      IF elem.trigger.active(context)
           AND elem.trigger.pattern().match(event) THEN
        (* The generic match method also filters not existing nodes,
           because a trigger only depends on a node, if the pattern
           specifies TRUE for its existance. So when an event declares
           a node as non existent, the match will fail for dependent
           triggers. *)
        found := TRUE;
        EXIT;
      END;
      elem := elem.next;
    END;
  END ListGetFirstMatch;

PROCEDURE ListGetNextMatch (<* UNUSED *>     list   : REF ListElement;
                                             event  : Event.T;
                                             context: ContextSet.T;
                                         VAR elem   : REF ListElement;
                                         VAR found  : BOOLEAN          ) =
  BEGIN
    elem := elem.next;
    found := FALSE;
    LOOP
      IF elem = NIL THEN EXIT END;
      IF elem.trigger.active(context)
           AND elem.trigger.pattern().match(event) THEN
        (* The generic match method also filters not existing nodes,
           because a trigger only depends on a node, if the pattern
           specifies TRUE for its existance. So when an event declares
           a node as non existent, the match will fail for dependent
           triggers. *)
        found := TRUE;
        EXIT;
      END;
      elem := elem.next;
    END;
  END ListGetNextMatch;

PROCEDURE MapInsert (VAR map: REF ListElement; new: REF ListElement) =
  VAR p: REF ListElement;
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

PROCEDURE MapRemove (VAR map: REF ListElement; elem: REF ListElement) =
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

PROCEDURE MapGet (    map : REF ListElement;
                      id  : CARDINAL;
                  VAR elem: REF ListElement  ): BOOLEAN =
  BEGIN
    elem := map;
    WHILE elem # NIL AND elem.id < id DO elem := elem.next; END;
    RETURN elem # NIL AND elem.id = id;
  END MapGet;


TYPE
  FiredAction = RECORD
                  action   : Action.T;
                  depsFirst: BOOLEAN;
                  depsTarg : BOOLEAN;
                  userdata : REFANY;
                  coupling : Trigger.CouplingMode;
                  priority : CARDINAL;
                  next     : REF FiredAction;
                END;

VAR actions, FreeFiredActions: REF FiredAction;

PROCEDURE NewFiredAction (): REF FiredAction =
  VAR new: REF FiredAction;
  BEGIN
    IF FreeFiredActions = NIL THEN
      RETURN NEW(REF FiredAction);
    ELSE
      new := FreeFiredActions;
      FreeFiredActions := new.next;
      RETURN new;
    END;
  END NewFiredAction;

PROCEDURE DisposeFiredAction (act: REF FiredAction) =
  BEGIN
    act^ := FiredAction{NIL, FALSE, FALSE, NIL,
                        Trigger.CouplingMode.Immediate, 0, NIL};
    act.next := FreeFiredActions;
    FreeFiredActions := act;
  END DisposeFiredAction;

PROCEDURE CheckType (type: CARDINAL) =
  VAR op: INTEGER;
  BEGIN
    IF NOT PrivateGraphEvents.TypeToOp.get(type, op) THEN
      <* ASSERT FALSE *>
    END;
  END CheckType;

BEGIN
END GraphTriggerStorage.
