MODULE GraphActivatedActions;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.2  1998/01/21 14:07:45  roland
    Bugfixes in KillTransaction. ClientMap and LevelMap were mixed up.

    Revision 1.1  1997/11/12 15:23:52  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

IMPORT Event, Action, ContextSet, GraphActiveActionPQueue,
       GraphActiveAction, GraphEvents, EventType;
IMPORT Node, Transaction, DeletedNodes;

REVEAL
  T = Public BRANDED OBJECT
        time: CARDINAL;
        maxLevel: CARDINAL := 0; (* highest currently stored transaction
                                    level *)
        actions: GraphActiveActionPQueue.T;  (* access actions ordered by
                                                priority *)
        levelMap : LevelMap;     (* access sorted by transaction level *)
        clientMap: ClientMap;    (* access sorted by client number *)
        delNodes: DeletedNodes.T;
      OVERRIDES
        init               := Init;
        store              := Store;
        killClient         := KillClient;
        killTransaction    := KillTransaction;
        notifyNodeDeletion := NotifyNodeDeletion;
        get                := Get;
        highest            := Highest;
        notEmpty           := NotEmpty;
      END;

PROCEDURE Init (aa: T): T =
  BEGIN
    aa.time := 0;
    aa.actions := NEW(GraphActiveActionPQueue.T).init();
    aa.levelMap := LevelMap{NIL, ..};
    aa.clientMap := ClientMap{NIL, ..};
    aa.delNodes := NEW(DeletedNodes.T).init();
    RETURN aa;
  END Init;

PROCEDURE Store (aa       : T;
                 event    : Event.T;
                 depsFirst: BOOLEAN;
                 depsTarg : BOOLEAN;
                 context  : ContextSet.T;
                 level    : CARDINAL;
                 priority : CARDINAL;
                 act      : Action.T;
                 userdata : REFANY        ) =
  VAR
    aact      : GraphActiveAction.T;
    hl, hc: CARDINAL;
  BEGIN
    INC(aa.time);
    aact := NewActiveAction();
    aact^ :=
      GraphActiveAction.Struct{
        priority := GraphActiveAction.PriorityType{priority, aa.time},
        action := act, depsFirst := depsFirst, depsTarg := depsTarg,
        event := event, userdata := userdata, context := context, level :=
        level, deleted := FALSE, nextClient := NIL, prevClient := NIL,
        nextLevel := NIL, prevLevel := NIL};
    hl := LevelHash(level);
    LevelMapInsert(aa.levelMap[hl], aact);
    hc := ClientHash(act.client());
    ClientMapInsert(aa.clientMap[hc], aact);
    aa.maxLevel := MAX(aa.maxLevel, level);
    aa.actions.insert(aact);
  END Store;


PROCEDURE KillClient (aa: T; c: CARDINAL) =
  VAR
    hc, hl: CARDINAL;
    act       : GraphActiveAction.T;
    found     : BOOLEAN;
  BEGIN
    hc := ClientHash(c);
    ClientMapGetFirst(aa.clientMap[hc], c, act, found);
    WHILE found DO
      hl := LevelHash(act.level);
      LevelMapRemove(aa.levelMap[hl], act);
      ClientMapRemove(aa.clientMap[hc], act);
      act.deleted := TRUE;
      ClientMapGetFirst(aa.clientMap[hc], c, act, found);
    END;
  END KillClient;

PROCEDURE KillTransaction (aa: T; level: CARDINAL) =
  VAR
    hc, hl: CARDINAL;
    act       : GraphActiveAction.T;
    found     : BOOLEAN;
  BEGIN
    (* kill also all deeper nested transactions *)
    FOR lev := aa.maxLevel TO level BY -1 DO
      hl := LevelHash(lev);
      LevelMapGetFirst(aa.levelMap[hl], lev, act, found);
      WHILE found DO
        hc := ClientHash(act.action.client());
        ClientMapRemove(aa.clientMap[hc], act);
        LevelMapRemove(aa.levelMap[hl], act);
        act.deleted := TRUE;
        LevelMapGetFirst(aa.levelMap[hl], lev, act, found);
      END;
    END;
    aa.maxLevel := MAX(0, level - 1);
  END KillTransaction;

PROCEDURE NotifyNodeDeletion (aa: T; level: Transaction.Level; node: Node.T) =
  BEGIN
    IF NOT aa.actions.isEmpty() THEN
      INC(aa.time);
      aa.delNodes.insert(node, level, aa.time);
    END;
  END NotifyNodeDeletion;

PROCEDURE Get (    aa      : T;
               VAR event   : Event.T;
               VAR context : ContextSet.T;
               VAR level   : CARDINAL;
               VAR action  : Action.T;
               VAR userdata: REFANY        ): BOOLEAN =
  CONST
    EdgeOps = SET OF
                GraphEvents.Operation{GraphEvents.Operation.CreateEdge,
                                      GraphEvents.Operation.DeleteEdge};
  VAR
    res, invalid       : BOOLEAN;
    depsFirst, depsTarg: BOOLEAN;
    node               : Node.T;
    time               : CARDINAL;
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    LOOP
      IF GetNextFromQueue(aa, event, depsFirst, depsTarg, context, level,
                          action, userdata, time) THEN
        invalid := FALSE;
        (* check whether activated action is invalidated by deleted node *)
        IF depsFirst THEN
          (* get number of first node and check if in deleted node set *)
          IF GraphEvents.GetOperation(event) IN EdgeOps THEN
            node := GraphEvents.GetSourceNode(event);
          ELSE
            node := GraphEvents.GetNode(event);
          END;
          invalid := aa.delNodes.invalid(node, time);
        END;
        IF NOT invalid AND depsTarg THEN
          (* get number of target node and check if in deleted node set *)
          node := GraphEvents.GetTargetNode(event);
          invalid := aa.delNodes.invalid(node, time);
        END;
        IF NOT invalid THEN
          (* neither first node nor target node invalidated the action *)
          res := TRUE;
          EXIT;
        END;
      ELSE
        res := FALSE;
        EXIT;
      END;
    END;
    IF aa.actions.isEmpty() THEN
      (* clean deleted node set *)
      aa.delNodes.clear();
    END;
    RETURN res;
  END Get;


PROCEDURE Highest (aa: T): CARDINAL =
  BEGIN
    IF NOT aa.actions.isEmpty() THEN
      RETURN aa.actions.highest().prio;
    ELSE
      RETURN 0;
    END;
  END Highest;

PROCEDURE NotEmpty (aa: T): BOOLEAN =
  BEGIN
    RETURN NOT aa.actions.isEmpty();
  END NotEmpty;


PROCEDURE GetNextFromQueue (    aa       : T;
                            VAR event    : Event.T;
                            VAR depsFirst: BOOLEAN;
                            VAR depsTarg : BOOLEAN;
                            VAR context  : ContextSet.T;
                            VAR level    : CARDINAL;
                            VAR action   : Action.T;
                            VAR userdata : REFANY;
                            VAR time     : CARDINAL      ): BOOLEAN =
  VAR
    act      : GraphActiveAction.T;
    hlev, hcl: CARDINAL;
  BEGIN
    WHILE NOT aa.actions.isEmpty() DO
      act := aa.actions.get();
      IF NOT act.deleted THEN
        event := act.event;
        depsFirst := act.depsFirst;
        depsTarg := act.depsTarg;
        context := act.context;
        level := act.level;
        action := act.action;
        userdata := act.userdata;
        time := act.priority.timeStamp;
        hlev := LevelHash(level);
        LevelMapRemove(aa.levelMap[hlev], act);
        hcl := ClientHash(action.client());
        ClientMapRemove(aa.clientMap[hcl], act);
        DisposeActiveAction(act);
        RETURN TRUE;
      ELSE
        (* action was deleted by killTransaction or killClient *)
        DisposeActiveAction(act);
      END;
    END;
    RETURN FALSE;
  END GetNextFromQueue;


VAR FreeActiveActions: GraphActiveAction.T := NIL;

PROCEDURE NewActiveAction (): GraphActiveAction.T =
  VAR new: GraphActiveAction.T;
  BEGIN
    IF FreeActiveActions = NIL THEN
      RETURN NEW(GraphActiveAction.T);
    ELSE
      new := FreeActiveActions;
      FreeActiveActions := new.nextClient;
      RETURN new;
    END;
  END NewActiveAction;

PROCEDURE DisposeActiveAction (act: GraphActiveAction.T) =
  BEGIN
    act^ := GraphActiveAction.Struct{
              GraphActiveAction.PriorityType{0, 0}, NIL, FALSE, FALSE, NIL,
              ContextSet.Empty(), NIL, 0, FALSE, NIL, NIL, NIL, NIL};
    act.nextClient := FreeActiveActions;
    FreeActiveActions := act;
  END DisposeActiveAction;

CONST LevelMapSize = 11;

TYPE
  LevelIndex = [0 .. LevelMapSize - 1];
  LevelMap = ARRAY LevelIndex OF GraphActiveAction.T;

PROCEDURE LevelHash (l: CARDINAL): LevelIndex =
  BEGIN
    RETURN l MOD LevelMapSize;
  END LevelHash;

PROCEDURE LevelMapInsert (VAR map: GraphActiveAction.T;
                              new: GraphActiveAction.T  ) =
  VAR p: GraphActiveAction.T;
  BEGIN
    IF map = NIL OR map.level <= new.level THEN
      (* empty map or new action preceeds/equals first element *)
      new.nextLevel := map;
      map := new;
      IF new.nextLevel # NIL THEN new.nextLevel.prevLevel := new; END;
    ELSE
      p := map;
      WHILE p.nextLevel # NIL AND new.level < p.level DO
        p := p.nextLevel;
      END;
      new.nextLevel := p.nextLevel;
      new.prevLevel := p;
      p.nextLevel := new;
      IF new.nextLevel # NIL THEN new.nextLevel.prevLevel := new; END;
    END;
  END LevelMapInsert;

PROCEDURE LevelMapRemove (VAR map : GraphActiveAction.T;
                              elem: GraphActiveAction.T  ) =
  BEGIN
    IF elem.prevLevel = NIL THEN
      map := elem.nextLevel;
    ELSE
      elem.prevLevel.nextLevel := elem.nextLevel;
    END;
    IF elem.nextLevel # NIL THEN
      elem.nextLevel.prevLevel := elem.prevLevel;
    END;
    elem.prevLevel := NIL;
    elem.nextLevel := NIL;
  END LevelMapRemove;

PROCEDURE LevelMapGetFirst (    map  : GraphActiveAction.T;
                                level: CARDINAL;
                            VAR elem : GraphActiveAction.T;
                            VAR found: BOOLEAN              ) =
  BEGIN
    elem := map;
    found := FALSE;
    IF elem # NIL AND elem.level >= level THEN found := TRUE; END;
  END LevelMapGetFirst;

(**
PROCEDURE LevelMapGetNext (<* UNUSED *>    map  : GraphActiveAction.T;
                             level: CARDINAL;
                         VAR elem : GraphActiveAction.T;
                         VAR found: BOOLEAN           ) =
BEGIN
  elem := elem.nextLevel;
  found := FALSE;
  IF elem # NIL AND elem.level >= level THEN found := TRUE; END;
END LevelMapGetNext;
*)



CONST ClientMapSize = 11;

TYPE
  ClientIndex = [0 .. ClientMapSize - 1];
  ClientMap = ARRAY ClientIndex OF GraphActiveAction.T;

PROCEDURE ClientHash (cl: CARDINAL): ClientIndex =
  BEGIN
    RETURN cl MOD ClientMapSize;
  END ClientHash;

PROCEDURE ClientMapInsert (VAR map: GraphActiveAction.T;
                               new: GraphActiveAction.T  ) =
  VAR p: GraphActiveAction.T;
  BEGIN
    IF map = NIL OR map.action.client() <= new.action.client() THEN
      (* empty map or new action preceeds/equals first element *)
      new.nextClient := map;
      map := new;
      IF new.nextClient # NIL THEN new.nextClient.prevClient := new; END;
    ELSE
      p := map;
      WHILE p.nextClient # NIL AND new.action.client() < p.action.client() DO
        p := p.nextClient;
      END;
      new.nextClient := p.nextClient;
      new.prevClient := p;
      p.nextClient := new;
      IF new.nextClient # NIL THEN new.nextClient.prevClient := new; END;
    END;
  END ClientMapInsert;

PROCEDURE ClientMapRemove (VAR map : GraphActiveAction.T;
                               elem: GraphActiveAction.T  ) =
  BEGIN
    IF elem.prevClient = NIL THEN
      map := elem.nextClient;
    ELSE
      elem.prevClient.nextClient := elem.nextClient;
    END;
    IF elem.nextClient # NIL THEN
      elem.nextClient.prevClient := elem.prevClient;
    END;
    elem.prevClient := NIL;
    elem.nextClient := NIL;
  END ClientMapRemove;

PROCEDURE ClientMapGetFirst (    map   : GraphActiveAction.T;
                                 client: CARDINAL;
                             VAR elem  : GraphActiveAction.T;
                             VAR found : BOOLEAN              ) =
  BEGIN
    elem := map;
    WHILE elem # NIL AND elem.action.client() > client DO
      elem := elem.nextClient;
    END;
    found := elem # NIL AND elem.action.client() = client;
  END ClientMapGetFirst;

(**
PROCEDURE ClientMapGetNext (<* UNUSED*>    map   : GraphActiveAction.T;
                              client: CARDINAL;
                          VAR elem  : GraphActiveAction.T;
                          VAR found : BOOLEAN           ) =
BEGIN
  elem := elem.nextClient;
  found := elem # NIL AND elem.action.client() = client;
END ClientMapGetNext;
*)


BEGIN
END GraphActivatedActions.
