MODULE ActivatedActions;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.2  1998/01/21 14:07:54  roland
    Bugfixes in KillTransaction. ClientMap and LevelMap were mixed up.

    Revision 1.1  1997/10/31 14:03:56  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The event handler
    subsystem implements the mapping between events and action via triggers.

*)
(***************************************************************************)

IMPORT Event, Action, ContextSet, ActiveActionPQueue, ActiveAction;

TYPE Public = T OBJECT METHODS init (): Default; END;

REVEAL
  Default = Public BRANDED OBJECT
              time: CARDINAL;
              maxLevel: CARDINAL := 0; (* highest currently stored transaction
                                          level *)
              actions: ActiveActionPQueue.T;  (* access actions ordered by
                                                 priority *)
              levelMap: LevelMap;  (* access sorted by transaction level *)
              clientMap: ClientMap;  (* access sorted by client number *)
            OVERRIDES
              init            := Init;
              store           := Store;
              killClient      := KillClient;
              killTransaction := KillTransaction;
              get             := Get;
              highest         := Highest;
              notEmpty        := NotEmpty;
            END;


PROCEDURE Init (aa: Default): Default =
  BEGIN
    aa.time := 0;
    aa.actions := NEW(ActiveActionPQueue.T).init();
    aa.levelMap := LevelMap{NIL, ..};
    aa.clientMap := ClientMap{NIL, ..};
    RETURN aa;
  END Init;

PROCEDURE Store (aa      : Default;
                 event   : Event.T;
                 context : ContextSet.T;
                 level   : CARDINAL;
                 priority: CARDINAL;
                 act     : Action.T;
                 userdata: REFANY        ) =
  VAR aact: ActiveAction.T;
    hl, hc: CARDINAL;
  BEGIN
    INC(aa.time);
    aact := NewActiveAction();
    aact^ :=
      ActiveAction.Struct{
        priority := ActiveAction.PriorityType{priority, aa.time}, action :=
        act, event := event, userdata := userdata, context := context,
        level := level, deleted := FALSE, nextClient := NIL, prevClient :=
        NIL, nextLevel := NIL, prevLevel := NIL};
    hl := LevelHash(level);
    LevelMapInsert(aa.levelMap[hl], aact);
    hc := ClientHash(act.client());
    ClientMapInsert(aa.clientMap[hc], aact);
    aa.maxLevel := MAX(aa.maxLevel, level);
    aa.actions.insert(aact);
  END Store;


PROCEDURE KillClient (aa: Default; c: CARDINAL) =
  VAR
    hc, hl: CARDINAL;
    act   : ActiveAction.T;
    found : BOOLEAN;
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

PROCEDURE KillTransaction (aa: Default; level: CARDINAL) =
  VAR
    hc, hl: CARDINAL;
    act   : ActiveAction.T;
    found : BOOLEAN;
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


PROCEDURE Get (    aa      : Default;
               VAR event   : Event.T;
               VAR context : ContextSet.T;
               VAR level   : CARDINAL;
               VAR action  : Action.T;
               VAR userdata: REFANY        ): BOOLEAN =
  BEGIN
    RETURN GetNextFromQueue(
             aa, event, context, level, action, userdata);
  END Get;


PROCEDURE Highest (aa: Default): CARDINAL =
  BEGIN
    IF NOT aa.actions.isEmpty() THEN
      RETURN aa.actions.highest().prio;
    ELSE
      RETURN 0;
    END;
  END Highest;

PROCEDURE NotEmpty (aa: Default): BOOLEAN =
  BEGIN
    RETURN NOT aa.actions.isEmpty();
  END NotEmpty;


PROCEDURE GetNextFromQueue (    aa      : Default;
                            VAR event   : Event.T;
                            VAR context : ContextSet.T;
                            VAR level   : CARDINAL;
                            VAR action  : Action.T;
                            VAR userdata: REFANY        ): BOOLEAN =
  VAR
    act      : ActiveAction.T;
    hlev, hcl: CARDINAL;
  BEGIN
    WHILE NOT aa.actions.isEmpty() DO
      act := aa.actions.get();
      IF NOT act.deleted THEN
        event := act.event;
        context := act.context;
        level := act.level;
        action := act.action;
        userdata := act.userdata;
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


VAR FreeActiveActions: ActiveAction.T := NIL;

PROCEDURE NewActiveAction (): ActiveAction.T =
  VAR new: ActiveAction.T;
  BEGIN
    IF FreeActiveActions = NIL THEN
      RETURN NEW(ActiveAction.T);
    ELSE
      new := FreeActiveActions;
      FreeActiveActions := new.nextClient;
      RETURN new;
    END;
  END NewActiveAction;

PROCEDURE DisposeActiveAction (act: ActiveAction.T) =
  BEGIN
    act^ := ActiveAction.Struct{ActiveAction.PriorityType{0, 0},
                                NIL, NIL, ContextSet.Empty(), NIL, 0, FALSE,
                                NIL, NIL, NIL, NIL};
    act.nextClient := FreeActiveActions;
    FreeActiveActions := act;
  END DisposeActiveAction;

CONST LevelMapSize = 11;

TYPE
  LevelIndex = [0 .. LevelMapSize - 1];
  LevelMap = ARRAY LevelIndex OF ActiveAction.T;

PROCEDURE LevelHash (l: CARDINAL): LevelIndex =
  BEGIN
    RETURN l MOD LevelMapSize;
  END LevelHash;

PROCEDURE LevelMapInsert (VAR map: ActiveAction.T; new: ActiveAction.T) =
  VAR p: ActiveAction.T;
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

PROCEDURE LevelMapRemove (VAR map: ActiveAction.T; elem: ActiveAction.T) =
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

PROCEDURE LevelMapGetFirst (    map  : ActiveAction.T;
                                level: CARDINAL;
                            VAR elem : ActiveAction.T;
                            VAR found: BOOLEAN         ) =
  BEGIN
    elem := map;
    found := FALSE;
    IF elem # NIL AND elem.level >= level THEN found := TRUE; END;
  END LevelMapGetFirst;

(**
PROCEDURE LevelMapGetNext (<* UNUSED *>    map  : ActiveAction.T;
                             level: CARDINAL;
                         VAR elem : ActiveAction.T;
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
  ClientMap = ARRAY ClientIndex OF ActiveAction.T;

PROCEDURE ClientHash (cl: CARDINAL): ClientIndex =
  BEGIN
    RETURN cl MOD ClientMapSize;
  END ClientHash;

PROCEDURE ClientMapInsert (VAR map: ActiveAction.T; new: ActiveAction.T) =
  VAR p: ActiveAction.T;
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

PROCEDURE ClientMapRemove (VAR map: ActiveAction.T; elem: ActiveAction.T) =
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

PROCEDURE ClientMapGetFirst (    map   : ActiveAction.T;
                                 client: CARDINAL;
                             VAR elem  : ActiveAction.T;
                             VAR found : BOOLEAN         ) =
  BEGIN
    elem := map;
    WHILE elem # NIL AND elem.action.client() > client DO
      elem := elem.nextClient;
    END;
    found := elem # NIL AND elem.action.client() = client;
  END ClientMapGetFirst;

(**
PROCEDURE ClientMapGetNext (<* UNUSED*>    map   : ActiveAction.T;
                              client: CARDINAL;
                          VAR elem  : ActiveAction.T;
                          VAR found : BOOLEAN           ) =
BEGIN
  elem := elem.nextClient;
  found := elem # NIL AND elem.action.client() = client;
END ClientMapGetNext;
*)
BEGIN

END ActivatedActions.
