MODULE LocalRuleHandler;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.5  1998/08/12 11:04:43  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.4  1997/12/15 16:32:07  roland
    Bugfix: Use contexts of transaction units rather than global context
    for handling events.

    Revision 1.3  1997/11/12 17:22:27  roland
    Initialize internals before official login to RuleEngine.

    Revision 1.2  1997/11/12 15:33:26  roland
    Bugfixes in support for specialized EventHandlers.

    Revision 1.1  1997/10/31 14:02:58  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling.

*)
(***************************************************************************)

IMPORT CardSeq, CardEventHandlerTbl, EventHandlerSeq;
IMPORT Trigger, EventHandler, Event, Action, ContextSet, TriggerStorage;
IMPORT EventDetector, EventDetectorSeq, CardEventDetectorSeqTbl;

VAR
  DefaultHandler: EventHandler.T;
  TypeMap       : CardEventHandlerTbl.T;
  IdMap         : CardEventHandlerTbl.T;
  Handlers      : EventHandlerSeq.T;
  Detectors     : CardEventDetectorSeqTbl.T;
  loggedIn      : BOOLEAN               := FALSE;
  NextTrigger   : CARDINAL              := 0;
  Access        : MUTEX;

(* Initialization and set up*)

PROCEDURE Login () =
  VAR triggerstorage: TriggerStorage.T;
  BEGIN
    IF NOT loggedIn THEN
      loggedIn := TRUE;
      triggerstorage := NEW(TriggerStorage.Default).init();
      DefaultHandler := NEW(EventHandler.T).init(triggerstorage);
      TypeMap := NEW(CardEventHandlerTbl.Default).init();
      IdMap := NEW(CardEventHandlerTbl.Default).init();
      Handlers := NEW(EventHandlerSeq.T).init();
      Detectors := NEW(CardEventDetectorSeqTbl.Default).init();
      Access := NEW(MUTEX);
    END;
  END Login;


PROCEDURE RegisterEventHandler (handler: EventHandler.T; types: CardSeq.T) =
  VAR un: INTEGER;
  BEGIN
    LOCK Access DO
      Handlers.addhi(handler);
      (* update type map *)
      FOR i := 0 TO types.size() - 1 DO
        EVAL TypeMap.put(types.get(i), handler);
      END;
      (* propagate all known transaction units *)
      un := UnitsGetFirst();
      WHILE un >= 0 DO
        handler.newTransactionUnit(un);
        un := UnitsGetNext(un);
      END;
    END;
  END RegisterEventHandler;

PROCEDURE RegisterEventDetector(detector: EventDetector.T; types: CardSeq.T) =
  VAR dtcs: EventDetectorSeq.T;
  BEGIN
    FOR i := 0 TO types.size() -1 DO      
      IF Detectors.get(types.get(i), dtcs) THEN
        dtcs.addhi(detector);
      ELSE
        dtcs := NEW(EventDetectorSeq.T).init(sizeHint := 1);
        dtcs.addhi(detector);
        EVAL Detectors.put(types.get(i), dtcs);
      END;
    END;
  END RegisterEventDetector;
  
PROCEDURE RegisterTransactionUnit (): CARDINAL =
  BEGIN
    WITH id = NewUnit() DO
      DefaultHandler.newTransactionUnit(id);
      FOR i := 0 TO Handlers.size() - 1 DO
        Handlers.get(i).newTransactionUnit(id);
      END;
      RETURN id;
    END;
  END RegisterTransactionUnit;

PROCEDURE UnregisterTransactionUnit (unit: CARDINAL) =
  BEGIN
    DefaultHandler.newTransactionUnit(unit);
    FOR i := 0 TO Handlers.size() - 1 DO
      Handlers.get(i).newTransactionUnit(unit);
    END;
    DelUnit(unit);
  END UnregisterTransactionUnit;

(* Trigger handling *)

PROCEDURE NextTriggerId (): CARDINAL =
  BEGIN
    LOCK Access DO INC(NextTrigger); RETURN NextTrigger; END;
  END NextTriggerId;

PROCEDURE RegisterTrigger (trigger : Trigger.T;
                           userdata: REFANY;
                           id      : CARDINAL   ) =
  VAR handler: EventHandler.T;
      type: CARDINAL;
      dtcs: EventDetectorSeq.T;
  BEGIN
    LOCK Access DO
      type := trigger.pattern().type();
      IF TypeMap.get(type, handler) THEN
        handler.storeTrigger(trigger, userdata, id);
        EVAL IdMap.put(id, handler);
      ELSE
        DefaultHandler.storeTrigger(trigger, userdata, id);
      END;
      IF Detectors.get(type, dtcs) THEN
        FOR i := 0 TO dtcs.size() - 1 DO
          dtcs.get(i).notifyRegistration(type);
        END;
      END;
    END;
  END RegisterTrigger;

PROCEDURE UnregisterTrigger (id: CARDINAL) =
  VAR handler: EventHandler.T;
      type: CARDINAL;
      dtcs: EventDetectorSeq.T;
  BEGIN
    LOCK Access DO
      IF IdMap.get(id, handler) THEN
        handler.removeTrigger(id, type);
      ELSE
        DefaultHandler.removeTrigger(id, type);
      END;
      IF Detectors.get(type, dtcs) THEN
        FOR i := 0 TO dtcs.size() -1 DO
          dtcs.get(i).notifyUnregistration(type);
        END;
      END;
    END;
  END UnregisterTrigger;


(* Context handling *)

PROCEDURE ActivateContext (tu: CARDINAL; number: CARDINAL) =
  BEGIN
    LOCK Access DO InsertUnitContext(tu, number); END;
  END ActivateContext;

PROCEDURE DeactivateContext (tu: CARDINAL; number: CARDINAL) =
  BEGIN
    LOCK Access DO RemoveUnitContext(tu, number); END;
  END DeactivateContext;


(* Event handling *)

PROCEDURE Handle (tu: CARDINAL; event: Event.T; level: CARDINAL) =
  VAR handler: EventHandler.T;
  BEGIN
    LOCK Access DO
      IF TypeMap.get(event.type(), handler) THEN
        handler.handle(tu, event, Units^[tu].context, level);
      ELSE
        DefaultHandler.handle(tu, event, Units^[tu].context, level);
      END;
    END;
  END Handle;


PROCEDURE GetNextAction (    tu              : CARDINAL;
                             coupling        : Trigger.CouplingMode;
                         VAR event           : Event.T;
                         VAR context         : ContextSet.T;
                         VAR transactionLevel: CARDINAL;
                         VAR action          : Action.T;
                         VAR userdata        : REFANY                ):
  BOOLEAN =
  VAR
    max        : CARDINAL;
    mh, handler: EventHandler.T;
  BEGIN
    LOCK Access DO
      max := DefaultHandler.highestPriority(tu, coupling);
      mh := DefaultHandler;
      FOR i := 0 TO Handlers.size() - 1 DO
        handler := Handlers.get(i);
        IF handler.hasActivatedActions(tu, coupling) THEN
          WITH prio = Handlers.get(i).highestPriority(tu, coupling) DO
            IF max < prio THEN max := prio; mh := Handlers.get(i); END;
          END;
        END;
      END;
      RETURN mh.getNextAction(tu, coupling, event, context,
                              transactionLevel, action, userdata);
    END;
  END GetNextAction;


PROCEDURE KillClientActions (client: CARDINAL) =
  BEGIN
    LOCK Access DO
      FOR i := 0 TO Handlers.size() - 1 DO
        Handlers.get(i).killClientActions(client);
      END;
      DefaultHandler.killClientActions(client);
    END;
  END KillClientActions;


PROCEDURE KillTransaction (tu: CARDINAL; level: CARDINAL) =
  BEGIN
    LOCK Access DO
      FOR i := 0 TO Handlers.size() - 1 DO
        Handlers.get(i).killTransaction(tu, level);
      END;
      DefaultHandler.killTransaction(tu, level);
    END;
  END KillTransaction;

(* Transaction units *)

CONST UnitInitLen = 10;

TYPE
  UnitInfo = RECORD
               context: ContextSet.T;
               used   : BOOLEAN;
               next   : INTEGER;
             END;

VAR
  Units   : REF ARRAY OF UnitInfo := NIL;
  FreeUnit: INTEGER               := -1;

PROCEDURE NewUnit (): CARDINAL =
  VAR id: CARDINAL;
  BEGIN
    IF Units = NIL OR FreeUnit < 0 THEN
      (* extend unit array *)
      VAR
        len: CARDINAL;
        nu : REF ARRAY OF UnitInfo;
      BEGIN
        IF Units = NIL THEN
          (* initialization *)
          len := UnitInitLen;
          Units := NEW(REF ARRAY OF UnitInfo, len);
          FOR i := 0 TO len - 1 DO
            Units^[i] := UnitInfo{ContextSet.Empty(), FALSE, i + 1};
          END;
          Units^[len - 1].next := -1;
          FreeUnit := 0;
        ELSE
          (* extension *)
          len := NUMBER(Units^);
          nu := NEW(REF ARRAY OF UnitInfo, 2 * len);
          SUBARRAY(nu^, 0, len) := Units^;
          FOR i := len TO 2 * len - 1 DO
            nu^[i] := UnitInfo{ContextSet.Empty(), FALSE, i + 1};
          END;
          nu^[2 * len - 1].next := -1;
          Units := nu;
          FreeUnit := len;
        END;
      END;
    END;
    id := FreeUnit;
    FreeUnit := Units^[id].next;
    Units^[id].used := TRUE;
    RETURN id;
  END NewUnit;

PROCEDURE DelUnit (tu: CARDINAL) =
  BEGIN
    Units^[tu].next := FreeUnit;
    FreeUnit := tu;
    Units^[tu].context := ContextSet.Empty();
    Units^[tu].used := FALSE;
  END DelUnit;

PROCEDURE UnitsGetFirst (): INTEGER =
  VAR un: INTEGER;
  BEGIN
    IF Units = NIL THEN RETURN -1 END;
    un := 0;
    WHILE un < NUMBER(Units^) AND NOT Units^[un].used DO INC(un); END;
    IF un >= NUMBER(Units^) THEN RETURN -1 ELSE RETURN un; END;
  END UnitsGetFirst;

PROCEDURE UnitsGetNext (un: INTEGER): INTEGER =
  BEGIN
    INC(un);
    WHILE un < NUMBER(Units^) AND NOT Units^[un].used DO INC(un); END;
    IF un >= NUMBER(Units^) THEN RETURN -1 ELSE RETURN un; END;
  END UnitsGetNext;

PROCEDURE InsertUnitContext (tu: CARDINAL; cont: CARDINAL) =
  BEGIN
    Units^[tu].context := ContextSet.Insert(Units^[tu].context, cont);
  END InsertUnitContext;

PROCEDURE RemoveUnitContext (tu: CARDINAL; cont: CARDINAL) =
  BEGIN
    Units^[tu].context := ContextSet.Remove(Units^[tu].context, cont);
  END RemoveUnitContext;

BEGIN
  (* small hack: to be able to register specialized event handlers before
     triggers from other clients are propagated, we have to register them
     before login to RuleEngine.  On the other hand, internal data
     structures of LocalRuleHandler are initialized only during
     RuleEngin.Login, and a RegisterEventHandler would fail.  So: *)
  Login();
END LocalRuleHandler.
