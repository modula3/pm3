MODULE RuleEngine;

(** This is the main interface of subsystem RuleEngine. *)
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

    Revision 1.5  1998/08/12 11:04:47  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.4  1997/11/12 17:23:13  roland
    WaitForRemoteActions now does a Thread.AlertWait, so that the waiting
    thread can be interrupted.

    Revision 1.3  1997/11/12 15:33:29  roland
    Bugfixes in support for specialized EventHandlers.

    Revision 1.2  1997/11/03 12:40:27  roland
    New procedures to check connection to rule server.

    Revision 1.1  1997/10/31 14:03:01  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling.

*)
(***************************************************************************)

IMPORT CardSeq;
IMPORT Trigger, EventDetector, EventHandler, Event, Action, ContextSet;
IMPORT LocalRuleHandler, RemoteRuleHandler;
IMPORT Thread;

(* ------------ Initialization and set up --------------- *)

PROCEDURE Login (    host, id        : TEXT;
                 VAR remoteMonitoring: BOOLEAN;
                 VAR error           : TEXT     ) =
  (* Initializes the rule engine.  The rule engine tries to connect to the
     rule server via name server on machine 'host' registered with 'id'.
     The return value remoteMonitoring indicates whether this connection
     was successful.  If not, 'error' states a reason in readable form.
     Even if remoteMonitoring is FALSE local event handling will work
     without problem. *)
  BEGIN
    LocalRuleHandler.Login();
    RemoteRuleHandler.Login(
      host, id, NotifyRemoteAction, remoteMonitoring, error);
    (* Create first transaction unit for remote transactions *)
    RemoteUnit := LocalRuleHandler.RegisterTransactionUnit();
    NewTransactionUnit(RemoteUnit);
  END Login;

PROCEDURE Logout () =
  (* Disconnect from the rule server *)
  BEGIN
    RemoteRuleHandler.Logout();
  END Logout;

PROCEDURE CheckRuleServer (VAR connected: BOOLEAN; VAR msg: TEXT) =
  BEGIN
    RemoteRuleHandler.CheckRuleServer(connected, msg);
  END CheckRuleServer;
  
PROCEDURE RegisterTransactionUnit (): CARDINAL =
  BEGIN
    WITH tu = LocalRuleHandler.RegisterTransactionUnit() DO
      NewTransactionUnit(tu);
      RETURN tu;
    END;
  END RegisterTransactionUnit;

PROCEDURE UnregisterTransactionUnit (unit: CARDINAL) =
  BEGIN
    DeleteTransactionUnit(unit);
    LocalRuleHandler.UnregisterTransactionUnit(unit);
  END UnregisterTransactionUnit;

PROCEDURE RegisterEventDetector (detector: EventDetector.T; types: CardSeq.T) =
  BEGIN
    LocalRuleHandler.RegisterEventDetector(detector, types);
  END RegisterEventDetector; 

PROCEDURE RegisterEventHandler (handler: EventHandler.T; types: CardSeq.T) =
  (* Register handler as event handler for the event types in types.  All
     occurring events of these types will be passed to handler after
     calling this procedure.  Note: It is not checked whether another
     handler for some of the types already is registered. *)
  BEGIN
    LocalRuleHandler.RegisterEventHandler(handler, types);
  END RegisterEventHandler;

(* ---------------- Remote signaled events -------------- *)

VAR
  NotificationWhen: NotificationTime;
  NotifyCallback  : Notifier         := NIL;
  ArrivedActions                     := NEW(Thread.Condition);

PROCEDURE NotifyRemoteAction () =
  BEGIN
    IF NotifyCallback # NIL THEN
      IF NotificationWhen = NotificationTime.Always THEN
        NotifyCallback();
      ELSE
        IF UnitLevel(RemoteUnit) = 0 THEN NotifyCallback(); END;
      END;
    END;
    Thread.Broadcast(ArrivedActions);
  END NotifyRemoteAction;

PROCEDURE WaitForRemoteActions (lock: MUTEX) RAISES {Thread.Alerted} =
  BEGIN
    LOCK lock DO Thread.AlertWait(lock, ArrivedActions); END;
  END WaitForRemoteActions;

PROCEDURE RegisterRemoteActionNotifier (n: Notifier; when: NotificationTime) =
  (* When the rule server signals an event, the client might not be
     executing GRAS operations, so that the triggered actions are never
     executed (because the execution cycle is not run after a commit to the
     top-level transaction).  If a notifier is registered, the RuleEngine
     calls this procedure if remote events are signaled.  notifier is
     called either every time a remote event arrives
     (NotificationTime.Always) or only if no transaction is active
     (NotificationTime.OutsideTransaction).  Note: notification takes is
     done a separate thread.  If you want the main thread to execute the
     triggered actions, you must propagate the notification to the main
     thread via signals or some other mechanism (writing on a file
     descriptor is a very popular method for notifying window managers). *)
  BEGIN
    NotificationWhen := when;
    NotifyCallback := n;
  END RegisterRemoteActionNotifier;

PROCEDURE ExecuteActions (unit: CARDINAL) =
  (* Inside a transaction, if action execution is not delayed, the action
     execution cycle is started.  This should have no effect at all.
     Outside transaction, the decoupled cycle is started, which will also
     execute remote actions.  This is especially useful, when the program
     was notified about remote actions and normal operation wouldn't
     eventually start action execution automatically. *)
  BEGIN
    IF UnitLevel(unit) > 0 THEN
      IF NOT IsDelayed(RemoteUnit) AND NOT ExecutingActions() THEN
        InitExecution();
        ImmediateExecutionCycle(RemoteUnit);
        FinishExecution();
      END;
    ELSE
      IF NOT ExecutingActions() THEN
        InitExecution();
        DecoupledExecutionCycle(RemoteUnit);
        FinishExecution();
      END;
    END;
  END ExecuteActions;

PROCEDURE ExecuteRemoteActions () =
  (* If action execution is not delayed, a special action execution cycle
     is started.  The cycle handles remote actions right after immediate
     actions.  This is especially useful, when the program was notified
     about remote actions and these should be executed immediately. *)
  BEGIN
    IF UnitLevel(RemoteUnit) > 0 THEN
      IF NOT ExecutingActions() THEN
        InitExecution();
        RemoteExecutionCycle();
        FinishExecution();
      END;
    ELSE
      IF NOT ExecutingActions() THEN
        InitExecution();
        DecoupledExecutionCycle(RemoteUnit);
        FinishExecution();
      END;
    END;
  END ExecuteRemoteActions;

(* ---------------------- Triggers ---------------------- *)


PROCEDURE RegisterTrigger (trigger : Trigger.T;
                           interest: Interest;
                           userdata: <*TRANSIENT*> REFANY := NIL): CARDINAL =
  TYPE InterestSet = SET OF Interest;
  CONST
    LocalInterests  = InterestSet{Interest.Self, Interest.All};
    RemoteInterests = InterestSet{Interest.Others, Interest.All};
  (* Registers trigger with the corresponding event handler.  The returned
     number serves as unique identifier for this trigger.  If interest is
     Self, only local events are monitored.  Interest.Others specifies to
     monitor only remote events and Interst.All monitors both. *)
  VAR id: CARDINAL;
  BEGIN
    id := LocalRuleHandler.NextTriggerId();
    IF interest IN LocalInterests THEN
      LocalRuleHandler.RegisterTrigger(trigger, userdata, id);
    END;
    IF interest IN RemoteInterests THEN
      RemoteRuleHandler.RegisterTrigger(trigger, userdata, id);
    END;
    RETURN id;
  END RegisterTrigger;

PROCEDURE UnregisterTrigger (id: CARDINAL) =
  (* Unregisters trigger with id *)
  BEGIN
    (* Unregistering a not registered trigger doesn't hurt either
       handler *)
    LocalRuleHandler.UnregisterTrigger(id);
    RemoteRuleHandler.UnregisterTrigger(id);
  END UnregisterTrigger;

(* ---------------------- Contexts ---------------------- *)

PROCEDURE ActivateContext (tu: CARDINAL; context: CARDINAL) =
  (* Activates context as declared with ContextSet.  Only triggers will be
     fired which inhibiting contexts are all inactive and permitting
     contexts are all active.  Use ContextSet interface to declare new
     contexts and handle context sets. *)
  BEGIN
    LocalRuleHandler.ActivateContext(tu, context);
  END ActivateContext;

PROCEDURE DeactivateContext (tu: CARDINAL; context: CARDINAL) =
  (* Deactivates context as declared with ContextSet. *)
  BEGIN
    LocalRuleHandler.DeactivateContext(tu, context);
  END DeactivateContext;


(* --------------------- Event signaling ---------------- *)

PROCEDURE Signal (tu: CARDINAL; event: Event.T) =
  (* Signals event.  All matching triggers are executed (or collected for
     later execution, depending on coupling mode etc.  Cf.  below. *)
  BEGIN
    LocalRuleHandler.Handle(tu, event, UnitLevel(tu));
    IF NOT IsDelayed(tu) THEN
      IF NOT ExecutingActions() THEN
        InitExecution();
        IF UnitLevel(tu) = 0 THEN
          DecoupledExecutionCycle(tu);
        ELSE
          ImmediateExecutionCycle(tu);
        END;
        FinishExecution();
      END;
    END;
  END Signal;

PROCEDURE DelayActionExecution (tu: CARDINAL) =
  (* Delay execution of triggered rules until commit of the current
     (nested) transaction.  Note: Following DelayActionExecution calls have
     no effect until the current transaction commits ore aborts or a call
     to ReleaseActionExecution succeeds. *)
  BEGIN
    IF NOT IsDelayed(tu) THEN
      DelayUnit(tu);
    END;
  END DelayActionExecution;

PROCEDURE ReleaseActionExecution (tu: CARDINAL) =
  (* If action execution is delayed because of a call to
     DelayActionExecution within the same transaction (level), action
     execution resumes. *)
  BEGIN
    IF IsDelayed(tu) AND UnitLevel(tu) = DelayLevel(tu) THEN
      ReleaseUnit(tu);
      IF NOT ExecutingActions() THEN
        InitExecution();
        ImmediateExecutionCycle(tu);
        FinishExecution();
      END;
    END;
  END ReleaseActionExecution;

(* --------------------- Transactions ------------------- *)

(* Note: The following procedures are called by GRAS automatically.  You
   should not use them unless you know very well what you are doing. *)

PROCEDURE NotifyBeginTransaction (tu: CARDINAL) =
  (* Increments trnasaction level *)
  BEGIN
    IncUnitLevel(tu);
  END NotifyBeginTransaction;

PROCEDURE PreCommitTransaction (tu: CARDINAL) =
  (* Executes all deferred actions, except when action execution is
     delayed. *)
  BEGIN
    IF UnitLevel(tu) > 0 THEN
      IF IsDelayed(tu) AND DelayLevel(tu) >= UnitLevel(tu) THEN
        ReleaseUnit(tu);
      END;
      IF NOT IsDelayed(tu) THEN
        IF NOT ExecutingActions() THEN
          InitExecution();
          DeferredExecutionCycle(tu);
          FinishExecution();
        END;
      END;
    END;
  END PreCommitTransaction;

PROCEDURE PostCommitTransaction (tu: CARDINAL) =
  (* Decrements transaction level.  If level is 0 after decrement, all
     decoupled action are executed. *)
  BEGIN
    IF UnitLevel(tu) > 0 THEN
      DecUnitLevel(tu);
      IF UnitLevel(tu) = 0 THEN
        IF NOT ExecutingActions() THEN
          InitExecution();
          DecoupledExecutionCycle(tu);
          FinishExecution();
        END;
      END;
    END;
  END PostCommitTransaction;

PROCEDURE NotifyChainTransaction (tu: CARDINAL) =
  (* Executes all deferred actions, except when action execution is
     delayed. *)
  BEGIN
    IF UnitLevel(tu) > 0 THEN
      IF IsDelayed(tu) AND DelayLevel(tu) >= UnitLevel(tu) THEN
        ReleaseUnit(tu);
      END;
      IF NOT IsDelayed(tu) THEN
        IF NOT ExecutingActions() THEN
          InitExecution();
          DeferredExecutionCycle(tu);
          FinishExecution();
        END;
      END;
    END;
  END NotifyChainTransaction;

PROCEDURE NotifyAbortTransaction (tu: CARDINAL) =
  (* Cancels all actions which were triggered by this or any nested
     transaction *)
  BEGIN
    LocalRuleHandler.KillTransaction(tu, UnitLevel(tu));
    DecUnitLevel(tu);
  END NotifyAbortTransaction;

(* Internal procedures *)

VAR
  InExecutionCycle: BOOLEAN  := FALSE;
  ExecutedActions : CARDINAL := 0;

PROCEDURE ExecutingActions (): BOOLEAN =
  BEGIN
    RETURN InExecutionCycle;
  END ExecutingActions;

PROCEDURE InitExecution () =
  BEGIN
    ExecutedActions := 0;
    InExecutionCycle := TRUE;
  END InitExecution;

PROCEDURE FinishExecution () =
  BEGIN
    InExecutionCycle := FALSE;
  END FinishExecution;

PROCEDURE ImmediateExecutionCycle (tu: CARDINAL) =
  CONST immediate = Trigger.CouplingMode.Immediate;
  VAR
    event   : Event.T;
    context : ContextSet.T;
    level   : CARDINAL;
    action  : Action.T;
    userdata: <*TRANSIENT*> REFANY;
  BEGIN
    WHILE LocalRuleHandler.GetNextAction(tu, 
            immediate, event, context, level, action, userdata) DO
      ExecuteOneAction(action, event, context, userdata, TRUE);
    END;
  END ImmediateExecutionCycle;

PROCEDURE DeferredExecutionCycle (tu: CARDINAL) =
  CONST deferred = Trigger.CouplingMode.Deferred;
  VAR
    event   : Event.T;
    context : ContextSet.T;
    level   : CARDINAL;
    action  : Action.T;
    userdata: <*TRANSIENT*> REFANY;
  BEGIN
    ImmediateExecutionCycle(tu);
    WHILE LocalRuleHandler.GetNextAction(
            tu, deferred, event, context, level, action, userdata) DO
      ExecuteOneAction(action, event, context, userdata, TRUE);
      ImmediateExecutionCycle(tu);
    END;
  END DeferredExecutionCycle;

PROCEDURE DecoupledExecutionCycle (tu: CARDINAL) =
  CONST decoupled = Trigger.CouplingMode.Decoupled;
  VAR
    event   : Event.T;
    context : ContextSet.T;
    level   : CARDINAL;
    action  : Action.T;
    userdata: <*TRANSIENT*> REFANY;
  BEGIN
    LOOP
      IF LocalRuleHandler.GetNextAction(
           tu, decoupled, event, context, level, action, userdata) THEN
        ExecuteOneAction(action, event, context, userdata, TRUE);
      ELSIF RemoteRuleHandler.GetNextAction(
              event, context, action, userdata) THEN
        ExecuteOneAction(action, event, context, userdata, FALSE);
      ELSE
        EXIT;
      END;
      DeferredExecutionCycle(tu);
    END;
  END DecoupledExecutionCycle;

PROCEDURE RemoteExecutionCycle () =
  VAR
    event   : Event.T;
    context : ContextSet.T;
    action  : Action.T;
    userdata: <*TRANSIENT*> REFANY;
  BEGIN
    LOOP
      ImmediateExecutionCycle(RemoteUnit);
      IF RemoteRuleHandler.GetNextAction(event, context, action, userdata) THEN
        ExecuteOneAction(action, event, context, userdata, FALSE);
      ELSE
        EXIT;
      END;
    END;
  END RemoteExecutionCycle;

PROCEDURE ExecuteOneAction (action  : Action.T;
                            event   : Event.T;
                            context : ContextSet.T;
                            userdata: <*TRANSIENT*> REFANY;
                            local   : BOOLEAN       ) =
  BEGIN
    TYPECASE action OF
      Action.Remote =>
        RemoteRuleHandler.SendRemoteAction(event, context, action);
    | Action.Local (la) => la.proc()(event, context, local, userdata);
    ELSE
      <* ASSERT FALSE *>
    END
  END ExecuteOneAction;

(* ------------------ Transaction Units ------------------ *)

CONST TUnitsInitLen = 10;

TYPE
  TransactionUnitInfo =
    RECORD
      level  : CARDINAL;  (* transaction level *)
      delLevel: CARDINAL; (* transaction level when action execution were delayed *)
      delayed: BOOLEAN;   (* action execution *)
    END;

VAR TUnits := NEW(<*TRANSIENT*> REF ARRAY OF TransactionUnitInfo,
                  TUnitsInitLen);

PROCEDURE ExtendTUnits () =
  VAR
    len := NUMBER(TUnits^);
    n   := NEW(<*TRANSIENT*> REF ARRAY OF TransactionUnitInfo, 2 * len);
  BEGIN
    SUBARRAY(n^, 0, len) := TUnits^;
    FOR i := len TO 2*len - 1 DO
      n^[i] := TransactionUnitInfo{0, 0, FALSE};
    END;
    TUnits := n;
  END ExtendTUnits;

PROCEDURE NewTransactionUnit (tu: CARDINAL) =
  BEGIN
    WHILE tu > LAST(TUnits^) DO ExtendTUnits(); END;
    TUnits^[tu] := TransactionUnitInfo{0, 0, FALSE};
  END NewTransactionUnit;

PROCEDURE DeleteTransactionUnit(tu: CARDINAL) =
  BEGIN
    TUnits^[tu] := TransactionUnitInfo{0, 0, FALSE};
  END DeleteTransactionUnit;

PROCEDURE UnitLevel (tu: CARDINAL): CARDINAL =
  BEGIN
    RETURN TUnits^[tu].level;
  END UnitLevel; 

PROCEDURE IncUnitLevel (tu: CARDINAL) =
  BEGIN
    INC(TUnits^[tu].level);
  END IncUnitLevel; 

PROCEDURE DecUnitLevel (tu: CARDINAL) =
  BEGIN
    DEC(TUnits^[tu].level);
  END DecUnitLevel;

PROCEDURE DelayLevel(tu: CARDINAL): CARDINAL =
  BEGIN
    RETURN TUnits^[tu].delLevel;
  END DelayLevel;
  
PROCEDURE IsDelayed (tu: CARDINAL): BOOLEAN =
  BEGIN
    RETURN TUnits^[tu].delayed;
  END IsDelayed;

PROCEDURE DelayUnit (tu: CARDINAL) =
  BEGIN
    TUnits^[tu].delayed := TRUE;
    TUnits^[tu].delLevel := TUnits^[tu].level;
  END DelayUnit;

PROCEDURE ReleaseUnit (tu: CARDINAL) =
  BEGIN
    TUnits^[tu].delayed := FALSE;
  END ReleaseUnit;

VAR RemoteUnit: CARDINAL;
  
BEGIN
END RuleEngine.
