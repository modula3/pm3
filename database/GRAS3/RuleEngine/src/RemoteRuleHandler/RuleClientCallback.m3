MODULE RuleClientCallback;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.2  1998/09/14 08:15:27  roland
    Modified code to remove compiler warnings.

    Revision 1.1  1997/10/31 14:05:19  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT RuleEngineCallback, IntIntTbl, IntSeq, IntTextTbl, TextSeq,
       CardRelation, Thread;
IMPORT Event, EventPattern, Trigger, Action, LocalRuleHandler,
       EventTranslation, TriggerMap, RemoteActivatedActions,
       RemoteTriggerStorage, ContextSet;

TYPE
  RuleClient =
    RuleEngineCallback.T BRANDED OBJECT
      clientTriggerMap: CardRelation.T;  (* map client numbers to local
                                            trigger ids *)
    METHODS
      init (): RuleClient := InitClient;
    OVERRIDES
      registerTrigger   := RegisterTrigger;
      unregisterTrigger := UnregisterTrigger;
      killClient        := KillClient;
      notifyEvent       := NotifyEvent;
    END;

PROCEDURE InitClient (client: RuleClient): RuleClient =
  BEGIN
    client.clientTriggerMap := NEW(CardRelation.T).init();
    RETURN client;
  END InitClient;

PROCEDURE RegisterTrigger (client   : RuleClient;
                           id       : CARDINAL;
                           clientno : CARDINAL;
                           pType    : TEXT;
                           pBools   : IntIntTbl.T;
                           pInts    : IntIntTbl.T;
                           pTexts   : IntTextTbl.T;
                           coupling : CARDINAL;
                           priority : CARDINAL;
                           inh, perm: TextSeq.T     ) RAISES {} =

  PROCEDURE CouplingMode (c: CARDINAL): Trigger.CouplingMode =
    BEGIN
      IF c = RuleEngineCallback.ImmediateCoupling THEN
        RETURN Trigger.CouplingMode.Immediate;
      ELSIF c = RuleEngineCallback.DeferredCoupling THEN
        RETURN Trigger.CouplingMode.Deferred;
      ELSE
        RETURN Trigger.CouplingMode.Decoupled;
      END;
    END CouplingMode;

  VAR
    pattern        : EventPattern.T;
    action         : Action.T;
    trigger        : Trigger.T;
    localId        : CARDINAL;
    inhibit, permit: ContextSet.T;
  BEGIN
    (* Monitor events according to the parameters (see RuleEngineServer).
       Use id to report occurred events back to the server. *)
    TRY
      pattern :=
        EventTranslation.ComposePattern(pType, pBools, pInts, pTexts);
      inhibit := ContextSet.FromSeq(inh);
      permit := ContextSet.FromSeq(perm);
      localId := LocalRuleHandler.NextTriggerId();
      action := NEW(Action.Remote).init(clientno, id);
      trigger := Trigger.Create(pattern, action, CouplingMode(coupling),
                                priority, inhibit, permit);
      TriggerMap.Bind(localId, id);
      client.clientTriggerMap.insert(clientno, localId);
      LocalRuleHandler.RegisterTrigger(trigger, NIL, localId);
    EXCEPT
      EventTranslation.UnknownType, EventTranslation.AttributeMismatch,
          ContextSet.Unknown =>
      (* ignore, don't register trigger *)
    END;
  END RegisterTrigger;

PROCEDURE UnregisterTrigger (client: RuleClient; trigger: CARDINAL)
  RAISES {} =
  VAR
    localId: CARDINAL;
    cl     : CARDINAL;
    found  : BOOLEAN;
  BEGIN
    (* Stop Monitoring trigger. *)
    IF TriggerMap.GetLocal(trigger, localId) THEN
      LocalRuleHandler.UnregisterTrigger(localId);
      TriggerMap.RemoveWithLocal(localId);
      cl := client.clientTriggerMap.singleQueryProjection1(localId, found);
      IF found THEN
        client.clientTriggerMap.deleteElement(cl, localId);
      END;
    END;
  END UnregisterTrigger;


PROCEDURE KillClient (client: RuleClient; clientno: CARDINAL) RAISES {} =
  VAR
    delTriggers            := NEW(IntSeq.T).init();
    tclient, tid: CARDINAL;
    found       : BOOLEAN;
  BEGIN
    (* Kill all triggers and activated actions for client. *)
    client.clientTriggerMap.loop();
    client.clientTriggerMap.get(tclient, tid, found);
    (* collect all trigger ids of client *)
    WHILE found DO
      IF tclient = clientno THEN delTriggers.addhi(tid) END;
      client.clientTriggerMap.get(tclient, tid, found);
    END;
    FOR i := 0 TO delTriggers.size() - 1 DO
      tid := delTriggers.get(i);
      (* remove from local trigger storage *)
      LocalRuleHandler.UnregisterTrigger(tid);
      (* remove from client map *)
      client.clientTriggerMap.deleteElement(clientno, tid);
      (* remove from trigger map *)
      TriggerMap.RemoveWithLocal(tid);
    END;
    (* kill all activated actions for client *)
    LocalRuleHandler.KillClientActions(clientno);
  END KillClient;


PROCEDURE NotifyEvent (<* UNUSED *> client    : RuleClient;
                                    trigger   : CARDINAL;
                       <* UNUSED *> clientno  : CARDINAL;
                                    eBools    : IntIntTbl.T;
                                    eInts     : IntIntTbl.T;
                                    eTexts    : IntTextTbl.T;
                                    contextSeq: TextSeq.T     ) RAISES {} =
  VAR
    event       : Event.T;
    context     : ContextSet.T;
    type        : TEXT;
    action      : Action.T;
    localTrigger: CARDINAL;
    priority    : CARDINAL;
    userdata    : REFANY;
  BEGIN
    (* clientno has monitored the event described by the parameters. *)
    TRY
      IF TriggerMap.GetLocal(trigger, localTrigger) THEN
        IF RemoteTriggerStorage.Find(
             localTrigger, type, priority, action, userdata) THEN
          event := EventTranslation.ComposeEvent(
                     type, eBools, eInts, eTexts);
          context := ContextSet.FromSeq(contextSeq);
          RemoteActivatedActions.Store(event, context, priority, action, userdata);
          Thread.Signal(ActionsArived);
        END;
      END;
    EXCEPT
      EventTranslation.UnknownType, EventTranslation.AttributeMismatch,
          ContextSet.Unknown =>
      (* ignore, don't notify event *)
    END;
  END NotifyEvent;

PROCEDURE NotifierApply (<* UNUSED *> cl: Thread.Closure): REFANY =
  BEGIN
    LOOP
      LOCK NotifyLock DO
        Thread.Wait(NotifyLock, ActionsArived);
        IF Notification # NIL THEN Notification(); END;
      END;
    END;
  END NotifierApply;

VAR
  ClientObj    : RuleClient;
  Notification : ActionsArivedCallback;
  Notifier     : Thread.T;
  ActionsArived: Thread.Condition;
  NotifyLock   : MUTEX;

PROCEDURE Init (n: ActionsArivedCallback): RuleEngineCallback.T =
  BEGIN
    Notification := n;
    ActionsArived := NEW(Thread.Condition);
    NotifyLock := NEW(MUTEX);
    Notifier := Thread.Fork(NEW(Thread.Closure, apply := NotifierApply));

    ClientObj := NEW(RuleClient).init();
    RETURN ClientObj;
  END Init;

BEGIN
END RuleClientCallback.
