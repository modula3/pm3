MODULE EventHandler EXPORTS EventHandler, EventHandlerClass;

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

    Revision 1.3  1998/08/12 11:05:00  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.2  1997/11/12 15:33:38  roland
    Bugfixes in support for specialized EventHandlers.

    Revision 1.1  1997/10/31 14:04:01  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The event handler
    subsystem implements the mapping between events and action via triggers.

*)
(***************************************************************************)

IMPORT Trigger, TriggerStorage, ActivatedActions, Action, Event, ContextSet;

REVEAL
  T = Private BRANDED OBJECT
      OVERRIDES
        init                := Init;
        storeTrigger        := StoreTrigger;
        removeTrigger       := RemoveTrigger;
        handle              := Handle;
        getNextAction       := GetNextAction;
        highestPriority     := HighestPriority;
        hasActivatedActions := HasActivatedActions;
        killClientActions   := KillClientActions;
        killTransaction     := KillTransaction;
        delTransactionUnit  := DelTransactionUnit;
        newTransactionUnit  := NewTransactionUnit;
      END;


PROCEDURE Init (eh: T; ts: TriggerStorage.T): T =
  CONST InitialLength = 10;
  BEGIN
    eh.ts := ts;
    eh.aa := NEW(<*TRANSIENT*> REF ARRAY OF ActionStorage, InitialLength);
    RETURN eh;
  END Init;

PROCEDURE NewTransactionUnit (eh: T; tu: CARDINAL) =
  BEGIN
    WHILE tu > LAST(eh.aa^) DO
      VAR
        len := NUMBER(eh.aa^);
        n   := NEW(<*TRANSIENT*> REF ARRAY OF ActionStorage, 2 * len);
      BEGIN
        SUBARRAY(n^, 0, len) := eh.aa^;
        FOR i := len TO 2 * len - 1 DO
          n^[i] := ActionStorage{NIL, NIL, NIL};
        END;
        eh.aa := n;
      END;
    END;
    eh.aa^[tu][Trigger.CouplingMode.Immediate] :=
      NEW(ActivatedActions.Default).init();
    eh.aa^[tu][Trigger.CouplingMode.Deferred] :=
      NEW(ActivatedActions.Default).init();
    eh.aa^[tu][Trigger.CouplingMode.Decoupled] :=
      NEW(ActivatedActions.Default).init();
  END NewTransactionUnit;

PROCEDURE DelTransactionUnit (eh: T; tu: CARDINAL) =
  BEGIN
    eh.aa^[tu][Trigger.CouplingMode.Immediate] := NIL;
    eh.aa^[tu][Trigger.CouplingMode.Deferred] := NIL;
    eh.aa^[tu][Trigger.CouplingMode.Decoupled] := NIL;
  END DelTransactionUnit;

PROCEDURE StoreTrigger (eh: T; t: Trigger.T; userdata: <*TRANSIENT*> REFANY; id: CARDINAL) =
  BEGIN
    eh.ts.storeTrigger(t, userdata, id);
  END StoreTrigger;

PROCEDURE RemoveTrigger (eh: T; id: CARDINAL; VAR type: CARDINAL) =
  BEGIN
    eh.ts.removeTrigger(id, type);
  END RemoveTrigger;

PROCEDURE Handle (eh              : T;
                  tu              : CARDINAL;
                  e               : Event.T;
                  context         : ContextSet.T;
                  transactionLevel: CARDINAL      ) =
  VAR
    a       : Action.T;
    p       : CARDINAL;
    c       : Trigger.CouplingMode;
    userdata: <*TRANSIENT*> REFANY;
  BEGIN
    eh.ts.notifyEvent(e, context);
    WHILE eh.ts.getNextAction(a, c, p, userdata) DO
      eh.aa^[tu][c].store(e, context, transactionLevel, p, a, userdata);
    END;
  END Handle;

PROCEDURE GetNextAction (    eh              : T;
                             tu              : CARDINAL;
                             coupling        : Trigger.CouplingMode;
                         VAR event           : Event.T;
                         VAR context         : ContextSet.T;
                         VAR transactionLevel: CARDINAL;
                         VAR action          : Action.T;
                         VAR userdata        : <*TRANSIENT*> REFANY ):
  BOOLEAN =
  BEGIN
    RETURN eh.aa^[tu][coupling].get(
             event, context, transactionLevel, action, userdata);
  END GetNextAction;

PROCEDURE HighestPriority (eh      : T;
                           tu      : CARDINAL;
                           coupling: Trigger.CouplingMode): CARDINAL =
  BEGIN
    RETURN eh.aa^[tu][coupling].highest();
  END HighestPriority;

PROCEDURE HasActivatedActions (eh      : T;
                               tu      : CARDINAL;
                               coupling: Trigger.CouplingMode): BOOLEAN =
  BEGIN
    RETURN eh.aa^[tu][coupling].notEmpty();
  END HasActivatedActions;

PROCEDURE KillClientActions (eh: T; client: CARDINAL) =
  BEGIN
    FOR i := 0 TO LAST(eh.aa^) DO
      IF eh.aa^[i][Trigger.CouplingMode.Immediate] # NIL THEN
        eh.aa^[i][Trigger.CouplingMode.Immediate].killClient(client);
        eh.aa^[i][Trigger.CouplingMode.Deferred].killClient(client);
        eh.aa^[i][Trigger.CouplingMode.Decoupled].killClient(client);
      END;
    END;
  END KillClientActions;

PROCEDURE KillTransaction (eh: T; tu: CARDINAL; level: CARDINAL) =
  BEGIN
    eh.aa^[tu][Trigger.CouplingMode.Immediate].killTransaction(level);
    eh.aa^[tu][Trigger.CouplingMode.Deferred].killTransaction(level);
    eh.aa^[tu][Trigger.CouplingMode.Decoupled].killTransaction(level);
  END KillTransaction;

BEGIN
END EventHandler.
