MODULE GraphEventHandler;

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

    Revision 1.1  1997/11/12 15:23:57  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

IMPORT Trigger, TriggerStorage, Action, Event, ContextSet, Node, Txn;
IMPORT EventHandler AS Super;
IMPORT EventHandlerClass, GraphTriggerStorage, GraphActivatedActions;

REVEAL
  T = Public BRANDED Brand OBJECT
      OVERRIDES
        init               := Init;
        newTransactionUnit := NewTransactionUnit;
        notifyNodeDeletion := NotifyNodeDeletion;
        handle             := Handle;
      END;

PROCEDURE Init (eh: T; ts: TriggerStorage.T): T =
  BEGIN
    <* ASSERT ISTYPE(ts, GraphTriggerStorage.T) *>
    RETURN Super.T.init(eh, ts);
  END Init;

PROCEDURE NewTransactionUnit (eh: T; tu: CARDINAL) =
  BEGIN
    WHILE tu > LAST(eh.aa^) DO
      VAR
        len := NUMBER(eh.aa^);
        n   := NEW(<*TRANSIENT*> REF ARRAY OF EventHandlerClass.ActionStorage,
                   2 * len);
      BEGIN
        SUBARRAY(n^, 0, len) := eh.aa^;
        FOR i := len TO 2 * len - 1 DO
          n^[i] := EventHandlerClass.ActionStorage{NIL, NIL, NIL};
        END;
        eh.aa := n;
      END;
    END;
    eh.aa^[tu][Trigger.CouplingMode.Immediate] :=
      NEW(GraphActivatedActions.T).init();
    eh.aa^[tu][Trigger.CouplingMode.Deferred] :=
      NEW(GraphActivatedActions.T).init();
    eh.aa^[tu][Trigger.CouplingMode.Decoupled] :=
      NEW(GraphActivatedActions.T).init();
  END NewTransactionUnit;

PROCEDURE NotifyNodeDeletion (eh   : T;
                              tu   : CARDINAL;
                              level: Txn.Level;
                              node : Node.T             ) =
  BEGIN
    NARROW(eh.aa^[tu][Trigger.CouplingMode.Immediate],
           GraphActivatedActions.T).notifyNodeDeletion(level, node);
    NARROW(eh.aa^[tu][Trigger.CouplingMode.Deferred],
           GraphActivatedActions.T).notifyNodeDeletion(level, node);
    NARROW(eh.aa^[tu][Trigger.CouplingMode.Decoupled],
           GraphActivatedActions.T).notifyNodeDeletion(level, node);
  END NotifyNodeDeletion;

PROCEDURE Handle (eh              : T;
                  tu              : CARDINAL;
                  e               : Event.T;
                  context         : ContextSet.T;
                  transactionLevel: CARDINAL      ) =
  VAR
    a                  : Action.T;
    p                  : CARDINAL;
    c                  : Trigger.CouplingMode;
    userdata           : REFANY;
    depsFirst, depsTarg: BOOLEAN;
  BEGIN
    WITH gts = NARROW(eh.ts, GraphTriggerStorage.T) DO
      gts.notifyEvent(e, context);
      WHILE gts.getNextAction(a, depsFirst, depsTarg, c, p, userdata) DO
        NARROW(eh.aa^[tu][c], GraphActivatedActions.T).store(
          e, depsFirst, depsTarg, context, transactionLevel, p, a, userdata);
      END;
    END
  END Handle;

BEGIN
END GraphEventHandler.
