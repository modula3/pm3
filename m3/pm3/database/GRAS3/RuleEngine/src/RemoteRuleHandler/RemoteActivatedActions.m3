MODULE RemoteActivatedActions;

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

    Revision 1.1  1997/10/31 14:05:05  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT Event, ContextSet, Action, RemoteActiveAction,
       RemoteActiveActionPQueue;

VAR
  Queue            := NEW(RemoteActiveActionPQueue.T).init();
  Time  : CARDINAL := 0;
  Access           := NEW(MUTEX);

PROCEDURE Store (event   : Event.T;
                 context : ContextSet.T;
                 priority: CARDINAL;
                 act     : Action.T;
                 userdata: <*TRANSIENT*> REFANY        ) =
  VAR aact: RemoteActiveAction.T;
  BEGIN
    LOCK Access DO
      INC(Time);
      aact := NewActiveAction();
      aact^ :=
        RemoteActiveAction.Struct{
          priority := RemoteActiveAction.PriorityType{priority, Time},
          action := act, event := event, userdata := userdata, context :=
          context, next := NIL};
      Queue.insert(aact);
    END;
  END Store;

PROCEDURE Get (VAR event   : Event.T;
               VAR context : ContextSet.T;
               VAR act     : Action.T;
               VAR userdata: <*TRANSIENT*> REFANY        ): BOOLEAN =
  VAR action: RemoteActiveAction.T;
  BEGIN
    LOCK Access DO
      IF NOT Queue.isEmpty() THEN
        action := Queue.get();
        event := action.event;
        context := action.context;
        act := action.action;
        userdata := action.userdata;
        DisposeActiveAction(action);
        RETURN TRUE;
      END;
      RETURN FALSE;
    END;
  END Get;

PROCEDURE Highest (): CARDINAL =
  BEGIN
    LOCK Access DO RETURN Queue.highest().prio; END;
  END Highest;

PROCEDURE NotEmpty (): BOOLEAN =
  BEGIN
    LOCK Access DO RETURN NOT Queue.isEmpty(); END;
  END NotEmpty;

VAR FreeActiveActions: RemoteActiveAction.T := NIL;

PROCEDURE NewActiveAction (): RemoteActiveAction.T =
  VAR new: RemoteActiveAction.T;
  BEGIN
    IF FreeActiveActions = NIL THEN
      RETURN NEW(RemoteActiveAction.T);
    ELSE
      new := FreeActiveActions;
      FreeActiveActions := new.next;
      RETURN new;
    END;
  END NewActiveAction;

PROCEDURE DisposeActiveAction (act: RemoteActiveAction.T) =
  BEGIN
    act^ :=
      RemoteActiveAction.Struct{RemoteActiveAction.PriorityType{0, 0}, NIL,
                                NIL, ContextSet.Empty(), NIL, NIL};
    act.next := FreeActiveActions;
    FreeActiveActions := act;
  END DisposeActiveAction;

BEGIN
END RemoteActivatedActions.
