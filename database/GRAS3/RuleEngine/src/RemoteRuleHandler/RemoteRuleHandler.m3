MODULE RemoteRuleHandler;

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

    Revision 1.2  1997/11/03 12:40:35  roland
    New procedures to check connection to rule server.

    Revision 1.1  1997/10/31 14:05:10  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT Trigger, Event, Action, ContextSet;
IMPORT RuleClient, RuleClientCallback, RemoteActivatedActions;

TYPE Proc = PROCEDURE ();

VAR Connected: BOOLEAN := FALSE;

PROCEDURE Login (    host, id             : TEXT;
                     arrivedActionCallback: Proc;
                 VAR ok                   : BOOLEAN;
                 VAR error                : TEXT     ) =
  BEGIN
    WITH callback = RuleClientCallback.Init(arrivedActionCallback) DO
      RuleClient.Connect(host, id, callback, ok, error);
      Connected := ok;
    END;
  END Login;

PROCEDURE Logout () =
  BEGIN
    RuleClient.Disconnect();
  END Logout;

PROCEDURE CheckRuleServer (VAR connected: BOOLEAN; VAR msg: TEXT) =
  BEGIN
    RuleClient.CheckConnection(connected, msg);
  END CheckRuleServer;

(* Trigger handling *)

PROCEDURE RegisterTrigger (trigger : Trigger.T;
                           userdata: <*TRANSIENT*> REFANY;
                           id      : CARDINAL   ) =
  BEGIN
    IF Connected THEN
      TRY
        RuleClient.RegisterTrigger(trigger, userdata, id);
      EXCEPT
        RuleClient.CommError => Connected := FALSE;
      END;
    END;
  END RegisterTrigger;

PROCEDURE UnregisterTrigger (id: CARDINAL) =
  BEGIN
    IF Connected THEN
      TRY
        RuleClient.UnregisterTrigger(id);
      EXCEPT
        RuleClient.CommError => Connected := FALSE;
      END;
    END;
  END UnregisterTrigger;

(* Event handling *)

PROCEDURE SendRemoteAction (event  : Event.T;
                            context: ContextSet.T;
                            action : Action.T      ) =
  BEGIN
    IF Connected THEN
      TRY
        RuleClient.SendAction(event, context, action);
      EXCEPT
        RuleClient.CommError => Connected := FALSE;
      END;
    END;
  END SendRemoteAction;

PROCEDURE GetNextAction (VAR event   : Event.T;
                         VAR context : ContextSet.T;
                         VAR action  : Action.T;
                         VAR userdata: <*TRANSIENT*> REFANY        ): BOOLEAN =
  BEGIN
    RETURN RemoteActivatedActions.Get(event, context, action, userdata);
  END GetNextAction;

BEGIN
END RemoteRuleHandler.
