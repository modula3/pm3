(*! DATA OBJECT MODULE *)
INTERFACE RuleClient;

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

    Revision 1.2  1997/11/03 12:40:36  roland
    New procedures to check connection to rule server.

    Revision 1.1  1997/10/31 14:05:15  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT RuleEngineCallback, Event, Action, ContextSet, Trigger;

EXCEPTION CommError;

PROCEDURE Connect (host, id: TEXT; callback: RuleEngineCallback.T;
                   VAR ok: BOOLEAN; VAR error: TEXT);
  (* Tries to connect to RuleServer via name server on host *)

PROCEDURE Disconnect ();
  (* Unregisters client from rule server *)

PROCEDURE CheckConnection (VAR connected: BOOLEAN; VAR msg: TEXT);
  (* checks connection to the server *)
  
PROCEDURE RegisterTrigger (trigger: Trigger.T; userdata: <*TRANSIENT*> REFANY;  id: CARDINAL)
  RAISES {CommError};
  (* Registers trigger with the rule server.  The id identifies the trigger
     locally. *)

PROCEDURE UnregisterTrigger (id: CARDINAL) RAISES {CommError};
  (* Unregisters trigger with id *)

PROCEDURE SendAction (event  : Event.T;
                      context: ContextSet.T;
                      action : Action.T      ) RAISES {CommError};
  (* Report action to server *)

END RuleClient.
