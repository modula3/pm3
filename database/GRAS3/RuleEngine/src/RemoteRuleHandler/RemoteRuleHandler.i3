(*!  DATA OBJECT MODULE *)
INTERFACE RemoteRuleHandler;

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

    Revision 1.2  1997/11/03 12:40:34  roland
    New procedures to check connection to rule server.

    Revision 1.1  1997/10/31 14:05:09  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT Trigger, Event, Action, ContextSet;

(* Initialization and set up*)

PROCEDURE Login (    host, id             : TEXT;
                     arrivedActionCallback: PROCEDURE ();
                 VAR ok                   : BOOLEAN;
                 VAR error                : TEXT          );
  (* Tries to connect to a rule server with id as identifier via a
     nameserver specified by host.  If this is successful, ok will be set
     to TRUE an error contains something like "No error".  Otherwise ok
     will be FALSE and all other calls to this interface will have no
     effect at all.  The procedure arrivedActionCallback will be called
     everytime the rule server reports an event (if NIL, no notification is
     done). *)

PROCEDURE Logout ();
  (* Closes connection to rule server *)

PROCEDURE CheckRuleServer (VAR connected: BOOLEAN; VAR msg: TEXT);
  (* Check connection to the rule server. If not connected, msg describes
     the error in readable form. *)
  
(* Trigger handling *)

PROCEDURE RegisterTrigger (trigger : Trigger.T;
                           userdata: <*TRANSIENT*> REFANY;
                           id      : CARDINAL   );
  (* Registers trigger with the corresponding event handler.  The id
     servers as unique identifier for this trigger. *)

PROCEDURE UnregisterTrigger (id: CARDINAL);
  (* Unregisters trigger with id *)

(* Event handling *)

PROCEDURE SendRemoteAction (event  : Event.T;
                            context: ContextSet.T;
                            action : Action.T      );
  (* Notifies rulr server about triggered action.  action must be of type
     Action.Remote. *)

PROCEDURE GetNextAction (VAR event   : Event.T;
                         VAR context : ContextSet.T;
                         VAR action  : Action.T;
                         VAR userdata: <*TRANSIENT*> REFANY        ): BOOLEAN;
  (* Returns TRUE if events were reported by the rule server.  action then
     contains the triggered action with highest priority. *)


END RemoteRuleHandler.
