(*!  DATA OBJECT MODULE *)
INTERFACE RuleClientCallback;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:05:18  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

(* This data object module instantiates an RuleEngineCallback.T.  With this
   it handles the callbacks from the rule server. *)

IMPORT RuleEngineCallback;

TYPE ActionsArivedCallback = PROCEDURE ();

PROCEDURE Init (notify: ActionsArivedCallback): RuleEngineCallback.T;
  (* Initializes the RuleEngineCallback.  Use the returned when registering
     as new client with the rule server.  Everytime the RuleClientCallback
     receives an action from the server it calls notify (if not NIL). *)

END RuleClientCallback.
