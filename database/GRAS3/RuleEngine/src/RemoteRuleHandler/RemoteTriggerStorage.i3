(*! DATA OBJECT MODULE *)
INTERFACE RemoteTriggerStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:05:12  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT Action;

(* Maps trigger ids to their event types, priorities and local actions. *)

PROCEDURE Store (id       : CARDINAL;
                 eventType: TEXT;
                 priority : CARDINAL;
                 action   : Action.T;
                 userdata : REFANY);

PROCEDURE Remove (id: CARDINAL);

PROCEDURE Find (    id      : CARDINAL;
                VAR type    : TEXT;
                VAR priority: CARDINAL;
                VAR action  : Action.T;
                VAR userdata: REFANY): BOOLEAN;

END RemoteTriggerStorage.
