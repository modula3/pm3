(*! DATA OBJECT MODULE *)
INTERFACE TriggerMap;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:05:20  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

PROCEDURE Bind(local, remote: CARDINAL);
PROCEDURE RemoveWithLocal(local: CARDINAL);
PROCEDURE RemoveWithRemote(remote: CARDINAL);

PROCEDURE GetLocal(remote: CARDINAL; VAR local: CARDINAL): BOOLEAN;
PROCEDURE GetRemote(local: CARDINAL; VAR remote: CARDINAL): BOOLEAN;
  (* Return value is true iff a binding exists *)

END TriggerMap.
