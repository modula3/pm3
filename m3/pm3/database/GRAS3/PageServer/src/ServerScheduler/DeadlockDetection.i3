INTERFACE DeadlockDetection;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.2  1996/08/06 16:32:42  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/11 12:30:48  rbnix
    	First version of functional module to detect deadlocks. The
    	main function of this module works in a separate thread.

*)
(***************************************************************************)
(*
 | --- DeadlockDetection --------------------------------------------------
 
 | ------------------------------------------------------------------------
 *)
IMPORT
  ServerWaitTbl;


PROCEDURE StartChecks	(         waitingClients :ServerWaitTbl.T);

END DeadlockDetection.
