INTERFACE ServerLockEntry;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.2  1996/03/06 16:17:01  rbnix
    	Method/Procedure Fmt added.

    Revision 1.1  1996/02/26 17:59:54  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- ServerLockEntry ----------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageLock;


CONST
  Brand			= "ServerLockEntry";


TYPE
  T			= RECORD
    lock		:PageLock.ServerMode;
  END;


PROCEDURE Fmt		(         entry		:T) :TEXT;


END ServerLockEntry.
