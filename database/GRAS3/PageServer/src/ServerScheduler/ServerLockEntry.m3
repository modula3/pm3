MODULE ServerLockEntry;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.1  1996/03/06 16:17:03  rbnix
    	Method/Procedure Fmt added.

*)
(***************************************************************************)
(*
 | --- ServerLockEntry ----------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageLock;


PROCEDURE Fmt		(         entry		:T) :TEXT =
  BEGIN
    RETURN "lock = " & PageLock.FmtMode (entry.lock);
  END Fmt;


BEGIN
END ServerLockEntry.
