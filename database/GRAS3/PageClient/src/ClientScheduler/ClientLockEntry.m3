MODULE ClientLockEntry;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.2  1996/03/06 16:11:29  rbnix
    	Journal output improved.

    Revision 1.1  1996/03/06 14:02:58  rbnix
    	New function Fmt added to get a formatted representation of the
    	lock entry's value.

*)
(***************************************************************************)
IMPORT
  PageLock;


PROCEDURE Fmt		(         entry		:T) :TEXT =
  VAR
    handleText		:TEXT;
  BEGIN
    IF entry.handle = NIL THEN
      handleText := "NIL";
    ELSE
      handleText := "(" & entry.handle.fmt () & ")";
    END;

    RETURN "lock = " & PageLock.FmtMode (entry.lock) & ", handle = " & handleText;
  END Fmt;


BEGIN
END ClientLockEntry.
