MODULE ServerWaitEntry;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.2  1996/08/06 16:33:01  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/11 12:13:34  rbnix
    	First version of module providing a data type holding
    	information about waiting clients for data request.

*)
(***************************************************************************)
IMPORT Fmt AS StdFmt;


PROCEDURE Fmt		(         entry		:T) :TEXT =
  
  BEGIN
    RETURN "waitingClient = " & entry.waitingClient.getID () &
           (* ", otherClients = (" & entry.otherClients.fmt () & ")" & *)
           ", age = " & StdFmt.Int (entry.age) &
           ", pageNo = " & StdFmt.Int (entry.pageNo) &
           ", file = " & entry.file;
  END Fmt;

BEGIN
END ServerWaitEntry.
