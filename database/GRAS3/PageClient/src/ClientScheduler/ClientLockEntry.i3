INTERFACE ClientLockEntry;

(*
  | *************************************************************************
  | Created by:  Reiner Nix 
  |
  | $Author$
  | $Revision$
  | $Date$
  | $Log$
  | Revision 1.1  2003/03/27 15:25:36  hosking
  | Initial revision
  |
  | Revision 1.2  1996/03/06 14:02:55  rbnix
  | 	New function Fmt added to get a formatted representation of the
  | 	lock entry's value.
  |
  | Revision 1.1  1996/02/09 16:46:38  rbnix
  | 	First version of client scheduler added.
  |
  | *************************************************************************
*)

(*
 | --- ClientLockEntry ----------------------------------------------------
  This data type represents data for a lock entry on a client node.
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageLock,
  PageHandle;


CONST
  Brand			="ClientLockEntry";

TYPE
  T			=RECORD
    lock		:PageLock.ClientMode;
    handle		:PageHandle.T;
  END;

  
PROCEDURE Fmt		(         entry		:T) :TEXT;
  

END ClientLockEntry.
