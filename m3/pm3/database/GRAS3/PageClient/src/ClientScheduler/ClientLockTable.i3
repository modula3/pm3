INTERFACE ClientLockTable;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:47  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:36  hosking
    Import of GRAS3 1.1

    Revision 1.3  1997/01/20 08:54:21  roland
    ClientLockTable uses an array now directly to gain more performance
    (getLastEntry is called much more often than any other procedure of
    the module, so this must be fast).
    Minor, non-critical changes in ScheduledClientPage.

    Revision 1.2  1996/03/06 14:04:07  rbnix
    	New method fmt added to get a formatted representation of the
    	lock table's value.

    Revision 1.1  1996/02/09 16:46:40  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- ClientLockTable ----------------------------------------------------
  The abstract data type ClientLockTable represents a map
 | p (t) --> (l, h)
  where (l, h) is a lock entry. The map is initialized with
 | p (t) --> (PageLocks.ClientLock.O, NIL)  for t:TransactionLevel
  The search operations gets the first entry from searchLevel downto
  TransactionLevel.Envelope where l # PageLocks.ClientLock.O otherwise
  p(TransactionLevel.Envelope).
 | ------------------------------------------------------------------------
 *)

IMPORT
  Txn,
  PageLock,
  ClientLockEntry;


TYPE
  T			<: Public;

  Public		= <*TRANSIENT*> ROOT OBJECT
    METHODS
      init		() :T;

      putEntry		(         level		:Txn.Level;
                                  entry		:ClientLockEntry.T);

      getEntry		(         level		:Txn.Level)
			:ClientLockEntry.T;

      getLastEntry	(         searchLevel	:Txn.Level;
                         VAR	  lastLevel	:Txn.Level) 
			:ClientLockEntry.T;

      exists		(         searchLevel	:Txn.Level;
                                  lock		:PageLock.ClientMode) :BOOLEAN;


      fmt		() :TEXT;
    END;


END ClientLockTable.
