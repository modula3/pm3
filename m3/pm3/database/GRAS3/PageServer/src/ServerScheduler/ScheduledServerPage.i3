INTERFACE ScheduledServerPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:49  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:39  hosking
    Import of GRAS3 1.1

    Revision 1.6  1997/06/16 12:21:43  rbnix
    	Changed data on server is now stored temporary in a local
    	shadow file until clients close the file. This keeps the
    	persistent files in a consistant state and speeds up file
    	handling a little. Flushing data for log files is removed due
    	to this minimal variant of crash recovery.

    Revision 1.5  1996/10/29 14:40:53  rbnix
    	New parameter pageAge added.

    	StartTransaction returns the actual transaction number.

    Revision 1.4  1996/08/06 16:32:52  roland
    Merge of PAGESERVER and main branch.

    Revision 1.3.2.2  1996/08/01 18:00:48  rbnix
    	Method shutdown added to free page resources.

    Revision 1.3.2.1  1996/07/11 11:10:06  rbnix
    	Method waitAccess added to provide global deadlock detection.

    Revision 1.3  1996/03/11 17:23:20  rbnix
    	Method close for pages is removed. All cached data and locks
    	must be returned before a file can be closed.

    Revision 1.2  1996/03/08 11:39:45  rbnix
    	Close procedure for pages added. This procedure checks closing
    	the file is ok due to pages can be given up (unused C-locks)
    	or must be hold (used C or X-locks).

    Revision 1.1  1996/02/26 17:59:47  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- ScheduledServerPage ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Page,
  PageLock, Access, Txn,
  CommunicationEntry,
  ServedClient,
  BaseScheduledServerFile;


CONST
  Brand			= "ScheduledServerPage";

TYPE
  T			<: Public;

  Public		= <*TRANSIENT*> ROOT OBJECT
    METHODS
      init		(         file		:BaseScheduledServerFile.T;
                                  pageNo	:CARDINAL)
			:T;

      getData		(         client	:ServedClient.T;
                         VAR      pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  transferData	:BOOLEAN)
			:Page.T
			RAISES {Access.Invalid, Access.Locked};

      checkData		(         client	:ServedClient.T;
                                  end		:Txn.End;
                         READONLY entry		:CommunicationEntry.T)
			RAISES {Access.Invalid};

      putData		(         client	:ServedClient.T;
                                  end		:Txn.End;
                         READONLY entry		:CommunicationEntry.T)
			RAISES {Access.Invalid};
      
      relocate		();

      waitAccess	(         client	:ServedClient.T;
                                  lock		:PageLock.ServerMode)
			RAISES {Access.Invalid, Access.Locked};

      killClient	(	  client	:ServedClient.T);

      shutdown		();

      inUse		(	  client	:ServedClient.T)
			:BOOLEAN;
    END;

END ScheduledServerPage.
