INTERFACE InternalScheduledServerFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.6  1997/06/16 12:21:40  rbnix
    	Changed data on server is now stored temporary in a local
    	shadow file until clients close the file. This keeps the
    	persistent files in a consistant state and speeds up file
    	handling a little. Flushing data for log files is removed due
    	to this minimal variant of crash recovery.

    Revision 1.5  1997/04/24 12:13:48  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.4  1996/11/21 07:56:00  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.3  1996/10/29 14:40:47  rbnix
    	New parameter pageAge added.

    	StartTransaction returns the actual transaction number.

    Revision 1.2  1996/08/06 16:32:48  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/08/01 17:57:56  rbnix
    	Semantic of method inUse extended to allow check over all clients.

    Revision 1.1.2.1  1996/07/11 11:06:15  rbnix
    	Parameters of procedure WaitAccess enhanced by lock table.

    Revision 1.1  1996/02/26 17:59:41  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- InternalScheduledServerFile ----------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Page,
  PageFile,
  Access, PageLock, Transaction,
  CommunicationEntry, ClientInfoSeq,
  ServedClient,
  ScheduledServerFile;

REVEAL
  ScheduledServerFile.T	<: Internal;

TYPE
  Internal		= ScheduledServerFile.Public OBJECT
    METHODS
      open		(         client	:ServedClient.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind)
			RAISES {Access.Denied, PageFile.NoAccess, Access.Invalid};
      
      close		(         client	:ServedClient.T)
			RAISES {Access.Invalid};
      
      shutdown		();

      killClient	(	  client	:ServedClient.T);
      
      inUse		(	  client	:ServedClient.T)
			:BOOLEAN;
      (*
        Checks wether file is used. If client is not NIL this operation
        test only for the specified client otherwise the test is
        generalized to all clients.
      *)

      user              ()
			:ClientInfoSeq.T;

      checkData         (         client        :ServedClient.T;
                                  end		:Transaction.End;
                         READONLY entry		:CommunicationEntry.T)
			RAISES {Access.Invalid};
      
      putData		(         client	:ServedClient.T;
                                  end		:Transaction.End;
                         READONLY entry		:CommunicationEntry.T)
			RAISES {Access.Invalid};

      getData		(         client	:ServedClient.T;
                                  pageNo	:CARDINAL;
                         VAR      pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  transferData	:BOOLEAN)
			:Page.T
			RAISES {Access.Invalid, Access.Locked};

      waitAccess	(         client	:ServedClient.T;
                                  pageNo	:CARDINAL;
                                  lock		:PageLock.ServerMode)
			RAISES {Access.Invalid, Access.Locked};
    END;

END InternalScheduledServerFile.
