INTERFACE CommunicationPort;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.8  1997/04/24 11:54:36  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.7  1996/11/21 07:53:45  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.6  1996/10/29 14:06:30  rbnix
    	New variable pageAge added.

    Revision 1.5  1996/08/06 16:24:59  roland
    Merge of PAGESERVER and main branch.

    Revision 1.4.2.2  1996/08/01 18:06:24  rbnix
    	New file administration methods deleteRemoteFile,
    	copyRemoteFile, renameRemoteFile, existsRemoteFile,
    	remoteFileInUse, remoteFileSize and getRemoteFiles added.

    Revision 1.4.2.1  1996/07/11 10:46:38  rbnix
    	Method waitAccess added: registration of deadlocks
    	(waitAccess) is now send to the server to perform global
    	deadlock detection.

    Revision 1.4  1996/02/29 09:27:49  rbnix
    	Methods close and closeRemoteFile added.

    Revision 1.3  1996/02/26 17:56:12  rbnix
    	Exception Access.Invalid added.

    Revision 1.2  1996/02/21 13:50:28  rbnix
    	Comment adjusted.

    Revision 1.1  1996/02/09 16:42:59  rbnix
    	First version of specification layer for network objects
    	added.

*)
(***************************************************************************)

(*
 | --- CommunicationPort --------------------------------------------------
 This abstract data type CommunicationPort specifies the major communication
 interface to be implemented by server.

 All file access is supervised by server. Therefore the methods
 registerLocalFile resp. unregisterLocalFile are used to keep track on
 local files for exclusive usage. (Because access to the files itself is
 not supervised only exclusive access is possible).

 Remote files are opened in respect with the allready assigned access
 mode. The method getData is mainly used to demand page data and locks. If
 the call is revoked the exception Access.Locked will be raised. The method
 putData may be used to free locks and return changed data to the server.
 Terminating a transaction with putData underlie some restrictions.
 Including currently freed locks it isn't possible to hold
 exclusive locks (PageLock.Mode.X). Furthermore it is only valid to return
 changed data if a transaction is terminated with success
 (end = Transaction.End.Commit) in the same call. To minimize the amount of
 communication it is possible to free locks when demanding data. At least
 access to a remote file must be prepended by startTransaction as first
 call. Transactions may not be nested. 
 | ------------------------------------------------------------------------
 *)

IMPORT
  Pathname, TextSeq,
  Thread, NetObj,
  Page,
  PageFile,
  PageLock, Access, Transaction, 
  RemoteFile, CommunicationSeq, ClientInfoSeq;


TYPE
  T                     = NetObj.T BRANDED "CommunicationPort" OBJECT
    METHODS
      (* port administration *)
      close		()
			RAISES {Thread.Alerted, NetObj.Error, Access.Invalid};


      (* other file support *)
      registerLocalFile	(         baseName	:Pathname.T)
			RAISES {Thread.Alerted, NetObj.Error,
                                PageFile.NoAccess, Access.Invalid};

      unregisterLocalFile(        baseName	:Pathname.T)
			RAISES {Thread.Alerted, NetObj.Error,
                                PageFile.NoAccess, Access.Invalid};

      deleteRemoteFile	(         baseName	:Pathname.T)
			RAISES {Thread.Alerted, NetObj.Error,
                                PageFile.NoAccess};

      copyRemoteFile	(         sourceName	:Pathname.T;
                                  destName	:Pathname.T)
			RAISES {Thread.Alerted, NetObj.Error,
                                PageFile.NoAccess};

      renameRemoteFile	(         oldName	:Pathname.T;
                                  newName	:Pathname.T)
			RAISES {Thread.Alerted, NetObj.Error,
                                PageFile.NoAccess};

      existsRemoteFile	(         baseName	:Pathname.T)
			:BOOLEAN
			RAISES {Thread.Alerted, NetObj.Error};

      remoteFileInUse	(         baseName	:Pathname.T)
			:BOOLEAN
			RAISES {Thread.Alerted, NetObj.Error};

      getRemoteFileUser (         baseName      :Pathname.T):
                        ClientInfoSeq.T
                        RAISES {Thread.Alerted, NetObj.Error};

      remoteFileSize	(         baseName	:Pathname.T)
			:CARDINAL
			RAISES {Thread.Alerted, NetObj.Error,
                                PageFile.NoAccess};

      getRemoteFiles	()
			:TextSeq.T
			RAISES {Thread.Alerted, NetObj.Error};


      (* transaction file support *)
      openRemoteFile    (         baseName      :Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:RemoteFile.T
			RAISES {Thread.Alerted, NetObj.Error,
                                PageFile.NoAccess, Access.Invalid,
                                Access.Denied};

      closeRemoteFile	(         file		:RemoteFile.T)
		        RAISES {Thread.Alerted, NetObj.Error, Access.Invalid};


      getData           (         file          :RemoteFile.T;
                                  pageNo	:CARDINAL;
                         VAR      pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  transferData	:BOOLEAN;
                                  putEntries	:CommunicationSeq.T)
			:Page.T
			RAISES {Thread.Alerted, NetObj.Error,
                                Access.Invalid, Access.Locked};

      putData		(         end		:Transaction.End;
                                  entries       :CommunicationSeq.T)
			RAISES {Thread.Alerted, NetObj.Error, Access.Invalid};


      startTransaction	()
			:CARDINAL
			RAISES {Thread.Alerted, NetObj.Error, Access.Invalid};

      waitAccess	(         file		:RemoteFile.T;
                                  pageNo	:CARDINAL;
                                  lock		:PageLock.ServerMode;)
			RAISES {Thread.Alerted, NetObj.Error,
                                Access.Invalid, Access.Locked};
    END;

END CommunicationPort.
