INTERFACE ScheduledServerResource;

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

    Revision 1.5  1997/04/24 12:13:51  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.4  1996/11/21 07:56:04  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.3  1996/10/29 14:40:56  rbnix
    	New parameter pageAge added.

    	StartTransaction returns the actual transaction number.

    Revision 1.2  1996/08/06 16:32:54  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/08/01 18:01:52  rbnix
    	New administration methods deleteFile, copyFile, renameFile,
    	existsFile, fileInUse and getFiles added.

    Revision 1.1.2.1  1996/07/11 11:08:34  rbnix
    	Method waitAccess added to provide global deadlock detection.

    Revision 1.1  1996/02/26 17:59:50  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- ScheduledServerResource --------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT BaseScheduledServerResource AS Super;
IMPORT
  Pathname, TextTransientSeq AS TextSeq,
  Page,
  PageFile,
  Access, PageLock, Txn,
  CommunicationSeq, RemoteFile,
  ServedClient, ClientInfoSeq;

CONST
  Brand			= "ScheduledServerResource";

TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      (* resource administration *)
      close		(         client	:ServedClient.T)
			RAISES {Access.Invalid};


      (* other file support *)
      registerLocalFile	(         client	:ServedClient.T;
                                  baseName	:Pathname.T)
			RAISES {PageFile.NoAccess, Access.Invalid};

      unregisterLocalFile(        client	:ServedClient.T;
                                  baseName	:Pathname.T)
			RAISES {PageFile.NoAccess, Access.Invalid};


      deleteFile	(         baseName	:Pathname.T)
			RAISES {PageFile.NoAccess};

      copyFile		(         sourceName	:Pathname.T;
                                  destName	:Pathname.T)
			RAISES {PageFile.NoAccess};

      renameFile	(         oldName	:Pathname.T;
                                  newName	:Pathname.T)
			RAISES {PageFile.NoAccess};

      existsFile	(         baseName	:Pathname.T)
			:BOOLEAN;

      fileInUse		(         baseName	:Pathname.T)
			:BOOLEAN;

      getFileUser	(         baseName	:Pathname.T)
			:ClientInfoSeq.T;

      fileSize		(         baseName	:Pathname.T)
			:CARDINAL
			RAISES {PageFile.NoAccess};

      getFiles		()
			:TextSeq.T;


      (* transaction file support *)
      openFile		(         client	:ServedClient.T;
                                  baseName      :Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:RemoteFile.T
			RAISES {Access.Denied, PageFile.NoAccess, Access.Invalid};
      
      closeFile		(         client	:ServedClient.T;
                                  file		:RemoteFile.T)
			RAISES {Access.Invalid};

      getData		(         client	:ServedClient.T;
                                  file          :RemoteFile.T;
                                  pageNo	:CARDINAL;
                         VAR      pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  transferData	:BOOLEAN;
                                  putEntries	:CommunicationSeq.T)
			:Page.T
			RAISES {Access.Invalid, Access.Locked};

      putData		(         client	:ServedClient.T;
                                  end		:Txn.End;
                                  entries       :CommunicationSeq.T)
			:CARDINAL
			RAISES {Access.Invalid};

      startTransaction	(         client	:ServedClient.T)
			:CARDINAL
			RAISES {Access.Invalid};

      waitAccess	(         client	:ServedClient.T;
                                  file          :RemoteFile.T;
                                  pageNo	:CARDINAL;
                                  lock		:PageLock.ServerMode)
			RAISES {Access.Invalid, Access.Locked};
    END;      

END ScheduledServerResource.
