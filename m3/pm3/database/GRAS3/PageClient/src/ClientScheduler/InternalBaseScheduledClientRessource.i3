INTERFACE InternalBaseScheduledClientRessource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.8  1997/04/24 12:12:32  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.7  1996/11/18 17:51:40  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.6  1996/10/29 15:01:38  rbnix
    	New parameter for page age added.

    Revision 1.5  1996/08/06 16:24:50  roland
    Merge of PAGESERVER and main branch.

    Revision 1.4.2.1  1996/06/13 12:48:21  rbnix
    	Method getID moved to exported interface file

    Revision 1.4  1996/03/15 14:24:31  rbnix
    	Method getID added.

    Revision 1.3  1996/03/02 15:03:27  rbnix
    	Bug fixed: errorneous partial revelation changed into
    	complete one.

    Revision 1.2  1996/02/29 09:34:56  rbnix
    	Release of resources added.

    Revision 1.1  1996/02/09 16:46:44  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- InternalBaseScheduledClientRessource -------------------------------
 This interface extends the public one for subsystem internal use only.

 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  Page,
  PageFile,
  Access, PageLock,
  RemoteFile,
  BaseScheduledClientRessource;

REVEAL
  BaseScheduledClientRessource.T <: Internal;

TYPE
  Internal		= BaseScheduledClientRessource.Public OBJECT
    METHODS
      openRemoteFile	(         baseName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:RemoteFile.T
			RAISES {PageFile.NoAccess, Access.Denied,
                                BaseScheduledClientRessource.FatalError};

      closeRemoteFile	(         file		:RemoteFile.T)
      RAISES {BaseScheduledClientRessource.FatalError};

      waitAccess	(         file		:RemoteFile.T;
                                  pageNo	:CARDINAL;
                                  lock		:PageLock.ServerMode)
			RAISES {Access.Locked,
                                BaseScheduledClientRessource.FatalError};

      signalAccess	();

      getData		(         file		:RemoteFile.T;
                                  pageNo	:CARDINAL;
                         VAR      pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  transferData	:BOOLEAN)
			:Page.T
			RAISES {Access.Locked,
                                BaseScheduledClientRessource.FatalError};

      putData		(         file		:RemoteFile.T;
                                  pageNo	:CARDINAL;
                                  pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  page		:Page.T);
    END;


END InternalBaseScheduledClientRessource.
