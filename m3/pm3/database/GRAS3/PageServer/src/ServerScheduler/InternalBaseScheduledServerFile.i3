INTERFACE InternalBaseScheduledServerFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.8  1997/06/27 07:06:16  roland
    Files remove their shadow when they are closed. Therefore, shadow
    files have to initialized on every true open of a file with
    initShadow.

    Revision 1.7  1997/06/16 12:21:38  rbnix
    	Changed data on server is now stored temporary in a local
    	shadow file until clients close the file. This keeps the
    	persistent files in a consistant state and speeds up file
    	handling a little. Flushing data for log files is removed due
    	to this minimal variant of crash recovery.

    Revision 1.6  1997/05/15 17:06:20  roland
    Bugfix: ScheduledClientFile has to set mode and kind in open method when
    it is not used by another client. Therfore: methods setAccessMode and setKind
    added to internal interface of BaseScheduledServerFile.

    Revision 1.5  1997/04/24 12:13:47  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.4  1996/08/06 16:32:45  roland
    Merge of PAGESERVER and main branch.

    Revision 1.3.2.1  1996/07/11 11:06:11  rbnix
    	Parameters of procedure WaitAccess enhanced by lock table.

    Revision 1.3  1996/03/01 13:02:45  rbnix
    	Method getBaseName moved from private to public parts of
    	subsystem interfaces.

    Revision 1.2  1996/02/28 11:03:16  rbnix
    	File and resource pathes are now related to a root path
    	via Config. Errors related to creation of are submittet to
    	clients via exception PageFile.NoAccess.

    Revision 1.1  1996/02/26 17:59:38  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- InternalBaseScheduledServerFile ------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  PageFile,
  SimpleMedia, ShadowMedia,
  Access, PageLock,
  ServedClient,
  BaseScheduledServerResource, BaseScheduledServerFile, ServerLockTable;

REVEAL
  BaseScheduledServerFile.T	<: Internal;

TYPE
  Internal		= BaseScheduledServerFile.Public OBJECT
    METHODS
      init		(         resource	:BaseScheduledServerResource.T;
                                  baseName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:BaseScheduledServerFile.T
			RAISES {PageFile.NoAccess};

      initShadow	()
			RAISES {PageFile.NoAccess};

      setAccessMode     (mode: Access.Mode);
      setKind           (kind: Access.Kind);
      
      getKind		() :Access.Kind;
      getPersistentMedia() :SimpleMedia.T;
      getTemporaryMedia	() :ShadowMedia.T;
      getAccessMode	() :Access.Mode;

      waitAccess	(         client	:ServedClient.T;
                                  locks		:ServerLockTable.T;
                                  pageNo        :CARDINAL;
                                  lock          :PageLock.ServerMode)
			RAISES {Access.Locked};
    END;

END InternalBaseScheduledServerFile.
