INTERFACE InternalBaseScheduledClientFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.6  1998/01/21 14:11:08  roland
    Method baseName now in public interface.
    Files can now be opened as read-only in read-write-exclusive resources.

    Revision 1.5  1997/04/24 12:12:31  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.4  1996/11/18 17:51:39  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.3  1996/10/29 15:01:36  rbnix
    	New parameter for page age added.

    Revision 1.2  1996/03/11 17:16:07  rbnix
    	Method close moved from public to internal interface.

    	Method getBaseName added.

    Revision 1.1  1996/02/09 16:46:43  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- InternalBaseScheduledClientFile ------------------------------------
  
 | ------------------------------------------------------------------------
 *)

IMPORT
  Pathname,
  Page,
  PageFile,
  PageLock, Access,
  OriginalMedia, ShadowMedia,
  BaseScheduledClientFile, BaseScheduledClientRessource;


REVEAL
  BaseScheduledClientFile.T	<: Internal;

  
TYPE
  Internal		= BaseScheduledClientFile.Public OBJECT
    METHODS
      open		(         ressource	:BaseScheduledClientRessource.T;
                                  baseName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:BaseScheduledClientFile.T
			RAISES {PageFile.NoAccess, Access.Denied,
                                BaseScheduledClientFile.FatalError};

      close		() RAISES {BaseScheduledClientFile.FatalError};

      waitAccess	(         pageNo	:CARDINAL;
                                  lock		:PageLock.ServerMode)
			RAISES {Access.Locked, BaseScheduledClientFile.FatalError};

      getData		(         pageNo	:CARDINAL;
                         VAR      pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  transferData	:BOOLEAN) :Page.T
			RAISES {Access.Locked,
                                BaseScheduledClientFile.FatalError};

      putData		(         pageNo	:CARDINAL;
                                  pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode;
                                  page		:Page.T);

      getOriginalMedia	() :OriginalMedia.T;
      getShadowMedia	() :ShadowMedia.T;
    END;
  

END InternalBaseScheduledClientFile.
