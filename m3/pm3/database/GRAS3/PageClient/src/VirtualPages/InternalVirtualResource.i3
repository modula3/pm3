INTERFACE InternalVirtualResource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.6  1997/05/16 08:48:47  roland
    Stack exceptions will never be raised, hence open methods need not raise
    FatalError.

    Revision 1.5  1997/05/09 16:26:53  renehuel
    The files have been changed to enable transaction semantic on closing
    of remote files. You may now close a graph within a transaction without
    an exception to be raised, and the final closing of the file
    Depends On the following action : a commit (of the top level
    transaction) closes the file, an abort aborts and leaves the resource
    still open.

    Revision 1.4  1997/04/24 12:12:59  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.3  1996/11/18 17:52:17  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.2  1996/03/11 17:25:48  rbnix
    	Method ScheduledClientFile.close is replaced by
    	ScheduledClientResource.closeRemoteFile due to additional
    	clean up. Therefore VirtualResource must redirect the
    	operation.

    Revision 1.1  1996/02/29 17:44:15  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
IMPORT
  Pathname,
  PageFile,
  Access,
  ScheduledClientFile,
  VirtualResource;


REVEAL
  VirtualResource.T	<: Internal;

TYPE
  Internal		= VirtualResource.Public OBJECT
    METHODS
      openRemoteFile	(         baseName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:ScheduledClientFile.T
			RAISES {Access.Denied, PageFile.NoAccess};

      closeRemoteFile	(         file		:ScheduledClientFile.T)
      RAISES {VirtualResource.FatalError};
      
      registerLocalFile	(         baseName	:Pathname.T)
			RAISES {PageFile.NoAccess, VirtualResource.FatalError};

      unregisterLocalFile(        baseName	:Pathname.T)
			RAISES {PageFile.NoAccess, VirtualResource.FatalError};
    END;
      

END InternalVirtualResource.
