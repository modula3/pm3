INTERFACE VirtualRemoteFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.5  1997/10/31 14:14:48  roland
    Adapted to new RuleEngine.

    Revision 1.4  1997/05/16 08:48:49  roland
    Stack exceptions will never be raised, hence open methods need not raise
    FatalError.

    Revision 1.3  1997/05/09 16:26:59  renehuel
    The files have been changed to enable transaction semantic on closing
    of remote files. You may now close a graph within a transaction without
    an exception to be raised, and the final closing of the file
    Depends On the following action : a commit (of the top level
    transaction) closes the file, an abort aborts and leaves the resource
    still open.

    Revision 1.2  1997/04/24 12:13:01  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.1  1996/02/29 17:44:26  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualRemoteFile --------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT VirtualFile AS Super;
IMPORT
  Pathname,
  PageFile,
  Access,
  VirtualResource;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      open		(         resource	:VirtualResource.T;
                                  fileName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:T
			RAISES {Access.Denied, PageFile.NoAccess};

    END;
  

END VirtualRemoteFile.
