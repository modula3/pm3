MODULE VirtualRemoteFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.9  1998/01/21 14:12:13  roland
    New method baseName.

    Revision 1.8  1997/10/31 14:14:49  roland
    Adapted to new RuleEngine.

    Revision 1.7  1997/05/16 08:48:50  roland
    Stack exceptions will never be raised, hence open methods need not raise
    FatalError.

    Revision 1.6  1997/05/09 16:27:01  renehuel
    The files have been changed to enable transaction semantic on closing
    of remote files. You may now close a graph within a transaction without
    an exception to be raised, and the final closing of the file
    Depends On the following action : a commit (of the top level
    transaction) closes the file, an abort aborts and leaves the resource
    still open.

    Revision 1.5  1997/04/24 12:13:02  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.4  1996/11/18 17:52:23  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.3  1996/09/09 11:43:37  rbnix
    	Method getResource to relate files to their resource created
    	in base class. Therefore internal variables are removed.

    Revision 1.2  1996/03/11 17:25:49  rbnix
    	Method ScheduledClientFile.close is replaced by
    	ScheduledClientResource.closeRemoteFile due to additional
    	clean up. Therefore VirtualResource must redirect the
    	operation.

    Revision 1.1  1996/02/29 17:44:28  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualRemoteFile --------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  PageFile,
  PageCache,
  Access,
  ScheduledClientFile,
  VirtualResource, InternalVirtualResource,
  InternalVirtualFile,
  VirtualPage, VirtualRemotePage;


REVEAL
  T			= Public BRANDED OBJECT
      file		:ScheduledClientFile.T;

    OVERRIDES
      open		:= Open;
      close		:= Close;
      createPage	:= CreatePage;
      getBaseName       := GetBaseName;
    END;

  
PROCEDURE Open		(         self		:T;
                                  resource	:VirtualResource.T;
                                  fileName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:T
			RAISES {Access.Denied, PageFile.NoAccess} =
  BEGIN
    self.file := resource.openRemoteFile (fileName, mode, kind, new);

    RETURN self.init (resource);
  END Open;


PROCEDURE Close		(         self		:T)
  RAISES {VirtualResource.FatalError} =
  BEGIN
    self.getResource ().closeRemoteFile (self.file);
  END Close;

PROCEDURE GetBaseName(self: T): Pathname.T =
  BEGIN
    RETURN self.file.getBaseName();
  END GetBaseName;
  
PROCEDURE CreatePage	(         self		:T;
                                  pageNo	:CARDINAL)
			:VirtualPage.T =
  VAR
    page		:VirtualRemotePage.T;
  BEGIN
    PageCache.BeginAccess ();
    page := NEW (VirtualRemotePage.T).init (self.file.getPage (pageNo));
    PageCache.EndAccess ();

    RETURN page;
  END CreatePage;


BEGIN
END VirtualRemoteFile.
