MODULE BaseScheduledServerResource
EXPORTS BaseScheduledServerResource, InternalBaseScheduledServerResource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.8  1997/06/13 16:13:01  roland
    Temporary path of a resource must always be created.

    Revision 1.7  1997/06/13 11:46:24  rbnix
    	Adapted to unified resource paths of module
    	BaseServerScheduler.

    Revision 1.6  1997/06/10 12:54:49  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.5  1997/03/26 11:22:38  roland
    Open file routines can now handle subdirectories.

    Revision 1.4  1996/08/22 14:16:31  rbnix
    	Support for complex resource names added. Resource names are
    	structured like file paths and are directly mapped to the disk
    	file system. Neccessary path components are supervised and
    	constructed transparently to the user.

    Revision 1.3  1996/08/06 16:32:39  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.6  1996/08/01 17:57:28  rbnix
    	Construction of resource paths changed with using
    	BaseServerScheduler.MakeResourcePath.

    Revision 1.2.2.5  1996/07/30 09:40:12  rbnix
    	New method makeFileName added.

    Revision 1.2.2.4  1996/07/30 07:53:09  rbnix
    	Module FS replaced by PageFileSystem.

    Revision 1.2.2.3  1996/07/23 14:19:21  rbnix
    	Bug fixed: handling of access mode adjusted. The access mode
    	is now determined by the actual first active client rather
    	than the very first client.

    Revision 1.2.2.2  1996/07/11 11:06:07  rbnix
    	Parameters of procedure WaitAccess enhanced by lock table.

    Revision 1.2.2.1  1996/06/13 13:24:20  rbnix
    	Error handling improved using new module ErrorSupport.

    Revision 1.2  1996/02/28 11:03:11  rbnix
    	File and resource pathes are now related to a root path
    	via Config. Errors related to creation of are submittet to
    	clients via exception PageFile.NoAccess.

    Revision 1.1  1996/02/26 17:59:33  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- BaseScheduledServerResource ----------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  TextSeq,
  PageFile,
  Access, PageLock,
  ServedClient,
  BaseServerScheduler, ServerLockTable;

REVEAL
  T			= Internal BRANDED OBJECT
      baseName		:Pathname.T;
      access		:Access.Mode;

    OVERRIDES
      init		:= Init;
      cleanUp           := CleanUp;
      setAccessMode	:= SetAccessMode;
      getAccessMode	:= GetAccessMode;
      getBaseName	:= GetBaseName;
      getPath		:= GetPath;
      makeFileName	:= MakeFileName;
      waitAccess	:= WaitAccess;
    END;


PROCEDURE Init		(         self		:T;
                                  baseName	:Pathname.T;
                                  new           :BOOLEAN)
			:T
			RAISES {PageFile.NoAccess} =
  BEGIN
    self.baseName := baseName;

    IF new THEN
      (* persistent path should exist, if resource is not new *)
      BaseServerScheduler.CreateResourcePath (self.baseName, temporary := FALSE);
    END;
    (* make sure temporary path exists *)
    BaseServerScheduler.CreateResourcePath (self.baseName, temporary := TRUE);

    RETURN self;
  END Init;

PROCEDURE CleanUp       (         self          :T) =
  BEGIN
    TRY
      (* remove temporary directory *)
      BaseServerScheduler.DestroyResourcePath (self.baseName, temporary := TRUE);
    EXCEPT
    | PageFile.NoAccess =>
      (* ignore *)
    END;
  END CleanUp;
  
PROCEDURE SetAccessMode	(         self		:T;
                                  access	:Access.Mode) =
  BEGIN
    self.access := access;
  END SetAccessMode;


PROCEDURE GetAccessMode	(         self		:T) :Access.Mode =
  BEGIN
    RETURN self.access;
  END GetAccessMode;


PROCEDURE GetBaseName	(         self		:T) :Pathname.T =
  BEGIN
    RETURN self.baseName;
  END GetBaseName;


PROCEDURE GetPath	(         self		:T) :Pathname.Arcs =
  <* FATAL PageFile.NoAccess *>
  BEGIN
    RETURN BaseServerScheduler.MakeResourcePath (self.baseName, temporary := FALSE);
  END GetPath;
  

PROCEDURE MakeFileName	(         self		:T;
                                  baseName	:Pathname.T;
                                  temporary	:BOOLEAN)
			:Pathname.T 
			RAISES {PageFile.NoAccess} =
  VAR
    rootPath, basePath	:Pathname.Arcs;
    fileName		:Pathname.T;
  BEGIN
    TRY
      (* check for relative file path *)
      IF Pathname.Absolute (baseName) THEN
        RAISE PageFile.NoAccess (
                  "File with absolute pathname is not allowed: " & baseName);
      END;

      (* remove trailing NIL component indicating a relative path *)
      basePath := Pathname.Decompose (baseName);
      EVAL basePath.remlo ();

      (* compose full path *)
      rootPath := BaseServerScheduler.MakeResourcePath (self.baseName, temporary);
      fileName := Pathname.Compose (
                      TextSeq.Cat (rootPath, basePath));
    EXCEPT
    | Pathname.Invalid =>
      RAISE PageFile.NoAccess (
                "Wrong path name for file '" & baseName & "' ");
    END;

    RETURN fileName;
  END MakeFileName;


PROCEDURE WaitAccess	(         <* UNUSED *>
			          self          :T;
                                  client	:ServedClient.T;
                                  locks		:ServerLockTable.T;
                                  file          :Pathname.T;
                                  pageNo        :CARDINAL;
                                  lock          :PageLock.ServerMode)
			RAISES {Access.Locked} =
  BEGIN
    BaseServerScheduler.WaitAccess (client, locks, file, pageNo, lock);
  END WaitAccess;

BEGIN
END BaseScheduledServerResource.
