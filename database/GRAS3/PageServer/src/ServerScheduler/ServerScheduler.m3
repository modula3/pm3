MODULE ServerScheduler;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.18  1998/05/19 10:22:00  roland
    Typos and "'" removed from error messages.

    Revision 1.17  1998/02/17 16:24:47  roland
    Error message corrected.

    Revision 1.16  1997/06/13 11:46:30  rbnix
    	Adapted to unified resource paths of module
    	BaseServerScheduler.

    Revision 1.15  1997/06/10 12:55:05  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.14  1997/01/20 08:55:32  roland
    Minor fixes.

    Revision 1.13  1996/12/03 09:55:51  roland
    A not existing resource cannot be opened with new=FALSE. Open will
    raise Access.Denied when this is attempted.

    Revision 1.12  1996/11/21 15:23:42  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.11  1996/11/21 07:56:09  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.10  1996/11/14 14:13:51  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.9  1996/11/08 14:46:07  roland
    GetResources in ServerScheduler handles PageFile.NoAccess correct now.

    Revision 1.8  1996/10/29 14:43:31  rbnix
    	Procedure SignalAccess based on BaseServerScheduler.SignalAccess
    	added.

    Revision 1.7  1996/10/08 11:23:18  rbnix
    	Buf fixed in RenameResource: obsolete entry is now deleted
    	not set to NIL.

    Revision 1.6  1996/08/22 14:16:37  rbnix
    	Support for complex resource names added. Resource names are
    	structured like file paths and are directly mapped to the disk
    	file system. Neccessary path components are supervised and
    	constructed transparently to the user.

    Revision 1.5  1996/08/12 10:29:13  rbnix
    	Bug fixed in DeleteResource: entry from resourceTable is now
    	really removed.

    Revision 1.4  1996/08/06 16:32:58  roland
    Merge of PAGESERVER and main branch.

    Revision 1.3.2.3  1996/08/01 18:03:18  rbnix
    	New administration functions DeleteResource, CopyResource,
    	RenameResource, ExistsResource, ResourceInUse and GetResources
    	added.

    	Bug fixed in function OpenResource:  now opening a resource with
    	new = TRUE deletes an existing resource.

    Revision 1.3.2.2  1996/07/23 14:19:30  rbnix
    	Bug fixed: handling of access mode adjusted. The access mode
    	is now determined by the actual first active client rather
    	than the very first client.

    Revision 1.3.2.1  1996/07/23 13:58:32  rbnix
    	In OpenRessource: test relaxed when open existing but new
    	resources. (This should be enhanced in future to delete
    	existing files.)

    Revision 1.3  1996/03/06 08:16:05  rbnix
    	Function KillClient working over all resources added.

    Revision 1.2  1996/03/02 15:07:34  rbnix
    	Bug fixed: use of ScheduledClientSetDef instead abstract type
    	ScheduledClientSet. Objects of this type are now initialzed
    	well.

    	Bug fixed: check on found/new files/resources adjusted.

    Revision 1.1  1996/02/26 18:00:00  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- ServerScheduler ----------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Thread, Pathname, TextSeq,
  Config,
  PageFile, PageFileSystem,
  Access, Termination,
  ServedClient,
  ScheduledServerResource, InternalScheduledServerResource,
  BaseServerScheduler, InternalBaseScheduledServerResource,
  ScheduledServerResourceTbl,
  ClientInfoSeq;


VAR
  resourceTable		:= NEW (ScheduledServerResourceTbl.Default).init ();
  

PROCEDURE OpenResource	(         client	:ServedClient.T;
                                  baseName      :Pathname.T;
                                  access	:Access.Mode;
                                  new		:BOOLEAN)
			:ScheduledServerResource.T
			RAISES {PageFile.NoAccess, Access.Denied,
                                Access.Invalid} =
  VAR
    resource		:ScheduledServerResource.T;
  BEGIN
    IF new AND ExistsResource (baseName) THEN
      DeleteResource (baseName);
    ELSIF NOT new AND NOT ExistsResource (baseName) THEN
      RAISE Access.Denied("Resource not exists.")
    END;

    IF NOT resourceTable.get (baseName, resource) THEN
      resource := NEW (ScheduledServerResource.T).init (baseName, new);
      EVAL resourceTable.put (baseName, resource);
    END;

    RETURN resource.open (client, access);
  END OpenResource;

  
PROCEDURE DeleteResource(         baseName	:Pathname.T)
			RAISES {PageFile.NoAccess} =
  <* FATAL Termination.StillInUse *>
  VAR
    resource		:ScheduledServerResource.T;

  PROCEDURE DeleteDir	(         baseName	:Pathname.T)
			RAISES {PageFile.NoAccess} =
    <* FATAL Pathname.Invalid *>
    VAR
      entries		:TextSeq.T;
      basePath		:Pathname.Arcs;
    BEGIN
      (* delete nested directories *)
      entries := PageFileSystem.GetDirNames (baseName);
      FOR i := 0 TO entries.size () - 1 DO
        basePath := Pathname.Decompose (baseName);
        basePath.addhi (entries.get (i));

        DeleteDir (Pathname.Compose (basePath));
      END;

      (* delete files *)
      entries := PageFileSystem.GetFileNames (baseName);
      FOR i := 0 TO entries.size () - 1 DO
        basePath := Pathname.Decompose (baseName);
        basePath.addhi (entries.get (i));

        PageFileSystem.DeleteFile (Pathname.Compose (basePath));
      END;

      (* delete directory itself *)
      PageFileSystem.RemoveDir (baseName);
    END DeleteDir;

  (* DeleteResource *)
  BEGIN
    IF ResourceInUse (baseName) THEN
      RAISE PageFile.NoAccess (
                "Cannot delete resource '" & baseName & "'. It is in use.");
    END;

    (* delete memory resource *)
    IF resourceTable.get (baseName, resource) THEN
      resource.shutdown (Termination.Mode.Strikt);
      EVAL resourceTable.delete (baseName, resource);
    END;

    (* delete disk resource *)
    DeleteDir (
        BaseServerScheduler.MakeResourceName (baseName, temporary := FALSE));
    BaseServerScheduler.DestroyResourcePath (
        baseName, temporary := FALSE, omitLastComponent := TRUE);
  END DeleteResource;


PROCEDURE CopyResource	(         sourceName	:Pathname.T;
                                  destName	:Pathname.T)
			RAISES {PageFile.NoAccess} =

  PROCEDURE CopyDir	(         sourceBaseName:Pathname.T;
                                  destBaseName	:Pathname.T)
			RAISES {PageFile.NoAccess} =
    <* FATAL Pathname.Invalid *>
    VAR
      entries		:TextSeq.T;
      sourcePath,
      destPath		:Pathname.Arcs;      
    BEGIN
      (* 'copy' directory itself *)
      PageFileSystem.MakeDir (destBaseName);

      (* copy files *)
      entries := PageFileSystem.GetFileNames (sourceBaseName);
      FOR i := 0 TO entries.size () - 1 DO
        sourcePath := Pathname.Decompose (sourceBaseName);
        sourcePath.addhi (entries.get (i));

        destPath := Pathname.Decompose (destBaseName);
        destPath.addhi (entries.get (i));

        PageFileSystem.CopyFile (
            Pathname.Compose (sourcePath),
            Pathname.Compose (destPath));
      END;

      (* copy nested directories *)
      entries := PageFileSystem.GetDirNames (sourceBaseName);
      FOR i := 0 TO entries.size () - 1 DO
        sourcePath := Pathname.Decompose (sourceBaseName);
        sourcePath.addhi (entries.get (i));

        destPath := Pathname.Decompose (destBaseName);
        destPath.addhi (entries.get (i));

        CopyDir (Pathname.Compose (sourcePath), Pathname.Compose (destPath));
      END;
    END CopyDir;

  (* CopyResource *)
  BEGIN
    IF ResourceInUse (sourceName) THEN
      RAISE PageFile.NoAccess (
                "Cannot copy resource '" & sourceName & "' to '" & destName &
                "'. The source resource is in use.");
    END;
    IF ExistsResource (destName) THEN
      RAISE PageFile.NoAccess (
                "Cannot copy resource '" & sourceName & "' to '" & destName &
                "'. The destination resource already exists.");
    END;

    BaseServerScheduler.CreateResourcePath (
        destName, temporary := FALSE, omitLastComponent := TRUE);
    CopyDir (
        BaseServerScheduler.MakeResourceName (sourceName, temporary := FALSE),
        BaseServerScheduler.MakeResourceName (destName, temporary := FALSE));
  END CopyResource;


PROCEDURE RenameResource(         oldName	:Pathname.T;
                                  newName	:Pathname.T)
			RAISES {PageFile.NoAccess} =
  <* FATAL Termination.StillInUse *>
  VAR
    resource		:ScheduledServerResource.T;
  BEGIN
    IF ResourceInUse (oldName) THEN
      RAISE PageFile.NoAccess (
                "Cannot rename resource '" & oldName & "' to '" & newName &
                "'. The resource is in use.");
    END;
    IF ExistsResource (newName) THEN
      RAISE PageFile.NoAccess (
                "Cannot rename resource '" & oldName & "' to '" & newName &
                "'. The new name is already used.");
    END;

    (* delete memory object (it can't be renamed) *)
    IF resourceTable.get (oldName, resource) THEN
      resource.shutdown (Termination.Mode.Strikt);
      EVAL resourceTable.delete (oldName, resource);
    END;

    (* rename disk object *)
    BaseServerScheduler.CreateResourcePath (
        newName, temporary := FALSE, omitLastComponent := TRUE);
    PageFileSystem.RenameFile (
        BaseServerScheduler.MakeResourceName (oldName, temporary := FALSE),
        BaseServerScheduler.MakeResourceName (newName, temporary := FALSE));
    BaseServerScheduler.DestroyResourcePath (
        oldName, temporary := FALSE, omitLastComponent := TRUE);
  END RenameResource;


PROCEDURE ExistsResource(baseName	:Pathname.T)
			:BOOLEAN =
  VAR
    resourceName	:Pathname.T;
  BEGIN
    TRY
      IF ResourceInUse (baseName) THEN
        RETURN TRUE;

      ELSE
        resourceName := BaseServerScheduler.MakeResourceName (
                        baseName, temporary := FALSE);
        RETURN (PageFileSystem.ExistsDir (resourceName)) AND
               (PageFileSystem.GetFileNames (resourceName).size () > 0);
      END;
      
    EXCEPT
    | PageFile.NoAccess =>
      RETURN FALSE;
    END;
  END ExistsResource;


PROCEDURE ResourceInUse	(baseName	:Pathname.T)
			:BOOLEAN =
  VAR
    resource		:ScheduledServerResource.T;
  BEGIN
    RETURN resourceTable.get (baseName, resource) AND
           resource.inUse (NIL);
  END ResourceInUse;


PROCEDURE GetResourceUser	(baseName	:Pathname.T)
                                :ClientInfoSeq.T =
  VAR
    resource		:ScheduledServerResource.T;
  BEGIN
    IF resourceTable.get (baseName, resource) THEN
      RETURN resource.user ();
    ELSE
      RETURN NEW(ClientInfoSeq.T).init();
    END;
  END GetResourceUser; 


PROCEDURE GetResources	()
			:TextSeq.T
                        RAISES {PageFile.NoAccess} =
    (* The Server call Config.Login. Config.Unspecified will never be
       raised. Server ensures a valid pathname for root. *)
    <* FATAL Config.Unspecified, Pathname.Invalid *>
  VAR
    result, dirs, subDirs: TextSeq.T;
    rootlen: CARDINAL;
    currentDir: Pathname.T;
    currentPath :Pathname.Arcs;
  BEGIN
    dirs := NEW(TextSeq.T).init();
    result := NEW(TextSeq.T).init();
    rootlen := Pathname.Decompose(Config.GetRootPrefix(temporary := FALSE)).size();

    dirs.addhi(Config.GetRootPrefix (temporary := FALSE));
    WHILE dirs.size() > 0 DO
      currentDir := dirs.remhi();

      (* check if this is a resource *)
      IF PageFileSystem.ExistsDir (currentDir) THEN
        currentPath := Pathname.Decompose (currentDir);

        IF PageFileSystem.GetFileNames (currentDir).size () > 0 THEN
          (* this is a resource, make it relative to the root *)
          currentPath := TextSeq.Sub (currentPath, rootlen);
          currentPath.addlo (NIL);
          result.addhi (Pathname.Compose (currentPath));
        ELSE
          (* this is not a resource, but maybe its sub dirs are *)
          subDirs := PageFileSystem.GetDirNames (currentDir);
          IF subDirs.size () > 0 THEN
            FOR i := 0 TO subDirs.size () - 1 DO
              currentPath.addhi (subDirs.get (i));
              dirs.addhi(Pathname.Compose (currentPath));
              EVAL currentPath.remhi();
            END;
          END;
        END;
        
      END;
    END;

    RETURN result;
  END GetResources;


PROCEDURE KillClient	(         client	:ServedClient.T;
                                  why		:TEXT := NIL) =
  VAR
    i			:ScheduledServerResourceTbl.Iterator;
    resource		:ScheduledServerResource.T;
    baseName		:Pathname.T;
  BEGIN
    i := resourceTable.iterate ();
    WHILE i.next (baseName, resource) DO
      resource.killClient (client, why);
    END;
  END KillClient;


(*
 | --- shut down ----------------------------------------------------------
 *)
VAR
  shutdownCondition	:= NEW (Thread.Condition);


PROCEDURE Shutdown	(         termination	:Termination.Mode)
			RAISES {Termination.StillInUse} =
  VAR
    i			:ScheduledServerResourceTbl.Iterator;
    resource		:ScheduledServerResource.T;
    baseName		:Pathname.T;
  BEGIN
    i := resourceTable.iterate ();
    WHILE i.next (baseName, resource) DO
      resource.shutdown (termination);
    END;

    Thread.Signal (shutdownCondition);
  END Shutdown;
  

PROCEDURE WaitShutdown	() =
  VAR
    shutdownMutex	:= NEW (Thread.Mutex);
  BEGIN
    LOCK shutdownMutex DO
      Thread.Wait (shutdownMutex, shutdownCondition);
    END;
  END WaitShutdown;
  
 
PROCEDURE SignalAccess	() =
  BEGIN
    BaseServerScheduler.SignalAccess ();
  END SignalAccess;


BEGIN
END ServerScheduler.
