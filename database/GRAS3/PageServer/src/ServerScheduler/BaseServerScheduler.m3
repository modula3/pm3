MODULE BaseServerScheduler;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.14  1998/08/04 16:44:40  roland
    Server now tolerates empty sub-directories below a resource path.

    Revision 1.13  1998/05/19 10:21:55  roland
    Typos and "'" removed from error messages.

    Revision 1.12  1997/06/13 16:13:02  roland
    Temporary path of a resource must always be created.

    Revision 1.11  1997/06/13 11:44:24  rbnix
    	Parameter omitLastComponent for functions CreateResourcePath
    	and DestroyResourcePath added. Resource paths are unified and
    	build with Config.GetRootPrefix.

    Revision 1.10  1997/06/10 12:54:55  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.9  1996/11/26 15:09:54  roland
    DestroyResourcePath fixed: non-empty directories are now handled correct.

    Revision 1.8  1996/11/21 15:23:40  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.7  1996/11/20 12:11:34  roland
    Improved exception handling and startup.

    Revision 1.6  1996/10/29 14:39:00  rbnix
        Unconditional debug output transformed into conditional.

    Revision 1.5  1996/09/26 14:25:31  rbnix
        Bug fixed in SignalAccess: Thread.Signal changed into
        Thread.Broadcast this works for multiple waiting threads.

    Revision 1.4  1996/08/22 14:16:35  rbnix
        Support for complex resource names added. Resource names are
        structured like file paths and are directly mapped to the disk
        file system. Neccessary path components are supervised and
        constructed transparently to the user.

    Revision 1.3  1996/08/06 16:32:41  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.3  1996/08/01 17:40:18  rbnix
        New functions MakeResourcePath and MakeResourceName added to
        standardize the construction of the path.

    Revision 1.2.2.2  1996/07/11 11:06:59  rbnix
        In WaitAccess:
        Parameters of procedure WaitAccess enhanced by lock table.

        Debug output enhanced to supervise deadlock detection.

        For waiting clients there is now a table of wait entries
        maintained. This table is used for deadlock detection.

        In module body:
        Parallel deadlock detection is started now.

    Revision 1.2.2.1  1996/05/20 09:33:34  rbnix
        Handling of orhpans added.

    Revision 1.2  1996/03/08 11:19:59  rbnix
        Journal output for variant TestServerScheduler added.

    Revision 1.1  1996/02/26 17:59:36  rbnix
        First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- BaseServerScheduler ------------------------------------------------

 | ------------------------------------------------------------------------
 *)
IMPORT Thread, Fmt, Pathname, TextSeq;
IMPORT Config, Variant, Journal,
       PageFile, PageFileSystem, PageCache, Access, PageLock, ServedClient,
       ServerLockTable, ServerWaitEntry, ServerWaitTbl, DeadlockDetection;


VAR
  dataChanged    := NEW(Thread.Condition);
  waitingClients := NEW(ServerWaitTbl.Default).init();


PROCEDURE WaitAccess (client: ServedClient.T;
                      locks : ServerLockTable.T;
                      file  : Pathname.T;
                      pageNo: CARDINAL;
                      lock  : PageLock.ServerMode) RAISES {Access.Locked} =
  VAR waitEntry: ServerWaitEntry.T;
  BEGIN
    IF Variant.TestServerScheduler THEN
      Journal.Add(
        "BaseServerScheduler.WaitAccess START (" & "client = "
          & client.getID() & ", file = " & file & ", pageNo = "
          & Fmt.Int(pageNo) & ", lock = " & PageLock.FmtMode(lock) & ")");
    END;

    TRY
      TRY
        waitEntry :=
          ServerWaitEntry.T{waitingClient := client, otherClients := locks,
                            waitingThread := Thread.Self(), age :=
                            client.getTransactionNumber(), file := file,
                            pageNo := pageNo, lock := lock};
        EVAL waitingClients.put(client, waitEntry);
        PageCache.WaitAccess(dataChanged);
      FINALLY
        EVAL waitingClients.delete(client, waitEntry);
      END;

    EXCEPT
    | Thread.Alerted =>
        IF Variant.TestServerScheduler THEN
          Journal.Add("BaseServerScheduler.WaitAccess ABORTED ("
                        & "client = " & client.getID() & ", file = " & file
                        & ", pageNo = " & Fmt.Int(pageNo) & ", lock = "
                        & PageLock.FmtMode(lock) & ")");
        END;

        (* orphan or deadlock detected, silent exit *)
        RAISE Access.Locked;
    END;

    IF Variant.TestServerScheduler THEN
      Journal.Add(
        "BaseServerScheduler.WaitAccess END (" & "client = "
          & client.getID() & ", file = " & file & ", pageNo = "
          & Fmt.Int(pageNo) & ", lock = " & PageLock.FmtMode(lock) & ")");
    END;
  END WaitAccess;


PROCEDURE SignalAccess () =
  BEGIN
    Thread.Broadcast(dataChanged);
  END SignalAccess;


PROCEDURE MakeResourcePath (resourceName: Pathname.T;
                            temporary: BOOLEAN): Pathname.Arcs
  RAISES {PageFile.NoAccess} =
  (* The Server calls Config.Login. Config.Unspecified will never be raised. *)
  <* FATAL Config.Unspecified *>
  VAR
    rootPath, resourcePath: Pathname.Arcs;
  BEGIN
    TRY
      rootPath := Pathname.Decompose (Config.GetRootPrefix (temporary));

      (* assure that resource names are only relative *)
      IF Pathname.Absolute (resourceName) THEN
        RAISE PageFile.NoAccess(
                  "Resource with absolute pathname not allowed: " & resourceName);
      END;

      (* a relative path has a leading NIL arc, this has to be removed *)
      resourcePath := Pathname.Decompose (resourceName);
      EVAL resourcePath.remlo ();

      RETURN TextSeq.Cat (rootPath, resourcePath);
    EXCEPT
    | Pathname.Invalid =>
      RAISE PageFile.NoAccess (
                "Wrong path name for resource '" & resourceName & "'");
    END
  END MakeResourcePath;


PROCEDURE MakeResourceName (resourceName: Pathname.T;
                            temporary: BOOLEAN): Pathname.T
  RAISES {PageFile.NoAccess} =
  BEGIN
    TRY
      RETURN Pathname.Compose(MakeResourcePath(resourceName, temporary));
    EXCEPT
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess(
                "Wrong path name for resource '" & resourceName & "'");
    END;
  END MakeResourceName;


(*
  This function checks wether the given path joined with the existing
  resources forms a tree. The resourceName designates a well formed path if
  it extends the hierarchy with a leaf.

  Formally this is with a new path resourceName = R = (R1, .., Rn) iff:
  1) for all i = 1..n-1. no resource exists with name (R1, .., Ri),
     (R will not cover an existing resource path.)

  2) if (R1, .., Rn) exists => there exists no (R1, .., Rn, Rn+1)
     and the dir Rn is empty.
     (R will not be covered by an existing resource path.)

  A directory is currently identified as resource only if it contains at
  least one file and any number of possibly nested subdirectories. 
 *)
PROCEDURE CheckResourcePath (resourceName: Pathname.T; temporary: BOOLEAN)
  RAISES {PageFile.NoAccess} =
  (* The Server calls Config.Login.  Config.Unspecified will never be raised. *)
  <* FATAL Config.Unspecified *>
  VAR
    rootPath, resourcePath: Pathname.Arcs;

  PROCEDURE IsResourcePath	(name		:Pathname.T) :BOOLEAN
				RAISES {PageFile.NoAccess} =
  BEGIN
    RETURN PageFileSystem.ExistsDir (name) AND
           (PageFileSystem.GetFileNames (name).size() > 0)
  END IsResourcePath;

  PROCEDURE EmptySubTree(name: Pathname.T): BOOLEAN RAISES {PageFile.NoAccess} =
    (* checks the complete subtree under name for files *)
    VAR subs: TextSeq.T;
    BEGIN
      IF PageFileSystem.GetFileNames(name).size () > 0 THEN
        RETURN FALSE;
      ELSE
        subs := PageFileSystem.GetDirNames(name);
        FOR i := 0 TO subs.size() - 1 DO
          IF NOT EmptySubTree(Pathname.Join(name, subs.get(i),NIL)) THEN
            RETURN FALSE;
          END;
        END;
      END;
      RETURN TRUE;
    END EmptySubTree;
    
  (* CheckResourcePath *)
  BEGIN
    TRY
      rootPath := Pathname.Decompose (Config.GetRootPrefix (temporary));

      (* assure that resource names are only relative *)
      IF Pathname.Absolute (resourceName) THEN
        RAISE PageFile.NoAccess(
                  "Resource with absolute pathname not allowed: " & resourceName);
      END;

      resourcePath := Pathname.Decompose(resourceName);

      (* check for empty path *)
      IF resourcePath.size() < 1 THEN
        RAISE
          PageFile.NoAccess("Illegal resource path '" & resourceName &
                            "'. The must be at least one path component.");
      END;

      (* check point 1) *)
      FOR i := 1 TO resourcePath.size() - 2 DO
        rootPath.addhi(resourcePath.get(i));
        IF IsResourcePath (Pathname.Compose (rootPath)) THEN
          RAISE PageFile.NoAccess(
                  " Illegal resource path '" & resourceName &
                  "'. It covers an already existing path.");
        END;
      END;
      
      (* check point 2) *)
      rootPath.addhi(resourcePath.get(resourcePath.size ()-1));
      WITH rootName = Pathname.Compose (rootPath) DO
        IF PageFileSystem.ExistsDir (rootName) AND NOT EmptySubTree(rootName) THEN
          RAISE PageFile.NoAccess(
                    " Illegal resource path '" & resourceName &
                    "'. It is covered by an already existing path.");
        END;
      END;
    EXCEPT
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess(
                "Wrong path name for resource '" & resourceName & "'");
    END;
  END CheckResourcePath;


PROCEDURE CreateResourcePath (resourceName: Pathname.T;
                              temporary: BOOLEAN;
                              omitLastComponent :BOOLEAN := FALSE)
  RAISES {PageFile.NoAccess} =
  (* The Server calls Config.Login.  Config.Unspecified will never be raised. *)
  <* FATAL Config.Unspecified *>
  VAR
    rootPath, resourcePath: Pathname.Arcs;
    name                  : Pathname.T;
    n			  : CARDINAL;
  BEGIN
    CheckResourcePath(resourceName, temporary);

    TRY
      rootPath := Pathname.Decompose(Config.GetRootPrefix (temporary));
      IF temporary THEN
        (* ensure temporary root exists *)
        PageFileSystem.MakePath(Config.GetRootPrefix(temporary));
      END;
      resourcePath := Pathname.Decompose(resourceName);

      n := resourcePath.size () - 1;
      IF omitLastComponent THEN DEC (n); END;

      FOR i := 1 TO n DO
        rootPath.addhi(resourcePath.get(i));
        name := Pathname.Compose(rootPath);
        IF NOT PageFileSystem.ExistsDir(name) THEN
          PageFileSystem.MakeDir(name);
        END;
      END;

    EXCEPT
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess(
                "Wrong path name for resource '" & resourceName & "'");
    END;
  END CreateResourcePath;


PROCEDURE DestroyResourcePath (resourceName: Pathname.T; temporary: BOOLEAN;
                               omitLastComponent :BOOLEAN := FALSE)
  RAISES {PageFile.NoAccess} =
  (* The Server calls Config.Login.  Config.Unspecified will never be raised. *)
  <* FATAL Config.Unspecified *>
  VAR
    name                  : Pathname.T;
    rootPath, resourcePath: Pathname.Arcs;
    n                     : CARDINAL;
  BEGIN
    TRY
      rootPath := Pathname.Decompose(Config.GetRootPrefix (temporary));
      resourcePath := Pathname.Decompose(resourceName);

      n := resourcePath.size () - 1;
      IF omitLastComponent THEN DEC (n); END;

      FOR i := 1 TO n DO
        rootPath.addhi(resourcePath.get(i));
      END;

      (* removing dirs starting from inner most to root *)
      FOR i := n TO 1 BY -1 DO
        name := Pathname.Compose (rootPath);
        IF PageFileSystem.ExistsDir (name) AND
           (PageFileSystem.GetDirNames (name).size () = 0) AND
           (PageFileSystem.GetFileNames (name).size () = 0) THEN
          PageFileSystem.RemoveDir (name);
        ELSE
          RETURN
        END;
        EVAL rootPath.remhi ();
      END;
    EXCEPT
    | Pathname.Invalid =>
        RAISE PageFile.NoAccess(
                "Wrong path name for resource '" & resourceName & "'");
    END;
  END DestroyResourcePath;


BEGIN
  DeadlockDetection.StartChecks(waitingClients);
END BaseServerScheduler.
