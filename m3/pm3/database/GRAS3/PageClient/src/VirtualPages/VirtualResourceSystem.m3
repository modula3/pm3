MODULE VirtualResourceSystem;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.15  1998/07/29 15:11:19  roland
    Increase the default stack size at login time.

    Revision 1.14  1998/02/10 16:38:17  roland
    LocalResourceSystem is now used for renaming, copying etc. of the
    local part of resources.

    Revision 1.13  1997/11/03 12:41:19  roland
    Removed debug output.

    Revision 1.12  1997/10/31 14:14:51  roland
    Adapted to new RuleEngine.

    Revision 1.11  1997/05/16 08:48:54  roland
    Stack exceptions will never be raised, hence open methods need not raise
    FatalError.

    Revision 1.10  1997/03/26 16:08:54  renehuel
    Added handling of new exception NameServer.InvalidServerIdentification.

    Revision 1.9  1997/03/21 17:04:51  roland
    Adapted to changed Config. Login parameters are all optional except
    for root directory. Default server name is computed by Config.

    Revision 1.8  1997/03/20 16:55:08  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.7  1997/02/21 08:21:58  roland
    PageChache initialization at login time.

    Revision 1.6  1996/11/21 15:22:55  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.5  1996/11/21 13:47:41  roland
    FATAL pragma in delete resource removed.

    Revision 1.4  1996/11/21 07:54:51  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.3  1996/11/18 17:52:29  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.2  1996/08/06 16:34:25  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/08/01 18:14:04  rbnix
        New module to administrate resources in whole added.

*)
(***************************************************************************)
(*
 | --- VirtualResourceSystem ----------------------------------------------
 Because of resource handling for whole resources is separated completely
 from scheduled resources there is no neccessity to guard operations
 (PageCache.BeginAccess / PageCache.EndAccess).
 | ------------------------------------------------------------------------
 *)
IMPORT Pathname, TextSeq, Thread, NetObj, Config, PageCache, PageFile,
       ErrorSupport, EntryPort, ClientInfoSeq, NameServer;
IMPORT RuleEngine, Process;
IMPORT LocalResourceSystem;

(*
 | --- support stuff ------------------------------------------------------
 *)
PROCEDURE EasyImportEntry (): EntryPort.T RAISES {FatalError} =
  VAR
    port: EntryPort.T;
    NS  : NameServer.T;
  BEGIN
    TRY
      NS := NetObj.Import(NameServer.GrasNameServerID,
                          NetObj.Locate(Config.GetNameServer()));
      IF NS # NIL THEN
        port := NS.getserver(Config.GetGrasServerId());
      ELSE
        RAISE NameServer.NoNameServer
      END;
    EXCEPT
    | Thread.Alerted =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.EasyImportEntry",
                           "Connect to server interrupted!"));

    | NetObj.Error (code) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "VirtualResourceSystem.EasyImportEntry",
                           "NetObj.Error", code));

    | NetObj.Invalid =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.EasyImportEntry",
                           "Identification of name server invalid" & " ("
                             & Config.GetNameServer() & ")"));
    | NameServer.NoNameServer =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.EasyImportEntry",
                           "No gras-nameserver found!"));
    | NameServer.ServerNotInList =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.EasyImportEntry",
                           "Specified server " & Config.GetGrasServerId()
                             & " does not exist!"))
    | NameServer.InvalidServerIdentification =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.EasyImportEntry",
                           "Identification of page server is invalid"))

    END;

    RETURN port;
  END EasyImportEntry;


(*
 | --- public stuff -------------------------------------------------------
 *)

PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := 0;
                 grasserver: TEXT         := NIL;
                 nameserver: TEXT         := NIL  ) =
  VAR remoteMonitoring: BOOLEAN;
      msg: TEXT;
  BEGIN
    IF cachesize = 0 THEN cachesize := Config.DefaultCacheSize END;
    IF nameserver = NIL THEN nameserver := Config.DefaultNameServer END;
    IF grasserver = NIL THEN grasserver := Config.DefaultServerId() END;
    Config.Login(root, cachesize, nameserver, grasserver);
    Thread.MinDefaultStackSize(Config.DefaultStackSize);
    WITH sid = Config.GetGrasServerId() DO
      RuleEngine.Login(nameserver, sid, remoteMonitoring, msg);
      Process.RegisterExitor(RuleEngine.Logout);
    END;
    PageCache.BeginAccess();
    PageCache.Init(Config.GetCacheSize());
    PageCache.EndAccess();
  END Login;

PROCEDURE DeleteResource (baseName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      EasyImportEntry().deleteRemoteResource(baseName);
      LocalResourceSystem.DeleteLocalResource(baseName);
    EXCEPT
      NetObj.Error (code) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "VirtualResourceSystem.DeleteResource",
                           "NetObj.Error ", code));
    | Thread.Alerted =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.DeleteResource",
                           "Thread.Alerted"));
    END;
  END DeleteResource;


PROCEDURE CopyResource (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      EasyImportEntry().copyRemoteResource(sourceName, destName);
      LocalResourceSystem.CopyLocalResource(sourceName, destName);
    EXCEPT
      NetObj.Error (code) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "VirtualResourceSystem.CopyResource",
                           "NetObj.Error ", code));
    | Thread.Alerted =>
        RAISE FatalError(
                ErrorSupport.Create(
                  "VirtualResourceSystem.CopyResource", "Thread.Alerted"));
    END;
  END CopyResource;


PROCEDURE RenameResource (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      EasyImportEntry().renameRemoteResource(oldName, newName);
      LocalResourceSystem.RenameLocalResource(oldName, newName);
    EXCEPT
      NetObj.Error (code) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "VirtualResourceSystem.RenameResource",
                           "NetObj.Error ", code));
    | Thread.Alerted =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.RenameResource",
                           "Thread.Alerted"));
    END;
  END RenameResource;


PROCEDURE ExistsResource (baseName: Pathname.T): BOOLEAN
  RAISES {FatalError} =
  BEGIN
    TRY
      RETURN EasyImportEntry().existsRemoteResource(baseName);
    EXCEPT
      NetObj.Error (code) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "VirtualResourceSystem.ExistsResource",
                           "NetObj.Error ", code));
    | Thread.Alerted =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.ExistsResource",
                           "Thread.Alerted"));
    END;
  END ExistsResource;


PROCEDURE ResourceInUse (baseName: Pathname.T): BOOLEAN
  RAISES {FatalError} =
  BEGIN
    TRY
      RETURN EasyImportEntry().remoteResourceInUse(baseName);
    EXCEPT
      NetObj.Error (code) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "VirtualResourceSystem.ResourceInUse",
                           "NetObj.Error ", code));
    | Thread.Alerted =>
        RAISE
          FatalError(
            ErrorSupport.Create(
              "VirtualResourceSystem.ResourceInUse", "Thread.Alerted"));
    END;
  END ResourceInUse;


PROCEDURE GetResourceUser (baseName: Pathname.T): ClientInfoSeq.T
  RAISES {FatalError} =
  BEGIN
    TRY
      RETURN EasyImportEntry().getRemoteResourceUser(baseName);
    EXCEPT
      NetObj.Error (code) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "VirtualResourceSystem.GetResourceUser",
                           "NetObj.Error ", code));
    | Thread.Alerted =>
        RAISE FatalError(ErrorSupport.Create(
                           "VirtualResourceSystem.GetResourceUser",
                           "Thread.Alerted"));
    END;
  END GetResourceUser;


PROCEDURE GetResources (): TextSeq.T
  RAISES {PageFile.NoAccess, FatalError} =
  BEGIN
    TRY
      RETURN EasyImportEntry().getRemoteResources();
    EXCEPT
      NetObj.Error (code) =>
        RAISE FatalError(ErrorSupport.Propagate(
                           "VirtualResourceSystem.GetResources",
                           "NetObj.Error ", code));
    | Thread.Alerted =>
        RAISE FatalError(
                ErrorSupport.Create(
                  "VirtualResourceSystem.GetResources", "Thread.Alerted"));
    END;
  END GetResources;


BEGIN
END VirtualResourceSystem.
