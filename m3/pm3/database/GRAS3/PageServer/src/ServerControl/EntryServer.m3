MODULE EntryServer;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:49  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:38  hosking
    Import of GRAS3 1.1

    Revision 1.18  1998/01/30 13:19:34  roland
    Lock on PageCache must not be hold after raising an exception.

    Revision 1.17  1997/10/31 17:11:54  roland
    A simple access control scheme.

    Revision 1.16  1997/05/16 08:16:40  roland
    Bugfix: Method exit must use the same name server as init.

    Revision 1.15  1997/03/26 16:12:10  renehuel
    A new pageserver now logs in to the nameserver with his id, handle
    and new : the id of the user who started it.

    Revision 1.14  1997/03/20 16:55:26  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.13  1996/11/21 07:55:40  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.12  1996/11/20 12:11:13  roland
    Improved exception handling and startup.

    Revision 1.11  1996/11/14 14:13:28  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.10  1996/11/08 14:45:54  roland
    GetResources in ServerScheduler handles PageFile.NoAccess correct now.

    Revision 1.9  1996/10/08 13:09:08  roland
    Only one entry is allowed in a network. Error handling improved.
    Changes to EntryServer undone.

    Revision 1.8  1996/10/08 12:24:47  rbnix
        Function Init extended for check on already existing server.

    Revision 1.7  1996/08/06 16:32:25  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.3  1996/08/01 18:12:29  rbnix
        New resource administration methods for remote resource parts
        added: deleteResource, copyResource, renameResource,
        existsResource, resourceInUse and getResources.

    Revision 1.6.2.2  1996/07/24 12:52:48  rbnix
        New parameter clientInfo added.

    Revision 1.6.2.1  1996/04/29 14:23:00  roland
    ExceptionHandling and error messages improved.

    Revision 1.6  1996/03/15 14:22:26  rbnix
        In method init/open attribute clientID added.

    Revision 1.5  1996/03/01 13:38:30  rbnix
        New method EntryServer.T.exit to remove port.

    Revision 1.4  1996/03/01 13:01:04  rbnix
        Output to journal for variant TestServerCommunication added.

    Revision 1.3  1996/02/29 09:25:51  rbnix
        Method shutdown to terminate server added.

    Revision 1.2  1996/02/28 11:04:43  rbnix
        Usage of new configuration storage introduced.

    Revision 1.1  1996/02/26 17:58:22  rbnix
        First version of subsystem ServerControl.

*)
(***************************************************************************)
IMPORT Thread, NetObj, Pathname, Fmt,
       TextTransientSeq AS TextSeq, Config, Variant, Journal,
       PageFile, PageCache, Access, Termination, CallbackPort,
       CommunicationPort, ClientInfo, ClientInfoSeq, ServerScheduler,
       CommunicationServer, NameServer, Uugid, Text;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init := Init;
        exit := Exit;
        open := Open;
        ping := Ping;

        deleteRemoteResource  := DeleteRemoteResource;
        copyRemoteResource    := CopyRemoteResource;
        renameRemoteResource  := RenameRemoteResource;
        existsRemoteResource  := ExistsRemoteResource;
        remoteResourceInUse   := RemoteResourceInUse;
        getRemoteResources    := GetRemoteResources;
        getRemoteResourceUser := GetRemoteResourceUser;

        shutdown := Shutdown;
      END;

PROCEDURE Ping (<* UNUSED *> self: T) =
  BEGIN
  END Ping;


PROCEDURE Init (self: T; ID: TEXT; UserID: INTEGER := 0): T
  RAISES {NetObj.Error, NetObj.Invalid, Thread.Alerted,
          NameServer.InvalidServerIdentification,
          NameServer.ServerAlreadyInList, NameServer.NoNameServer} =
  VAR GrasNameServer: NameServer.T;
  BEGIN
    GrasNameServer := NetObj.Import(NameServer.GrasNameServerID,
                                    NetObj.Locate(Config.GetNameServer()));
    IF GrasNameServer # NIL THEN
      GrasNameServer.addserver(ID, self, UserID)
    ELSE
      RAISE NameServer.NoNameServer
    END;
    RETURN self;
  END Init;


PROCEDURE Exit (<* UNUSED *> self: T; ID: TEXT)
  RAISES {NetObj.Error, NetObj.Invalid, Thread.Alerted} =
  VAR GrasNameServer: NameServer.T;
  BEGIN
    TRY
      GrasNameServer :=
        NetObj.Import(NameServer.GrasNameServerID,
                      NetObj.Locate(Config.GetNameServer()));
      GrasNameServer.removeserver(ID)
    EXCEPT
    | NameServer.ServerNotInList =>
    | NameServer.InvalidServerIdentification =>
        Journal.Add("Invalid server identification!");
    END;
  END Exit;


PROCEDURE Open (<* UNUSED *>               self      : T;
                                           baseName  : Pathname.T;
                                           access    : Access.Mode;
                                           new       : BOOLEAN;
                                           callback  : CallbackPort.T;
                                           clientInfo: ClientInfo.T;
                             VAR (* out *) clientID  : TEXT            ):
  CommunicationPort.T
  RAISES {PageFile.NoAccess, Access.Denied, Access.Invalid} =
  VAR communicationServer: CommunicationServer.T;
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add(
        "EntryServer.Open (" & "baseName = " & baseName & ", access = "
          & Access.FmtMode(access) & ", new = " & Fmt.Bool(new) & ")");
    END;

    TRY
      (* A simple access control pattern: if this server has a default
         ServerId and the user-id of the server-process is not equal to that
         of the client, deny access. *)
      IF Text.Equal(Config.GetGrasServerId(), Config.DefaultServerId()) THEN
        IF Uugid.getuid() # clientInfo.userid THEN
          RAISE Access.Denied("Not a public server!");
        END;
      END;
      communicationServer :=
        NEW(CommunicationServer.T).init(
          baseName, access, new, callback, clientInfo, clientID);
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN communicationServer;
  END Open;


PROCEDURE DeleteRemoteResource (<* UNUSED *> self: T; baseName: Pathname.T)
  RAISES {PageFile.NoAccess} =
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add("EntryServer.DeleteRemoteResource (" & "baseName = "
                    & baseName & ")");
    END;

    TRY
      ServerScheduler.DeleteResource(baseName);
    FINALLY
      PageCache.EndAccess();
    END;
  END DeleteRemoteResource;


PROCEDURE CopyRemoteResource (<* UNUSED *> self      : T;
                                           sourceName: Pathname.T;
                                           destName  : Pathname.T  )
  RAISES {PageFile.NoAccess} =
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add("EntryServer.CopyRemoteResource (" & "sourceName = "
                    & sourceName & ", destName = " & destName & ")");
    END;

    TRY
      ServerScheduler.CopyResource(sourceName, destName);
    FINALLY
      PageCache.EndAccess();
    END;
  END CopyRemoteResource;


PROCEDURE RenameRemoteResource (<* UNUSED *> self   : T;
                                             oldName: Pathname.T;
                                             newName: Pathname.T  )
  RAISES {PageFile.NoAccess} =
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add("EntryServer.RenameRemoteResource (" & "oldName = "
                    & oldName & ", newName = " & newName & ")");
    END;

    TRY
      ServerScheduler.RenameResource(oldName, newName);
    FINALLY
      PageCache.EndAccess();
    END;
  END RenameRemoteResource;


PROCEDURE ExistsRemoteResource (<* UNUSED *> self: T; baseName: Pathname.T):
  BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add("EntryServer.ExistRemoteResource (" & "baseName = "
                    & baseName & ")");
    END;

    TRY
      result := ServerScheduler.ExistsResource(baseName);
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END ExistsRemoteResource;


PROCEDURE RemoteResourceInUse (<* UNUSED *> self: T; baseName: Pathname.T):
  BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add("EntryServer.RemoteResourceInUse (" & "baseName = "
                    & baseName & ")");
    END;

    TRY
      result := ServerScheduler.ResourceInUse(baseName);
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END RemoteResourceInUse;


PROCEDURE GetRemoteResourceUser (<* UNUSED *> self: T; baseName: Pathname.T):
  ClientInfoSeq.T =
  VAR result: ClientInfoSeq.T;
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add("EntryServer.RemoteResourceUsers (" & "baseName = "
                    & baseName & ")");
    END;

    TRY
      result := ServerScheduler.GetResourceUser(baseName);
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END GetRemoteResourceUser;


PROCEDURE GetRemoteResources (<* UNUSED *> self: T): TextSeq.T
  RAISES {PageFile.NoAccess} =
  VAR result: TextSeq.T;
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add("EntryServer.GetRemoteResources ()");
    END;

    TRY
      result := ServerScheduler.GetResources();
    FINALLY
      PageCache.EndAccess();
    END;

    RETURN result;
  END GetRemoteResources;


PROCEDURE Shutdown (<* UNUSED *> self: T; termination: Termination.Mode)
  RAISES {Termination.StillInUse} =
  BEGIN
    PageCache.BeginAccess();
    IF Variant.TestServerCommunication THEN
      Journal.Add("EntryServer.Shutdown (" & "termination = "
                    & Termination.FmtMode(termination) & ")");
    END;

    TRY
      ServerScheduler.Shutdown(termination);
    FINALLY
      PageCache.EndAccess();
    END
  END Shutdown;


BEGIN
END EntryServer.
