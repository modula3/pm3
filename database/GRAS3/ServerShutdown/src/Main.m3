MODULE Main;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.6  1997/12/15 16:33:48  roland
    New man page and unified command line switches.

    Revision 1.5  1997/10/14 09:03:53  roland
    Return code 1 if server rejected shutdown request.

    Revision 1.4  1997/05/16 08:19:36  roland
    Executable renamed back to g3shutdown. Parameter logic and output
    messages changed.

    Revision 1.3  1997/03/26 16:17:15  renehuel
    Program can now :
    - shut down pageserver named by an id
    - list all running pageservers
    A user can only shut down a server which he has started.

    Revision 1.2  1997/03/25 16:15:23  renehuel
    The program was modified to use the new gras name-server.
    A server id can be specified to shut down a specific server.

    Revision 1.1  1996/11/26 16:25:56  roland
    A small program to shutdown the Gras-3-Server from outside. Based on
    Test/Shutdown/src/TestShutdown.m3 with a little more exception
    handling.

*)
(***************************************************************************)

IMPORT ParseParams, IO, Process, Thread, NetObj, Pathname, Fmt, Uugid;
IMPORT EntryPort, ErrorSupport, Termination, Config, NameServer;

VAR
  entry     : EntryPort.T;
  nameserver: TEXT;
  ServerID  : TEXT         := NIL;
  NS        : NameServer.T;
  PrintList : BOOLEAN;
  Terminate : BOOLEAN;

PROCEDURE PrintHelpAndExit (name: TEXT; status: CARDINAL) =
  BEGIN
    IO.Put("USAGE: " & name
             & " [-id [<serverid>]] [-agent <hostname>] [-l] [-h]\n\n");
    IO.Put("       <serverid> is the id of the page-server "
             & "that should be terminated.\n" & "        Default is '"
             & Config.DefaultServerId() & "'.\n");
    IO.Put("       <hostname> is the name of the machine "
             & "running the netobject demon.\n");
    IO.Put("       -l prints a list of running gras name-servers.\n");
    IO.Put("       -h prints this help text\n\n");
    IO.Put("       Without arguments tries to shut down a GRAS-3 "
             & "server with default id.\n\n");
    Process.Exit(status);
  END PrintHelpAndExit;

PROCEDURE ReadParameters () =
  BEGIN
    WITH pp       = NEW(ParseParams.T).init(NIL),
         progname = Pathname.Last(pp.arg[0])      DO
      IF pp.keywordPresent("-h") THEN PrintHelpAndExit(progname, 0); END;

      IF pp.keywordPresent("-agent") THEN
        TRY
          nameserver := pp.getNext();
        EXCEPT
          ParseParams.Error =>
            IO.Put("Parameter -agent must have an additional value!\n");
            Process.Exit(0)
        END
      ELSE
        (* ok, we can live without *)
        IO.Put("Name server not specified, using default.\n");
        nameserver := Config.DefaultNameServer;
      END;

      (* Program terminates a server only if '-id' is present or '-id' and
         '-l' are not present. *)
      Terminate := FALSE;
      ServerID := Config.DefaultServerId();
      IF pp.keywordPresent("-id") THEN
        Terminate := TRUE;
        TRY
          ServerID := pp.getNext();
        EXCEPT
          ParseParams.Error =>
            IO.Put("Parameter serverid not specified, using default.\n");
            ServerID := Config.DefaultServerId();
        END
      END;

      PrintList := pp.keywordPresent("-l");
      Terminate := Terminate OR NOT PrintList;

      TRY
        pp.finish();
      EXCEPT
        ParseParams.Error => PrintHelpAndExit(progname, 1);
      END;
    END;
  END ReadParameters;

PROCEDURE PrintServerList (NS: NameServer.T) =
  VAR serverlist: NameServer.AllServerList;
  BEGIN
    TRY
      serverlist := NS.listallservers();
    EXCEPT
      NetObj.Error => IO.Put("NetObj Error.\n");
    | Thread.Alerted => IO.Put("Thread Alerted.\n");
    END;
    IF serverlist # NIL THEN
      IO.Put("\nList of known page-servers :\n");
      FOR i := 1 TO NUMBER(serverlist^) DO
        IO.Put(Fmt.Int(i) & ". " & serverlist^[i - 1].ID & "\t OwnerID : "
                 & Fmt.Int(serverlist^[i - 1].OwnerID) & "\n")
      END;
      IO.Put("\n")
    ELSE
      IO.Put("The server-list is empty!\n")
    END
  END PrintServerList;

PROCEDURE ErrorAndExit (error: TEXT) =
  BEGIN
    IO.Put(error);
    Process.Exit(1);
  END ErrorAndExit;


PROCEDURE TryShutDown (NS: NameServer.T; ServerID: TEXT) =
  VAR serverinfo: NameServer.ServerInfo;
  BEGIN
    (* Getting a handle to the desired server, and checking if ownerid of
       the server is equal to current userid.  Otherwise it is not allowed
       to shut down the server.*)
    TRY
      serverinfo := NS.getserverinfo(ServerID);
      IF serverinfo.OwnerID = 0 THEN
        IO.Put("Server '" & ServerID & "' not found!\n");
      ELSIF serverinfo.OwnerID = Uugid.getuid() THEN
        entry := NS.getserver(ServerID);
        (* trying to shut down server *)
        IO.Put("Server found. Trying to shut down...\n");
        entry.shutdown(Termination.Mode.Try);
        IO.Put("Server shut down sucessful\n");
      ELSE
        IO.Put("You are not owner of server '" & ServerID
                 & "'. Permission denied!\n")
      END
    EXCEPT
    | NameServer.ServerNotInList =>
        ErrorAndExit("Server '" & ServerID & "' not found!\n");
    | Termination.StillInUse =>
        IO.Put("Server rejected shut down request.\n");
        Process.Exit(1);
    | NetObj.Error (code) =>
        IO.Put("Communication error: " & ErrorSupport.Fmt(code) & "\n");
    | Thread.Alerted => IO.Put("Interrupted!\n");
    | NameServer.InvalidServerIdentification =>
        IO.Put("Invalid server identification!\n")
    END
  END TryShutDown;

BEGIN
  ReadParameters();
  (* getting a handle to the nameserver *)
  IO.Put(
    "Looking for gras name server via name server '" & nameserver & "'\n");
  TRY
    NS := NetObj.Import(
            NameServer.GrasNameServerID, NetObj.Locate(nameserver));
    IF NS = NIL THEN RAISE NameServer.NoNameServer END;
  EXCEPT
  | NetObj.Invalid => ErrorAndExit("Invalid hostname.\n");
  | NetObj.Error (code) =>
      ErrorAndExit("Communication error: " & ErrorSupport.Fmt(code)
                     & "\nProbably netobjd is not running on host "
                     & nameserver & "\n");
  | Thread.Alerted => ErrorAndExit("Interrupted!\n");
  | NameServer.NoNameServer =>
      ErrorAndExit("No gras name-server found!\n");
  END;

  IF PrintList THEN PrintServerList(NS) END;

  IF Terminate THEN TryShutDown(NS, ServerID) END
END Main.
