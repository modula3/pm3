MODULE Main;

IMPORT NameServer, NetObj, IO, Thread, ErrorSupport, NetObjNotifier,
       Pathname, Fmt, Rd, Wr, OSError, Process, Env, FileWr;
IMPORT Atom, TCP, Text, ParseParams, Stdio, EntryPort, Config;
IMPORT Daemon;

(* The GrasNameServer has two purposes.  First of all it implements the
   NameServer-Interface for GRAS and hence can act as the name server to
   all GRAS-Applications.  Second, it is able to communicate with an
   already running name server and query its state.  In this way, users are
   able to receive the status of the GRAS name service. *)

TYPE
  NSMode = {Verbose, Quiet};

  Operation = {None, Install, Shutdown, List, CleanUp, Help};
  OpSet = SET OF Operation;

  ServerEntry = RECORD
                  ID     : TEXT;
                  Handle : EntryPort.T;
                  OwnerID: INTEGER;
                END;

  InternServerList = REF ARRAY OF ServerEntry;

  Server = NameServer.T BRANDED "GrasNameServer" OBJECT
             serverList          : InternServerList;
             notifier            : NetObjNotifier.NotifierClosure;
             mainMutex, listMutex: Thread.Mutex;
             shutDown            : Thread.Condition;
           METHODS
             init (force: BOOLEAN): Server RAISES {} := Init;
           OVERRIDES
             listallservers := ListAllServers;
             addserver      := AddServer;
             removeserver   := RemoveServer;
             getserver      := GetServer;
             serverinlist   := ServerInList;
             shutdown       := ShutDown;
             getserverinfo  := GetServerInfo;
             cleanup        := CleanUp;
           END;

(* ----------------- Internal stuff ----------------------------------- *)

(* We have two kinds of threads: A Pinger pings one server and removes it
   from the serverList if an error occurs.  The PeriodicPinger wakes up
   every 60 mins and traverses the list of servers to start a Pinger for
   each entry. *)

TYPE
  Pinger = Thread.Closure OBJECT
             ns    : Server;
             server: ServerEntry;
           OVERRIDES
             apply := PingerApply;
           END;

PROCEDURE PingerApply (cl: Pinger): REFANY =
  BEGIN
    TRY
      cl.server.Handle.ping();
    EXCEPT
    ELSE
      (* try to remove the server from the main list *)
      TRY RemServerFromList(cl.ns, cl.server.ID); EXCEPT ELSE END;
    END;
    RETURN NIL;
  END PingerApply;

PROCEDURE ForkPingers (ns: Server) =
  VAR list: InternServerList;
  BEGIN
    LOCK ns.listMutex DO
      (* copy the list *)
      list := NEW(InternServerList, NUMBER(ns.serverList^));
      list^ := ns.serverList^;
    END;
    FOR i := 0 TO LAST(list^) DO
      WITH cl = NEW(Pinger) DO
        cl.ns := ns;
        cl.server := list^[i];
        EVAL Thread.Fork(cl);
      END;
    END;
  END ForkPingers;

TYPE
  PeriodicPinger = Thread.Closure OBJECT
                     server: Server;
                   OVERRIDES
                     apply := PeriodicPingerApply;
                   END;

PROCEDURE PeriodicPingerApply (pp: PeriodicPinger): REFANY =
  BEGIN
    TRY
      LOOP Thread.AlertPause(60.0D0 * 60.0D0); ForkPingers(pp.server); END;
    EXCEPT
      Thread.Alerted =>
    END;
    RETURN NIL;
  END PeriodicPingerApply;


VAR Log: Wr.T := Stdio.stderr;

PROCEDURE GetIndexToID (NS: Server; ID: TEXT): INTEGER
  RAISES {NameServer.InvalidServerIdentification} =
  VAR i: INTEGER;
  BEGIN
    (* An index is used to run through the list.  When the end is reached
       or the desired entry is found, the increase of i is stopped.*)
    IF ID # NIL THEN
      i := 0;
      WHILE (i < NUMBER(NS.serverList^))
              AND (NOT Text.Equal(NS.serverList^[i].ID, ID)) DO
        i := i + 1
      END;
      IF i >= NUMBER(NS.serverList^) THEN i := -1 END;
      RETURN i
    ELSE
      RAISE NameServer.InvalidServerIdentification
    END
  END GetIndexToID;

PROCEDURE GetIndexToHandle (NS: Server; Handle: EntryPort.T): INTEGER =
  VAR i: INTEGER;
  BEGIN
    (* An index is used to run through the list.  When the end is reached
       or the desired entry is found, the increase of i is stopped.*)
    i := 0;
    WHILE (i < NUMBER(NS.serverList^))
            AND (NS.serverList^[i].Handle # Handle) DO
      i := i + 1
    END;
    IF i >= NUMBER(NS.serverList^) THEN i := -1 END;
    RETURN i
  END GetIndexToHandle;

PROCEDURE IsServerIdle (NS: Server; ID: TEXT): BOOLEAN
  RAISES {Thread.Alerted, NameServer.InvalidServerIdentification} =
  VAR index: INTEGER;
  BEGIN
    TRY
      index := GetIndexToID(NS, ID);
      NS.serverList^[index].Handle.ping();
      (* When no netobject error was raised, the ping call was succesfull,
         and the other server with the same id was still active.  The
         return value is FALSE.*)
      RETURN FALSE;
    EXCEPT
      (* When this exception is raised, the other server with the same ID
         was shut down, and can be removed from the list.*)
      NetObj.Error => RETURN TRUE;
    END;
  END IsServerIdle;

PROCEDURE RemServerFromList (NS: Server; ID: TEXT)
  RAISES {NameServer.ServerNotInList,
          NameServer.InvalidServerIdentification} =
  VAR
    i      : INTEGER;
    newList: InternServerList;
  BEGIN
    i := GetIndexToID(NS, ID);
    IF i # -1 THEN
      (* If the server which has to be deleted was found in the list, he is
         going to be removed. *)
      newList := NEW(InternServerList, NUMBER(NS.serverList^) - 1);
      FOR i := 0 TO i - 1 DO newList^[i] := NS.serverList[i] END;
      FOR i := i + 1 TO NUMBER(NS.serverList^) - 1 DO
        newList^[i - 1] := NS.serverList[i]
      END;
      NS.serverList := NIL;
      NS.serverList := newList;
      IF serverMode = NSMode.Verbose THEN
        IO.Put("Removed server '" & ID & "' from list." & "\n", Log)
      END;
    ELSE
      (* When the server which is to be removed from the list could not be
         found, an exception is raised. *)
      IF serverMode = NSMode.Verbose THEN
        IO.Put("Server \"" & ID & "\" not found!" & "\n", Log)
      END;
      RAISE NameServer.ServerNotInList
    END;
  END RemServerFromList;

PROCEDURE Notify (<* UNUSED *> NC : NetObjNotifier.NotifierClosure;
                               obj: NetObj.T;
                               st : NetObjNotifier.OwnerState       ) =
  VAR
    i : INTEGER;
    id: TEXT;
  BEGIN
    (* The notify method is used internally to remove servers from the list
       which no longer run.  The procedure is called by an notifier object,
       when the server was shut down. *)
    IF st = NetObjNotifier.OwnerState.Dead THEN
      LOCK nameServer.listMutex DO
        i := GetIndexToHandle(nameServer, obj);
        IF i # -1 THEN
          id := nameServer.serverList^[i].ID;
          IF serverMode = NSMode.Verbose THEN
            IO.Put("Server \"" & id
                     & "\" just died, and is beeing removed from list."
                     & "\n", Log)
          END;
          TRY
            RemServerFromList(nameServer, id)
          EXCEPT
            (* This exception cannot be raised, cause the list operations
               are locked by the listmutex. *)
            NameServer.ServerNotInList =>
            (* Internally this error cannot occur *)
          | NameServer.InvalidServerIdentification =>
          END
        END;
      END
    END;
  END Notify;


(* -------------- NameServer method implementations --------------- *)

PROCEDURE Init (NS: Server; force: BOOLEAN): Server RAISES {} =
  VAR
    existentServer: NameServer.T;
    noOtherServer : BOOLEAN;
    netobjd       : NetObj.Address;
    rd            : Rd.T;
    wr            : Wr.T;
    t             : TEXT;

  PROCEDURE CloseAndExit (wr: Wr.T) =
    BEGIN
      TRY Wr.Close(wr); EXCEPT ELSE END;
      Process.Exit(0);
    END CloseAndExit;

  BEGIN
    IF NOT Debug THEN
      TRY
        IF NOT Daemon.ForkDaemon(rd, wr) THEN
          (* This is the parent.  It waits until the input from the child
             is closed. *)
          TRY
            Wr.Close(wr);
            WHILE NOT Rd.EOF(rd) DO
              t := Rd.GetLine(rd);
              IO.Put(t & "\n", Stdio.stderr);
            END;
            Rd.Close(rd);
            Process.Exit(0);
          EXCEPT
          | Thread.Alerted =>
              IO.Put("Alerted!\n", Stdio.stderr);
              Process.Exit(1);
          | Rd.Failure =>
              IO.Put("Read failed!\n", Stdio.stderr);
              Process.Exit(1);
          | Wr.Failure =>
              IO.Put("Write failed!\n", Stdio.stderr);
              Process.Exit(1);
          | Rd.EndOfFile =>
              TRY Rd.Close(rd); EXCEPT ELSE END;
              Process.Exit(0);
          END;
        END;
      EXCEPT
        OSError.E (e) =>
          IO.Put("Unable to fork: " & ErrorSupport.ToText(e), Stdio.stderr);
          Process.Exit(1);
      END;

      (* This is the child *)
      TRY Rd.Close(rd); EXCEPT ELSE END;
    ELSE
      (* start in debugging mode, i.e.  do not fork and use stderr as wr *)
      wr := Stdio.stderr;
    END;

    TRY
      netobjd := NetObj.Locate(NetObjdServer);
    EXCEPT
    | NetObj.Error (al) =>
        IO.Put("NetObject Error:" & ErrorSupport.ToText(al) & "\n", wr);
        CloseAndExit(wr);
    | NetObj.Invalid =>
        IO.Put("NetObject invalid!\n", wr);
        IO.Put("Host '" & NetObjdServer & "' does not exist!\n", wr);
        CloseAndExit(wr);
    | Thread.Alerted => IO.Put("Alerted!\n", wr); CloseAndExit(wr);
    END;

    IF netobjd = NIL THEN
      IO.Put(
        "netobjd is not running on host '" & NetObjdServer & "'\n", wr);
      CloseAndExit(wr);
    END;

    TRY
      (* First we have to check whether another nameserver is running.*)
      existentServer :=
        NetObj.Import(NameServer.GrasNameServerID, netobjd);
      (* Another running server is indicated by a value # NIL *)
      IF existentServer # NIL THEN
        (* If the force flag is set, we have to try to shut down the other
           server. *)
        IF force THEN
          IF serverMode = NSMode.Verbose THEN
            IO.Put("Trying to shut down other server...", wr)
          END;
          noOtherServer := existentServer.shutdown();
          IF serverMode = NSMode.Verbose THEN
            IF noOtherServer THEN
              IO.Put("done." & "\n", wr);
            ELSE
              IO.Put("failed!" & "\n", wr);
            END
          END;
        END;
      ELSE
        noOtherServer := TRUE
      END;
    EXCEPT
      (* The following exception is only raised when the netobject demon
         holds the entry for a server which is no longer running.  It has
         to be removed from the list by exporting its name with the handle
         NIL if the force flag is set. *)
      (* This is not true anymore.  The exception can also be raised, if
         netobjd is not running. *)
      NetObj.Error (info) =>
        IF Atom.Equal(info.head, NetObj.CommFailure) THEN
          IF info.tail # NIL AND Atom.Equal(info.tail.head, TCP.Refused) THEN
            IF force THEN
              IO.Put("Idle server is beeing shut down...", wr);
              TRY
                NetObj.Export(NameServer.GrasNameServerID, NIL, netobjd);
              EXCEPT
                NetObj.Error (info) =>
                  IO.Put("NetObject Error:" & ErrorSupport.ToText(info)
                           & "\n", wr);
                  CloseAndExit(wr);
              | Thread.Alerted =>
                  IO.Put("Alerted!\n", wr);
                  CloseAndExit(wr);
              END;
              IO.Put("done.\n", wr);
              noOtherServer := TRUE
            ELSE
              IO.Put("Entry exists!\n", wr);
            END;
          ELSIF info.tail # NIL
                  AND Atom.Equal(info.tail.head, TCPUnexpected) THEN
            IO.Put("NetObj Error: netobjd might not be running on host '"
                     & NetObjdServer & "'.\n", wr);
            CloseAndExit(wr);
          ELSE
            IO.Put(
              "NetObject Error:" & ErrorSupport.ToText(info) & "\n", wr);
            CloseAndExit(wr);
          END;
        ELSE
          IO.Put("NetObject Error:" & ErrorSupport.ToText(info) & "\n", wr);
          CloseAndExit(wr);
        END;
    | Thread.Alerted => IO.Put("Alerted!\n", wr); CloseAndExit(wr);
    END;
    (* The boolean value NoOtherServer indicates that no other server is
       running any more. *)
    IF noOtherServer THEN
      (* When no other nameserver is registered at the netobject demon, the
         new server can be initialized and logged on to the netobject
         demo.*)

      NS.serverList := NEW(InternServerList, 0);
      NS.notifier := NEW(NetObjNotifier.NotifierClosure, notify := Notify);
      NS.listMutex := NEW(Thread.Mutex);
      NS.mainMutex := NEW(Thread.Mutex);
      NS.shutDown := NEW(Thread.Condition);

      TRY
        NetObj.Export(NameServer.GrasNameServerID, NS, netobjd);
      EXCEPT
        NetObj.Error (info) =>
          IO.Put("NetObject Error:" & ErrorSupport.ToText(info) & "\n", wr);
          CloseAndExit(wr);
      | Thread.Alerted => IO.Put("Alerted!\n", wr); CloseAndExit(wr);
      END;

      IF serverMode = NSMode.Verbose THEN
        VAR logfile, logdir: Pathname.T;
        BEGIN
          (* Open log file *)
          logdir := Env.Get("LOGDIR");
          IF logdir = NIL THEN logdir := "/var/tmp/" END;
          logfile := Pathname.Join(logdir, "g3ns", "log");
          TRY
            Log := FileWr.OpenAppend(logfile);
          EXCEPT
            OSError.E (e) =>
              IO.Put("Warning: cannot open logfile '" & logfile & "': "
                       & ErrorSupport.ToText(e) & "\n", wr);
              IO.Put("No messages will be logged.\n\n", wr);
              serverMode := NSMode.Quiet;
          END;
        END;
      END;
      TRY Wr.Close(wr); EXCEPT ELSE END;

      (* Start periodic pinger *)
      WITH pp = NEW(PeriodicPinger, server := NS) DO
        EVAL Thread.Fork(pp);
      END;

      RETURN NS
    ELSE
      (* When no new server could be started, we finish *)
      IO.Put("NameServer already running!\n", wr);
      CloseAndExit(wr);
      RETURN NIL;
    END
  END Init;

PROCEDURE ListAllServers (NS: Server): NameServer.AllServerList =
  VAR
    returnList  : NameServer.AllServerList;
    lengthOfList: CARDINAL;
  BEGIN
    LOCK NS.listMutex DO
      (* A new list is beeing generated, containing the id's of all known
         servers. *)
      lengthOfList := NUMBER(NS.serverList^);
      returnList := NEW(NameServer.AllServerList, lengthOfList);
      FOR i := 0 TO lengthOfList - 1 DO
        returnList[i].ID := NS.serverList[i].ID;
        returnList[i].OwnerID := NS.serverList[i].OwnerID;
      END;
    END;
    RETURN returnList
  END ListAllServers;

PROCEDURE CleanUp (NS: Server) =
  BEGIN
    ForkPingers(NS);
  END CleanUp;

PROCEDURE AddServer (NS     : Server;
                     ID     : TEXT;
                     Handle : EntryPort.T;
                     OwnerID: INTEGER      )
  RAISES {NameServer.InvalidServerIdentification,
          NameServer.ServerAlreadyInList, Thread.Alerted} =
  VAR
    newList: InternServerList;
    length : CARDINAL;
    index  : INTEGER;
  BEGIN
    (* This procedure is used to add a new gras-server to the list hold by
       the nameserver.  This happens when a new gras-server is initialized;
       it uses this method to log on to the nameserver.*)
    IF (Handle # NIL) AND (ID # NIL) AND (NOT Text.Equal(ID, "")) THEN
      LOCK NS.listMutex DO
        index := GetIndexToID(NS, ID);
        (* First it has to be checked if no other server with the same ID
           is already in the list.  If an entry for this server already
           exists, it has to be checked if he is still alive or shut down.
           This can be achieved by using the method "ping" of the
           gras-server.  Using this method of a shut down server will cause
           an NetObject error. *)
        IF index # -1 THEN
          IF IsServerIdle(NS, ID) THEN
            IF serverMode = NSMode.Verbose THEN
              IO.Put(
                "Removing idle server " & ID & " from list..... ", Log);
            END;
            TRY
              RemServerFromList(NS, ID);
            EXCEPT
              (* This exception cannot be raised, cause the list operations
                 are locked with the listmutex. *)
              NameServer.ServerNotInList =>
            END
          ELSE
            RAISE NameServer.ServerAlreadyInList;
          END;
        END;
        (* When we get to this point, no exception was raised, and the new
           server can be added to the list.  Therefore a new array is
           created which is one field longer than the old one, and the new
           server is added at the end. *)
        length := NUMBER(NS.serverList^) + 1;
        newList := NEW(InternServerList, length);
        SUBARRAY(newList^, 0, length - 1) := NS.serverList^;
        NS.serverList := newList;
        NS.serverList^[length - 1].ID := ID;
        NS.serverList^[length - 1].Handle := Handle;
        NS.serverList^[length - 1].OwnerID := OwnerID;
        (* A notification for the new server is created, so that the end of
           the server process will be recognized by the nameserver, and the
           entry for the dead server can be removed from the list..*)
        NetObjNotifier.AddNotifier(Handle, NS.notifier);
        IF serverMode = NSMode.Verbose THEN
          IO.Put("Added server \"" & ID & "\" to list." & "\n", Log)
        END;
      END
    ELSE
      RAISE NameServer.InvalidServerIdentification
    END
  END AddServer;

PROCEDURE RemoveServer (NS: Server; ID: TEXT)
  (* This method is the one which is visible from outside.  The mutex must
     be locked for the remove operations.  Internallty the other procedure
     is used because when the remove operation is used a lock on listmutex
     already exists. *)
  RAISES {NameServer.ServerNotInList,
          NameServer.InvalidServerIdentification} =
  BEGIN
    LOCK NS.listMutex DO RemServerFromList(NS, ID) END;
  END RemoveServer;

PROCEDURE GetServer (NS: Server; ID: TEXT): EntryPort.T
  RAISES {NameServer.ServerNotInList,
          NameServer.InvalidServerIdentification} =
  (* This method is used to get the handle of a server through his id *)
  VAR i: INTEGER;
  BEGIN
    LOCK NS.listMutex DO
      i := GetIndexToID(NS, ID);
      IF i = -1 THEN
        RAISE NameServer.ServerNotInList;
      ELSE
        RETURN NS.serverList[i].Handle
      END
    END;
  END GetServer;

PROCEDURE GetServerInfo (NS: Server; ID: TEXT): NameServer.ServerInfo
  RAISES {NameServer.InvalidServerIdentification} =
  VAR
    index : INTEGER;
    result: NameServer.ServerInfo;
  BEGIN
    LOCK NS.listMutex DO
      index := GetIndexToID(NS, ID);
      result.OwnerID := 0;
      IF index # -1 THEN
        result.OwnerID := NS.serverList^[index].OwnerID;
      END;
    END;
    RETURN result
  END GetServerInfo;

PROCEDURE ShutDown (NS: Server): BOOLEAN RAISES {Thread.Alerted} =
  (* The nameserver can only be shut down, when his list of gras-servers is
     empty, or all entries are idle.*)
  VAR s: CARDINAL := 0;
  BEGIN
    LOCK NS.listMutex DO
      IF NUMBER(NS.serverList^) = 0 THEN
        Thread.Signal(NS.shutDown);
        RETURN TRUE
      ELSE
        (* If there are entries in the list, it has to be figured out if
           they are still active *)
        LOOP
          TRY
            IF NOT IsServerIdle(NS, NS.serverList[s].ID) THEN
              RETURN FALSE;
            END;
          EXCEPT
            (* Internally this error cannot occur. *)
            NameServer.InvalidServerIdentification =>
          END;
          IF s < NUMBER(NS.serverList^) - 1 THEN
            INC(s)
          ELSE
            (* no active server in the list *)
            Thread.Signal(NS.shutDown);
            RETURN TRUE;
          END;
        END;
      END
    END
  END ShutDown;

PROCEDURE ServerInList (NS: Server; ID: TEXT): BOOLEAN
  RAISES {NameServer.InvalidServerIdentification} =
  BEGIN
    (* This function just checks whether the server with the id ID is in
       the list, or not. *)
    LOCK NS.listMutex DO
      IF GetIndexToID(NS, ID) = -1 THEN RETURN FALSE ELSE RETURN TRUE END
    END
  END ServerInList;


(* ------ Procedures for startup and interactive querying ------ *)

PROCEDURE ReadParameters () =

  BEGIN
    commands := OpSet{};
    (* Checks command line parameters.  The -v flag switches to verbose
       mode.  If it's missing, the quiet mode is used instead. *)
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-v") THEN
        serverMode := NSMode.Verbose
      ELSE
        serverMode := NSMode.Quiet
      END;

      (* check for command *)
      IF pp.keywordPresent("-h") OR pp.keywordPresent("help") THEN
        commands := commands + OpSet{Operation.Help};
      END;

      IF pp.keywordPresent("-l") OR pp.keywordPresent("list") THEN
        commands := commands + OpSet{Operation.List};
      END;

      IF pp.keywordPresent("-s") OR pp.keywordPresent("shutdown") THEN
        commands := commands + OpSet{Operation.Shutdown};
      END;

      IF pp.keywordPresent("-c") OR pp.keywordPresent("cleanup") THEN
        commands := commands + OpSet{Operation.CleanUp};
      END;

      IF commands = OpSet{} OR pp.keywordPresent("install") THEN
        commands := commands + OpSet{Operation.Install};
        IF pp.keywordPresent("-f") OR pp.keywordPresent("force") THEN
          commands := commands + OpSet{Operation.Shutdown};
        END;
      END;

      IF pp.keywordPresent("-debug") THEN Debug := TRUE END;


      IF pp.keywordPresent("-agent") THEN
        TRY
          NetObjdServer := pp.getNext();
        EXCEPT
          ParseParams.Error =>
            IO.Put(
              "Option '-agent' requieres an argument.\n", Stdio.stderr);
            commands := OpSet{Operation.Help};
        END;
      END;

      IF Operation.Help IN commands THEN
        DisplayHelpText(Pathname.Last(pp.arg[0]));
      END;
    END;
  END ReadParameters;


PROCEDURE DisplayHelpText (exName: TEXT) =
  BEGIN
    IO.Put("\nUSAGE: " & exName
             & " [-v] [-l | list] [-h | help] [[install] [-f | force]]\n"
             & "\t    [-s | shutdown] [-agent <hostname>]\n\n");
    IO.Put("Command line parameters have thw following meaning:\n");
    IO.Put("-v           :\tSwitch on verbose mode."
             & " This results in a more chattering behaviour.\n");
    IO.Put("-h, help     :\tDisplays this message." & "\n");
    IO.Put("-l, list     :\tDisplay a list of registered gras servers.\n");
    IO.Put("-s, shutdown :\tTry to shutdown an existing name server.\n");
    IO.Put("install      :\tThis is the default action of "
             & "installing a name server\n");
    IO.Put("\t\tA name server keeps a list of gras servers. "
             & "Applications\n");
    IO.Put("\t\tof GRAS can find their respective "
             & "server by querying the\n");
    IO.Put("\t\tthe name server for it.\n");
    IO.Put("-f,force     :\tOnly in combination with install "
             & "(or without list or help).\n");
    IO.Put("\t\tOnly one name server runs at a time."
             & " If -f (force)is present,\n");
    IO.Put("\t\ta possibly running old name server "
             & "is shutdown and removed\n");
    IO.Put("\t\tfrom Modula-3's netobjd. If -f "
             & "is not present, an already\n");
    IO.Put("\t\trunning name server is respected "
             & "and reported via stdout.\n");
    IO.Put("-agent <hostname>:\tUse <hostname> as location for netobjd. \n"
             & "\t\tDefault is 'kodaly'\n\n");
  END DisplayHelpText;

PROCEDURE DisplayServerList () =
  VAR
    ns  : NameServer.T;
    list: NameServer.AllServerList;
  BEGIN
    (* First we have got to check whether another nameserver is running.*)
    TRY
      ns := NetObj.Import(
              NameServer.GrasNameServerID, NetObj.Locate(NetObjdServer));

      (* A running server is indicated by a value # NIL *)
      IF ns # NIL THEN
        list := ns.listallservers();
        IF list # NIL AND NUMBER(list^) > 0 THEN
          IO.Put("Th following gras servers are registered:\n");
          IO.Put("User Id\tServer Id\n");
          FOR i := 0 TO NUMBER(list^) - 1 DO
            IO.Put(Fmt.Pad(Fmt.Int(list^[i].OwnerID), 7) & "\t"
                     & list^[i].ID & "\n");
          END;
        ELSE
          IO.Put("No gras servers registerd!\n");
        END;
        IO.Put("\n");
      ELSE
        IO.Put("No name server found!\n\n");
      END;
    EXCEPT
      NetObj.Error (info) =>
        IO.Put(
          "Communication Error: name server or netobjd might not be running!"
            & ErrorSupport.ToText(info) & "\n");
    | NetObj.Invalid => IO.Put("Invalid host name!\n");
    | Thread.Alerted => IO.Put("Alerted!\n");
    END;
  END DisplayServerList;

PROCEDURE SendCleanUp () =
  VAR ns: NameServer.T;
  BEGIN
    (* First we have got to check whether another nameserver is running.*)
    TRY
      ns := NetObj.Import(
              NameServer.GrasNameServerID, NetObj.Locate(NetObjdServer));

      (* A running server is indicated by a value # NIL *)
      IF ns # NIL THEN
        ns.cleanup();
      ELSE
        IO.Put("No name server found!\n\n");
      END;
    EXCEPT
      NetObj.Error (info) =>
        IO.Put(
          "Communication Error: name server or netobjd might not be running!"
            & ErrorSupport.ToText(info) & "\n");
    | NetObj.Invalid => IO.Put("Invalid host name!\n");
    | Thread.Alerted => IO.Put("Alerted!\n");
    END;
  END SendCleanUp;

PROCEDURE TryShutdown () =
  VAR
    existentServer: NameServer.T;
    netobjd       : NetObj.Address;
  BEGIN
    TRY
      netobjd := NetObj.Locate(NetObjdServer);
      (* Check if another nameserver is running.*)
      existentServer :=
        NetObj.Import(NameServer.GrasNameServerID, netobjd);
      (* Another running server is indicated by a value # NIL *)
      IF existentServer # NIL THEN
        (* Try to shut down the other server. *)
        TRY
          IO.Put("Trying to shut down ...");
          IF existentServer.shutdown() THEN
            IO.Put("done." & "\n");
          ELSE
            IO.Put("failed!" & "\n");
          END;
        EXCEPT
          NetObj.Error =>
            (* NameServer still registered but not running.  Unregister
               it. *)
            IO.Put("unreachable!\n");
            NetObj.Export(NameServer.GrasNameServerID, NIL, netobjd);
            IO.Put("Successfully unregistered!\n");
        END;
      ELSE
        IO.Put("No name server found.\n");
      END;
    EXCEPT
    | NetObj.Error (al) =>
        IO.Put("NetObject Error:" & ErrorSupport.ToText(al) & "\n");
    | NetObj.Invalid => IO.Put("NetObject invalid!\n");
    | Thread.Alerted => IO.Put("Thread Alerted!\n");
    END;
  END TryShutdown;

VAR
  nameServer   : Server;
  serverMode   : NSMode;
  commands     : OpSet;
  NetObjdServer: TEXT    := Config.DefaultNameServer;
  Debug        : BOOLEAN := FALSE;
  TCPUnexpected: Atom.T;

BEGIN
  (* CM3 and SRC disagree about where to define TCP.Unexpected.
     So we simply redefine it here. *)
  TCPUnexpected := Atom.FromText("TCP.Unexpected");
  
  NameServer.EntryExists := Atom.FromText("Entry exists!\n");

  ReadParameters();

  IF Operation.CleanUp IN commands THEN SendCleanUp(); END;
  IF Operation.List IN commands THEN DisplayServerList(); END;

  IF Operation.Install IN commands THEN
    TRY
      nameServer := NEW(Server).init(Operation.Shutdown IN commands);

      LOCK nameServer.mainMutex DO
        Thread.AlertWait(nameServer.mainMutex, nameServer.shutDown);
      END;
      TRY
        NetObj.Export(
          NameServer.GrasNameServerID, NIL, NetObj.Locate(NetObjdServer));
      EXCEPT
        NetObj.Error =>          (* ignore *)
      | NetObj.Invalid =>        (* ignore *)
      END;

    EXCEPT
    | Thread.Alerted => IO.Put("Thread Alerted!\n");
    END
  ELSIF Operation.Shutdown IN commands THEN
    TryShutdown();
  END;
END Main.
