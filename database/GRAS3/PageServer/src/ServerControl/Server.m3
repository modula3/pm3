MODULE Server EXPORTS Main;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.24  1998/02/17 16:24:16  roland
    Introduced explicit garbage collection on regular intervalls.

    Revision 1.23  1997/12/15 16:30:27  roland
    Updated man page and unified command line switches.

    Revision 1.22  1997/10/31 14:17:21  roland
    Startup and shutdown code for RuleServer added.

    Revision 1.21  1997/06/13 11:48:58  rbnix
    	Adapted to unified root prefix (Config.GetRootPrefix).

    Revision 1.20  1997/03/26 16:12:13  renehuel
    A new pageserver now logs in to the nameserver with his id, handle
    and new : the id of the user who started it.

    Revision 1.19  1997/03/25 13:45:43  renehuel
    Startup message of page server now displays the id of the server.

    Revision 1.18  1997/03/21 16:57:31  roland
    Command line parameter id is optional now. Default is read from Config.

    Revision 1.17  1997/03/20 16:55:28  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.16  1997/02/04 08:45:43  roland
    New command line parameter -pingclients determines the number of
    seconds between client life check.
    Removed prefix g3 from all options.

    Revision 1.15  1997/01/20 08:55:12  roland
    Minor fixes.

    Revision 1.14  1996/11/21 15:23:19  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.13  1996/11/20 12:11:15  roland
    Improved exception handling and startup.

    Revision 1.12  1996/11/11 09:37:19  rbnix
        Error messages for Netobj.Error evaluated.

    Revision 1.11  1996/11/08 14:45:55  roland
    GetResources in ServerScheduler handles PageFile.NoAccess correct now.

    Revision 1.10  1996/10/30 12:47:25  roland
    EntryServer does not try to unregister from netobjd when program
    receives a termination signal.

    Revision 1.9  1996/10/29 14:44:34  rbnix
        Periodical call of SignalAccess added.

        Procedure RegisteredShutdown added.

    Revision 1.8  1996/10/08 13:09:12  roland
    Only one entry is allowed in a network. Error handling improved.
    Changes to EntryServer undone.

    Revision 1.7  1996/08/06 16:32:26  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.2  1996/06/13 12:45:16  rbnix
        Error handling improved using new module ErrorSupport.

    Revision 1.6.2.1  1996/04/29 14:23:02  roland
    ExceptionHandling and error messages improved.

    Revision 1.6  1996/03/08 11:12:21  rbnix
        Default stack size of threads set to hold at least 3
        pages. This should be safe to avoid some irregular crashes.

    Revision 1.5  1996/03/06 08:48:41  rbnix
        Periodical check on killed clients in separate thread added.

    Revision 1.4  1996/03/01 13:38:32  rbnix
        New method EntryServer.T.exit to remove port.

    Revision 1.3  1996/02/28 14:19:01  rbnix
        Journal messages for regular server execution added.

    Revision 1.2  1996/02/28 11:04:45  rbnix
        Usage of new configuration storage introduced.

    Revision 1.1  1996/02/26 17:58:24  rbnix
        First version of subsystem ServerControl.

*)
(***************************************************************************)
(*
 | --- Server -------------------------------------------------------------
 This module implements the main program of the server. After some
 initialization it waits until a shutdown signal arrives.

 Clients are checked periodically if there are marked killed or unable to
 response in this case they will be killed finally and removed from the
 client table.
 | ------------------------------------------------------------------------
 *)
IMPORT Fmt, Thread, NetObj, Process, ParseParams, Pathname, Stdio, Uugid;
IMPORT Config, Variant, Journal, PageData, ErrorSupport, PageCache, Termination;
IMPORT ServedClient, ServedClientTable, ServedClientTbl, EntryServer,
       ServerScheduler, NameServer, RuleServer;
IMPORT RTCollector;

(* The Server calls Config.Login.  Config.Unspecified will never be
   raised. *)
<* FATAL Config.Unspecified *>

PROCEDURE ParseCommandline () =
  CONST
    RootKey       = "-root";
    NameServerKey = "-agent";
    CacheSizeKey  = "-cachesize";
    PingKey       = "-pingclients";
    IDKey         = "-id";

    RootSyn       = RootKey & " <dir>";
    NameServerSyn = NameServerKey & " <hostname>";
    CacheSizeSyn  = CacheSizeKey & " <pages>";
    PingSyn       = PingKey & " <secs>";
    IDSyn         = IDKey & " <serverid>";
    
    USAGE = " " & RootSyn & " [" & IDSyn & "] [" & NameServerSyn & "]" & "\n\t ["
              & CacheSizeSyn & "]" & " [" & PingSyn & "]";
  PROCEDURE ParseError (prog, err: TEXT) =
    BEGIN
      Journal.Add(err & "\n");
      Journal.Add(prog & USAGE & "\n");
      Process.Exit(1);
    END ParseError;

  VAR
    rootPath, nameServer: Pathname.T;
    cacheSize           : CARDINAL;

  BEGIN
    rootPath := NIL;
    serverID := Config.DefaultServerId();
    nameServer := Config.DefaultNameServer;
    cacheSize := Config.DefaultCacheSize;
    pingtime := 30.0d0;
    
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        (* root keyword is mandatory *)
        pp.getKeyword(RootKey);
        rootPath := pp.getNext();
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Parameter " & RootSyn & " must be present.");
      END;

      IF NOT Pathname.Valid(rootPath) THEN
        ParseError(Pathname.Last(pp.arg[0]),
                   "Rootpath must be a valid directory name.");
      END;

      TRY
        (* id is optional *)
        IF pp.keywordPresent(IDKey) THEN
          serverID := pp.getNext();
        END;
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Parameter " & IDSyn & " requires an argument.");
      END;

      TRY
        (* For compatibility: also check -nameserver *)
        IF pp.keywordPresent(NameServerKey) OR
          pp.keywordPresent("-nameserver") THEN
          nameServer := pp.getNext();
        END;
      EXCEPT
        ParseParams.Error =>
          ParseError(
            Pathname.Last(pp.arg[0]),
            "Parameter " & NameServerSyn & " requieres an argument.");
      END;


      TRY
        IF pp.keywordPresent(CacheSizeKey) THEN
          cacheSize := pp.getNextInt();
        END;
      EXCEPT
        ParseParams.Error =>
          ParseError(
            Pathname.Last(pp.arg[0]),
            "Parameter " & CacheSizeSyn & " requieres a numeric argument.");
      END;

      TRY
        IF pp.keywordPresent(PingKey) THEN
          pingtime := FLOAT(pp.getNextInt(), LONGREAL);
        END;
      EXCEPT
        ParseParams.Error =>
          ParseError(
            Pathname.Last(pp.arg[0]),
            "Parameter " & PingSyn & " requieres a numeric argument.");
      END;

    END;

    Config.Login(rootPath, cacheSize, nameServer, serverID);
  END ParseCommandline;

(*
 | --- check closure ------------------------------------------------------
 *)
VAR pingtime: LONGREAL := 30.0d0;

PROCEDURE CheckClients (<* UNUSED *> self: Thread.Closure): REFANY =
  VAR
    client       : ServedClient.T;
    clientID     : TEXT;
    killedClients: ServedClientTbl.Default;
    i1                           := NEW(ServedClientTable.Iterator);
    i2: ServedClientTbl.Iterator;
    collect: BOOLEAN;
    loopCount: CARDINAL;

  BEGIN
    loopCount := 0;
    LOOP
      PageCache.BeginAccess();
      EVAL i1.init();
      killedClients := NEW(ServedClientTbl.Default).init();

      (*
        collect obsolete clients
        (they can't be removed while using the iterator)
      *)
      WHILE i1.next(client) DO
        IF client.isKilled() THEN
          EVAL killedClients.put(client.getID(), client);

        ELSE
          TRY
            client.getCallbackPort().ping();
          EXCEPT
          | Thread.Alerted =>
              client.kill(
                "Network connection failed on check. " & "Thread alerted.");
              EVAL killedClients.put(client.getID(), client);

          | NetObj.Error (code) =>
              client.kill("Network connection failed on check. "
                            & ErrorSupport.Fmt(code));
              EVAL killedClients.put(client.getID(), client);
          END
        END;
      END;

      (* finally kill and remove collected clients *)
      i2 := killedClients.iterate();
      WHILE i2.next(clientID, client) DO
        IF Variant.RegularServerJournal THEN
          Journal.Add(
            "Client " & clientID & " removed from all services."
              & " [" & client.whyKilled() & "]");
        END;

        ServerScheduler.KillClient(client);
        ServedClientTable.Delete(client);
        collect := TRUE;
      END;
      i2 := NIL;

      INC(loopCount);
      IF loopCount MOD 10 = 0 THEN
        loopCount := 0;
        collect := TRUE;
      END;
      
      IF collect THEN
        RTCollector.Collect();
        collect := FALSE;
      END;
      
      (* force continuation, maybe some messages are crossed and aborted *)
      ServerScheduler.SignalAccess();

      PageCache.EndAccess();

      (* wait for a while and check again *)
      Thread.Pause(pingtime);
    END;
  END CheckClients;


(*
 | --- server main part ---------------------------------------------------
 *)
CONST
  minStackPages = 3;
  minStackSize  = BYTESIZE(PageData.T) * minStackPages;

VAR entryServer: EntryServer.T;

PROCEDURE RegisteredShutdown () =
  <* FATAL Termination.StillInUse, NetObj.Error, Thread.Alerted *>
  BEGIN
    IF entryServer # NIL THEN
      entryServer.shutdown(Termination.Mode.Strikt);
      (* entryServer.exit (); *)
      entryServer := NIL;
      RuleServer.Shutdown();
      
      Journal.Add("\nGras server terminated (forced).");
    END;
  END RegisteredShutdown;

VAR serverID: TEXT;
    
(* Server *)
BEGIN
  ParseCommandline();

  IF Variant.RegularServerJournal THEN
    Journal.Add("Starting up gras server...");
  END;

  (* register shutdown procedure *)
  Process.RegisterExitor(RegisteredShutdown);

  (* set stack size for threads *)
  IF Variant.RegularServerJournal THEN
    Journal.Add(
      "Setting default stack size at least " & Fmt.Int(minStackSize)
	& " bytes (" & Fmt.Int(minStackPages) & " pages).");
  END;
  Thread.MinDefaultStackSize(minStackSize);

  (* initializing cache *)
  IF Variant.RegularServerJournal THEN
    Journal.Add("Initializing cache with " & Fmt.Int(Config.GetCacheSize())
		  & " pages.");
  END;
  PageCache.BeginAccess();
  PageCache.Init(Config.GetCacheSize());
  PageCache.EndAccess();


  (* info about data root *)
  IF Variant.RegularServerJournal THEN
    Journal.Add ("Root path for persistent data is '" &
		 Config.GetRootPrefix (temporary := FALSE) & "'");
    Journal.Add ("Root path for temporary data is '" &
		 Config.GetRootPrefix (temporary := TRUE) & "'");
  END;


  (* offering communication *)
  IF Variant.RegularServerJournal THEN
    Journal.Add("Offering communication via name server on '"
		  & Config.GetNameServer() & "' ID is '" & serverID & "'");
  END;
  TRY
    entryServer := NEW(EntryServer.T).init(serverID,Uugid.getuid());
  EXCEPT
    NetObj.Error (code) =>
      Journal.Add("*** Error: Unable to install communication entry. "
		    & ErrorSupport.Fmt(code));
      Process.Exit(1);
  | NameServer.NoNameServer =>
      Journal.Add("No name server running!");
      Process.Exit(1);
  | NameServer.ServerAlreadyInList =>
      Journal.Add("A server with this identification is already running!");
      Process.Exit(1);
  ELSE
    Journal.Add("*** Error: cannot connect to name server (netobjd)!");
    Process.Exit(1)
  END;


  (* starting check tread *)
  IF Variant.RegularServerJournal THEN
    Journal.Add("Starting thread to check clients");
  END;
  EVAL Thread.Fork(NEW(Thread.Closure, apply := CheckClients));

  (* Start rule server *)
  TRY
    RuleServer.Setup(Config.GetNameServer(), Config.GetGrasServerId());
    IF Variant.RegularServerJournal THEN
      Journal.Add("RuleServer successfully installed.");
    END;
  EXCEPT
  | RuleServer.Failure (msg) =>
      IF Variant.RegularServerJournal THEN
	Journal.Add("Ruleserver: " & msg);
      END;
  END;


  (* waiting for shutdown *)
  IF Variant.RegularServerJournal THEN
    Journal.Add("Initialization of gras server successful.");
    Journal.Add("Waiting for requests until shutdown...");
  END;
  ServerScheduler.WaitShutdown();
  TRY
    entryServer.exit(serverID);
    entryServer := NIL;
    RuleServer.Shutdown();
  EXCEPT
  ELSE
    Journal.Add("*** Error: during Shutdown!");
    Process.Exit(1)
  END;

  (* regular termination *)
  IF Variant.RegularServerJournal THEN
    Journal.Add("Gras server terminated normally.");
  END;
END Server.
