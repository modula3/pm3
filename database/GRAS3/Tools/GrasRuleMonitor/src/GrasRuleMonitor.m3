MODULE GrasRuleMonitor EXPORTS Main;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.4  1997/12/15 16:35:17  roland
    New man page and unified command line switches.

    Revision 1.3  1997/11/12 17:24:17  roland
    Start/Stop fixed. The listener really gets killed when stopping the
    monitor.

    Revision 1.2  1997/11/07 08:58:05  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

    Revision 1.1  1997/10/31 14:28:27  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

IMPORT RuleMonitor, GrasParams, TypedGraphSystem, RuleEngine;
IMPORT Pathname, IO, Process, Thread;
IMPORT Panel, Patterns;
(* Next import needed to install all event types *)
IMPORT TypedGraph;               <* NOWARN *>

VAR
  rootPath            : Pathname.T;
  valid               : BOOLEAN;
  cachesize           : CARDINAL;
  serverid, nameserver: TEXT;
  lock                             := NEW(MUTEX);

BEGIN
  GrasParams.ParseComandLine(
    rootPath, valid, cachesize, serverid, nameserver, quiet := TRUE);
  IF NOT valid THEN
    IO.Put("g3monitor: root path not specified, supplying default '/tmp'.\n");
    rootPath := "/tmp";
  END;
  TypedGraphSystem.Login(rootPath, cachesize, serverid, nameserver);
  (* check connection *)
  VAR
    conn: BOOLEAN;
    msg : TEXT;
  BEGIN
    RuleEngine.CheckRuleServer(conn, msg);
    IF NOT conn THEN
      IO.Put("Not connected to RuleServer:\n" & msg & "\n");
      Process.Exit(1);
    END;
  END;
  Patterns.Init();
  IF NOT Panel.Install() THEN
    RuleMonitor.InstallMonitor(interest := RuleEngine.Interest.Others);
    Process.RegisterExitor(RuleMonitor.UninstallMonitor);
    TRY
      LOOP
        RuleEngine.WaitForRemoteActions(lock);
        RuleEngine.ExecuteRemoteActions();
      END;
    EXCEPT
      Thread.Alerted => RuleMonitor.UninstallMonitor();
    END;
  END;
END GrasRuleMonitor.
