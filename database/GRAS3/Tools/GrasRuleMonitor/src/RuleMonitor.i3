INTERFACE RuleMonitor;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/11/07 08:58:14  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

*)
(***************************************************************************)

IMPORT Wr;
FROM RuleEngine IMPORT Interest;
FROM Trigger IMPORT CouplingMode;
FROM Action IMPORT Procedure;

(* Monitor events local and/or remote.  The default action procedure which
   prints all events to wr (stdout if wr is NIL) can be overriden by
   submitting an own procedure. InstallMonitor reads the event types which
   should be monitored from MonitoredTypes and the patterns for these types
   from Patterns. *)

PROCEDURE InstallMonitor (wr      : Wr.T         := NIL;
                          interest: Interest     := Interest.All;
                          actproc : Procedure    := NIL;
                          coupling: CouplingMode := CouplingMode.Immediate;
                          priority: CARDINAL     := LAST(CARDINAL));

PROCEDURE UninstallMonitor ();

END RuleMonitor.
