INTERFACE RuleMonitor;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:26:15  roland
    Basic monitoring for RuleEngine.

*)
(***************************************************************************)

IMPORT IntSeq, Wr;
FROM RuleEngine IMPORT Interest;
FROM Trigger IMPORT CouplingMode;
FROM Action IMPORT Procedure;

(* Monitor events local and/or remote.  The default action procedure which
   prints all events to wr (stdout if wr is NIL) can be overriden by
   submitting an own procedure.  If types is non nil it must list a
   sequence of existing event types.  These events types will be monitored.
   If types=NIL, all event types will be monitored. *)

PROCEDURE InstallMonitor (wr      : Wr.T         := NIL;
                          types   : IntSeq.T     := NIL;
                          interest: Interest     := Interest.All;
                          actproc : Procedure    := NIL;
                          coupling: CouplingMode := CouplingMode.Immediate;
                          priority: CARDINAL     := LAST(CARDINAL));

PROCEDURE UninstallMonitor ();

END RuleMonitor.
