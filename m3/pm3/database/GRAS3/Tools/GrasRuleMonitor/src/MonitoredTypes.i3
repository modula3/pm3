INTERFACE MonitoredTypes;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:28:29  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

IMPORT IntSeq;

PROCEDURE Insert(type: CARDINAL);
PROCEDURE Remove(type: CARDINAL);
PROCEDURE IsMonitored(type: CARDINAL): BOOLEAN;
PROCEDURE Number(): CARDINAL;
PROCEDURE Get(): IntSeq.T;

END MonitoredTypes.
