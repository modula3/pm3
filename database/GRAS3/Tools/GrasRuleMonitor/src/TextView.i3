INTERFACE TextView;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:28:31  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

IMPORT Wr;

PROCEDURE Open(notifyClose: PROCEDURE()): Wr.T;

END TextView.
