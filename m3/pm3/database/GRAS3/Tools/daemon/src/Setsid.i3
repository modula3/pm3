INTERFACE Setsid;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.1  1997/08/14 08:07:41  roland
    A procedure to fork a daemon process. The child can communicate with
    its parent via reader and writer.

*)
(***************************************************************************)

IMPORT Utypes;

<* EXTERNAL *>
PROCEDURE setsid(): Utypes.pid_t;

END Setsid.
