INTERFACE Daemon;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.1  1997/08/14 08:07:39  roland
    A procedure to fork a daemon process. The child can communicate with
    its parent via reader and writer.

*)
(***************************************************************************)

IMPORT Rd, Wr, OSError;

PROCEDURE ForkDaemon (VAR rd: Rd.T; VAR wr: Wr.T): BOOLEAN
  RAISES {OSError.E};
  (* Executes a fork().  The created child becomes session leader and
     closes all file descritpors (except for the two belonging to wr and
     rd, see below), making itself a daemon process.  Before fork, two
     Pipes are created that can be used for communication between child and
     parent.  The respective endpoints for child and parent are returned in
     rd and wr.  The BOOLEAN return value indicates whether the child
     (TRUE) or the parent returns (FALSE).

     NOTES: The child closes all file descriptors, i.e.  it closes all
     files with Unix.close.  This confuses the M3 runtime if e.g.  NetObj
     was used before.  Therefore, a process should use fancy features of M3
     only after a ForkDaemon. *)


END Daemon.
