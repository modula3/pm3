MODULE Daemon;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.1  1997/08/14 08:07:40  roland
    A procedure to fork a daemon process. The child can communicate with
    its parent via reader and writer.

*)
(***************************************************************************)

IMPORT FilePosix, FileRd, FileWr, OSError, OSErrorPosix, Pipe, Rd, Wr,
       Utypes, Uresource, Unix, Process;
IMPORT Setsid;


PROCEDURE ForkDaemon (VAR rd: Rd.T; VAR wr: Wr.T): BOOLEAN
  RAISES {OSError.E} =
  VAR
    cpid                                : Utypes.pid_t;
    rlp                                 : Uresource.struct_rlimit;
    hwChild, hwParent, hrParent, hrChild: Pipe.T;
  BEGIN
    Pipe.Open(hr := hrChild, hw := hwParent);
    Pipe.Open(hr := hrParent, hw := hwChild);

    CASE Unix.fork() OF
      -1 => OSErrorPosix.Raise();
    | 0 =>                       (* this is the child *)
    ELSE
      (* parent is done *)
      wr := NEW(FileWr.T).init(hwParent);
      rd := NEW(FileRd.T).init(hrParent);
      TRY
        hwChild.close();
        hrChild.close();
      EXCEPT
        OSError.E => (* ignore *)
      END;
      RETURN FALSE;
    END;
    (* Only forked child gets to this point *)
    (* make child a daemon *)
    (* 1.  become session leader *)
    cpid := Setsid.setsid();     (* session leader has no controlling
                                    terminal *)
    (* 2.  cd '/' to avoid unmount conflicts at reboot *)
    Process.SetWorkingDirectory("/");
    (* 3.  set umask to 0 to prevent restrictive permission at file
       creation *)
    EVAL Unix.umask(0);
    (* 4.  close all inherited files *)
    EVAL Uresource.getrlimit(Uresource.RLIMIT_NOFILE, rlp);
    FOR fd := 0 TO rlp.rlim_cur DO
      (* close all file descriptors except for hwChild and hrChild *)
      IF fd # hrChild.fd AND fd # hwChild.fd THEN EVAL Unix.close(fd); END;
    END;

    (* Now the child should be a real daemon. *)
    wr := NEW(FileWr.T).init(hwChild);
    rd := NEW(FileRd.T).init(hrChild);
    TRY
      hwParent.close();
      hrParent.close();
    EXCEPT
      OSError.E => (* ignore *)
    END;
    RETURN TRUE;
  END ForkDaemon;


BEGIN
END Daemon.
