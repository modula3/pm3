(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Wed Mar  4 11:53:52 PST 1992 by muller    *)
(* ow 30.09.1994 *)

INTERFACE Uuio;

FROM Ctypes IMPORT int, char_star, void_star;
FROM Utypes IMPORT ssize_t, size_t, off_t;

(*** sys/uio.h ***)


TYPE
  struct_iovec = RECORD
    iov_base: void_star;
    iov_len: size_t;
  END;
  struct_iovec_star = UNTRACED REF struct_iovec;

  (* There's no corresponding structure to struct_uio - be careful *)
  struct_uio = RECORD
    uio_iov: struct_iovec_star;
    uio_iovcnt: int;
    uio_offset: off_t;
    uio_resid: size_t;
    uio_segflg: uio_seg;
    uio_rw: uio_rw;
    uio_procp: void_star;	(* struct proc * *)
  END;

  uio_rw = {UIO_READ, UIO_WRITE};
  uio_seg = {UIO_USERSPACE, UIO_SYSSPACE};

(*
 * Segment flag values (should be enum).
 *)

CONST
  IOV_MAX =		1024;	(* max # of iovec's for readv(2) etc *)
				(* from sys/syslimits.h (from limits.h) *)
  UIO_MAXIOV =		IOV_MAX; (* Deprecated, use IOV_MAX *)
  MAX_IOVEC = 		IOV_MAX; (* Deprecated, use IOV_MAX *)



(*** read, readv(2) - read from a file ***)

<*EXTERNAL*> PROCEDURE read (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE readv (d: int; iov: struct_iovec_star;
                              iovcnt: int): int;
<*EXTERNAL*> PROCEDURE preadv (d: int; buf: struct_iovec_star;
                              iovcnt: int; offset: off_t): ssize_t;

(*** write, writev(2) - write on a file ***)

<*EXTERNAL*> PROCEDURE write (d: int; buf: char_star; nbytes: int): int;
<*EXTERNAL*> PROCEDURE writev (d: int; iov: struct_iovec_star;
                               ioveclen: int): int;

<*EXTERNAL*> PROCEDURE pwritev (d: int; buf: struct_iovec_star;
                               iovcnt: int; offset: off_t): ssize_t;

END Uuio.
