(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Oct  4 11:53:09 PDT 1994 by ericv         *)
(*      modified on Fri Mar 16 12:16:52 1990 by muller        *)

UNSAFE MODULE Usignal;

IMPORT Word, Ctypes;

PROCEDURE sigmask (n: Ctypes.int): Ctypes.int =
BEGIN
  RETURN Word.Shift (1, n-1);
END sigmask;

BEGIN
  SIG_ERR := LOOPHOLE (-1, SignalHandler);
  SIG_DFL := LOOPHOLE ( 0, SignalHandler);
  SIG_IGN := LOOPHOLE ( 1, SignalHandler);
  SIG_HOLD := LOOPHOLE ( 2, SignalHandler);
END Usignal.
