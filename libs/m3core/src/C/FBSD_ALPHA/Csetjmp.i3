(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Oct 18 08:40:23 PDT 1994 by kalsow     *)
(*      modified on Fri Apr 30 16:25:40 PDT 1993 by muller     *)
(*      Olaf Wagner 12.09.1994                                 *)

INTERFACE Csetjmp;		(* for FreeBSD *)

FROM Ctypes IMPORT int, long;

TYPE 
  jmp_buf = ARRAY [0..81] OF long;

<*EXTERNAL "setjmp"   *> PROCEDURE setjmp (VAR env: jmp_buf): int;
<*EXTERNAL "longjmp"  *> PROCEDURE longjmp (VAR env: jmp_buf; val: int);

<*EXTERNAL "_setjmp" *>  PROCEDURE usetjmp (VAR env: jmp_buf): int;
<*EXTERNAL "_longjmp" *> PROCEDURE ulongjmp (VAR env: jmp_buf; val: int);

END Csetjmp.

