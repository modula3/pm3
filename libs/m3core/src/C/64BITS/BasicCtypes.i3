(* Copyright (C) 1989, 1990, Digital Equipment Corporation     *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu May  6 12:08:19 PDT 1993 by muller     *)
(*      modified on Wed Feb  7 15:49:32 1990 by jerome         *)

INTERFACE BasicCtypes;

IMPORT Word;

TYPE
  (* the four signed integer types *)
  signed_char        = [-16_80 .. 16_7f];
  short_int          = [-16_8000 .. 16_7fff];
  int                = [-16_80000000 .. 16_7fffffff];
  long_int           = [-16_7fffffffffffffff - 1 .. 16_7fffffffffffffff];

  (* the four unsigned integer types *)
  unsigned_char      = [16_0 .. 16_ff];
  unsigned_short_int = [16_0 .. 16_ffff];
  unsigned_int       = [16_0 .. 16_ffffffff];
  unsigned_long_int  = Word.T;

  (* the three floating types *)
  float              = REAL;
  double             = LONGREAL;
  long_double        = EXTENDED;

  (* char *)
  char               = signed_char;

END BasicCtypes.


