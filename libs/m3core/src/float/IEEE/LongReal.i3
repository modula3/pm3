(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Aug 16 15:09:23 PDT 1996 by heydon     *)
(*      modified on Wed May  5 08:13:33 PDT 1993 by mcjones    *)
(*      modified on Thu Apr 29 16:10:49 PDT 1993 by muller     *)
(*      modified on Fri Feb 28 20:35:22 PST 1992 by stolfi     *)
(*      modified on Wed Sep 25 00:03:30 1991 by kalsow         *)

INTERFACE LongReal; 

(* Properties of LONGREAL (for ANSI/IEEE Standard 754-1985).

   This package defines some basic properties of the 
   built-in float type LONGREAL. If you want to instantiate
   a generic type with double-precision real numbers, you
   should import the "LongRealType" interface instead.

   Index: LONGREAL; floating-point; generics
*)

TYPE T = LONGREAL; 

CONST 
  Base: INTEGER = 2; 
  (* The radix of the floating-point representation for T *)

  Precision: INTEGER = 53;
  (* The number of digits of precision in the given Base for T. *)

  MaxFinite: T = 1.7976931348623157D+308;
  (* The maximum finite value in T.  For non-IEEE implementations,
     this is the same as LAST(T). *)

  MinPos: T = 4.9406564584124654D-324;
  (* The minimum positive value in T. *)

  MinPosNormal: T = 2.2250738585072014D-308;
  (* The minimum positive "normal" value in T; differs from MinPos
     only for implementations with denormalized numbers. *)

CONST
  MaxExpDigits = 3;
  MaxSignifDigits = 17;
(* "MaxExpDigits" is the smallest integer with the property that every
   finite number of type "T" can be written in base-10 scientific
   notation using an exponent with at most "MaxExpDigits".
   "MaxSignifDigits" is the smallest integer with the property that
   floating-decimal numbers with "MaxSignifDigits" are more closely
   spaced, all along the number line, than are numbers of type "T".
   Typically, *)
(*
| MaxExpDigits    = ceiling(log_10(log_10(MaxFinite)))
| MaxSignifDigits = ceiling(log_10(Base^Precision)) + 1.
*)
END LongReal.
