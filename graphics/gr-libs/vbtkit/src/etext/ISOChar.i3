(* Copyright � 1993, Digital Equipment Corporation            *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* by Jim Meehan and Henri Gouraud                             *)
(* Last modified on Sat May 29 17:44:21 PDT 1993 by meehan     *)

INTERFACE ISOChar;

(* ISO Latin-1 Characters and Character Sets

   ISOChar deals with individual characters, including the ISO Latin-1
   8-bit characters.  It classifies characters into groups, like
   digits or punctuation; each group is represented as a set of
   characters.  Finally, it provides mapping tables that translate
   lower-case letters into upper-case and the like.

*)

TYPE T = CHAR;

CONST
  All           = SET OF T {'\000'.. '\377'};
  Controls      = SET OF T {'\000'.. '\037', '\177'.. '\217', '\231', '\234'};
  Spaces        = SET OF T {' ', '\t', '\n', '\r', '\f', '\240'};
  Digits        = SET OF T {'0'.. '9'};
  Uppers        = SET OF T {'A'.. 'Z', '\300'.. '\326', '\330'.. '\336'};
  Lowers        = SET OF T {'a'.. 'z', '\337'.. '\366', '\367'.. '\377'};
  Letters       = Uppers + Lowers;
  AlphaNumerics = Letters + Digits;
  Graphics      = Asciis - Controls;
  Punctuation   = Graphics - AlphaNumerics;
  Asciis        = All;

VAR
  Upper   : ARRAY T OF T;
  Lower   : ARRAY T OF T;
  Control : ARRAY T OF T;
  (* These constant arrays implement character conversions (mappings):

         Upper[c]   = the upper-case equivalent of c if c is a letter, else c
         Lower[c]   = the lower-case equivalent of c if c is a letter, else c
         Control[c] = the control-shifted equivalent of c if c is in Graphics
                       (i.e. BitAnd (c, 037B)), else c
    *)

END ISOChar. 
