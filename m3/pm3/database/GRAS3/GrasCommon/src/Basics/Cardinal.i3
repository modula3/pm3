INTERFACE Cardinal;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:26  hosking
    Initial revision

    Revision 1.6  1998/01/21 14:14:41  roland
    Comments fixed.

    Revision 1.5  1997/03/21 16:42:45  roland
    New generic module Relation realizes binary relations between objects
    of two (distinct) abstract data types.

    Revision 1.4  1996/09/17 12:50:29  roland
    Basics now contains everything CardinalCollections did. Comfortable
    use of generics with the procedures of basics.tmpl (CursorList,
    CursorSet, Stack, NTree).

    Corrections in NTree.

    Revision 1.2  1996/02/09 16:09:38  rbnix
        Constant Brand added this is neccessary for some generic
        modules.

    Revision 1.1  1996/01/12 13:39:11  grover
    added CardinalCollections. The generics from Basics are used

# Revision 1.2  1995/06/08  15:42:18  grover
# changes made for new implementations in RecoverableGraph
#
# Revision 1.1  1994/03/30  17:18:47  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Word, Type;

CONST
  Brand = "Cardinal";
  Null  = 0;

TYPE T = CARDINAL;


PROCEDURE Equal (a, b: T): BOOLEAN;
  (* Return "a = b". *)

PROCEDURE Hash (a: T): Word.T;
  (* Return "a". *)

PROCEDURE Compare (a, b: T): [-1 .. 1];
  (* Return "-1" if "a < b", "0" if "a = b", or "+1" if "b > a".  '<' is
     not the canonical order relation on cardinals, but a bit-wise
     lexikographic comparison starting from bit 0. *)

PROCEDURE Eq (a, b: T): BOOLEAN;
  (* Return "a = b". *)

PROCEDURE Lt (a, b: T): BOOLEAN;
  (* Return "a < b". *)

PROCEDURE FromByteArray (READONLY ba: ARRAY OF Type.Byte; VAR x: CARDINAL);
PROCEDURE ToByteArray (x: CARDINAL; VAR ba: ARRAY OF Type.Byte);
  (* Convert cardinals to and from byte arrays.  It is a checked runtime
     error if NUMBER(ba)<BYTESIZE(CARDINAL) *)

END Cardinal.
