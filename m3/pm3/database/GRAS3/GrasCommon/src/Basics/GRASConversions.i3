INTERFACE GRASConversions;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/09/17 12:50:32  roland
    Basics now contains everything CardinalCollections did. Comfortable
    use of generics with the procedures of basics.tmpl (CursorList,
    CursorSet, Stack, NTree).

    Corrections in NTree.

*)
(***************************************************************************)

(* Pimitive operations to convert between byte-streams (TEXT) and
   Modula-3 scalar types *)

EXCEPTION ConversionFailed;

PROCEDURE IntToString4 (no: INTEGER): TEXT;
  (* converts a 4 byte cardinal to a (non-readable) TEXT-string of length 4 *)

PROCEDURE String4ToInt (string: TEXT): INTEGER RAISES {ConversionFailed};
  (* converts a TEXT-string of length 4 to a 4-byte cardinal *)

END GRASConversions.
