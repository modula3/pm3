GENERIC INTERFACE InternCursorSet(Set);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1997/08/15 11:20:15  roland
    Copying of relations now works by using an internal interface of CursorSet.

*)
(***************************************************************************)

PROCEDURE InternCopySet(source, target: Set.T);
  (* Copy source to target. Both mus be initialized sets. *)

END InternCursorSet.
