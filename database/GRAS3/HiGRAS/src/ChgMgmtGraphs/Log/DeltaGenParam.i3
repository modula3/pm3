INTERFACE DeltaGenParam;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.2  1997/07/21 10:38:22  roland
    Brand needed for new set implementation.

    Revision 1.1  1997/04/23 13:32:34  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/17 12:57:09  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* This is only for generic instantiation of DeltaList.  The procedures do
   nothing sensible. *)

IMPORT Delta;

CONST Brand = "Delta";
      
TYPE T = Delta.T;

PROCEDURE Equal (a, b: T): BOOLEAN;
PROCEDURE Compare (a, b: T): [-1 .. 1];

END DeltaGenParam.
