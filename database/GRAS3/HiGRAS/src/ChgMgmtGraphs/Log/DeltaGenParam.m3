MODULE DeltaGenParam;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:32:39  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/17 12:57:11  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

PROCEDURE Equal(<* UNUSED *> a, b: T): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Equal;
  
PROCEDURE Compare(<* UNUSED *> a, b: T): [-1 .. 1] =
  BEGIN
    RETURN -1;
  END Compare;

BEGIN
END DeltaGenParam.
