INTERFACE VolatileLabelTable;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:35:13  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.2  1996/09/20 13:59:34  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:58:37  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT LabelTable AS Super;

TYPE
  T <: Public;

  Public = Super.T OBJECT
           METHODS
             init (): T;
                   (* Before using a VolatileLabelTable it must be
                      initialized with init. *)
           END;

END VolatileLabelTable.
