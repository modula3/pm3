INTERFACE PersistentDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:34:09  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.4  1996/11/20 12:21:09  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/23 08:35:25  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:59:05  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:55  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

(* A PersistentDelta.T stores commands in a stream. *)

IMPORT Delta AS Super;
IMPORT GraphCommandStream, FilePos, PCIStream;

TYPE
  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      init (checkpoints   : PCIStream.T;
            commands      : GraphCommandStream.T;
            forward       : BOOLEAN;
            cppos         : FilePos.T): T RAISES {};
            (* Open a delta on stream.  commands must be an open command
               stream.  The delta will be a forward or backward delta
               according to parameter forward.  The position cppos must a be
               valid stream positions for checkpoints.  It points to the
               position in checkpoints where the information for the delta
               is stored. *)

    END;

  
END PersistentDelta.
