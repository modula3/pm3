INTERFACE PersistentCheckpointTree;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1997/04/24 14:30:13  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:04  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.4  1996/11/20 12:21:06  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/23 08:35:23  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:59:02  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:52  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT CheckpointTree AS Super;
IMPORT GraphCommandStream;
IMPORT PageFile, Access, VirtualResource;
IMPORT Pathname;

TYPE
  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      open (resource: VirtualResource.T;
            path    : Pathname.T;
            access  : Access.Mode;
            new     : BOOLEAN;
            fw, bw  : GraphCommandStream.T): T
            RAISES {Access.Locked, PageFile.NoAccess, Super.InternalError,
                    Access.Denied};
            (* Open a checkpoint tree.  The tree is stored within a
               checkpoint info stream.  The Deltas of this tree will all
               refer to the streams fw and bw and be stored in deltas. *)

      close () RAISES {Super.InternalError};
             (* Close the tree. *)
    END;


END PersistentCheckpointTree.
