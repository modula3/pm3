INTERFACE PersistentLabelTable;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1997/04/24 14:30:20  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:13  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/11/20 12:21:12  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/09/20 13:59:07  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:59  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT LabelTable AS Super;
IMPORT Access, PageFile, VirtualResource, Pathname;

TYPE
  T <: Public;

  Public = Super.T OBJECT
           METHODS
             open (resource: VirtualResource.T;
                   path    : Pathname.T;
                   access  : Access.Mode;
                   new     : BOOLEAN            ): T
                   RAISES {Access.Denied, Access.Locked, PageFile.NoAccess,
                           Super.InternalError};
                   (* Open a label table.  The table is stored as a simple
                      binary search tree in a label path info stream. *)

             close () RAISES {Super.InternalError};
                    (* Close the table. *)
           END;

END PersistentLabelTable.
