INTERFACE PersistentMetaOpStack;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1997/04/24 14:30:33  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:24  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.2  1996/11/20 12:21:17  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.1  1996/09/23 08:35:29  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)

IMPORT MetaOpStack AS Super;
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
                   (* Open a stack.  The stack is stored as sequence of
                      cardinals in a stream. *)

             close () RAISES {Super.InternalError};
                    (* Close the stack. *)
           END;

END PersistentMetaOpStack.
