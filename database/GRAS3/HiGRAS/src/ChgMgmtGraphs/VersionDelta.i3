INTERFACE VersionDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.2  1997/04/24 14:29:20  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:31:38  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/12/20 17:30:59  roland
    First version of ChgMgmtGraphSystem. Difference to
    PersistentGraphSystem: Graphs might be connected via deltas
    (DeltaCopyGraph) to allow efficient versioning.

*)
(***************************************************************************)

(* A VersionDelta.T stores differences of two graph versions in a separate
   resource using a PersistentDelta. *)

IMPORT Delta AS Super;
IMPORT Access, PersistentGraphPool;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = Super.T OBJECT
           METHODS
             open (pool  : PersistentGraphPool.T;
                   path  : Pathname.T;
                   access: Access.Mode;
                   new   : BOOLEAN;
                   local : BOOLEAN                ): T
                   RAISES {Access.Locked, Super.Error, Access.Denied};
                   (* Open a version delta. *)

             close () RAISES {Super.Error};
                    (* Close the delta. *)

             append (suffix: Super.T) RAISES {Access.Locked, Super.Error};

             prepend (prefix: Super.T) RAISES {Access.Locked, Super.Error};
           END;

END VersionDelta.
