INTERFACE VolatileMetaOpStack;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:35:23  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/23 08:35:53  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

*)
(***************************************************************************)


IMPORT MetaOpStack AS Super;

TYPE
  T <: Public;

  Public = Super.T OBJECT
           METHODS
             init (): T;
                   (* Before using a VolatileMetaOpStack it must be
                      initialized with init. *)
           END;

END VolatileMetaOpStack.
