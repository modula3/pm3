INTERFACE RGRASNames;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:44  hosking
    Initial revision

    Revision 1.1  1997/10/24 14:39:13  renehuel
    These files implement the new RGRASGraph interface.

*)
(***************************************************************************)

IMPORT AtomList, Access, ChgMgmtGraphPool, Pathname, Names;
IMPORT TypedNames AS Super;

TYPE
  T <: Public;
  Public =
    Super.T OBJECT
    METHODS
      login (pool: ChgMgmtGraphPool.T; collection: Pathname.T)
             RAISES {InternalError, Access.Locked};
      getUniqueId (): CARDINAL RAISES {InternalError, Access.Locked,
                                       Names.Undeclared, Names.Unknown};
    END;

EXCEPTION InternalError(AtomList.T);
END RGRASNames.
