INTERFACE ChgMgmtGraphSystem;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/04/23 14:09:28  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

    Revision 1.5  1997/03/21 17:10:18  roland
    Adapted to changed Config. Login parameters are all optional except
    for root directory. Default server name is computed by Config.

    Revision 1.4  1997/03/20 16:53:53  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.3  1997/01/31 10:19:42  roland
    Minor corrections in exception handling.

    Revision 1.2  1996/12/23 10:34:57  roland
    Implementation of ChgMgmtGraphSystem separated from ChgMgmtGraph.

    Revision 1.1  1996/12/20 17:30:50  roland
    First version of ChgMgmtGraphSystem. Difference to
    PersistentGraphSystem: Graphs might be connected via deltas
    (DeltaCopyGraph) to allow efficient versioning.

*)
(***************************************************************************)

(*
 | --- PersistentGraphSystem ----------------------------------------------
 This abstract data object module manages a collection of pools. Pools must
 not be open during deleting, copying, and renaming.
 | ------------------------------------------------------------------------
 *)

IMPORT Pathname, TextCursorSet, PageFile, ClientInfoSeq;
IMPORT AtomList;


PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := 0;
                 grasserver: TEXT         := NIL;
                 nameserver: TEXT         := NIL);
  (* Supply basic system parameters.  A call to Login is mandatory before
     any operations can be performed.  Trying to open a pool without Login
     will result in a PageFile.NoAccess complaining about this.  If not
     specified, cachesize and nameserver will be set to system defaults
     (see Config.i3). *)


PROCEDURE DeletePool (baseName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError};
  (*
    This operation frees all occupied resources of the pool.
  *)

PROCEDURE CopyPool (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError};
  (*
    Create a copy of the source pool. This operation will fail if the
    destination name points to an allready existing pool.
  *)

PROCEDURE RenamePool (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError};
  (*
    Changes the pool to be refered by oldName to be found now as
    newName. This operation will fail if the newName points to an allready
    existing pool.
  *)

PROCEDURE ExistsPool (baseName: Pathname.T): BOOLEAN
  RAISES {InternalError};
  (*
    Tests if the specified pool exists.
  *)

PROCEDURE PoolInUse (baseName: Pathname.T): BOOLEAN
  RAISES {InternalError};
  (*
    Tests if the specified pool is allready in use by any client.
  *)

PROCEDURE GetPoolUser (baseName: Pathname.T): ClientInfoSeq.T
  RAISES {InternalError};
  (*
    Return information about all clients working with pool baseName
  *)

PROCEDURE GetPools (): TextCursorSet.T
  RAISES {PageFile.NoAccess, InternalError};
  (*
    Returns a name list of all managed pools.
  *)

EXCEPTION InternalError(AtomList.T);

END ChgMgmtGraphSystem.
