INTERFACE PersistentGraphSystem;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.2  1997/04/23 14:33:57  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:33  roland
    Subsystem PersistentGraph adapted to handle graph boundary crossing
    edges. This has consequences on the architecture of the subsystem as
    well as on the graph model and interface.

    Graphs are organized in pools. Every graph has a number in the
    pool. Pools are the units of transaction management. Two graphs might
    be related by one external relation storage storing the edges between
    nodes of them. Nodes are identified by pairs (graph, entity), where
    graph is the number of the graph in the pool and entity the node
    number within the graph. Graphs and external relation storages are
    administered by the pool in a separate graph.

    Revision 1.6  1997/03/20 16:54:11  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.5  1996/11/21 15:21:54  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.4  1996/11/21 07:54:05  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.3  1996/11/20 12:23:11  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/10/08 13:05:43  roland
    GetGraphs returns a TextCursorSet instead of TextSeq.

    Revision 1.1  1996/08/23 14:51:12  rbnix
        Administration operations moved to a new module.

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

END PersistentGraphSystem.
