MODULE PersistentGraphSystem;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.5  1997/11/12 17:21:18  roland
    Initialization order changed: specialized event handler is now
    installed before login to rule engine. This ensures that triggers of
    other clients are propagated to the specialized event handler at
    login time.

    Revision 1.4  1997/11/12 15:23:37  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

    Revision 1.3  1997/07/21 10:43:06  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.2  1997/04/23 14:33:58  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:35  roland
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

    Revision 1.7  1997/03/20 16:54:12  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.6  1996/11/21 15:21:56  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.5  1996/11/21 07:54:07  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.4  1996/11/20 12:23:12  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/11/08 15:25:33  roland
    Bugfix in translating sequence to set.

    Revision 1.2  1996/10/08 13:05:46  roland
    GetGraphs returns a TextCursorSet instead of TextSeq.

    Revision 1.1  1996/08/23 14:51:14  rbnix
        Administration operations moved to a new module.

*)
(***************************************************************************)
(*
 | --- PersistentGraphSystem ----------------------------------------------
 As long as pools are directly mapped to resources the operations in this
 module are calling the pendants in VirtualResourceSystem without doing
 further work. An exception is GetGraphs which copies the sequence of
 resource names to a set of resource names.
 | ------------------------------------------------------------------------
 *)
IMPORT Pathname, TextCursorSet, TextSeq, PageFile,
       VirtualResourceSystem, ErrorSupport, ClientInfoSeq,
       PersistentGraphEventHandler;

PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := 0;
                 grasserver: TEXT         := NIL;
                 nameserver: Pathname.T   := NIL  ) =
  BEGIN
    (* install separate event handler for graph events *)
    PersistentGraphEventHandler.Install();
    VirtualResourceSystem.Login(root, cachesize, grasserver, nameserver);
  END Login;

PROCEDURE DeletePool (baseName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      VirtualResourceSystem.DeleteResource(baseName);
    EXCEPT
    | VirtualResourceSystem.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphSystem.DeletePool",
                              "VirtualResourceSystem.FatalError", info));
    END;
  END DeletePool;


PROCEDURE CopyPool (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      VirtualResourceSystem.CopyResource(sourceName, destName);
    EXCEPT
    | VirtualResourceSystem.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphSystem.CopyPool",
                              "VirtualResourceSystem.FatalError", info));
    END;
  END CopyPool;


PROCEDURE RenamePool (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      VirtualResourceSystem.RenameResource(oldName, newName);
    EXCEPT
    | VirtualResourceSystem.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphSystem.RenamePool",
                              "VirtualResourceSystem.FatalError", info));
    END;
  END RenamePool;


PROCEDURE ExistsPool (baseName: Pathname.T): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN VirtualResourceSystem.ExistsResource(baseName);
    EXCEPT
    | VirtualResourceSystem.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphSystem.ExistsPool",
                              "VirtualResourceSystem.FatalError", info));
    END;
  END ExistsPool;


PROCEDURE PoolInUse (baseName: Pathname.T): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN VirtualResourceSystem.ResourceInUse(baseName);
    EXCEPT
    | VirtualResourceSystem.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphSystem.PoolInUse",
                              "VirtualResourceSystem.FatalError", info));
    END;
  END PoolInUse;


PROCEDURE GetPoolUser (baseName: Pathname.T): ClientInfoSeq.T
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN VirtualResourceSystem.GetResourceUser(baseName);
    EXCEPT
    | VirtualResourceSystem.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphSystem.GetPoolUser",
                              "VirtualResourceSystem.FatalError", info));
    END;
  END GetPoolUser;


PROCEDURE GetPools (): TextCursorSet.T
  RAISES {PageFile.NoAccess, InternalError} =

  PROCEDURE SeqToSet (seq: TextSeq.T): TextCursorSet.T RAISES {} =
    VAR set: TextCursorSet.T;
    BEGIN
      set := TextCursorSet.New();
      FOR i := 0 TO seq.size() - 1 DO set.insert(seq.get(i)); END;
      RETURN set;
    END SeqToSet;

  BEGIN
    TRY
      RETURN SeqToSet(VirtualResourceSystem.GetResources());
    EXCEPT
    | VirtualResourceSystem.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphSystem.GetPools",
                              "VirtualResourceSystem.FatalError", info));
    END;
  END GetPools;


BEGIN
END PersistentGraphSystem.
