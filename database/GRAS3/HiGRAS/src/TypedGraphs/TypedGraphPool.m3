MODULE TypedGraphPool EXPORTS TypedGraphPool, InternTypedGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.17  1998/08/27 16:12:30  roland
    Bugfixes for IntraCopyGraph.

    Revision 1.16  1998/03/18 13:39:47  roland
    More slight modifications to local parameters (default values and
    parameter ordering)

    Revision 1.15  1998/03/18 12:13:28  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.14  1998/03/18 09:27:28  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.13  1998/03/17 14:14:34  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.12  1998/02/16 12:04:23  roland
    Introduced a two-phase commit to coordinate graph commits.

    Revision 1.11  1997/10/31 14:23:29  roland
    Adapted to new RuleEngine.

    Revision 1.10  1997/10/24 14:10:35  renehuel
    New procedure 'CopyScheme' to copy a scheme from one pool to another.

    Revision 1.9  1997/10/17 16:45:15  renehuel
    bugfix in deltaCopyGraph.

    Revision 1.8  1997/07/25 14:12:32  roland
    Commit-Transaction in OpenCGForScheme must be outside of If-Statement.

    Revision 1.7  1997/07/21 10:48:10  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.6  1997/07/07 15:40:24  roland
    Added caches for relations in a Scheme and for node types in a typed
    graph.

    Revision 1.5  1997/06/27 07:09:37  roland
    Transactions added so that a client might open a graph outside without
    a transaction.

    Revision 1.4  1997/06/10 12:47:27  roland
    Bugfix: Opencounter must start with 1.

    Revision 1.3  1997/06/05 12:18:10  roland
    Method getUser implemented.

    Revision 1.2  1997/05/05 10:50:44  roland
    Bugfixes in open routines for schemes.
    Dependency information moved from intern to public interface.

    Revision 1.1  1997/05/01 13:23:23  roland
    TypedGraph layer adapted to graph boundary crossing edges.

*)
(***************************************************************************)

IMPORT ErrorSupport;
IMPORT Pathname, Text;
IMPORT ChgMgmtGraphPool AS Super;
IMPORT TypedNames, InternChgMgmtGraphPool, ChgMgmtGraph, ChgMgmtNames,
       PersistentNames;
IMPORT TextCursorSet, PageFile;
IMPORT Access, ClientInfoSeq;
IMPORT IdCache, NameCache, AttributeCache, LabelCache, SourceCache,
       TargetCache;
IMPORT IdCacheStorage, NameCacheStorage, AttributeCacheStorage,
       LabelCacheStorage, SourceCacheStorage, TargetCacheStorage;

REVEAL
  T = Internal BRANDED OBJECT
        graphs     : TypedNames.T;
        openSchemes: OpenSchemeList;
        openGraphs : CallbackList;
      OVERRIDES
        open                := Open;
        close               := Close;
        beginTransaction    := BeginTransaction;
        commitTransaction   := CommitTransaction;
        abortTransaction    := AbortTransaction;
        deleteScheme        := DeleteScheme;
        copyScheme          := IntraCopyScheme;
        renameScheme        := RenameScheme;
        existsScheme        := ExistsScheme;
        getSchemes          := GetSchemes;
        hasScheme           := HasScheme;
        getScheme           := GetScheme;
        getGraphsWithScheme := GetGraphsWithScheme;
        getVersion          := GetVersion;
        deleteGraph         := DeleteGraph;
        copyGraph           := IntraCopyGraph;
        renameGraph         := RenameGraph;
        getGraphs           := GetGraphs;
        existsGraph         := ExistsGraph;
        inUse               := ItemInUse;
        getUser             := GetItemUser;

        deltaCopyGraph := DeltaCopyGraph;

        (* Internal interface *)
        openIntern       := OpenIntern;
        loginToNames     := LoginToNames;
        openCG           := OpenCG;
        closeCG          := CloseCG;
        openCGForScheme  := OpenCGForScheme;
        closeCGForScheme := CloseCGForScheme;
      END;

PROCEDURE ErrAbort (pool: T) =
  BEGIN
    TRY
      Super.T.abortTransaction(pool);
    EXCEPT
      Super.NotInTransaction =>  (* ignore *)
    | Super.InternalError =>     (* ignore *)
    END;
  END ErrAbort;

PROCEDURE AssertGraph (pool: T; name: Pathname.T; local: BOOLEAN := FALSE)
  RAISES {Access.Locked, NoGraph, InternalError} =
  BEGIN
    TRY
      IF pool.graphs.existsScheme(name, local) THEN RAISE NoGraph; END;
    EXCEPT
      TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.AssertGraph",
                                       "TypedNames.InternalError", info));
    END;
  END AssertGraph;

PROCEDURE AssertScheme (pool: T; name: Pathname.T; local: BOOLEAN := FALSE)
  RAISES {Access.Locked, NoScheme, InternalError} =
  BEGIN
    TRY
      IF NOT pool.graphs.existsScheme(name, local) THEN
        RAISE NoScheme;
      END;
    EXCEPT
      TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.AssertScheme",
                                       "TypedNames.InternalError", info));
    END;
  END AssertScheme;

(* Implementation of TypedGraphPool-Interface *)

PROCEDURE Open (pool  : T;
                name  : Pathname.T;
                access: Access.Mode;
                new   : BOOLEAN      ): T
  RAISES {Access.Locked, InternalError, Access.Denied, PageFile.NoAccess} =
  BEGIN
    pool := OpenIntern(pool, name, access, new);
    LoginToNames(pool, NEW(TypedNames.T));
    RETURN pool;
  END Open;

PROCEDURE Close (pool: T) RAISES {InternalError} =
  BEGIN
    TRY
      IF pool.graphs # NIL THEN pool.graphs.logout(); END;
      Super.T.close(pool);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.Close",
                              "ChgMgmtGraphPool.FatalError", info));
    END;
  END Close;

PROCEDURE BeginTransaction (pool: T) RAISES {InternalError} =
  VAR
    cbl      : CallbackList;
    callbacks: CallbackSuite;
    graph    : ChgMgmtGraph.T;
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      cbl := CBLFirst(pool.openGraphs);
      (* notify each open graph of beginning transaction *)
      WHILE cbl # NIL DO
        CBLGet(cbl, graph, callbacks);
        callbacks.begin(graph);
        cbl := CBLNext(cbl);
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.BeginTransaction",
                              "ChgMgmtGraphPool.InternalError", info));
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("TypedGraphPool.BeginTransaction",
                                   "ChgMgmtGraph.InternalError", info));
    END;
  END BeginTransaction;

PROCEDURE CommitTransaction (pool: T)
  RAISES {InternalError, NotInTransaction, CardinalityError} =
  VAR
    cbl      : CallbackList;
    callbacks: CallbackSuite;
    graph    : ChgMgmtGraph.T;
  BEGIN
    TRY
      cbl := CBLFirst(pool.openGraphs);
      (* notify each open graph of committing transaction *)
      WHILE cbl # NIL DO
        CBLGet(cbl, graph, callbacks);
        callbacks.prepare(graph);
        cbl := CBLNext(cbl);
      END;

      cbl := CBLFirst(pool.openGraphs);
      (* everyone is ready to commit, do it now *)
      WHILE cbl # NIL DO
        CBLGet(cbl, graph, callbacks);
        callbacks.commit(graph);
        cbl := CBLNext(cbl);
      END;
      Super.T.commitTransaction(pool);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.CommitTransaction",
                                       "Super.InternalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("TypedGraphPool.CommitTransaction",
                                   "ChgMgmtGraph.InternalError", info));
    END;
  END CommitTransaction;

PROCEDURE AbortTransaction (pool: T)
  RAISES {InternalError, NotInTransaction} =
  VAR
    hdl        : OpenSchemeList;
    idCache    : IdCache.T;
    nameCache  : NameCache.T;
    attrCache  : AttributeCache.T;
    labelCache : LabelCache.T;
    sourceCache: SourceCache.T;
    targetCache: TargetCache.T;
    cbl        : CallbackList;
    callbacks  : CallbackSuite;
    graph      : ChgMgmtGraph.T;
  BEGIN
    TRY
      hdl := OSLFirst(pool.openSchemes);
      (* caches might contain invalid temporary data *)
      WHILE hdl # NIL DO
        OSLGet(hdl, idCache, nameCache, labelCache, attrCache, sourceCache,
               targetCache);
        idCache.flush(save := FALSE);
        nameCache.flush(save := FALSE);
        labelCache.flush(save := FALSE);
        attrCache.flush(save := FALSE);
        sourceCache.flush(save := FALSE);
        targetCache.flush(save := FALSE);
        hdl := OSLNext(hdl);
      END;

      cbl := CBLFirst(pool.openGraphs);
      (* notify each open graph of abort of transaction *)
      WHILE cbl # NIL DO
        CBLGet(cbl, graph, callbacks);
        callbacks.abort(graph);
        cbl := CBLNext(cbl);
      END;
      Super.T.abortTransaction(pool);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.AbortTransaction",
                                       "Super.InternalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    | IdCache.Error (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.AbortTransaction",
                              "IdCache.Error", info));
    | NameCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.AbortTransaction",
                                       "NameCache.Error", info));
    | LabelCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.AbortTransaction",
                                       "LabelCache.Error", info));
    | AttributeCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.AbortTransaction",
                                       "AttributeCache.Error", info));
    | SourceCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.AbortTransaction",
                                       "SourceCache.Error", info));
    | TargetCache.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.AbortTransaction",
                                       "TargetCache.Error", info));
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("TypedGraphPool.AbortTransaction",
                                   "ChgMgmtGraph.InternalError", info));
    END;
  END AbortTransaction;


PROCEDURE DeleteGraph (pool    : T;
                       baseName: Pathname.T;
                       local   : BOOLEAN      := FALSE)
  RAISES {InUse, Access.Locked, NoGraph, InternalError, NotExistent} =
  BEGIN
    TRY
      AssertGraph(pool, baseName, local);
      Super.T.deleteGraph(pool, baseName, local);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.DeleteGraph",
                              "ChgMgmtGraphPool.InternalError", info));
    | Super.InUse => RAISE InUse;
    | Super.NotExistent => RAISE NotExistent;
    END;
  END DeleteGraph;

PROCEDURE DeleteScheme (pool    : T;
                        baseName: Pathname.T;
                        local   : BOOLEAN      := FALSE)
  RAISES {Access.Locked, NoScheme, InUse, InternalError, NotExistent} =
  BEGIN
    TRY
      AssertScheme(pool, baseName, local);
      IF NOT pool.graphs.existsScheme(baseName, local) THEN RETURN END;
      IF NOT pool.graphs.getGraphsWithScheme(baseName, local).isEmpty() THEN
        RAISE InUse;
      END;
      Super.T.deleteGraph(pool, baseName, local);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.DeleteGraph",
                              "ChgMgmtGraphPool.InternalError", info));
    | TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.DeleteScheme",
                                       "TypedNames.InternalError", info));
    | TypedNames.Unknown =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraphPool.DeleteScheme",
                                            "TypedNames.Unknown"));
    | Super.NotExistent => RAISE NotExistent;
    | Super.InUse => RAISE InUse;
    END;
  END DeleteScheme;


PROCEDURE IntraCopyGraph (pool      : T;
                          sourceName: Pathname.T;
                          destName  : Pathname.T;
                          embedded  : BOOLEAN;
                          local     : BOOLEAN      := FALSE)
  RAISES {Access.Locked, NoGraph, InternalError, Existent, InUse,
          NotExistent} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      AssertGraph(pool, sourceName, local);
      Super.T.copyGraph(pool, sourceName, destName, embedded, local);
      (* attach to scheme *)
      pool.graphs.connectToScheme(
        destName, pool.graphs.getScheme(sourceName, local), local);
      Super.T.commitTransaction(pool);
    EXCEPT
      TypedNames.Unknown =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("TypedGraphPool.IntraCopyGraph",
                                    "TypedNames.IntraCopyGraph"));
    | TypedNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.IntraCopyGraph",
                                       "TypedNames.IntraCopyGraph", info));
    | Super.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.CopyGraph",
                              "ChgMgmtGraphPool.InternalError", info));
    | Super.Existent => ErrAbort(pool); RAISE Existent;
    | Super.NotExistent => ErrAbort(pool); RAISE NotExistent;
    | Super.InUse => ErrAbort(pool); RAISE InUse;
    END;
  END IntraCopyGraph;

PROCEDURE DeltaCopyGraph (pool   : T;
                          source : Pathname.T;
                          target : Pathname.T;
                          local  : BOOLEAN      := FALSE;
                          forward: BOOLEAN                )
  RAISES {Access.Locked, Super.InUse, Super.Existent, Super.NotExistent,
          Super.InternalError} =
  BEGIN
    TRY
      WITH scheme = pool.graphs.getScheme(source, local) DO
        Super.T.deltaCopyGraph(pool, source, target, local, forward);
        pool.graphs.connectToScheme(target, scheme, local);
      END;
    EXCEPT
      TypedNames.Unknown =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "TypedGraphPool.DeltaCopyGraph", "TypedNames.Unknown"));
    | TypedNames.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("TypedGraphPool.DeltaCopyGraph",
                                       "TypedNames.InternalError", info));
    END;
  END DeltaCopyGraph;

PROCEDURE IntraCopyScheme (pool      : T;
                           sourceName: Pathname.T;
                           destName  : Pathname.T;
                           local     : BOOLEAN      := FALSE)
  RAISES {Access.Locked, NoScheme, InternalError, Existent, NotExistent,
          InUse} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      AssertScheme(pool, sourceName, local);
      Super.T.copyGraph(
        pool, sourceName, destName, FALSE (*embedded*), local);
      pool.graphs.insertScheme(destName, local);
      Super.T.commitTransaction(pool);
    EXCEPT
    | Super.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.CopyGraph",
                              "ChgMgmtGraphPool.InternalError", info));
    | TypedNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.CopyScheme",
                                       "TypedNames.InternalError", info));
    | Super.Existent => ErrAbort(pool); RAISE Existent;
    | Super.NotExistent => ErrAbort(pool); RAISE NotExistent;
    | Super.InUse => ErrAbort(pool); RAISE InUse;
    END;
  END IntraCopyScheme;

PROCEDURE RenameScheme (pool   : T;
                        oldName: Pathname.T;
                        newName: Pathname.T;
                        local  : BOOLEAN      := FALSE)
  RAISES {Access.Locked, NoScheme, InternalError, Existent, NotExistent,
          InUse} =
  BEGIN
    TRY
      AssertScheme(pool, oldName, local);
      Super.T.renameGraph(pool, oldName, newName, local);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.RenameGraph",
                              "ChgMgmtGraphPool.InternalError", info));
    | Super.Existent => RAISE Existent;
    | Super.NotExistent => RAISE NotExistent;
    | Super.InUse => RAISE InUse;
    END;
  END RenameScheme;

PROCEDURE RenameGraph (pool   : T;
                       oldName: Pathname.T;
                       newName: Pathname.T;
                       local  : BOOLEAN      := FALSE)
  RAISES {Access.Locked, NoGraph, InternalError, Existent, NotExistent,
          InUse} =
  BEGIN
    TRY
      AssertGraph(pool, oldName, local);
      Super.T.renameGraph(pool, oldName, newName, local);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.RenameGraph",
                              "ChgMgmtGraphPool.InternalError", info));
    | Super.Existent => RAISE Existent;
    | Super.NotExistent => RAISE NotExistent;
    | Super.InUse => RAISE InUse;
    END;
  END RenameGraph;


PROCEDURE ExistsGraph (pool    : T;
                       baseName: Pathname.T;
                       local   : BOOLEAN      := FALSE): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      IF Super.T.existsGraph(pool, baseName, local) THEN
        RETURN NOT pool.graphs.existsScheme(baseName, local);
      ELSE
        RETURN FALSE;
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.ExistsGraph",
                              "ChgMgmtGraphPool.InternalError", info));
    | TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.ExistsGraph",
                                       "TypedNames.InternalError", info));
    END;
  END ExistsGraph;


PROCEDURE GetGraphs (pool: T; local: BOOLEAN := FALSE): TextCursorSet.T
  RAISES {Access.Locked, InternalError} =
  VAR res: TextCursorSet.T;
  BEGIN
    TRY
      res := Super.T.getGraphs(pool, local);
      WITH schemes = pool.graphs.getSchemes(local) DO
        res.difference(schemes);
        schemes.dispose();
      END;
      RETURN res;
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.GetGraphs",
                              "ChgMgmtGraphPool.InternalError", info));
    | TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.GetGraphs",
                                       "TypedNames.InternalError", info));
    END;
  END GetGraphs;

PROCEDURE ExistsScheme (pool    : T;
                        baseName: Pathname.T;
                        local   : BOOLEAN      := FALSE): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN pool.graphs.existsScheme(baseName, local);
    EXCEPT
      TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.ExistsScheme",
                                       "TypedNames.InternalError", info));
    END;
  END ExistsScheme;

PROCEDURE GetSchemes (pool: T; local: BOOLEAN := FALSE): TextCursorSet.T
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN pool.graphs.getSchemes(local);
    EXCEPT
      TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.GetSchemes",
                                       "TypedNames.InternalError", info));
    END;
  END GetSchemes;

PROCEDURE HasScheme (pool: T; graph: Pathname.T; local: BOOLEAN := FALSE):
  BOOLEAN RAISES {Access.Locked, InternalError, NoGraph, NotExistent} =
  BEGIN
    TRY
      AssertGraph(pool, graph, local);
      IF NOT Super.T.existsGraph(pool, graph, local) THEN
        RAISE NotExistent;
      END;
      RETURN pool.graphs.hasScheme(graph, local);
    EXCEPT
      TypedNames.Unknown =>
        RAISE InternalError(ErrorSupport.Create("TypedGraphPool.HasScheme",
                                                "TypedNames.Unknown"));
    | TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.HasScheme",
                                       "TypedNames.InternalError", info));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.HasScheme",
                              "ChgMgmtGraphPool.InternalError", info));
    END;
  END HasScheme;

PROCEDURE GetScheme (pool: T; graph: Pathname.T; local: BOOLEAN := FALSE):
  Pathname.T
  RAISES {Access.Locked, InternalError, NotExistent, NoGraph, NoScheme} =
  BEGIN
    TRY
      AssertGraph(pool, graph, local);
      IF NOT Super.T.existsGraph(pool, graph, local) THEN
        RAISE NotExistent;
      END;
      RETURN pool.graphs.getScheme(graph, local);
    EXCEPT
      TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.GetScheme",
                                       "TypedNames.InternalError", info));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.GetScheme",
                              "ChgMgmtGraphPool.InternalError", info));
    | TypedNames.Unknown => RAISE NoScheme;
    END;
  END GetScheme;

PROCEDURE GetGraphsWithScheme (pool  : T;
                               scheme: Pathname.T;
                               local : BOOLEAN      := FALSE):
  TextCursorSet.T RAISES {Access.Locked, InternalError, NoScheme} =
  BEGIN
    TRY
      IF NOT pool.graphs.existsScheme(scheme, local) THEN
        RAISE NoScheme;
      ELSE
        RETURN pool.graphs.getGraphsWithScheme(scheme, local);
      END;
    EXCEPT
      TypedNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.GetGraphsWithScheme",
                              "TypedNames.InternalError", info));
    | TypedNames.Unknown =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraphPool.GetGraphsWithScheme",
                                    "TypedNames.Unknown"));
    END;
  END GetGraphsWithScheme;

PROCEDURE GetVersion (pool: T; scheme: Pathname.T; local: BOOLEAN := FALSE):
  CARDINAL RAISES {Access.Locked, InternalError, NoScheme} =
  BEGIN
    TRY
      IF NOT pool.graphs.existsScheme(scheme, local) THEN
        RAISE NoScheme;
      ELSE
        RETURN pool.graphs.getVersion(scheme, local);
      END;
    EXCEPT
      TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.GetVersion",
                                       "TypedNames.InternalError", info));
    | TypedNames.Unknown =>
        RAISE
          InternalError(ErrorSupport.Create("TypedGraphPool.GetVersion",
                                            "TypedNames.Unknown"));
    END;
  END GetVersion;


PROCEDURE ItemInUse (pool: T; name: Pathname.T; local: BOOLEAN := FALSE):
  BOOLEAN RAISES {InternalError} =
  BEGIN
    TRY
      RETURN Super.T.graphInUse(pool, name, local);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraphPool.InUse", "Super.InternalError", info));
    END;
  END ItemInUse;

PROCEDURE GetItemUser (pool: T; name: Pathname.T): ClientInfoSeq.T
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN Super.T.getGraphUser(pool, name);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "TypedGraphPool.InUse", "Super.InternalError", info));
    END;
  END GetItemUser;

(* internal interface *)
PROCEDURE OpenIntern (pool  : T;
                      name  : Pathname.T;
                      access: Access.Mode;
                      new   : BOOLEAN      ): T
  RAISES {Access.Denied, PageFile.NoAccess} =
  BEGIN
    pool := Super.T.openIntern(pool, name, access, new);
    OSLInit(pool.openSchemes);
    CBLInit(pool.openGraphs);
    RETURN pool;
  END OpenIntern;

PROCEDURE LoginToNames (pool: T; names: TypedNames.T)
  RAISES {InternalError, Access.Locked} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      pool.graphs := names;
      Super.T.loginToNames(pool, names);
      TypedNames.T.login(names, pool, ".GRAS");
      Super.T.commitTransaction(pool);
    EXCEPT
      TypedNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.LoginToNames",
                                       "TypedNames.InternalError", info));
    | Super.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.Open",
                              "ChgMgmtGraphPool.InternalError", info));
    | Access.Locked => ErrAbort(pool); RAISE Access.Locked;
    END;
  END LoginToNames;

PROCEDURE OpenCG (    pool        : T;
                      graph       : ChgMgmtGraph.T;
                      baseName    : Pathname.T;
                      access      : ChgMgmtGraph.AccessMode;
                      errorChecks : BOOLEAN;
                      local       : BOOLEAN                   := FALSE;
                      log         : LogMode;
                      scheme      : Pathname.T;
                      callbacks   : CallbackSuite;
                  VAR schemeNumber: CARDINAL                            ):
  ChgMgmtGraph.T RAISES {Access.Locked, InternalError, PageFile.NoAccess,
                         Access.Denied, NotExistent, InUse} =
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      IF NOT pool.graphs.existsGraph(baseName, local) THEN
        graph := ChgMgmtGraph.T.open(
                   graph, pool, baseName, access, new := TRUE,
                   local := local, errorChecks := errorChecks, log := log);
        pool.graphs.connectToScheme(baseName, scheme, local);
      ELSE
        graph := ChgMgmtGraph.T.open(
                   graph, pool, baseName, access, new := FALSE,
                   local := local, errorChecks := errorChecks, log := log);
      END;
      schemeNumber := pool.graphs.getGraphNumber(scheme, local);
      Super.T.commitTransaction(pool);
      CBLInsert(pool.openGraphs, graph, callbacks);
      RETURN graph;
    EXCEPT
      ChgMgmtGraph.InternalError (info) =>
        ErrAbort(pool);
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "TypedGraphPool.OpenCG", "ChgMgmtGraph.InternalError", info));
    | Super.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.OpenCG",
                              "ChgMgmtGraphPool.InternalError", info));
    | Super.NotInTransaction =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraphPool.OpenCG",
                                    "ChgMgmtGraphPool.NotInTransaction"));
    | PersistentNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.OpenCG",
                              "PersistentNames.InternalError", info));
    | PersistentNames.Unknown =>
        ErrAbort(pool);
        RAISE
          InternalError(ErrorSupport.Create("TypedGraphPool.OpenCG",
                                            "PersistentNames.Unknown"));
    | ChgMgmtGraph.InUse => ErrAbort(pool); RAISE InUse;
    | ChgMgmtGraph.NotExistent => ErrAbort(pool); RAISE NotExistent;
    | ChgMgmtNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "TypedGraphPool.OpenCG", "ChgMgmtNames.InternalError", info));
    | TypedNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.OpenCG",
                                       "TypedNames.InternalError", info));
    | TypedNames.Unknown =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Create("TypedGraphPool.OpenCG",
                                                "TypedNames.Unknown"));
    END;
  END OpenCG;

PROCEDURE CloseCG (pool: T; graph: ChgMgmtGraph.T; keepLog: BOOLEAN)
  RAISES {InternalError} =
  BEGIN
    TRY
      ChgMgmtGraph.T.close(graph, keepLog);
      CBLRemove(pool.openGraphs, graph);
    EXCEPT
      ChgMgmtGraph.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "TypedGraphPool.CloseCG",
                          "ChgMgmtGraph.InternalError", info));
    END;
  END CloseCG;

PROCEDURE OpenCGForScheme (    pool      : T;
                           VAR graph     : ChgMgmtGraph.T;
                               schemeName: Pathname.T;
                               local     : BOOLEAN          := FALSE;
                               access     : ChgMgmtGraph.AccessMode;
                               errorChecks: BOOLEAN;
                               log        : ChgMgmtGraph.LogMode;
                           VAR idCache    : IdCache.T;
                           VAR nameCache  : NameCache.T;
                           VAR attrCache  : AttributeCache.T;
                           VAR labelCache : LabelCache.T;
                           VAR sourceCache: SourceCache.T;
                           VAR targetCache: TargetCache.T            )
  RAISES {NoScheme, InternalError, PageFile.NoAccess, Access.Locked,
          Access.Denied, InUse, NotExistent} =
  VAR hdl: OpenSchemeList;
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      IF OSLFind(pool.openSchemes, schemeName, hdl) THEN
        (* this scheme was already opened *)
        (* schemes should only be open once for each client *)
        WITH count = OSLGetOpenCount(hdl) DO
          OSLSetOpenCount(hdl, count + 1);
          graph := OSLGetGraph(hdl);
          OSLGet(hdl, idCache, nameCache, labelCache, attrCache,
                 sourceCache, targetCache)
        END;
      ELSE
        IF pool.graphs.existsScheme(schemeName, local) THEN
          graph := ChgMgmtGraph.T.open(
                     graph, pool, schemeName, access, FALSE (*new*),
                     errorChecks, local, log := log);
        ELSIF NOT pool.graphs.existsGraph(schemeName, local) THEN
          graph := ChgMgmtGraph.T.open(
                     graph, pool, schemeName, access, TRUE (*new*),
                     errorChecks, local, log := log);
          pool.graphs.insertScheme(schemeName, local);
        ELSE
          RAISE NoScheme;
        END;
        VAR
          idSt   := NEW(IdCacheStorage.T).init(graph);
          nameSt := NEW(NameCacheStorage.T).init(graph);
          attSt  := NEW(AttributeCacheStorage.T).init(graph);
          labSt  := NEW(LabelCacheStorage.T).init(graph);
          souSt  := NEW(SourceCacheStorage.T).init(graph);
          tarSt  := NEW(TargetCacheStorage.T).init(graph);
        BEGIN
          idCache := NEW(IdCache.T).init(idSt);
          nameCache := NEW(NameCache.T).init(nameSt);
          attrCache := NEW(AttributeCache.T).init(attSt);
          labelCache := NEW(LabelCache.T).init(labSt);
          sourceCache := NEW(SourceCache.T).init(souSt);
          targetCache := NEW(TargetCache.T).init(tarSt);
        END;
        OSLInsert(pool.openSchemes, graph, schemeName, idCache, nameCache,
                  labelCache, attrCache, sourceCache, targetCache);
      END;
      Super.T.commitTransaction(pool);
    EXCEPT
      ChgMgmtGraph.InternalError (info) =>
        ErrAbort(pool);
        RAISE
          InternalError(
            ErrorSupport.Propagate("TypedGraphPool.OpenCGForScheme",
                                   "ChgMgmtGraph.InternalError", info));
    | ChgMgmtGraph.InUse => ErrAbort(pool); RAISE InUse;
    | ChgMgmtGraph.NotExistent => ErrAbort(pool); RAISE NotExistent;
    | Super.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedGraphPool.OpenCGForScheme",
                              "ChgMgmtGraphPool.InternalError", info));
    | Super.NotInTransaction =>
        RAISE InternalError(
                ErrorSupport.Create("TypedGraphPool.OpenCGForScheme",
                                    "ChgMgmtGraphPool.NotInTransaction"));
    | ChgMgmtNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE
          InternalError(
            ErrorSupport.Propagate("TypedGraphPool.OpenCGForScheme",
                                   "ChgMgmtNames.InternalError", info));
    | TypedNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.OpenCGForScheme",
                                       "TypedNames.InternalError", info));
    END;
  END OpenCGForScheme;

PROCEDURE CloseCGForScheme (pool: T; graph: ChgMgmtGraph.T)
  RAISES {InternalError} =
  VAR hdl: OpenSchemeList;
  BEGIN
    TRY
      IF OSLFindGraph(pool.openSchemes, graph, hdl) THEN
        WITH count = OSLGetOpenCount(hdl) DO
          IF count <= 1 THEN
            ChgMgmtGraph.T.close(graph, keepLog := FALSE);
            OSLRemove(pool.openSchemes, graph);
          ELSE
            OSLSetOpenCount(hdl, count - 1);
          END
        END;
      END;
    EXCEPT
      ChgMgmtGraph.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("TypedGraphPool.CloseCGForScheme",
                                   "ChgMgmtGraph.InternalError", info));
    END;
  END CloseCGForScheme;


PROCEDURE CopyGraph (sourcePool : T;
                     sourceGraph: Pathname.T;
                     sourcelocal: BOOLEAN;
                     targetPool : T;
                     targetGraph: Pathname.T;
                     targetlocal: BOOLEAN     )
  RAISES {InUse, NotExistent, Existent, InTransaction, InternalError,
          Access.Locked} =
  BEGIN
    TRY
      Super.CopyGraph(sourcePool, sourceGraph, sourcelocal, targetPool,
                      targetGraph, targetlocal);
    EXCEPT
      Super.InUse => RAISE InUse;
    | Super.Existent => RAISE Existent;
    | Super.NotExistent => RAISE NotExistent;
    | Super.InTransaction => RAISE InTransaction;
    | Super.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "TypedGraphPool.CopyGraph", "Super.InternalError", info));
    END;
  END CopyGraph;

PROCEDURE CopyScheme (sourcePool  : T;
                      sourceScheme: Pathname.T;
                      sourcelocal : BOOLEAN;
                      targetPool  : T;
                      targetScheme: Pathname.T;
                      targetlocal : BOOLEAN     )
  RAISES {InUse, NotExistent, Existent, InTransaction, InternalError,
          Access.Locked} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      (* Copying the scheme as a graph. *)
      Super.CopyGraph(sourcePool, sourceScheme, sourcelocal,
                      targetPool, targetScheme, targetlocal);
      (* Additionally adding it to the schemes collection. *)
      Super.T.beginTransaction(targetPool);
      targetPool.graphs.insertScheme(targetScheme, targetlocal);
      Super.T.commitTransaction(targetPool);
    EXCEPT
      Super.InUse => RAISE InUse;
    | Super.Existent => RAISE Existent;
    | Super.NotExistent => RAISE NotExistent;
    | Super.InTransaction => RAISE InTransaction;
    | Super.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "TypedGraphPool.CopyGraph", "Super.InternalError", info));
    | TypedNames.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("TypedGraphPool.CopyScheme",
                                       "TypedNames.InternalError", info));
    END;
  END CopyScheme;

(* LOCAL ADT INTERFACE OpenSchemeList; *)

TYPE
  OpenSchemeList = REF OpenSchemeInfo;
  OpenSchemeInfo = RECORD
                     name       : Pathname.T;
                     graph      : ChgMgmtGraph.T;
                     idCache    : IdCache.T;
                     nameCache  : NameCache.T;
                     labelCache : LabelCache.T;
                     attrCache  : AttributeCache.T;
                     sourceCache: SourceCache.T;
                     targetCache: TargetCache.T;
                     count      : CARDINAL;
                     next       : OpenSchemeList;
                   END;

PROCEDURE OSLInit (VAR list: OpenSchemeList) =
  BEGIN
    list := NIL;
  END OSLInit;

PROCEDURE OSLFirst (list: OpenSchemeList): OpenSchemeList =
  BEGIN
    RETURN list;
  END OSLFirst;

PROCEDURE OSLNext (list: OpenSchemeList): OpenSchemeList =
  BEGIN
    IF list # NIL THEN RETURN list^.next; ELSE RETURN NIL; END;
  END OSLNext;

PROCEDURE OSLGet (    list       : OpenSchemeList;
                  VAR idCache    : IdCache.T;
                  VAR nameCache  : NameCache.T;
                  VAR labelCache : LabelCache.T;
                  VAR attrCache  : AttributeCache.T;
                  VAR sourceCache: SourceCache.T;
                  VAR targetCache: TargetCache.T     ) =
  BEGIN
    IF list # NIL THEN
      labelCache := list^.labelCache;
      nameCache := list^.nameCache;
      attrCache := list^.attrCache;
      idCache := list^.idCache;
      sourceCache := list^.sourceCache;
      targetCache := list^.targetCache;
    END;
  END OSLGet;

PROCEDURE OSLGetOpenCount (list: OpenSchemeList): CARDINAL =
  BEGIN
    IF list # NIL THEN RETURN list^.count; ELSE RETURN 0; END;
  END OSLGetOpenCount;

PROCEDURE OSLSetOpenCount (list: OpenSchemeList; count: CARDINAL) =
  BEGIN
    IF list # NIL THEN list^.count := count; END;
  END OSLSetOpenCount;

PROCEDURE OSLGetGraph (list: OpenSchemeList): ChgMgmtGraph.T =
  BEGIN
    IF list # NIL THEN RETURN list^.graph; ELSE RETURN NIL; END;
  END OSLGetGraph;

PROCEDURE OSLFind (    list: OpenSchemeList;
                       name: Pathname.T;
                   VAR hdl : OpenSchemeList  ): BOOLEAN =
  BEGIN
    hdl := list;
    WHILE hdl # NIL AND NOT Text.Equal(hdl^.name, name) DO
      hdl := hdl^.next;
    END;
    RETURN hdl # NIL;
  END OSLFind;

PROCEDURE OSLFindGraph (    list : OpenSchemeList;
                            graph: ChgMgmtGraph.T;
                        VAR hdl  : OpenSchemeList  ): BOOLEAN =
  BEGIN
    hdl := list;
    WHILE hdl # NIL AND hdl^.graph # graph DO hdl := hdl^.next; END;
    RETURN hdl # NIL;
  END OSLFindGraph;

PROCEDURE OSLInsert (VAR list       : OpenSchemeList;
                         graph      : ChgMgmtGraph.T;
                         name       : Pathname.T;
                         idCache    : IdCache.T;
                         nameCache  : NameCache.T;
                         labelCache : LabelCache.T;
                         attrCache  : AttributeCache.T;
                         sourceCache: SourceCache.T;
                         targetCache: TargetCache.T     ) =
  BEGIN
    list :=
      NEW(OpenSchemeList, graph := graph, name := name, idCache := idCache,
          labelCache := labelCache, attrCache := attrCache,
          nameCache := nameCache, sourceCache := sourceCache,
          targetCache := targetCache, next := list, count := 1);
  END OSLInsert;

PROCEDURE OSLRemove (VAR list: OpenSchemeList; graph: ChgMgmtGraph.T) =
  VAR h: OpenSchemeList;
  BEGIN
    IF list^.graph = graph THEN
      list := list^.next;
    ELSE
      h := list;
      WHILE h^.next # NIL AND h^.next^.graph # graph DO h := h^.next; END;
      IF h^.next # NIL THEN h^.next := h^.next^.next; END;
    END;
  END OSLRemove;

(* END LOCAL ADT OpenSchemeList *)

(* INTERFACE LOCAL ADT CallbackList *)
TYPE
  CallbackList = REF CallbackInfo;
  CallbackInfo = RECORD
                   graph    : ChgMgmtGraph.T;
                   callbacks: CallbackSuite;
                   next     : CallbackList;
                 END;

PROCEDURE CBLInit (VAR list: CallbackList) =
  BEGIN
    list := NIL;
  END CBLInit;

PROCEDURE CBLFirst (list: CallbackList): CallbackList =
  BEGIN
    RETURN list;
  END CBLFirst;

PROCEDURE CBLNext (list: CallbackList): CallbackList =
  BEGIN
    IF list # NIL THEN RETURN list^.next; ELSE RETURN NIL; END;
  END CBLNext;

PROCEDURE CBLGet (    list     : CallbackList;
                  VAR graph    : ChgMgmtGraph.T;
                  VAR callbacks: CallbackSuite   ) =
  BEGIN
    IF list # NIL THEN
      graph := list^.graph;
      callbacks := list^.callbacks;
    END;
  END CBLGet;

PROCEDURE CBLInsert (VAR list     : CallbackList;
                         graph    : ChgMgmtGraph.T;
                         callbacks: CallbackSuite   ) =
  BEGIN
    list := NEW(CallbackList, graph := graph, callbacks := callbacks,
                next := list);
  END CBLInsert;

PROCEDURE CBLRemove (VAR list: CallbackList; graph: ChgMgmtGraph.T) =
  VAR h: CallbackList;
  BEGIN
    IF list^.graph = graph THEN
      list := list^.next;
    ELSE
      h := list;
      WHILE h^.next # NIL AND h^.next^.graph # graph DO h := h^.next; END;
      IF h^.next # NIL THEN h^.next := h^.next^.next; END;
    END;
  END CBLRemove;

(* END LOCAL ADT CallbackList *)

BEGIN
END TypedGraphPool.
