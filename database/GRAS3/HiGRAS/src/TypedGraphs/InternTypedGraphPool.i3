INTERFACE InternTypedGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.5  1998/03/18 09:27:20  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.4  1998/03/17 14:14:25  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.3  1998/02/16 12:04:19  roland
    Introduced a two-phase commit to coordinate graph commits.

    Revision 1.2  1997/07/07 15:40:18  roland
    Added caches for relations in a Scheme and for node types in a typed
    graph.

    Revision 1.1  1997/05/01 13:23:13  roland
    TypedGraph layer adapted to graph boundary crossing edges.

*)
(***************************************************************************)

IMPORT PageFile, Access;
IMPORT ChgMgmtGraph, TypedGraphPool, Pathname;
IMPORT TypedNames;
IMPORT IdCache, NameCache, AttributeCache, LabelCache, SourceCache,
       TargetCache;

REVEAL TypedGraphPool.T <: Internal;

TYPE
  BeginTransactionProc =
    PROCEDURE (graph: ChgMgmtGraph.T) RAISES {ChgMgmtGraph.InternalError};

  CommitTransactionProc = PROCEDURE (graph: ChgMgmtGraph.T) RAISES {};

  PrepareCommitTransactionProc =
    PROCEDURE (graph: ChgMgmtGraph.T)
      RAISES {ChgMgmtGraph.InternalError, TypedGraphPool.CardinalityError};

  AbortTransactionProc =
    PROCEDURE (graph: ChgMgmtGraph.T) RAISES {ChgMgmtGraph.InternalError};

  CallbackSuite = RECORD
                    begin  : BeginTransactionProc;
                    prepare: PrepareCommitTransactionProc;
                    commit : CommitTransactionProc;
                    abort  : AbortTransactionProc;
                  END;

  Internal =
    TypedGraphPool.Public OBJECT
    METHODS
      openIntern (name: Pathname.T; access: Access.Mode; new: BOOLEAN):
                  TypedGraphPool.T
                  RAISES {Access.Denied, PageFile.NoAccess};
                  (* Used for opening a pool through the inheritance
                     hierarchie *)

      loginToNames (names: TypedNames.T)
                    RAISES {TypedGraphPool.InternalError, Access.Locked};
                    (* Used for opening a pool through the inheritance
                       hierarchie *)

      openCG (    graph       : ChgMgmtGraph.T;
                  baseName    : Pathname.T;
                  access      : ChgMgmtGraph.AccessMode;
                  errorChecks : BOOLEAN;
                  local       : BOOLEAN;
                  log         : TypedGraphPool.LogMode;
                  scheme      : Pathname.T;
                  callbacks   : CallbackSuite;
              VAR schemeNumber: CARDINAL                 ): ChgMgmtGraph.T
              RAISES {TypedGraphPool.InternalError, PageFile.NoAccess,
                      Access.Locked, Access.Denied, TypedGraphPool.InUse,
                      TypedGraphPool.NotExistent};
              (* Open and register graph in pool.  If graph exists it will
                 be opened.  If not, it will be created. *)

      closeCG (graph: ChgMgmtGraph.T; keepLog: BOOLEAN)
               RAISES {TypedGraphPool.InternalError};
               (* close graph and the according logs. *)

      openCGForScheme (VAR graph     : ChgMgmtGraph.T;
                           schemeName: Pathname.T;
                           local     : BOOLEAN                   := FALSE;
                           access    : ChgMgmtGraph.AccessMode;
                           errorChecks: BOOLEAN;
                           log        : ChgMgmtGraph.LogMode;
                       VAR idCache    : IdCache.T;
                       VAR nameCache  : NameCache.T;
                       VAR attrCache  : AttributeCache.T;
                       VAR labelCache : LabelCache.T;
                       VAR sourceCache: SourceCache.T;
                       VAR targetCache: TargetCache.T         )
                       RAISES {TypedGraphPool.InternalError,
                               PageFile.NoAccess, Access.Locked,
                               Access.Denied, TypedGraphPool.NoScheme,
                               TypedGraphPool.NotExistent,
                               TypedGraphPool.InUse};

      closeCGForScheme (graph: ChgMgmtGraph.T)
                        RAISES {TypedGraphPool.InternalError};


    END;

END InternTypedGraphPool.
