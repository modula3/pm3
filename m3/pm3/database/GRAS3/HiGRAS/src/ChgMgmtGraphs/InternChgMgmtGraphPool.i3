INTERFACE InternChgMgmtGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.5  1998/05/19 10:17:35  roland
    Support for log-groups implemented.

    Revision 1.4  1998/03/18 09:27:11  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.3  1998/03/17 14:13:56  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.2  1997/04/24 14:29:18  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 14:09:51  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

*)
(***************************************************************************)

IMPORT ChgMgmtGraphPool, ChgMgmtNames, PersistentGraph, Log, PageFile,
       Access, ChgMgmtOpenGraphs;
IMPORT Pathname;

REVEAL ChgMgmtGraphPool.T <: Internal;

TYPE
  Internal =
    ChgMgmtGraphPool.Public OBJECT
    METHODS
      openIntern (name: Pathname.T; access: Access.Mode; new: BOOLEAN):
                  ChgMgmtGraphPool.T
                  RAISES {Access.Denied, PageFile.NoAccess};
                  (* Used for opening a pool through the inheritance
                     hierarchie *)

      loginToNames (names: ChgMgmtNames.T)
                    RAISES {Access.Locked, ChgMgmtGraphPool.InternalError};
                    (* Used for opening a pool through the inheritance
                       hierarchie *)

      openPG (baseName   : Pathname.T;
              access     : PersistentGraph.AccessMode;
              errorChecks: BOOLEAN;
              local      : BOOLEAN;
              graph      : PersistentGraph.T           ): PersistentGraph.T
              RAISES {ChgMgmtGraphPool.InternalError, PageFile.NoAccess,
                      Access.Locked, Access.Denied};
              (* Open and register graph in pool.  If graph exists it will
                 be opened.  If not, it will be created. *)

      closePG (graph: PersistentGraph.T)
               RAISES {ChgMgmtGraphPool.InternalError};
               (* close graph and the according logs. *)

      bindGraphToLog (graph: CARDINAL; loghandle: CARDINAL);

      openLog (    graph, logGroup: Pathname.T;
                   local          : BOOLEAN;
                   access         : PersistentGraph.AccessMode;
               VAR mode           : ChgMgmtGraphPool.LogMode    ): CARDINAL
               RAISES {ChgMgmtGraphPool.InternalError, Access.Locked,
                       PageFile.NoAccess, Access.Denied};
               (* Ensure that a log for graph is open.  If logGroup = NIL,
                  graph gets a separate log.  Otherwise the log of the
                  logGroup is opened.  Logs will be created/opened
                  according to log mode.  If log#LogMode.None, the number
                  returned must be used to access the forward and backward
                  log of the graph.  logMode returns the actual logmode for
                  the graph.  This may differ from log due to access of
                  other clients in different modes. *)

      closeLog (loghandle: CARDINAL; keep: BOOLEAN)
                RAISES {ChgMgmtGraphPool.InternalError};
                (* Close the log belonging to loghandle if no one uses
                   it. *)

      getLog (loghandle: CARDINAL): Log.T;
              (* Get the current lock for loghandle *)

      getOpenGraphs(): ChgMgmtOpenGraphs.T;

      sameLogGroup (g1, g2: CARDINAL): BOOLEAN
                    RAISES {Access.Locked, ChgMgmtGraphPool.InternalError};
                    (* Check whether g1 and g2 belong to the same log
                       group *)
    END;

END InternChgMgmtGraphPool.
