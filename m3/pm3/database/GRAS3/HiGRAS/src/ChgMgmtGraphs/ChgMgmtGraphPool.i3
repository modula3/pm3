INTERFACE ChgMgmtGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.8  1998/05/19 10:17:28  roland
    Support for log-groups implemented.

    Revision 1.7  1998/03/18 13:39:18  roland
    More slight modifications to local parameters (default values and
    parameter ordering)

    Revision 1.6  1998/03/18 12:13:14  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.5  1998/03/18 09:27:08  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.4  1998/03/17 14:13:52  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.3  1997/10/31 14:22:30  roland
    Adapted to new RuleEngine.

    Revision 1.2  1997/05/01 13:21:04  roland
    Changed raises sets of transaction procedures. Pretty printing.

    Revision 1.1  1997/04/23 14:09:16  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

*)
(***************************************************************************)


(* This abstract data type implements a collection for graphs that may be
   related with each other by deltas.  These graphs can be directly or
   indirectly accessible.  Indirectly accessible graphs are not kept as
   physical files.  They have to be reconstructed from other, directly
   accessible graphs using of command logs (deltas).  With this mechanism,
   two graphs, which are connected with a delta, need not be stored both.
   To "produce" indirect graphs, one should apply the command
   DeltaCopyGraph to a graph.  Invisible graphs represent graph states that
   were deleted but that are still necessary to reconstruct other,
   indirectly accessible graphs.  They are not accessible to users of
   ChgMgmtGraph but only within this subsystem (and below).

   To keep information about graphs and deltas, a special graph ".GRAS" is
   created.  It is handled as a PersistentGraph.T.  This implements a
   similar functionality as PeristentGraphPool.  Though, graphs visible
   there might not be accessible here (invisible graphs) and also graphs
   visible here might not exist in PersistentGraphSystem (indirectly
   accessible graphs).

   Information about new graphs is promoted to this module by ChgMgmtGraph.
   This module does not open or create the graphs by itself, but merely
   does the bookkeeping.

   Like in PersistentGraphPool, the following operations are only allowed,
   if the graphs concerned are not use by anyone: 1.) CopyGraph to make one
   (complete) copy of the actual graph 2.) CheckOutGraph, to create a
   working copy of a graph.  3.) CheckInGraph to replace the original graph
   with the working-copy

   There are also some operations to get informations about the
   administration. *)

IMPORT Pathname, TextCursorSet, PageFile, ClientInfoSeq, Access;
IMPORT AtomList;
IMPORT PersistentGraphPool AS Super;

TYPE
  LogMode = {None, Linear, Tree};

  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      (* pool administration *)
      open (name: Pathname.T; access: Access.Mode; new: BOOLEAN): T
            RAISES {Access.Locked, InternalError, Access.Denied,
                    PageFile.NoAccess};

      close () RAISES {InternalError};


      (* other support *)
      beginTransaction  () RAISES {InternalError};
      commitTransaction () RAISES {InternalError, NotInTransaction};
      abortTransaction  () RAISES {InternalError, NotInTransaction};

      deleteGraph (name: Pathname.T; local: BOOLEAN := FALSE)
                   RAISES {Access.Locked, InUse, NotExistent, InternalError};

      copyGraph (sourceName: Pathname.T;
                 destName  : Pathname.T;
                 embedded  : BOOLEAN;
                 local     : BOOLEAN      := FALSE)
                 RAISES {Access.Locked, InUse, Existent, NotExistent,
                         InternalError};
                 (* Copy sourceName to destName.  If embedded, all external
                    relations of sourceName are copied, too. *)

      renameGraph (oldName: Pathname.T;
                   newName: Pathname.T;
                   local  : BOOLEAN      := FALSE)
                   RAISES {Access.Locked, InUse, Existent, NotExistent,
                           InternalError};

      existsGraph (name: Pathname.T; local: BOOLEAN := FALSE): BOOLEAN
                   RAISES {Access.Locked, InternalError};

      graphNumber (name: Pathname.T; local: BOOLEAN := FALSE): CARDINAL
                   RAISES {Access.Locked, InternalError, NotExistent};

      graphInUse (name: Pathname.T; local: BOOLEAN := FALSE): BOOLEAN
                  RAISES {InternalError};

      getGraphUser (name: Pathname.T): ClientInfoSeq.T
                    RAISES {InternalError};

      getGraphs (local: BOOLEAN := FALSE): TextCursorSet.T
                 RAISES {Access.Locked, InternalError};

      getNeighbours (graph: Pathname.T; local: BOOLEAN := FALSE):
                     TextCursorSet.T
                     RAISES {Access.Locked, NotExistent, InternalError};
                     (* Return the name of all graphs to which 'graph' has
                        external relations. *)

      (* LogGroups are the units of logging in GRAS.  For each log group
         exists one log which records all operations on all memebers of the
         group.  If a graph does not belong to a log group, it belongs to
         an implicit group containing only the graph. *)
      createLogGroup (groupname: Pathname.T; local: BOOLEAN := FALSE)
                      RAISES {Existent, Access.Locked, InternalError};
                      (* Create a new empty log group *)

      deleteLogGroup (groupname: Pathname.T; local: BOOLEAN := FALSE)
                      RAISES {InUse, Access.Locked, InternalError};
                      (* Delete a log group.  The group must be empty. *)

      existsLogGroup (groupname: Pathname.T; local: BOOLEAN := FALSE):
                      BOOLEAN RAISES {Access.Locked, InternalError};
                      (* Check for existence of log group. *)

      addToLogGroup (groupname, graph: Pathname.T; local: BOOLEAN := FALSE)
                     RAISES {NotExistent, NotAllowed, Access.Locked,
                             InternalError};
                     (* Add a graph to a log group.  The graph must not be
                        member of another group and must not have a log
                        already.  If a log for the group exists, the graph
                        must not be neighbor to any of the groups
                        members. *)

      removeFromLogGroup (groupname, graph: Pathname.T;
                          local           : BOOLEAN      := FALSE)
                          RAISES {NotExistent, NotAllowed, Access.Locked,
                                  InternalError};
                          (* Only allowed, if currently no log for the
                             group exists. *)

      logGroupMember (groupname: Pathname.T; local: BOOLEAN := FALSE):
                      TextCursorSet.T
                      RAISES {NotExistent, Access.Locked, InternalError};
                      (* Return all members of the group. *)

      getLogGroup (graph: Pathname.T; local: BOOLEAN := FALSE): Pathname.T
                   RAISES {NotExistent, Access.Locked, InternalError};
                   (* Return the log group of graph.  NIL if graph has no
                      group. *)

      checkOutGraph (name : Pathname.T;
                     as   : Pathname.T;
                     local: BOOLEAN      := FALSE)
                     RAISES {Access.Locked, InUse, NotExistent, Existent,
                             InternalError};
                     (* Produces a working-copy of the graph 'name'.  The
                        name of the copy will be as.  The copy will always
                        be directly accessible.  CheckOutGraph fails if
                        'as' already exists. *)

      checkInGraph (name: Pathname.T; local: BOOLEAN := FALSE)
                    RAISES {Access.Locked, InUse, NotExistent,
                            InternalError, NoCheckOutCopy};
                    (* Copy the graph 'name' to its original.  'name' must
                       be the name of graph created with CheckOutGraph *)

      deltaCopyGraph (source : Pathname.T;
                      target : Pathname.T;
                      forward: BOOLEAN;
                      local  : BOOLEAN      := FALSE)
                      RAISES {Access.Locked, InUse, Existent, NotExistent,
                              InternalError};
                      (* Copy the graph named 'source' to 'target'.  This
                         will not copy the actual graph, but make one of
                         them only indirectly accessible.  If forward is
                         TRUE, target will be indirect and can later be
                         reconstructed from source (by application of the
                         commands contained in deltaName).  Otherwise
                         (forward = FALSE) target will be directly
                         accessible (with current contents of source) and
                         the graph source can be reconstructed from
                         target. *)

      (* operations on the collection of graphs and deltas *)

      reorganisation () RAISES {Access.Locked, InternalError};
                      (* Reorganisation of the administration.  For example
                         the informations about deleted graphs, which are
                         no longer needed for the reconstruction of other
                         graphs, may be deleted.  This Operation can be
                         applied to the system at arbitrary times and time
                         intervalls.  It will only clean up bookkeeping
                         information and has no other effects on the
                         system. *)
    END;

PROCEDURE CopyGraph (sourcePool : T;
                     sourceGraph: Pathname.T;
                     sourcelocal: BOOLEAN;
                     targetPool : T;
                     targetGraph: Pathname.T;
                     targetlocal: BOOLEAN     )
  RAISES {InUse, NotExistent, Existent, InTransaction, InternalError,
          Access.Locked};
  (* Copy the graph with name 'sourceGraph' of pool 'sourcePool' to pool
     'targetPool' and name the copy 'targetGraph'. *)


EXCEPTION
  InternalError(AtomList.T);
  NotInTransaction;              (* Commit/AbortTransaction called outside
                                    a transaction *)
  InTransaction;                 (* Copying files between two pools is only
                                    possible outside of transactions *)
  Existent;                      (* Operation would overwrite an existing
                                    graph *)
  NotExistent;                   (* Operation tries to access a not
                                    existing graph *)
  InUse;                         (* The operation is only allowed on graphs
                                    which are not used (open) by any
                                    clients. *)
  NotAllowed;                    (* Operation is not allowed. *)
  NoCheckOutCopy;

END ChgMgmtGraphPool.
