INTERFACE PersistentGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.5  1998/03/18 13:39:31  roland
    More slight modifications to local parameters (default values and
    parameter ordering)

    Revision 1.4  1998/03/17 14:14:17  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.3  1997/10/31 14:20:32  roland
    Adapted to new RuleEngine.

    Revision 1.2  1997/04/23 14:33:54  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:31  roland
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

*)
(***************************************************************************)

(* A GraphPool.T is a collection of graphs that logically belong together.
   Before accessing the graphs of a graph pool, an application must open
   the pool.  Afterwards, every access to a graph of the pool has to be
   enclosed in a transaction (on the pool). *)

IMPORT VirtualResource AS Super;
IMPORT Access, PageFile, ClientInfoSeq, TextCursorSet, Names;
IMPORT Pathname, AtomList, TextSeq;

TYPE

  T <: Public;

  Public = Super.T OBJECT
           METHODS
             (* pool administration *)
             open (name: Pathname.T; access: Access.Mode; new: BOOLEAN): T
                   RAISES {InternalError, Access.Denied, PageFile.NoAccess,
                           Access.Locked};

             close () RAISES {InternalError};


             (* other support *)
             beginTransaction  () RAISES {InternalError};
             commitTransaction () RAISES {InternalError, NotInTransaction};
             abortTransaction  () RAISES {InternalError, NotInTransaction};

             getNames (): Names.T;
                       (* Every pool maintains a persistent collection of
                          names.  This method returns a handle to this
                          collection *)

             (* The following operation act on local and remote graphs *)
             deleteGraph (name: Pathname.T; local: BOOLEAN := FALSE)
                          RAISES {InUse, NotExistent, InternalError,
                                  Access.Locked};

             copyGraph (sourceName: Pathname.T;
                        destName  : Pathname.T;
                        embedded  : BOOLEAN;
                        local     : BOOLEAN      := FALSE)
                        RAISES {InUse, Existent, NotExistent,
                                InternalError, Access.Locked};
                        (* Copy sourceName to destName.  If embedded, all
                           external relations of sourceName are copied,
                           too. *)

             copyGraphs (sources     : TextSeq.T;
                         destinations: TextSeq.T;
                         embedded    : BOOLEAN;
                         local       : BOOLEAN     := FALSE)
                         RAISES {InUse, Existent, NotExistent,
                                 InternalError, Access.Locked};
                         (* Copy all graphs in sources together with all
                            external relations between them to
                            destinations.  That means, for each 0 <= i <=
                            sources.size()-1, copy sources.get(i) to
                            destinations.get(i).  So destinations must
                            contain at least as many names as sources.  If
                            embedded, all external relations of sources
                            that lead to graphs that are not in sources are
                            copied, too.  If one of sources does not exists
                            (NotExistent), one of destinations exists
                            (Existent), or destinations.size() <
                            sources.size() (NotExistent) this will fail. *)

             renameGraph (oldName: Pathname.T;
                          newName: Pathname.T;
                          local  : BOOLEAN      := FALSE)
                          RAISES {InUse, Existent, NotExistent,
                                  InternalError, Access.Locked};

             existsGraph (name: Pathname.T; local: BOOLEAN := FALSE):
                          BOOLEAN RAISES {InternalError, Access.Locked};

             graphInUse (name: Pathname.T; local: BOOLEAN := FALSE):
                         BOOLEAN RAISES {InternalError};

             graphNumber (name: Pathname.T; local: BOOLEAN := FALSE):
                          CARDINAL RAISES {InternalError, NotExistent,
                                           Access.Locked};
                          (* for remote graphs only *)
             getGraphUser (name: Pathname.T): ClientInfoSeq.T
                           RAISES {InternalError};

             getGraphs (local: BOOLEAN := FALSE): TextCursorSet.T
                        RAISES {InternalError, Access.Locked};

             getNeighbours (graph: Pathname.T; local: BOOLEAN := FALSE):
                            TextCursorSet.T
                            RAISES {NotExistent, InternalError,
                                    Access.Locked};
                            (* Return the name of all graphs to which
                               'graph' has external relations. *)
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
END PersistentGraphPool.
