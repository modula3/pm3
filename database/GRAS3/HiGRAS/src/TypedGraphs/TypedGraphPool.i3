INTERFACE TypedGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:35  hosking
    Initial revision

    Revision 1.7  1998/03/18 13:39:45  roland
    More slight modifications to local parameters (default values and
    parameter ordering)

    Revision 1.6  1998/03/18 12:13:27  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.5  1998/03/18 09:27:27  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.4  1998/03/17 14:14:33  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.3  1997/10/31 14:23:29  roland
    Adapted to new RuleEngine.

    Revision 1.2  1997/10/24 14:10:33  renehuel
    New procedure 'CopyScheme' to copy a scheme from one pool to another.

    Revision 1.1  1997/05/01 13:23:21  roland
    TypedGraph layer adapted to graph boundary crossing edges.

*)
(***************************************************************************)


(* This abstract data type enhances ChgMgmtGraphPool with the notion of
   schemes.  Graphs and schemes share a common name space. *)

IMPORT Pathname, TextCursorSet, PageFile, ClientInfoSeq, Access;
IMPORT AtomList;
IMPORT ChgMgmtGraphPool AS Super;

TYPE
  LogMode = Super.LogMode;

  T <: Public;

  Public = Super.T OBJECT
           METHODS
             (* pool administration *)
             open (name: Pathname.T; access: Access.Mode; new: BOOLEAN): T
                   RAISES {InternalError, Access.Denied, Access.Locked,
                           PageFile.NoAccess};

             close () RAISES {InternalError};


             (* other support *)
             beginTransaction () RAISES {InternalError};
             commitTransaction () RAISES {InternalError, NotInTransaction,
                                          CardinalityError};
             abortTransaction () RAISES {InternalError, NotInTransaction};

             (* operations on schemes *)

             deleteScheme (name: Pathname.T; local: BOOLEAN := FALSE)
                           RAISES {InUse, NoScheme, NotExistent,
                                   InternalError, Access.Locked};
                           (* Deltes the scheme name in the collection.  If
                              the scheme still serves as type for some
                              graph in the collection, NoScheme will be
                              raised.  InUse is raised if either the scheme
                              is opened by some client or graphs exists
                              which use the scheme for their type
                              information. *)

             copyScheme (sourceName: Pathname.T;
                         destName  : Pathname.T;
                         local     : BOOLEAN      := FALSE)
                         RAISES {NoScheme, NotExistent, InternalError,
                                 Access.Locked, Existent, InUse};
                         (* Create a copy of the source scheme.  This
                            operation will fail if the destination name
                            points to an already existing scheme/graph. *)

             renameScheme (oldName: Pathname.T;
                           newName: Pathname.T;
                           local  : BOOLEAN      := FALSE)
                           RAISES {NoScheme, NotExistent, InternalError,
                                   Access.Locked, Existent, InUse};
                           (* Changes the scheme to be refered by oldName
                              to be found now as newName.  This operation
                              will fail if newName is an already existing
                              name. *)

             existsScheme (baseName: Pathname.T; local: BOOLEAN := FALSE):
                           BOOLEAN RAISES {Access.Locked, InternalError};
                           (* Tests if the specified graph exists. *)

             getSchemes (local: BOOLEAN := FALSE): TextCursorSet.T
                         RAISES {InternalError, Access.Locked};
                         (* Returns a name list of all managed graphs. *)

             hasScheme (graph: Pathname.T; local: BOOLEAN := FALSE):
                        BOOLEAN RAISES {InternalError, NoGraph,
                                        Access.Locked, NotExistent};
                        (* Test whether graph has a scheme *)

             getScheme (graph: Pathname.T; local: BOOLEAN := FALSE):
                        Pathname.T
                        RAISES {InternalError, NoScheme, NoGraph,
                                NotExistent, Access.Locked};
                        (* Get a graphs scheme. *)

             getGraphsWithScheme (scheme: Pathname.T;
                                  local : BOOLEAN      := FALSE):
                                  TextCursorSet.T
                                  RAISES {InternalError, NoScheme,
                                          Access.Locked, NotExistent};
                                  (* Return all graphs connected to
                                     scheme. *)

             getVersion (scheme: Pathname.T; local: BOOLEAN := FALSE):
                         CARDINAL
                         RAISES {InternalError, NoScheme, Access.Locked};
                         (* Get the version number of scheme *)

             (* operation for schemes and graphs *)

             inUse (name: Pathname.T; local: BOOLEAN := FALSE): BOOLEAN
                    RAISES {InternalError};

             getUser (name: Pathname.T): ClientInfoSeq.T
                      RAISES {InternalError};

             (* The following operation act on graphs only *)
             existsGraph (baseName: Pathname.T; local: BOOLEAN := FALSE):
                          BOOLEAN RAISES {Access.Locked, InternalError};

             deleteGraph (name: Pathname.T; local: BOOLEAN := FALSE)
                          RAISES {NoGraph, Access.Locked, InUse,
                                  NotExistent, InternalError};

             copyGraph (sourceName: Pathname.T;
                        destName  : Pathname.T;
                        embedded  : BOOLEAN;
                        local     : BOOLEAN      := FALSE)
                        RAISES {Access.Locked, InUse, Existent,
                                NotExistent, InternalError, NoGraph};
                        (* Copy sourceName to destName.  If embedded, all
                           external relations of sourceName are copied,
                           too. *)

             renameGraph (oldName: Pathname.T;
                          newName: Pathname.T;
                          local  : BOOLEAN      := FALSE)
                          RAISES {Access.Locked, InUse, Existent,
                                  NotExistent, NoGraph, InternalError};

             getGraphs (local: BOOLEAN := FALSE): TextCursorSet.T
                        RAISES {Access.Locked, InternalError};
           END;

PROCEDURE CopyGraph (sourcePool : T;
                     sourceGraph: Pathname.T;
                     sourcelocal: BOOLEAN;
                     targetPool : T;
                     targetGraph: Pathname.T;
                     targetlocal: BOOLEAN  )
  RAISES {InUse, NotExistent, Existent, InTransaction, InternalError,
          Access.Locked};
  (* Copy the graph with name 'sourceGraph' of pool 'sourcePool' to pool
     'targetPool' and name the copy 'targetGraph'. *)

PROCEDURE CopyScheme (sourcePool  : T;
                      sourceScheme: Pathname.T;
                      sourcelocal : BOOLEAN;
                      targetPool  : T;
                      targetScheme: Pathname.T;
                      targetlocal : BOOLEAN  )
  RAISES {InUse, NotExistent, Existent, InTransaction, InternalError,
          Access.Locked};
  (* Copy the scheme with name 'sourceScheme' of pool 'sourcePool' to pool
     'targetPool' and name the copy 'targetScheme'. *)


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
                                    which are not used (openk) by any
                                    clients. *)
  NoCheckOutCopy;

  NoScheme;
  NoGraph;

  CardinalityError(CARDINAL);    (* If a graph detects a cardinality error
                                    during transaction commit, it reports
                                    it with this exception.  The number is
                                    the pool number of the faulting
                                    graph.*)

END TypedGraphPool.
