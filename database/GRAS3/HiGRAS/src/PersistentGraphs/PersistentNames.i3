INTERFACE PersistentNames;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.3  1998/03/18 13:39:34  roland
    More slight modifications to local parameters (default values and
    parameter ordering)

    Revision 1.2  1998/03/17 14:14:19  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.1  1997/03/26 11:39:36  roland
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

(* A specialization of Names for PersistentGraphs. *)

IMPORT Pathname, TextCursorSet, Access;
IMPORT AtomList;
IMPORT Names AS Super;

TYPE
  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      insertGraph (name: Pathname.T; local: BOOLEAN := FALSE)
                   RAISES {Access.Locked, InternalError};
                   (* Insertion of a new graph with name 'name'. *)
      removeGraph (    name   : Pathname.T;
                       local  : BOOLEAN;
                   VAR extRels: TextCursorSet.T)
                   RAISES {Access.Locked, InternalError};
                   (* Deletion of the specified graph and all its external
                      relations from Names.  The set extRels contains all
                      external relations to which the graph was
                      connected. *)
      renameGraph (local: BOOLEAN := FALSE; old, new: Pathname.T)
                   RAISES {Access.Locked, InternalError, Unknown};
                   (* Change the name for graph old into new. *)
      copy (source, target: Pathname.T)
            RAISES {Access.Locked, InternalError};
            (* Creates a new graph with name target in Names. *)
      getGraphNumber (graph: Pathname.T; local: BOOLEAN): CARDINAL
                      RAISES {Access.Locked, InternalError, Unknown};
                      (* Returns a numeric id for graph *)
      getGraphName (number: CARDINAL; local: BOOLEAN): Pathname.T
                    RAISES {Access.Locked, InternalError, Unknown};
                    (* Returns the name of graph denoted by number *)

      insertExtRelation (graph1  : Pathname.T;
                         graph2  : Pathname.T;
                         local   : BOOLEAN;
                         relation: Pathname.T  )
                         RAISES {Access.Locked, InternalError, Unknown,
                                 Existent};
                         (* Insert an external relation between graph1 and
                            graph2 with name relation.  Two graphs might be
                            connected by at most one external relation *)

      existsExtRelation (graph1: Pathname.T;
                         graph2: Pathname.T;
                         local : BOOLEAN     ): BOOLEAN
                         RAISES {Access.Locked, InternalError, Unknown};
                         (* Test whether an external relation between
                            graph1 and graph2 exists *)

      getExternalRelation (graph1: Pathname.T;
                           graph2: Pathname.T;
                           local : BOOLEAN     ): Pathname.T
                           RAISES {Access.Locked, InternalError, Unknown};
                           (* Return the name of the external relation
                              between graph1 and graph2. *)

      getAllNeighbours (graph: Pathname.T; local: BOOLEAN): TextCursorSet.T
                        RAISES {Access.Locked, InternalError, Unknown};
                        (* Return all graphs g, such that between graph and
                           g exists an external relation *)

      uniqueName (): TEXT RAISES {Access.Locked, InternalError};
                  (* Returns a text containing a number which is unique for
                     this instance of Names *)

      (* --- Queries --- *)
      existsGraph (name: Pathname.T; local: BOOLEAN): BOOLEAN
                   RAISES {Access.Locked, InternalError};
                   (* returns TRUE, if the graph with name name exists *)

      getGraphs (local: BOOLEAN): TextCursorSet.T
                 RAISES {Access.Locked, InternalError};
                 (* returns the names of the graphs in administration that
                    conform to st. *)

    END;


EXCEPTION
  Unknown;
  Existent;
  InternalError(AtomList.T);

END PersistentNames.
