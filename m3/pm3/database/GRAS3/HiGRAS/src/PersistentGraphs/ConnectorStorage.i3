INTERFACE ConnectorStorage;

(***************************************************************************)
(* Connectors are directed connections between entity numbers which can be
   traversed in both directions.  Each connector has a "type" identified by
   a number.  Note that the sources and targets of connectors are mere
   numbers, they have no relation to any entity in EntityStorage. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:38:58  roland
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

    Revision 1.4  1996/11/20 12:22:49  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/17 13:08:59  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.2  1996/08/06 16:26:24  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/07/24 09:19:52  rbnix
    	Error handling adjusted: internal errors are now guarded by
    	assertions rather than exceptions. This should simplify
    	locating errors.

    Revision 1.1.2.1  1996/04/29 13:43:31  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.1  1994/01/20  18:41:17  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT AtomList;
IMPORT
  CardSet,
  Access,
  ITPFile,
  CardRelation;

CONST IndexTree = 3;

PROCEDURE PutConnector (file          : ITPFile.T;
                        source, target: CARDINAL;
                        type          : CARDINAL   )
		       RAISES {Access.Locked, InternalError};
  (* Store a connector of the given type between source and target
     entities. *)


PROCEDURE DeleteConnector (file          : ITPFile.T;
                           source, target: CARDINAL;
                           type          : CARDINAL   )
  RAISES {Access.Locked, RelationNotFound, InternalError};
  (* Remove a connector of the given type between source and target
     entities. *)


PROCEDURE DeleteConnectorsByEntity (    file             : ITPFile.T;
                                        entity           : CARDINAL;
                                    VAR forward, backward: CardRelation.T )
  RAISES {Access.Locked, RelationNotFound, InternalError};
  (* Deletes all connectors connected to entity in either direction.  All
     deleted connectors are returned with their type and target entity. *)


PROCEDURE AreConnected (file          : ITPFile.T;
                        source, target: CARDINAL;
                        type          : CARDINAL   ): BOOLEAN
  RAISES {Access.Locked, InternalError};
  (* Returns TRUE if source and target are connected by a connector of the
     given type. *)


PROCEDURE GetTargets (file: ITPFile.T; source: CARDINAL; type: CARDINAL):
  CardSet.T RAISES {Access.Locked, InternalError};
  (* Returns all targets connected to source by a connector of the given
     type. *)


PROCEDURE GetSources (file: ITPFile.T; target: CARDINAL; type: CARDINAL):
  CardSet.T RAISES {Access.Locked, InternalError};
  (* Returns all sources connected to target by a connector of the given
     type. *)


PROCEDURE GetAllTargets (file: ITPFile.T; source: CARDINAL):
  CardSet.T RAISES {Access.Locked, InternalError};
  (* Returns all target entities connected to source. *)


PROCEDURE GetAllSources (file: ITPFile.T; target: CARDINAL):
  CardSet.T RAISES {Access.Locked, InternalError};
  (* Returns all source entities connected to target. *)


PROCEDURE GetAllOutTypes (file: ITPFile.T; source: CARDINAL):
  CardSet.T RAISES {Access.Locked, InternalError};
  (* Get all types of connectors starting at source. *)


PROCEDURE GetAllInTypes (file: ITPFile.T; target: CARDINAL):
  CardSet.T RAISES {Access.Locked, InternalError};
  (* Get all types of connectors ending at target. *)


PROCEDURE GetAllOutConnectors (file: ITPFile.T; source: CARDINAL):
  CardRelation.T RAISES {Access.Locked, InternalError};
  (* Returns type and target for each connector starting at source. *)


PROCEDURE GetAllInConnectors (file: ITPFile.T; target: CARDINAL):
  CardRelation.T RAISES {Access.Locked, InternalError};
  (* Returns type and source for each connector ending at target. *)

EXCEPTION
  InternalError(AtomList.T);
  RelationNotFound;
  
END ConnectorStorage.
