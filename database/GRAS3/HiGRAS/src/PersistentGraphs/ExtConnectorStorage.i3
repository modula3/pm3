INTERFACE ExtConnectorStorage;

(***************************************************************************)
(* External connectors are directed connections between entity numbers of
   different databases which can be traversed in both directions.  Each
   connector has a "type" identified by a number.  Note that the sources
   and targets of connectors are mere numbers, they have no relation to any
   entity in EntityStorages. *)
(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.2  1997/04/24 14:32:19  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/03/26 11:39:08  roland
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

IMPORT AtomList, Pathname;
IMPORT Node, NodeSet, Access, NodeLabelRelation, VirtualResource, PageFile,
       CardSet;

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      open (resource: VirtualResource.T;
            name    : Pathname.T;
            access  : Access.Mode;
            new     : BOOLEAN;
            local   : BOOLEAN;
            db1, db2: CARDINAL           ): T
            RAISES {PageFile.NoAccess, InternalError, Access.Locked,
                    Access.Denied};
            (* Open an external connector storage for the connectors
               between databases db1 and db2 (order is not important). *)
      close () RAISES {InternalError};

      putConnector (source, target: Node.T; type: CARDINAL)
                    RAISES {NotOwner, Access.Locked, InternalError};
                    (* Store a connector of the given type between source
                       and target entities. *)


      deleteConnector (source, target: Node.T; type: CARDINAL)
                       RAISES {NotOwner, Access.Locked, RelationNotFound,
                               InternalError};
                       (* Remove a connector of the given type between
                          source and target entities. *)


      deleteConnectorsByEntity (    entity           : Node.T;
                                VAR forward, backward: NodeLabelRelation.T)
                                RAISES {NotOwner, Access.Locked,
                                        RelationNotFound, InternalError};
                                (* Deletes all connectors connected to
                                   entity in either direction.  All deleted
                                   connectors are returned with their type
                                   and target entity. *)


      areConnected (source, target: Node.T; type: CARDINAL): BOOLEAN
                    RAISES {NotOwner, Access.Locked, InternalError};
                    (* Returns TRUE if source and target are connected by a
                       connector of the given type. *)


      getTargets (source: Node.T; type: CARDINAL): NodeSet.T
                  RAISES {NotOwner, Access.Locked, InternalError};
                  (* Returns all targets connected to source by a connector
                     of the given type. *)


      getSources (target: Node.T; type: CARDINAL): NodeSet.T
                  RAISES {NotOwner, Access.Locked, InternalError};
                  (* Returns all sources connected to target by a connector
                     of the given type. *)


      getAllTargets (source: Node.T): NodeSet.T
                     RAISES {NotOwner, Access.Locked, InternalError};
                     (* Returns all target entities connected to source. *)


      getAllSources (target: Node.T): NodeSet.T
                     RAISES {NotOwner, Access.Locked, InternalError};
                     (* Returns all source entities connected to target. *)


      getAllOutTypes (source: Node.T): CardSet.T
                      RAISES {NotOwner, Access.Locked, InternalError};
                      (* Get all types of connectors starting at source. *)


      getAllInTypes (target: Node.T): CardSet.T
                     RAISES {NotOwner, Access.Locked, InternalError};
                     (* Get all types of connectors ending at target. *)


      getAllOutConnectors (source: Node.T): NodeLabelRelation.T
                           RAISES {NotOwner, Access.Locked, InternalError};
                           (* Returns type and target for each connector
                              starting at source. *)


      getAllInConnectors (target: Node.T): NodeLabelRelation.T
                          RAISES {NotOwner, Access.Locked, InternalError};
                          (* Returns type and source for each connector
                             ending at target. *)
    END;

EXCEPTION
  InternalError(AtomList.T);
  RelationNotFound;
  NotOwner;

END ExtConnectorStorage.
