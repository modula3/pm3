INTERFACE PersistentGraph;

(***************************************************************************)
(* Object type for persistent, attributed, node- and edge-labelled
   graphs. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.6  1998/03/17 14:14:15  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.5  1998/03/09 14:56:39  roland
    New method local that returns true if the graph is a local graph.

    Revision 1.4  1998/01/21 12:35:42  roland
    New method baseName to determine filename.

    Revision 1.3  1997/04/24 14:32:41  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.2  1997/04/23 14:33:50  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:27  roland
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

    Revision 1.16  1997/01/31 10:24:32  roland
    Comments in PersistentGraph.
    Two more interface procedures in Relation.

    Revision 1.15  1996/11/22 16:06:20  roland
    Exception-Handling in PersistentGraph changed so that no exceptions of
    VirtualResource are passed to calling procedures.

    Revision 1.14  1996/11/20 12:23:06  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.13  1996/11/14 14:17:43  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Access in mode ReadOnlyShared is now considered when opening graphs.

    Revision 1.12  1996/09/17 13:09:08  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.11  1996/08/23 14:51:08  rbnix
        Administration operations moved to a new module.

    Revision 1.10  1996/08/06 16:26:36  roland
    Merge of PAGESERVER and main branch.

    Revision 1.8.2.3  1996/07/25 09:20:53  rbnix
        Methods open and create merged.

    Revision 1.8.2.2  1996/07/24 09:20:12  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.8.2.1  1996/04/29 13:43:48  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

    Revision 1.8  1995/11/28 09:49:26  grover
    added getPath

# Revision 1.7  1995/07/05  16:03:16  alex
# Add the method deleteAttribute to PersistentGraph
#
# Revision 1.6  1994/04/01  16:14:36  pk
# forward/backward parameters for deleteNode renamed to out/inEdges.
#
# Revision 1.5  1994/04/01  15:18:58  pk
# names parameter for deleteNode renamed to indexes.
#
# Revision 1.4  1994/04/01  15:09:58  pk
# New method errorCheckOn.
#
# Revision 1.3  1994/04/01  14:50:42  pk
# CreateEntity/Node returns the node number.
#
# Revision 1.2  1994/03/30  18:32:00  pk
# Adaptions for new Files subsystem.
#
# Revision 1.1  1994/01/20  18:41:37  pk
# Initial revision
#
*)
(***************************************************************************)
(*
 | --- PersistentGraph ----------------------------------------------------
 Each graph constitutes a database, an access mode and a transaction
 model.

 Graphs are refered by it's name, that is structured as and mapped to a
 directory path. A new graph can only be opened if its name points to a
 leaf of the formed path hierarchy.

 On Opening a graph some additional error checks can be turned on by the
 client. If consistency checks are active, the existance of nodes is
 checked before attribute, name and edge operations are executed. Note that
 inactive checks don't prevent *any* exceptions to be raised when a node
 does not exist, only additional checks mede in this module are turned off.
 | ------------------------------------------------------------------------
 *)
IMPORT PersistentGraphPool;
IMPORT Pathname, Access, CardSet, TaggedNameSet, PageFile,
       AttributeDescriptionSet, Node, NodeLabelRelation, NodeSet;
IMPORT AtomList;

TYPE
  AccessMode = {Inherit,             (* = take access mode of pool *)
                ReadWriteShared,
                ReadWriteExclusive,
                ReadOnlyShared};
  
  T <: Public;

  Public =
    OBJECT
    METHODS
      open (pool       : PersistentGraphPool.T;
            baseName   : Pathname.T;
            access     : AccessMode;
            new        : BOOLEAN;
            local      : BOOLEAN;
            errorChecks: BOOLEAN): T
            RAISES {NotExistent, InUse, Access.Locked, PageFile.NoAccess,
                    InternalError, Access.Denied};
            (* Opens the graph specified by baseName.  If new is set to
               TRUE a new graph will be created maybe destroying an old
               version.  Creating a graph will fail if access is set to a
               read only mode.  If new is set to FALSE an existing graph to
               open is expected. *)

      close () RAISES {InternalError};

      errorCheckOn (): BOOLEAN;
                    (* Returns TRUE if the error check flag is set. *)

      number (): CARDINAL;
              (* Return the number of the graph in its pool. *)

      local () : BOOLEAN;
              (* TRUE if graph was opened as local graph. *)
      
      baseName(): Pathname.T;
      (*
       | --- node operations ----------------------------------------------------
       Each graph may contain an "arbitrary" number of nodes. Each node
       carries a CARDINAL-typed label.
       | ------------------------------------------------------------------------
       *)
      createNodeNumber (neighbour: Node.T): Node.T
                        RAISES {NotOwner, Access.Locked, InternalError};
                        (* First step to create a node in the neighbourship
                           of the given node.  The new node number is
                           returned.  If neighbour does not exist, the new
                           node will get the neighbour number.  Note that
                           createNodeNumber must be followed immediately by
                           a createNode call. *)

      createNode (label: CARDINAL): Node.T
                  RAISES {Access.Locked, InternalError};
                  (* Creates the node with the number returned by
                     createNodeNumber and the given label.  This must be
                     immediately preceeded by createNodeNumber.  The node
                     number is returned again. *)

      ownsNode (node: Node.T): BOOLEAN;
                (* Returns TRUE, if the node could be owned by the graph,
                   i.e.  if the node.graph number corresponds to the graphs
                   number in the pool. *)

      deleteNode (    node      : Node.T;
                  VAR attributes: AttributeDescriptionSet.T;
                  VAR indexes   : TaggedNameSet.T;
                  VAR outEdges  : NodeLabelRelation.T;
                  VAR inEdges   : NodeLabelRelation.T        )
                  RAISES {NotOwner, Access.Locked, InternalError};
                  (* Deletes the given node.  The method returns the
                     attributes and indexes stored for the node and all
                     incident edges.  Edges are returned as (label, node)
                     tuples.  These attributes, names, and edges were
                     deleted with the node. *)

      deleteNodeNoInfo (node: Node.T)
                        RAISES {NotOwner, Access.Locked, InternalError};
                        (* Same as deleteNode, but no information abput
                           attributes, indexes, and edges is returned. *)

      existsNode (node: Node.T): BOOLEAN
                  RAISES {NotOwner, Access.Locked, InternalError};
                  (* Returns TRUE if node exists. *)

      putNodeLabel (node: Node.T; label: CARDINAL)
                    RAISES {NotOwner, Access.Locked, NodeNotFound,
                            InternalError};
                    (* Stores label at node. *)

      getNodeLabel (node: Node.T): CARDINAL
                    RAISES {NotOwner, Access.Locked, NodeNotFound,
                            InternalError};
                    (* Retrieves the label of node. *)


      (*
       | --- attribute operations -----------------------------------------------
       Each node may carry an "arbitrary" number of attributes identified
       by an attribute number. Each attribute may store an
       "arbitrary"-sized text.
       | ------------------------------------------------------------------------
       *)
      putAttribute (node       : Node.T;
                    attributeNo: CARDINAL;
                    start      : CARDINAL;
                    attribute  : TEXT      )
                    RAISES {NotOwner, Access.Locked, NodeNotFound,
                            InternalError};
                    (* Stores the attribute at attributeNo of the given
                       node starting at start. *)

      deleteAttribute (node: Node.T; attributeNo: CARDINAL)
                       RAISES {NotOwner, Access.Locked, NodeNotFound,
                               InternalError};
                       (* Deletes the attribute with attributeNo of the
                          given node. *)

      getAttribute (node       : Node.T;
                    attributeNo: CARDINAL;
                    start      : CARDINAL;
                    length     : CARDINAL  ): TEXT
                    RAISES {NotOwner, Access.Locked, NodeNotFound,
                            InternalError};
                    (* Read length bytes from the attribute at attributeNo
                       of the given entity node at start.  The returned
                       value might be shorter than length if less than
                       length bytes were stored. *)

      truncateAttribute (node       : Node.T;
                         attributeNo: CARDINAL;
                         size       : CARDINAL  )
                         RAISES {NotOwner, Access.Locked, NodeNotFound,
                                 InternalError};
                         (* The given attribute is truncated to size bytes.
                            The attribute must contain at least size bytes
                            before. *)

      getAllAttributeNumbers (node: Node.T): CardSet.T
                              RAISES {NotOwner, Access.Locked,
                                      NodeNotFound, InternalError};
                              (* Returns a set with all attribute numbers
                                 in use for the given node. *)


      (*
       | --- index attribute operations -----------------------------------------
        Each node may carry an "arbitrary" number of index attributes. An
        index attribute may contain a user-defined value as a TEXT of
        max. 255 bytes. Index attributes differ from "normal" attributes in
        that all nodes with a certain value for a certain index attribute
        number can be queried.
       | ------------------------------------------------------------------------
       *)
      putIndex (node: Node.T; indexNo: CARDINAL; index: TEXT)
                RAISES {NotOwner, Access.Locked, NodeNotFound, IndexUsed,
                        InternalError};
                (* Stores index as index attribute for the node. *)

      deleteIndex (node: Node.T; indexNo: CARDINAL; index: TEXT)
                   RAISES {NotOwner, Access.Locked, NodeNotFound,
                           IndexUnused, InternalError};
                   (* Delete the given index attribute. *)

      getIndex (node: Node.T; indexNo: CARDINAL; VAR found: BOOLEAN): TEXT
                RAISES {NotOwner, Access.Locked, NodeNotFound,
                        InternalError};
                (* Returns the value of the given index attribute. *)

      getNodesWithIndex (indexNo: CARDINAL; index: TEXT): NodeSet.T
                         RAISES {Access.Locked, InternalError};
                         (* Returns a set of all nodes with the given index
                            attribute. *)

      getIndexNosForNode (node: Node.T): CardSet.T
                          RAISES {NotOwner, Access.Locked, NodeNotFound,
                                  InternalError};
                          (* Returns a set of all index numbers which
                             contain an index for node. *)


      (*
       | --- edge operations ----------------------------------------------------
       Nodes may be connected by edges. Each edge has a label stored as a
       CARDINAL number.
       | ------------------------------------------------------------------------
      *)
      createEdge (source: Node.T; target: Node.T; label: CARDINAL)
                  RAISES {NotOwner, Access.Locked, NodeNotFound,
                          InternalError};
                  (* Creates an edge between source and target with the
                     given label.*)

      deleteEdge (source: Node.T; target: Node.T; label: CARDINAL)
                  RAISES {NotOwner, Access.Locked, NodeNotFound,
                          InternalError};
                  (* Deletes the edge between source and target with the
                     given label.*)

      existsEdge (source: Node.T; target: Node.T; label: CARDINAL): BOOLEAN
                  RAISES {NotOwner, Access.Locked, NodeNotFound,
                          InternalError};
                  (* Returns TRUE if an edge between source and target with
                     the given label exists.*)

      getTargets (source: Node.T; label: CARDINAL): NodeSet.T
                  RAISES {NotOwner, Access.Locked, NodeNotFound,
                          InternalError};
                  (* Returns a set of all nodes who are connected to source
                     by outgoing edges with the given label.*)

      getSources (target: Node.T; label: CARDINAL): NodeSet.T
                  RAISES {NotOwner, Access.Locked, NodeNotFound,
                          InternalError};
                  (* Returns a set of all nodes who are connected to target
                     by incoming edges with the given label.*)

      getTargetsTo (source: Node.T; label: CARDINAL;
                    neighbour: CARDINAL := 0): NodeSet.T
                  RAISES {NotOwner, Access.Locked, NodeNotFound,
                          InternalError};
                  (* Returns a set of all nodes who are connected to source
                     by outgoing edges with the given label to nodes of graph
                     with number neighbour only. If neighbour = 0 internal
                     edges are returned. *)

      getSourcesFrom (target: Node.T; label: CARDINAL;
                      neighbour: CARDINAL := 0): NodeSet.T
                  RAISES {NotOwner, Access.Locked, NodeNotFound,
                          InternalError};
                  (* Returns a set of all nodes who are connected to target
                     by incoming edges with the given label from graph
                     with number neighbour only.*)

      getAllTargets (source: Node.T): NodeSet.T
                     RAISES {NotOwner, Access.Locked, NodeNotFound,
                             InternalError};
                     (* Returns a set of all nodes who are connected to
                        source by outgoing edges.*)

      getAllSources (target: Node.T): NodeSet.T
                     RAISES {NotOwner, Access.Locked, NodeNotFound,
                             InternalError};
                     (* Returns a set of all nodes who are connected to
                        target by incoming edges.*)

      getAllOutLabels (source: Node.T): CardSet.T
                       RAISES {NotOwner, Access.Locked, NodeNotFound,
                               InternalError};
                       (* Returns a set of all edge labels of edges leaving
                          source.*)

      getAllInLabels (target: Node.T): CardSet.T
                      RAISES {NotOwner, Access.Locked, NodeNotFound,
                              InternalError};
                      (* Returns a set of all edge labels of edges entering
                         target.*)

      getAllOutEdges (source: Node.T): NodeLabelRelation.T
                      RAISES {NotOwner, Access.Locked, NodeNotFound,
                              InternalError};
                      (* Returns a set of all edges leaving source as
                         (target, label) tuples.*)

      getAllInEdges (target: Node.T): NodeLabelRelation.T
                     RAISES {NotOwner, Access.Locked, NodeNotFound,
                             InternalError};
                     (* Returns a set of all edges entering target as
                        (source, label) tuples.*)
    END;

EXCEPTION
  InternalError(AtomList.T);
  NodeNotFound;                  (* Raised when error checks are active and
                                    the node does not exist. *)
  IndexUsed;                     (* Raised when put index is called for an
                                    already set index no. *)
  IndexUnused;                   (* deleteIndex was called with an index
                                    number that was not used for this
                                    node. *)
  NotOwner;                      (* Node or edge does not belong to this
                                    graph. *)
  NotExistent;
  InUse;
END PersistentGraph.
