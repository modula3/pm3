INTERFACE Database;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.3  1998/01/21 12:35:40  roland
    New method baseName to determine filename.

    Revision 1.2  1997/04/24 14:32:12  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/03/26 11:39:01  roland
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

(*
 | --- Database ----------------------------------------------------
 A Database.T represents the contents of one graph.

 Databases are accessed by their names, which are filenames relative
 to a (client or server) root directory. A new database can only be
 created if its name points to a leaf of the path hierarchy.

 On Opening a database some additional error checks can be turned on
 by the client. If consistency checks are active, the existance of
 nodes is checked before attribute, name and edge operations are
 executed. Note that inactive checks don't prevent *any* exceptions to
 be raised when a node does not exist, only additional checks made in
 this module are switched off.
 | ------------------------------------------------------------------------
 *)

IMPORT VirtualResource, PageFile, Access, CardSet, TaggedNameSet,
       AttributeDescriptionSet, CardRelation;
IMPORT AtomList, Pathname;

TYPE

  T <: Public;

  Public =
    OBJECT
    METHODS
      open (resource   : VirtualResource.T;
            name       : Pathname.T;
            mode       : Access.Mode;
            new        : BOOLEAN;
            local      : BOOLEAN;
            errorChecks: BOOLEAN            ): T
            RAISES {PageFile.NoAccess, Access.Denied, Access.Locked};
            (* Opens the graph specified by name.  If new is set to TRUE a
               new graph will be created maybe destroying an old version.
               Creating a graph will fail if access is set to read only
               mode.  If new is FALSE, a graph with name 'name' must
               exist. *)

      close () RAISES {InternalError};

      errorCheckOn (): BOOLEAN;
                    (* Returns TRUE if the error check flag is set. *)

      baseName(): Pathname.T;


      (*
       | --- node operations ----------------------------------------------------
       Each graph may contain an "arbitrary" number of nodes. Each node
       carries a CARDINAL-typed label.
       | ------------------------------------------------------------------------
       *)
      createNodeNumber (neighbour: CARDINAL): CARDINAL
                        RAISES {Access.Locked, InternalError};
                        (* First step to create a node in the neighbourship
                           of the given node.  The new node number is
                           returned.  If neighbour does not exist, the new
                           node will get the neighbour number.  Note that
                           createNodeNumber must be followed immediately by
                           a createNode call. *)

      createNode (label: CARDINAL): CARDINAL
                  RAISES {Access.Locked, InternalError};
                  (* Creates the node with the number returned by
                     createNodeNumber and the given label.  This must be
                     immediately preceeded by createNodeNumber.  The node
                     number is returned again. *)

      deleteNode (    node      : CARDINAL;
                  VAR attributes: AttributeDescriptionSet.T;
                  VAR indexes   : TaggedNameSet.T;
                  VAR outEdges  : CardRelation.T;
                  VAR inEdges   : CardRelation.T                 )
                  RAISES {Access.Locked, InternalError};
                  (* Deletes the given node.  The method returns the
                     attributes and indexes stored for the node and all
                     incident edges.  Edges are returned as (label, node)
                     tuples.  These attributes, names, and edges were
                     deleted with the node. *)

      deleteNodeNoInfo (node: CARDINAL)
                        RAISES {Access.Locked, InternalError};
                        (* Same as deleteNode, but no information abput
                           attributes, indexes, and edges is returned. *)

      existsNode (node: CARDINAL): BOOLEAN
                  RAISES {Access.Locked, InternalError};
                  (* Returns TRUE if node exists. *)

      putNodeLabel (node: CARDINAL; label: CARDINAL)
                    RAISES {Access.Locked, NodeNotFound, InternalError};
                    (* Stores label at node. *)

      getNodeLabel (node: CARDINAL): CARDINAL
                    RAISES {Access.Locked, NodeNotFound, InternalError};
                    (* Retrieves the label of node. *)


      (*
       | --- attribute operations -----------------------------------------------
       Each node may carry an "arbitrary" number of attributes identified
       by an attribute number. Each attribute may store an
       "arbitrary"-sized text.
       | ------------------------------------------------------------------------
       *)
      putAttribute (node       : CARDINAL;
                    attributeNo: CARDINAL;
                    start      : CARDINAL;
                    attribute  : TEXT      )
                    RAISES {Access.Locked, NodeNotFound, InternalError};
                    (* Stores the attribute at attributeNo of the given
                       node starting at start. *)

      deleteAttribute (node: CARDINAL; attributeNo: CARDINAL)
                       RAISES {Access.Locked, NodeNotFound, InternalError};
                       (* Deletes the attribute with attributeNo of the
                          given node. *)

      getAttribute (node       : CARDINAL;
                    attributeNo: CARDINAL;
                    start      : CARDINAL;
                    length     : CARDINAL  ): TEXT
                    RAISES {Access.Locked, NodeNotFound, InternalError};
                    (* Read length bytes from the attribute at attributeNo
                       of the given entity node at start.  The returned
                       value might be shorter than length if less than
                       length bytes were stored. *)

      truncateAttribute (node       : CARDINAL;
                         attributeNo: CARDINAL;
                         size       : CARDINAL  ) RAISES {Access.Locked,
                                                          NodeNotFound,
                                                          InternalError};
                         (* The given attribute is truncated to size bytes.
                            The attribute must contain at least size bytes
                            before. *)

      getAllAttributeNumbers (node: CARDINAL): CardSet.T
                              RAISES {Access.Locked, NodeNotFound,
                                      InternalError};
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
      putIndex (node: CARDINAL; indexNo: CARDINAL; index: TEXT)
                RAISES {Access.Locked, NodeNotFound, IndexUsed,
                        InternalError};
                (* Stores index as index attribute for the node. *)

      deleteIndex (node: CARDINAL; indexNo: CARDINAL; index: TEXT)
                   RAISES {Access.Locked, NodeNotFound, IndexUnused,
                           InternalError};
                   (* Delete the given index attribute. *)

      getIndex (node: CARDINAL; indexNo: CARDINAL; VAR found: BOOLEAN):
                TEXT RAISES {Access.Locked, NodeNotFound, InternalError};
                (* Returns the value of the given index attribute. *)

      getNodesWithIndex (indexNo: CARDINAL; index: TEXT): CardSet.T
                         RAISES {Access.Locked, InternalError};
                         (* Returns a set of all nodes with the given index
                            attribute. *)

      getIndexNosForNode (node: CARDINAL): CardSet.T
                          RAISES {Access.Locked, NodeNotFound,
                                  InternalError};
                          (* Returns a set of all index numbers which
                             contain an index for node. *)


      (*
       | --- edge operations ----------------------------------------------------
       Nodes may be connected by edges. Each edge has a label stored as a
       CARDINAL number.
       | ------------------------------------------------------------------------
      *)
      createEdge (source: CARDINAL; target: CARDINAL; label: CARDINAL)
                  RAISES {Access.Locked, NodeNotFound, InternalError};
                  (* Creates an edge between source and target with the
                     given label.*)

      deleteEdge (source: CARDINAL; target: CARDINAL; label: CARDINAL)
                  RAISES {Access.Locked, NodeNotFound, InternalError};
                  (* Deletes the edge between source and target with the
                     given label.*)

      existsEdge (source: CARDINAL; target: CARDINAL; label: CARDINAL):
                  BOOLEAN
                  RAISES {Access.Locked, NodeNotFound, InternalError};
                  (* Returns TRUE if an edge between source and target with
                     the given label exists.*)

      getTargets (source: CARDINAL; label: CARDINAL): CardSet.T
                  RAISES {Access.Locked, NodeNotFound, InternalError};
                  (* Returns a set of all nodes who are connected to source
                     by outgoing edges with the given label.*)

      getSources (target: CARDINAL; label: CARDINAL): CardSet.T
                  RAISES {Access.Locked, NodeNotFound, InternalError};
                  (* Returns a set of all nodes who are connected to target
                     by incoming edges with the given label.*)

      getAllTargets (source: CARDINAL): CardSet.T
                     RAISES {Access.Locked, NodeNotFound, InternalError};
                     (* Returns a set of all nodes who are connected to
                        source by outgoing edges.*)

      getAllSources (target: CARDINAL): CardSet.T
                     RAISES {Access.Locked, NodeNotFound, InternalError};
                     (* Returns a set of all nodes who are connected to
                        target by incoming edges.*)

      getAllOutLabels (source: CARDINAL): CardSet.T
                       RAISES {Access.Locked, NodeNotFound, InternalError};
                       (* Returns a set of all edge labels of edges leaving
                          source.*)

      getAllInLabels (target: CARDINAL): CardSet.T
                      RAISES {Access.Locked, NodeNotFound, InternalError};
                      (* Returns a set of all edge labels of edges entering
                         target.*)

      getAllOutEdges (source: CARDINAL): CardRelation.T
                      RAISES {Access.Locked, NodeNotFound, InternalError};
                      (* Returns a set of all edges leaving source as
                         (label, target) tuples.*)

      getAllInEdges (target: CARDINAL): CardRelation.T
                     RAISES {Access.Locked, NodeNotFound, InternalError};
                     (* Returns a set of all edges entering target as
                        (label, source) tuples.*)
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
  NotInTransaction;

END Database.
