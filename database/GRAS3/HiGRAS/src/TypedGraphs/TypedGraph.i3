INTERFACE TypedGraph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:35  hosking
    Initial revision

    Revision 1.7  1998/06/10 10:57:05  kluck
    Definition of type LogMode from ChgMgmtGraph repeated.

    Revision 1.6  1998/03/18 09:27:24  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.5  1998/03/17 14:14:30  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.4  1998/01/21 12:38:38  roland
    New exception to signal undefined attribute value for getTypedAttribute.

    Revision 1.3  1997/10/14 10:33:43  roland
    Description for getAllOut/InEdges corrected.

    Revision 1.2  1997/09/09 10:37:06  roland
    closeAndFlush now also for graphs.

    Revision 1.1  1997/05/01 13:23:18  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.4  1997/02/26 17:04:22  roland
    Type EvaluationFunction replaced with type Evaluator. This should
    ease inheritance as well as redefinition in modules using TypedGraph.

    Revision 1.3  1997/02/13 12:16:08  roland
    Type information of nodes is now also stored in the node
    labels. Therefore, getNodeLabel and putNodeLabel removed.

    Revision 1.2  1997/02/04 11:16:03  roland
    It is now possible to disable logging in ChgMgmtGraph completely.

    Revision 1.1  1997/01/31 10:33:24  roland
    First version of new scheme layer for GRAS_3. Schemes are stored in
    separate graphs. Caches are used for accessing scheme.

*)
(***************************************************************************)

IMPORT Pathname, PageFile, Access, NodeSet, NodeTypeRelation, Node;
IMPORT AtomList;
IMPORT ChgMgmtGraph AS Super;
IMPORT Scheme, AttributeValue, TaggedNameSet, AttributeDescriptionSet,
       TypedGraphPool;

TYPE
  ErrorCheck = {Node,            (* check whether nodes exists *)
                Type,            (* check whether nodes have
                                    existing/appropriate type *)
                Edge,            (* check existing/appropriate edge type *)
                Cardinality,     (* check edge cardinality *)
                Attribute        (* check types and cardinality of
                                    attribute values *)
               };
  ErrorCheckSet = SET OF ErrorCheck;

CONST
  AllChecks = ErrorCheckSet{FIRST(ErrorCheck).. LAST(ErrorCheck)};
  NoChecks  = ErrorCheckSet{};

TYPE
  AccessMode = Super.AccessMode;
  LogMode = Super.LogMode;
  Evaluator = OBJECT
              METHODS
                apply (graph: T; node: Node.T; attr: Scheme.ID)
                       RAISES {CyclicEvaluation, Access.Locked,
                               InternalError, LogError};

              END;
    (* An Evaluator is able to compute a value for an attribute of a
       certain node class/type.  Its apply-method is called whenever GRAS
       cannot read a valid attribute instance for attribute 'attr' at
       'node'.  Each attribute number / node-type cobination might have its
       own evaluation object (see Scheme.i3).  While the name of an
       evaluation function is statically declared in the graphs scheme, the
       actual function has to be bound to this name at runtime with the
       method bindEvaluator. *)

TYPE
  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      open (pool       : TypedGraphPool.T;
            baseName   : Pathname.T;
            access     : AccessMode;
            new        : BOOLEAN;
            errorChecks: ErrorCheckSet;
            local      : BOOLEAN            := FALSE;
            log        : LogMode            := LogMode.Linear;
            scheme     : Pathname.T         := NIL             ): T
            RAISES {NoScheme, PageFile.NoAccess, Access.Locked,
                    Access.Denied, InternalError, NotExistent, InUse};
            (* Opens the graph specified by baseName.  If new is TRUE a new
               graph will be created maybe destroying an old version;
               scheme must be a valid scheme name.  Creating a graph will
               fail if access is set to a read only mode.  If new is set to
               FALSE an existing graph to open is expected and scheme is
               ignored. *)

      close (keepLog: BOOLEAN) RAISES {InternalError};

      closeAndFlush (keepLog: BOOLEAN) RAISES {InternalError};
                     (* closeAndFlush commits all pending transactions,
                        closes the graph and restarts the pending
                        transactions again.  NOTE: this violated
                        transaction semantics. *)

      setErrorChecks (checks: ErrorCheckSet) RAISES {InternalError};

      getErrorChecks (): ErrorCheckSet;
                      (* Set and get error checks for the current
                         transaction or for the graph, if no transaction is
                         running. *)

      getScheme (): Scheme.T;

      (*
       | --- node operations ----------------------------------------------------
       *)
      createNodeNumber (neighbour: Node.T): Node.T
                        RAISES {NotOwner, Access.Locked, InternalError};
                        (* First step to create a node in the neighbourship
                           of the given node.  The new node number is
                           returned.  If neighbour does not exist, the new
                           node will get the neighbour number.  Note that
                           createNodeNumber must be followed immediately by
                           a createNode call. *)

      createNode (type: Scheme.ID): Node.T
                  RAISES {Unknown, Access.Locked, LogError, InternalError};
                  (* Creates the node of type type with the number returned
                     by createNodeNumber.  This must be immediately
                     preceeded by createNodeNumber.  The node number is
                     returned again. *)

      deleteNode (    node      : Node.T;
                  VAR attributes: AttributeDescriptionSet.T;
                  VAR indexes   : TaggedNameSet.T;
                  VAR outEdges  : NodeTypeRelation.T;
                  VAR inEdges   : NodeTypeRelation.T         )
                  RAISES {NotOwner, Access.Locked, LogError, InternalError};

      deleteNodeNoInfo (node: Node.T) RAISES {NotOwner, Access.Locked,
                                              LogError, InternalError};
                        (* Delete node in graph.  If node does not exist,
                           nothing changes. *)

      existsNode (node: Node.T): BOOLEAN
                  RAISES {NotOwner, Access.Locked, InternalError};
                  (* Returns TRUE if node exists. *)

      getNodeType (node: Node.T): Scheme.ID
                   RAISES {NotOwner, Unknown, NodeNotFound, Access.Locked,
                           InternalError};
                   (* Returns the type of node *)

      nodeIsInstanceOf (node: Node.T; ct: Scheme.ID): BOOLEAN
                        RAISES {NotOwner, Unknown, NodeNotFound,
                                Access.Locked, InternalError};
                        (* TRUE, iff ct is a declared node class/type and
                           node is an instance of (a sub type of) ct. *)

      getAllNodesOfType (type: Scheme.ID): NodeSet.T
                         RAISES {Access.Locked, InternalError};
                         (* Return a set of all nodes that have type
                            'type'. *)


      (*
       | --- attribute operations -----------------------------------------------
       *)
      putAttribute (node       : Node.T;
                    attributeNo: Scheme.ID;
                    start      : CARDINAL;
                    attribute  : TEXT       )
                    RAISES {NotOwner, NodeNotFound, Unknown, WrongType,
                            Access.Locked, LogError, InternalError};
                    (* Stores the attribute at attributeNo of the given
                       node starting at start. *)

      putTypedAttribute (node: Node.T;
                         attr: Scheme.ID;
                         val : AttributeValue.T)
                         RAISES {NotOwner, NodeNotFound, Unknown,
                                 WrongType, CardinalityError,
                                 Access.Locked, LogError, InternalError};
                         (* Stores val as attribute no attr of node.
                            AttributeValues have a type, so that type and
                            cardinality checks on attributes can be
                            performed. *)

      getAttribute (node       : Node.T;
                    attributeNo: Scheme.ID;
                    start      : CARDINAL;
                    length     : CARDINAL   ): TEXT
                    RAISES {NotOwner, Access.Locked, Unknown, WrongType,
                            CyclicEvaluation, NodeNotFound, InternalError,
                            LogError};
                    (* Read length characters of attribute attributeNo at
                       node node beginning at position start.  The returned
                       value might be shorter than length if less than
                       length bytes were stored.  If GRAS cannot find an
                       attribute value, it will try to recompute the value
                       by calling an evaluation function declared for the
                       attribute and the type of node.  Evaluation
                       functions have to be declared in the scheme and
                       bound to the graph at runtime with bindEvaluator. *)

      getTypedAttribute (node: Node.T;
                         attr: Scheme.ID;
                         val : AttributeValue.T)
                         RAISES {ValueUndefined, NotOwner, NodeNotFound,
                                 Unknown, WrongType, CardinalityError,
                                 CyclicEvaluation, Access.Locked,
                                 InternalError, LogError};
                         (* Read typed attribute attr from node.
                            Evaluation works the same way as in
                            getAttribute. *)

      truncateAttribute (node       : Node.T;
                         attributeNo: Scheme.ID;
                         size       : CARDINAL   )
                         RAISES {NotOwner, Access.Locked, Unknown,
                                 NodeNotFound, WrongType, LogError,
                                 InternalError};
                         (* The given attribute is truncated to size bytes.
                            The attribute must contain at least size bytes
                            before.  Attributes of length 0 will be treated
                            as invalid. *)

      bindEvaluator (name: TEXT; eval: Evaluator) RAISES {AlreadyBound};
                     (* Binds eval to the function name name.  Gras does
                        not check whether this name is a declared
                        evaluation function name.  If it is not eval will
                        never be used. *)


      (*
       | --- index attribute operations -----------------------------------------
       *)
      getNodesWithAttribute (attr: Scheme.ID; val: TEXT): NodeSet.T
                             RAISES {CyclicEvaluation, Unknown, NoIndex,
                                     Access.Locked, InternalError, LogError};
                             (* Returns a set of all nodes with the given
                                index attribute. *)

      (*
       | --- edge operations ----------------------------------------------------
      *)
      createEdge (source: Node.T; target: Node.T; type: Scheme.ID)
                  RAISES {NotOwner, Unknown, NodeNotFound, WrongType,
                          CardinalityError, Access.Locked, LogError,
                          InternalError};
                  (* Creates an edge between source and target with the
                     given type.*)

      deleteEdge (source: Node.T; target: Node.T; type: Scheme.ID)
                  RAISES {NotOwner, NodeNotFound, Unknown, WrongType,
                          CardinalityError, Access.Locked, LogError,
                          InternalError};
                  (* Deletes the edge between source and target with the
                     given type.*)

      existsEdge (source: Node.T; target: Node.T; type: Scheme.ID): BOOLEAN
                  RAISES {NotOwner, NodeNotFound, Unknown, Access.Locked,
                          InternalError};
                  (* Returns TRUE if an edge between source and target with
                     the given type exists.*)

      validEdge (source   : Node.T;
                 target   : Node.T;
                 type     : Scheme.ID;
                 checkCard: BOOLEAN     := FALSE): BOOLEAN
                 RAISES {NotOwner, NodeNotFound, Access.Locked,
                         InternalError};
                 (* Returns TRUE if the scheme allows an edge between
                    source and target with the given type, i.e.
                    createEdge(source, target, type) would not raise
                    WrongType or CardinalityError.  Note: When the check
                    and the create operations are executed in two different
                    top-level transactions, the state of the graph might
                    have changed in the meantime. *)

      getTargets (source: Node.T; type: Scheme.ID): NodeSet.T
                  RAISES {NotOwner, NodeNotFound, Unknown, Access.Locked,
                          InternalError};
                  (* Returns a set of all nodes who are connected to source
                     by outgoing edges with the given type.*)

      getSources (target: Node.T; type: Scheme.ID): NodeSet.T
                  RAISES {NotOwner, NodeNotFound, Unknown, Access.Locked,
                          InternalError};
                  (* Returns a set of all nodes who are connected to target
                     by incoming edges with the given type.*)

      getTargetsTo (source   : Node.T;
                    type     : Scheme.ID;
                    neighbour: CARDINAL    := 0): NodeSet.T
                    RAISES {NotOwner, Access.Locked, NodeNotFound,
                            InternalError, Unknown};
                    (* Returns a set of all nodes who are connected to
                       source by outgoing edges with the given type to
                       nodes of graph with number neighbour only.  If
                       neighbour = 0 internal edges are returned. *)

      getSourcesFrom (target   : Node.T;
                      type     : Scheme.ID;
                      neighbour: CARDINAL    := 0): NodeSet.T
                      RAISES {NotOwner, Access.Locked, NodeNotFound,
                              InternalError, Unknown};
                      (* Returns a set of all nodes who are connected to
                         target by incoming edges with the given type from
                         graph with number neighbour only.*)

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

      getAllOutTypes (source: Node.T): Scheme.IDSet
                      RAISES {NotOwner, Access.Locked, NodeNotFound,
                              InternalError};
                      (* Returns a set of all edge types of edges leaving
                         source.*)

      getAllInTypes (target: Node.T): Scheme.IDSet
                     RAISES {NotOwner, Access.Locked, NodeNotFound,
                             InternalError};
                     (* Returns a set of all edge types of edges entering
                        target.*)

      getAllOutEdges (source: Node.T): NodeTypeRelation.T
                      RAISES {NotOwner, Access.Locked, NodeNotFound,
                              InternalError};
                      (* Returns a set of all edges leaving source as
                         (target, type) tuples.  (type is the type of the
                         edge) *)

      getAllInEdges (target: Node.T): NodeTypeRelation.T
                     RAISES {NotOwner, Access.Locked, NodeNotFound,
                             InternalError};
                     (* Returns a set of all edges entering target as
                        (source, type) tuples.  (type is the type of the
                        edge) *)
    END;

EXCEPTION
  InternalError(AtomList.T);
  LogError(AtomList.T);
  NotExistent;
  InUse;

  NodeNotFound;                  (* Raised when error checks are active and
                                    the node does not exist. *)
  IndexUsed;                     (* Raised when put index is called for an
                                    already set index no. *)
  IndexUnused;                   (* deleteIndex was called with an index
                                    number that was not used for this
                                    node. *)
  Unknown;                       (* The given node/edge class/type or
                                    attribute number is not declared *)
  WrongType;                     (* This operation is not allowed for the
                                    given node/edge class/type *)
  ValueUndefined;                (* A typed attribute cannot be read,
                                    because no value was previously
                                    written *)
  NoIndex;                       (* This operation is only allowed for
                                    index attributes *)
  NotOwner;                      (* Operation must be called on a different
                                    graph *)
  CardinalityError;              (* Edge or attribute value cardinalities
                                    are violated *)
  NoScheme;                      (* Either no scheme found/specified or the
                                    name given is no scheme. *)
  CyclicEvaluation;              (* An attribute occured twice in an
                                    attribute evaluation sequence. *)
  AlreadyBound;                  (* An evaluation function different from
                                    the one given is bound to name. *)
  NotInTransaction;


END TypedGraph.
