INTERFACE Scheme;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:34  hosking
    Initial revision

    Revision 1.10  1998/03/18 09:27:21  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.9  1998/03/17 14:14:26  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.8  1998/01/21 12:36:49  roland
    New method baseName to determine filename.

    Revision 1.7  1997/12/08 11:52:21  roland
    New feature attribute placement. An attribute placement defines a
    physical location for a logical attribute.

    Revision 1.6  1997/11/21 15:17:00  roland
    New method typeHasSuperClass. Also: All temporary sets are disposed
    after use.

    Revision 1.5  1997/09/22 18:41:41  roland
    Scheme must not use cache when opened for writing.

    Revision 1.4  1997/09/03 13:06:58  roland
    Method closeAndFlush added to ensure real close 'inside' transactions.

    Revision 1.3  1997/06/02 15:54:31  roland
    Added two methods to convert between numbers of edge-types,
    node-types, and node-classes and their respective names.

    Revision 1.2  1997/05/05 10:50:40  roland
    Bugfixes in open routines for schemes.
    Dependency information moved from intern to public interface.

    Revision 1.1  1997/05/01 13:23:14  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:33:17  roland
    First version of new scheme layer for GRAS_3. Schemes are stored in
    separate graphs. Caches are used for accessing scheme.

*)
(***************************************************************************)

(* A scheme defines the type of a graph.  It determines what node types a
   graph can contain, what attributes a node of certain type has and the
   type of edges in a graph.  All components of a scheme might be
   identified by their name or via a number that is returned at declaration
   time.  The identifiers used for node types, node classes, and edge types
   have distinct name spaces, so equal identifiers might be used for
   different declaration items.  The identifiers of attributes have a name
   space per class, so that different classes might have attributes with
   the same name. *)

IMPORT AtomList, Pathname, TypedGraphPool;
IMPORT Access, NodeSet, Node;

TYPE
  ID = Node.T;
  IDSet = NodeSet.T;

  AccessMode = {ReadOnlyShared, ReadWriteExclusive};

  AttributeKind = {Intrinsic, Derived, Dummy, Meta};

  IndexProperties = {Normal, Index, Key};

  DependencyKind =               (* dependencies of attributes *)
    {Self,                       (* - depends on another attribute of the
                                    same node *)
     Incoming,                   (* - dependent on an attribute of another
                                    node via an incoming edge *)
     Outgoing};                  (* - dependent on an attribute of another
                                    node via an outgoing edge *)

  (* Cardinality restricts the number of emanating or incoming edges of a
     certain type at one node. *)
  Cardinality = RECORD
                  min: CARDINAL;
                  max: CARDINAL;
                END;

CONST
  ArbitraryCard = Cardinality{min := 0, max := LAST(CARDINAL)};
  (* - any number of edges *)
  AtMostOneCard = Cardinality{min := 0, max := 1};
  (* - at most one edge *)
  AtLeastOneCard = Cardinality{min := 1, max := LAST(CARDINAL)};
  (* - at least one edge *)
  ExactlyOneCard = Cardinality{min := 1, max := 1};
  (* - exactly one edge *)

TYPE

  T <: Public;

  (* A detailed specification of the methods of a Scheme.T, can be found at
     the bottom of this file. *)

  Public =
    OBJECT
    METHODS
      open (pool      : TypedGraphPool.T;
            schemeName: Pathname.T;
            local     : BOOLEAN            := FALSE;
            access: AccessMode := AccessMode.ReadWriteExclusive;
            new   : BOOLEAN    := TRUE                           ): T
            RAISES {NoValidScheme, Access.Locked, Access.Denied, Existent,
                    NotExistent, InternalError, InUse};

      close () RAISES {InternalError};

      closeAndFlush () RAISES {InternalError};

      baseName (): Pathname.T;

      (* Methods for managing node classes. *)

      declareNodeClass (name: TEXT): ID
                        RAISES {AlreadyDeclared, InternalError};

      existsNodeClassByName   (name: TEXT): BOOLEAN RAISES {InternalError};
      existsNodeClassByNumber (class: ID): BOOLEAN RAISES {InternalError};

      getNodeClassName (class: ID): TEXT
                        RAISES {NotDeclared, InternalError};
      getNodeClassNumber (name: TEXT): ID
                          RAISES {NotDeclared, InternalError};

      appendNodeClass (class, super: ID)
                       RAISES {NotDeclared, Cyclic, AttributeNameClash,
                               AttributePlacementClash, InternalError};

      getAllRootClasses (): IDSet RAISES {InternalError};
      getDirectSubClasses (class: ID): IDSet
                           RAISES {NotDeclared, InternalError};
      getAllSubClasses (class: ID): IDSet
                        RAISES {NotDeclared, InternalError};
      getDirectSuperClasses (class: ID): IDSet
                             RAISES {NotDeclared, InternalError};
      getAllSuperClasses (class: ID): IDSet
                          RAISES {NotDeclared, InternalError};
      getAllClasses (): IDSet RAISES {InternalError};

      (* Methods for managing node types *)

      declareNodeType (name: TEXT): ID
                       RAISES {AlreadyDeclared, InternalError};

      existsNodeTypeByName   (name: TEXT): BOOLEAN RAISES {InternalError};
      existsNodeTypeByNumber (type: ID): BOOLEAN RAISES {InternalError};

      getNodeTypeName (type: ID): TEXT RAISES {NotDeclared, InternalError};
      getNodeTypeNumber (name: TEXT): ID
                         RAISES {NotDeclared, InternalError};

      appendNodeType (type, class: ID)
                      RAISES {AlreadyDeclared, NotDeclared,
                              AttributePlacementClash, InternalError};

      getDirectNodeTypesOfClass (class: ID): IDSet
                                 RAISES {NotDeclared, InternalError};
      getAllNodeTypesOfClass (class: ID): IDSet
                              RAISES {NotDeclared, InternalError};
      typeHasSuperClass (type: ID): BOOLEAN
                         RAISES {NotDeclared, InternalError};
      getSuperClassOfNodeType (type: ID): ID
                               RAISES {NotDeclared, InternalError};
      getAllNodeTypes (): IDSet RAISES {InternalError};

      isSubClassOrType (sub, super: ID): BOOLEAN
                        RAISES {NotDeclared, InternalError};

      (* Methods for managing edge types *)

      declareEdgeType (name             : TEXT;
                       sourceNodeCoT    : ID;
                       targetNodeCoT    : ID;
                       sourceCardinality: Cardinality := ArbitraryCard;
                       targetCardinality: Cardinality := ArbitraryCard  ):
                       ID RAISES {NotDeclared, AlreadyDeclared,
                                  InternalError};

      existsEdgeTypeByName (name: TEXT; ): BOOLEAN RAISES {InternalError};
      existsEdgeTypeByNumber (type: ID): BOOLEAN RAISES {InternalError};

      getEdgeTypeName (type: ID): TEXT RAISES {NotDeclared, InternalError};
      getEdgeTypeNumber (name: TEXT): ID
                         RAISES {NotDeclared, InternalError};

      showEdgeTypeProps (    edgeType  : ID;
                         VAR sourceCT  : ID;
                         VAR targetCT  : ID;
                         VAR sourceCard: Cardinality;
                         VAR targetCard: Cardinality  )
                         RAISES {NotDeclared, InternalError};

      getAllEdgeTypes (): IDSet RAISES {InternalError};
      getIncomingEdgeTypes (CoT: ID): IDSet
                            RAISES {NotDeclared, InternalError};
      getOutgoingEdgeTypes (CoT: ID): IDSet
                            RAISES {NotDeclared, InternalError};

      (* For convenience *)

      getTypeName   (type: ID): TEXT RAISES {NotDeclared, InternalError};
      getTypeNumber (name: TEXT): ID RAISES {NotDeclared, InternalError};

      (* Methods for managing attributes *)

      declareAttribute (cot             : ID;
                        Name            : TEXT;
                        Kind            : AttributeKind;
                        Props           : IndexProperties;
                        ValueType       : CARDINAL;
                        ValueCardinality: Cardinality;
                        ConstantLength  : BOOLEAN;
                        Length          : CARDINAL         ): ID
                        RAISES {NotDeclared, AlreadyDeclared,
                                AttributeNameClash,
                                AttributePlacementClash, InternalError};

      existsAttributeByName (classOrType: ID; name: TEXT): BOOLEAN
                             RAISES {NotDeclared, InternalError};
      existsAttributeByNumber (attr: ID): BOOLEAN RAISES {InternalError};

      getAttributeNameAndClass (    attr       : ID;
                                VAR classOrType: ID;
                                VAR name       : TEXT)
                                RAISES {NotDeclared, InternalError};
      getAttributeNumber (classOrType: ID; name: TEXT): ID
                          RAISES {NotDeclared, InternalError};

      showAttributeProps (    attribute  : ID;
                          VAR Kind       : AttributeKind;
                          VAR Props      : IndexProperties;
                          VAR ValueType  : CARDINAL;
                          VAR AttCard    : Cardinality;
                          VAR ConstLength: BOOLEAN;
                          VAR Length     : CARDINAL         )
                          RAISES {NotDeclared, InternalError};

      getAllAttributesOfNodeClassOrType (type: ID): IDSet
                                         RAISES {NotDeclared, InternalError};

      (* Attribute placement *)

      defineAttributePlacement (classOrType    : ID;
                                attribute      : ID;
                                physAttributeNo: CARDINAL;
                                physStart      : CARDINAL;
                                physLength     : INTEGER   )
                                RAISES {NotDeclared,
                                        AttributePlacementClash,
                                        InternalError};

      showAttributePlacement (    classOrType: ID;
                                  attribute  : ID;
                              VAR physNo     : CARDINAL;
                              VAR physStart  : CARDINAL;
                              VAR physMaxLen : INTEGER   )
                              RAISES {NotDeclared, InternalError};

      (* Meta attributes *)

      putMetaAttribute (classOrType, attr: ID;
                        begin            : CARDINAL;
                        value            : TEXT;     )
                        RAISES {NotDeclared, InternalError};

      getMetaAttribute (classOrType, attr: ID; begin, length: CARDINAL):
                        TEXT RAISES {NotDeclared, InternalError};

      (* Automatic attribute evaluation *)

      setEvaluationFunction (CoT, attr: ID; name: TEXT)
                             RAISES {NotDeclared, InternalError};

      getEvaluationFunction (CoT, attr: ID): TEXT
                             RAISES {NotDeclared, InternalError};

      definesEvaluationFunction (CoT, attr: ID): BOOLEAN
                                 RAISES {NotDeclared, InternalError};

      (* Derived attributes *)

      declareDependency (CoT          : ID;
                         dependentAttr: ID;
                         dependsOnAttr: ID;
                         kind         : DependencyKind;
                         edgeType     : ID              )
                         RAISES {NotDeclared, InternalError};

      getDefiningDependencies (CoT, dependentAttr: ID): IDSet
                               RAISES {NotDeclared, InternalError};

      dependsOn (CoT, attr, dependency: ID): BOOLEAN
                 RAISES {NotDeclared, InternalError};

      definesDependency (CoT, attr: ID): BOOLEAN
                         RAISES {NotDeclared, InternalError};

      getAllDependencies (): IDSet RAISES {InternalError};

      showDependencyInfo (    dep                         : ID;
                          VAR dependentAttr, dependsOnAttr: ID;
                          VAR dependentCOT, dependsOnCOT  : ID;
                          VAR kind    : DependencyKind;
                          VAR edgeType: ID              )
                          RAISES {NotDeclared, InternalError};

      deleteDependencies (cot, dependentAttr: ID)
                          RAISES {NotDeclared, InternalError};


    END;

EXCEPTION
  InternalError(AtomList.T);
  InUse;
  Existent;
  NotExistent;
  NoValidScheme;
  NotDeclared;                   (* A given type/class/attribute is not
                                    declared *)
  AlreadyDeclared;               (* An item with this name already
                                    exists *)
  Cyclic;                        (* The inheritance relation would form a
                                    cycle *)
  AttributeNameClash;            (* A class inherits two attributes with
                                    the same name from different
                                    super-classes *)
  AttributePlacementClash;       (* A class has two attributes with
                                    overlapping placements *)

(* A Scheme.T, or simply scheme or schema defines the type of a GRAS graph
   instantiated as TypedGraph.T.  It defines node types, node classes, edge
   types, and attributes for node classes and types.

   A node type describes which attributes nodes of this type have and in
   which relationships a node of the type can participate.  Node types can
   be grouped in node classes, which in turn can form an inheritance
   hierarchy wiht multiple inheritance.  This leads to a stratified type
   system, with node classes defining the generalization/specialization
   relationships and node types the instances of node classes.  In common
   OO terminology, node classes are always abstract classes, because a node
   cannot be a direct instance of a node class, but must instantiate a node
   type.

   1) Methods for administration

   Before a scheme can be read or manipulated it must be opened with the
   'open' method.  If parameter 'new' is set to FALSE an already existing
   scheme is expected.  By default the scheme will be in
   'ReadWriteExclusive Mode'.  Beware of overwriting an existing scheme,
   while new is set to TRUE by default.  Open raises NotExistent if new is
   FALSE and no scheme with name schemName is found.  It raises Existent,
   if new is TRUE and a scheme with name schemName is found and graphs
   exists belonging to this scheme.

   To end access to a scheme, use close or closeAndFlush.  The difference
   between these two is that closeAndFlush makes sure the scheme will be
   closed at server side, too.  To achieve this, all pending (nested)
   transaction are commited, the scheme is closed, and an equal number of
   transaction as commited is restarted.  WARNING: This violates
   transaction semantics. *)
(**
   2) Node classes

   declareNodeClass: Declare the node class 'name'.  The name of the node
                     class must be unique for this scheme, otherwise
                     'AlreadyDeclared' will be raised.

   existsNodeClassByName,
   existsNodeClassByNumber: existance checks for node classes by name and
                            numerical identifier, repsectively.

   getNodeClassName,
   getNodeClassNumber: Given the numerical id of a class, deliver its
                       name and vice versa. The class has to be delcared
                       in advance, otherwise 'NotDeclared' is raised.

   appendNodeClass: node class 'class' becomes an heir of node class
                    'super', i.e. afterwards 'class' possesses the same
                    attributes and can participate in the same relation-
                    ships as 'super' ('class' becomes a specialization
                    of 'super'). This is illegal if a cycle in the
                    inheritance hierarchy would be introduced, an attribute
                    naming or placement conflict arises (see below in
                    'Attributes' and 'Attribute placement').

   getAllRootClasses,
   getDirectSubClasses,
   getDirectSuperClasses,
   getAllSubClasses,
   getAllSuperClasses,
   getAllClasses:         These methods querie the scheme for all classes w/o
                          a super-class, for direct successor/predecessor
                          classes, for the transitive closure of successor/
                          predecessor classes, and simply all defined classes,
                          respectively.


   Methods for querying a scheme about other scheme components (node types,
   edge types, and attributes) are quite canonical to those for node classes.
   Therefore, we will only describe the more relevant methods in the following.

   3) Node types

   declareNodeType: Declare the node type 'name'.  The name of the node
                    type must be unique for this scheme, otherwise
                    'AlreadyDeclared' will be raised.

   appendNodeType: Like appendNodeClass, adds a specialization/generalization
                   relation to the scheme.  The node type 'type' will
                   instantiate the node class 'class' and inherits all
                   attributes and relationships.  A node type can only be
                   an instance of one node class.

   typeHasSuperClass: Checks whether 'type' is an instance of any class.

   isSubClassOrType: check wheter node class/node type 'sub' inherits from
                     node class 'super'.

   4) Edge types

   declareEdgeType: Edge types define binary relationships between node classes/
                    node types. Each edge type has a name, a source and
                    target node class/node type, and also source and target
                    cardinalities. All nodes that have a type equal to
                    sourceNodeCoT or belonging to class sourceNodeCoT (CoT
                    stands for Class or Type), can participate as sources
                    in this relationship. The analogous holds for
                    targetNodeCoT. Source and target cardinalities might
                    restrict the number of relationships of this typ in which
                    a node can participate as source / target. A cardinality
                    is a [min:max] pair. The most common cases are defined
                    as constants: ArbitraryCard ([0:*]), AtMostOneCard ([0:1]),
                    AtLeastOneCard ([1:*]), and ExactlyOneCard [1:1].

   showEdgeTypeProps: Delivers the source/target node class/type and the
                      respective cardinalities of the edge type.

   getIncomingEdgeTypes,
   getOutgoingEdgeTypes: Return all relationships, in which a node class/
                         node type can participate as target and source,
                         respectively.

   5) Attributes

   Attributes are declared in a name space belonging to the node class / node
   type they are declared for. So attributes of different classes/types can
   have the same name. Care must be taken, though, to avoid name clashes when
   using inheritance. Name clashes are detected and will raise an exception.

   Attributes can have various properties. In the most simple form, an
   attribute of a node is a sequence of bytes (represented as TEXT) without
   a constraint to length or contents. It can be explicitly set/read by the
   application using the methods of TypedGraph.T. This attribute form is
   called intrinsic ( = AttributeKind), normal ( = IndexProperties).

   An instrinsic attribute can also have other index properties. An
   'index' attribute allows to query all nodes with the same attribute
   value. A 'key' attribute allows this, too, but only one node should
   have this value. This is not checked by GRAS, though.

   If an attribute is not intrinsic, it can be derived, i.e. the application
   provides an evaluation function at run-time which will be called every time
   GRAS detects a non-initialized attribute. For details, see 'Derived
   attributes' below.

   Other attribute kinds are 'meta' and 'dummy'. Meta attributes define
   class/type attributes, i.e. the value is the same for all nodes of
   the same type. Meta attributes are never 'index' or 'key' attributes.
   Dummy attributes are used to statically define dependencies for derived
   attributes. They have no type, but barely propagate validity / invalidity
   of derived attributes. See below for details.

   Besides these basic properties, GRAS supports a few constraints on
   attributes. An attribute can be typed, i.e. it gets a type identifier
   determining what values it can store. GRAS support for typed attributes
   is rather poor, though. The main purpose of typed attributes is to
   give the application a means to perform checks of its own. The same
   holds for attribute cardinalities.

   Attributes can have a fixed length. This is can be checked by GRAS
   automatically.

   Methods:

   declareAttribute: Declares an attribute for node classe / node type 'ID'.
                     This defines all its properties, as described above.
                     All nodes belonging to the class/type 'ID' will carry
                     the new attribute.  The name of the attribute must be
                     unique with respect to the class/type. 'Kind' specifies
                     whether the attribute is derived (with its subcase dummy),
                     i.e. depends on other attributes, or whether it is
                     an intrinsic attribute (with its subcase meta attribute
                     which has the same value for all nodes of a certain type,
                     i.e. is an attribute of the node type itself). If
                     constant length flag is set, 'Length' specifies the size
                     of the values that will be stored in that attribute
                     (in Bytes).  'Props' contains information about the
                     properties of the attribute, i.e.  whether it is a normal,
                     an index or a key attribute.  'ValueType' is an almost
                     uninterpreted number which gives applications the chance
                     to store information about the type of an attribute's
                     value (which is normally considered to be an
                     uninterpreted byte stream).  The numbers for standard
                     types like integer boolean etc.  are fixed!  All
                     remaining numbers may be used as 'pointers' to
                     application defined types.  'ValueCardinality' specifies
                     how many values can/should be stored for the attribute.
                     Note that 'ValueCardinality' is an attribute property
                     which has no meaning for GRAS (like 'ValueType') because
                     (up to now) all (sets of) attribute values are
                     uninterpreted arrays of byte for GRAS.

   showAttributeProps: Return all properties declared for an attribute.

   putMetaAttribute,
   getMetaAttribute: Modify and read meta attributes for a certain node
                     class/ node type. Note that these are schema operations,
                     because they modify attributes of node types/classes.

   6) Attribute placement

   Normal (i.e.  not index or key and also not meta or dummy)
   attributes can be assigned a special physical placement.  This is
   especially useful to optimize access times and space requirements
   for small often used attributes.

   The first 250 bytes of the physical attribute 0 of a ndoe are stored
   on the same page as the node information itself.  So accessing this
   attribute part after a node access, will not require to load an
   additional page.

   To use this physical attribute, one can set a placement for an
   attribute.  A placement defines the physical attribute number, the
   start position within the physical attribute from which the
   logical attribute should be read/written, and the physical maximum
   length (to allow multiple logical attributes sharing the same
   physical attribute).  If physMaxLen < 0, no limitation is assumed,
   i.e.  no other attribute can follow this one on the same physical
   attribute.

   Attribute placement is inherited along the inheritance hierarchy.
   If any of the attributes of any of the classes and types along the
   inhertiance hierarchy have placements conflicting with the
   proposed placement, AttributePlacementClash will be raised.

   Things to keep in mind: some features of normal attributes do not
   work properly or in a different way for attributes with a physical
   placement.  Truncation is one example.  Instead of really
   truncating the physical attribute, it will be filled with '\000'
   appropriately, because other attributes might follow this one on
   the physical attribute.  This is the reason, why physical
   placement for derived attributes works only when physMaxLen < 0
   (otherwise AttributePlacementClash is raised).

   Attributes with constant length must have physMaxLen at least as
   large as their declared length or unrestricted.

   Methods

   defineAttributePlacement,
   showAttributePlacement:   introduce and query attribute placement
                             for 'attribute' and node class / type
                             'classOrType'.

   7) Evaluation functions

   Similar to a placement, attributes can have an evaluation function.
   Evaluation functions serve two purposes: The provide an initial value
   for instrinsic/meta attributes and define an evaluation rule for
   derived attributes. Each class/type can have ist own evaluation function
   for its (inherited) attributes. To actually use evaluation functions,
   an application must provide GRAS with a function (an Evaluator object)
   at run-time. See TypedGraph.i3.

   Note: The main purpose of an evaluation function is to compute a new
   value for an invalid attribute and to store this value within the
   proper attribute itself by using putUntypedAttribute. GRAS is unable
   to check what an evaluation function does, it simply calls pocedures
   with a certain formal parameter profile.

   setEvaluationFunction,
   showEvaluationFunction: set/get evaluation function for the specified
                           node class/ type and attribute.

   definesEvaluationFunction: Checks whether node class/ type 'CoT'
                              defines (and not inherits) an evaluation
                              function for attribute 'attr'.

   8) Derived attributes

   Attributes of kind 'derived' normally depend on the values of other
   attributes. To compute the value of a derived attribute, the application
   has to declare an evaluation function and possibly also a number
   of dependecies. During run-time, when an attribute is read and is
   empty, GRAS assumes its value is invalid. It then looks for an evaluation
   function for this attribute (according to the node type of concerned
   node) within the graphs schema, checks whether the application has bound
   a procedure / evaluator to this function and calls this procedure.

   This is as simple as it sounds. The hard part begins, when GRAS needs
   to invalidate derived attributes due to changes of the attributes they
   depend on. For this purpose one can define dependencies among attributes.
   A dependency between attribute a at node class/type A and attribute
   b at node class/type B states that A.a depends on B.b. This
   dependency can be a 'self', 'incoming', or 'outgoing' dependency. A self
   dependency states, that A.a depends on B.b at the same node (so A and B
   should be in a (reflexive) generalization relationship). Incoming and
   outgoing dependencies are related to an edge type e. The meaning is that
   A.a of node n1 depends on B.b at node n2, where n1 is connected via
   an incoming/outgoing edge of type e to n2.

   With this information, GRAS is able to invalidate all attributes that
   depend on B.b when its value changes. The invalidation simply discardes
   the whole attribute value. It is recomputed only on demand, i.e. GRAS
   has a lazy attribute evaluation strategy.

   To be able to declare dependencies between attributes of nodes that
   are not directly connected, GRAS provides dummy attributes. Dummy
   attributes are also derived attributes, but no evaluation function is
   needed for them, because the can only be valid or invalid. A dependency
   to a node in the n-context of another, has to be modelled with n
   dependencies in the 1-context of the node using dummy attributes to
   propagate invalidity/validity of attributes.

   Methods

   declareDependency: Introduce a dependency between 'dependentAttr'
                      at node class/type 'CoT' and 'dependsOnAttr'. 'kind'
                      specifies whether this is a self, incoming, or
                      outgoing dependency. For the latter two, 'edgeType'
                      is the edge type over which the dependency is defined.

   getDefiningDependencies: Return all dependencies on which
                            'CoT.dependentAttr' depends.

   dependsOn: Checks whether 'CoT.attr' depends on 'dependency'.

   definesDependency: Checks whether 'CoT' defines (not inherits)
                      a dependency for 'attr'.

   showDependencyInfo: Return the properties of a dependency.

   deleteDependencies: Deletes all dependencies of attribute 'dependentAttr'
                       of node class/node type 'cot'.

   Note: Ther are some more methods related to dependencies defined in
   InternScheme.i3. These are used by GRAS at runtime and should not be
   interesting for normal applications.

   9) Miscellaneous methods

   getTypeName,
   getTypeNumber: simply for convenience. They check internally for node
                  classes, node types, and edge types with an appropriate
                  name / numerical id and return the numerical id / name.
                  Due to separate name spaces, the conversion between name
                  and id might not be unique.

*)

END Scheme.
