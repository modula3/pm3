INTERFACE RGGlobal;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:45  hosking
    Initial revision

    Revision 1.5  1998/07/27 13:30:59  roland
    Procedure AGDeleteScheme added.

    Revision 1.4  1998/05/15 12:21:56  renehuel
    Adapted the RGRASGraph interface to handle events.

    Revision 1.3  1997/12/23 12:26:34  renehuel
    Now referencing sets and relation sets via a number, and organizing
    them in seperate lists.

    Revision 1.2  1997/10/24 14:39:02  renehuel
    These files implement the new RGRASGraph interface.

    Revision 1.1  1996/09/13 15:46:17  walkmark
    Interface to use old GRAS Implementations with new GRAS (basic
    procedures) (hope it will work)

*)
(***************************************************************************)

IMPORT Node, Scheme;

FROM Types IMPORT SHORTCARD;

CONST

  MaxRecordLength = 251;         (* maximal length of a record to store
                                    attribute values etc. *)

  MaxNormalAttributeLength = MaxRecordLength * LAST(SHORTCARD);
  (** Maximum length of normal attributes  *)

  MaxIndexAttributeLength = MaxRecordLength;
  (** Maximum length of index attributes   *)

TYPE

  PoolName = TEXT;

  GraphName = TEXT;

  SchemeName = TEXT;

  TStatus = {NoError, NotExistent, AlreadyExistent, AlreadyOpen, NotEmpty,
             NoScheme, StillOpen, StillPendingTransaction, StillUsed};

  ApplicationName = TEXT;

  GraphPoolMode = {NewPool, OldPool, GetPool};

  ExternNumber = CARDINAL;
    (** Extern number of a graph.            *)

  GraphNumber = CARDINAL;
    (** A graph number identifies each open  *)
    (** graph.                               *)

  SimpleElement = Node.T;

  SimpleSet = CARDINAL;

  GraphType = CARDINAL;
    (** Type of a graph. 0 means undefined.  *)

  RelSet = CARDINAL;

  GroupMode = {NewGroup, OldGroup, GetGroup};

  GroupName = TEXT;

  GraphMode = {NewGraph, OldGraph, GetGraph};

  GroupType = {Session, Transaction, Operation};

  NodeNumber = Node.T;

  TypeNumber = Node.T;

  AttributeNumber = Node.T;
    (** Data type for attribute numbers.     *)

  SchemeNumber = CARDINAL;

  Cardinality =                  (** Restricts the number of emanating or *)
    (** incoming edges of a certain type at  *)
    (** one node.                            *)
    {OptUnique,                  (** - at most one edge                   *)
     OblUnique,                  (** - exactly one edge                   *)
     OptSet,                     (** - any number of edges                *)
     OblSet};                    (** - at least one edge                  *)

  AttributeKind =
    Scheme.AttributeKind;        (** for GRAS++: possible kind of attri-  *)
    (** butes.
    {Derived, Intrinsic, Dummy, Meta};*)

  IndexProperties =
    Scheme.IndexProperties;      (** for GRAS++: see AttributedGraph.def  *)
    (** for details                          *)

  EvalFunction =
    PROCEDURE (p0: GraphNumber; p1: NodeNumber; p2: AttributeNumber);

  ValueTypeNumber = CARDINAL;    (** identifies the type of an attribute *)
    (** value.                              *)

  DependencyKind =               (** for GRAS++: possible dependencies of *)
    (** attributes                           *)
    {SelfDependent,              (** - dependent on another attribute of  *)
     (**   the same node                      *)
     IncomingDependent,          (** - dependent on an attribute of       *)
     (**   another node via an incoming edge  *)
     OutgoingDependent};         (** - dependent on an attribute of       *)
    (**   another node via an outgoing edge  *)

  ActionProc = PROCEDURE (g: GraphNumber; e: GraphEvent);
    (** Procedure type for actions.          *)

  ActionPriority =
    CARDINAL;                    (** lowest priority = 0.                 *)

  EventKind =                    (** possible event kinds *)

    {EVNewNode, EVDeleteNode, EVNewEdge, EVDeleteEdge, EVModifyAttribute,
     EVIndexKeyModified, EVTransactionStart, EVTransactionEnd,
     EVSetCheckpoint, EVUndo, EVRedo, EVOpenGraph, EVCloseGraph,
     EVUserDefined};

  (* The variables SNode, TNode and TNo have changed their type from
     integer to node number.  In those cases where an integer number is to
     be returned in a variable of the type node number, the value can be
     found in the entity part of the node number. *)

  GraphEvent =
    RECORD                       (** possible events                      *)
      Kind       : EventKind;
      SNode      : NodeNumber;
      SNodeExists: BOOLEAN;
      TNode      : NodeNumber;
      TNodeExists: BOOLEAN;
      TNo        : TypeNumber;
      Self       : BOOLEAN;
    END;

END RGGlobal.
