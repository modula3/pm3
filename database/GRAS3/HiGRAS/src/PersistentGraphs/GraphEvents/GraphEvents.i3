INTERFACE GraphEvents;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:46  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:32  hosking
    Import of GRAS3 1.1

    Revision 1.3  1998/01/13 15:57:26  roland
    Bugfix: Graph and Pool attributes are RefAny.

    Revision 1.2  1997/11/21 09:37:15  roland
    New GraphEvents PutAttribute and TruncateAttribute replace ModifyAttribute

    Revision 1.1  1997/11/10 10:46:30  roland
    Graph event type definitions and handling.

*)
(***************************************************************************)

IMPORT Event, Txn, Node;
FROM EventType IMPORT Mismatch, Unknown;

TYPE
  Operation = {CreateNode, DeleteNode, CreateEdge, DeleteEdge,
               PutAttribute, TruncateAttribute, PutIndex, DeleteIndex};

CONST
  EventTypeName = ARRAY Operation OF
                    TEXT{
                    "CreateNode", "DeleteNode", "CreateEdge", "DeleteEdge",
                    "PutAttribute", "TruncateAttribute",
                    "PutIndex", "DeleteIndex"};
  (* GraphEvents are declared in EventTypes.i3 with these names. *)

CONST
  (* Attribute names of the above event types *)
  LevelAttribute       = "Level";
  PoolNameAttribute    = "PoolName";
  PoolAttribute        = "Pool";
  IsPreEventAttribute  = "IsPreEvent";
  GraphNumberAttribute = "GraphNumber";
  GraphAttribute       = "Graph";

  (* for node events *)
  NodeAttribute      = "Node";
  NodeLabelAttribute = "NodeLabel";

  (* for edge events *)
  SourceGraphAttribute  = "SourceGraph";
  SourceNodeAttribute   = "SourceNode";
  TargetGraphAttribute  = "TargetGraph";
  TargetNodeAttribute   = "TargetNode";
  EdgeLabelAttribute    = "EdgeLabel";
  SourceExistsAttribute = "SourceExists";
  TargetExistsAttribute = "TargetExists";

  (* for attribute and index events *)
  (* NodeAttribute = "Node";*)
  AttributeNoAttribute = "AttributeNo";
  NodeExistsAttribute  = "NodeExists";

  (* for index and put attribute *)
  ValueAttribute = "Value";

  (* for truncate *)
  TruncateLengthAttribute = "TruncateLength";

  (* for put attribute *)
  PutAttributeStartAttribute = "PutAttributeStart";


TYPE
  T = Event.T;
    (** All GraphEvents have the following attributes:
          poolName,
          pool      : the pool in which the event occurred its name.
          isPreEvent: Events can be signaled before (pre) or after (post)
                        the operation has been performed.
          level     : the transaction-level of the transaction
                        that signaled the event.
          graphNo,
          graph     : the graph in which the event occurred and its number.

        Additionally to this basic set of event attributes, different event
        types have different attributes.

        Node events (create/delete) additionally carry attributes
          node      : the created/deleted node
          nodelabel : its label

        edge events (create/delete) additionally carry attributes
          source,
          target    : source and target nodes
          edgelabel : the label of the edge
          sourceexists,
          targetexists: information whether source and target nodes
                        still exist when the event is signalled to the
                        action.

        attribute and index events (modify/put/delete) additionally
        carry attributes
          node      : the concerned node node
          attrno    : the attribute number
          nodeexists: information whether node still exist when the
                      event is signalled to the action.

    *)

PROCEDURE SignalCreateNode (         transUnit : CARDINAL;
                                     poolName  : TEXT;
                                     pool      : REFANY;
                                     graphNo   : CARDINAL;
                                     graph     : REFANY;
                                     isPreEvent: BOOLEAN;
                                     level     : Txn.Level;
                            READONLY node      : Node.T;
                                     label     : CARDINAL           );

PROCEDURE SignalDeleteNode (         transUnit : CARDINAL;
                                     poolName  : TEXT;
                                     pool      : REFANY;
                                     graphNo   : CARDINAL;
                                     graph     : REFANY;
                                     isPreEvent: BOOLEAN;
                                     level     : Txn.Level;
                            READONLY node      : Node.T;
                                     label     : CARDINAL           );

PROCEDURE SignalCreateEdge (         transUnit     : CARDINAL;
                                     poolName      : TEXT;
                                     pool          : REFANY;
                                     graphNo       : CARDINAL;
                                     graph         : REFANY;
                                     isPreEvent    : BOOLEAN;
                                     level         : Txn.Level;
                            READONLY source, target: Node.T;
                                     label         : CARDINAL;
                            sourceEx, targetEx: BOOLEAN);

PROCEDURE SignalDeleteEdge (         transUnit     : CARDINAL;
                                     poolName      : TEXT;
                                     pool          : REFANY;
                                     graphNo       : CARDINAL;
                                     graph         : REFANY;
                                     isPreEvent    : BOOLEAN;
                                     level         : Txn.Level;
                            READONLY source, target: Node.T;
                                     label         : CARDINAL;
                            sourceEx, targetEx: BOOLEAN);

PROCEDURE SignalPutAttribute (         transUnit : CARDINAL;
                                       poolName  : TEXT;
                                       pool      : REFANY;
                                       graphNo   : CARDINAL;
                                       graph     : REFANY;
                                       isPreEvent: BOOLEAN;
                                       level     : Txn.Level;
                              READONLY node      : Node.T;
                                       attrno    : CARDINAL;
                                       start     : CARDINAL;
                                       value     : TEXT;
                                       nodeEx    : BOOLEAN            );

PROCEDURE SignalTruncateAttribute (         transUnit : CARDINAL;
                                            poolName  : TEXT;
                                            pool      : REFANY;
                                            graphNo   : CARDINAL;
                                            graph     : REFANY;
                                            isPreEvent: BOOLEAN;
                                            level     : Txn.Level;
                                   READONLY node      : Node.T;
                                            attrno    : CARDINAL;
                                            length    : CARDINAL;
                                            nodeEx    : BOOLEAN;           );

PROCEDURE SignalPutIndex (         transUnit : CARDINAL;
                                   poolName  : TEXT;
                                   pool      : REFANY;
                                   graphNo   : CARDINAL;
                                   graph     : REFANY;
                                   isPreEvent: BOOLEAN;
                                   level     : Txn.Level;
                          READONLY node      : Node.T;
                                   attrno    : CARDINAL;
                                   value     : TEXT;
                                   nodeEx    : BOOLEAN            );

PROCEDURE SignalDeleteIndex (         transUnit : CARDINAL;
                                      poolName  : TEXT;
                                      pool      : REFANY;
                                      graphNo   : CARDINAL;
                                      graph     : REFANY;
                                      isPreEvent: BOOLEAN;
                                      level     : Txn.Level;
                             READONLY node      : Node.T;
                                      attrno    : CARDINAL;
                                      value     : TEXT;
                                      nodeEx    : BOOLEAN            );



(* Queries for event attributes. *)
PROCEDURE GetOperation (ev: T): Operation RAISES {Unknown};
PROCEDURE GetPoolName (ev: T): TEXT RAISES {Mismatch, Unknown};
PROCEDURE GetPool (ev: T): REFANY RAISES {Mismatch, Unknown};
PROCEDURE GetGraphNo (ev: T): CARDINAL RAISES {Mismatch, Unknown};
PROCEDURE GetGraph (ev: T): REFANY RAISES {Mismatch, Unknown};
PROCEDURE GetIsPreEvent (ev: T): BOOLEAN RAISES {Mismatch, Unknown};
PROCEDURE GetLevel (ev: T): Txn.Level RAISES {Mismatch, Unknown};

(* node events *)
PROCEDURE GetNode (ev: T): Node.T RAISES {Mismatch, Unknown};
PROCEDURE GetNodeLabel (ev: T): CARDINAL RAISES {Mismatch, Unknown};

(* edge events *)
PROCEDURE GetSourceNode (ev: T): Node.T RAISES {Mismatch, Unknown};
PROCEDURE GetTargetNode (ev: T): Node.T RAISES {Mismatch, Unknown};
PROCEDURE GetEdgeLabel (ev: T): CARDINAL RAISES {Mismatch, Unknown};
PROCEDURE GetSourceNodeExists (ev: T): BOOLEAN RAISES {Mismatch, Unknown};
PROCEDURE GetTargetNodeExists (ev: T): BOOLEAN RAISES {Mismatch, Unknown};

(* attribute/index events *)
(* PROCEDURE GetNode(ev: T): Node.T RAISES {Mismatch, Unknown}; *)
PROCEDURE GetAttributeNo (ev: T): CARDINAL RAISES {Mismatch, Unknown};
PROCEDURE GetNodeExists (ev: T): BOOLEAN RAISES {Mismatch, Unknown};

PROCEDURE GetLength(ev: T): CARDINAL RAISES {Mismatch, Unknown};
  (* length for truncate attribute *)

PROCEDURE GetStart(ev: T): CARDINAL RAISES {Mismatch, Unknown};
  (* start for put attribute *)

PROCEDURE GetValue(ev: T): TEXT RAISES {Mismatch, Unknown};
  (* value for put attribute. put index and delete index *)
  
END GraphEvents.
