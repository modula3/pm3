INTERFACE GraphEventPattern;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.3  1997/11/21 09:37:13  roland
    New GraphEvents PutAttribute and TruncateAttribute replace ModifyAttribute

    Revision 1.2  1997/11/12 15:23:59  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

    Revision 1.1  1997/11/10 10:46:28  roland
    Graph event type definitions and handling.

*)
(***************************************************************************)

IMPORT EventPattern, GraphEvents, Transaction, Node;
FROM EventType IMPORT Unknown, Mismatch;

TYPE
  T = EventPattern.T;
    (* GraphEventPatterns have the same attributes as
       GraphEvents. *)

PROCEDURE Create (op: GraphEvents.Operation): T;
  (* Create an event pattern.  All its attributes are set to wildcards *)

(* Updates to pattern attributes *)
PROCEDURE SetPoolName (p: T; name: TEXT) RAISES {Unknown, Mismatch};
PROCEDURE SetPool (p: T; pool: REFANY) RAISES {Unknown, Mismatch};
PROCEDURE SetGraphNumber (p: T; number: CARDINAL) RAISES {Unknown, Mismatch};
PROCEDURE SetGraph (p: T; graph: REFANY) RAISES {Unknown, Mismatch};
PROCEDURE SetPreEvent (p: T; ispre: BOOLEAN) RAISES {Unknown, Mismatch};
PROCEDURE SetLevel (p: T; level: Transaction.Level)
  RAISES {Unknown, Mismatch};

(* node events *)
PROCEDURE SetNode (p: T; node: Node.T) RAISES {Mismatch, Unknown};
PROCEDURE SetNodeLabel (p: T; label: CARDINAL) RAISES {Mismatch, Unknown};

(* edge events *)
PROCEDURE SetSourceNode (p: T; source: Node.T) RAISES {Mismatch, Unknown};
PROCEDURE SetTargetNode (p: T; target: Node.T) RAISES {Mismatch, Unknown};
PROCEDURE SetEdgeLabel (p: T; label: CARDINAL) RAISES {Mismatch, Unknown};
PROCEDURE SetSourceNodeExists (p: T; ex: BOOLEAN) RAISES {Mismatch, Unknown};
PROCEDURE SetTargetNodeExists (p: T; ex: BOOLEAN) RAISES {Mismatch, Unknown};

(* attribute/index events *)
(* PROCEDURE SetNode(p: T; node: Node.T) RAISES {Mismatch, Unknown}; *)
PROCEDURE SetAttributeNo (p: T; no: CARDINAL) RAISES {Mismatch, Unknown};
PROCEDURE SetNodeExists (p: T; ex: BOOLEAN) RAISES {Mismatch, Unknown};

PROCEDURE SetValue(p: T; val: TEXT) RAISES {Mismatch, Unknown};
PROCEDURE SetStart(p: T; start: CARDINAL) RAISES {Mismatch, Unknown};
PROCEDURE SetLength(p: T; length: CARDINAL) RAISES {Mismatch, Unknown};

  
(* Attribute queries *)

PROCEDURE GetOperation (p: T): GraphEvents.Operation RAISES {Unknown};
PROCEDURE GetPoolName (p: T): TEXT RAISES {Mismatch, Unknown};
PROCEDURE GetPool (p: T): REFANY RAISES {Mismatch, Unknown};
PROCEDURE GetGraphNo (p: T): CARDINAL RAISES {Mismatch, Unknown};
PROCEDURE GetGraph (p: T): REFANY RAISES {Mismatch, Unknown};
PROCEDURE GetIsPreEvent (p: T): BOOLEAN RAISES {Mismatch, Unknown};
PROCEDURE GetLevel (p: T): Transaction.Level RAISES {Mismatch, Unknown};

(* node events *)
PROCEDURE GetNode (p: T): Node.T RAISES {Mismatch, Unknown};
PROCEDURE GetNodeLabel (p: T): CARDINAL RAISES {Mismatch, Unknown};

(* edge events *)
PROCEDURE GetSourceNode (p: T): Node.T RAISES {Mismatch, Unknown};
PROCEDURE GetTargetNode (p: T): Node.T RAISES {Mismatch, Unknown};
PROCEDURE GetEdgeLabel (p: T): CARDINAL RAISES {Mismatch, Unknown};
PROCEDURE GetSourceNodeExists (p: T): BOOLEAN RAISES {Mismatch, Unknown};
PROCEDURE GetTargetNodeExists (p: T): BOOLEAN RAISES {Mismatch, Unknown};

(* attribute/index events *)
(* PROCEDURE GetNode(p: T): Node.T RAISES {Mismatch, Unknown}; *)
PROCEDURE GetAttributeNo (p: T): CARDINAL RAISES {Mismatch, Unknown};
PROCEDURE GetNodeExists (p: T): BOOLEAN RAISES {Mismatch, Unknown};

PROCEDURE GetLength(ev: T): CARDINAL RAISES {Mismatch, Unknown};
  (* length for truncate attribute *)

PROCEDURE GetStart(ev: T): CARDINAL RAISES {Mismatch, Unknown};
  (* start for put attribute *)

PROCEDURE GetValue(ev: T): TEXT RAISES {Mismatch, Unknown};
  (* value for put attribute. put index and delete index *)
  
END GraphEventPattern.
