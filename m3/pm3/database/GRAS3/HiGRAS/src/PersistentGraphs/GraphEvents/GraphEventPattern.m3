MODULE GraphEventPattern;

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

    Revision 1.3  1997/11/21 09:37:14  roland
    New GraphEvents PutAttribute and TruncateAttribute replace ModifyAttribute

    Revision 1.2  1997/11/12 15:24:00  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

    Revision 1.1  1997/11/10 10:46:29  roland
    Graph event type definitions and handling.

*)
(***************************************************************************)

IMPORT EventPattern, GraphEvents, Txn, EventType, Node;
FROM EventType IMPORT Mismatch, Unknown;
FROM PrivateGraphEvents IMPORT TypeNumber, TypeToOp, PoolNameANo, PoolANo,
                               GraphNoANo, GraphANo, IsPreANo, LevelANo,
                               FirstNodeANo, FirstLabelANo, FirstNodeExANo,
                               TargetNodeANo, TargetNodeExANo,
                               TargetGraphANo, SourceGraphANo,
                               TextANo, AttrLengthANo, AttrStartANo;


PROCEDURE Create (op: GraphEvents.Operation): T =
  <* FATAL EventType.Unknown *>
  BEGIN
    RETURN NEW(EventPattern.T).init(TypeNumber[op]);
  END Create;


(* Updates to pattern attributes *)
PROCEDURE SetPoolName (p: T; name: TEXT) RAISES {Unknown, Mismatch} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setTextAttribute(PoolNameANo, name);
    END;
  END SetPoolName;

PROCEDURE SetPool (p: T; pool: REFANY) RAISES {Unknown, Mismatch} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setRefAnyAttribute(PoolANo, pool);
    END;
  END SetPool;

PROCEDURE SetGraphNumber (p: T; number: CARDINAL) RAISES {Unknown, Mismatch} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setIntAttribute(GraphNoANo, number);
    END;
  END SetGraphNumber;

PROCEDURE SetGraph (p: T; graph: REFANY) RAISES {Unknown, Mismatch} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setRefAnyAttribute(GraphANo, graph);
    END;
  END SetGraph;

PROCEDURE SetPreEvent (p: T; ispre: BOOLEAN) RAISES {Unknown, Mismatch} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setBoolAttribute(IsPreANo, ispre);
    END;
  END SetPreEvent;

PROCEDURE SetLevel (p: T; level: Txn.Level)
  RAISES {Unknown, Mismatch} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setIntAttribute(LevelANo, level);
    END;
  END SetLevel;


(* node events *)
PROCEDURE SetNode (p: T; node: Node.T) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.CreateNode,
                 GraphEvents.Operation.DeleteNode,
                 GraphEvents.Operation.PutAttribute,
                 GraphEvents.Operation.TruncateAttribute,
                 GraphEvents.Operation.DeleteIndex,
                 GraphEvents.Operation.PutIndex};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(FirstNodeANo, node.entity);
      p.setIntAttribute(GraphANo, node.graph);
    ELSE
      RAISE Mismatch;
    END;
  END SetNode;

PROCEDURE SetNodeLabel (p: T; label: CARDINAL) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.CreateNode,
                 GraphEvents.Operation.DeleteNode};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(FirstLabelANo, label);
    ELSE
      RAISE Mismatch;
    END;
  END SetNodeLabel;

(* edge events *)
PROCEDURE SetSourceNode (p: T; source: Node.T) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.CreateEdge,
                 GraphEvents.Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(SourceGraphANo, source.graph);
      p.setIntAttribute(FirstNodeANo, source.entity);
    ELSE
      RAISE Mismatch;
    END;
  END SetSourceNode;

PROCEDURE SetTargetNode (p: T; target: Node.T) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.CreateEdge,
                 GraphEvents.Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(TargetGraphANo, target.graph);
      p.setIntAttribute(TargetNodeANo, target.entity);
    ELSE
      RAISE Mismatch;
    END;
  END SetTargetNode;

PROCEDURE SetEdgeLabel (p: T; label: CARDINAL) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.CreateEdge,
                 GraphEvents.Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(FirstLabelANo, label);
    ELSE
      RAISE Mismatch;
    END;
  END SetEdgeLabel;

PROCEDURE SetSourceNodeExists (p: T; ex: BOOLEAN)
  RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.CreateEdge,
                 GraphEvents.Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setBoolAttribute(FirstNodeExANo, ex);
    ELSE
      RAISE Mismatch;
    END;
  END SetSourceNodeExists;

PROCEDURE SetTargetNodeExists (p: T; ex: BOOLEAN)
  RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.CreateEdge,
                 GraphEvents.Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setBoolAttribute(TargetNodeExANo, ex);
    ELSE
      RAISE Mismatch;
    END;
  END SetTargetNodeExists;


(* attribute/index events *)
PROCEDURE SetAttributeNo (p: T; no: CARDINAL) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.PutAttribute,
                 GraphEvents.Operation.TruncateAttribute,
                 GraphEvents.Operation.PutIndex,
                 GraphEvents.Operation.DeleteIndex};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(FirstLabelANo, no);
    ELSE
      RAISE Mismatch;
    END;
  END SetAttributeNo;

PROCEDURE SetNodeExists (p: T; ex: BOOLEAN) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.PutAttribute,
                 GraphEvents.Operation.TruncateAttribute,
                 GraphEvents.Operation.PutIndex,
                 GraphEvents.Operation.DeleteIndex};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setBoolAttribute(FirstNodeExANo, ex);
    ELSE
      RAISE Mismatch;
    END;
  END SetNodeExists;


PROCEDURE SetValue(p: T; val: TEXT) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.PutAttribute,
                 GraphEvents.Operation.PutIndex,
                 GraphEvents.Operation.DeleteIndex};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setTextAttribute(TextANo, val);
    ELSE
      RAISE Mismatch;
    END;
  END SetValue;
  
PROCEDURE SetStart(p: T; start: CARDINAL) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.PutAttribute};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(AttrStartANo, start);
    ELSE
      RAISE Mismatch;
    END;
  END SetStart;
  
PROCEDURE SetLength(p: T; length: CARDINAL) RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 GraphEvents.Operation{
                 GraphEvents.Operation.TruncateAttribute};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, GraphEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(AttrLengthANo, length);
    ELSE
      RAISE Mismatch;
    END;
  END SetLength;


(* Attribute queries *)

PROCEDURE GetOperation (p: T): GraphEvents.Operation RAISES {Unknown} =
  BEGIN
    RETURN GraphEvents.GetOperation(p);
  END GetOperation;

PROCEDURE GetPoolName (p: T): TEXT RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetPoolName(p);
  END GetPoolName;

PROCEDURE GetPool (p: T): REFANY RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetPool(p);
  END GetPool;

PROCEDURE GetGraphNo (p: T): CARDINAL RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetGraphNo(p);
  END GetGraphNo;

PROCEDURE GetGraph (p: T): REFANY RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetGraph(p);
  END GetGraph;

PROCEDURE GetIsPreEvent (p: T): BOOLEAN RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetIsPreEvent(p);
  END GetIsPreEvent;

PROCEDURE GetLevel (p: T): Txn.Level RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetLevel(p);
  END GetLevel;


(* node events *)
PROCEDURE GetNode (p: T): Node.T RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetNode(p);
  END GetNode;

PROCEDURE GetNodeLabel (p: T): CARDINAL RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetNodeLabel(p);
  END GetNodeLabel;


(* edge events *)
PROCEDURE GetSourceNode (p: T): Node.T RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetSourceNode(p);
  END GetSourceNode;

PROCEDURE GetTargetNode (p: T): Node.T RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetTargetNode(p);
  END GetTargetNode;

PROCEDURE GetEdgeLabel (p: T): CARDINAL RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetEdgeLabel(p);
  END GetEdgeLabel;

PROCEDURE GetSourceNodeExists (p: T): BOOLEAN RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetSourceNodeExists(p);
  END GetSourceNodeExists;

PROCEDURE GetTargetNodeExists (p: T): BOOLEAN RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetTargetNodeExists(p);
  END GetTargetNodeExists;


(* attribute/index events *)
PROCEDURE GetAttributeNo (p: T): CARDINAL RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetAttributeNo(p);
  END GetAttributeNo;

PROCEDURE GetNodeExists (p: T): BOOLEAN RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetNodeExists(p);
  END GetNodeExists;

PROCEDURE GetLength(p: T): CARDINAL RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetLength(p);
  END GetLength;

PROCEDURE GetStart(p: T): CARDINAL RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetStart(p);
  END GetStart;

PROCEDURE GetValue(p: T): TEXT RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN GraphEvents.GetValue(p);
  END GetValue;

BEGIN
END GraphEventPattern.
