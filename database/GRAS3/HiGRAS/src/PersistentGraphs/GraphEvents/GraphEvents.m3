MODULE GraphEvents EXPORTS GraphEvents, PrivateGraphEvents;

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

    Revision 1.5  1998/08/12 11:04:15  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.4  1998/05/15 12:14:31  renehuel
    Bugfix in the GetNode procedure.

    Revision 1.3  1998/01/13 15:57:27  roland
    Bugfix: Graph and Pool attributes are RefAny.

    Revision 1.2  1997/11/21 09:37:16  roland
    New GraphEvents PutAttribute and TruncateAttribute replace ModifyAttribute

    Revision 1.1  1997/11/10 10:46:31  roland
    Graph event type definitions and handling.

*)
(***************************************************************************)

IMPORT Event, Txn, EventTypes, EventType, RuleEngine,
       GraphEventInfos, Node, EventDetector;
FROM EventType IMPORT Mismatch, Unknown;
IMPORT IntIntTbl, Bundle, CardSeq;

PROCEDURE NewGraphEvent (op        : Operation;
                         poolName  : TEXT;
                         pool      : REFANY;
                         graphNo   : CARDINAL;
                         graph     : REFANY;
                         isPreEvent: BOOLEAN;
                         level     : CARDINAL;
                         firstNode : CARDINAL;
                         firstLabel: CARDINAL   ): Event.T =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    WITH e = NEW(Event.T).init(TypeNumber[op]) DO
      e.setRefAnyAttribute(PoolANo, pool);
      e.setTextAttribute(PoolNameANo, poolName);
      e.setIntAttribute(GraphNoANo, graphNo);
      e.setRefAnyAttribute(GraphANo, graph);
      e.setBoolAttribute(IsPreANo, isPreEvent);
      e.setIntAttribute(LevelANo, level);
      e.setIntAttribute(FirstNodeANo, firstNode);
      e.setIntAttribute(FirstLabelANo, firstLabel);
      RETURN e;
    END;
  END NewGraphEvent;

PROCEDURE SignalCreateNode (         transUnit : CARDINAL;
                                     poolName  : TEXT;
                                     pool      : REFANY;
                                     graphNo   : CARDINAL;
                                     graph     : REFANY;
                                     isPreEvent: BOOLEAN;
                                     level     : Txn.Level;
                            READONLY node      : Node.T;
                                     label     : CARDINAL           ) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.CreateNode]) THEN
      WITH e = NewGraphEvent(Operation.CreateNode, poolName, pool, graphNo,
                             graph, isPreEvent, level, node.entity, label) DO
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalCreateNode;

PROCEDURE SignalDeleteNode (         transUnit : CARDINAL;
                                     poolName  : TEXT;
                                     pool      : REFANY;
                                     graphNo   : CARDINAL;
                                     graph     : REFANY;
                                     isPreEvent: BOOLEAN;
                                     level     : Txn.Level;
                            READONLY node      : Node.T;
                                     label     : CARDINAL           ) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.DeleteNode]) THEN
      WITH e = NewGraphEvent(Operation.DeleteNode, poolName, pool, graphNo,
                             graph, isPreEvent, level, node.entity, label) DO
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalDeleteNode;

PROCEDURE SignalCreateEdge (         transUnit     : CARDINAL;
                                     poolName      : TEXT;
                                     pool          : REFANY;
                                     graphNo       : CARDINAL;
                                     graph         : REFANY;
                                     isPreEvent    : BOOLEAN;
                                     level         : Txn.Level;
                            READONLY source, target: Node.T;
                                     label         : CARDINAL;
                            sourceEx, targetEx: BOOLEAN) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.CreateEdge]) THEN
      WITH e = NewGraphEvent(Operation.CreateEdge, poolName, pool, graphNo,
                             graph, isPreEvent, level, source.entity, label) DO
        e.setIntAttribute(SourceGraphANo, source.graph);
        e.setIntAttribute(TargetGraphANo, target.graph);
        e.setIntAttribute(TargetNodeANo, target.entity);
        e.setBoolAttribute(FirstNodeExANo, sourceEx);
        e.setBoolAttribute(TargetNodeExANo, targetEx);
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalCreateEdge;

PROCEDURE SignalDeleteEdge (         transUnit     : CARDINAL;
                                     poolName      : TEXT;
                                     pool          : REFANY;
                                     graphNo       : CARDINAL;
                                     graph         : REFANY;
                                     isPreEvent    : BOOLEAN;
                                     level         : Txn.Level;
                            READONLY source, target: Node.T;
                                     label         : CARDINAL;
                            sourceEx, targetEx: BOOLEAN) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.DeleteEdge]) THEN
      WITH e = NewGraphEvent(Operation.DeleteEdge, poolName, pool, graphNo,
                             graph, isPreEvent, level, source.entity, label) DO
        e.setIntAttribute(SourceGraphANo, source.graph);
        e.setIntAttribute(TargetGraphANo, target.graph);
        e.setIntAttribute(TargetNodeANo, target.entity);
        e.setBoolAttribute(FirstNodeExANo, sourceEx);
        e.setBoolAttribute(TargetNodeExANo, targetEx);
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalDeleteEdge;

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
                                       nodeEx    : BOOLEAN            ) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.PutAttribute]) THEN
      WITH e = NewGraphEvent(Operation.PutAttribute, poolName, pool, graphNo,
                             graph, isPreEvent, level, node.entity, attrno) DO
        e.setBoolAttribute(FirstNodeExANo, nodeEx);
        e.setIntAttribute(AttrStartANo, start);
        e.setTextAttribute(TextANo, value);
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalPutAttribute;

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
                                            nodeEx    : BOOLEAN            ) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.TruncateAttribute]) THEN
      WITH e = NewGraphEvent(
                 Operation.TruncateAttribute, poolName, pool, graphNo, graph,
                 isPreEvent, level, node.entity, attrno) DO
        e.setBoolAttribute(FirstNodeExANo, nodeEx);
        e.setIntAttribute(AttrLengthANo, length);
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalTruncateAttribute;

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
                                   nodeEx    : BOOLEAN            ) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.PutIndex]) THEN
      WITH e = NewGraphEvent(Operation.PutIndex, poolName, pool, graphNo,
                             graph, isPreEvent, level, node.entity, attrno) DO
        e.setBoolAttribute(FirstNodeExANo, nodeEx);
        e.setTextAttribute(TextANo, value);
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalPutIndex;

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
                                      nodeEx    : BOOLEAN            ) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.DeleteIndex]) THEN
      WITH e = NewGraphEvent(Operation.DeleteIndex, poolName, pool, graphNo,
                             graph, isPreEvent, level, node.entity, attrno) DO
        e.setBoolAttribute(FirstNodeExANo, nodeEx);
        e.setTextAttribute(TextANo, value);
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalDeleteIndex;


PROCEDURE GetOperation (ev: T): Operation RAISES {Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN VAL(opno, Operation);
    END;
  END GetOperation;


PROCEDURE GetPoolName (ev: T): TEXT RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getTextAttribute(PoolNameANo);
    END;
  END GetPoolName;

PROCEDURE GetPool (ev: T): REFANY RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getRefAnyAttribute(PoolANo);
    END;
  END GetPool;

PROCEDURE GetGraphNo (ev: T): CARDINAL RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getIntAttribute(GraphNoANo);
    END;
  END GetGraphNo;

PROCEDURE GetGraph (ev: T): REFANY RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getRefAnyAttribute(GraphANo);
    END;
  END GetGraph;

PROCEDURE GetIsPreEvent (ev: T): BOOLEAN RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getBoolAttribute(IsPreANo);
    END;
  END GetIsPreEvent;

PROCEDURE GetLevel (ev: T): Txn.Level RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getIntAttribute(LevelANo);
    END;
  END GetLevel;


(* node events *)
PROCEDURE GetNode (ev: T): Node.T RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 Operation{
                 Operation.CreateNode, Operation.DeleteNode,
                 Operation.PutAttribute, Operation.TruncateAttribute,
                 Operation.DeleteIndex, Operation.PutIndex};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN Node.T{ev.getIntAttribute(GraphNoANo),
                    ev.getIntAttribute(FirstNodeANo)};
    ELSE
      RAISE Mismatch;
    END;
  END GetNode;

PROCEDURE GetNodeLabel (ev: T): CARDINAL RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF Operation{Operation.CreateNode, Operation.DeleteNode};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getIntAttribute(FirstLabelANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetNodeLabel;


(* edge events *)
PROCEDURE GetSourceNode (ev: T): Node.T RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF Operation{Operation.CreateEdge, Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN Node.T{ev.getIntAttribute(SourceGraphANo),
                    ev.getIntAttribute(FirstNodeANo)};
    ELSE
      RAISE Mismatch;
    END;
  END GetSourceNode;

PROCEDURE GetTargetNode (ev: T): Node.T RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF Operation{Operation.CreateEdge, Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN Node.T{ev.getIntAttribute(TargetGraphANo),
                    ev.getIntAttribute(TargetNodeANo)};
    ELSE
      RAISE Mismatch;
    END;
  END GetTargetNode;

PROCEDURE GetEdgeLabel (ev: T): CARDINAL RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF Operation{Operation.CreateEdge, Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getIntAttribute(FirstLabelANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetEdgeLabel;

PROCEDURE GetSourceNodeExists (ev: T): BOOLEAN RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF Operation{Operation.CreateEdge, Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getBoolAttribute(FirstNodeExANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetSourceNodeExists;

PROCEDURE GetTargetNodeExists (ev: T): BOOLEAN RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF Operation{Operation.CreateEdge, Operation.DeleteEdge};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getBoolAttribute(TargetNodeExANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetTargetNodeExists;


(* attribute/index events *)
PROCEDURE GetAttributeNo (ev: T): CARDINAL RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 Operation{
                 Operation.PutAttribute, Operation.TruncateAttribute,
                 Operation.PutIndex, Operation.DeleteIndex};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getIntAttribute(FirstLabelANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetAttributeNo;

PROCEDURE GetNodeExists (ev: T): BOOLEAN RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 Operation{
                 Operation.PutAttribute, Operation.TruncateAttribute,
                 Operation.PutIndex, Operation.DeleteIndex};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getBoolAttribute(FirstNodeExANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetNodeExists;


PROCEDURE GetLength (ev: T): CARDINAL RAISES {Mismatch, Unknown} =
  CONST ValidOps = SET OF Operation{Operation.TruncateAttribute};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getIntAttribute(AttrLengthANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetLength;

PROCEDURE GetStart (ev: T): CARDINAL RAISES {Mismatch, Unknown} =
  CONST ValidOps = SET OF Operation{Operation.PutAttribute};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getIntAttribute(AttrStartANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetStart;

PROCEDURE GetValue (ev: T): TEXT RAISES {Mismatch, Unknown} =
  CONST
    ValidOps = SET OF
                 Operation{Operation.PutAttribute, Operation.PutIndex,
                           Operation.DeleteIndex};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getTextAttribute(TextANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetValue;

CONST
  InfoName = ARRAY Operation OF
               TEXT{"NodeEventInfo", "NodeEventInfo", "EdgeEventInfo",
                    "EdgeEventInfo", "AttrEventInfo", "AttrEventInfo",
                    "AttrEventInfo", "AttrEventInfo"};
VAR
  Detector: EventDetector.T;
  
BEGIN
  VAR attrno: CARDINAL;
  <* FATAL EventTypes.Unknown, Mismatch *>
  BEGIN
    TypeToOp := NEW(IntIntTbl.Default).init();
    WITH bundle  = GraphEventInfos.Get(),
         genInfo = Bundle.Get(bundle, "GraphEventInfo") DO
      FOR op := FIRST(Operation) TO LAST(Operation) DO
        EType[op] := EventTypes.NewEventType(EventTypeName[op]);
        (* Define attributes *)
        attrno := EType[op].addTextAttribute(PoolNameAttribute);
        <* ASSERT attrno = PoolNameANo *>
        attrno := EType[op].addRefAnyAttribute(PoolAttribute);
        <* ASSERT attrno = PoolANo *>
        attrno := EType[op].addIntAttribute(GraphNumberAttribute);
        <* ASSERT attrno = GraphNoANo *>
        attrno := EType[op].addRefAnyAttribute(GraphAttribute);
        <* ASSERT attrno = GraphANo *>
        attrno := EType[op].addBoolAttribute(IsPreEventAttribute);
        <* ASSERT attrno = IsPreANo *>
        attrno := EType[op].addIntAttribute(LevelAttribute);
        <* ASSERT attrno = LevelANo *>

        EType[op].addInfo(genInfo & Bundle.Get(bundle, InfoName[op]));
      END;
    END;

    (* special attribtues for node events *)
    FOR op := Operation.CreateNode TO Operation.DeleteNode DO
      attrno := EType[op].addIntAttribute(NodeAttribute);
      <* ASSERT attrno = FirstNodeANo *>
      attrno := EType[op].addIntAttribute(NodeLabelAttribute);
      <* ASSERT attrno = FirstLabelANo *>
    END;

    (* special attribtues for edge events *)
    FOR op := Operation.CreateEdge TO Operation.DeleteEdge DO
      attrno := EType[op].addIntAttribute(SourceNodeAttribute);
      <* ASSERT attrno = FirstNodeANo *>
      attrno := EType[op].addIntAttribute(EdgeLabelAttribute);
      <* ASSERT attrno = FirstLabelANo *>
      attrno := EType[op].addBoolAttribute(SourceExistsAttribute);
      <* ASSERT attrno = FirstNodeExANo *>
      attrno := EType[op].addIntAttribute(TargetNodeAttribute);
      <* ASSERT attrno = TargetNodeANo *>
      attrno := EType[op].addBoolAttribute(TargetExistsAttribute);
      <* ASSERT attrno = TargetNodeExANo *>
      attrno := EType[op].addIntAttribute(TargetGraphAttribute);
      <* ASSERT attrno = TargetGraphANo *>
      attrno := EType[op].addIntAttribute(SourceGraphAttribute);
      <* ASSERT attrno = SourceGraphANo *>
    END;

    (* special attribtues for attribute events *)
    FOR op := Operation.PutAttribute TO Operation.DeleteIndex DO
      attrno := EType[op].addIntAttribute(NodeAttribute);
      <* ASSERT attrno = FirstNodeANo *>
      attrno := EType[op].addIntAttribute(AttributeNoAttribute);
      <* ASSERT attrno = FirstLabelANo *>
      attrno := EType[op].addBoolAttribute(NodeExistsAttribute);
      <* ASSERT attrno = FirstNodeExANo *>
    END;

    attrno :=
      EType[Operation.PutAttribute].addTextAttribute(ValueAttribute);
    <* ASSERT attrno = TextANo *>
    attrno := EType[Operation.PutAttribute].addIntAttribute(
                PutAttributeStartAttribute);
    <* ASSERT attrno = AttrStartANo *>
    attrno := EType[Operation.TruncateAttribute].addIntAttribute(
                TruncateLengthAttribute);
    <* ASSERT attrno = AttrLengthANo *>
    attrno := EType[Operation.PutIndex].addTextAttribute(ValueAttribute);
    <* ASSERT attrno = TextANo *>
    attrno :=
      EType[Operation.DeleteIndex].addTextAttribute(ValueAttribute);
    <* ASSERT attrno = TextANo *>

    WITH types = NEW(CardSeq.T).init(NUMBER(EType)) DO
      (* finish initialization *)
      FOR op := FIRST(Operation) TO LAST(Operation) DO
        EType[op].finishInitialization();
        TypeNumber[op] := EventTypes.GetNumber(EventTypeName[op]);
        EVAL TypeToOp.put(TypeNumber[op], ORD(op));
        types.addhi(TypeNumber[op]);
      END;
      Detector := NEW(EventDetector.T).init(types);
      RuleEngine.RegisterEventDetector(Detector, types);
    END;
  END;
END GraphEvents.
