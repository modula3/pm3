MODULE LogEvents EXPORTS LogEvents, PrivateLogEvents;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:45  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:29  hosking
    Import of GRAS3 1.1

    Revision 1.2  1998/08/12 11:03:22  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.1  1997/12/02 17:56:41  roland
    New event types and event contexts for user recovery operations
    introduced.

*)
(***************************************************************************)

IMPORT Event, Txn, EventTypes, EventType, RuleEngine,
       GraphEventInfos, EventDetector;
FROM EventType IMPORT Mismatch, Unknown;
IMPORT IntIntTbl, Bundle, CardSeq;

PROCEDURE NewLogEvent (op        : Operation;
                       poolName  : TEXT;
                       pool      : REFANY;
                       graphNo   : CARDINAL;
                       graph     : REFANY;
                       isPreEvent: BOOLEAN;
                       level     : CARDINAL   ): Event.T =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    WITH e = NEW(Event.T).init(TypeNumber[op]) DO
      e.setRefAnyAttribute(PoolANo, pool);
      e.setTextAttribute(PoolNameANo, poolName);
      e.setIntAttribute(GraphNoANo, graphNo);
      e.setRefAnyAttribute(GraphANo, graph);
      e.setBoolAttribute(IsPreANo, isPreEvent);
      e.setIntAttribute(LevelANo, level);
      RETURN e;
    END;
  END NewLogEvent;

PROCEDURE SignalCheckpoint (transUnit : CARDINAL;
                            poolName  : TEXT;
                            pool      : REFANY;
                            graphNo   : CARDINAL;
                            graph     : REFANY;
                            isPreEvent: BOOLEAN;
                            level     : Txn.Level) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Checkpoint]) THEN
      WITH e = NewLogEvent(Operation.Checkpoint, poolName, pool, graphNo,
			   graph, isPreEvent, level) DO
	RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalCheckpoint;

PROCEDURE SignalUndo (transUnit : CARDINAL;
                      poolName  : TEXT;
                      pool      : REFANY;
                      graphNo   : CARDINAL;
                      graph     : REFANY;
                      isPreEvent: BOOLEAN;
                      level     : Txn.Level) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Undo]) THEN
      WITH e = NewLogEvent(Operation.Undo, poolName, pool, graphNo, graph,
                           isPreEvent, level) DO
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalUndo;

PROCEDURE SignalRedo (transUnit : CARDINAL;
                      poolName  : TEXT;
                      pool      : REFANY;
                      graphNo   : CARDINAL;
                      graph     : REFANY;
                      isPreEvent: BOOLEAN;
                      level     : Txn.Level) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Redo]) THEN
      WITH e = NewLogEvent(Operation.Redo, poolName, pool, graphNo, graph,
                           isPreEvent, level) DO
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalRedo;

PROCEDURE SignalRedoNext (transUnit : CARDINAL;
                          poolName  : TEXT;
                          pool      : REFANY;
                          graphNo   : CARDINAL;
                          graph     : REFANY;
                          isPreEvent: BOOLEAN;
                          level     : Txn.Level) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.RedoNext]) THEN
      WITH e = NewLogEvent(Operation.RedoNext, poolName, pool, graphNo, graph,
                           isPreEvent, level) DO
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalRedoNext;

PROCEDURE SignalRedoPrev (transUnit : CARDINAL;
                          poolName  : TEXT;
                          pool      : REFANY;
                          graphNo   : CARDINAL;
                          graph     : REFANY;
                          isPreEvent: BOOLEAN;
                          level     : Txn.Level) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.RedoPrev]) THEN
      WITH e = NewLogEvent(Operation.RedoPrev, poolName, pool, graphNo, graph,
                           isPreEvent, level) DO
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalRedoPrev;

PROCEDURE SignalRedoIth (transUnit : CARDINAL;
                         poolName  : TEXT;
                         pool      : REFANY;
                         graphNo   : CARDINAL;
                         graph     : REFANY;
                         isPreEvent: BOOLEAN;
                         level     : Txn.Level;
                         son       : CARDINAL           ) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.RedoIth]) THEN
      WITH e = NewLogEvent(Operation.RedoIth, poolName, pool, graphNo, graph,
                           isPreEvent, level) DO
        e.setIntAttribute(SonNoANo, son);
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalRedoIth;

PROCEDURE SignalBackstep (transUnit : CARDINAL;
                          poolName  : TEXT;
                          pool      : REFANY;
                          graphNo   : CARDINAL;
                          graph     : REFANY;
                          isPreEvent: BOOLEAN;
                          level     : Txn.Level) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Backstep]) THEN
      WITH e = NewLogEvent(Operation.Backstep, poolName, pool, graphNo,
                           graph, isPreEvent, level) DO
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalBackstep;

PROCEDURE SignalForstep (transUnit : CARDINAL;
                         poolName  : TEXT;
                         pool      : REFANY;
                         graphNo   : CARDINAL;
                         graph     : REFANY;
                         isPreEvent: BOOLEAN;
                         level     : Txn.Level) =
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Forstep]) THEN
      WITH e = NewLogEvent(Operation.Forstep, poolName, pool, graphNo, graph,
                           isPreEvent, level) DO
        RuleEngine.Signal(transUnit, e);
      END;
    END;
  END SignalForstep;



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
      RETURN ev.getTextAttribute(PoolANo);
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
      RETURN ev.getTextAttribute(GraphANo);
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


(* redoIth events *)
PROCEDURE GetSonNo (ev: T): CARDINAL RAISES {Mismatch, Unknown} =
  CONST ValidOps = SET OF Operation{Operation.RedoIth};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) IN ValidOps THEN
      RETURN ev.getIntAttribute(SonNoANo);
    ELSE
      RAISE Mismatch;
    END;
  END GetSonNo;

CONST
  RedoIthInfo = "SonNo     : The relative position of the target\n"
                  & "            check point\n";

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

        IF op # Operation.RedoIth THEN
          EType[op].addInfo(genInfo);
        ELSE
          EType[op].addInfo(genInfo & RedoIthInfo);
        END;
      END;
    END;

    attrno := EType[Operation.RedoIth].addIntAttribute(SonNoAttribute);
    <* ASSERT attrno = SonNoANo *>

    WITH types = NEW(CardSeq.T).init(NUMBER(TypeNumber)) DO
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
END LogEvents.
