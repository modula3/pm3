MODULE VirtualPageEvent EXPORTS VirtualPageEvent, PrivateVirtualPageEvent;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

    Revision 1.3  1998/08/12 11:04:28  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.2  1997/11/07 09:07:09  roland
    Methods added to provide readable information on event types.

    Revision 1.1  1997/10/31 14:14:19  roland
    Subsystem introduces event and pattern types for virtual resources.

*)
(***************************************************************************)

IMPORT Event, Txn, EventTypes, EventType, RuleEngine, EventDetector;
FROM EventType IMPORT Mismatch, Unknown;
IMPORT IntIntTransientTbl AS IntIntTbl, CardSeq;

PROCEDURE SignalBegin (unit        : CARDINAL;
                       resourceName: TEXT;
                       resource    : REFANY;
                       isPreEvent  : BOOLEAN;
                       level       : Txn.Level) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Begin]) THEN
      WITH e = NEW(Event.T).init(TypeNumber[Operation.Begin]) DO
	e.setRefAnyAttribute(Resource[Operation.Begin], resource);
	e.setTextAttribute(ResourceName[Operation.Begin], resourceName);
	e.setBoolAttribute(IsPre[Operation.Begin], isPreEvent);
	e.setIntAttribute(Level[Operation.Begin], level);
	RuleEngine.Signal(unit, e);
      END;
    END;
  END SignalBegin;

PROCEDURE SignalCommit (unit        : CARDINAL;
                        resourceName: TEXT;
                        resource    : REFANY;
                        isPreEvent  : BOOLEAN;
                        level       : Txn.Level) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Commit]) THEN
      WITH e = NEW(Event.T).init(TypeNumber[Operation.Commit]) DO
	e.setRefAnyAttribute(Resource[Operation.Commit], resource);
	e.setTextAttribute(ResourceName[Operation.Commit], resourceName);
	e.setBoolAttribute(IsPre[Operation.Commit], isPreEvent);
	e.setIntAttribute(Level[Operation.Commit], level);
	RuleEngine.Signal(unit, e);
      END;
    END;
  END SignalCommit;

PROCEDURE SignalChain (unit        : CARDINAL;
                       resourceName: TEXT;
                       resource    : REFANY;
                       isPreEvent  : BOOLEAN;
                       level       : Txn.Level) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Chain]) THEN
      WITH e = NEW(Event.T).init(TypeNumber[Operation.Chain]) DO
	e.setRefAnyAttribute(Resource[Operation.Chain], resource);
	e.setTextAttribute(ResourceName[Operation.Chain], resourceName);
	e.setBoolAttribute(IsPre[Operation.Chain], isPreEvent);
	e.setIntAttribute(Level[Operation.Chain], level);
	RuleEngine.Signal(unit, e);
      END;
    END;
  END SignalChain;

PROCEDURE SignalAbort (unit        : CARDINAL;
                       resourceName: TEXT;
                       resource    : REFANY;
                       isPreEvent  : BOOLEAN;
                       level       : Txn.Level) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.Abort]) THEN
      WITH e = NEW(Event.T).init(TypeNumber[Operation.Abort]) DO
	e.setRefAnyAttribute(Resource[Operation.Abort], resource);
	e.setTextAttribute(ResourceName[Operation.Abort], resourceName);
	e.setBoolAttribute(IsPre[Operation.Abort], isPreEvent);
	e.setIntAttribute(Level[Operation.Abort], level);
	RuleEngine.Signal(unit, e);
      END;
    END;
  END SignalAbort;

PROCEDURE SignalRemoteCommit (unit        : CARDINAL;
                              resourceName: TEXT;
                              resource    : REFANY    ) =
  <* FATAL EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF Detector.triggersActive(TypeNumber[Operation.RemoteCommit]) THEN
      WITH e = NEW(Event.T).init(TypeNumber[Operation.RemoteCommit]) DO
	e.setRefAnyAttribute(Resource[Operation.RemoteCommit], resource);
	e.setTextAttribute(
	  ResourceName[Operation.RemoteCommit], resourceName);
	RuleEngine.Signal(unit, e);
      END;
    END;
  END SignalRemoteCommit;


PROCEDURE GetResourceName (ev: T): TEXT RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getTextAttribute(ResourceName[VAL(opno, Operation)]);
    END;
  END GetResourceName;

PROCEDURE GetOperation (ev: T): Operation RAISES {Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN VAL(opno, Operation);
    END;
  END GetOperation;

PROCEDURE GetIsPreEvent (ev: T): BOOLEAN RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) = Operation.RemoteCommit THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getBoolAttribute(IsPre[VAL(opno, Operation)]);
    END;
  END GetIsPreEvent;

PROCEDURE GetLevel (ev: T): Txn.Level RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, Operation) = Operation.RemoteCommit THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getIntAttribute(Level[VAL(opno, Operation)]);
    END;
  END GetLevel;

PROCEDURE GetResource (ev: T): REFANY RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(ev.type(), opno) THEN
      RAISE Unknown;
    ELSE
      RETURN ev.getRefAnyAttribute(Resource[VAL(opno, Operation)]);
    END;
  END GetResource;

CONST
  GenInfo =  "\nAttributes:\n" &
    "resourceName,\n" &
    "resource\t: the address and the name of the\n" &
    "\t\t  resource to which the operation was\n" &
    "\t\t  applied.\n" &
    "isPreEvent\t: Events can be signaled before (pre)\n" &
    "\t\t  or after (post) the operation has\n" &
    "\t\t  been performed.\n" &
    "level\t\t: the transaction-level of the\n" &
    "\t\t  started or ended transaction\n" &
    "\t\t  (i.e. never 0).\n";

  BeginInfo = "Signaled on transaction start.";
  CommitInfo = "Signaled on transaction commit.";
  ChainInfo = "Signaled on transaction chain.";
  AbortInfo = "Signaled on transaction abort.";
    
  RemoteCommitInfo =
   "The page server propagated changed pages for a\n" &
   "resource from another client. This event type\n" &
   "is frequently used to clear caches held across\n" &
   "top-level transactions.\n" &
   "Attributes:\n" &
   "resourceName,\n" &
   "resource    : the address and name of the resource.\n";

  OpInfo = ARRAY Operation OF TEXT{BeginInfo & GenInfo,
                                   CommitInfo & GenInfo,
                                   ChainInfo & GenInfo,
                                   AbortInfo & GenInfo,
                                   RemoteCommitInfo};
VAR Detector: EventDetector.T;
  
BEGIN
  <* FATAL EventTypes.Unknown, Mismatch *>
  BEGIN
    TypeToOp := NEW(IntIntTbl.Default).init();
    FOR op := FIRST(Operation) TO Operation.Abort DO
      EType[op] := EventTypes.NewEventType(EventTypeName[op]);
      (* Define attributes *)
      Resource[op] := EType[op].addRefAnyAttribute(ResourceAttribute);
      ResourceName[op] :=
        EType[op].addTextAttribute(ResourceNameAttribute);
      IsPre[op] := EType[op].addBoolAttribute(IsPreEventAttribute);
      Level[op] := EType[op].addIntAttribute(LevelAttribute);
      EType[op].addInfo(OpInfo[op]);
      EType[op].finishInitialization();
      TypeNumber[op] := EventTypes.GetNumber(EventTypeName[op]);
      EVAL TypeToOp.put(TypeNumber[op], ORD(op));
    END;
    EType[Operation.RemoteCommit] :=
      EventTypes.NewEventType(EventTypeName[Operation.RemoteCommit]);
    (* Define attributes *)
    Resource[Operation.RemoteCommit] :=
      EType[Operation.RemoteCommit].addRefAnyAttribute(ResourceAttribute);
    ResourceName[Operation.RemoteCommit] :=
      EType[Operation.RemoteCommit].addTextAttribute(ResourceNameAttribute);
    EType[Operation.RemoteCommit].addInfo(RemoteCommitInfo);
    EType[Operation.RemoteCommit].finishInitialization();
    TypeNumber[Operation.RemoteCommit] :=
      EventTypes.GetNumber(EventTypeName[Operation.RemoteCommit]);
    EVAL
      TypeToOp.put(
        TypeNumber[Operation.RemoteCommit], ORD(Operation.RemoteCommit));
  END;
  WITH types = NEW(CardSeq.T).init(NUMBER(TypeNumber)) DO
    FOR op := FIRST(Operation) TO LAST(Operation) DO
      types.addhi(TypeNumber[op]);
    END;
    Detector := NEW(EventDetector.T).init(types);
    RuleEngine.RegisterEventDetector(Detector, types);
  END;
END VirtualPageEvent.
