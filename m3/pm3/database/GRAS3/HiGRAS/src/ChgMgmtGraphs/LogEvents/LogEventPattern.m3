MODULE LogEventPattern;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/12/02 17:56:40  roland
    New event types and event contexts for user recovery operations
    introduced.

*)
(***************************************************************************)

IMPORT EventPattern, LogEvents, Transaction, EventType;
FROM EventType IMPORT Mismatch, Unknown;
FROM PrivateLogEvents IMPORT TypeNumber, TypeToOp, PoolNameANo, PoolANo,
                             GraphNoANo, GraphANo, IsPreANo, LevelANo,
                             SonNoANo;


PROCEDURE Create (op: LogEvents.Operation): T =
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

PROCEDURE SetGraphNumber (p: T; number: CARDINAL)
  RAISES {Unknown, Mismatch} =
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

PROCEDURE SetLevel (p: T; level: Transaction.Level)
  RAISES {Unknown, Mismatch} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setIntAttribute(LevelANo, level);
    END;
  END SetLevel;


(* redoIth patterns *)
PROCEDURE SetSonNo (p: T; son: CARDINAL) RAISES {Mismatch, Unknown} =
  CONST ValidOps = SET OF LogEvents.Operation{LogEvents.Operation.RedoIth};
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, LogEvents.Operation) IN ValidOps THEN
      p.setIntAttribute(SonNoANo, son);
    ELSE
      RAISE Mismatch;
    END;
  END SetSonNo;


(* Attribute queries *)

PROCEDURE GetOperation (p: T): LogEvents.Operation RAISES {Unknown} =
  BEGIN
    RETURN LogEvents.GetOperation(p);
  END GetOperation;

PROCEDURE GetPoolName (p: T): TEXT RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN LogEvents.GetPoolName(p);
  END GetPoolName;

PROCEDURE GetPool (p: T): REFANY RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN LogEvents.GetPool(p);
  END GetPool;

PROCEDURE GetGraphNo (p: T): CARDINAL RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN LogEvents.GetGraphNo(p);
  END GetGraphNo;

PROCEDURE GetGraph (p: T): REFANY RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN LogEvents.GetGraph(p);
  END GetGraph;

PROCEDURE GetIsPreEvent (p: T): BOOLEAN RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN LogEvents.GetIsPreEvent(p);
  END GetIsPreEvent;

PROCEDURE GetLevel (p: T): Transaction.Level RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN LogEvents.GetLevel(p);
  END GetLevel;


(* redoIth patterns *)
PROCEDURE GetSonNo (p: T): CARDINAL RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN LogEvents.GetSonNo(p);
  END GetSonNo; 


BEGIN
END LogEventPattern.
