MODULE VirtualPageEventPattern;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:14:20  roland
    Subsystem introduces event and pattern types for virtual resources.

*)
(***************************************************************************)

IMPORT EventPattern, VirtualPageEvent, Transaction, EventType;
FROM EventType IMPORT Mismatch, Unknown;
FROM PrivateVirtualPageEvent IMPORT TypeNumber, TypeToOp, Resource,
                                    ResourceName, Level, IsPre;

PROCEDURE Create (op: VirtualPageEvent.Operation): T =
  <* FATAL EventType.Unknown *>
  BEGIN
    RETURN NEW(EventPattern.T).init(TypeNumber[op]);
  END Create;

(* Updates to pattern attributes *)
PROCEDURE SetResourceName (p: T; name: TEXT) RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setTextAttribute(
        ResourceName[VAL(opno, VirtualPageEvent.Operation)], name);
    END;
  END SetResourceName;

PROCEDURE SetPreEvent (p: T; ispre: BOOLEAN) RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, VirtualPageEvent.Operation)
            = VirtualPageEvent.Operation.RemoteCommit THEN
      RAISE Unknown
    ELSE
      p.setBoolAttribute(
        IsPre[VAL(opno, VirtualPageEvent.Operation)], ispre);
    END;
  END SetPreEvent;

PROCEDURE SetLevel (p: T; level: Transaction.Level)
  RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSIF VAL(opno, VirtualPageEvent.Operation)
            = VirtualPageEvent.Operation.RemoteCommit THEN
      RAISE Unknown
    ELSE
      p.setIntAttribute(
        Level[VAL(opno, VirtualPageEvent.Operation)], level);
    END;
  END SetLevel;

PROCEDURE SetResource (p: T; resource: REFANY) RAISES {Mismatch, Unknown} =
  VAR opno: INTEGER;
  BEGIN
    IF NOT TypeToOp.get(p.type(), opno) THEN
      RAISE Unknown;
    ELSE
      p.setRefAnyAttribute(
        Resource[VAL(opno, VirtualPageEvent.Operation)], resource);
    END;
  END SetResource;


(* Queries for pattern attributes. *)
PROCEDURE GetResourceName (p: T): TEXT RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN VirtualPageEvent.GetResourceName(p);
  END GetResourceName;

PROCEDURE GetOperation (p: T): VirtualPageEvent.Operation
  RAISES {Unknown} =
  BEGIN
    RETURN VirtualPageEvent.GetOperation(p);
  END GetOperation;

PROCEDURE GetIsPreEvent (p: T): BOOLEAN RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN VirtualPageEvent.GetIsPreEvent(p);
  END GetIsPreEvent;

PROCEDURE GetLevel (p: T): Transaction.Level RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN VirtualPageEvent.GetLevel(p);
  END GetLevel;

PROCEDURE GetResource (p: T): REFANY RAISES {Mismatch, Unknown} =
  BEGIN
    RETURN VirtualPageEvent.GetResource(p);
  END GetResource;

BEGIN
END VirtualPageEventPattern.
