MODULE Patterns;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/11/07 08:58:13  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

*)
(***************************************************************************)

IMPORT EventPattern, EventTypes, EventType;

PROCEDURE Init() =
  <* FATAL EventType.Unknown *>
  BEGIN
    pats := NEW(REF ARRAY OF EventPattern.T, EventTypes.GetNumberOfTypes());
    FOR i := 0 TO LAST(pats^) DO
      pats^[i] := NEW(EventPattern.T).init(i+1);
    END;
  END Init;
    
PROCEDURE Get(type: CARDINAL): EventPattern.T =
  <* FATAL EventType.Unknown, EventTypes.Unknown, EventType.Mismatch *>
  BEGIN
    IF pats # NIL AND type <= NUMBER(pats^) THEN
      WITH res = NEW(EventPattern.T).init(type),
           etype = EventTypes.Get(type),
           pat = pats^[type-1] DO
        (* copy pattern *)
        FOR a := 1 TO etype.getNumberOfAttributes() DO
          IF NOT pat.isWildcard(a) THEN
            IF etype.isBoolAttribute(a) THEN
              res.setBoolAttribute(a, pat.getBoolAttribute(a));
            ELSIF etype.isIntAttribute(a) THEN
              res.setIntAttribute(a, pat.getIntAttribute(a));              
            ELSIF etype.isTextAttribute(a) THEN
              res.setTextAttribute(a, pat.getTextAttribute(a));
            ELSE
              res.setRefAnyAttribute(a, pat.getRefAnyAttribute(a));
            END;
          END;
        END;
        RETURN res;
      END;
    ELSE
      RETURN NIL;
    END;
  END Get;
  
PROCEDURE Set(type: CARDINAL; pattern: EventPattern.T) =
  BEGIN
    IF pats # NIL AND type <= NUMBER(pats^) THEN
      pats^[type-1] := pattern;
    END;    
  END Set;

VAR
  pats: REF ARRAY OF EventPattern.T;
  
BEGIN
END Patterns.
