MODULE EventDetector;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:50  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:40  hosking
    Import of GRAS3 1.1

    Revision 1.1  1998/08/12 11:04:41  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

*)
(***************************************************************************)

IMPORT CardSeq, IntIntTransientTbl AS IntIntTbl;

REVEAL T = Public BRANDED Brand OBJECT
             counters: IntIntTbl.T;
           OVERRIDES
             init := Init;
             notifyRegistration := NotifyRegistration;
             notifyUnregistration := NotifyUnregistration;
             triggersActive := TriggersActive;
           END;
       
PROCEDURE Init (det: T; types: CardSeq.T): T =
  BEGIN
    (* initialize all counters to 0 *)
    det.counters := NEW(IntIntTbl.Default).init(sizeHint := types.size());
    FOR i := 0 TO types.size() - 1 DO
      EVAL det.counters.put(types.get(i), 0);
    END;
    RETURN det;
  END Init;
  
PROCEDURE NotifyRegistration (det: T; type: CARDINAL) =
  VAR count: INTEGER;
  BEGIN
    IF det.counters.get(type, count) THEN
      EVAL det.counters.put(type, count + 1);
    END;
  END NotifyRegistration;
  
PROCEDURE NotifyUnregistration (det: T; type: CARDINAL) =
  VAR count: INTEGER;
  BEGIN
    IF det.counters.get(type, count) THEN
      IF count > 0 THEN
        EVAL det.counters.put(type, count - 1);
      END;
    END;
  END NotifyUnregistration;
  
PROCEDURE TriggersActive (det: T; type: CARDINAL): BOOLEAN =
  VAR count: INTEGER;
  BEGIN
    IF det.counters.get(type, count) THEN
      RETURN count > 0;
    ELSE
      RETURN FALSE;
    END;
  END TriggersActive;

BEGIN
END EventDetector.
