MODULE MonitoredTypes;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:28:30  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

IMPORT IntSeq;

PROCEDURE Insert(type: CARDINAL) =
  VAR h, prev: REF Elem;
  BEGIN
    IF List = NIL OR List.type > type THEN
      List := NEW(REF Elem, type := type, next := List);
      INC(Num);
    ELSE
      h := List;
      WHILE h # NIL AND h^.type < type DO
	prev := h;
	h := h^.next;
      END;
      IF h = NIL OR h^.type # type THEN
        prev^.next := NEW(REF Elem, type := type, next := h);
        INC(Num);
      END;
    END;
  END Insert;
  
PROCEDURE Remove(type: CARDINAL) =
  VAR h, prev: REF Elem;
  BEGIN
    h := List;
    WHILE h # NIL AND h^.type < type DO
      prev := h;
      h := h^.next;
    END;
    IF h # NIL AND h^.type = type THEN
      IF h = List THEN
        List := h^.next;
      ELSE
        prev^.next := h^.next;
      END;
      DEC(Num);
    END;
  END Remove;
  
PROCEDURE IsMonitored(type: CARDINAL): BOOLEAN =
  VAR h: REF Elem;
  BEGIN
    h := List;
    WHILE h # NIL AND h^.type < type DO
      h := h^.next;
    END;
    RETURN h # NIL AND h^.type = type;
  END IsMonitored;

PROCEDURE Number(): CARDINAL =
  BEGIN
    RETURN Num;
  END Number;
  
PROCEDURE Get(): IntSeq.T =
  VAR res := NEW(IntSeq.T).init();
      h: REF Elem;
  BEGIN
    h := List;
    WHILE h # NIL DO
      res.addhi(h^.type);
      h := h^.next;
    END;
    RETURN res;
  END Get;

TYPE
  Elem = RECORD
           type: CARDINAL;
           next: REF Elem;
         END;

VAR List: REF Elem := NIL;
    Num: CARDINAL := 0;
  
BEGIN
END MonitoredTypes.
