GENERIC MODULE PriorityQueue(Element);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:08:26  roland
    New generic implementations for PriorityQueues. New templates and a new
    order function for Cardinal.

*)
(***************************************************************************)

CONST InitialSize = 20;

TYPE PriorityQueueArray = REF ARRAY OF Element.T;

REVEAL
  T = Public BRANDED OBJECT
        N   : CARDINAL;
        cont: PriorityQueueArray;
      OVERRIDES
        init    := Init;
        isEmpty := IsEmpty;
        highest := Highest;
        insert  := Insert;
        get     := Get;
      END;

PROCEDURE Init (q: T): T =
  BEGIN
    q.cont := NEW(PriorityQueueArray, InitialSize);
    RETURN q;
  END Init;

PROCEDURE Extend (q: T) =
  VAR new: PriorityQueueArray;
  BEGIN
    new := NEW(PriorityQueueArray, 2 * NUMBER(q.cont^));
    SUBARRAY(new^, 0, NUMBER(q.cont^)) := q.cont^;
    q.cont := new;
  END Extend;

PROCEDURE UpHeap (q: T; k: CARDINAL) =
  VAR act: Element.T := q.cont^[k];
  BEGIN
    WHILE k > 1 AND Element.PrioLess(Element.Priority(q.cont^[k DIV 2]),
                                     Element.Priority(act)) DO
      q.cont^[k] := q.cont^[k DIV 2];
      k := k DIV 2;
    END;
    q.cont^[k] := act;
  END UpHeap;

PROCEDURE DownHeap (q: T; k: CARDINAL) =
  VAR
    j  : CARDINAL;
    act           := q.cont^[k];
  BEGIN
    WHILE k <= q.N DIV 2 DO
      j := k + k;
      IF j < q.N AND Element.PrioLess(Element.Priority(q.cont^[j]),
                                      Element.Priority(q.cont^[j + 1])) THEN
        INC(j);
      END;
      IF NOT Element.PrioLess(
               Element.Priority(act), Element.Priority(q.cont^[j])) THEN
        EXIT
      END;
      q.cont^[k] := q.cont^[j];
      k := j;
    END;
    q.cont^[k] := act;
  END DownHeap;

PROCEDURE IsEmpty (q: T): BOOLEAN =
  BEGIN
    RETURN q.N = 0;
  END IsEmpty;

PROCEDURE Highest (q: T): Element.PriorityType =
  BEGIN
    IF q.N = 0 THEN <* ASSERT FALSE *>
    ELSE
      RETURN Element.Priority(q.cont^[1])
    END;
  END Highest;

PROCEDURE Insert (q: T; elem: Element.T) =
  BEGIN
    IF q.N = LAST(q.cont^) THEN Extend(q); END;
    INC(q.N);
    q.cont^[q.N] := elem;
    UpHeap(q, q.N);
  END Insert;

PROCEDURE Get (q: T): Element.T =
  VAR elem: Element.T;
  BEGIN
    IF q.N > 0 THEN
      elem := q.cont^[1];
      q.cont^[1] := q.cont^[q.N];
      DEC(q.N);
      DownHeap(q, 1);
      RETURN elem;
    ELSE
      <* ASSERT FALSE *>
    END;
  END Get;

BEGIN
END PriorityQueue.
