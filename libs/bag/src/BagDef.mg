(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Sat Sep  4 21:46:59 PDT 1993 by detlefs *)

GENERIC MODULE BagDef(Elem, ElemBag, ElemIntTable);
(* WHERE "ElemBag = Bag(Elem)" and "ElemIntTable = Table(Elem, Int)". *)

REVEAL
  T = Public BRANDED OBJECT
    elts: CARDINAL;
    t: ElemIntTable.Default;
   OVERRIDES
    init := Init;
    fromArray := FromArray;
    copy := Copy;
    member := Member;
    count := Count;
    insert := Insert;
    delete := Delete;
    size := Size;
    intersection := Intersection;
    diff := Diff;
    unionD := UnionD;
    intersectionD := IntersectionD;
    iterate := Iterate;
  END (* OBJECT *);

  Iterator = ElemBag.Iterator BRANDED OBJECT
    elem: Elem.T;
    count := 0;
    tIter: ElemIntTable.Iterator;
   OVERRIDES
    next := Next;
  END (* OBJECT *);

PROCEDURE Init(s: T; sizeHint: CARDINAL): T =
  BEGIN
    s.elts := 0;
    s.t := NEW(ElemIntTable.Default).init(sizeHint);
    RETURN s
  END Init;

PROCEDURE FromArray(s: T; READONLY a: ARRAY OF Elem.T): ElemBag.T =
  BEGIN
    s.t := NEW(ElemIntTable.Default).init(NUMBER(a));
    FOR i := 0 TO LAST(a) DO s.insert(a[i]) END (* FOR *);
    RETURN s
  END FromArray;

PROCEDURE Copy(s: T): ElemBag.T =
  VAR res := NEW(T).init(s.t.size());
      iter := s.iterate();
      e: Elem.T;
  BEGIN
    WHILE iter.next(e) DO res.insert(e) END (* WHILE *);
    RETURN res
  END Copy;

PROCEDURE Member(s: T; e: Elem.T): BOOLEAN =
  VAR count: INTEGER; BEGIN
    RETURN s.t.get(e, count)
  END Member;

PROCEDURE Count(s: T; e: Elem.T): INTEGER =
  VAR count: INTEGER; BEGIN
    IF s.t.get(e, count) THEN
      <* ASSERT count > 0 *>
      RETURN count;
    ELSE RETURN 0 END
  END Count;

PROCEDURE Insert(s: T; e: Elem.T) =
  BEGIN
    INC(s.elts);
    EVAL s.t.put(e, s.count(e) + 1)
  END Insert;

PROCEDURE Delete(s: T; e: Elem.T): BOOLEAN =
  VAR count: INTEGER; BEGIN
    IF s.t.get(e, count) THEN
      <* ASSERT count > 0 *>
      DEC(s.elts);
      DEC(count);
      IF count = 0 THEN RETURN s.t.delete(e, count) END;
      RETURN s.t.put(e, count);
    END;
    RETURN FALSE;
  END Delete;

PROCEDURE Size(s: T): CARDINAL =
  BEGIN RETURN s.elts END Size;

PROCEDURE Intersection(s1: T; s2: ElemBag.T): ElemBag.T =
  VAR s3: T;
      larger, smaller: ElemBag.T;
      iter: ElemBag.Iterator;
      e: Elem.T;
  BEGIN
    IF s1.size() >= s2.size() THEN
      larger := s1; smaller := s2;
      s3 := NEW(T).init(s1.t.size())
    ELSE
      larger := s2; smaller := s1;
      s3 := NEW(T).init(s2.size())
    END;
    iter := smaller.iterate();
    WHILE iter.next(e) DO
      WITH count = MIN(larger.count(e), smaller.count(e)) DO
        IF count > 0 THEN
          INC(s3.elts, count);
          EVAL s3.t.put(e, count)
        END (* IF *)
      END (* WITH *)
    END (* WHILE *);
    RETURN s3
  END Intersection;
    
PROCEDURE Diff(s1: T; s2: ElemBag.T): ElemBag.T =
  VAR s3 := NEW(T).init(s1.t.size());
      iter := s1.iterate();
      e: Elem.T;
  BEGIN
    WHILE iter.next(e) DO
      WITH count = s1.count(e) - s2.count(e) DO
        IF count > 0 THEN
          INC(s3.elts, count);
          EVAL s3.t.put(e, count);
        END (* IF *)
      END (* WITH *)
    END (* WHILE *);
    RETURN s3
  END Diff;

PROCEDURE UnionD(s1: T; s2: ElemBag.T): ElemBag.T =
  VAR iter := s2.iterate(); e: Elem.T; BEGIN
    WHILE iter.next(e) DO
      WITH count1 = s1.count(e), count2 = s2.count(e) DO
        IF count1 < count2 THEN
          INC(s1.elts, count2 - count1);
          EVAL s1.t.put(e, count2);
        END (* IF *)
      END (* WITH *)
    END (* WHILE *);
    RETURN s1
  END UnionD;

CONST SizeFactor = 3;

(* This is overridden because there is a more efficient implementation than
   the default when "s2" is significantly smaller than "s1". *)
PROCEDURE IntersectionD(s1: T; s2: ElemBag.T): ElemBag.T =
  BEGIN
    IF s2.size() * SizeFactor < s1.size() THEN
      VAR tOld := s1.t;
          tNew := NEW(ElemIntTable.Default).init(s2.size());
          iter := s2.iterate();
          e: Elem.T;
          count: INTEGER;
          elts := 0;
      BEGIN
        WHILE iter.next(e) DO
          IF tOld.get(e, count) THEN
            count := MIN(count, s2.count(e));
            IF count > 0 THEN
              INC(elts, count);
              EVAL tNew.put(e, count);
            END (* IF *)
          END (* IF *)
        END (* WHILE *);
        s1.t := tNew;
        s1.elts := elts;
      END (* BEGIN *);
      RETURN s1
    ELSE
      RETURN ElemBag.T.intersectionD(s1, s2)
    END (* IF *)
  END IntersectionD;

PROCEDURE Iterate(s: T): ElemBag.Iterator =
  VAR res := NEW(Iterator, count := 0, tIter := s.t.iterate()); BEGIN
    RETURN res;
  END Iterate;

PROCEDURE Next(iter: Iterator; VAR e: Elem.T): BOOLEAN =
  VAR count := iter.count; BEGIN
    IF count > 0 THEN
      iter.count := count - 1;
      e := iter.elem;
      RETURN TRUE;
    ELSIF iter.tIter.next(e, count) THEN
      iter.count := count - 1;
      iter.elem := e;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Next;
    
BEGIN
END BagDef.
