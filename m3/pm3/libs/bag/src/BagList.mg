(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Jul 25 11:51:56 PDT 1994 by detlefs *)

GENERIC MODULE BagList(Elem, ElemBag, ElemList);
(* WHERE "ElemBag = Bag(Elem)", and "ElemList = List(Elem)". *)

REVEAL
  T = Public BRANDED OBJECT
    l: ElemList.T := NIL
   OVERRIDES
    init := Init;
    fromArray := FromArray;
    copy := Copy;
    member := Member;
    count := Count;
    insert := Insert;
    delete := Delete;
    size := Size;
    iterate := Iterate;
  END (* OBJECT *);

  Iterator = ElemBag.Iterator BRANDED OBJECT
    l: ElemList.T
   OVERRIDES
    next := Next;
  END (* OBJECT *);

PROCEDURE Init(s: T): T =
  BEGIN RETURN s END Init;

PROCEDURE FromArray(s: T; READONLY a: ARRAY OF Elem.T): ElemBag.T =
  BEGIN
    s.l := NIL;
    FOR i := 0 TO LAST(a) DO s.insert(a[i]) END (* FOR *);
    RETURN s
  END FromArray;

PROCEDURE Copy(s: T): ElemBag.T =
  VAR res := NEW(T).init(); rl := s.l; BEGIN
    WHILE rl # NIL DO
      res.l := ElemList.Cons(rl.head, res.l);
      rl := rl.tail
    END (* WHILE *);
    RETURN res
  END Copy;

PROCEDURE Member(s: T; e: Elem.T): BOOLEAN =
  VAR rl := s.l; BEGIN
    WHILE rl # NIL DO
      IF Elem.Equal(rl.head, e) THEN RETURN TRUE END (* IF *);
      rl := rl.tail
    END (* WHILE *);
    RETURN FALSE
  END Member;

PROCEDURE Count(s: T; e: Elem.T): INTEGER =
  VAR count := 0; rl := s.l; BEGIN
    WHILE rl # NIL DO
      IF Elem.Equal(e, rl.head) THEN INC(count) END (* IF *);
      rl := rl.tail;
    END (* WHILE *);
    RETURN count
  END Count;  

PROCEDURE Insert(s: T; e: Elem.T) =
  BEGIN
    s.l := ElemList.Cons(e, s.l);
  END Insert;

PROCEDURE Delete(s: T; e: Elem.T): BOOLEAN =
  VAR pl: ElemList.T := NIL; rl := s.l; BEGIN
    WHILE rl # NIL DO
      IF Elem.Equal(e, rl.head) THEN
        IF pl = NIL THEN
          s.l := rl.tail
        ELSE
          pl.tail := rl.tail
        END (* IF *);
        RETURN TRUE
      ELSE
        pl := rl;
        rl := rl.tail
      END (* IF *)
    END (* WHILE *);
    RETURN FALSE
  END Delete;

PROCEDURE Size(s: T): CARDINAL =
  BEGIN RETURN ElemList.Length(s.l) END Size;

PROCEDURE Iterate(s: T): ElemBag.Iterator =
  VAR res := NEW(Iterator, l := s.l); BEGIN
    RETURN res;
  END Iterate;

PROCEDURE Next(iter: Iterator; VAR e: Elem.T): BOOLEAN =
  BEGIN
    IF iter.l = NIL THEN
      RETURN FALSE
    ELSE
      e := iter.l.head;
      iter.l := iter.l.tail;
      RETURN TRUE
    END (* IF *)
  END Next;
    
BEGIN
END BagList.
