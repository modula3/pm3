(* Copyright (C) 1994 Digital Equipment Corporation.           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Sep 22 19:30:02 PDT 1994 by heydon     *)
(*      modified on Tue Aug 23 08:21:20 PDT 1994 by kalsow     *)
(*      modified on Thu Apr 29 15:10:36 PDT 1993 by mcjones    *)
(*      modified on Wed Oct  7 16:07:16 PDT 1992 by johnh      *)

(* Heap-ordered binary trees, with top-down updates.
    Reference:
    Cecilia Aragon and Raimund Seidel, FOCS 1989. *)

GENERIC MODULE SortedTable (Key, Value, Tbl, Rep);
(* Where "Key.T" and "Value.T" are types, "Tbl=Table(Key, Value)", and
   "Key.Compare(READONLY k1, k2: Key.T): [-1..1]" is a total order. *)

IMPORT Random;
FROM Rep IMPORT Node;

TYPE
  Cmp = [-1 .. +1]; (* comparison:  <, =, > *)

TYPE
  Public = T OBJECT METHODS
    init(): Default;
    keyCompare(READONLY k1, k2: Key.T): Cmp;
  END;

REVEAL
  Default = Public BRANDED DefaultBrand OBJECT
    h          : Node;          (* the root of the tree is h.hi *)
    rand       : Random.T;      (* used to generate priorities for heap *)
    sz         : CARDINAL := 0; (* number of entries *)
    topL, topR : Node;          (* used in SplitNode *)
    topNode    : Node;          (* used in Concat *)
  OVERRIDES
    get             := Get;
    put             := Put;
    delete          := Delete;
    size            := Size;
    iterate         := Iterate;
    iterateOrdered  := IterateOrdered;
    init            := Init;
    keyCompare      := KeyCompare
  END;

CONST
  MaxPriority = LAST(INTEGER);

(*
<*UNUSED*>
PROCEDURE Validate (table: Default) RAISES {} =
  (* Check that the tree is a heap on priorities and that each node's key
     is greater than every key in its left subtree and less than every key
     in its right subtree. *)
  VAR count := 0;
  PROCEDURE V (x                     : Node;     (* not NIL *)
               parentPriority        : INTEGER;
               lowerBound, upperBound: Key.T;
               checkLowerBound, checkUpperBound: BOOLEAN) =
    BEGIN
      INC(count);
      IF checkLowerBound THEN
        <* ASSERT table.keyCompare (lowerBound, x.key) = -1 *>
      END;
      IF checkUpperBound THEN
        <* ASSERT table.keyCompare (x.key, upperBound) = -1 *>
      END;
      <* ASSERT parentPriority >= x.priority *>
      IF x.lo # NIL THEN
        V (x.lo, x.priority, lowerBound, x.key, checkLowerBound, TRUE);
      END;
      IF x.hi # NIL THEN
        V (x.hi, x.priority, x.key, upperBound, TRUE, checkUpperBound);
      END
    END V;
  BEGIN
    <* ASSERT (table.h.priority = MaxPriority) AND (table.h.lo = NIL) *>
    IF table.h.hi # NIL THEN
      V(table.h.hi, MaxPriority, table.h.key, table.h.key, FALSE, FALSE)
      (* h.key is just a valid element of type Key.T; it never participates
         in a comparison *)
    END;
    <* ASSERT count = table.sz *>
  END Validate;
*)

PROCEDURE KeyCompare (<*UNUSED*> tbl: Default;  READONLY k1, k2: Key.T): Cmp =
  BEGIN
    RETURN Key.Compare (k1, k2);
  END KeyCompare;

PROCEDURE Init (tbl: Default): Default =
  BEGIN
    tbl.rand    := NEW (Random.Default).init();
    tbl.topL    := NEW (Node);
    tbl.topR    := NEW (Node);
    tbl.topNode := NEW (Node);
    tbl.h       := NEW (Node, priority := MaxPriority);
    RETURN tbl
  END Init;

(*
<*UNUSED*>
PROCEDURE FindNode (           tbl  : Default;
                    READONLY   k    : Key.T;
                    VAR(*OUT*) f, x : Node): BOOLEAN =
  VAR cmp: Cmp;
  BEGIN
    f := tbl.h;
    x := f.hi;
    WHILE (x # NIL) DO
      cmp := tbl.keyCompare (k, x.key);
      IF cmp = 0 THEN RETURN TRUE; END;
      f := x;
      IF (cmp < 0)
        THEN x := x.lo;
        ELSE x := x.hi;
      END:
    END;
    RETURN FALSE;
  END FindNode;
*)

PROCEDURE Get (           tbl : Default;
               READONLY   k   : Key.T;
               VAR(*OUT*) v   : Value.T): BOOLEAN =
  VAR
    x   : Node := tbl.h.hi;
    cmp : Cmp;
  BEGIN
    WHILE (x # NIL) DO
      cmp := tbl.keyCompare (k, x.key);
      IF cmp = 0 THEN  v := x.value;  RETURN TRUE; END;
      IF (cmp < 0)
        THEN x := x.lo;
        ELSE x := x.hi;
      END;
    END;
    RETURN FALSE;
  END Get;

PROCEDURE SplitNode (            tbl     : Default;
                     READONLY    splitKey  : Key.T;
                                 toSplit   : Node;
                     VAR (*OUT*) outL, outR: Node): BOOLEAN =
(* Split the subtree of tbl rooted at toSplit into two trees,
   composed of those nodes left of (with keys less than) splitKey and
   right of (with keys greater than) splitKey.  Delete any node with
   key = splitKey that appears in the tree. Return TRUE iff a node
   with splitKey was present in the tree.  Return the result subtrees
   in outL and outR.  This is a destructive operation, so the caller
   should ensure that only one call to this procedure is active at any
   time. *)
(* Since SortedTable.T can be subtyped by a client, Split should not
   do a NEW; instead, the caller should pass in tables to be modified
   by Split. --PMcJ 11 Nov 92 *)
  VAR
    found : BOOLEAN := FALSE;
    x     : Node    := tbl.topL; (* Walk down left tree with this node *)
    y     : Node    := tbl.topR; (* Walk down right tree with this node *)
  BEGIN
    WHILE (toSplit # NIL) DO
      CASE tbl.keyCompare (toSplit.key, splitKey) OF
      | -1 => x.hi := toSplit;  x := toSplit;
              toSplit := toSplit.hi;
      |  0 => found := TRUE;  (* delete toSplit *)
              y.lo := toSplit.hi;  y := toSplit;
              toSplit := toSplit.lo;
      | +1 => y.lo := toSplit;  y := toSplit;
              toSplit := toSplit.lo;
      END;
    END;
    x.hi := NIL;
    y.lo := NIL;
    outL := tbl.topL.hi;
    outR := tbl.topR.lo;
    RETURN found;
  END SplitNode;

PROCEDURE Put (tbl: Default; READONLY k: Key.T; READONLY v: Value.T): BOOLEAN =
  VAR
    cmp  : Cmp     := 1;     (* initially, x is right child of f *)
    f    : Node    := tbl.h; (* father of x *)
    x    : Node    := f.hi;
    pri  : INTEGER := tbl.rand.integer(0, LAST(INTEGER));
  BEGIN
    WHILE (x # NIL) AND (pri < x.priority) DO
      cmp := tbl.keyCompare (k, x.key);
      IF cmp = 0 THEN  x.value := v;  RETURN TRUE;  END;
      f := x;
      IF (cmp < 0)
        THEN x := x.lo;
        ELSE x := x.hi;
      END;
    END;

    (* didn't find a match and we have a "big" heap priority => insert *)
    VAR
      new   := NEW (Node, key := k, value := v, priority := pri);
      found := (x # NIL) AND SplitNode (tbl, k, x,new.lo,new.hi);
    BEGIN
      IF NOT found THEN INC(tbl.sz) END;
      IF (cmp < 0)
        THEN f.lo := new;
        ELSE f.hi := new;
      END;
      RETURN found;
    END;
  END Put;

PROCEDURE Concat (table: Default;  left, right: Node): Node =
(* Left and right are nodes in table (or at least with keys comparable
   with table.keyCompare).  All the keys in the left subtree are less
   than the keys in the right subtree.  Build and return a tree that
   is the concatenation of the two subtrees. *)
  VAR
    upper : Node;
    f     : Node     := table.topNode;
    prev  : [0..1]   := 1;
    next  : [0..1];
  BEGIN
    IF (left = NIL)  THEN RETURN right; END;
    IF (right = NIL) THEN RETURN left;  END;

    WHILE (left # right) DO (* depends on disjoint trees with NIL leaves *)
      IF (left = NIL) THEN
        upper := right;
        right := right.lo;
        next  := 0;
      ELSIF (right = NIL) OR (left.priority > right.priority) THEN
        upper := left;
        left  := left.hi;
        next  := 1;
      ELSE (* left.priority <= right.priority *)
        upper := right;
        right := right.lo;
        next  := 0;
      END;
      IF (prev = 0)
        THEN f.lo := upper;
        ELSE f.hi := upper;
      END;
      f := upper;
      prev := next;
    END;
    IF (prev = 0)
      THEN f.lo := NIL;
      ELSE f.hi := NIL;
    END;
    RETURN table.topNode.hi;
  END Concat;

PROCEDURE Delete (           tbl : Default;
                  READONLY   k   : Key.T;
                  VAR(*OUT*) v   : Value.T) : BOOLEAN =
  VAR
    f    : Node   := tbl.h;
    x    : Node   := f.hi;
    prev : [0..1] := 1;
    cmp  : Cmp;
    n    : Node;
  BEGIN
    WHILE (x # NIL) DO
      cmp := tbl.keyCompare (k, x.key);
      IF cmp = 0 THEN
        n := Concat (tbl, x.lo, x.hi);
        IF (prev = 0)
          THEN f.lo := n;
          ELSE f.hi := n;
        END;
        v := x.value;
        DEC(tbl.sz);
        RETURN TRUE;
      END;
      f := x;
      IF (cmp < 0)
        THEN x := x.lo;  prev := 0;
        ELSE x := x.hi;  prev := 1;
      END;
    END;
    RETURN FALSE;
  END Delete;

PROCEDURE Size (tbl: Default): CARDINAL =
  BEGIN
    RETURN tbl.sz
  END Size;

CONST
  MaxStack = 50;(* allows tree with 2**(50/2) = 33,554,432 entries *)
                (* randomness ==> maybe fewer entries *)

TYPE
  PublicIterator = Tbl.Iterator OBJECT METHODS
    seek (READONLY key: Key.T)
  END;

TYPE
  DefaultIterator = PublicIterator BRANDED OBJECT
    table : Default  := NIL;
    done  : BOOLEAN  := FALSE;
    ascend: BOOLEAN  := FALSE;
    top   : CARDINAL := 0;
    stack : ARRAY [0 .. MaxStack] OF Node
  OVERRIDES
    init := InitIterator;
    next := Next;
    seek := Seek
  END;

PROCEDURE IterateOrdered (table: Default; up: BOOLEAN := TRUE): Iterator =
  VAR it := NEW(DefaultIterator, table := table, ascend := up);
  BEGIN
    EVAL it.init();
    RETURN it;
  END IterateOrdered;

PROCEDURE Iterate (table: Default): Tbl.Iterator =
  BEGIN
    RETURN table.iterateOrdered()
  END Iterate;

PROCEDURE InitIterator (it: DefaultIterator): Tbl.Iterator =
  VAR n: Node;
  BEGIN
    it.done := FALSE;
    it.top  := 0;
    IF it.ascend
      THEN n := it.table.h;
      ELSE n := NEW(Node, lo := it.table.h.hi);
    END;
    it.stack[it.top] := n;
    RETURN it;
  END InitIterator;

PROCEDURE Next (            it   : DefaultIterator;
                VAR (*OUT*) key  : Key.T;
                VAR (*OUT*) value: Value.T   ): BOOLEAN =
(* Advance to the successor of the current node (the element at the
   top of the iterator stack).  The stack contains the ancestors of
   the current node that have not yet been visited.  If it.ascend =
   TRUE (FALSE), this means that the current node is in the left
   (right) subtree of every other node on the stack. *)
  VAR
    x    : Node   := it.stack[it.top];
    next : [0..1] := ORD (it.ascend);
    prev : [0..1] := ORD (NOT it.ascend);
    n    : Node;
  BEGIN
    <* ASSERT NOT it.done *>

    IF (next = 0) THEN n := x.lo; ELSE n := x.hi; END;
    IF n = NIL THEN
      IF it.top = 0 THEN  it.done := TRUE; RETURN FALSE;  END;
      DEC(it.top);
      x := it.stack[it.top];
      key := x.key;
      value := x.value;
      RETURN TRUE;
    END;

    (* Replace the top of the stack with its "next" child,    *)
    (* and push the path to the most previous descendant of x *)
    IF (next = 0) THEN x := x.lo; ELSE x := x.hi; END;
    it.stack[it.top] := x;
    LOOP
      IF (prev = 0) THEN n := x.lo; ELSE n := x.hi; END;
      IF (n = NIL) THEN EXIT; END;
      x := n;
      INC(it.top);
      it.stack[it.top] := x
    END;

    key := x.key;
    value := x.value;
    RETURN TRUE
  END Next;

PROCEDURE Seek (it: DefaultIterator; READONLY key: Key.T) =
(* If table.done, then crash.  Otherwise, define x = stack[top], and
   define pred(x) and succ(x) to be the predecessor and successor of x
   in symmetric order in the table.  Reset it.stack so that x.key <
   key <= succ(x).key if it.ascend and pred(x).key <= key < x.key
   otherwise.

   The implementation is more efficient if key is still ahead of the
   current position of the iterator. *)
  CONST CmpVal = ARRAY BOOLEAN OF Cmp { -1, +1 };
  VAR
    tbl    : Default  := it.table;
    cmpval : Cmp      := CmpVal [it.ascend]; (* parameterize branches *)
    cmp    : Cmp;
    x, n   : Node;
    topelt : Node;
    top    : CARDINAL;
  BEGIN
    <* ASSERT NOT it.done *>

    (* Fix up stack if key is on the wrong side of it.  Avoid
       comparisons with keys that have not been explicitly set, such
       as tbl.h.key. *)
    x := it.stack[it.top];
    IF (x # tbl.h) AND (x.lo # tbl.h.hi)   (* not a virgin stack *)
       AND (tbl.keyCompare (key, x.key) # cmpval) THEN
      it.top := 0;
      IF it.ascend
        THEN n := tbl.h;
        ELSE n := NEW (Node);  n.lo := tbl.h.hi;
      END;
      it.stack[0] := n;
    END;
    (* Now key lies strictly to the right (left if NOT it.ascend) of
       the path stored in it.stack. *)

    WHILE (it.top # 0)
      AND (tbl.keyCompare (key, it.stack[it.top - 1].key) = cmpval) DO
      DEC(it.top);
    END;
    (* Now key lies to the right (left if NOT it.ascend) of
       stack[top] and, if top>0, not right (not left) of
       stack[top-1].  All stack entries strictly below top will
       remain unchanged; top may or may not be changed. *)

    (* The discussion of the following algorithm is phrased for
       it.ascend = TRUE; the code handles both cases by
       parameterizing with cmpval.  Search for key, as in FindNode.
       Each step localizes key to a particular subtree.  The stack
       records all the right ancestors of the current subtree.  top
       and topelt record the closest left ancestor of the current
       subtree.  Whenever we fall off the bottom of the tree, we
       throw away all the right ancestors below topelt and replace
       them by topelt: key lies in the interval (topelt,
       succ(topelt)] in symmetric order; we make topelt the last
       node on the stack so subsequent calls to Next will find the
       correct element. *)
    cmp := cmpval;
    top := it.top;
    topelt := it.stack[top];
    x := topelt;
    LOOP
      IF (cmp < 0) THEN x := x.lo; ELSE x := x.hi; END;
      IF x = NIL THEN EXIT END;
      cmp := tbl.keyCompare (key, x.key);
      IF cmp = 0 THEN cmp := -cmpval; END;
      IF cmp = cmpval THEN
        topelt := x;
        top := it.top;
      ELSE
        it.stack[it.top] := x;
        INC(it.top);
      END;
    END;
    it.top := top;
    it.stack[top] := topelt;
  END Seek;

BEGIN
END  SortedTable.
