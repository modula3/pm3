GENERIC MODULE BPlusTree(Key, Value, Tbl, SortedTbl);

REVEAL T = Public BRANDED Brand OBJECT
  root: Page := NIL;
  head : Page;
  tail : Page;
  n : INTEGER;
  nn : INTEGER;
OVERRIDES
  keyCompare := KeyCompare;
  init := Init;
  size := Size;
  get := Get;
  put := Put;
  delete := Delete;
  iterate := Iterate;
  iterateOrdered := IterateOrdered;
END;

TYPE
  Page = REF RECORD
    m: INTEGER;
    e: REF ARRAY OF Item;
    prev: Page;
    next: Page;
  END;

  Item = RECORD
    k: Key.T;
    p: Page;
    v: Value.T;
  END;

CONST
  IterBrand = "(Iterator " & Brand & ")";

REVEAL
  Iterator = IteratorPublic BRANDED IterBrand OBJECT
    t: T;
    curr: Page; (* current node in iteration *)
    i: INTEGER; (* current index *)
  END;

TYPE
  IteratorUp = Iterator OBJECT OVERRIDES
    reset := ResetUp;
    next := NextUp;
    seek := SeekUp;
  END;
  IteratorDown = Iterator OBJECT OVERRIDES
    reset := ResetDown;
    next := NextDown;
    seek := SeekDown;
  END;

(* Tree method implementations --------------------------------------------- *)

PROCEDURE KeyCompare(<*UNUSED*> t: T; READONLY k1, k2: Key.T): [-1..1] =
  BEGIN RETURN Key.Compare(k1, k2) END KeyCompare;

PROCEDURE Get(t: T; READONLY x: Key.T; VAR v: Value.T): BOOLEAN =
  VAR
    a := t.root;
    k,l,r: INTEGER;
  BEGIN
    (* search key x on page a *)
    LOOP
      IF a = NIL THEN
        (* item with key x is not in tree *)
        RETURN FALSE;
      END;
      (* binary array search *)
      l:= 1; r:= a.m;
      REPEAT
        k := (l+r) DIV 2;
        IF Key.Compare(x, a.e[k].k) <= 0 THEN r := k-1; END;
        IF Key.Compare(x, a.e[k].k) >= 0 THEN l := k+1; END;
      UNTIL r < l;
      IF l-r > 1 THEN
        WITH val = a.e[k].v DO
          a := a.e[k].p;
          IF a = NIL THEN
            (* found *)
            v := val;
            RETURN TRUE;
          END;
        END;
      ELSE
        a := a.e[r].p;
      END;
    END;
  END Get;

PROCEDURE Put(t: T; READONLY x: Key.T; READONLY val: Value.T): BOOLEAN =
  VAR
    q: Page;
    u: Item;
    found := FALSE;
    n := t.n;
    nn := t.nn;

  PROCEDURE search(a: Page; VAR v: Item): BOOLEAN =
    (* Search key x on tree with root a; if found, set new value.
       Otherwise insert an item with key x in tree.  If an item emerges to be
       passed to a lower level, then assign it to v;
       return TRUE if "tree has become higher", FALSE otherwise *)
    VAR
      k,l,r: INTEGER;
      q: Page;
      u: Item;

    PROCEDURE insert(): BOOLEAN =
      VAR
        b: Page;
      BEGIN
        (* insert u to the right of a.e[r] *)
        IF a.m < nn THEN
          INC(a.m);
          FOR i := a.m TO r+2 BY -1 DO a.e[i] := a.e[i-1]; END;
          a.e[r+1] := u;
          RETURN FALSE;
        END;

        (* page a is full; split it and assign the emerging item to v *)
        b := NEW(Page, e := NEW(REF ARRAY OF Item, nn+1));
        IF a.e[r].p = NIL THEN (* leaf *)
          b.next := a.next; a.next := b; b.next.prev := b; b.prev := a;
          IF r < n THEN (* insert u in left page *)
            FOR i := 1 TO n+1 DO b.e[i] := a.e[i-1+n]; END;
            FOR i := n TO r+2 BY -1 DO a.e[i] := a.e[i-1]; END;
            a.e[r+1] := u;
          ELSE (* insert u in right page *)
            DEC(r, n);
            FOR i := 1 TO r DO b.e[i] := a.e[i+n]; END;
            b.e[r+1] := u;
            FOR i := r+1 TO n DO b.e[i+1] := a.e[i+n]; END;
          END;
          a.m := n; b.m := n+1; v.k := b.e[1].k; v.p := b;
          RETURN TRUE;
        END;
        (* inner node *)
        IF r <= n THEN
          IF r = n THEN
            v := u;
          ELSE
            v := a.e[n];
            FOR i := n TO r+2 BY -1 DO a.e[i] := a.e[i-1]; END;
            a.e[r+1] := u;
          END;
          FOR i:= 1 TO n DO b.e[i] := a.e[i+n]; END;
        ELSE
          (* insert u in right page *)
          DEC(r, n);
          v := a.e[n+1];
          FOR i:= 1 TO r-1 DO b.e[i] := a.e[i+n+1]; END;
          b.e[r] := u;
          FOR i := r+1 TO n DO b.e[i] := a.e[i+n]; END;
        END;
        a.m := n; b.m := n; b.e[0].p := v.p; v.p := b;
        RETURN TRUE;
      END insert;

    BEGIN
      (* search key x on page a *)
      IF a = NIL THEN
        (* item with key x is not in tree *)
        v.k := x;
        v.v := val;
        v.p := NIL;
        RETURN TRUE;
      END;
      (* binary array search *)
      l:= 1; r:= a.m;
      REPEAT
        k:= (l+r) DIV 2;
        IF Key.Compare(x, a.e[k].k) <= 0 THEN r := k-1; END;
        IF Key.Compare(x, a.e[k].k) >= 0 THEN l := k+1; END;
      UNTIL r < l;
      IF l-r > 1 THEN
        q := a.e[k].p;
        r := k;
        IF q = NIL THEN
          (* found *)
          found := TRUE;
          a.e[k].v := val;
          RETURN FALSE;
        END;
      ELSE
        q := a.e[r].p;
      END;
      (* item is not on this page *)
      IF search(q, u) THEN RETURN insert(); END;
      RETURN FALSE;
    END search;

  BEGIN
    WITH root = t.root DO
      IF search(root, u) THEN
        (* insert new base page *)
        q := root;
        root := NEW(Page, e := NEW(REF ARRAY OF Item, nn+1));
        root.m := 1;
        root.e[0].p := q;
        root.e[1] := u;
        IF q = NIL THEN
          t.head.next := root;
          t.tail.prev := root;
          root.next := t.tail;
          root.prev := t.head;
        END;
      END;
    END;
    RETURN found;
  END Put;

PROCEDURE Delete(t: T; READONLY x: Key.T; VAR v: Value.T): BOOLEAN =
  VAR
    q: Page;
    found := FALSE;
    n := t.n;
    nn := t.nn;

  PROCEDURE delete(a: Page): BOOLEAN =
    (* Search and delete key x in tree a.  If a page underflow is necessary,
       balance with adjacent page if possible, otherwise merge.
       Return TRUE if "page is undersize", FALSE otherwise. *)
    VAR
      k,l,r: INTEGER;
      q: Page;

    PROCEDURE underflow(c, a: Page; s: INTEGER): BOOLEAN =
      (* a = underflow page, c = ancestor page *)
      VAR
        b: Page;
        k,mb: INTEGER;
        mc := c.m;
      BEGIN
        (* a.m = n-1 *)
        IF a.e[a.m].p = NIL THEN
          (* leaf *)
          IF s < mc THEN
            (* b := page to the right of a *)
            INC(s);
            b := c.e[s].p; <* ASSERT b = a.next *>
            mb := b.m; k := (mb-n+1) DIV 2;
            (* k = no. of items available on adjacent page b *)
            IF k > 0 THEN
              (* move k items from b to a *)
              FOR i := 1 TO k DO a.e[i-1+n] := b.e[i]; END;
              c.e[s].k := b.e[k+1].k; c.e[s].p := b;
              <* ASSERT b.e[0].p = NIL *>
              DEC(mb, k);
              FOR i := 1 TO mb DO b.e[i] := b.e[i+k]; END;
              b.m := mb; a.m := n-1+k;
              RETURN FALSE;
            END;
            (* merge pages a and b *)
            FOR i := 1 TO n DO a.e[i-1+n] := b.e[i]; END;
            FOR i := s TO mc-1 DO c.e[i] := c.e[i+1]; END;
            a.m := nn-1; c.m := mc-1;
            a.next := b.next; b.next.prev := a;
            RETURN c.m < n;
          END;
          (* b := page to the left of a*)
          b := c.e[s-1].p; <* ASSERT b = a.prev *>
          mb := b.m + 1; k := (mb-n) DIV 2;
          IF k > 0 THEN
            (* move k items from page b to a *)
            FOR i := n-1 TO 1 BY -1 DO a.e[i+k] := a.e[i]; END;
            DEC(mb, k);
            FOR i := k TO 1 BY -1 DO a.e[i] := b.e[i-1+mb]; END;
            <* ASSERT a.e[0].p = NIL *>
            c.e[s].k := b.e[mb].k; c.e[s].p := a;
            b.m := mb-1; a.m := n-1+k;
            RETURN FALSE;
          END;
          (* merge pages a and b *)
          FOR i := 1 TO n-1 DO b.e[i-1+mb] := a.e[i]; END;
          b.m := nn-1; c.m := mc-1;
          b.next := a.next; a.next.prev := b;
          RETURN c.m < n;
        END;
        IF s < mc THEN
          (* b := page to the right of a *)
          INC(s);
          b := c.e[s].p; mb := b.m; k:= (mb-n+1) DIV 2;
          (* k = no. of items available on adjacent page b *)
          a.e[n] := c.e[s]; a.e[n].p := b.e[0].p;
          IF k > 0 THEN
            (* move k items from b to a *)
            FOR i := 1 TO k-1 DO a.e[i+n]:= b.e[i]; END;
            c.e[s] := b.e[k]; c.e[s].p := b;
            b.e[0].p := b.e[k].p; DEC(mb, k);
            FOR i := 1 TO mb DO b.e[i] := b.e[i+k]; END;
            b.m := mb; a.m := n-1+k;
            RETURN FALSE;
          END;
          (* merge pages a and b *)
          FOR i := 1 TO n DO a.e[i+n] := b.e[i]; END;
          FOR i := s TO mc-1 DO c.e[i] := c.e[i+1]; END;
          a.m := nn; c.m := mc-1;
          RETURN c.m < n;
        END;
        (* b:= page to the left of a*)
        b:= c.e[s-1].p;
        mb := b.m + 1; k := (mb-n) DIV 2;
        IF k > 0 THEN
          (* move k items from page b to a *)
          FOR i:= n-1 TO 1 BY -1 DO a.e[i+k] := a.e[i]; END;
          a.e[k] := c.e[s]; a.e[k].p := a.e[0].p; DEC(mb, k);
          FOR i := k-1 TO 1 BY -1 DO a.e[i] := b.e[i+mb]; END;
          a.e[0].p := b.e[mb].p;
          c.e[s] := b.e[mb]; c.e[s].p := a;
          b.m := mb-1; a.m := n-1+k;
          RETURN FALSE;
        END;
        (* merge pages a and b *)
        b.e[mb] := c.e[s]; b.e[mb].p := a.e[0].p;
        FOR i := 1 TO n-1 DO b.e[i+mb] := a.e[i]; END;
        b.m := nn; c.m := mc-1;
        RETURN c.m < n;
      END underflow;

    BEGIN
      IF a = NIL THEN
        found := FALSE;
        RETURN FALSE;
      END;
      (* binary array search *)
      l:= 1; r := a.m;
      REPEAT
        k := (l+r) DIV 2;
        IF Key.Compare(x, a.e[k].k) <= 0 THEN r := k-1; END;
        IF Key.Compare(x, a.e[k].k) >= 0 THEN l := k+1; END;
      UNTIL l>r;
      IF l-r > 1 THEN
        q := a.e[k].p;
        IF q = NIL THEN
          (* found, now delete a.e[k] *)
          found := TRUE;
          v := a.e[k].v;
          DEC(a.m);
          FOR i := k TO a.m DO a.e[i] := a.e[i+1]; END;
          RETURN a.m < n;
        END;
        r := k;
      ELSE
        q := a.e[r].p;
      END;
      IF delete(q) THEN RETURN underflow(a,q,r); END;
      RETURN FALSE;
    END delete;

  BEGIN
    WITH root = t.root DO
      IF delete(root) THEN
        (* base page size was reduced *)
        IF root.m = 0 THEN
          q := root; root := q.e[0].p;
        END;
      END;
    END;
    RETURN found;
  END Delete;

PROCEDURE Size(t: T): CARDINAL =
  VAR
    size := 0;
    p := t.head.next;
  BEGIN
    WHILE p # NIL AND p # t.tail DO
      INC(size, p.m);
      p := p.next;
    END;
    RETURN size;
  END Size;

PROCEDURE Init(self: T; n: INTEGER) :T =
  BEGIN
    self.head := NEW(Page);
    self.tail := NEW(Page);
    self.head.next := self.tail;
    self.tail.prev := self.head;
    self.n := n;
    self.nn := 2 * n;
    RETURN self;
  END Init;

PROCEDURE Iterate(t: T): Tbl.Iterator =
  BEGIN RETURN IterateOrdered(t, TRUE) END Iterate;

PROCEDURE IterateOrdered(t: T; up: BOOLEAN): SortedTbl.Iterator =
  VAR res: Iterator;
  BEGIN
    IF up THEN
      res := NEW(IteratorUp);
    ELSE
      res := NEW(IteratorDown);
    END;
    res.t := t;
    res.reset();
    RETURN res
  END IterateOrdered;

(* Iterator method implementations ---------------------------------------- *)

PROCEDURE ResetUp(it: Iterator) =
  VAR t := it.t;
  BEGIN
    it.curr := t.head.next;
    it.i := 1;
  END ResetUp;

PROCEDURE ResetDown(it: Iterator) =
  VAR t := it.t;
  BEGIN
    it.curr := t.tail.prev;
    it.i := it.curr.m;
  END ResetDown;

PROCEDURE NextUp(it: Iterator; VAR (*OUT*) k: Key.T; VAR (*OUT*) v: Value.T):
  BOOLEAN =
  VAR
    curr := it.curr;
    t := it.t;
  BEGIN
    (* handle empty iterator *)
    IF curr = NIL OR curr = t.tail THEN RETURN FALSE; END;

    (* save key and value in current mode *)
    k:= curr.e[it.i].k; v:= curr.e[it.i].v;

    (* advance "it.curr" to next node in order *)
    IF it.i < curr.m THEN
      INC(it.i);
    ELSE
      it.curr := curr.next;
      it.i := 1;
    END;
    RETURN TRUE;
  END NextUp; 

PROCEDURE NextDown(it: Iterator; VAR (*OUT*) k: Key.T; VAR (*OUT*) v: Value.T):
  BOOLEAN =
  VAR
    curr := it.curr;
    t := it.t;
  BEGIN
    (* handle empty iterator *)
    IF curr = NIL OR curr = t.head THEN RETURN FALSE; END;

    (* save key and value in current node *)
    k:= curr.e[it.i].k; v:= curr.e[it.i].v;

    (* advance "it.curr" to next node in order *)
    IF it.i > 1 THEN
      DEC(it.i)
    ELSE
      it.curr := curr.prev;
      it.i := it.curr.m;
    END;
    RETURN TRUE;
  END NextDown;

PROCEDURE SeekUp(it: Iterator; READONLY key: Key.T) =
  VAR
    k: Key.T;
    v: Value.T;
  BEGIN
    SeekDown(it, key);   
    IF it.curr = it.t.head THEN
      it.reset();
    ELSIF it.curr # NIL THEN
      IF it.curr.e[it.i].k # key THEN
        EVAL it.next(k, v);
      END
    END;
  END SeekUp; 
  
PROCEDURE SeekDown(it: Iterator; READONLY key: Key.T) =
  VAR
    a := it.t.root;
    k,l,r: INTEGER;
  BEGIN
    (* search key x on page a *)
    LOOP
      IF a = NIL THEN
        (* empty tree *)
        it.reset();
        RETURN;
      END;
      (* binary array search *)
      l:= 1; r:= a.m;
      REPEAT
        k := (l+r) DIV 2;
        IF Key.Compare(key, a.e[k].k) <= 0 THEN r := k-1; END;
        IF Key.Compare(key, a.e[k].k) >= 0 THEN l := k+1; END;
      UNTIL r < l;
      IF l-r > 1 THEN
        it.curr := a;
        it.i := k;
        a := a.e[k].p;
        IF a = NIL THEN RETURN END;
      ELSE
        IF r = 0 THEN
          it.curr := a.prev;
          it.i := it.curr.m;
        ELSE
          it.curr := a;
          it.i := r;
        END;
        a := a.e[r].p;
        IF a = NIL THEN RETURN END;
      END;
    END;
  END SeekDown; 

BEGIN
END BPlusTree.
