GENERIC MODULE CursorSet(Element);

(***************************************************************************)
(** Created by:  Peter Klein                                               *)
(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.7  1998/01/21 14:15:21  roland
    Debugging code added (in comments).

    Revision 1.6  1997/08/15 11:20:14  roland
    Copying of relations now works by using an internal interface of CursorSet.

    Revision 1.5  1997/07/25 16:26:51  renehuel
    Removed bugs in procedure intersection.

    Revision 1.4  1997/07/21 10:29:31  roland
    New implementation of sets using red-black trees.

*)
(***************************************************************************)

(* Implementation of a set with red-black-trees according to [1] *)
(* [1] "Introduction to Algorithms", Thomas Corman, Charles Leserson,
   Ronald Rivest; MIT Press, McGraw-Hill Book Company, 2nd printing 1990 *)

IMPORT Journal, Variant, Fmt;

TYPE
  NodeInfo = RECORD
               data  : Element.T;  (* Data *)
               red   : BOOLEAN;    (* color of teh node *)
               left  : Node;       (* left subtree *)
               right : Node;       (* right subtree *)
               parent: Node;
             END;

  Node =                         (* a set element: *)
    BRANDED REF NodeInfo;


REVEAL
  T = Public BRANDED Brand OBJECT
        root   : Node := NIL;    (* pointer to search tree root *)
        current: Node := NIL;    (* pointer to current element *)
        sink   : Node := NIL;
        noOfElements: CARDINAL := 0; (* no.  of elements *)

        rangeCompare: CompareProcedure := FullLoopCompare;
        (* RangeLoop compare procedure *)

        copies  : T := NIL;
        copyOf  : T := NIL;
        nextCopy: T := NIL;
        prevCopy: T := NIL;
        (* CursorSet performs a lazy copy strategy.  Sets are really copied
           only when the original or the copy changes.  If set is the
           original, its copies are listed in set.copies.  For each copy c,
           c.copyOf = set and c.nextCopy/c.prevCopy points to the
           next/previous copy of set in the list set.copies.  A invariant
           is set.copies # NIL -> set.copyOf = NIL AND set.copyOf # NIL ->
           set.copies = NIL*)
      OVERRIDES
        init                   := Init;
        copy                   := Copy;
        card                   := Card;
        isEmpty                := IsEmpty;
        deleteElement          := DeleteElement;
        extractAnyElement      := ExtractAnyElement;
        rangeExtractAnyElement := RangeExtractAnyElement;
        dispose                := Dispose;
        insert                 := Insert;
        replaceValue           := ReplaceValue;
        getValue               := GetValue;
        in                     := In;
        compare                := Compare;
        isSubset               := IsSubset;
        isStrictSubset         := IsStrictSubset;
        isDisjunct             := IsDisjunct;
        union                  := Union;
        intersection           := Intersection;
        difference             := Difference;
        symmetricDifference    := SymmetricDifference;
        gotoElement            := GotoElement;
        gotoMinimum            := GotoMinimum;
        gotoMaximum            := GotoMaximum;
        loop                   := Loop;
        rangeLoop              := RangeLoop;
        get                    := Get;
      END;

VAR
  FreeNodes                 : Node     := NIL;
  MaxFreeNodes, FreeNodeSize: CARDINAL := 0;

PROCEDURE NewNode (): Node =
  VAR node: Node;
  BEGIN
    IF FreeNodes # NIL THEN
      node := FreeNodes;
      FreeNodes := FreeNodes.left;
      DEC(FreeNodeSize);
    ELSE
      node := NEW(Node);
    END;
    RETURN node;
  END NewNode;

PROCEDURE DisposeNode (n: Node) =
  BEGIN
    n.left := NIL;
    n.right := NIL;
    n.parent := NIL;

    n^.left := FreeNodes;
    FreeNodes := n;
    INC(FreeNodeSize);
    IF Variant.FreeMemoryListLog > 0 THEN
      IF MaxFreeNodes < FreeNodeSize THEN
        MaxFreeNodes := FreeNodeSize;       
        IF MaxFreeNodes MOD Variant.FreeMemoryListLog = 0 THEN
          Journal.Add(Element.Brand & "CursorSet free node list "
                      & Fmt.Int(MaxFreeNodes));
        END;
      END;
    END;
  END DisposeNode;

VAR
  FreeSets                : T        := NIL;
  MaxFreeSets, FreeSetSize: CARDINAL := 0;

PROCEDURE New (): T =
  VAR set: T;
  BEGIN
    IF FreeSets # NIL THEN
      set := FreeSets;
      FreeSets := FreeSets.nextCopy;
      DEC(FreeSetSize);
    ELSE
      set := NEW(T);
    END;
    RETURN Init(set);
  END New;

PROCEDURE DisposeSet (set: T) =
(**
  PROCEDURE AssertNotInList() =
    VAR h: T;
    BEGIN
      h := FreeSets;
      WHILE h # NIL AND h # set DO h := h.nextCopy; END;
      IF h = set THEN <* ASSERT FALSE *> END;
    END AssertNotInList;
*)
  BEGIN
    (* AssertNotInList();*)
    DisposeNode(set.sink);
    set.sink := NIL;
    set.root := NIL;
    set.current := NIL;
    set.nextCopy := NIL;
    set.prevCopy := NIL;
    set.copies := NIL;
    set.copyOf := NIL;
    set.nextCopy := FreeSets;
    FreeSets := set;
    INC(FreeSetSize);
    IF Variant.FreeMemoryListLog > 0 THEN
      IF FreeSetSize > MaxFreeSets THEN
        MaxFreeSets := FreeSetSize;
        IF MaxFreeSets MOD Variant.FreeMemoryListLog = 0 THEN
          Journal.Add(Element.Brand & "CursorSet free memory list "
                        & Fmt.Int(MaxFreeSets));
        END;
      END;
    END;
  END DisposeSet;

PROCEDURE Init (set: T): T =
  BEGIN
    set.noOfElements := 0;
    set.sink := NewNode();
    set.root := set.sink;
    set.current := set.sink;
    set.rangeCompare := FullLoopCompare;

    set.copyOf := NIL;
    set.copies := NIL;
    set.nextCopy := NIL;
    set.prevCopy := NIL;
    RETURN set;
  END Init;


PROCEDURE InternCopySet (source, target: T) RAISES {} =
  BEGIN
    WITH orig = Orig(source) DO
      target.copyOf := orig;
      (* insert as head of orig.copies *)
      IF orig.copies # NIL THEN
        target.nextCopy := orig.copies;
        orig.copies.prevCopy := target;
      END;
      orig.copies := target;
    END;
  END InternCopySet; 

PROCEDURE Copy (set: T): T RAISES {} =
  VAR target: T;
  BEGIN
    target := New();
    InternCopySet(set, target);
    RETURN target;
  END Copy;


PROCEDURE Card (set: T): CARDINAL RAISES {} =
  BEGIN
    RETURN Orig(set).noOfElements;
  END Card;


PROCEDURE IsEmpty (set: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN Orig(set).noOfElements = 0;
  END IsEmpty;


PROCEDURE DeleteElement (         set  : T;
                         READONLY data : Element.T;
                         VAR      found: BOOLEAN    ) RAISES {} =
  VAR
    parent, node: Node;
    left        : BOOLEAN;
  BEGIN
    WriteAccess(set);
    Find(set, data, CompareData, parent, node, found, left);
    IF found THEN RBDelete(set, node) END;
  END DeleteElement;


PROCEDURE ExtractAnyElement (set: T; VAR found: BOOLEAN): Element.T
  RAISES {} =
  VAR
    node: Node;
    data: Element.T;

  BEGIN
    WriteAccess(set);
    set.rangeCompare := FullLoopCompare;
    node := GetNode(set, set.current, found);
    IF (NOT found) THEN
      set.current := InorderFirst(set);
      node := GetNode(set, set.current, found);
    END;

    IF (found) THEN data := node.data; RBDelete(set, node); END;
    RETURN data;
  END ExtractAnyElement;


PROCEDURE RangeExtractAnyElement (set             : T;
                                  compareProcedure: CompareProcedure;
                                  READONLY data : Element.T;
                                  VAR      found: BOOLEAN    ): Element.T
  RAISES {IllegalCompare} =
  VAR
    node : Node;
    dummy: Element.T;

  BEGIN
    WriteAccess(set);
    IF (set.noOfElements = 0) THEN found := FALSE; RETURN dummy; END;

    node := GetNode(set, set.current, found);
    IF (NOT found) THEN
      RangeLoop(set, compareProcedure, data);
      node := GetNode(set, set.current, found);
    END;

    IF (found) THEN dummy := node.data; RBDelete(set, node); END;
    RETURN dummy;
  END RangeExtractAnyElement;

PROCEDURE Dispose (set: T; insertInFreeList: BOOLEAN := TRUE) RAISES {} =

  PROCEDURE PostorderFirst (set: T; node: Node): Node =
    VAR res := node;
    BEGIN
      IF node = set.sink THEN RETURN set.sink END;
      WHILE res.left # set.sink DO res := res.left END;
      RETURN res;
    END PostorderFirst;

  PROCEDURE PostorderNext (set: T; node: Node): Node =
    BEGIN
      IF node = set.sink THEN RETURN set.sink; END;
      IF node.parent = set.sink THEN RETURN set.sink END;

      IF node.parent.left = node THEN
        (* node is a left child *)
        IF node.parent.right # set.sink THEN
          RETURN PostorderFirst(set, node.parent.right);
        END;
      END;
      RETURN node.parent;
    END PostorderNext;

  VAR
    first     : T;
    node, disp: Node;
  BEGIN
    IF set.copyOf # NIL THEN
      (* This is a copy.  Simply remove from list of copies *)
      IF set.nextCopy # NIL THEN
        set.nextCopy.prevCopy := set.prevCopy;
      END;
      IF set.prevCopy # NIL THEN
        set.prevCopy.nextCopy := set.nextCopy;
      ELSE
        set.copyOf.copies := set.nextCopy;
      END;
    ELSIF set.copies # NIL THEN
      (* Copies exist.  Don't clean up but clone first copy as set *)
      first := set.copies;
      first.noOfElements := set.noOfElements;
      first.root := set.root;
      first.sink := set.sink;
      first.copies := first.nextCopy;
      first.copyOf := NIL;
      first.nextCopy := NIL;
      first.prevCopy := NIL;
      (* empty set *)
      set.copies := NIL;
      set.sink := NewNode();
      set.root := set.sink;
      set.noOfElements := 0;
      set.copyOf := NIL;
      set.nextCopy := NIL;
    ELSE
      (* This set is an original with no copies.  Collect its nodes in
         free-list *)
      node := PostorderFirst(set, set.root);
      WHILE node # set.sink DO
        disp := node;
        node := PostorderNext(set, node);
        DisposeNode(disp);
      END;
      set.root := set.sink;
      set.noOfElements := 0;
    END;
    IF insertInFreeList THEN DisposeSet(set); END;
  END Dispose;

PROCEDURE Insert (set: T; READONLY data: Element.T) RAISES {} =
  BEGIN
    WriteAccess(set);
    RBInsert(set, data);
  END Insert;

PROCEDURE ReplaceValue (         set  : T;
                        READONLY data : Element.T;
                        VAR      found: BOOLEAN    ) RAISES {} =
  VAR
    node, parent: Node;
    left        : BOOLEAN;
  BEGIN
    WriteAccess(set);
    Find(
      set, data, CompareData, parent, node, found := found, left := left);
    IF found THEN node.data := data; END;
  END ReplaceValue;


PROCEDURE GetValue (set: T; READONLY data: Element.T; VAR found: BOOLEAN):
  Element.T RAISES {} =
  VAR
    node, parent: Node;
    left        : BOOLEAN;
  BEGIN
    Find(Orig(set), data, CompareData, parent, node, found := found,
         left := left);
    RETURN node.data;
  END GetValue;


PROCEDURE In (set: T; READONLY data: Element.T): BOOLEAN RAISES {} =
  VAR
    node, parent: Node;
    left, found : BOOLEAN;

  BEGIN
    Find(Orig(set), data, CompareData, parent, node, found := found,
         left := left);
    RETURN found;
  END In;


PROCEDURE Compare (    firstSet            : T;
                       secondSet           : T;
                   VAR equal, less, greater: BOOLEAN) RAISES {} =
  BEGIN
    equal := FALSE;
    less := FALSE;
    greater := FALSE;

    WITH fs = Orig(firstSet),
         ss = Orig(secondSet) DO
      IF (fs.noOfElements < ss.noOfElements) THEN
        less := IsSubset(ss, ss);
      ELSIF (fs.noOfElements > ss.noOfElements) THEN
        greater := IsSubset(ss, fs);
      ELSE
        equal := IsSubset(fs, ss);
      END;
    END;
  END Compare;


PROCEDURE IsSubset (firstSet: T; secondSet: T): BOOLEAN RAISES {} =
  VAR node: Node;
  BEGIN
    WITH fs = Orig(firstSet),
         ss = Orig(secondSet) DO
      IF fs.noOfElements > ss.noOfElements THEN RETURN FALSE; END;

      node := InorderFirst(fs);
      WHILE node # fs.sink DO
        IF NOT In(ss, node.data) THEN RETURN FALSE; END;
        node := InorderNext(fs, node);
      END;
    END;
    RETURN TRUE;
  END IsSubset;


PROCEDURE IsStrictSubset (firstSet: T; secondSet: T): BOOLEAN RAISES {} =
  BEGIN
    IF (Orig(firstSet).noOfElements >= Orig(secondSet).noOfElements) THEN
      RETURN FALSE;
    END;
    RETURN IsSubset(firstSet, secondSet);
  END IsStrictSubset;


PROCEDURE IsDisjunct (firstSet: T; secondSet: T): BOOLEAN RAISES {} =
  VAR node: Node;
  BEGIN
    WITH fs = Orig(firstSet),
         ss = Orig(secondSet) DO
      node := InorderFirst(fs);
      WHILE node # fs.sink DO
        IF In(ss, node.data) THEN RETURN FALSE; END;
        node := InorderNext(fs, node);
      END;
    END;
    RETURN TRUE;
  END IsDisjunct;


PROCEDURE Union (firstSet: T; secondSet: T) RAISES {} =
  VAR node: Node;
  BEGIN
    WriteAccess(firstSet);
    WITH ss = Orig(secondSet) DO
      node := InorderFirst(ss);
      WHILE node # ss.sink DO
        RBInsert(firstSet, node.data);
        node := InorderNext(ss, node);
      END;
    END;
  END Union;


PROCEDURE Intersection (firstSet: T; secondSet: T) RAISES {} =
  VAR
    node1, node2, parent, pufferNode: Node;
    found, left         : BOOLEAN;
  BEGIN
    WriteAccess(firstSet);
    WITH ss = Orig(secondSet) DO
      node1 := InorderFirst(firstSet);
      WHILE node1 # firstSet.sink DO
        Find(ss, node1.data, CompareData, parent, node2, found := found,
             left := left);
        pufferNode := InorderNext(firstSet, node1);
        IF NOT found THEN RBDelete(firstSet, node1); END;
        node1 := pufferNode;
      END;
    END;
  END Intersection;


PROCEDURE Difference (firstSet: T; secondSet: T) RAISES {} =
  VAR
    node : Node;
    found: BOOLEAN;
  BEGIN
    WriteAccess(firstSet);
    WITH ss = Orig(secondSet) DO
      node := InorderFirst(ss);
      WHILE node # ss.sink DO
        DeleteElement(firstSet, node.data, found);
        node := InorderNext(ss, node);
      END;
    END;
  END Difference;


PROCEDURE SymmetricDifference (firstSet: T; secondSet: T) RAISES {} =
  VAR
    act, node, parent: Node;
    found, left      : BOOLEAN;
  BEGIN
    WriteAccess(firstSet);
    WITH ss = Orig(secondSet) DO
      act := InorderFirst(ss);
      WHILE act # ss.sink DO
        Find(firstSet, act.data, CompareData, parent, node, found := found,
             left := left);
        IF found THEN
          RBDelete(firstSet, node);
        ELSE
          RBInsert(firstSet, act.data);
        END;
        act := InorderNext(ss, act);
      END;
    END;
  END SymmetricDifference;


PROCEDURE GotoElement (set: T; READONLY data: Element.T; VAR found: BOOLEAN)
  RAISES {} =
  VAR
    node, parent: Node;
    left        : BOOLEAN;

  BEGIN
    Find(Orig(set), data, CompareData, parent, node, found := found,
         left := left);
    IF found THEN set.current := node; END;
  END GotoElement;


PROCEDURE GotoMinimum (set: T) RAISES {} =
  BEGIN
    set.current := InorderFirst(Orig(set));
  END GotoMinimum;


PROCEDURE GotoMaximum (set: T) RAISES {} =
  BEGIN
    WITH s = Orig(set) DO
      set.current := s.root;
      IF s.root # s.sink THEN
        WHILE set.current.right # s.sink DO
          set.current := set.current.right
        END;
      END;
    END;
  END GotoMaximum;


PROCEDURE Loop (set: T) RAISES {} =
  BEGIN
    set.rangeCompare := FullLoopCompare;
    set.current := InorderFirst(Orig(set));
  END Loop;


PROCEDURE RangeLoop (         set             : T;
                              compareProcedure: CompareProcedure;
                     READONLY data            : Element.T         )
  RAISES {IllegalCompare} =
  VAR
    found: BOOLEAN;
    node : Node;

  BEGIN
    WITH s = Orig(set) DO
      set.rangeCompare := compareProcedure;
      set.current := s.root;
      found := FALSE;

      WHILE set.current # s.sink AND NOT found DO
        CASE compareProcedure(data, set.current.data) OF
          CompareResult.Equal => found := TRUE;
        | CompareResult.Less => set.current := set.current.left;
        | CompareResult.Greater => set.current := set.current.right;
        END;
      END;

      IF NOT found THEN RETURN; END;
      node := set.current.left;
      WHILE node # s.sink DO
        CASE compareProcedure(data, node.data) OF
          CompareResult.Equal => set.current := node; node := node.left;
        | CompareResult.Greater => node := node.right;
        | CompareResult.Less => RAISE IllegalCompare;
        END;
      END;
    END;
  END RangeLoop;


PROCEDURE Get (set: T; VAR found: BOOLEAN): Element.T RAISES {} =
  BEGIN
    RETURN GetNode(Orig(set), set.current, found).data;
  END Get;


(* Here begin the internal procedures of CursorSet.  All internal
   procedures must be called with non-copies!!  Exceptions are of course
   Orig, WriteAccess, and RealCopy. *)

PROCEDURE GetNode (set: T; VAR cursor: Node; VAR found: BOOLEAN): Node
  RAISES {} =
  VAR node: Node;
  BEGIN
    found := FALSE;
    IF set.noOfElements = 0 THEN RETURN set.sink END;
    node := cursor;

    IF cursor # set.sink THEN
      found := TRUE;
      cursor := InorderNext(set, cursor);
      IF cursor # set.sink AND set.rangeCompare(cursor.data, node.data)
                                 # CompareResult.Equal THEN
        cursor := set.sink;
      END;
    END;
    RETURN node;
  END GetNode;


PROCEDURE FullLoopCompare (<* UNUSED *> READONLY data1, data2: Element.T):
  CompareResult =
  BEGIN
    RETURN CompareResult.Equal;
  END FullLoopCompare;


PROCEDURE CompareData (READONLY a, b: Element.T): CompareResult =
  BEGIN
    CASE Element.Compare(a, b) OF
      -1 => RETURN CompareResult.Less;
    | 0 => RETURN CompareResult.Equal;
    | 1 => RETURN CompareResult.Greater;
    END;
  END CompareData;

PROCEDURE Find (         set         : T;
                READONLY data        : Element.T;
                         compare     : CompareProcedure;
                VAR      parent, node: Node;
                VAR      found       : BOOLEAN;
                VAR      left        : BOOLEAN           ) =
  BEGIN
    (* if tree is empty, parent and node will be set.sink *)
    (* if root.data = data, parent will be set.sink and node = set.root *)
    (* otherwise, parent will point to the parent of the found node or to
       the node which will become parent of the node with data *)
    found := FALSE;
    parent := set.sink;
    node := set.root;
    LOOP
      IF node = set.sink THEN
        found := FALSE;
        EXIT;
      ELSE
        CASE compare(data, node.data) OF
        | CompareResult.Equal => found := TRUE; EXIT;
        | CompareResult.Less =>
            left := TRUE;
            parent := node;
            node := node.left;
        | CompareResult.Greater =>
            left := FALSE;
            parent := node;
            node := node.right;
        END;
      END;
    END;
  END Find;

PROCEDURE LeftRotate (set: T; node: Node) =
  (* left rotation around node *)
  VAR y: Node;
  BEGIN
    y := node.right;
    node.right := y.left;
    IF y.left # set.sink THEN y.left.parent := node END;
    y.parent := node.parent;
    IF node.parent = set.sink THEN
      (* node was root *)
      set.root := y;
    ELSIF node.parent.left = node THEN
      (* node was a left child *)
      node.parent.left := y;
    ELSE
      node.parent.right := y;
    END;
    y.left := node;
    node.parent := y;
  END LeftRotate;

PROCEDURE RightRotate (set: T; node: Node) =
  (* right rotation around node *)
  VAR y: Node;
  BEGIN
    y := node.left;
    node.left := y.right;
    IF y.right # set.sink THEN y.right.parent := node END;
    y.parent := node.parent;
    IF node.parent = set.sink THEN
      (* node was root *)
      set.root := y;
    ELSIF node.parent.left = node THEN
      (* node was a left child *)
      node.parent.left := y;
    ELSE
      node.parent.right := y;
    END;
    y.right := node;
    node.parent := y;
  END RightRotate;

PROCEDURE RBInsert (set: T; READONLY data: Element.T) RAISES {} =
  VAR
    parent, node: Node;
    found, left : BOOLEAN;
    y           : Node;
  BEGIN
    Find(set, data, CompareData, parent, node, found, left);
    IF NOT found THEN
      (* insert for normal binary tree *)
      node := NewNode();
      node.data := data;
      node.left := set.sink;
      node.right := set.sink;
      node.parent := parent;
      IF parent = set.sink THEN
        (* tree was empty *)
        set.root := node;
      ELSIF left THEN
        parent.left := node;
      ELSE
        parent.right := node;
      END;
      INC(set.noOfElements);
      (* now consider colors (cf.  [1]) (dont use parent in the loop, it
         points to the wrong node after rotation) *)
      node.red := TRUE;
      WHILE node # set.root AND node.parent.red DO
        IF node.parent = node.parent.parent.left THEN
          (* nodes parent is a left child *)
          y := node.parent.parent.right;
          IF y.red THEN
            node.parent.red := FALSE;
            y.red := FALSE;
            node.parent.parent.red := TRUE;
            node := node.parent.parent;
          ELSE
            IF node.parent.right = node THEN
              (* we need double rotation *)
              node := node.parent;
              LeftRotate(set, node);
            END;
            node.parent.red := FALSE;
            node.parent.parent.red := TRUE;
            RightRotate(set, node.parent.parent);
          END;
        ELSE
          (* nodes parent is a right child *)
          y := node.parent.parent.left;
          IF y.red THEN
            node.parent.red := FALSE;
            y.red := FALSE;
            node.parent.parent.red := TRUE;
            node := node.parent.parent;
          ELSE
            IF parent.left = node THEN
              (* we need double rotation *)
              node := node.parent;
              RightRotate(set, node);
            END;
            node.parent.red := FALSE;
            node.parent.parent.red := TRUE;
            LeftRotate(set, node.parent.parent);
          END;
        END;
      END;
      set.root.red := FALSE;
      set.sink.red := FALSE;
    END
  END RBInsert;

PROCEDURE RBDelete (set: T; z: Node) =
  (* cf.  [1] *)

  PROCEDURE TreeSuccessor (z: Node): Node =
    BEGIN
      z := z.left;
      WHILE z.right # set.sink DO z := z.right END;
      RETURN z;
    END TreeSuccessor;

  PROCEDURE FixUp (set: T; x: Node) =
    VAR w: Node;
    BEGIN
      WHILE x # set.root AND NOT x.red DO
        IF x = x.parent.left THEN
          (* x is a left child *)
          w := x.parent.right;
          IF w.red THEN
            w.red := FALSE;
            x.parent.red := TRUE;
            LeftRotate(set, x.parent);
            w := x.parent.right;
          END;
          IF NOT w.left.red AND NOT w.right.red THEN
            w.red := TRUE;
            x := x.parent;
          ELSE
            IF NOT w.right.red THEN
              w.left.red := FALSE;
              w.red := TRUE;
              RightRotate(set, w);
              w := x.parent.right;
            END;
            w.red := x.parent.red;
            x.parent.red := FALSE;
            w.right.red := FALSE;
            LeftRotate(set, x.parent);
            x := set.root;
          END;
        ELSE
          (* x is a right child *)
          w := x.parent.left;
          IF w.red THEN
            w.red := FALSE;
            x.parent.red := TRUE;
            RightRotate(set, x.parent);
            w := x.parent.left;
          END;
          IF NOT w.left.red AND NOT w.right.red THEN
            w.red := TRUE;
            x := x.parent;
          ELSE
            IF NOT w.left.red THEN
              w.right.red := FALSE;
              w.red := TRUE;
              LeftRotate(set, w);
              w := x.parent.left;
            END;
            w.red := x.parent.red;
            x.parent.red := FALSE;
            w.left.red := FALSE;
            RightRotate(set, x.parent);
            x := set.root;
          END;
        END;
      END;
      x.red := FALSE;
    END FixUp;

  VAR y, x: Node;
  BEGIN                          (* RBDelete *)
    IF z = set.current THEN set.current := InorderNext(set, z); END;
    IF z.left = set.sink OR z.right = set.sink THEN
      y := z;
    ELSE
      y := TreeSuccessor(z);
    END;

    IF y.left # set.sink THEN x := y.left; ELSE x := y.right; END;
    (* this is correct even if x = set.sink *)
    (* even more: we need the property x.parent = y for the fixup
       procedure *)
    x.parent := y.parent;

    IF y.parent = set.sink THEN
      (* y was root *)
      set.root := x;
    ELSIF y.parent.left = y THEN
      y.parent.left := x;
    ELSE
      y.parent.right := x;
    END;

    IF y # z THEN
      (* copy y's data to z *)
      z.data := y.data;
    END;
    DEC(set.noOfElements);
    DisposeNode(y);

    IF NOT y.red THEN
      (* the path from root to x has one black edge less now than any other
         path, so we must fix it *)
      FixUp(set, x);
    END;
  END RBDelete;

PROCEDURE InorderFirst (set: T): Node =
  VAR res: Node := set.root;
  BEGIN
    IF set.root = set.sink THEN RETURN set.sink END;
    WHILE res.left # set.sink DO res := res.left END;
    RETURN res;
  END InorderFirst;

PROCEDURE InorderNext (set: T; node: Node): Node =
  BEGIN
    IF node = set.sink THEN RETURN node END;
    IF node.right # set.sink THEN
      (* return smallest node of right sub-tree *)
      node := node.right;
      WHILE node.left # set.sink DO node := node.left; END;
      RETURN node;
    ELSE
      (* find next bigger ancestor (node must be part of the left sub-tree
         of ancestor) *)
      WHILE node.parent # set.sink AND node.parent.left # node DO
        node := node.parent;
      END;
      RETURN node.parent;
    END;
  END InorderNext;

<* INLINE *> PROCEDURE Orig (set: T): T =
  BEGIN
    IF set.copyOf # NIL THEN RETURN set.copyOf; ELSE RETURN set; END;
  END Orig;

PROCEDURE WriteAccess (set: T) =
  VAR first: T;
  BEGIN
    IF set.copyOf # NIL THEN
      RealCopy(set);
    ELSIF set.copies # NIL THEN
      (* Copies exist.  Make first copy a clone of set and set a RealCopy
         of this clone. *)
      first := set.copies;
      first.noOfElements := set.noOfElements;
      first.root := set.root;
      first.sink := set.sink;
      first.copies := first.nextCopy;
      first.copyOf := NIL;
      first.nextCopy := NIL;
      first.prevCopy := NIL;
      (* empty set and make it a copy of first *)
      set.copies := NIL;
      set.sink := NewNode();
      set.root := set.sink;
      set.noOfElements := 0;
      set.copyOf := first;
      set.nextCopy := first.copies;
      IF first.copies # NIL THEN first.copies.prevCopy := set; END;
      first.copies := set;
      RealCopy(set);
    END;
  END WriteAccess;

PROCEDURE RealCopy (copy: T) RAISES {} =
  VAR
    node, parent: Node;
    found, left : BOOLEAN;
    orig        : T;
  BEGIN
    IF copy.copyOf # NIL THEN
      orig := copy.copyOf;
      node := InorderFirst(orig);
      WHILE node # orig.sink DO
        RBInsert(copy, node.data);
        IF node = copy.current THEN
          (* adjust cursor *)
          Find(copy, node.data, CompareData, parent, copy.current, found,
               left);
        END;
        node := InorderNext(orig, node);
      END;
      copy.copyOf := NIL;
      (* remove from copy-list *)
      IF copy.nextCopy # NIL THEN
        copy.nextCopy.prevCopy := copy.prevCopy;
      END;
      IF copy.prevCopy # NIL THEN
        copy.prevCopy.nextCopy := copy.nextCopy;
      ELSE
        orig.copies := copy.nextCopy;
      END;
    END;
  END RealCopy;

(**
PROCEDURE Print (set: T) =
  VAR
    act  : Node;
    depth: CARDINAL;
  PROCEDURE Color (n: Node): TEXT =
    BEGIN
      IF n.red THEN RETURN "r" ELSE RETURN "b" END;
    END Color;
  BEGIN
    IF set.noOfElements # 0 THEN
      (* goto maximum *)
      act := set.root;
      depth := 0;
      WHILE act.right # set.sink DO INC(depth); act := act.right END;
      (* do a 'reverse inorder' walk *)
      REPEAT
        IO.Put(
          Fmt.Pad(Fmt.Int(act.data) & Color(act), 3 * depth + 3) & "\n");
        IF act.left # set.sink THEN
          (* find biggest node of left sub-tree *)
          act := act.left;
          INC(depth);
          WHILE act.right # set.sink DO INC(depth); act := act.right; END;
        ELSE
          (* find next smaller ancestor (act must be part of the right
             sub-tree of ancestor) *)
          WHILE act.parent # set.sink AND act.parent.right # act DO
            act := act.parent;
            DEC(depth);
          END;
          act := act.parent;
          IF act # set.sink THEN DEC(depth); END;
        END;
      UNTIL act = set.sink;
    END;
    IO.Put("\n");
  END Print;
*)

BEGIN
END CursorSet.
