(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the files COPYRIGHT and COPYRIGHT.extras for details.   *)
(*                                                             *)
(* Last Modified On Tue Nov  1 10:14:25 PST 1994 By kalsow     *)
(* Derived from Solve.m3 by kalsow                             *)

MODULE Solve2;

IMPORT RefIntTbl, Word, RefSeq, Solve;
IMPORT Thread, Fmt (*, Wr, Stdio*);

(*--------------------------------------------------------------- control ---*)

TYPE
  Status = { Success, Failure, Aborted, GiveUp, Exhausted, Solved };
(* The interpretation of Success and Failure are context dependent.
|   The others have fixed uses:
|       Aborted   => the user interrupted the search
|       GiveUp    => the depth-first search reached its limit
|       Exhausted => the total search reached its limit
|       Solved    => a solution was found.
*)

CONST
  StopName = ARRAY Status OF TEXT {
    "",
    "Game is not winnable",
    "Aborted",
    "Couldn't find winning move.  Middle click Hint for deep search",
    "Couldn't find winning move.  Middle click Hint for deep search",
    "Solved"
  };

CONST
  StopReason = ARRAY Status OF Solve.WhyStop {
    Solve.WhyStop.Solution,
    Solve.WhyStop.NoSolution,
    Solve.WhyStop.Aborted,
    Solve.WhyStop.GiveUp,
    Solve.WhyStop.Exhausted,
    Solve.WhyStop.Solution
  };

(*----------------------------------------------------------------- cards ---*)

TYPE
  Card = [0..52];
  (*Suit = Solve.Suit;*)  (* Spade, Heart, Diamond, Club *)
  Rank = [0..13];

CONST
  NoCard = 52;

CONST
  RankOf = ARRAY Card OF Rank {
     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
     0
  };

CONST
  CardName = ARRAY Card OF TEXT {
     "Ace of Spades",  "2 of Spades",   "3 of Spades",     "4 of Spades",
     "5 of Spades",    "6 of Spades",   "7 of Spades",     "8 of Spades",
     "9 of Spades",    "10 of Spades",  "Jack of Spades",  "Queen of Spades",
     "King of Spades",
     "Ace of Hearts",  "2 of Hearts",   "3 of Hearts",     "4 of Hearts",
     "5 of Hearts",    "6 of Hearts",   "7 of Hearts",     "8 of Hearts",
     "9 of Hearts",    "10 of Hearts",  "Jack of Hearts",  "Queen of Hearts",
     "King of Hearts",
     "Ace of Diamonds","2 of Diamonds", "3 of Diamonds",   "4 of Diamonds",
     "5 of Diamonds",  "6 of Diamonds", "7 of Diamonds",   "8 of Diamonds",
     "9 of Diamonds",  "10 of Diamonds","Jack of Diamonds","Queen of Diamonds",
     "King of Diamonds",
     "Ace of Clubs",    "2 of Clubs",   "3 of Clubs",      "4 of Clubs",
     "5 of Clubs",      "6 of Clubs",   "7 of Clubs",      "8 of Clubs",
     "9 of Clubs",      "10 of Clubs",  "Jack of Clubs",   "Queen of Clubs",
     "King of Clubs",
     ""
  };

CONST (* "Below[x]" is the card that "fits" below "x" *)
  Below = ARRAY Card OF Card {
    NoCard,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11,
    NoCard, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
    NoCard, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,
    NoCard, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    NoCard
  };


(*------------------------------------------------------- table locations ---*)

TYPE
  Location = [0..17];
  Group = Solve.Group; (* {Foundation, Tableau, Talon} *)

CONST
  First_Foundation = 0;   Last_Foundation = 3;
  First_Tableau    = 4;   Last_Tableau    = 13;
  First_Talon      = 14;  Last_Talon      = 17;

CONST
  GroupOf = ARRAY Location OF Group {
    Group.Foundation, Group.Foundation, Group.Foundation, Group.Foundation,
    Group.Tableau,    Group.Tableau,    Group.Tableau,    Group.Tableau,
    Group.Tableau,    Group.Tableau,    Group.Tableau,    Group.Tableau,
    Group.Tableau,    Group.Tableau,
    Group.Talon,      Group.Talon,      Group.Talon,      Group.Talon
  };

CONST
  GroupName = ARRAY Group OF TEXT {
    "Foundation", "Tableau", "Talon"
  };

(*--------------------------------------------------------------- layouts ---*)

TYPE
  Layout = RECORD (* an arrangement of cards *)
    head : ARRAY Location OF Card;
    next : ARRAY Card OF Card;
  END;

(* return the sum of ranks of the foundation cards *)
PROCEDURE NumFnd (READONLY layout: Layout): CARDINAL =
  VAR fndSize: CARDINAL := 0;
  BEGIN
    FOR i := First_Foundation TO Last_Foundation DO
       INC(fndSize, RankOf [layout.head[i]]);
    END;
    RETURN (fndSize);
  END NumFnd;

(* return card on 'top' of pile 'loc' in layout 'x' *)
PROCEDURE Top (READONLY x: Layout;  loc: Location): Card =
  VAR prev: Card := x.head[loc];  cur := x.next[prev];
  BEGIN
    WHILE cur # NoCard DO prev := cur; cur := x.next[cur]; END;
    RETURN prev;
  END Top;

(* compute the new layout resulting from the move src -> dest *)
PROCEDURE NewLayout (READONLY layout    : Layout;
                     READONLY src, dest : Location;
                   VAR(*OUT*) result    : Tree): Status =
  VAR
    card: Card;
    src_group  := GroupOf [src];
    dest_group := GroupOf [dest];
    tmp        := layout;
  BEGIN
    (* remove card from 'src' *)
    card := tmp.head[src];
    tmp.head[src] := tmp.next[card];

    (* add card to 'dest' *)
    tmp.next[card] := tmp.head[dest];
    tmp.head[dest] := card;

    (* deal with non-uniqueness of layouts by sorting *)
    IF (src_group = Group.Talon) OR (dest_group = Group.Talon) THEN
      SortTalon(tmp);
    END;
    IF (src_group = Group.Tableau) AND (tmp.head[src] = NoCard) THEN
      CollapseTableau(tmp, src);
    END;
    IF (dest_group = Group.Tableau) AND (RankOf[card] = 13) THEN
      MoveKing(tmp, dest);
    END;

    RETURN NewTree (tmp, result);
  END NewLayout;

(*---------------------------------------------------- cannonical layouts ---*)

(* move king into place. Know that positions >'start' are empty *)
PROCEDURE MoveKing (VAR x: Layout;  start: Location) =
  VAR
    i : Location := start - 1;
    card := x.head[start];
  BEGIN
    WHILE (i >= First_Tableau) AND (card < Top (x, i)) DO
      x.head[i + 1] := x.head[i];
      DEC (i);
    END;
    x.head[i + 1] := card;
  END MoveKing;

PROCEDURE CollapseTableau (VAR x: Layout;  start: Location) =
  BEGIN
    FOR i := start TO Last_Tableau-1 DO x.head[i] := x.head[i + 1]; END;
    x.head[Last_Tableau] := NoCard;
  END CollapseTableau;

PROCEDURE Sort (VAR layout: Layout) =
  BEGIN
    SortTableau (layout);
    SortTalon (layout);
  END Sort;

PROCEDURE SortTableau (VAR x: Layout) =
  (* insertion sort *)
  VAR j: Location;  save, key: Card;
  BEGIN
    FOR i := First_Tableau + 1 TO Last_Tableau DO
      save := x.head[i];
      key := Top (x, i);
      j := i - 1;
      WHILE (j >= First_Tableau) AND (key < Top (x, j)) DO
        x.head[j+1] := x.head[j];
        DEC (j);
      END;
      x.head[j+1] := save;
    END;
  END SortTableau;

PROCEDURE SortTalon (VAR x: Layout) =
  (* insertion sort *)
  VAR j: Location;  key: Card;
  BEGIN
    FOR i := First_Talon + 1 TO Last_Talon DO
      key := x.head[i];
      j := i - 1;
      WHILE (j >= First_Talon) AND (key < x.head[j]) DO
        x.head[j+1] := x.head[j];
        DEC (j);
      END;
      x.head[j+1] := key;
    END;
  END SortTalon;

(*------------------------------------------------------------- game tree ---*)


TYPE
  Tree = REF RECORD (* tree of all possible layouts *)
    layout  : Layout;
    parent  : Tree := NIL;
    children: Tree := NIL;
    sibling : Tree := NIL;
    hash    : INTEGER := 0;
  END;

(* add 'child' as a child of 'parent' in the tree *)
PROCEDURE AddToTree (parent, child: Tree) =
  BEGIN
    <*ASSERT child.sibling = NIL AND child.parent=NIL *>
    child.parent    := parent;
    child.sibling   := parent.children;
    parent.children := child;
  END AddToTree;

(*------------------------------------------------------------ hash table ---*)

TYPE
  Key = REFANY;

TYPE
  LayoutTable = RefIntTbl.Default OBJECT OVERRIDES
    keyEqual := EqualProc;
    keyHash  := HashProc;
  END;

VAR
  tbl: LayoutTable;

VAR
  numLayouts1: CARDINAL;  (* number of layouts in this depth-first probe *)
  numLayouts : CARDINAL;  (* total number of layouts evaluated *)
  sizeHTable : CARDINAL;
  hashCnt    : CARDINAL;  (* # of times a layout was hashed *)
  eqCnt      : CARDINAL;  (* # of times EqualProc was called *)
  fastCnt    : CARDINAL;  (* # of times took the fast path through EqualProc *)

VAR (* a standby REF value used during table lookups *)
  layoutPool := NEW (Tree);

PROCEDURE NewTree (READONLY layout: Layout;  VAR(*OUT*) result: Tree): Status =
  BEGIN
    INC(numLayouts);
    INC(numLayouts1);

    IF numLayouts MOD 512 = 0 THEN
      IF Thread.TestAlert() THEN RETURN Status.Aborted; END;
      IF callback # NIL     THEN callback(numLayouts);   END;
    END;

    IF numLayouts1 >= depthLim THEN RETURN Status.GiveUp;    END;
    IF numLayouts >= lim       THEN RETURN Status.Exhausted; END;

    layoutPool.layout := layout;
    layoutPool.hash   := 0;

    IF tbl.put(layoutPool, 0) THEN RETURN Status.Failure; END;

    (* a new entry! *)
    INC(sizeHTable);
    result := layoutPool;
    layoutPool := NEW(Tree); (* allocate a new spare *)
    RETURN Status.Success;
  END NewTree;

PROCEDURE HashProc (<*UNUSED*>tbl: LayoutTable;  READONLY key: Key): Word.T =
  CONST konst = 1234567;
  VAR h: Tree := key;  sum := h.hash;
  BEGIN
    INC(hashCnt);
    IF (sum = 0) THEN
      WITH x = h.layout DO
        FOR i := FIRST (x.head) TO LAST (x.head) DO
          sum := Word.Plus (Word.Times (konst, sum), x.head[i]);
        END;
        FOR i := FIRST (x.next) TO LAST (x.next) DO
          sum := Word.Plus (Word.Times (konst, sum), x.next[i]);
        END;
      END;
      h.hash := sum;
    END;
    RETURN sum;
  END HashProc;

PROCEDURE EqualProc (<*UNUSED*>tbl: LayoutTable; READONLY a, b: Key): BOOLEAN =
  VAR h1: Tree := a;  h2: Tree := b;
  BEGIN
    INC(eqCnt);
    IF h1.hash # h2.hash THEN INC(fastCnt); RETURN FALSE; END;
    RETURN h1.layout = h2.layout;
  END EqualProc;

(*------------------------------------------------------------- debugging ---*)

(********************
PROCEDURE Put (a, b, c: TEXT := NIL) =
  <*FATAL ANY*>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (Stdio.stderr, a); END;
    IF (b # NIL) THEN Wr.PutText (Stdio.stderr, b); END;
    IF (c # NIL) THEN Wr.PutText (Stdio.stderr, c); END;
  END Put;

PROCEDURE PutLayout (READONLY x: Layout) =
  BEGIN
    FOR i := First_Foundation TO Last_Foundation DO
      PutList (x, x.head[i]);
      Put (" ");
    END;
    Put (" -- ");
    FOR i := First_Talon TO Last_Talon DO
      PutList (x, x.head[i]);
      Put (" ");
    END;
    Put (" -- ");
    FOR i := First_Tableau TO Last_Tableau DO
      PutList (x, x.head[i]);
      Put (" ");
    END;
  END PutLayout;

CONST
  CName = ARRAY Card OF TEXT {
   "As","2s","3s","4s","5s","6s","7s","8s","9s","Xs","Js","Qs","Ks",
   "Ah","2h","3h","4h","5h","6h","7h","8h","9h","Xh","Jh","Qh","Kh",
   "Ad","2d","3d","4d","5d","6d","7d","8d","9d","Xd","Jd","Qd","Kd",
   "Ac","2c","3c","4c","5c","6c","7c","8c","9c","Xc","Jc","Qc","Kc",
   "*"
  };

PROCEDURE PutList (READONLY x: Layout;  c: Card) =
  BEGIN
    IF (c = NoCard) THEN
      Put ("[*]");
    ELSE
      Put ("[");
      WHILE (c # NoCard) DO
        Put (CName[c]);
        c := x.next[c];
      END;
      Put ("]");
    END;
  END PutList;

PROCEDURE DumpResult () =
  VAR n := NUMBER (resultArr^);
  BEGIN
    Put ("----- new path --- ", Fmt.Int (n), " moves ---\n");
    FOR i := 0 TO MIN (20, n-1) DO
      PutLayout (resultArr[i].layout);
      Put ("\n");
    END;
  END DumpResult;
******************************)

(*-------------------------------------------------------- solution cache ---*)
VAR
  resultArr : REF ARRAY OF Tree := NIL;
  resultInd : CARDINAL;

PROCEDURE KnownResult (READONLY layout: Layout): TEXT =
(* If 'layout' is a known layout in the cached solution,
   return a description of the next move.  Otherwise, return NIL. *)
  BEGIN
    IF resultArr = NIL THEN RETURN NIL END;
    FOR i := MAX (resultInd-3, 0) TO MIN(resultInd+25, LAST (resultArr^)-1) DO
      WITH x = resultArr[i].layout DO
        IF layout = x THEN
          resultInd := i + 1;
          RETURN DescribeMove (x, resultArr[resultInd].layout);
        END;
      END;
    END;
    IF NumFnd (layout) = 52 THEN RETURN "Game is already won" END;
    RETURN NIL;
  END KnownResult;

(*
 * RecordResult records the winning moves in resultArr.
 *)
PROCEDURE RecordResult (tree: Tree) =
  VAR n: INTEGER;  t: Tree;
  BEGIN
    (* first, find the path length *)
    t := tree;  n := 0;
    WHILE (t # NIL) DO INC (n);  t := t.parent; END;

    (* allocate space *)
    resultArr := NEW (REF ARRAY OF Tree, n);

    (* finally, copy the values into the cache *)
    t := tree;
    WHILE (t # NIL) DO DEC (n);  resultArr[n] := t;  t := t.parent;  END;

    resultInd := 0;
  END RecordResult;

PROCEDURE DescribeMove (READONLY a, b: Layout): TEXT =
  VAR card: Card;  src, dest: Location;
  BEGIN
    ComputeMove(a, b, card, src, dest);
    RETURN Fmt.F("%s: %s -> %s", CardName[card], GroupName[GroupOf[src]],
                    GroupName[GroupOf[dest]]);
  END DescribeMove;

TYPE CardMap = ARRAY Card OF Location;

(* determine the single move that gets from 'layout1' to 'layout2' *)
PROCEDURE ComputeMove (READONLY     layout1, layout2: Layout;
                       VAR (* out*) card            : Card;
                       VAR (* out*) src, dest       : Location ) =
  VAR before, after: CardMap;
  VAR top1, top2: ARRAY Location OF Card;
  VAR canon1, canon2: Layout;
  BEGIN
    FindLocations (layout1, before);
    FindLocations (layout2, after);

    (* find the card that changed groups or was restacked *)
    FOR i := FIRST (before) TO LAST (before) DO
      IF (GroupOf[before[i]] # GroupOf[after[i]])
        OR (layout1.next[i] # layout2.next[i]) THEN
        card := i;
        src  := before[i];
        dest := after[i];
        RETURN;
      END;
    END;

    (* we're dead... *)
    FOR i := FIRST (top1) TO LAST (top1) DO top1[i] := Top(layout1, i) END;
    FOR i := FIRST (top2) TO LAST (top2) DO top2[i] := Top(layout2, i) END;
    canon1 := layout1;  Sort (canon1);
    canon2 := layout2;  Sort (canon2);

    <*ASSERT FALSE*>
  END ComputeMove;

PROCEDURE FindLocations (READONLY layout: Layout;  VAR loc: CardMap) =
  VAR c: Card;
  BEGIN
    loc [NoCard] := LAST (Location);
    FOR i := FIRST (layout.head) TO LAST (layout.head) DO
      c := layout.head[i];
      WHILE (c # NoCard) DO loc[c] := i;  c := layout.next[c];  END;
    END;
  END FindLocations;

(*------------------------------------------------------- move generation ---*)

(*
 * Returns Status.Success if the card at 'loc' has a place to move to,
 * returns Status.Failure if it doesn't.
 *
 * If a move is found, it is used to generate a new layout, which is
 * returned in result.
 *
 * The only case in which there are two spots to move to is if the Talon
 * and Tableau are both possible.  FindSpot returns Talon first.  Then
 * a child of this move will contain the other possibility, namely the
 * move to the Tableau.
 *)

PROCEDURE FindFoundationSpot (READONLY layout : Layout;
                              READONLY loc    : Location;
                            VAR(*OUT*) result : Tree): Status =
  VAR
    card := layout.head [loc];
    dest : Location := First_Foundation + (card DIV 13);
  BEGIN
    IF layout.head[dest] = Below [card]
      THEN RETURN NewLayout(layout, loc, dest, result);
      ELSE RETURN Status.Failure;
    END;
  END FindFoundationSpot;

PROCEDURE FindTalonSpot (READONLY layout : Layout;
                         READONLY loc    : Location;
                       VAR(*OUT*) result : Tree): Status =
  VAR
    card := layout.head [loc];
    dest : Location;
  BEGIN
    (* Don't move a single king to talon. *)
    IF (RankOf [card] = 13) AND (layout.next[card] = NoCard) THEN
      RETURN Status.Failure;
    END;

    (* consider moving the card to the talon *)
    IF (layout.head[Last_Talon] = NoCard) THEN
      (* there's a free spot *)
      dest := Last_Talon-1;
      WHILE (dest >= First_Talon) AND (layout.head[dest] = NoCard) DO
        DEC (dest);
      END;
      RETURN NewLayout (layout, loc, dest+1, result);
    END;

    RETURN Status.Failure;
  END FindTalonSpot;

PROCEDURE FindTableauSpot (READONLY layout : Layout;
                           READONLY loc    : Location;
                         VAR(*OUT*) result : Tree): Status =
  VAR
    card   := layout.head [loc];
    rank   := RankOf [card];
    status : Status;
    c      : Card;
  BEGIN
    (* Don't move a single king from tableau to tableau. *)
    IF (RankOf [card] = 13)
      AND (GroupOf[loc] = Group.Tableau)
      AND (layout.next[card] = NoCard) THEN
      RETURN Status.Failure;
    END;

    (* finally, try moving the card to the tableau *)
    FOR i := First_Tableau TO Last_Tableau DO
      IF (i # loc) THEN
        c := layout.head[i];
        IF (c # NoCard AND card = Below[c])
        OR (c = NoCard AND rank = 13) THEN
          status := NewLayout(layout, loc, i, result);
          IF (status # Status.Failure) THEN RETURN status END;
        END;
      END;
    END;
    RETURN Status.Failure;
  END FindTableauSpot;

(*
 * Identify children and add them to 'tree'.
 *    Status.Success => found zero or more children
 *    Status.Failure => ** is never returned **
 *    otherwise, bailing out.
 *)
PROCEDURE FindChildren (tree: Tree): Status =
  VAR result: Tree;  status: Status;
  BEGIN
    WITH layout = tree.layout DO
      FOR i := First_Tableau TO Last_Talon DO
        IF layout.head[i] # NoCard THEN
          status := FindFoundationSpot(layout, i, result);
          IF (status # Status.Failure) THEN
            IF (status = Status.Success) THEN AddToTree(tree, result); END;
            RETURN status;
          END;
        END;
      END;

      FOR i := First_Tableau TO Last_Tableau DO
        IF layout.head[i] # NoCard THEN
          status := FindTalonSpot(layout, i, result);
          IF    (status = Status.Failure) THEN (* can't move this card *)
          ELSIF (status = Status.Success) THEN AddToTree(tree, result);
          ELSE                                 RETURN status;
          END;
        END;
      END;

      FOR i := First_Tableau TO Last_Talon DO
        IF layout.head[i] # NoCard THEN
          status := FindTableauSpot(layout, i, result);
          IF    (status = Status.Failure) THEN (* can't move this card *)
          ELSIF (status = Status.Success) THEN AddToTree(tree, result);
          ELSE                                 RETURN status;
          END;
        END;
      END;
    END;

    RETURN Status.Success;
  END FindChildren;

(*
 * Recursively generate all possible layouts reachable from 'tree'.
 *    Status.Failure => no winning position is reachable
 *    Status.Success => ** is never returned **
 *    otherwise, bailing out.
 *)
PROCEDURE Generate (tree: Tree): Status =
  VAR status: Status;
  BEGIN
    (* if there is a move to foundation, generate only that move *)
    status := FindChildren(tree);
    IF (status # Status.Success) THEN RETURN status; END;

    IF tree.children = NIL THEN
      (* no legal moves from this point *)
      IF NumFnd(tree.layout) = 52 THEN
        RecordResult(tree);
        RETURN Status.Solved;
      END;
      RETURN Status.Failure;
    END;

    (* now, call Generate on children *)
    tree := tree.children;
    WHILE (tree # NIL) DO
      status := Generate(tree);
      IF (status # Status.Failure) THEN RETURN status; END;
      tree := tree.sibling;
    END;

    RETURN Status.Failure;
  END Generate;

(* do depth-first search of each position on queue *)
PROCEDURE GenerateDepth (queue: RefSeq.T): Status=
  VAR
    max    := 0;
    min    := LAST(INTEGER);
    result := Status.Failure;
    status : Status;
    n      : INTEGER;
    tmp    : Tree;
  BEGIN
    (* find the range of foundation values *)
    FOR i := 0 TO queue.size () - 1 DO
      tmp := queue.get (i);
      n := NumFnd(tmp.layout);
      max := MAX(max, n);
      min := MIN(min, n);
    END;

    (* search the positions in decreasing foundation order *)
    FOR i := max TO min BY -1 DO
      FOR j := 0 TO queue.size () - 1 DO
        tmp := queue.get (j);
        IF NumFnd (tmp.layout) = i THEN
          numLayouts1 := 0;  (* reset the depth-first limit *)
          status := Generate (tmp);
          IF (status = Status.Failure) THEN
            (* nothing found *)
          ELSIF (status = Status.GiveUp) THEN
            (* bottomed-out *)
            result := Status.GiveUp;
          ELSE
            (* solved it or hit some hard limit *)
            RETURN status;
          END;
        END;
      END;
    END;

    RETURN result;
  END GenerateDepth;

(*
 * Generate tree of all possible layouts in level order, then
 * call GenerateDepth on each leaf.
 *    Status.Failure => game is not winnable
 *    Status.Success => ** is never returned **
 *)
PROCEDURE GenerateBreadth (tree: Tree): Status =
  VAR
    queue  := NEW(RefSeq.T).init(MIN (cutOver, 1000));
    status : Status;
    n      : INTEGER;
  BEGIN
    (* expand the queue with a breadth-first search until it contains
       at least 'cutOver' elements *)
    queue.addhi (tree);  n := 1;
    WHILE (n > 0) AND (n < cutOver) DO
      tree := queue.remlo ();  DEC (n);
      status := FindChildren (tree);
      IF (status # Status.Success) THEN RETURN status; END;

      IF tree.children = NIL THEN
        (* no legal moves from this point *)
        IF NumFnd(tree.layout) = 52 THEN
          RecordResult(tree);
          RETURN Status.Solved;
        END;
      END;

      (* now, add the children *)
      tree := tree.children;
      WHILE (tree # NIL) DO
        queue.addhi (tree);  INC (n);
        tree := tree.sibling;
      END;
    END;

    depthLim := userDepthLim;
    RETURN GenerateDepth (queue);
  END GenerateBreadth;

(*------------------------------------------------------------------ main ---*)

VAR
  cutOver      : CARDINAL;
  userDepthLim : CARDINAL;
  lim          : CARDINAL;
  depthLim     : CARDINAL;
  callback     : Solve.Callback;

PROCEDURE NextMove (READONLY initial  : Solve.Layout;
                         VAR whyStop  : Solve.WhyStop;
                             depth    : CARDINAL;
                             breadth  : CARDINAL;
                             total    : CARDINAL;
                  <*UNUSED*> vbose    : BOOLEAN;
                             callbck  : Solve.Callback): TEXT =
  VAR
    layout  : Layout;
    txt     : TEXT;
    tree    : Tree;
    status  : Status;
  BEGIN
    Encode (initial, layout);
    Sort(layout);
    txt := KnownResult (layout);
    IF (txt # NIL) THEN
      whyStop := Solve.WhyStop.Solution;
      RETURN txt;
    END;

    (* make args global *)
    lim := total;
    userDepthLim := depth;
    cutOver := breadth;
    callback := callbck;

    (* initialize the globals *)
    depthLim := LAST(INTEGER);
    tbl := NEW (LayoutTable).init (20000);
    numLayouts1 := 0;
    numLayouts := 0;
    sizeHTable := 0;
    eqCnt := 0;
    hashCnt := 0;
    fastCnt := 0;
    status := NewTree(layout, tree);   <*ASSERT status = Status.Success*>
    status := GenerateBreadth(tree);

    IF (status = Status.Solved)
      THEN txt := KnownResult (layout);
      ELSE txt := StopName [status];
    END;
    whyStop := StopReason [status];

    (* give the garbage collector a chance *)
    EVAL tbl.init (0);
    tbl := NIL;
    IF resultArr # NIL THEN 
      FOR i := FIRST (resultArr^) TO LAST (resultArr^) DO
        IF resultArr[i] # NIL THEN
          resultArr[i].children := NIL;
          resultArr[i].parent   := NIL;
          resultArr[i].sibling  := NIL;
        END;
      END;
    END;

    RETURN txt;
  END NextMove;

PROCEDURE Encode (READONLY x: Solve.Layout;  VAR(*OUT*) y: Layout) =
  (* convert a Solve layout to our format *)
  VAR l, m: Solve.CardList;  c, d: Card;
  BEGIN
    FOR i := FIRST (y.head) TO LAST (y.head) DO y.head[i] := NoCard END;
    FOR i := FIRST (y.next) TO LAST (y.next) DO y.next[i] := NoCard END;

    (* build the foundation chains *)
    FOR i := 1 TO 4 DO
      c := CvtCard (x.fnd[i]);
      IF (c # NoCard) THEN
        y.head [First_Foundation + (c DIV 13)] := c;
        d := Below[c];
        WHILE (d # NoCard) DO
          y.next[c] := d;
          c := d;
          d := Below[c];
        END;
      END;
    END;

    (* build the talon *)
    FOR i := 1 TO 4 DO
      y.head[i+(First_Talon-1)] := CvtCard (x.tal[i]);
    END;

    (* and finally, rebuild the tableau *)
    FOR i := 1 TO 10 DO
      l := x.tab[i];
      IF (l # NIL) THEN
        c := CvtCard (l.card);
        y.head[i+(First_Tableau-1)] := c;
        WHILE (l # NIL) DO
          m := l.nxt;
          IF (m # NIL) THEN
            d := CvtCard (m.card);
            y.next[c] := d;
            c := d;
          END;
          l := m;
        END;
      END;
    END;
  END Encode;

PROCEDURE CvtCard (READONLY c: Solve.CardType): Card =
  BEGIN
    IF (c.val = 0)
      THEN RETURN NoCard;
      ELSE RETURN 13 * ORD (c.suit) + c.val - 1;
    END;
  END CvtCard;

BEGIN
END Solve2.
