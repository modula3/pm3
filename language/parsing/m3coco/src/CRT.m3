MODULE CRT;

(* CRT   Table Handler
   ===   =============

  (1) handles a symbol table for terminals, pragmas and nonterminals
  (2) handles a table for character classes (for scanner generation)
  (3) handles a top-down graph for productions
  (4) computes various sets (start symbols, followers, any sets)
  (5) contains procedures for grammar tests

  --------------------------------------------------------------------*)

IMPORT Wr, Stdio, Fmt, Text, Process ;

<* FATAL ANY *>

CONST
  maxSetNr   = 256;  (* max. number of symbol sets *)
(* moved next declaration to def module Fri  08-20-1993, and was 150
  maxClasses = 250;  (* max. number of character classes *) *)
  maxNames   = 100;  (* max. number of declared token names *)

TYPE
  FirstSets   = ARRAY [0 .. maxNt] OF RECORD
    ts:    Set;      (* terminal symbols *)
    ready: BOOLEAN;  (* TRUE = ts is complete *)
  END;
  FollowSets  = ARRAY [0 .. maxNt] OF RECORD
    ts:  Set;        (* terminal symbols *)
    nts: Set;        (* nts whose start set is to be included in ts *)
  END;
  CharClass   = RECORD
    name: TEXT;      (* class name *)
    set:  INTEGER    (* ptr to set representing the class *)
  END;
  SymbolTable = ARRAY [0 .. maxSymbols] OF SymbolNode;
  ClassTable  = ARRAY [0 .. maxClasses] OF CharClass;
  GraphList   = ARRAY [0 .. maxNodes] OF GraphNode;
  SymbolSet   = ARRAY [0 .. maxSetNr] OF Set;
  NameTable   = ARRAY [1 .. maxNames] OF RECORD name, definition: TEXT END;

VAR
  (* moved symbol table to the heap Fri  08-20-1993 to allow larger one *)
  st:        REF  SymbolTable; (* symbol table for terminals,
                                         pragmas, and nonterminals *)
  gn:        REF  GraphList; (* top-down graph *)
  tt:        NameTable;   (* table of token name declarations *)
  first:     FirstSets;   (* first[i]  = first symbols of st[i+firstNt] *)
  follow:    FollowSets;  (* follow[i] = followers of st[i+firstNt] *)
  chClass:   ClassTable;  (* character classes *)
  set:       SymbolSet;   (* set[0] = all SYNC symbols *)
  maxSet:    INTEGER;     (* index of last symbol set *)
  lastName,
  dummyName: CARDINAL;    (* for unnamed character classes *)
  ch:        CHAR;


(* Restriction          Implementation restriction
----------------------------------------------------------------------*)
PROCEDURE Restriction (n : INTEGER ; <*UNUSED*> limit: INTEGER) =
(* Fri  08-20-1993 extended *)
  BEGIN
    Wr.PutText(Stdio.stdout, "Restriction  ") ;
    Wr.PutText(Stdio.stdout, Fmt.Int(n) & "\n") ;
    CASE n OF
       1 => Wr.PutText(Stdio.stdout, "Too many graph nodes")
    |  2 => Wr.PutText(Stdio.stdout, "Too many symbols")
    |  3 => Wr.PutText(Stdio.stdout, "Too many sets")
    |  4 => Wr.PutText(Stdio.stdout, "Too many character classes")
    |  5 => Wr.PutText(Stdio.stdout, "Too many conditions")
    |  6 => Wr.PutText(Stdio.stdout, "Too many token names")
    |  7 => Wr.PutText(Stdio.stdout, "Too many states")
    ELSE
      Wr.PutText(Stdio.stdout, "Compiler error")
    END ;
    IF (n > 0) THEN
      Wr.PutText(Stdio.stdout, " (limited to " & Fmt.Int(n) & ")\n")
    END ;
(* maybe we want CRX.WriteStatistics; *)
    Process.Exit(1)
  END Restriction;

(* MovePragmas          Move pragmas after terminals
----------------------------------------------------------------------*)
PROCEDURE MovePragmas() =
  VAR
    i: INTEGER;
  BEGIN
    IF maxP > firstNt THEN
      i := maxSymbols - 1; maxP := maxT;
      WHILE i > lastNt DO
        INC(maxP); IF maxP >= firstNt THEN Restriction(2, maxSymbols) END;
        st^[maxP] := st^[i]; DEC(i)
      END;
    END
  END MovePragmas;

(* ClearMarkList        Clear mark list m
----------------------------------------------------------------------*)
PROCEDURE ClearMarkList (VAR m: MarkList) =
  BEGIN
    m := MarkList{}
  END ClearMarkList;

(* GetNode              Get node with index gp in n
----------------------------------------------------------------------*)
PROCEDURE GetNode (gp: INTEGER; VAR n: GraphNode) =
  BEGIN
    n := gn^[gp]
  END GetNode;

(* PutNode              Replace node with index gp by n
----------------------------------------------------------------------*)
PROCEDURE PutNode (gp: INTEGER; n: GraphNode) =
  BEGIN
    gn^[gp] := n
  END PutNode;

(* NewName              Collects a user defined token name
----------------------------------------------------------------------*)
PROCEDURE NewName (n: TEXT; s: TEXT) =
  BEGIN
    IF lastName = maxNames THEN Restriction(6, maxNames)
    ELSE
      INC(lastName); symNames := TRUE;
      tt[lastName].name := n;
      tt[lastName].definition := s
    END;
  END NewName;

(* NewSym               Generate a new symbol and return its index
----------------------------------------------------------------------*)
PROCEDURE NewSym (typ: INTEGER; name: TEXT; line: INTEGER): INTEGER =
  VAR
    i: INTEGER;
  BEGIN
    IF (maxT + 1) = firstNt THEN Restriction(2, maxSymbols)
    ELSE
      CASE typ OF
        t=>  INC(maxT); i := maxT;
      | pr=> DEC(maxP); DEC(firstNt); DEC(lastNt); i := maxP;
      | nt, unknown=> DEC(firstNt); i := firstNt;
      ELSE
        Restriction(-1, 0)
      END;
      IF maxT >= maxTerminals THEN Restriction(2, maxSymbols) END;
      st^[i].typ := typ;
      st^[i].name := name;
      st^[i].constant := "";
      st^[i].struct := 0;
      st^[i].deletable := FALSE;
      st^[i].attrPos.beg := -1 ;
      st^[i].semPos.beg  := -1 ;
      st^[i].line := line;
    END;
    RETURN i;
  END NewSym;

(* GetSym               Get symbol sp in sn
----------------------------------------------------------------------*)
PROCEDURE GetSym (sp: INTEGER; VAR sn: SymbolNode) =
  BEGIN
    sn := st^[sp]
  END GetSym;

(* PutSym               Replace symbol with index snix by sn
----------------------------------------------------------------------*)
PROCEDURE PutSym (sp: INTEGER; READONLY sn: SymbolNode) =
  BEGIN
    st^[sp] := sn
  END PutSym;

(* FindSym              Find index of symbol with name n
----------------------------------------------------------------------*)
PROCEDURE FindSym (n: TEXT): INTEGER =
  VAR
    i: INTEGER;
  BEGIN
    i := 0; (*search in terminal list*)
    WHILE (i <= maxT) AND (NOT Text.Equal(st^[i].name, n)) DO
      INC(i)
    END;
    IF i <= maxT THEN RETURN i END;
    i := firstNt; (*search in nonterminal/pragma list*)
    WHILE (i < maxSymbols) AND (NOT Text.Equal(st^[i].name, n)) DO
      INC(i)
    END;
    IF i < maxSymbols THEN RETURN i ELSE RETURN noSym END
  END FindSym;

(* PrintSet             Print set s
----------------------------------------------------------------------*)
PROCEDURE PrintSet (wr: Wr.T; VAR s: Set; indent: INTEGER) =
  CONST
    maxLineLen = 80;
  VAR
    col, i, len: INTEGER;
    empty:       BOOLEAN;
    sn:          SymbolNode;
  BEGIN
    i := 0; col := indent; empty := TRUE;
    WHILE i <= maxT DO
      IF (i IN s) THEN
        empty := FALSE; GetSym(i, sn); len := Text.Length(sn.name);
        IF (col + len + 2) > maxLineLen THEN
          Wr.PutChar(wr, '\n'); col := 1;
          WHILE col < indent DO Wr.PutChar(wr, ' '); INC(col) END
        END;
        Wr.PutText(wr, sn.name & "  ") ;
        INC(col, len + 2)
      END;
      INC(i)
    END;
    IF empty THEN Wr.PutText(wr, "-- empty set --") END;
    Wr.PutChar(wr, '\n')
  END PrintSet;

(* NewSet               Stores s as a new set and return its index
----------------------------------------------------------------------*)
PROCEDURE NewSet (READONLY s: Set): INTEGER =
(*any-set computation requires not to search if s is already in set*)
  BEGIN
    INC(maxSet); IF maxSet > maxSetNr THEN Restriction(3, maxSetNr) END;
    set[maxSet] := s; RETURN maxSet
  END NewSet;

(* CompFirstSet         Compute first symbols of (sub) graph at gp
----------------------------------------------------------------------*)
PROCEDURE CompFirstSet (gp: INTEGER; VAR fs: Set) =
  VAR
    visited: MarkList;

  PROCEDURE CompFirst (gp: INTEGER; VAR fs: Set) =
    VAR
      s:  Set;
      gn: GraphNode;
      sn: SymbolNode;
    BEGIN
      fs := Set{} ;
      WHILE (gp # 0) AND (NOT (gp IN visited)) DO
        GetNode(gp, gn); visited := visited + MarkList{gp} ;
        CASE gn.typ OF
          nt=>
            IF first[gn.p1 - firstNt].ready THEN
              fs := fs + first[gn.p1 - firstNt].ts ;
            ELSE
              GetSym(gn.p1, sn);
              CompFirst(sn.struct, s);
              fs := fs + s ;
            END;
        | t, wt=>
            fs := fs + Set{gn.p1}
        | any=>
            fs := fs + set[gn.p1]
        | alt, iter, opt=>
            CompFirst(gn.p1, s);
            fs := fs + s ;
            IF gn.typ = alt THEN
              CompFirst(gn.p2, s);
              fs := fs + s
            END
        ELSE (* eps, sem, sync, ind: nothing *)
        END;
        IF NOT  DelNode(gn) THEN RETURN END;
        gp := ABS(gn.next)
       END
    END CompFirst;

  BEGIN (* ComputeFirstSet *)
    ClearMarkList(visited);
    CompFirst(gp, fs);
    IF ddt['I'] THEN
      Wr.PutText(Stdio.stdout, "ComputeFirstSet: gp = " & Fmt.Int(gp) & "\n") ;
      PrintSet(Stdio.stdout, fs, 0);
    END;
  END CompFirstSet;

(* CompFirstSets        Compute first symbols of nonterminals
----------------------------------------------------------------------*)
PROCEDURE CompFirstSets() =
  VAR
    i:  INTEGER;
    sn: SymbolNode;
  BEGIN
    i := firstNt;
    WHILE i <= lastNt DO first[i - firstNt].ready := FALSE; INC(i) END;
    i := firstNt;
    WHILE i <= lastNt DO (* for all nonterminals *)
      GetSym(i, sn); CompFirstSet(sn.struct, first[i - firstNt].ts);
      first[i - firstNt].ready := TRUE;
      INC(i)
    END;
  END CompFirstSets;

(* CompExpected     Compute symbols expected at location gp in Symbol sp
----------------------------------------------------------------------*)
PROCEDURE CompExpected (gp, sp: INTEGER; VAR exp: Set) =
  BEGIN
    CompFirstSet(gp, exp);
    IF DelGraph(gp) THEN
      exp := exp + follow[sp - firstNt].ts
    END
  END CompExpected;

(* CompFollowSets       Get follow symbols of nonterminals
----------------------------------------------------------------------*)
PROCEDURE CompFollowSets() =
  VAR
    sn:      SymbolNode;
    curSy:   INTEGER;
    visited: MarkList;

  PROCEDURE CompFol (gp: INTEGER) =
    VAR
      s:  Set;
      gn: GraphNode;
    BEGIN
      WHILE (gp > 0) AND (NOT (gp IN visited)) DO
        GetNode(gp, gn);
        visited := visited + MarkList{gp} ;
        IF gn.typ = nt THEN
          CompFirstSet(ABS(gn.next), s);
          follow[gn.p1 - firstNt].ts := follow[gn.p1 - firstNt].ts + s ;
          IF DelGraph(ABS(gn.next)) THEN
            follow[gn.p1 - firstNt].nts := follow[gn.p1 - firstNt].nts
                                                         + Set{curSy - firstNt}
          END
        ELSIF (gn.typ=opt) OR (gn.typ=iter) THEN CompFol(gn.p1)
        ELSIF gn.typ = alt THEN CompFol(gn.p1); CompFol(gn.p2)
        END;
        gp := gn.next
      END
    END CompFol;

  PROCEDURE Complete (i: INTEGER) =
    VAR
      j: INTEGER;
    BEGIN
      IF (i IN visited) THEN RETURN END;
      visited := visited + MarkList{i} ;
      j := 0;
      WHILE j <= (lastNt - firstNt) DO (* for all nonterminals *)
        IF (j IN follow[i].nts) THEN
          Complete(j);
          follow[i].ts := follow[i].ts + follow[j].ts ;
          follow[i].nts := follow[i].nts - Set{j} ;
        END;
        INC(j)
      END;
    END Complete;

  BEGIN (* GetFollowSets *)
    curSy := firstNt;
    WHILE curSy <= (lastNt + 1) DO (* also for dummy root nt*)
      follow[curSy - firstNt].ts := Set{} ;
      follow[curSy - firstNt].nts := Set{} ;
      INC(curSy)
    END;

    curSy := firstNt;         (*get direct successors of nonterminals*)
    WHILE curSy <= lastNt DO
      GetSym(curSy, sn); ClearMarkList(visited); CompFol(sn.struct);
      INC(curSy)
    END;
    CompFol(root); (*curSy=lastNt+1*)

    curSy := 0;               (*add indirect successors to follow.ts*)
    WHILE curSy <= (lastNt - firstNt) DO
      ClearMarkList(visited); Complete(curSy);
      INC(curSy);
    END;
  END CompFollowSets;

(* CompAnySets          Compute all any-sets
----------------------------------------------------------------------*)
PROCEDURE CompAnySets() =
  VAR
    curSy : INTEGER;
    sn    : SymbolNode;

  PROCEDURE LeadingAny (gp: INTEGER; VAR a: GraphNode): BOOLEAN =
    VAR
      gn: GraphNode;
    BEGIN
      IF gp <= 0 THEN RETURN FALSE END;
      GetNode(gp, gn);
      IF (gn.typ = any) THEN a := gn; RETURN TRUE
      ELSE
        RETURN ((gn.typ = alt) AND (LeadingAny(gn.p1, a)
               OR LeadingAny(gn.p2, a)))
               OR (((gn.typ=opt) OR (gn.typ=iter)) AND LeadingAny(gn.p1, a))
               OR (DelNode(gn) AND LeadingAny(gn.next, a))
      END
    END LeadingAny;

  PROCEDURE FindAS (gp: INTEGER) =
    VAR
      gn, gn2, a: GraphNode;
      s1, s2:     Set;
      p:          INTEGER;
    BEGIN
      WHILE gp > 0 DO
        GetNode(gp, gn);
        IF (gn.typ=opt) OR (gn.typ=iter) THEN
          FindAS(gn.p1);
          IF LeadingAny(gn.p1, a) THEN
            CompExpected(ABS(gn.next), curSy, s1);
            set[a.p1] := set[a.p1] - s1 ;
          END
        ELSIF gn.typ = alt THEN
          p := gp; s1 := Set{} ;
          WHILE p # 0 DO
            GetNode(p, gn2); FindAS(gn2.p1);
            IF LeadingAny(gn2.p1, a) THEN
              CompExpected(gn2.p2, curSy, s2);
              s2 := s2 + s1 ;
              set[a.p1] := set[a.p1] - s2 ;
            ELSE
              CompFirstSet(gn2.p1, s2);
              s1 := s1 + s2
            END;
            p := gn2.p2
          END
        END;
        gp := gn.next
      END
    END FindAS;

  BEGIN
    curSy := firstNt;
    WHILE curSy <= lastNt DO (* for all nonterminals *)
      GetSym(curSy, sn); FindAS(sn.struct);
      INC(curSy)
    END
  END CompAnySets;

(* CompSyncSets         Compute follow symbols of sync-nodes
----------------------------------------------------------------------*)
PROCEDURE CompSyncSets() =
  VAR
    curSy:   INTEGER;
    sn:      SymbolNode;
    visited: MarkList;

  PROCEDURE CompSync (gp: INTEGER) =
    VAR
      s:  Set;
      gn: GraphNode;
    BEGIN
      WHILE (gp > 0) AND NOT (gp IN visited) DO
        GetNode(gp, gn);
        visited := visited + MarkList{gp} ;
        IF gn.typ = sync THEN
          CompExpected(ABS(gn.next), curSy, s);
          s := s + Set{eofSy} ;
          set[0] := set[0] + s ;
          gn.p1 := NewSet(s); PutNode(gp, gn)
        ELSIF gn.typ = alt THEN CompSync(gn.p1); CompSync(gn.p2)
        ELSIF (gn.typ=opt) OR (gn.typ=iter) THEN CompSync(gn.p1)
        END;
        gp := gn.next
      END
    END CompSync;

  BEGIN
    curSy := firstNt; ClearMarkList(visited);
    WHILE curSy <= lastNt DO
      GetSym(curSy, sn); CompSync(sn.struct);
      INC(curSy);
    END
  END CompSyncSets;

(* CompDeletableSymbols Compute all deletable symbols and print them
----------------------------------------------------------------------*)
PROCEDURE CompDeletableSymbols(wr : Wr.T) =
  VAR
    changed, none: BOOLEAN;
    i:             INTEGER;
    sn:            SymbolNode;
  BEGIN
    REPEAT
      changed := FALSE;
      i := firstNt;
      WHILE i <= lastNt DO (*for all nonterminals*)
        GetSym(i, sn);
        IF NOT ( sn.deletable) AND DelGraph(sn.struct) THEN
          sn.deletable := TRUE; PutSym(i, sn); changed := TRUE
        END;
        INC(i)
      END;
    UNTIL NOT  changed;

    Wr.PutText(wr, "Deletable symbols:") ;
    i := firstNt; none := TRUE;
    WHILE i <= lastNt DO
      GetSym(i, sn);
      IF sn.deletable THEN
        none := FALSE;
        Wr.PutText(wr, "\n     " & sn.name) ;
      END;
      INC(i);
    END;
    IF none THEN Wr.PutText(wr, "        -- none --") END ;
    Wr.PutChar(wr, '\n');
  END CompDeletableSymbols;

(* CompSymbolSets       Get first-sets, follow-sets, and sync-set
----------------------------------------------------------------------*)
PROCEDURE CompSymbolSets(wr : Wr.T) =
  VAR
    i:  INTEGER;
    sn: SymbolNode;
  BEGIN
    MovePragmas();
    CompDeletableSymbols(wr);
    CompFirstSets();
    CompFollowSets();
    CompAnySets();
    CompSyncSets();
    IF ddt['F'] THEN
      i := firstNt;
      Wr.PutText(wr, "List of first & follow symbols:\n\n") ;
      WHILE i <= lastNt DO (* for all nonterminals *)
        GetSym(i, sn);
        Wr.PutText(wr, sn.name & "\nfirst:   ") ;
        PrintSet(wr, first[i - firstNt].ts, 10);
        Wr.PutText(wr, "follow:  ") ;
        PrintSet(wr, follow[i - firstNt].ts, 10);
        Wr.PutChar(wr, '\n') ;
        INC(i);
      END;

      i := 0;
      Wr.PutText(wr, "\n\nList of sets (ANY SYNC): ") ;
      IF maxSet < 0 THEN Wr.PutText(wr, "        -- none --") ;
      ELSE Wr.PutChar(wr, '\n');
      END;
      WHILE i <= maxSet DO
        Wr.PutText(wr, "     set[" & Fmt.Int(i) & "] = ") ;
        PrintSet(wr, set[i], 16);
        INC(i)
      END;
      Wr.PutChar(wr, '\n');
    END;
  END CompSymbolSets;

(* GetFirstSet          Get precomputed first-set for nonterminal sp
----------------------------------------------------------------------*)
(* UNUSED
PROCEDURE GetFirstSet (sp: INTEGER; VAR s: Set) =
  BEGIN
    s := first[sp - firstNt].ts
  END GetFirstSet;
*)

(* GetFollowSet         Get precomputed follow-set for nonterminal snix
----------------------------------------------------------------------*)
(* UNUSED
PROCEDURE GetFollowSet (sp: INTEGER; VAR s: Set) =
  BEGIN
    s := follow[sp - firstNt].ts
  END GetFollowSet;
*)

(* GetSet               Get set with index nr
----------------------------------------------------------------------*)
PROCEDURE GetSet (nr: INTEGER; VAR s: Set) =
  BEGIN
    s := set[nr]
  END GetSet;

(* PrintSymbolTable     Print symbol table
----------------------------------------------------------------------*)
PROCEDURE PrintSymbolTable(wr : Wr.T) =
  VAR
    i: INTEGER;

  BEGIN (* PrintSymbolTable *)
    Wr.PutText(wr, "SymbolTable:\n\n") ;
    Wr.PutText(wr, "nr    definition                ") ;
    IF ddt['N'] OR symNames THEN
      Wr.PutText(wr, "constant        ")
    END;
    Wr.PutText(wr, "typ    hasAttra struct del line\n\n") ;
    i := 0;
    WHILE i < maxSymbols DO
      Wr.PutText(wr, Fmt.F("%3s   %26s", Fmt.Int(i), st^[i].name)) ;
      IF (*CRT.*) ddt['N'] OR (*CRT.*) symNames THEN
        IF i <= maxT THEN
          Wr.PutText(wr, Fmt.Pad(st^[i].constant, 16))
        ELSE
          Wr.PutText(wr, Fmt.Pad("", 16))
        END;
      END;
      CASE (st^[i].typ) OF
        unknown => Wr.PutText(wr, " unknown")
      | t       => Wr.PutText(wr, " t      ")
      | pr      => Wr.PutText(wr, " pr     ")
      | nt      => Wr.PutText(wr, " nt     ")
      ELSE
        Wr.PutText(wr, " ???????")
      END ;
      Wr.PutText(wr, Fmt.F("%5s %5s %5s %5s\n",
                                Fmt.Bool(st^[i].attrPos.beg >= 0),
                                Fmt.Int(st^[i].struct),
                                Fmt.Bool(st^[i].deletable),
                                Fmt.Int(st^[i].line))) ;
      IF i = maxT THEN i := firstNt ELSE INC(i) END
    END;
    Wr.PutText(wr, "\n\n")
  END PrintSymbolTable;

(* NewClass             Define a new character class
----------------------------------------------------------------------*)
PROCEDURE NewClass (name: TEXT; READONLY set: Set): INTEGER =
  BEGIN
    INC(maxC); IF maxC > maxClasses THEN Restriction(4, maxClasses) END;
    IF Text.GetChar(name, 0) = '#' THEN
      name := "#" & Text.FromChar(VAL(ORD('A') + dummyName,CHAR)); INC(dummyName)
    END;
    chClass[maxC].name := name; chClass[maxC].set := NewSet(set);
    RETURN maxC
  END NewClass;

(* ClassWithName        Return index of class with name n
----------------------------------------------------------------------*)
PROCEDURE ClassWithName (n: TEXT): INTEGER =
  VAR
    i: INTEGER;
  BEGIN
    i := maxC;
    WHILE (i >= 0) AND (NOT Text.Equal(chClass[i].name, n)) DO
      DEC(i)
    END;
    RETURN i
  END ClassWithName;

(* ClassWithSet        Return index of class with the specified set
----------------------------------------------------------------------*)
PROCEDURE ClassWithSet (READONLY s: Set): INTEGER =
  VAR
    i: INTEGER;
  BEGIN
    i := maxC;
    WHILE (i >= 0) AND (set[chClass[i].set] # s) DO DEC(i) END;
    RETURN i
  END ClassWithSet;

(* GetClass             Return character class n
----------------------------------------------------------------------*)
PROCEDURE GetClass (n: INTEGER; VAR s: Set) =
  BEGIN
    GetSet(chClass[n].set, s);
  END GetClass;

(* GetClassName         Get the name of class n
----------------------------------------------------------------------*)
PROCEDURE GetClassName (n: INTEGER; VAR name: TEXT) =
  BEGIN
    name := chClass[n].name
  END GetClassName;

(* XRef                 Produce a cross reference listing of all symbols
----------------------------------------------------------------------*)
PROCEDURE XRef(wr : Wr.T) =
  CONST
    maxLineLen = 80;
  TYPE
    ListPtr  = REF  ListNode;
    ListNode = RECORD
      next: ListPtr;
      line: INTEGER;
    END;
    ListHdr  = RECORD
      name: TEXT;
      lptr: ListPtr;
    END;
  VAR
    gn: GraphNode;
    col, i: INTEGER;
    l, p, q: ListPtr;
    sn: SymbolNode;
    xList: ARRAY [0 .. maxSymbols] OF ListHdr;

  BEGIN (* XRef *)
    IF maxT <= 0 THEN RETURN END;
    MovePragmas();
    (* initialize cross reference list *)
    i := 0;
    WHILE i <= lastNt DO (* for all symbols *)
      GetSym(i, sn); xList[i].name := sn.name; xList[i].lptr := NIL;
      IF i = maxP THEN i := firstNt ELSE INC(i) END
    END;

    (* search lines where symbol has been referenced *)
    i := 1;
    WHILE i <= nNodes DO (* for all graph nodes *)
      GetNode(i, gn);
      IF (gn.typ = t) OR (gn.typ = wt) OR (gn.typ = nt) THEN
        l := NEW(ListPtr) ;
        l^.next := xList[gn.p1].lptr; l^.line := gn.line;
        xList[gn.p1].lptr := l
      END;
      INC(i);
    END;

    (* search lines where symbol has been defined and insert in order *)
    i := 1;
    WHILE i <= lastNt DO (*for all symbols*)
      GetSym(i, sn); p := xList[i].lptr; q := NIL;
      WHILE (p # NIL) AND (p^.line > sn.line) DO q := p; p := p^.next END;
      l := NEW(ListPtr) ;
      l^.next := p;
      l^.line := -sn.line;
      IF q # NIL THEN q^.next := l ELSE xList[i].lptr := l END;
      IF i = maxP THEN i := firstNt ELSE INC(i) END
    END;

    (* print cross reference listing *)
    Wr.PutText(wr, "Cross reference list:\n\n") ;
    Wr.PutText(wr, "Terminals:\n") ;
    Wr.PutText(wr, " 0  EOF\n") ;
    i := 1;
    WHILE i <= lastNt DO (* for all symbols *)
      IF i = maxT THEN
        Wr.PutText(wr, "\nPragmas:\n")
      ELSE
        Wr.PutText(wr, Fmt.F("%3s  %25s", Fmt.Int(i), xList[i].name)) ;
        l := xList[i].lptr; col := 35;
        WHILE l # NIL DO
          IF (col + 5) > maxLineLen THEN
            Wr.PutText(wr, "\n" & Fmt.Pad("", 30)) ;
            col := 35
          END;
          IF l^.line = 0 THEN Wr.PutText(wr, "undef")
          ELSE Wr.PutText(wr, Fmt.Pad(Fmt.Int(l^.line), 5))
          END;
          INC(col, 5);
          l := l^.next
        END;
        Wr.PutChar(wr, '\n') ;
      END;
      IF i = maxP THEN
        Wr.PutText(wr, "\nNonterminals:\n") ;
        i := firstNt
      ELSE INC(i)
      END
    END;
    Wr.PutText(wr, "\n\n")
  END XRef;

(* NewNode              Generate a new graph node and return its index gp
----------------------------------------------------------------------*)
PROCEDURE NewNode (typ, p1, line: INTEGER): INTEGER =
  BEGIN
    INC(nNodes); IF nNodes > maxNodes THEN Restriction(1, maxNodes) END;
    gn^[nNodes].typ     := typ;    gn^[nNodes].next     := 0;
    gn^[nNodes].p1      := p1;     gn^[nNodes].p2       := 0;
    gn^[nNodes].pos.beg := -1; (* Bugfix - PDT *)
    gn^[nNodes].pos.len := 0;      gn^[nNodes].pos.col := 0;
    gn^[nNodes].line    := line;
    RETURN nNodes;
  END NewNode;

(* CompleteGraph        Set right ends of graph gp to 0
----------------------------------------------------------------------*)
PROCEDURE CompleteGraph (gp: INTEGER) =
  VAR
    p: INTEGER;
  BEGIN
    WHILE gp # 0 DO
      p := gn^[gp].next; gn^[gp].next := 0; gp := p
    END
  END CompleteGraph;

(* ConcatAlt            Make (gL2, gR2) an alternative of (gL1, gR1)
----------------------------------------------------------------------*)
PROCEDURE ConcatAlt (VAR gL1, gR1: INTEGER; gL2, gR2: INTEGER) =
  VAR
    p: INTEGER;
  BEGIN
    gL2 := NewNode(alt, gL2, 0); p := gL1;
    WHILE gn^[p].p2 # 0 DO p := gn^[p].p2 END;
    gn^[p].p2 := gL2; p := gR1;
    WHILE gn^[p].next # 0 DO p := gn^[p].next END;
    gn^[p].next := gR2
  END ConcatAlt;

(* ConcatSeq            Make (gL2, gR2) a successor of (gL1, gR1)
----------------------------------------------------------------------*)
PROCEDURE ConcatSeq (<*UNUSED*> VAR gL1 : INTEGER; VAR gR1: INTEGER; gL2, gR2: INTEGER) =
  VAR
    p, q: INTEGER;
  BEGIN
    p := gn^[gR1].next; gn^[gR1].next := gL2; (*head node*)
    WHILE p # 0 DO (*substructure*)
      q := gn^[p].next; gn^[p].next := -gL2; p := q
    END;
    gR1 := gR2
  END ConcatSeq;

(* MakeFirstAlt         Generate alt-node with (gL,gR) as only alternative
----------------------------------------------------------------------*)
PROCEDURE MakeFirstAlt (VAR gL, gR: INTEGER) =
  BEGIN
    gL := NewNode(alt, gL, 0); gn^[gL].next := gR; gR := gL
  END MakeFirstAlt;

(* MakeIteration        Enclose (gL, gR) into iteration node
----------------------------------------------------------------------*)
PROCEDURE MakeIteration (VAR gL, gR: INTEGER) =
  VAR
    p, q: INTEGER;
  BEGIN
    gL := NewNode(iter, gL, 0); p := gR; gR := gL;
    WHILE p # 0 DO
      q := gn^[p].next; gn^[p].next := - gL; p := q
    END
  END MakeIteration;

(* MakeOption           Enclose (gL, gR) into option node
----------------------------------------------------------------------*)
PROCEDURE MakeOption (VAR gL, gR: INTEGER) =
  BEGIN
    gL := NewNode(opt, gL, 0); gn^[gL].next := gR; gR := gL
  END MakeOption;

(* StrToGraph           Generate node chain from characters in s
----------------------------------------------------------------------*)
PROCEDURE StrToGraph (s: TEXT; VAR gL, gR: INTEGER) =
  VAR
    i, len: CARDINAL;
  BEGIN
    gR := 0; i := 1; len := Text.Length(s) - 1; (*strip quotes*)
    WHILE i < len DO
      gn^[gR].next := NewNode(char, ORD(Text.GetChar(s, i)), 0); gR := gn^[gR].next;
      INC(i)
    END;
    gL := gn^[0].next; gn^[0].next := 0
  END StrToGraph;

(* DelGraph             Check if graph starting with index gp is deletable
----------------------------------------------------------------------*)
PROCEDURE DelGraph (gp: INTEGER): BOOLEAN =
  VAR
    gn: GraphNode;
  BEGIN
    IF gp = 0 THEN RETURN TRUE END; (*end of graph found*)
    GetNode(gp, gn);
    RETURN DelNode(gn) AND DelGraph(ABS(gn.next));
  END DelGraph;

(* DelNode              Check if graph node gn is deletable
----------------------------------------------------------------------*)
PROCEDURE DelNode (gn: GraphNode): BOOLEAN =
  VAR
    sn: SymbolNode;

  PROCEDURE DelAlt (gp: INTEGER): BOOLEAN =
    VAR
      gn: GraphNode;
    BEGIN
      IF gp <= 0 THEN RETURN TRUE END; (*end of graph found*)
      GetNode(gp, gn);
      RETURN DelNode(gn) AND DelAlt(gn.next);
    END DelAlt;

  BEGIN
    IF gn.typ = nt THEN GetSym(gn.p1, sn); RETURN sn.deletable
    ELSIF gn.typ = alt THEN
      RETURN DelAlt(gn.p1) OR ((gn.p2 # 0) AND DelAlt(gn.p2))
    ELSE RETURN (gn.typ = eps) OR (gn.typ = iter)
                OR (gn.typ = opt) OR (gn.typ = sem) OR (gn.typ = sync)
    END
  END DelNode;

(* PrintGraph           Print the graph
----------------------------------------------------------------------*)
PROCEDURE PrintGraph(wr : Wr.T) =
  VAR
    i: INTEGER;

  BEGIN (* PrintGraph *)
    Wr.PutText(wr, "GraphList:\n\n") ;
    Wr.PutText(wr, " nr   typ    next     p1     p2   line") ;
    Wr.PutText(wr, " posbeg poslen poscol\n\n") ;
    i := 0;
    WHILE i <= nNodes DO
      Wr.PutText(wr, Fmt.F("%3s  ", Fmt.Int(i))) ;
      CASE (gn^[i].typ) OF
        nt   => Wr.PutText(wr, "nt  ")
      | t    => Wr.PutText(wr, "t   ")
      | wt   => Wr.PutText(wr, "wt  ")
      | any  => Wr.PutText(wr, "any ")
      | eps  => Wr.PutText(wr, "eps ")
      | sem  => Wr.PutText(wr, "sem ")
      | sync => Wr.PutText(wr, "sync")
      | alt  => Wr.PutText(wr, "alt ")
      | iter => Wr.PutText(wr, "iter")
      | opt  => Wr.PutText(wr, "opt ")
      ELSE
        Wr.PutText(wr, "--- ")
      END ;
      Wr.PutText(wr, Fmt.F("%7s%7s%7s%7s",
                                Fmt.Int(gn^[i].next),
                                Fmt.Int(gn^[i].p1),
                                Fmt.Int(gn^[i].p2),
                                Fmt.Int(gn^[i].line))) ;
      Wr.PutText(wr, Fmt.F("%7s%7s%7s\n",
                                Fmt.Int(gn^[i].pos.beg),
                                Fmt.Int(gn^[i].pos.len),
                                Fmt.Int(gn^[i].pos.col))) ;
      INC(i);
    END;
    Wr.PutText(wr, "\n\n") ;
  END PrintGraph;

(* FindCircularProductions      Test grammar for circular derivations
----------------------------------------------------------------------*)
PROCEDURE FindCircularProductions(wr : Wr.T ; VAR ok: BOOLEAN) =
  CONST
    maxList = 150;
  TYPE
    ListEntry = RECORD
      left   : INTEGER;
      right  : INTEGER;
      deleted: BOOLEAN;
    END;
  VAR
    changed, onLeftSide,
    onRightSide:      BOOLEAN;
    i, j, listLength: INTEGER;
    list:             ARRAY [0 .. maxList] OF ListEntry;
    singles:          MarkList;
    sn:               SymbolNode;

  PROCEDURE GetSingles (gp: INTEGER; VAR singles: MarkList) =
    VAR
      gn: GraphNode;
    BEGIN
      IF gp <= 0 THEN RETURN END; (* end of graph found *)
      GetNode (gp, gn);
      IF gn.typ = nt THEN
        IF DelGraph(ABS(gn.next)) THEN singles := singles + MarkList{gn.p1} END
      ELSIF (gn.typ = alt) OR (gn.typ = iter) OR (gn.typ = opt) THEN
        IF DelGraph(ABS(gn.next)) THEN
          GetSingles(gn.p1, singles);
          IF gn.typ = alt THEN GetSingles(gn.p2, singles) END
        END
      END;
      IF DelNode(gn) THEN GetSingles(gn.next, singles) END
    END GetSingles;

  BEGIN (* FindCircularProductions *)
    i := firstNt; listLength := 0;
    WHILE i <= lastNt DO (* for all nonterminals i *)
      ClearMarkList(singles); GetSym(i, sn);
      GetSingles(sn.struct, singles); (* get nt's j such that i-->j *)
      j := firstNt;
      WHILE j <= lastNt DO (* for all nonterminals j *)
        IF (j IN singles) THEN
          list[listLength].left := i; list[listLength].right := j;
          list[listLength].deleted := FALSE;
          INC(listLength)
        END;
        INC(j)
      END;
      INC(i)
    END;

    REPEAT
      i := 0; changed := FALSE;
      WHILE i < listLength DO
        IF NOT  list[i].deleted THEN
          j := 0; onLeftSide := FALSE; onRightSide := FALSE;
          WHILE j < listLength DO
            IF NOT  list[j].deleted THEN
              IF list[i].left = list[j].right THEN onRightSide := TRUE END;
              IF list[j].left = list[i].right THEN onLeftSide := TRUE END
            END;
            INC(j)
          END;
          IF NOT ( onRightSide) OR NOT ( onLeftSide) THEN
            list[i].deleted := TRUE; changed := TRUE
          END
        END;
        INC(i)
      END
    UNTIL NOT  changed;

    Wr.PutText(wr, "Circular derivations:   ") ;
    i := 0; ok := TRUE;
    WHILE i < listLength DO
      IF NOT  list[i].deleted THEN
        ok := FALSE;
        GetSym(list[i].left, sn);
        Wr.PutText(wr, Fmt.F("\n     %20s --> ", sn.name)) ;
        GetSym(list[i].right, sn);
        Wr.PutText(wr, Fmt.F("%20s", sn.name))
      END;
      INC(i)
    END;
    IF ok THEN Wr.PutText(wr, " -- none --") END ;
    Wr.PutChar(wr, '\n')
  END FindCircularProductions;

(* LL1Test              Collect terminal sets and checks LL(1) conditions
----------------------------------------------------------------------*)
PROCEDURE LL1Test (wr : Wr.T ; VAR ll1: BOOLEAN) =
  VAR
    sn:    SymbolNode;
    curSy: INTEGER;

  PROCEDURE LL1Error (cond, ts: INTEGER) =
    VAR
      sn: SymbolNode;
    BEGIN
      ll1 := FALSE;
      GetSym(curSy, sn);
      Wr.PutText(wr, "\n LL(1) error in " & sn.name & ": ") ;
      IF ts > 0 THEN
        GetSym(ts, sn);
        Wr.PutText(wr, sn.name & " is the ")
      END;
      CASE cond OF
        1 => Wr.PutText(wr, "start of several alternatives.")
      | 2 => Wr.PutText(wr, "start & successor of deletable structure")
      | 3 => Wr.PutText(wr, "an ANY node matches no symbol")
      ELSE
        Wr.PutText(wr, "?????")
      END;
    END LL1Error;

  PROCEDURE Check (cond: INTEGER; VAR s1, s2: Set) =
    VAR
      i: INTEGER;
    BEGIN
      i := 0;
      WHILE i <= maxT DO
        IF ((i IN s1) AND (i IN s2)) THEN LL1Error(cond, i) END;
        INC(i)
      END
    END Check;

  PROCEDURE CheckAlternatives (gp: INTEGER) =
    VAR
      gn, gn1: GraphNode;
      s1, s2:  Set;
      p:       INTEGER;
    BEGIN
      WHILE gp > 0 DO
        GetNode(gp, gn);
        IF gn.typ = alt THEN
          p := gp; s1 := Set{};
          WHILE p # 0 DO (*for all alternatives*)
            GetNode(p, gn1); CompExpected(gn1.p1, curSy, s2);
            Check(1, s1, s2);
            s1 := s1 + s2 ;
            CheckAlternatives(gn1.p1);
            p := gn1.p2
          END
        ELSIF (gn.typ = opt) OR (gn.typ = iter) THEN
          CompExpected(gn.p1, curSy, s1);
          CompExpected(ABS(gn.next), curSy, s2);
          Check(2, s1, s2);
          CheckAlternatives(gn.p1)
        ELSIF gn.typ = any THEN
          GetSet(gn.p1, s1);
          IF (s1 = Set{}) THEN LL1Error(3, 0) END
          (*e.g. {ANY} ANY or [ANY] ANY*)
        END;
        gp := gn.next
      END
    END CheckAlternatives;

  BEGIN (* LL1Test *)
    Wr.PutText(wr, "LL(1) conditions:") ;
    curSy := firstNt; ll1 := TRUE;
    WHILE curSy <= lastNt DO (*for all nonterminals*)
      GetSym(curSy, sn); CheckAlternatives(sn.struct);
      INC(curSy)
    END;
    IF ll1 THEN Wr.PutText(wr, "        -- ok --") END ;
    Wr.PutChar(wr, '\n')
  END LL1Test;

(* TestCompleteness     Test if all nonterminals have productions
----------------------------------------------------------------------*)
PROCEDURE TestCompleteness(wr : Wr.T ; VAR ok: BOOLEAN) =
  VAR
    sp: INTEGER;
    sn: SymbolNode;
  BEGIN
    Wr.PutText(wr, "Undefined nonterminals:  ") ;
    sp := firstNt; ok := TRUE;
    WHILE sp <= lastNt DO (*for all nonterminals*)
      GetSym(sp, sn);
      IF sn.struct = 0 THEN
        ok := FALSE;
        Wr.PutText(wr, "\n     " & sn.name)
      END;
      INC(sp)
    END;
    IF ok THEN Wr.PutText(wr, " -- none --") END ;
    Wr.PutChar(wr, '\n')
  END TestCompleteness;

(* TestIfAllNtReached   Test if all nonterminals can be reached
----------------------------------------------------------------------*)
PROCEDURE TestIfAllNtReached (wr : Wr.T ; VAR ok: BOOLEAN) =
  VAR
    gn:      GraphNode;
    sp:      INTEGER;
    reached: MarkList;
    sn:      SymbolNode;

  PROCEDURE MarkReachedNts (gp: INTEGER) =
    VAR
      gn: GraphNode;
      sn: SymbolNode;
    BEGIN
      WHILE gp > 0 DO
        GetNode(gp, gn);
        IF gn.typ = nt THEN
          IF NOT  (gn.p1 IN reached) THEN (*new nt reached*)
            reached := reached + MarkList{gn.p1} ;
            GetSym(gn.p1, sn); MarkReachedNts(sn.struct)
          END
        ELSIF (gn.typ = alt) OR (gn.typ = iter) OR (gn.typ = opt) THEN
          MarkReachedNts(gn.p1);
          IF gn.typ = alt THEN MarkReachedNts(gn.p2) END
        END;
        gp := gn.next
      END
    END MarkReachedNts;

  BEGIN (* TestIfAllNtReached *)
    ClearMarkList(reached);
    GetNode(root, gn);
    reached := reached + MarkList{gn.p1} ;
    GetSym(gn.p1, sn); MarkReachedNts(sn.struct);

    Wr.PutText(wr, "Unreachable nonterminals:") ;
    sp := firstNt; ok := TRUE;
    WHILE sp <= lastNt DO (*for all nonterminals*)
      IF NOT  (sp IN reached) THEN
        ok := FALSE; GetSym(sp, sn);
        Wr.PutText(wr, "\n     " & sn.name)
      END;
      INC(sp)
    END;
    IF ok THEN Wr.PutText(wr, " -- none --") END ;
    Wr.PutChar(wr, '\n')
  END TestIfAllNtReached;

(* TestIfNtToTerm   Test if all nonterminals can be derived to terminals
----------------------------------------------------------------------*)
PROCEDURE TestIfNtToTerm(wr : Wr.T ; VAR ok: BOOLEAN) =
  VAR
    changed:  BOOLEAN;
    sp:       INTEGER;
    sn:       SymbolNode;
    termList: MarkList;

  PROCEDURE IsTerm (gp: INTEGER): BOOLEAN =
    VAR
      gn: GraphNode;
    BEGIN
      WHILE gp > 0 DO
        GetNode(gp, gn);
        IF ((gn.typ = nt) AND NOT (gn.p1 IN termList))
           OR ((gn.typ = alt) AND NOT ( IsTerm(gn.p1)) AND NOT ( IsTerm(gn.p2))) THEN
             RETURN FALSE
        END;
        gp := gn.next
      END;
      RETURN TRUE
    END IsTerm;

  BEGIN (* TestIfNtToTerm *)
    ClearMarkList(termList);
    REPEAT
      sp := firstNt; changed := FALSE;
      WHILE sp <= lastNt DO
        IF NOT  (sp IN termList) THEN
          GetSym(sp, sn);
          IF IsTerm(sn.struct) THEN
            termList := termList + MarkList{sp}; changed := TRUE
          END
        END;
        INC(sp)
      END
    UNTIL NOT  changed;

    Wr.PutText(wr, "Underivable nonterminals:") ;
    sp := firstNt; ok := TRUE;
    WHILE sp <= lastNt DO
      IF NOT  (sp IN termList) THEN
        ok := FALSE; GetSym(sp, sn);
        Wr.PutText(wr, "\n     " & sn.name)
      END;
      INC(sp)
    END;
    IF ok THEN Wr.PutText(wr, " -- none --") END ;
    Wr.PutChar(wr, '\n')
  END TestIfNtToTerm;

(* ASCIIName            Assigns the wellknown ASCII-Name in lowercase
----------------------------------------------------------------------*)

CONST
  AsciiCtrlNames = ARRAY ['\000'..'\037'] OF TEXT {
    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
    "BS",  "HT",  "NL",  "VT",  "NP",  "CR",  "SO",  "SI",
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US"
  } ;

  AsciiNumbers = ARRAY ['0' .. '9'] OF TEXT {
    "Zero", "One", "Two",   "Three", "Four",
    "Five", "Six", "Seven", "Eight", "Nine"
  } ;

  AsciiPunct1 = ARRAY [' ' .. '/'] OF TEXT {
    "Space", "Excl", "Dquo", "Hash", "Doll", "Pct", "Amp",
    "Squo", "Lbr", "Rbr", "Star", "Plus", "Comm", "Dash",
    "Dot", "Slash"
  } ;

  AsciiPunct2 = ARRAY[':' .. '@'] OF TEXT {
    "Colon", "Semic", "Less", "Equal", "Gtr", "Ques", "At"
  } ;

  AsciiPunct3 = ARRAY['[' .. '`'] OF TEXT {
    "Lsqu", "Bslash", "Rsqu", "Caret", "Under", "Bquo"
  } ;

  AsciiPunct4 = ARRAY['{' .. '~'] OF TEXT {
    "Lcurl", "Bar", "Rcurl", "Tilde"
  } ;

PROCEDURE ASCIIName (ascii: CHAR) : TEXT =
BEGIN
  CASE ascii OF
    '\000' .. '\037'       => RETURN AsciiCtrlNames[ascii]
  | '0' .. '9'             => RETURN AsciiNumbers[ascii]
  | 'A' .. 'Z', 'a' .. 'z' => RETURN Text.FromChar(ascii)
  | ' ' .. '/'             => RETURN AsciiPunct1[ascii]
  | ':' .. '@'             => RETURN AsciiPunct2[ascii]
  | '[' .. '`'             => RETURN AsciiPunct3[ascii]
  | '{' .. '~'             => RETURN AsciiPunct4[ascii]
  | '\177'                 => RETURN "del"
  ELSE
    RETURN "c" & Fmt.Int(ORD(ascii))
  END
END ASCIIName ;

(* BuildName            Build new Name to represent old string
----------------------------------------------------------------------*)
PROCEDURE BuildName(old : TEXT) : TEXT =
VAR new := "" ;
    ch  : CHAR ;
BEGIN
  FOR i := 1 TO Text.Length(old) - 2 DO
    ch := Text.GetChar(old, i) ;
    IF (i = 1) THEN
      new := "s" & ASCIIName(ch)
    ELSIF (ch IN SET OF CHAR { 'A' .. 'Z', 'a' .. 'z', '0' .. '9', '_' }) THEN
      new := new & Text.FromChar(ch)
    ELSE
      new := new & ASCIIName(ch)
    END
  END ;
  RETURN new
END BuildName ;

(* SymName              Generates a new name for a symbol constant
----------------------------------------------------------------------*)
PROCEDURE SymName(symn: TEXT) : TEXT =
VAR ch := Text.GetChar(symn, 0) ;
BEGIN
  IF (ch = '"') OR (ch = '\'') THEN
    IF Text.Length(symn) = 3 THEN
      RETURN ASCIIName(Text.GetChar(symn, 1));
    ELSE
      RETURN BuildName(symn) ;
    END;
  ELSE
    RETURN symn
  END;
END SymName ;


(* AssignSymNames     Assigns the user defined or generated token names
----------------------------------------------------------------------*)
PROCEDURE AssignSymNames (default: BOOLEAN; VAR thereExists: BOOLEAN) =

  PROCEDURE AssignDef(n : TEXT) : TEXT =
  BEGIN
    FOR i := 1 TO lastName DO
      IF Text.Equal(n, tt[i].definition) THEN
        thereExists := TRUE ;
        RETURN tt[i].name
      END
    END ;
    RETURN SymName(n)
  END AssignDef ;

BEGIN
  thereExists := default;
  st[0].constant := "Symbol.Eof" ;
  FOR i := 1 TO maxP DO
    st[i].constant := "Symbol." & AssignDef(st[i].name)
  END ;
  st[maxT].name     := "*UNDEF*" ;
  st[maxT].constant := "Symbol.Undef"
END AssignSymNames ;

BEGIN (* CRT *)
  ch := 'A'; WHILE ch <= 'Z' DO ddt[ch] := FALSE; INC(ch) END;
  maxSet := 0; set[0] := Set{eofSy};
  firstNt := maxSymbols; maxP := maxSymbols; maxT := -1; maxC := -1;
  lastNt := maxP - 1;
  dummyName := 0; lastName := 0; symNames := FALSE;
  (* The dummy node gn^[0] ensures that none of the procedures
     above have to check for 0 indices. *)
  gn := NEW(REF GraphList) ;
  st := NEW(REF SymbolTable) ;
  nNodes := 0;
  gn^[0].typ := -1; gn^[0].p1 := 0; gn^[0].p2 := 0;
  gn^[0].next := 0; gn^[0].line := 0;
  gn^[0].pos.beg := -1 ;
  gn^[0].pos.len := 0; gn^[0].pos.col := 0;
(* debug info when choosing constants - PDT
  Wr.PutText(Stdio.stdout, "Symbol table " &
                           Fmt.Int(BYTESIZE(SymbolTable)) & "\n") ;
  Wr.PutText(Stdio.stdout, "Class table " &
                           Fmt.Int(BYTESIZE(ClassTable)) & "\n") ;
  Wr.PutText(Stdio.stdout, "Name table " &
                           Fmt.Int(BYTESIZE(NameTable)) & "\n") ;
  Wr.PutText(Stdio.stdout, "Graph list " &
                           Fmt.Int(BYTESIZE(GraphList)) & "\n") ;
*)
END CRT.
