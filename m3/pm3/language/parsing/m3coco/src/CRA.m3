MODULE CRA ;

(* CRA     Automaton and Scanner Generation
   ===     ================================

  (1) ConvertToStates translates a top-down graph into a NFA.
      MatchDFA tries to match literal strings against the DFA
  (2) MakeDeterministic converts the NFA into a DFA
  (3) WriteScanner generates the scanner source file

  ----------------------------------------------------------------*)

IMPORT CR, CRT, Frame ;
IMPORT ASCII, Wr, Text, TextWr, Fmt, Stdio, Process ;

<* FATAL ANY *>

CONST
  maxStates = 500;
  cr = '\015';

TYPE
  Action     = REF  ActionNode;
  Target     = REF  TargetNode;

  State = RECORD          (* state of finite automaton *)
    firstAction: Action;  (* to first action of this state *)
    endOf:       INTEGER; (* nr. of recognized token if state is final *)
    ctx:         BOOLEAN; (* TRUE: state reached by contextTrans *)
  END;
  ActionNode = RECORD     (* action of finite automaton *)
    typ:    INTEGER;      (* type of action symbol: char, class *)
    sym:    INTEGER;      (* action symbol *)
    tc:     INTEGER;      (* transition code: normTrans, contextTrans *)
    target: Target;       (* states after transition with input symbol *)
    next:   Action;
  END;
  TargetNode = RECORD     (* state after transition with input symbol *)
    state:  INTEGER;      (* target state *)
    next:   Target;
  END;

  Comment    = REF  CommentNode;
  CommentNode = RECORD    (* info about a comment syntax *)
    start,stop: ARRAY [0 .. 1] OF CHAR;
    nested:     BOOLEAN;
    next:       Comment;
  END;

  Melted     = REF  MeltedNode;
  MeltedNode = RECORD     (* info about melted states *)
    set:   CRT.Set;       (* set of old states *)
    state: INTEGER;       (* new state *)
    next:  Melted;
  END;

VAR
  state:         ARRAY [0 .. maxStates] OF State;
  lastSimState:  INTEGER;     (* last non melted state *)
  lastState:     INTEGER;     (* last allocated state  *)
  rootState:     INTEGER;     (* start state of DFA    *)
  firstMelted:   Melted;      (* list of melted states *)
  firstComment:  Comment;     (* list of comments      *)
  NewLine:       BOOLEAN;

PROCEDURE CharConst(ch: CHAR) : TEXT =
BEGIN
(* normal characters first *)

  IF ((ch IN ASCII.Graphics) AND (ch # '\\') AND (ch # '\'')) THEN
    RETURN Fmt.F("'%s'", Fmt.Char(ch))
  END ;

(* now chars that have special escapes *)

  CASE ch OF
    '\n' => RETURN "'\\n'"
  | '\t' => RETURN "'\\t'"
  | '\r' => RETURN "'\\r'"
  | '\f' => RETURN "'\\f'"
  | '\\' => RETURN "'\\\\'"
  | '\'' => RETURN "'\\''"
  ELSE
  END ;

(* final resort - use octal escape sequence *)

  RETURN Fmt.F("'\\%s'", Fmt.Pad(Fmt.Int(ORD(ch), 8), 3, '0'))
END CharConst ;

PROCEDURE SymbolName(i : INTEGER) : TEXT =
VAR sn : CRT.SymbolNode ;
BEGIN
  CRT.GetSym(i, sn) ;
  IF (Text.Length(sn.constant) > 0) THEN
    RETURN sn.constant
  ELSE
    RETURN Fmt.Int(i)
  END
END SymbolName ;

PROCEDURE PutRange(wr : Wr.T ; READONLY s: CRT.Set ; indent : CARDINAL) =
VAR lo, hi: ARRAY [0 .. 31] OF CHAR ;
    top, i: INTEGER;
    s1:     CRT.Set;
BEGIN
(* fill lo and hi *)
  top := -1 ;
  i := 0 ;
  WHILE (i < 256) DO
    IF (i IN s) THEN
      INC(top) ;
      lo[top] := VAL(i, CHAR) ;
      INC(i) ;
      WHILE ((i < 256) AND (i IN s)) DO
        INC(i)
      END ;
      hi[top] := VAL(i - 1, CHAR)
    ELSE
      INC(i)
    END
  END ;

(* print ranges *)
  IF ((top = 1) AND (lo[0] = '\000') AND (hi[1] = '\377')
      AND (VAL(ORD(hi[0]) + 2, CHAR) = lo[1])) THEN
    s1 := CRT.Set{0..CRT.maxTerminals} ;
    s1 := s1 - s ;
    Wr.PutText(wr, "(NOT ") ;
    PutRange(wr, s1, indent) ;
    Wr.PutChar(wr, ')')
  ELSE
    i := 0 ;
    WHILE (i <= top) DO
      IF (hi[i] = lo[i]) THEN
        Wr.PutText(wr, "(s.ch = " & CharConst(lo[i]))
      ELSIF (lo[i] = '\000') THEN
        Wr.PutText(wr, "(s.ch <= " & CharConst(hi[i]))
      ELSIF (hi[i] = '\377') THEN
        Wr.PutText(wr, "(s.ch >= " & CharConst(lo[i]))
      ELSE
        Wr.PutText(wr, "((s.ch >= " & CharConst(lo[i])) ;
        Wr.PutText(wr, ") AND (s.ch <= " & CharConst(hi[i])) ;
        Wr.PutChar(wr, ')')
      END ;
      Wr.PutChar(wr, ')') ;
      IF (i < top) THEN
        Wr.PutText(wr, " OR\n") ;
        Wr.PutText(wr, Fmt.Pad("", indent))
      END ;
      INC(i)
    END
  END
END PutRange ;

(* PrintSymbol          Print a symbol for tracing
-------------------------------------------------------------------------*)
PROCEDURE PrintSymbol(wr : Wr.T ; typ, val, width: INTEGER) =
VAR name : TEXT ;
BEGIN
  IF (typ = CRT.class) THEN
    CRT.GetClassName(val, name) ;
  ELSE
    name := CharConst(VAL(val, CHAR))
  END ;
  Wr.PutText(wr, Fmt.Pad(name, width, align := Fmt.Align.Left))
END PrintSymbol ;

PROCEDURE PrintSet(wr : Wr.T ; READONLY s: CRT.Set ; w, indent : INTEGER) =
VAR i   := 0 ;
    col := indent ;
BEGIN
  Wr.PutChar(wr, '{') ;
  WHILE i < CRT.maxTerminals DO
    IF (i IN s) THEN
      IF (col + 4) > w THEN
        Wr.PutText(wr, "\n" & Fmt.Pad("", indent)) ;
        col := indent
      END;
      Wr.PutText(wr, Fmt.F("%3s,", Fmt.Int(i))) ;
      INC(col, 4)
    END ;
    INC(i)
  END ;
  Wr.PutChar(wr, '}')
END PrintSet ;

(* PrintStates          List the automaton for tracing
-------------------------------------------------------------------------*)
PROCEDURE PrintStates(wr : Wr.T) =
VAR action : Action ;
    first  : BOOLEAN ;
    s, i   : INTEGER ;
    targ   : Target ;
    set    : CRT.Set ;
    name   : TEXT ;
    sn     : CRT.SymbolNode ;
BEGIN
  Wr.PutText(wr, "\n-------- states --------\n") ;
  s := rootState ;
  WHILE (s <= lastState) DO
    action := state[s].firstAction ;
    first := TRUE ;
    Wr.PutText(wr, Fmt.F("%3s: ", Fmt.Int(s))) ;
    WHILE (action # NIL) DO
      IF (NOT first) THEN
        Wr.PutText(wr, "     ")
      END ;
      first := FALSE ; 
      PrintSymbol(wr, action.typ, action.sym, 0) ;
      Wr.PutChar(wr, ' ') ;
      targ := action.target ;
      WHILE (targ # NIL) DO
        Wr.PutText(wr, Fmt.Int(targ.state)) ;
        Wr.PutChar(wr, ' ') ;
        targ := targ.next
      END ;
      IF (action.tc = CRT.contextTrans) THEN
        Wr.PutText(wr, " context")
      END ;
      Wr.PutChar(wr, '\n') ;
      action := action.next
    END ;
    IF (state[s].endOf # CRT.noSym) THEN
      IF (NOT first) THEN
        Wr.PutText(wr, "    ")
      END ;
      first := FALSE ;
      Wr.PutText(wr, "E(" & Fmt.Int(state[s].endOf) & ")") ;
      CRT.GetSym(state[s].endOf, sn) ;
      IF (sn.name # NIL) THEN
        Wr.PutText(wr, " [" & sn.name & "]")
      END ;
      Wr.PutChar(wr, '\n')
    END ;
    INC(s)
  END;
  Wr.PutText(wr, "\n-------- character classes --------\n") ;
  i := 0 ;
  WHILE (i <= CRT.maxC) DO
    CRT.GetClass(i, set) ;
    CRT.GetClassName(i, name) ;
    Wr.PutText(wr, Fmt.Pad(name, 10)) ;
    Wr.PutText(wr, ": ") ;
    PrintSet(wr, set, 80, 13) ;
    Wr.PutChar(wr, '\n') ;
    INC(i)
  END
END PrintStates ;

(* AddAction            Add a action to the action list of a state
------------------------------------------------------------------------*)
PROCEDURE AddAction (act: Action; VAR head: Action) =
  VAR
    a,lasta: Action;
  BEGIN
    a := head; lasta := NIL;
    LOOP
      IF (a = NIL) OR (act^.typ < a^.typ) THEN
        (*collecting classes at the front improves performance*)
        act^.next := a;
        IF lasta = NIL THEN head := act ELSE lasta^.next := act END;
        EXIT;
      END;
      lasta := a; a := a^.next;
    END;
  END AddAction;

(* DetachAction         Detach action a from list L
------------------------------------------------------------------------*)
PROCEDURE DetachAction (a: Action; VAR L: Action) =
  BEGIN
    IF L = a THEN L := a^.next
      ELSIF L # NIL THEN DetachAction(a, L^.next)
    END
  END DetachAction;

PROCEDURE TheAction (state: State; ch: CHAR): Action =
  VAR
    a: Action;
    set: CRT.Set;
  BEGIN
    a := state.firstAction;
    WHILE a # NIL DO
      IF a^.typ = CRT.char THEN
        IF VAL( ORD(ch),INTEGER) = a^.sym THEN RETURN a END
      ELSIF a^.typ = CRT.class THEN
        CRT.GetClass(a^.sym, set);
        IF (ORD(ch) IN set) THEN RETURN a END
      END;
      a := a^.next
    END;
    RETURN NIL
  END TheAction;

PROCEDURE AddTargetList (VAR lista, listb: Target) =
  VAR
    p,t: Target;

  PROCEDURE AddTarget (t: Target; VAR list: Target) =
    VAR
      p,lastp: Target;
    BEGIN
      p := list; lastp := NIL;
      LOOP
        IF (p = NIL) OR (t^.state < p^.state) THEN EXIT END;
        IF p^.state = t^.state THEN
          (* free(t) *)
          RETURN
        END;
        lastp := p; p := p^.next
      END;
      t^.next := p;
      IF lastp=NIL THEN list := t ELSE lastp^.next := t END
    END AddTarget;

  BEGIN
    p := lista;
    WHILE p # NIL DO
      t := NEW(Target) ;
      t^.state := p^.state; AddTarget(t, listb);
      p := p^.next
    END
  END AddTargetList;

(* NewMelted            Generate new info about a melted state
------------------------------------------------------------------------*)
PROCEDURE NewMelted (READONLY set: CRT.Set; s: INTEGER): Melted =
  VAR
    melt: Melted;
  BEGIN
    melt := NEW(Melted) ;
    melt^.set := set; melt^.state := s;
    melt^.next := firstMelted; firstMelted := melt;
    RETURN melt
  END NewMelted;

(* NewState             Return a new state node
------------------------------------------------------------------------*)
PROCEDURE NewState (): INTEGER =
  BEGIN
    INC(lastState);
    IF lastState > maxStates THEN CRT.Restriction(7, maxStates) END;
    state[lastState].firstAction := NIL;
    state[lastState].endOf := CRT.noSym;
    state[lastState].ctx := FALSE;
    RETURN lastState
  END NewState;

(* NewTransition        Generate transition (gn.state, gn.p1) --> toState
------------------------------------------------------------------------*)
PROCEDURE NewTransition (p : CR.Parser ; from: INTEGER; gn: CRT.GraphNode;
                         toState: INTEGER) =
  VAR
    a: Action;
    t: Target;
  BEGIN
    IF toState = rootState THEN
      p.error("token must not start with an iteration")
    END;
    t := NEW(Target) ;
    t^.state := toState; t^.next := NIL;
    a := NEW(Action) ;
    a^.typ := gn.typ; a^.sym := gn.p1; a^.tc := gn.p2; a^.target := t;
    AddAction(a, state[from].firstAction)
  END NewTransition;

(* NewComment           Define new comment
-------------------------------------------------------------------------*)
PROCEDURE NewComment (p : CR.Parser ; from, to: INTEGER; nested: BOOLEAN) =
  VAR
    com: Comment;

  PROCEDURE MakeStr (gp: INTEGER; VAR s: ARRAY OF CHAR) =
    VAR
      i, n: INTEGER;
      gn:   CRT.GraphNode;
      set:  CRT.Set;
    BEGIN
      i := 0;
      WHILE gp # 0 DO
        CRT.GetNode(gp, gn);
        IF gn.typ = CRT.char THEN
          IF i < 2 THEN s[i] := VAL(gn.p1,CHAR) END; INC(i)
        ELSIF gn.typ = CRT.class THEN
          CRT.GetClass(gn.p1, set);
          n := -1 ;
          FOR e := 0 TO CRT.maxTerminals DO
            IF (e IN set) THEN
              IF (n = -1) THEN n := e ELSE n := -2 END
            END
          END ;
          IF (n < 0) THEN
            p.error("character set contains more than one character") ;
            n := 0
          END ;
          IF i < 2 THEN s[i] := VAL(n,CHAR) END; INC(i)
        ELSE
          p.error("only characters allowed in comment declaration")
        END;
        gp := gn.next
      END;
      IF i > 2 THEN
        p.error("comment delimiter must not exceed 2 characters")
      ELSIF i < 2 THEN s[i] := '\000' END
    END MakeStr;

  BEGIN
    com := NEW(Comment) ;
    MakeStr(from, com^.start); MakeStr(to, com^.stop);
    com^.nested := nested;
    com^.next := firstComment; firstComment := com
  END NewComment;

(* DeleteTargetList     Delete a target list
-------------------------------------------------------------------------*)
PROCEDURE DeleteTargetList (list: Target) =
  BEGIN
    IF list # NIL THEN
      DeleteTargetList(list^.next);
      (* free(list) *)
    END;
  END DeleteTargetList;

(* DeleteActionList     Delete an action list
-------------------------------------------------------------------------*)
PROCEDURE DeleteActionList (action: Action) =
  BEGIN
    IF action # NIL THEN
      DeleteActionList(action^.next);
      DeleteTargetList(action^.target);
      (* free(action) *)
    END
  END DeleteActionList;

(* MakeSet              Expand action symbol into symbol set
-------------------------------------------------------------------------*)
PROCEDURE MakeSet (p: Action; VAR set: CRT.Set) =
  BEGIN
    IF p^.typ = CRT.class THEN
    CRT.GetClass(p^.sym, set)
    ELSE set := CRT.Set{p^.sym}
    END
  END MakeSet;

(* ChangeAction         Change the action symbol to set
-------------------------------------------------------------------------*)
PROCEDURE ChangeAction (a: Action; READONLY set: CRT.Set) =
  VAR
    nr: INTEGER;
    count : CARDINAL := 0 ;
  BEGIN
    FOR e := 0 TO CRT.maxTerminals DO
      IF (e IN set) THEN
        nr := e ;
        INC(count)
      END
    END ;
    IF count = 1 THEN a^.typ := CRT.char; a^.sym := nr
    ELSE
      nr := CRT.ClassWithSet(set);
      IF nr < 0 THEN nr := CRT.NewClass("##", set) END;
      a^.typ := CRT.class; a^.sym := nr
    END
  END ChangeAction;

(* CombineShifts     Combine shifts with different symbols into same state
-------------------------------------------------------------------------*)
PROCEDURE CombineShifts() =
  VAR
    s: INTEGER;
    a, b, c: Action;
    seta, setb: CRT.Set;
  BEGIN
    s := rootState;
    WHILE s <= lastState DO
      a := state[s].firstAction;
      WHILE a # NIL DO
        b := a^.next;
        WHILE b # NIL DO
          IF (a^.target^.state = b^.target^.state) AND (a^.tc = b^.tc) THEN
            MakeSet(a, seta); MakeSet(b, setb); seta := seta + setb;
            ChangeAction(a, seta);
            c := b; b := b^.next; DetachAction(c, a)
          ELSE b := b^.next
          END
        END;
        a := a^.next
      END;
      INC(s)
    END
  END CombineShifts;

(* DeleteRedundantStates   Delete unused and equal states
-------------------------------------------------------------------------*)

TYPE StateSet = SET OF [0 .. maxStates] ;

PROCEDURE DeleteRedundantStates() =
  VAR
    action: Action;
    s, s2, next: [0 .. maxStates];
    used:        StateSet ;
    newStateNr:  ARRAY [0 .. maxStates] OF [0 .. maxStates];

  PROCEDURE FindUsedStates (s: INTEGER) =
    VAR
      action: Action;
    BEGIN
      IF (s IN used) THEN RETURN END;
      used := used + StateSet{s} ;
      action := state[s].firstAction;
      WHILE action # NIL DO
        FindUsedStates(action^.target^.state);
        action := action^.next
      END
    END FindUsedStates;

  BEGIN
    used := StateSet{} ;
    FindUsedStates(rootState);
    (*---------- combine equal final states ------------*)
    s := rootState + 1; (*root state cannot be final*)
    WHILE s <= lastState DO
      IF (s IN used) AND (state[s].endOf # CRT.noSym) THEN
        IF (state[s].firstAction = NIL) AND NOT ( state[s].ctx) THEN
          s2 := s + 1;
          WHILE s2 <= lastState DO
            IF (s2 IN used) AND (state[s].endOf = state[s2].endOf) THEN
              IF (state[s2].firstAction = NIL) AND NOT ( state[s2].ctx) THEN
                used := used - StateSet{s2} ;
                newStateNr[s2] := s
              END
            END;
            INC(s2)
          END
        END
      END;
      INC(s)
    END;
    s := rootState;
    (* + 1 ?  PDT - was rootState, but Oberon had .next ie +1
                    seems to work both ways?? *)
    WHILE s <= lastState DO
      IF (s IN used) THEN
        action := state[s].firstAction;
        WHILE action # NIL DO
          IF (NOT (action^.target^.state IN used)) THEN
            action^.target^.state := newStateNr[action^.target^.state]
          END;
          action := action^.next
        END
      END;
      INC(s)
    END;
    (*-------- delete unused states --------*)
    s := rootState + 1; next := s;
    WHILE s <= lastState DO
      IF (s IN used) THEN
        IF (next < s) THEN state[next] := state[s] END;
        newStateNr[s] := next; INC(next)
      ELSE
        DeleteActionList(state[s].firstAction)
      END;
      INC(s)
    END;
    lastState := next - 1;
    s := rootState;
    WHILE s <= lastState DO
      action := state[s].firstAction;
      WHILE action # NIL DO
        action^.target^.state := newStateNr[action^.target^.state];
        action := action^.next
      END;
      INC(s)
    END
  END DeleteRedundantStates;

(* ConvertToStates    Convert the TDG in gp into a subautomaton of the DFA
------------------------------------------------------------------------*)
PROCEDURE ConvertToStates (p : CR.Parser ; gp, sp: INTEGER) =
(*note: gn.line is abused as a state number!*)

  PROCEDURE TheState (gp: INTEGER): INTEGER =
    VAR
      s: INTEGER;
      gn: CRT.GraphNode;
    BEGIN
      IF gp = 0 THEN s := NewState(); state[s].endOf := sp; RETURN s
      ELSE CRT.GetNode(gp, gn); RETURN gn.line
      END
    END TheState;

  PROCEDURE Step (from, gp: INTEGER) =
    VAR
      gn: CRT.GraphNode;
    BEGIN
      IF gp = 0 THEN RETURN END;
      CRT.GetNode(gp, gn);
      CASE gn.typ OF
        CRT.class, CRT.char=>
          NewTransition(p, from, gn, TheState(ABS(gn.next)))
      | CRT.alt=>
          Step(from, gn.p1); Step(from, gn.p2)
      | CRT.opt, CRT.iter=>
          Step(from, ABS(gn.next)); Step(from, gn.p1)
      ELSE
        Wr.PutText(Stdio.stderr, "fatal error in CRA.Step()!\n") ;
        Process.Exit(1)
      END
    END Step;

  PROCEDURE FindTrans (p, snr: INTEGER) =
    VAR
      gn:  CRT.GraphNode;
      new: BOOLEAN;
    BEGIN
      IF p = 0 THEN RETURN END; (*end of graph*)
      CRT.GetNode(p, gn);
      IF gn.line >= 0 THEN RETURN END;  (*already visited*)
      new := snr < rootState;
      IF new THEN snr := NewState() END;
      gn.line := snr; CRT.PutNode(p, gn);
      IF CRT.DelGraph(p) THEN state[snr].endOf := sp END;
      (*snr is end state*)
      CASE gn.typ OF
        CRT.class, CRT.char=>
          FindTrans(ABS(gn.next), rootState - 1);
      | CRT.opt=>
          FindTrans(ABS(gn.next), rootState - 1); FindTrans(gn.p1, snr)
      | CRT.iter=>
          FindTrans(ABS(gn.next), snr); FindTrans(gn.p1, snr)
      | CRT.alt=>
          FindTrans(gn.p1, snr); FindTrans(gn.p2, snr)
      ELSE
        Wr.PutText(Stdio.stderr, "fatal error in CRA.FindTrans()!\n") ;
        Process.Exit(1)
      END;
      IF new OR ((snr = rootState) AND (p = gp)) THEN Step(snr, p) END
    END FindTrans;

  VAR
    gn: CRT.GraphNode;
  BEGIN
    IF CRT.DelGraph(gp) THEN
      p.error("token may be empty")
    END;
    FOR i := 0 TO CRT.nNodes DO
      CRT.GetNode(i, gn); gn.line := -1; CRT.PutNode(i, gn)
    END ;
    FindTrans(gp, rootState)
  END ConvertToStates;

(* MatchesDFA         TRUE, if the string str can be recognized by the DFA
------------------------------------------------------------------------*)
(*--++
PROCEDURE MatchesDFA (str: ARRAY OF CHAR; VAR matchedSp: INTEGER): BOOLEAN;
  VAR
    len: CARDINAL;

  PROCEDURE Match (p: CARDINAL; s: INTEGER): BOOLEAN;
    VAR
      ch:    CHAR;
      a:     Action;
      equal: BOOLEAN;
      set:   CRT.Set;
    BEGIN
      IF p >= len THEN
        IF state[s].endOf # CRT.noSym
          THEN matchedSp := state[s].endOf; RETURN TRUE
        ELSE RETURN FALSE
        END
      END;
      a := state[s].firstAction; ch := str[p];
      WHILE a # NIL DO
        CASE a^.typ OF
          CRT.char:
            equal := VAL(INTEGER, ORD(ch)) = a^.sym
        | CRT.class:
            CRT.GetClass(a^.sym, set); equal := (ORD(ch) IN set)
        END;
        IF equal THEN RETURN Match(p + 1, a^.target^.state) END;
        a := a^.next
      END;
      RETURN FALSE
    END Match;

  BEGIN
    len := Text.Length(str) - 1; (*strip quotes*)
    RETURN Match(1, rootState)
  END MatchesDFA;
++--*)

  PROCEDURE MatchDFA (p : CR.Parser ; str: TEXT; sp: INTEGER;
                      VAR matchedSp: INTEGER) =
    VAR
      s, to: INTEGER (*State*);
      a: Action;
      gn : CRT.GraphNode;
      i, len: INTEGER;
    BEGIN (* s with quotes *)
      s := rootState; i := 1; len := Text.Length(str) - 1;
      LOOP (* try to match str against existing DFA *)
        IF i = len THEN EXIT END;
        a := TheAction(state[s], Text.GetChar(str, i));
        IF a = NIL THEN EXIT END;
        s := a^.target^.state; INC(i)
      END;
      WHILE i < len DO (* make new DFA for str[i..len-1] *)
        to := NewState();
        gn.typ := CRT.char; gn.p1 := ORD(Text.GetChar(str, i)); gn.p2 := CRT.normTrans;
        NewTransition(p, s, gn, to); (* PDT Tue  01-11-94 *)
        s := to; INC(i)
      END;
      matchedSp := state[s].endOf;
      IF state[s].endOf = CRT.noSym THEN state[s].endOf := sp END
    END MatchDFA;

(* SplitActions     Generate unique actions from two overlapping actions
-----------------------------------------------------------------------*)
PROCEDURE SplitActions (a, b: Action) =
  VAR
    c:                Action;
    seta, setb, setc: CRT.Set;

  PROCEDURE CombineTransCodes (t1, t2: INTEGER; VAR result: INTEGER) =
    BEGIN
      IF t1 = CRT.contextTrans THEN result := t1 ELSE result := t2 END
    END CombineTransCodes;

  BEGIN
    MakeSet(a, seta); MakeSet(b, setb);
    IF (seta = setb) THEN
      AddTargetList(b^.target, a^.target);
      DeleteTargetList(b^.target);
      CombineTransCodes(a^.tc, b^.tc, a^.tc);
      DetachAction(b, a);
      (* free(b) *)
    ELSIF (setb <= seta) THEN
      setc := seta; setc := setc - setb;
      AddTargetList(a^.target, b^.target);
      CombineTransCodes(a^.tc, b^.tc, b^.tc);
      ChangeAction(a, setc)
    ELSIF (seta <= setb) THEN
      setc := setb; setc := setc - seta ;
      AddTargetList(b^.target, a^.target);
      CombineTransCodes(a^.tc, b^.tc, a^.tc);
      ChangeAction(b, setc)
    ELSE
      setc := seta * setb ;
      seta := seta - setc ;
      setb := setb - setc ;
      ChangeAction(a, seta);
      ChangeAction(b, setb);
      c := NEW(Action) ;
      c^.target := NIL;
      CombineTransCodes(a^.tc, b^.tc, c^.tc);
      AddTargetList(a^.target, c^.target);
      AddTargetList(b^.target, c^.target);
      ChangeAction(c, setc);
      AddAction(c, a)
    END
  END SplitActions;

(* MakeUnique           Make all actions in this state unique
-------------------------------------------------------------------------*)
PROCEDURE MakeUnique (s: INTEGER; VAR changed: BOOLEAN) =
  VAR
    a, b: Action;

  PROCEDURE Overlap (a, b: Action): BOOLEAN =
    VAR
      seta, setb: CRT.Set;
    BEGIN
      IF a^.typ = CRT.char THEN
        IF b^.typ = CRT.char
          THEN RETURN a^.sym = b^.sym
          ELSE CRT.GetClass(b^.sym, setb); RETURN (a^.sym IN setb)
        END
      ELSE
        CRT.GetClass(a^.sym, seta);
        IF b^.typ = CRT.char
          THEN RETURN (b^.sym IN seta)
          ELSE CRT.GetClass(b^.sym, setb);
               RETURN (NOT ((seta * setb) = CRT.Set{}))
        END
      END
    END Overlap;

  BEGIN
    a := state[s].firstAction; changed := FALSE;
    WHILE a # NIL DO
      b := a^.next;
      WHILE b # NIL DO
        IF Overlap(a, b) THEN
          SplitActions(a, b); changed := TRUE; RETURN
          (* originally no RETURN.  FST blows up if we leave RETURN out.
             Somewhere there is a field that is not properly set, but I
             have not chased this down completely Fri  08-20-1993 *)
        END;
        b := b^.next;
      END;
      a := a^.next
    END;
  END MakeUnique;

(* MeltStates       Melt states appearing with a shift of the same symbol
-----------------------------------------------------------------------*)
PROCEDURE MeltStates (s: INTEGER; VAR correct: BOOLEAN) =
  VAR
    action:  Action;
    ctx:     BOOLEAN;
    endOf:   INTEGER;
    melt:    Melted;
    set:     CRT.Set;
    s1:      INTEGER;
    changed: BOOLEAN;

  PROCEDURE AddMeltedSet (nr: INTEGER; VAR set: CRT.Set) =
    VAR
      m: Melted;
    BEGIN
      m := firstMelted;
      WHILE (m # NIL) AND (m^.state # nr) DO m := m^.next END;
      IF m = NIL THEN CRT.Restriction(-1, 0) (* compiler error *) END;
      set := set + m^.set ;
    END AddMeltedSet;

  PROCEDURE GetStateSet (t: Target; VAR set: CRT.Set; VAR endOf: INTEGER;
                         VAR ctx: BOOLEAN) =
  (* Modified back to match Oberon version Fri  08-20-1993
     This seemed to cause problems with some larger automata *)
     (* new bug fix Wed  11-24-1993  from ETHZ incorporated *)
    VAR
      lastS : INTEGER;
    BEGIN
      set := CRT.Set{}; endOf := CRT.noSym; ctx := FALSE;
      lastS := lastState; (* Fri  08-20-1993 *)
      WHILE t # NIL DO
        IF t^.state <= lastSimState THEN set := set + CRT.Set{t^.state};
        ELSE AddMeltedSet(t^.state, set);
        END;
        IF state[t^.state].endOf # CRT.noSym THEN
          IF (endOf = CRT.noSym) OR (endOf = state[t^.state].endOf) THEN
             endOf := state[t^.state].endOf; lastS := t^.state
          ELSE
            Wr.PutText(Stdio.stderr, Fmt.F("\nTokens %s and %s cannot "
                                           & "be distinguished\n",
                                           Fmt.Int(endOf),
                                           Fmt.Int(state[t.state].endOf))) ;
            correct := FALSE;
          END;
        END;
        IF state[t^.state].ctx THEN
          ctx := TRUE;
          IF state[t^.state].endOf # CRT.noSym THEN
            Wr.PutText(Stdio.stderr, "\nAmbiguous CONTEXT clause.\n") ;
            correct := FALSE
          END
        END;
        t := t^.next
      END
    END GetStateSet;

  PROCEDURE FillWithActions (s: INTEGER; targ: Target) =
    VAR
      action, a: Action;
    BEGIN
      WHILE targ # NIL DO
        action := state[targ^.state].firstAction;
        WHILE action # NIL DO
          a := NEW(Action) ;
          a^ := action^; a^.target := NIL;
          AddTargetList(action^.target, a^.target);
          AddAction(a, state[s].firstAction);
          action := action^.next
        END;
        targ := targ^.next
      END;
    END FillWithActions;

  PROCEDURE KnownMelted (READONLY set: CRT.Set; VAR melt: Melted): BOOLEAN =
    BEGIN
      melt := firstMelted;
      WHILE melt # NIL DO
        IF (set = melt^.set) THEN RETURN TRUE END;
        melt := melt^.next
      END;
      RETURN FALSE
    END KnownMelted;

  BEGIN
    action := state[s].firstAction;
    WHILE action # NIL DO
      IF action^.target^.next # NIL THEN
        GetStateSet(action^.target, set, endOf, ctx);
        IF NOT  KnownMelted(set, melt) THEN
          s1 := NewState();
          state[s1].endOf := endOf; state[s1].ctx := ctx;
          FillWithActions(s1, action^.target);
          REPEAT MakeUnique(s1, changed) UNTIL NOT  changed;
          melt := NewMelted(set, s1);
        END;
        DeleteTargetList(action^.target^.next);
        action^.target^.next := NIL;
        action^.target^.state := melt^.state
      END;
      action := action^.next
    END
  END MeltStates;

(* MakeDeterministic     Make NDFA --> DFA
------------------------------------------------------------------------*)
PROCEDURE MakeDeterministic (VAR correct: BOOLEAN) =
  VAR
    s:       INTEGER;
    changed: BOOLEAN;

  PROCEDURE FindCtxStates() =
  (* Find states reached by a context transition *)
    VAR
      a: Action;
      s: INTEGER;
    BEGIN
      s := rootState;
      WHILE s <= lastState DO
        a := state[s].firstAction;
        WHILE a # NIL DO
          IF a^.tc = CRT.contextTrans THEN
            state[a^.target^.state].ctx := TRUE
          END;
          a := a^.next
        END;
        INC(s)
      END;
    END FindCtxStates;

  BEGIN
    lastSimState := lastState;
    FindCtxStates();
    s := rootState;
    WHILE s <= lastState DO
      REPEAT MakeUnique(s, changed) UNTIL NOT  changed;
      INC(s)
    END;
    correct := TRUE;
    s := rootState;
    WHILE s <= lastState DO MeltStates(s, correct); INC(s) END;
    DeleteRedundantStates();
    CombineShifts();
(* ====    IF CRT.ddt["A"] THEN PrintStates END ==== *)
  END MakeDeterministic;



(* GenComment            Generate a procedure to scan comments
-------------------------------------------------------------------------*)
PROCEDURE GenComment(wr : Wr.T ; com : Comment) =

  PROCEDURE GenBody(pad : TEXT) =
  BEGIN
    Wr.PutText(wr, pad & "LOOP\n") ;
    Wr.PutText(wr, pad & "  IF (s.ch = ") ;
    Wr.PutText(wr, CharConst(com.stop[0])) ;
    Wr.PutText(wr, ") THEN\n") ;
    IF (com.stop[1] = ASCII.NUL) THEN
      Wr.PutText(wr, pad & "    DEC(level) ;\n") ;
      Wr.PutText(wr, pad & "    s.oldEols := s.currentLine - startLine ;\n") ;
      Wr.PutText(wr, pad & "    NextCh(s) ;\n") ;
      Wr.PutText(wr, pad & "    IF (level = 0) THEN\n") ;
      Wr.PutText(wr, pad & "      RETURN TRUE\n") ;
      Wr.PutText(wr, pad & "    END ;\n")
    ELSE
      Wr.PutText(wr, pad & "    NextCh(s) ;\n") ;
      Wr.PutText(wr, pad & "    IF (s.ch = ") ;
      Wr.PutText(wr, CharConst(com.stop[1])) ;
      Wr.PutText(wr, ") THEN\n") ;
      Wr.PutText(wr, pad & "      DEC(level) ;\n") ;
      Wr.PutText(wr, pad & "      NextCh(s) ;\n") ;
      Wr.PutText(wr, pad & "      IF (level = 0) THEN\n") ;
      Wr.PutText(wr, pad & "        RETURN TRUE\n") ;
      Wr.PutText(wr, pad & "      END\n") ;
      Wr.PutText(wr, pad & "    END ;\n")
    END ;
    IF (com.nested) THEN
      Wr.PutText(wr, pad & "  ELSIF (s.ch = ") ;
      Wr.PutText(wr, CharConst(com.start[0])) ;
      Wr.PutText(wr, ") THEN\n") ;
      IF (com.start[1] = ASCII.NUL) THEN
        Wr.PutText(wr, pad & "    INC(level) ;\n") ;
        Wr.PutText(wr, pad & "    NextCh(s) ;\n")
      ELSE
        Wr.PutText(wr, pad & "    NextCh(s) ;\n") ;
        Wr.PutText(wr, pad & "    IF (s.ch = ") ;
        Wr.PutText(wr, CharConst(com.start[1])) ;
        Wr.PutText(wr, ") THEN\n") ;
        Wr.PutText(wr, pad & "      INC(level) ;\n") ;
        Wr.PutText(wr, pad & "      NextCh(s)\n") ;
        Wr.PutText(wr, pad & "    END ;\n") ;
      END
    END ;
    Wr.PutText(wr, pad & "  ELSIF (s.ch = ASCII.NUL) THEN\n") ;
    Wr.PutText(wr, pad & "    RETURN FALSE\n") ;
    Wr.PutText(wr, pad & "  ELSE\n") ;
    Wr.PutText(wr, pad & "    NextCh(s)\n") ;
    Wr.PutText(wr, pad & "  END\n") ;
    Wr.PutText(wr, pad & "END ;\n")
  END GenBody ;

BEGIN
  Wr.PutText(wr, "IF (s.ch = ") ;
  Wr.PutText(wr, CharConst(com.start[0])) ;
  Wr.PutText(wr, ") THEN\n") ;
  IF (com.start[1] = ASCII.NUL) THEN
    Wr.PutText(wr, "  NextCh(s) ;\n") ;
    GenBody("  ")
  ELSE
    Wr.PutText(wr, "  NextCh(s) ;\n") ;
    Wr.PutText(wr, "  IF (s.ch = ") ;
    Wr.PutText(wr, CharConst(com.start[1])) ;
    Wr.PutText(wr, ") THEN\n") ;
    Wr.PutText(wr, "    NextCh(s) ;\n") ;
    GenBody("    ") ;
    Wr.PutText(wr, "  ELSE\n") ;
    Wr.PutText(wr, "    IF (s.ch = '\\n') THEN\n") ;
    Wr.PutText(wr, "      DEC(s.currentLine)\n") ;
    Wr.PutText(wr, "    END ;\n") ;
    Wr.PutText(wr, "    PrevCh(s)\n") ;
    Wr.PutText(wr, "  END\n")
  END ;
  Wr.PutText(wr, "END ;\n") ;
END GenComment ;

(* GenLiterals           Generate CASE for the recognition of literals
-------------------------------------------------------------------------*)
PROCEDURE GenLiterals(wr : Wr.T) =
VAR FirstLine : BOOLEAN ;
    i, j, k   : INTEGER ;
    width     := 0 ;
    key       : ARRAY [0 .. 127] OF TEXT ;
    knr       : ARRAY [0 .. 127] OF INTEGER ;
    ch        : CHAR ;
    sn        : CRT.SymbolNode ;
BEGIN
(* sort literal list *)
  i := 0 ;
  k := 0 ;
  WHILE (i <= CRT.maxT) DO
    CRT.GetSym(i, sn) ;
    IF (sn.struct = CRT.litToken) THEN
      width := MAX(width, Text.Length(sn.name) - 2) ;
      j := k - 1 ;
      WHILE ((j >= 0) AND (Text.Compare(sn.name, key[j]) < 0)) DO
        key[j + 1] := key[j] ;
        knr[j + 1] := knr[j] ;
        DEC(j)
      END ;
      key[j + 1] := sn.name ;
      knr[j + 1] := i ;
      INC(k)
    END ;
    INC(i)
  END ;

  Frame.Put("MaxLiteralWidth", Fmt.Int(width)) ;

(* print CASE statement *)
  IF (k # 0) THEN
    Wr.PutText(wr, "PROCEDURE CheckLiteral(nomatch : Symbol) : Symbol =\n") ;
    Wr.PutText(wr, "BEGIN\n") ;
    Wr.PutText(wr, "  CASE s.buf[0] OF\n") ;
    i := 0 ;
    FirstLine := TRUE ;
    WHILE (i < k) DO
      IF (FirstLine) THEN
        Wr.PutText(wr, "    ") ;
        FirstLine := FALSE
      ELSE
        Wr.PutText(wr, "  | ")
      END ;
      ch := Text.GetChar(key[i], 1) ; (* key[i, 0] = quote *)
      Wr.PutText(wr, CharConst(ch)) ;
      j := i ;
      REPEAT
        IF (i = j) THEN
          Wr.PutText(wr, " => IF")
        ELSE
          Wr.PutText(wr, "         ELSIF")
        END ;
        Wr.PutText(wr, " ((s.bufLen = " & Fmt.Int(Text.Length(key[i]) - 2)) ;
        Wr.PutText(wr, ") AND Equal(" & key[i] & ")) THEN\n") ;
        Wr.PutText(wr, "             RETURN ") ;
        Wr.PutText(wr, SymbolName(knr[i])) ;
        Wr.PutChar(wr, '\n') ;
        INC(i)
      UNTIL ((i = k) OR (Text.GetChar(key[i], 1) # ch)) ;
      Wr.PutText(wr, "           END\n")
    END ;
    Wr.PutText(wr, "  ELSE\n") ;
    Wr.PutText(wr, "  END ;\n") ;
    Wr.PutText(wr, "  RETURN nomatch\n") ;
    Wr.PutText(wr, "END CheckLiteral ;\n")
  END
END GenLiterals ;

(* WriteState           Write the source text of a scanner state
-------------------------------------------------------------------------*)
PROCEDURE WriteState(wr : Wr.T ; s : INTEGER ; first : BOOLEAN) =
VAR action : Action ;
    ctxEnd : BOOLEAN ;
    sn     : CRT.SymbolNode ;
    endOf  : INTEGER ;
    set    : CRT.Set ;
BEGIN
  endOf := state[s].endOf ;
  IF ((endOf > CRT.maxT) AND (endOf # CRT.noSym)) THEN
    (*pragmas have been moved*)
    endOf := CRT.maxT + CRT.maxSymbols - endOf
  END ;
  IF (first) THEN
    Wr.PutText(wr, "  ")
  ELSE
    Wr.PutText(wr, "| ")
  END ;
  Wr.PutText(wr, Fmt.F("%3s => ", Fmt.Int(s))) ;
  first := TRUE ;
  ctxEnd := state[s].ctx ;
  action := state[s].firstAction ;
  WHILE (action # NIL) DO
    IF (action = state[s].firstAction) THEN
      Wr.PutText(wr, "IF ")
    ELSE
      Wr.PutText(wr, "         ELSIF ")
    END ;
    IF (action.typ = CRT.char) THEN
      Wr.PutText(wr, "(s.ch = " & CharConst(VAL(action.sym, CHAR)) & ")")
    ELSE
      CRT.GetClass(action.sym, set) ;
      PutRange(wr, set, 12)
    END ;
    Wr.PutText(wr, " THEN\n") ;
    IF (action.target.state # s) THEN
      Wr.PutText(wr, Fmt.F("           state := %s ;\n",
                                                Fmt.Int(action.target.state)))
    END ;
    IF (action.tc = CRT.contextTrans) THEN
      Wr.PutText(wr, "           INC(apx)\n") ;
      ctxEnd := FALSE
    ELSIF (state[s].ctx) THEN
      Wr.PutText(wr, "           apx := 0\n")
    END ;
    action := action.next
  END ;
  IF (state[s].firstAction # NIL) THEN
    Wr.PutText(wr, "         ELSE\n") ;
    Wr.PutText(wr, "           ")
  END ;
  IF (endOf = CRT.noSym) THEN
    Wr.PutText(wr, "RETURN " & SymbolName(CRT.maxT) & "\n")
  ELSE (*final state*)
    CRT.GetSym(endOf, sn);
    IF (ctxEnd) THEN (*cut appendix*)
      Wr.PutText(wr, "bp := bp - apx - 1 ;\n") ;
      Wr.PutText(wr, "           DEC(nextLen, apx) ;\n") ;
      Wr.PutText(wr, "           NextCh(s) ;\n") ;
      Wr.PutText(wr, "           ")
    END ;
    IF (sn.struct = CRT.classLitToken) THEN
      Wr.PutText(wr, "RETURN CheckLiteral(") ;
      Wr.PutText(wr, SymbolName(endOf)) ;
      Wr.PutText(wr, ")\n")
    ELSE
      Wr.PutText(wr, "RETURN ") ;
      Wr.PutText(wr, SymbolName(endOf)) ;
      Wr.PutChar(wr, '\n')
    END
  END ;
  IF (state[s].firstAction # NIL) THEN
    Wr.PutText(wr, "         END\n")
  END
END WriteState ;

(* Generate - generate the source code fragments for the scanner *)
PROCEDURE Generate() =
VAR startTab : ARRAY [0 .. 255] OF INTEGER ;
    com      : Comment ;
    s        : INTEGER ;
    gn       : CRT.GraphNode ;
    sn       : CRT.SymbolNode ;
    wr       : Wr.T ;

  PROCEDURE FillStartTab() =
  VAR action          : Action ;
      i, targetState,
      undefState      : INTEGER ;
      class           : CRT.Set ;
  BEGIN
    undefState := lastState + 2 ;
    i := 0 ;
    WHILE (i < 256) DO
      startTab[i] := undefState ;
      INC(i)
    END ;
    action := state[rootState].firstAction ;
    WHILE (action # NIL) DO
      targetState := action.target.state ;
      IF action.typ = CRT.char THEN
        startTab[action.sym] := targetState
      ELSE
        CRT.GetClass(action.sym, class) ;
        i := 0 ;
        WHILE (i < 256) DO
          IF (i IN class) THEN
            startTab[i] := targetState
          END ;
          INC(i)
        END
      END;
      action := action.next
    END
  END FillStartTab ;

BEGIN
  FillStartTab() ;

  CRT.GetNode(CRT.root, gn) ;
  CRT.GetSym(gn.p1, sn) ;

(* register the scanner name *)

  Frame.Put("Scanner", sn.name & "S") ;

(* define the unknown symbol *)

  Frame.Put("UnknownSym", SymbolName(CRT.maxT)) ;
  Frame.Put("EofSym", SymbolName(CRT.eofSy)) ;

  IF (CRT.ignoreCase) THEN
    Frame.Put("IgnoreCase", "TRUE")
  ELSE
    Frame.Put("IgnoreCase", "FALSE")
  END ;

  wr := TextWr.New() ;

(* generate the comment processing code *)

  com := firstComment ;
  WHILE (com # NIL) DO
    GenComment(wr, com) ;
    com := com.next
  END ;
  Frame.Put("Comment", TextWr.ToText(wr)) ;

(* generate the literal processing code *)

  GenLiterals(wr) ;
  Frame.Put("CheckLiteral", TextWr.ToText(wr)) ;

(* generate the character and comment ignoring code *)

  IF (NOT ORD(cr) IN CRT.ignored) THEN
    Wr.PutText(wr, "IF (s.oldEols > 0) THEN\n") ;
    Wr.PutText(wr, "  PrevCh(s) ;\n") ;
    Wr.PutText(wr, "  DEC(s.oldEols) ;\n") ;
    Wr.PutText(wr, "  s.ch := '\\n'\n") ;
    Wr.PutText(wr, "END ;\n")
  END ;
  Wr.PutText(wr, "WHILE ((s.ch = ' ')") ;
  IF (CRT.ignored # CRT.Set{}) THEN
    Wr.PutText(wr, " OR ") ;
    PutRange(wr, CRT.ignored, 7)
  END ;
  Wr.PutText(wr, ") DO\n") ;
  Wr.PutText(wr, "  NextCh(s)\n") ;
  Wr.PutText(wr, "END ;\n") ;

  IF (firstComment # NIL) THEN
    Wr.PutText(wr, "IF ((") ;
    com := firstComment ;
    WHILE (com # NIL) DO
      Wr.PutText(wr, "(s.ch = " & CharConst(com.start[0]) & ")") ;
      IF (com.next # NIL) THEN
        Wr.PutText(wr, " OR ")
      END ;
      com := com.next
    END ;
    Wr.PutText(wr, ") AND Comment(s)) THEN\n") ;
    Wr.PutText(wr, "  Get(s, ss) ;\n") ;
    Wr.PutText(wr, "  RETURN\n") ;
    Wr.PutText(wr, "END ;\n")
  END ;

  Frame.Put("GetPrelude", TextWr.ToText(wr)) ;

(* generate the automaton *)

  s := rootState + 1 ;
  WHILE (s <= lastState) DO
    WriteState(wr, s, (s = (rootState + 1))) ;
    INC(s)
  END ;

  Frame.Put("GetCore", TextWr.ToText(wr)) ;

(* generate the start table contents *)

  FOR i := 0 TO 255 DO
    IF ((i > 0) AND ((i MOD 16) = 0)) THEN
      Wr.PutChar(wr, '\n')
    END ;
    Wr.PutText(wr, Fmt.F("%3s", Fmt.Int(startTab[i]))) ;
    IF (i # 255) THEN
      Wr.PutChar(wr, ',')
    END
  END ;

  Frame.Put("StartTable", TextWr.ToText(wr)) ;
END Generate ;

BEGIN (* CRA *)
  lastState := -1 ;
  rootState := NewState() ;
  firstMelted := NIL ;
  firstComment := NIL ;
  NewLine := TRUE
END CRA.
