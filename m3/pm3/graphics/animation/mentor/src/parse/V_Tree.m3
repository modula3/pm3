(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Thu Jan  5 22:59:29 PST 1995 by najork *)
(*      modified on Fri Jan 15 09:25:40 PST 1993 by steveg *)
(*      modified on Wed Aug  5 09:12:56 PDT 1992 by kalsow *)
(*      modified on Sun Jul 26 10:25:16 1992 by rustan     *)

MODULE V_Tree;

IMPORT Text, R2, PaintOp, GraphVBT, View, ZeusPanel;
IMPORT Parse, ParseViewClass, ParseColor;
IMPORT Stdio, Wr, Fmt, Thread; (*for DEBUG*)

VAR DEBUG := FALSE;

TYPE
  T = ParseViewClass.T OBJECT
        state     : Parse.State  := NIL;
        graph     : GraphVBT.T   := NIL;
        nodes     : Node         := NIL;
        last_node : Node         := NIL;
        map       : NodeList     := NIL;
        length    : INTEGER      := 0;
        cursor    : INTEGER      := 0;
        highlight : GraphVBT.VertexHighlight := NIL;  (* NIL, unless active *)
        v_scale   : REAL;
        resize    : BOOLEAN;
        next_knee : INTEGER;
        font      : GraphVBT.WorldFont;
      OVERRIDES
        oeSetup      := Setup;
        oeScan       := Scan;
        oePush       := Push;
        oePop        := Pop;
        oeNoteError  := NoteError;
        oeNewNode    := NewNode;
        oeNewTerm    := NewTerm;
        oeDeleteLeaf := DeleteLeaf;
        oeNewEdge    := NewEdge;
        oeUpdateDone := UpdateDone;
      END;

TYPE
  Kind = { Leaf, Knee, Node };

TYPE
  NodeList = REF ARRAY OF Node;
  Node = GraphVBT.Vertex OBJECT
           uid          : INTEGER   := 0;
           kind         : Kind      := Kind.Leaf;
           parent       : Node      := NIL;
           children     : Node      := NIL;
           next_sibling : Node      := NIL;
           next         : Node      := NIL;
           x,y          : REAL;
           deleted      : BOOLEAN   := FALSE;
         END;

CONST
  Min_V = 0.1;
  Max_V = 0.9;

PROCEDURE Setup (t: T;  s: Parse.State) RAISES {Thread.Alerted} =
  VAR
    max_tok : INTEGER;
    n_chars : INTEGER;
    n       : Node;
    used    : INTEGER;
    len     : INTEGER;
    scale   : REAL;
    h_pos   : REAL;
  BEGIN
    (* remove any existing vertices from graph *)
    IF (t.nodes # NIL) THEN
      LOCK t.graph.mu DO
        n := t.nodes;
        WHILE (n # NIL) DO
          IF (NOT n.deleted) THEN n.remove() END;
          n := n.next;
        END;
        IF (t.highlight # NIL) THEN t.highlight.remove () END;
      END;
    END;

    (* compute the total # of characters of input *)
    n_chars := 1; (* the terminal space *)
    max_tok := 1;
    FOR i := 0 TO s.n_tokens-1 DO
      (*@@ INC (n_chars, Text.Length (s.tokens[i])); @@*)
      max_tok := MAX (max_tok, Text.Length (s.tokens[i])); (*@@*)
    END;
    INC (max_tok); (* leave a little breathing room *)
    n_chars := (max_tok+1) * (s.n_tokens+1); (*@@*)

    (* initialize the view *)
    t.state     := s;
    t.nodes     := NIL;
    t.last_node := NIL;
    t.map       := NEW (NodeList, 2 * n_chars + 10);  (* an initial guess *)
    t.length    := n_chars;
    t.cursor    := 0;
    t.highlight := NIL;
    t.v_scale   := 0.2;
    t.next_knee := 0;

    (* add the terminal (leaf) nodes *)
    FOR i := 0 TO s.n_tokens - 1 DO
      AddNode (t, i, NewLeaf (t, s.tokens[i], max_tok, ParseColor.Virgin));
    END;
    (* add an error node *)
    AddNode (t, s.n_tokens, NewLeaf (t, " ", max_tok, ParseColor.Clear));

    (* display all vertices in one place *)
    t.graph.redisplay ();

    (* move vertices to correct positions via animation *)
    LOCK t.graph.mu DO
      used := 0;  scale := 1.0 / FLOAT (n_chars + 1);
      FOR i := 0 TO s.n_tokens DO
        (*@@
        IF (i < s.n_tokens)
          THEN len := Text.Length (s.tokens[i]);
          ELSE len := 1;
        END; @@*) len := max_tok + 1;
        h_pos := (FLOAT (used) + 0.5 * FLOAT (len) + 0.5) * scale;
        Move (t.map[i], h_pos, Max_V);
        INC (used, len);
      END;
    END;

    (* and display the final result *)
    t.graph.animate (0.0, 1.0);
  END Setup;

PROCEDURE Move (n: Node;  x, y: REAL) =
  BEGIN
    IF (n.x # x) OR (n.y # y) THEN
      n.x := x;
      n.y := y;
      n.move (R2.T {x, y}, TRUE);
    END;
  END Move;

PROCEDURE AddNode (t: T;  id: INTEGER;  n: Node) =
  BEGIN
    (* append n to the tail of t.nodes *)
    n.next := NIL;
    IF (t.nodes = NIL) THEN
      t.nodes := n;
      t.last_node := n;
    ELSE
      t.last_node.next := n;
      t.last_node := n;
    END;

    (* and insert it into the direct map *)
    n.uid := id;
    IF (id >= 0) THEN
      IF (id > LAST (t.map^)) THEN ExpandMap (t) END;
      <*ASSERT t.map[id] = NIL *>
      t.map[id] := n;
    END;
  END AddNode;

PROCEDURE ExpandMap (t: T) =
  VAR new := NEW (NodeList, 2 * NUMBER (t.map^));
  BEGIN
    FOR i := 0 TO LAST (t.map^) DO new[i] := t.map[i] END;
    t.map := new;
  END ExpandMap;

PROCEDURE NewLeaf (t: T;  label: TEXT;  width: INTEGER;  c: PaintOp.T): Node =
  VAR len := Text.Length (label);  scaled_len: REAL;
  BEGIN
    IF (label = NIL) THEN c := ParseColor.Clear;  label := " ";  END;
    IF (width > 0) THEN len := width END;
    scaled_len := FLOAT (len) / FLOAT (t.length + 2);
    RETURN NEW (Node,
                   graph := t.graph,
                   shape := GraphVBT.VertexShape.Rectangle,
                   pos   := R2.T { 0.0, Max_V },
                   size := R2.T { scaled_len, t.v_scale },
                   color := c,
                   label := label,
                   font  := t.font,
                   uid   := -1,
                   kind  := Kind.Leaf,
                   x     := 0.0,
                   y     := Max_V
               ).init();
  END NewLeaf;

PROCEDURE NewNode (t: T;  id: INTEGER;  op: TEXT) =
  VAR
    n: Node;
    len := Text.Length (op);
    scaled_len := FLOAT (len+1) / FLOAT (t.length + 2);
  BEGIN
    n := NEW (Node,
                   graph := t.graph,
                   shape := GraphVBT.VertexShape.Ellipse,
                   pos   := R2.T { 1.0, 0.0 },
                   size := R2.T { scaled_len, t.v_scale },
                   color := ParseColor.Passive,
                   label := op,
                   font  := t.font,
                   uid   := -1,
                   kind  := Kind.Node,
                   x     := 1.0,
                   y     := 0.0
               ).init();
    AddNode (t, id, n);
  END NewNode;

PROCEDURE NewTerm (t: T;  id: INTEGER;  op: TEXT) =
  VAR
    n: Node;
    len := Text.Length (op);
    scaled_len := FLOAT (len+1) / FLOAT (t.length + 2);
  BEGIN
    n := NEW (Node,
                   graph := t.graph,
                   shape := GraphVBT.VertexShape.Ellipse,
                   pos   := R2.T { 1.0, 0.0 },
                   size := R2.T { scaled_len, t.v_scale },
                   color := ParseColor.Passive,
                   label := op,
                   font  := t.font,
                   uid   := -1,
                   kind  := Kind.Node,
                   x     := 1.0,
                   y     := 0.0
               ).init();
    AddNode (t, id, n);
  END NewTerm;

PROCEDURE DeleteLeaf (t: T;  id: INTEGER) =
  VAR  n := t.map [id];  p, x0, x1: Node;
  BEGIN
    LOCK t.graph.mu DO
      WHILE (n # NIL) DO
        IF (n.deleted) OR (n.children # NIL) THEN EXIT END;
        n.remove ();
        n.deleted := TRUE;

        (* remove n from its parent's list of children *)
        p := n.parent;
        IF (p # NIL) THEN
          x0 := p.children;  x1 := NIL;
          LOOP
            IF (x0 = NIL) THEN EXIT END;
            IF (x0 = n) THEN
              IF (x1 = NIL)
                THEN p.children := n.next_sibling;
                ELSE x1.next_sibling := n.next_sibling;
              END;
              EXIT;
            END;
            x1 := x0;
            x0 := x0.next_sibling;
          END;
        END;

        n := n.parent;
      END;
    END;
    t.graph.redisplay ();
  END DeleteLeaf;

PROCEDURE NewEdge (t: T;  child_id, parent_id: INTEGER) =
  VAR
    child  := t.map[child_id];
    parent := t.map[parent_id];
    knee, n: Node;
  BEGIN
   DEC (t.next_knee);
   knee := NEW (Node,
                   graph        := t.graph,
                   shape        := GraphVBT.VertexShape.Rectangle,
                   pos          := R2.T { child.x, child.y - t.v_scale },
                   size        := R2.Origin, (* empty size *)
                   uid          := t.next_knee,
                   parent       := parent,
                   children     := child,
                   next_sibling := NIL,
                   kind         := Kind.Knee,
                   x            := child.x,
                   y            := 0.0
               ).init();

    (* link the knee between the parent and children *)
    child.parent := knee;
    IF (parent.children = NIL) THEN
      parent.children := knee;
    ELSE
      (* scan for the end of the parent's list of children *)
      n := parent.children;
      WHILE (n.next_sibling # NIL) DO n := n.next_sibling END;
      n.next_sibling := knee;
    END;

    (* update the graph edges *)
    EVAL NEW (GraphVBT.Edge, vertex0 := child, vertex1 := knee).init ();
    EVAL NEW (GraphVBT.Edge, vertex0 := knee, vertex1 := parent).init ();
    AddNode (t, -1, knee);
  END NewEdge;

PROCEDURE UpdateDone (t: T) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR max_depth := 0;   max_h: REAL;  n: Node;  v_scale: REAL;
  BEGIN
    LOCK t.graph.mu DO
      (* find the height of the tree *)
      n := t.nodes;
      WHILE (n # NIL) DO
        IF (n.parent = NIL) AND (NOT n.deleted) THEN
          (* n is the root of a tree *)
          max_depth := MAX (max_depth, ScanNode (t, n, 0));
        END;
        n := n.next;
      END;

      (* compute the vertical scaling factor *)
      v_scale := (Max_V - Min_V) / FLOAT (max_depth + 2);
      t.resize := (v_scale < t.v_scale);
      IF (t.resize) THEN t.v_scale := v_scale END;

      (* place the nodes *)
      n := t.nodes;
      WHILE (n # NIL) DO
        IF (n.parent = NIL) AND (NOT n.deleted) THEN
          (* n is the root of a tree *)
          max_h := 0.0;
          MoveNode (t, n, Min_V, max_h);
        END;
        n := n.next;
      END;
    END;
    t.graph.animate (0.0, 1.0);
    IF DEBUG THEN
      Wr.PutText (Stdio.stdout, "-------\n");
      Wr.Flush (Stdio.stdout);
    END;
  END UpdateDone;

PROCEDURE ScanNode (t: T;  n: Node;  depth: INTEGER): INTEGER =
  VAR x: Node;  max_d := depth;
  BEGIN
    x := n.children;
    WHILE (x # NIL) DO
      max_d := MAX (max_d, ScanNode (t, x, depth + 1));
      x := x.next_sibling;
    END;
    RETURN max_d;
  END ScanNode;

PROCEDURE MoveNode (t: T;  n: Node;  min_v: REAL;  VAR max_h: REAL) =
  VAR w, new_h, h0, h1: REAL;  child: Node;
  BEGIN
    IF (n.kind = Kind.Leaf) THEN
DumpNode (n);
      (* we don't move *)
      new_h := n.x + 0.5 * Width (t, n);
    ELSIF (n.children = NIL) THEN
      (* this is an unattached internal node => take next spot to the right *)
      w := 0.5 * Width (t, n);
      new_h := max_h + w;
      Move (n, new_h, min_v);
      new_h := new_h + w;
DumpNode (n);
    ELSE
DumpNode (n);
      h0 := 1.0;
      h1 := 0.0;
      child := n.children;
      WHILE (child # NIL) DO
        MoveNode (t, child, min_v + t.v_scale, max_h);
        IF (child.x < h0) THEN h0 := child.x END;
        IF (child.x > h1) THEN h1 := child.x END;
        child := child.next_sibling;
      END;
      new_h := (h0 + h1) / 2.0;
      Move (n, new_h, min_v);
      new_h := new_h + 0.5 * Width (t, n);
DumpNode (n);
    END;

    IF (t.resize) AND (n.kind # Kind.Knee) THEN
      n.setSize (R2.T { n.size[0], t.v_scale });
    END;

    IF (new_h > max_h) THEN max_h := new_h END;
  END MoveNode;

PROCEDURE DumpNode (n: Node) =
  <*FATAL Thread.Alerted, Wr.Failure*>
  CONST KName = ARRAY Kind OF TEXT { ": leaf", ": knee", ": node" };
  VAR wr := Stdio.stdout;
  BEGIN
    IF NOT DEBUG THEN RETURN END;
    Wr.PutText (wr, Fmt.Int (n.uid));
    Wr.PutText (wr, KName [n.kind]);
    IF (n.label # NIL) THEN
      Wr.PutText (wr, "        ");
      Wr.PutText (wr, Fmt.Pad (n.label, 8));
    END;
    Wr.PutText (wr, " parent: ");
    Wr.PutText (wr, NodeID (n.parent));
    Wr.PutText (wr, " child: ");
    Wr.PutText (wr, NodeID (n.children));
    Wr.PutText (wr, " sib: ");
    Wr.PutText (wr, NodeID (n.next_sibling));
    Wr.PutText (wr, " (x,y): ");
    Wr.PutText (wr, Fmt.Real (n.x));
    Wr.PutText (wr, " ");
    Wr.PutText (wr, Fmt.Real (n.y));
    Wr.PutText (wr, "\n");
  END DumpNode;

PROCEDURE NodeID (n: Node): TEXT =
  BEGIN
    IF (n = NIL)
      THEN RETURN " * ";
      ELSE RETURN Fmt.Pad (Fmt.Int (n.uid), 3);
    END;
  END NodeID;

PROCEDURE Width (t: T;  n: Node): REAL =
  BEGIN
    IF (n = NIL) OR (n.kind = Kind.Knee) THEN
      RETURN 0.0;
    ELSE
      RETURN (FLOAT (Text.Length (n.label)) + 0.5) / FLOAT (1 + t.length);
    END;
  END Width;

PROCEDURE Scan (t: T;  <*UNUSED*> token: TEXT) =
  VAR n := t.cursor;
  BEGIN
    LOCK t.graph.mu DO
      IF (n > 0) THEN  t.map[n - 1].setColor (ParseColor.Accepted)  END;
      IF (n < t.state.n_tokens) THEN t.map[n].setColor(ParseColor.Current) END;
    END;
    INC (t.cursor);
    t.graph.redisplay ();
  END Scan;

PROCEDURE NoteError (t: T) =
  BEGIN
    LOCK t.graph.mu DO
      t.map [t.cursor].setColor (ParseColor.Error);
    END;
    t.graph.redisplay ();
  END NoteError;

PROCEDURE Push (t: T;  id: INTEGER; <*UNUSED*> tag: TEXT) =
  VAR n: Node;
  BEGIN
    IF (0 <= id) AND (id < NUMBER (t.map^)) THEN
      n := t.map [id];
      IF (n # NIL) THEN
        LOCK t.graph.mu DO
          n.setColor (ParseColor.Active);
        END;
        t.graph.redisplay ();
      END;
    END;
  END Push;

PROCEDURE Pop (t: T;  id: INTEGER) =
  VAR n: Node;
  BEGIN
    IF (0 <= id) AND (id < NUMBER (t.map^)) THEN
      n := t.map [id];
      IF (n # NIL) THEN
        LOCK t.graph.mu DO
          n.setColor (ParseColor.Passive);
        END;
        t.graph.redisplay ();
      END;
    END;
  END Pop;

PROCEDURE New (): View.T =
  VAR g := NEW(GraphVBT.T).init();
  BEGIN
    RETURN NEW (T, graph := g, font := g.font(size := 0.03)).init(g)
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "parse tree", "Parse");
END V_Tree.
