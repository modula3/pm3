(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Jan  6 00:15:07 PST 1995 by najork   *)
(*      modified on Tue Mar 23 09:44:24 PST 1993 by mhb      *)
(*      modified on Wed Jan  6 16:40:02 PST 1993 by steveg   *)
(*      modified on Sat Oct 24 03:23:02 PDT 1992 by broder   *)
(*      modified on Thu Jul 23 18:07:52 PDT 1992 by guario   *)

(* This view ``knows'' that it will be used only by KMP and
takes some shortcuts.   See comments below. *)

MODULE AutoView;

IMPORT StringSearchViewClass, View, ZeusPanel, ColorName, PaintOp, Thread,
       GraphVBT, Text, Fmt, R2, ViewsBase, FormsVBT, KMP, ZeusClass;

<* FATAL FormsVBT.Unimplemented *>

CONST
  VDiam   = 0.25;              (* Diameter of vertex *)
  VBorder = 0.03;              (* Width of border *)
  EWidth  = 0.03;              (* Width of edges *)
  Forward = ARRAY [0 .. 1] OF BOOLEAN{FALSE, TRUE};
  LSize   = 0.2;              (* Size of label *)
  HDiam   = 0.4;              (* Size of highligh. Must be > VBorder *)
  E0H     = -2.0 / 3.0;       (* Control pts (W units) for 0,0 edge *)
  E0V     = 4.0 / 3.0;

TYPE
  T = StringSearchViewClass.T OBJECT
        fv   : FormsVBT.T;
        graph: GraphVBT.T;
        p    : TEXT;            (* pattern *)
        m    : CARDINAL;        (* Length of pattern *)
        lasti: CARDINAL     := 0; (* Last probe value *)
        lMlen: CARDINAL     := 0; (* last match length *)
        v: REF ARRAY OF GraphVBT.Vertex;  (* Vertices *)
        l: REF ARRAY OF GraphVBT.Vertex;  (* Labels *)
        h: GraphVBT.Vertex;               (* Actually the highlight *)
        e: REF ARRAY OF ARRAY BOOLEAN OF GraphVBT.Edge;
        setupNotDone := TRUE;   (* Ignore events if not setup properly *)
        (* Parameters for tuning the Bezier curves *)
        py     := 0.25;
        px     := 0.25;         (* Bezier parameters *)
        cPSize := 0.0;          (* If not zero, control pts are visible *)
        ControlFont: GraphVBT.WorldFont;
      OVERRIDES
        reactivity          := Reactivity;
        startrun            := Startrun;
        isCompat            := IsCompat;
        oeKMPSetup          := KMPSetup;
        oeAddEdge           := AddEdge;
        oeProbe             := Probe;
        oeResult            := Result;
        oePartialMatch      := PartialMatch;
        oePartialMatchClear := PartialMatchClear;
        oeCompleteMatch     := CompleteMatch;
        oeSetup             := Setup;
      END;

VAR
  (* Color options; actually constants *)
  TrueC     := Color(ViewsBase.TrueC);
  FalseC    := Color(ViewsBase.FalseC);
  PartialC  := Color(ViewsBase.PartialC);
  CompleteC := Color(ViewsBase.CompleteC);
  StateC    := Color(ViewsBase.StateC);

PROCEDURE Color(color: TEXT): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR rgb := ColorName.ToRGB(color);
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END Color;

PROCEDURE Startrun(self:T) RAISES {}=
  BEGIN
    self.setupNotDone := TRUE;
  END Startrun;

PROCEDURE Reactivity(<* UNUSED *> self:T; <* UNUSED *> on: BOOLEAN) =
  BEGIN
  END Reactivity;


PROCEDURE IsCompat (<* UNUSED *> self : T; alg : ZeusClass.T): BOOLEAN =
  BEGIN
    RETURN ISTYPE (alg, KMP.T);
  END IsCompat;





PROCEDURE KMPSetup (self: T; p: TEXT) =

  VAR
    m := Text.Length(p);
    g := NEW(GraphVBT.T,
             world := GraphVBT.WorldRectangle{
                        w := -1.0, e := FLOAT(m + 1), n := 1.0, s := -1.0},
             aspect := 2.0 / FLOAT(m + 2)).init();
    LabelFont := g.font(family := "courier", weight := "bold",
                        slant := GraphVBT.Slant.Roman, size := 0.3);
    VertexFont := LabelFont;
    ControlFont := g.font(family := "clean", weight := "bold",
                          slant := GraphVBT.Slant.Roman, size := 0.3);
  BEGIN
    (* Put g in form *)
    self.ControlFont := ControlFont;
    TRY
      FormsVBT.PutGeneric(self.fv, "g", g);
      self.px := FLOAT(FormsVBT.GetInteger(self.fv, "px")) / 100.0;
      self.py := FLOAT(FormsVBT.GetInteger(self.fv, "py")) / 100.0;
      self.cPSize := FLOAT(FormsVBT.GetInteger(self.fv, "cpsize"));
    EXCEPT
      FormsVBT.Error (error) =>
        ZeusPanel.ReportError(
          "Can't add graph to form - FormsVBT.Error " & error);
        RETURN;
    END;

    self.graph := g;
    self.m := m;

    (* Make vertices *)
    self.v := NEW(REF ARRAY OF GraphVBT.Vertex, m + 1);

    FOR i := 0 TO m DO
      self.v[i] :=
        NEW(GraphVBT.Vertex, graph := g, pos := R2.T{FLOAT(i), 0.0},
            shape := GraphVBT.VertexShape.Ellipse,
            size := R2.T{VDiam, VDiam}, color := PaintOp.Bg,
            label := Fmt.Int(i), font := VertexFont,
            fontColor := PaintOp.Fg, border := VBorder).init()
    END;

    (* Make edges *)
    self.e := NEW(REF ARRAY OF ARRAY BOOLEAN OF GraphVBT.Edge, m + 1);
    self.l := NEW(REF ARRAY OF GraphVBT.Vertex, m + 1);

    FOR i := 0 TO m - 1 DO
      self.e[i, TRUE] :=
        NEW(GraphVBT.Edge, vertex0 := self.v[i], vertex1 := self.v[i + 1],
            width := EWidth, arrow := Forward).init();
      (* Label *)
      self.l[i] :=
        NEW(GraphVBT.Vertex, graph := g, pos := R2.T{FLOAT(i) + 0.5, 0.0},
            shape := GraphVBT.VertexShape.Rectangle,
            size := R2.T{LSize, LSize}, color := PaintOp.Bg,
            label := Text.Sub(p, i, 1), font := LabelFont,
            fontColor := PaintOp.Fg, border := 0.0).init()
    END;

    (* Special highlight vertex *)

    self.h := NEW(GraphVBT.Vertex, graph := g, pos := R2.T{0.0, 0.0},
                  shape := GraphVBT.VertexShape.Ellipse,
                  size := R2.T{HDiam, HDiam}, color := StateC).init();
    self.h.toBack();

    (* Special edge from 0 to 0 *)

    self.e[0, FALSE] :=
      NEW(GraphVBT.Edge, vertex0 := self.v[0], vertex1 := self.v[0],
          control0 := NEW(GraphVBT.Vertex, graph := g,
                          size := R2.T{self.cPSize, self.cPSize},
                          font := ControlFont, label := "000",
                          pos := R2.T{E0H, E0V}).init(),
          control1 := NEW(GraphVBT.Vertex, graph := g,
                          size := R2.T{self.cPSize, self.cPSize},
                          font := ControlFont, label := "001",
                          pos := R2.T{E0H, -E0V}).init(), width := EWidth,
          arrow := Forward).init();
    self.setupNotDone := FALSE;
  END KMPSetup;

PROCEDURE AddEdge (self: T; f, t: CARDINAL) =
  (* Uses the fact that the locations of the verts is known *)
  VAR
    d       := FLOAT(t - f);
    h: REAL;
  BEGIN
    IF self.setupNotDone THEN RETURN END;
    IF t = 0 THEN               (* Backward to 0 - go under x axis *)
      h := self.py * d;
    ELSE
      h := -self.py * d;
    END;

    self.e[f, FALSE] :=
      NEW(GraphVBT.Edge, vertex0 := self.v[f], vertex1 := self.v[t],
          control0 := NEW(GraphVBT.Vertex, graph := self.graph,
                          size := R2.T{self.cPSize, self.cPSize},
                          label := Fmt.Int(f) & Fmt.Int(t) & "0",
                          font := self.ControlFont,
                          pos := R2.T{FLOAT(f) + self.px * d, h}).init(),
          control1 := NEW(GraphVBT.Vertex, graph := self.graph,
                          size := R2.T{self.cPSize, self.cPSize},
                          label := Fmt.Int(f) & Fmt.Int(t) & "1",
                          font := self.ControlFont,
                          pos := R2.T{FLOAT(t) - self.px * d, h}).init(),
          width := EWidth, arrow := Forward).init();
    IF f = self.m THEN (* What a crock!!!! *)
      self.e[f, FALSE].setColor(CompleteC);
    END;
    self.graph.redisplay();
  END AddEdge;

PROCEDURE Setup (self: T; <* UNUSED *> p, s: TEXT) =
  BEGIN
    self.h.move(self.v[0].pos);
    self.h.toBack();
    self.graph.redisplay();
  END Setup;

PROCEDURE Probe (self: T; i: CARDINAL; <* UNUSED *> j: CARDINAL) =
  BEGIN
    IF self.setupNotDone THEN RETURN END;
    WITH v = self.v[i] DO
      v.setColor(PaintOp.Fg);
      v.setFontColor(PaintOp.Bg);
    END;
    self.lasti := i;
    self.graph.redisplay();
  END Probe;


PROCEDURE Result (self: T; r: BOOLEAN) RAISES {Thread.Alerted} =
  BEGIN
    IF self.setupNotDone THEN RETURN END;
    WITH v = self.v[self.lasti] DO
      v.setColor(PaintOp.Bg);
      v.setFontColor(PaintOp.Fg);
    END;
    IF r THEN
      self.e[self.lasti, TRUE].setColor(TrueC);
    ELSE
      self.e[self.lasti, FALSE].setColor(FalseC);
    END;
    self.graph.redisplay();
    MoveState(self, self.e[self.lasti, r]);
  END Result;


(* All partial matches are from 0 - only the length matters *)

PROCEDURE PartialMatch (self: T; 
                        <* UNUSED *> i, j: CARDINAL;
                        len: CARDINAL) =
  BEGIN
    IF self.setupNotDone THEN RETURN END;
    self.lMlen := len;
    self.e[self.lasti, TRUE].setColor(PaintOp.Fg);
    self.e[self.lasti, FALSE].setColor(PaintOp.Fg);
    FOR k := 0 TO len - 1 DO
      self.e[k, TRUE].setColor(PartialC);
      self.l[k].setColor(PartialC);
    END;
    self.graph.redisplay();
  END PartialMatch;

PROCEDURE PartialMatchClear (self: T) =
 BEGIN
    IF self.setupNotDone THEN RETURN END;
    self.e[self.lasti, TRUE].setColor(PaintOp.Fg);
    self.e[self.lasti, FALSE].setColor(PaintOp.Fg);
    FOR k := 0 TO self.lMlen - 1 DO
      self.e[k, TRUE].setColor(PaintOp.Fg);
      self.l[k].setColor(PaintOp.Bg);
    END;
    self.lMlen := 0;
    self.graph.redisplay();
  END PartialMatchClear;

PROCEDURE CompleteMatch (self: T; <* UNUSED *> j: CARDINAL) 
    RAISES {Thread.Alerted} =
  BEGIN
    IF self.setupNotDone THEN RETURN END;
    self.e[self.lasti, TRUE].setColor(PaintOp.Fg);
    self.e[self.lasti, FALSE].setColor(PaintOp.Fg);
    FOR k := 0 TO self.lMlen - 1 DO
      self.e[k, TRUE].setColor(PaintOp.Fg);
      self.l[k].setColor(PaintOp.Bg);
    END;
    self.lMlen := 0;
    self.v[self.m].setColor(CompleteC);
    self.graph.redisplay();
    MoveState(self, self.e[self.m, FALSE]); (* Last state has one false
                                               edge *)
    self.v[self.m].setColor(PaintOp.Bg);
    self.graph.redisplay();
  END CompleteMatch;

(* Animation stuff *)

TYPE
  EdgePath = GraphVBT.AnimationPath OBJECT
               e: GraphVBT.Edge
             OVERRIDES
               pos := EdgePos;
             END;  

PROCEDURE EdgePos (self: EdgePath; t: REAL): R2.T RAISES {} =
  VAR
    p0, p1, p2, p3: R2.T;
  BEGIN
    IF (self.e.control0 = NIL) OR (self.e.control1 = NIL) THEN
      p0 := self.e.vertex0.pos;
      p1 := self.e.vertex1.pos;
      RETURN R2.Mix(p0, 1.0 - t, p1, t);
    ELSE
      p0 := self.e.vertex0.pos;
      p1 := self.e.control0.pos;
      p2 := self.e.control1.pos;
      p3 := self.e.vertex1.pos;
      RETURN R2.T{Cubic(p0[0], p1[0], p2[0], p3[0], t),
                  Cubic(p0[1], p1[1], p2[1], p3[1], t)};
    END;
  END EdgePos;



PROCEDURE MoveState (self: T; edge: GraphVBT.Edge) RAISES {Thread.Alerted} =
  BEGIN
    IF edge = NIL THEN
      ZeusPanel.ReportError("NIL edge in MoveState");
      RETURN;
    END; 
    self.h.move (edge.vertex1.pos, TRUE, path := NEW (EdgePath, e := edge));
    self.h.toBack();
    self.graph.animate(0.0, 1.0);
  END MoveState;



PROCEDURE Cubic (c0, c1, c2, c3: REAL; t: REAL): REAL =
  BEGIN
    RETURN c0 + t * (3.0 * (c1 - c0)
                       + t * (3.0 * ((c2 - c1) - (c1 - c0))
                                + t * ((c3 - c0) - 3.0 * (c2 - c1))));
  END Cubic;


PROCEDURE New (): View.T =
  VAR v := ZeusPanel.NewForm("stringsearchautoview.fv");
  BEGIN
    RETURN NEW(T, fv := v).init(v);
  END New;


BEGIN
  ZeusPanel.RegisterView(New, "KMP Automaton", "StringSearch");
END AutoView.

