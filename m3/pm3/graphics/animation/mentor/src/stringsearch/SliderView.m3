(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Jan  6 00:01:22 PST 1995 by najork   *)
(*      modified on Thu Feb 11 13:52:04 PST 1993 by steveg   *)
(*      modified on Thu Aug 13 16:48:43 PDT 1992 by guarino  *)
(*      modified on Wed Aug  5 01:42:11 PDT 1992 by broder   *)
(*      modified on Tue Jul 21 06:43:42 PDT 1992 by mhb      *)

MODULE SliderView;

IMPORT Filter, StringSearchViewClass, View, ZeusPanel, ColorName, PaintOp,
       Grid, Text, MG, MGV, Font, ScaleFilter, VBT, ViewsBase, Thread;


CONST
  MaxPatLen  = 12;              (* 16 chars *)
  RestartPos = 4;
  EndLimit   = 4;
  SlideLen   = 8;               (* must be > RestartPos *)
  EllipsisLen = 3;
  Decoration   = 2 * EllipsisLen;              
  MovieLen   = RestartPos + MaxPatLen + SlideLen + EndLimit + Decoration;
  RectSize   = 20.0;

VAR
  SliderFont := Font.FromName(
             ARRAY OF TEXT{"-*-courier-bold-r-*-*-*-180-*-*-*-*-*-*"});

TYPE
  T = StringSearchViewClass.T OBJECT
        grid           : Grid.V;
        pat, str       : TEXT;
        startStr       : INTEGER;   (* First char from str in grid *)
        posPat         : INTEGER;   (* Position of pattern in grid *)
        lasti, lastj   : CARDINAL;  (* Last probe values *)
        m              : CARDINAL;  (* Length of pattern *)
        lMi, lMj, lMlen: CARDINAL;  (* last match i, j, length *)
        endLimit       : CARDINAL   := EndLimit;
        jumpSkip       : BOOLEAN    := FALSE;

        (* a is used to remember complete matches during sliding.  Not
           needed if we go for RestartPos=0.  The lenght of a is
           Length(str)+MovieLen, to allow for space after the last
           redisplay of text. *)
        a: REF ARRAY OF PaintOp.ColorScheme;
      OVERRIDES
        oeSetup             := Setup;
        oeProbe             := Probe;
        oeResult            := Result;
        oePartialMatch      := PartialMatch;
        oePartialMatchClear := PartialMatchClear;
        oeCompleteMatch     := CompleteMatch;
        oeSlideTo           := SlideTo;
      END;

  TopT = T OBJECT
         OVERRIDES
           oeSetup   := TopSetup;
           oeSlideTo := TopSlideTo;
         END;


  OverViewT = T OBJECT
              OVERRIDES
                oeSetup    := OverViewSetup;
                oeKMPSetup := KMPSetup;
              END;

PROCEDURE Color(color: TEXT): PaintOp.T =
  <* FATAL ColorName.NotFound *>
  VAR rgb := ColorName.ToRGB(color);
  BEGIN
    RETURN PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END Color;

VAR
  (* Color options; actually constants *)
  TrueCS := PaintOp.MakeColorScheme(
              fg := PaintOp.Fg,
              bg := Color(ViewsBase.TrueC));
  FalseCS := PaintOp.MakeColorScheme(fg := PaintOp.Fg,
                                          bg := Color(ViewsBase.FalseC));
  PartialCS := PaintOp.MakeColorScheme(
                 fg := PaintOp.Fg,
                 bg := Color(ViewsBase.PartialC));
  CompleteCS := PaintOp.MakeColorScheme(
                  fg := PaintOp.Fg,
                  bg := Color(ViewsBase.CompleteC));

  ProbeCS := PaintOp.MakeColorScheme(
               fg := PaintOp.Bg, bg := PaintOp.Fg);
  ClearCS := PaintOp.bgFg;


(* Setup might be called more than once during the run!  (KMP) *)

PROCEDURE Setup (self: T; p, s: TEXT) RAISES {Thread.Alerted} =
  VAR
    m := Text.Length(p);
    n := Text.Length(s);
  BEGIN
    self.pat := p;
    self.str := s;
    IF m > MaxPatLen THEN
      ZeusPanel.ReportError("Pattern too long for Slider View");
      RETURN;
    ELSE
      self.m := m;
    END;
    self.a := NEW(REF ARRAY OF PaintOp.ColorScheme, n + MovieLen);
    FOR j := 0 TO n + MovieLen - 1 DO self.a[j] := ClearCS END;
    LOCK self.grid.mu DO
      FOR j := EllipsisLen TO MovieLen - 1 - EllipsisLen DO
        self.grid.a[0, j].setColor(self.grid, ClearCS);
        self.grid.a[0, j].setLabel(
          self.grid, Text.Sub(self.str, j - EllipsisLen, 1));
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;

      (* start left-justified *)
      FOR j := 0 TO EllipsisLen - 1 DO
        self.grid.a[0, j].setColor(self.grid, ClearCS);
        self.grid.a[0, j].setVisible(self.grid, 1.0);
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;
      (* but maybe not right-justified *)
      FOR j := MovieLen - EllipsisLen TO MovieLen - 1 DO
        self.grid.a[0, j].setColor(self.grid, ClearCS);
        self.grid.a[0, j].setVisible(self.grid, 1.0);
        IF MovieLen - 1 - EllipsisLen < n THEN
          self.grid.a[0, j].setLabel(self.grid, ".");
        END;
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;

      FOR j := EllipsisLen TO m - 1 + EllipsisLen DO
        self.grid.a[1, j].setColor(self.grid, ClearCS);
        self.grid.a[1, j].setLabel(
          self.grid, Text.Sub(self.pat, j - EllipsisLen, 1));
        self.grid.a[1, j].setVisible(self.grid, 1.0);
      END;
      self.posPat := 0;
      self.startStr := 0;
      self.lasti := 0;
      self.lastj := 0;
      self.lMi := 0;
      self.lMj := 0;
      self.lMlen := 0;
    END;
    MGV.Animation(self.grid);
  END Setup;

PROCEDURE TopSetup (self: T; p, s: TEXT) RAISES {Thread.Alerted} =
  VAR
    m := Text.Length(p);
    n := Text.Length(s);
  BEGIN
    self.pat := p;
    self.str := s;
    IF m > MaxPatLen THEN
      ZeusPanel.ReportError("Pattern too long for Slider View");
      RETURN;
    ELSE
      self.m := m;
    END;
    self.a := NEW(REF ARRAY OF PaintOp.ColorScheme, n + MovieLen);
    FOR j := 0 TO n + MovieLen - 1 DO self.a[j] := ClearCS END;
    self.posPat := (MovieLen - m) DIV 2;
    self.startStr := -self.posPat;
    LOCK self.grid.mu DO
      FOR j := EllipsisLen TO MovieLen - 1 - EllipsisLen DO
        self.grid.a[0, j].setColor(self.grid, ClearCS);
        IF j >= self.posPat + EllipsisLen THEN
          self.grid.a[0, j].setLabel(
            self.grid, Text.Sub(self.str, j - self.posPat - EllipsisLen, 1));
        ELSE
          self.grid.a[0, j].setLabel(self.grid, "");
        END;
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;
      (* start left-justified *)
      FOR j := 0 TO EllipsisLen - 1 DO
        self.grid.a[0, j].setColor(self.grid, ClearCS);
        self.grid.a[0, j].setVisible(self.grid, 1.0);
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;
      (* but maybe not right-justified *)
      FOR j := MovieLen - EllipsisLen TO MovieLen - 1 DO
        self.grid.a[0, j].setColor(self.grid, ClearCS);
        self.grid.a[0, j].setVisible(self.grid, 1.0);
        IF self.startStr + MovieLen  - Decoration < n THEN
          self.grid.a[0, j].setLabel(self.grid, ".");
        END;
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;

      FOR j := EllipsisLen TO m - 1  + EllipsisLen DO
        self.grid.a[1, self.posPat + j].setColor(self.grid, ClearCS);
        self.grid.a[1, self.posPat + j].setLabel(
          self.grid, Text.Sub(self.pat, j - EllipsisLen, 1));
        self.grid.a[1, self.posPat + j].setVisible(self.grid, 1.0);
      END;
      self.lasti := 0;
      self.lastj := 0;
      self.lMi := 0;
      self.lMj := 0;
      self.lMlen := 0;
    END;
    MGV.Animation(self.grid);
  END TopSetup;

PROCEDURE OverViewSetup (self: T; p, s: TEXT) RAISES {Thread.Alerted} =
  VAR
    m    := Text.Length(p);
    n    := Text.Length(s);
    size := n + m + 1 + Decoration;
    g := NEW(Grid.V, doubleBuffer := FALSE).init(
           2, size, RectSize, RectSize);
  BEGIN
    self.pat := p;
    self.str := s;
    IF m > MaxPatLen THEN
      ZeusPanel.ReportError("Pattern too long for Slider View");
      RETURN;
    ELSE
      self.m := m;
    END;
    LOCK VBT.mu DO
      WITH scale = NEW(ScaleFilter.T).init(g) DO
        ScaleFilter.AutoScale(scale);
        EVAL Filter.Replace(self, scale);
      END;
    END;
    self.grid := g;
    self.a := NEW(REF ARRAY OF PaintOp.ColorScheme, 2 * n);
    FOR j := FIRST(self.a^) TO LAST(self.a^) DO self.a[j] := ClearCS END;
    LOCK self.grid.mu DO
      FOR j := FIRST(self.grid.a[0]) + EllipsisLen
          TO LAST(self.grid.a[0]) - EllipsisLen DO
        self.grid.a[0, j].setFont(self.grid, SliderFont);
        self.grid.a[0, j].setColor(self.grid, ClearCS);
        self.grid.a[0, j].setLabel(
          self.grid, Text.Sub(self.str, j - EllipsisLen, 1));
        self.grid.a[1, j].setFont(self.grid, SliderFont);
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;
      FOR j := 0 TO EllipsisLen - 1 DO
        self.grid.a[0, j].setVisible(self.grid, 0.0);
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;
      FOR j := LAST(self.grid.a[0]) - EllipsisLen
          TO LAST(self.grid.a[0]) DO
        self.grid.a[0, j].setVisible(self.grid, 0.0);
        self.grid.a[1, j].setVisible(self.grid, 0.0);
      END;
      FOR j := EllipsisLen TO m - 1 + EllipsisLen DO
        self.grid.a[1, j].setColor(self.grid, ClearCS);
        self.grid.a[1, j].setLabel(
          self.grid, Text.Sub(self.pat, j - EllipsisLen, 1));
        self.grid.a[1, j].setVisible(self.grid, 1.0);
      END;
      self.endLimit := 0;
      self.posPat := 0;
      self.startStr := 0;
      self.lasti := 0;
      self.lastj := 0;
      self.lMi := 0;
      self.lMj := 0;
      self.lMlen := 0;
      self.jumpSkip := TRUE;
    END;
    MGV.Animation(self.grid);
  END OverViewSetup;

PROCEDURE KMPSetup (self: T; p: TEXT) RAISES {Thread.Alerted} =
BEGIN
  OverViewSetup(self, p, p);
END KMPSetup;

PROCEDURE Probe (self: T; i, j: CARDINAL) RAISES {Thread.Alerted} =
  BEGIN
    LOCK self.grid.mu DO
      self.grid.a[0, j - self.startStr + EllipsisLen].setColor(
        self.grid, ProbeCS);
      self.grid.a[1, i + self.posPat + EllipsisLen].setColor(
        self.grid, ProbeCS);
      self.lasti := i;
      self.lastj := j;
    END;
    MGV.Animation(self.grid);
  END Probe;

PROCEDURE Result (self: T; r: BOOLEAN) RAISES {Thread.Alerted} =
  BEGIN
    LOCK self.grid.mu DO
      IF r THEN
        self.grid.a[0, self.lastj - self.startStr + EllipsisLen].setColor(
          self.grid, TrueCS);
        self.grid.a[1, self.lasti + self.posPat + EllipsisLen].setColor(
          self.grid, TrueCS);
      ELSE
        self.grid.a[0, self.lastj - self.startStr + EllipsisLen].setColor(
          self.grid, FalseCS);
        self.grid.a[1, self.lasti + self.posPat + EllipsisLen].setColor(
          self.grid, FalseCS);
      END;
    END;
    MGV.Animation(self.grid);
  END Result;


PROCEDURE PartialMatch (self: T; i, j, len: CARDINAL) RAISES {Thread.Alerted} =
  BEGIN
    self.lMi := i;
    self.lMj := j;
    self.lMlen := len;
    LOCK self.grid.mu DO
      self.grid.a[0, self.lastj - self.startStr + EllipsisLen].setColor(
        self.grid, ClearCS);
      self.grid.a[1, self.lasti + self.posPat + EllipsisLen].setColor(
        self.grid, ClearCS);
      FOR k := 0 TO len - 1 DO
        self.grid.a[0, j - self.startStr + k + EllipsisLen].setColor(
          self.grid, PartialCS);
        self.grid.a[1, i + self.posPat + k + EllipsisLen].setColor(
          self.grid, PartialCS);
      END;
    END;
    MGV.Animation(self.grid);
  END PartialMatch;

PROCEDURE PartialMatchClear (self: T) RAISES {Thread.Alerted} =
  BEGIN
    LOCK self.grid.mu DO
      self.grid.a[0, self.lastj - self.startStr + EllipsisLen].setColor(
        self.grid, ClearCS);
      self.grid.a[1, self.lasti + self.posPat + EllipsisLen].setColor(
        self.grid, ClearCS);
      FOR k := 0 TO self.lMlen - 1 DO
        self.grid.a[0, self.lMj - self.startStr + k + EllipsisLen].setColor(
          self.grid, ClearCS);

        self.grid.a[1, self.lMi + self.posPat + k + EllipsisLen].setColor(
          self.grid, ClearCS);
      END;
      self.lMlen := 0;
    END;
    MGV.Animation(self.grid);
  END PartialMatchClear;

PROCEDURE CompleteMatch (self: T; j: CARDINAL) RAISES {Thread.Alerted} =
  BEGIN
    LOCK self.grid.mu DO
      (* Clear the old probe *)
      self.grid.a[0, self.lastj - self.startStr + EllipsisLen].setColor(
        self.grid, ClearCS);
      self.grid.a[1, self.lasti + self.posPat + EllipsisLen].setColor(
        self.grid, ClearCS);

      (* Clear the old match *)
      FOR k := 0 TO self.lMlen - 1 DO
        self.grid.a[0, self.lMj - self.startStr + k + EllipsisLen].setColor(
          self.grid, ClearCS);
        self.grid.a[1, self.lMi + self.posPat + k + EllipsisLen].setColor(
          self.grid, ClearCS);
      END;
      self.lMlen := 0;

      (* Set the complete match mark *)
      self.grid.a[0, j - self.startStr + EllipsisLen].setColor(self.grid, CompleteCS);
      self.a[j] := CompleteCS;
    END;
    MGV.Animation(self.grid);
  END CompleteMatch;


PROCEDURE TopSlideTo (self: T; newPos: CARDINAL) RAISES {Thread.Alerted} =
  VAR
    skip           := newPos - self.startStr - self.posPat;
    duration: REAL;

  BEGIN
    IF skip = 0 THEN RETURN END;
    duration := 1.0 / FLOAT(skip);
    (* Clean old pattern *)

    IF skip > 0 THEN
      FOR i := 0 TO skip - 1 DO
        (* Redraw text *)
        INC(self.startStr);
        LOCK self.grid.mu DO
          FOR j := EllipsisLen TO MovieLen - 1 - EllipsisLen DO
            IF self.startStr  - EllipsisLen + j >= 0 THEN
              self.grid.a[0, j].setLabel(
                self.grid,
                Text.Sub(self.str, self.startStr - EllipsisLen + j, 1));
              self.grid.a[0, j].setColor(
                self.grid, self.a[self.startStr - EllipsisLen + j]);
            ELSE
              self.grid.a[0, j].setLabel(self.grid, "");
              self.grid.a[0, j].setColor(self.grid, ClearCS);
            END;
          END;
          IF self.startStr > 0 THEN
            FOR j := 0 TO EllipsisLen - 1 DO
              self.grid.a[0, j].setLabel(self.grid, ".");
            END;
          ELSE
            FOR j := 0 TO EllipsisLen - 1 DO
              self.grid.a[0, j].setLabel(self.grid, "");
            END;
          END;
          IF self.startStr + MovieLen - Decoration < Text.Length(self.str) THEN
            FOR j := MovieLen - EllipsisLen TO MovieLen - 1 DO
              self.grid.a[0, j].setLabel(self.grid, ".");
            END;
          ELSE
            FOR j := MovieLen - EllipsisLen TO MovieLen - 1 DO
              self.grid.a[0, j].setLabel(self.grid, "");
            END;
          END;
        END;
        MGV.Animation(self.grid, duration);
      END;
    ELSE
      ZeusPanel.ReportError("Trying to slide backwards");
    END;
  END TopSlideTo;


PROCEDURE SlideTo (self: T; newPos: CARDINAL) RAISES {Thread.Alerted} =
  VAR
    skip           := newPos - self.startStr - self.posPat;
    duration: REAL;

  BEGIN
    IF skip = 0 THEN RETURN END;
    duration := 0.5 / FLOAT(skip);

    IF skip > 0 THEN
      IF self.posPat + self.m + skip
           > NUMBER(self.grid.a[0]) - self.endLimit - EllipsisLen THEN
        IF self.jumpSkip THEN
          Slide(self, 1.0, skip);
        ELSE
          FOR i := 0 TO skip - 1 DO Slide(self, duration, 1); END;
        END;
      END;
      IF self.startStr > 0 THEN
        FOR j := 0 TO EllipsisLen - 1 DO
          self.grid.a[0, j].setLabel(self.grid, ".");
        END;
      ELSE
        FOR j := 0 TO EllipsisLen - 1 DO
          self.grid.a[0, j].setLabel(self.grid, "");
        END;
      END;
      IF self.startStr + NUMBER(self.grid.a[0]) - Decoration
           < Text.Length(self.str) THEN
        FOR j := NUMBER(self.grid.a[0]) - EllipsisLen TO LAST(self.grid.a[0]) DO
          self.grid.a[0, j].setLabel(self.grid, ".");
        END;
      ELSE
        FOR j := NUMBER(self.grid.a[0]) - EllipsisLen TO LAST(self.grid.a[0]) DO
          self.grid.a[0, j].setLabel(self.grid, "");
        END;
      END;
      (* Move Pattern *)
      FOR i := 0 TO skip - 1 DO
        LOCK self.grid.mu DO
          self.grid.a[1, self.posPat + EllipsisLen].setVisible(
            self.grid, 0.0);
          INC(self.posPat);
          (* Now redraw pattern *)
          FOR j := MAX(0, -self.posPat) TO self.m - 1 DO
            self.grid.a[1, self.posPat + j + EllipsisLen].setVisible(
              self.grid, 1.0);
            self.grid.a[1, self.posPat + j + EllipsisLen].setColor(
              self.grid, ClearCS);
            self.grid.a[1, self.posPat + j + EllipsisLen].setLabel(
              self.grid, Text.Sub(self.pat, j, 1));
          END;
        END;
        MGV.Animation(self.grid, duration);
      END;
    ELSE
      ZeusPanel.ReportError("Trying to slide backwards");
    END;
  END SlideTo;

PROCEDURE Slide (self: T; duration: REAL; howMany: INTEGER) RAISES {Thread.Alerted} =
  BEGIN
    LOCK self.grid.mu DO
      FOR j := 0 TO self.m - 1 DO
        self.grid.a[1, self.posPat + j + EllipsisLen].setVisible(
          self.grid, 0.0);
      END;

      (* Redraw text *)
      INC(self.startStr, howMany);
      FOR j := EllipsisLen TO NUMBER(self.grid.a[0]) - 1 - EllipsisLen DO
        self.grid.a[0, j].setLabel(
          self.grid, Text.Sub(self.str, self.startStr - EllipsisLen + j, 1));
        self.grid.a[0, j].setColor(
          self.grid, self.a[self.startStr  - EllipsisLen + j]);
      END;
      (* Move pattern *)
      DEC(self.posPat, howMany);

      (* Now redraw pattern *)
      FOR j := MAX(0, -self.posPat) TO self.m - 1 DO
        self.grid.a[1, self.posPat + j + EllipsisLen].setVisible(
          self.grid, 1.0);
        self.grid.a[1, self.posPat + j + EllipsisLen].setColor(
          self.grid, ClearCS);
        self.grid.a[1, self.posPat + j + EllipsisLen].setLabel(
          self.grid, Text.Sub(self.pat, j, 1));
      END;
    END;
    MGV.Animation(self.grid, duration);
  END Slide;

PROCEDURE New (): View.T =
  VAR g := NEW(Grid.V).init(2, MovieLen, RectSize, RectSize);
  BEGIN
    FOR i := 0 TO MovieLen - 1 DO
      g.a[0, i].setFont(g, SliderFont);
      g.a[1, i].setFont(g, SliderFont);
      g.a[1, i].setVisible(g, 0.0);
    END;
    FOR j := 0 TO EllipsisLen - 1 DO g.a[0, j].setWeight(g, 0.0); END;
    FOR j := MovieLen - EllipsisLen TO MovieLen - 1 DO
      g.a[0, j].setWeight(g, 0.0);
    END;
    WITH scale = NEW(ScaleFilter.T).init(g) DO
      ScaleFilter.AutoScale(scale);
      RETURN NEW(T, grid := g).init(scale);
    END;
  END New;


PROCEDURE New1 (): View.T =
  VAR g := NEW(Grid.V).init(2, MovieLen, RectSize, RectSize);
  BEGIN
    FOR i := 0 TO MovieLen - 1 DO
      g.a[0, i].setFont(g, SliderFont);
      g.a[1, i].setFont(g, SliderFont);
      g.a[1, i].setVisible(g, 0.0);
    END;
    FOR j := 0 TO EllipsisLen - 1 DO g.a[0, j].setWeight(g, 0.0); END;
    FOR j := MovieLen - EllipsisLen TO MovieLen - 1 DO
      g.a[0, j].setWeight(g, 0.0);
    END;
    WITH scale = NEW(ScaleFilter.T).init(g) DO
      ScaleFilter.AutoScale(scale);
      RETURN NEW(TopT, grid := g).init(scale);
    END;
  END New1;

PROCEDURE New2 (): View.T =
  VAR
    g := NEW(Grid.V, doubleBuffer := FALSE).init(
           2, MovieLen, RectSize, RectSize);
  BEGIN
    FOR i := 0 TO MovieLen - 1 DO
      g.a[0, i].setVisible(g, 0.0);
      g.a[1, i].setVisible(g, 0.0);
    END;
    WITH scale = NEW(ScaleFilter.T).init(g) DO
      ScaleFilter.AutoScale(scale);
      RETURN NEW(OverViewT, grid := g).init(scale);
    END;
  END New2;

BEGIN
  ZeusPanel.RegisterView(New, "Slider", "StringSearch");
  ZeusPanel.RegisterView(New1, "TopSlider", "StringSearch");
  ZeusPanel.RegisterView(New2, "OverView", "StringSearch");
END SliderView.
