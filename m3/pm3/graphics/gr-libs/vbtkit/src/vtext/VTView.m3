(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 30 15:17:05 PST 1995 by kalsow                   *)
(*      modified on Fri May 14 16:59:06 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 20:53:40 PDT 1992 by muller                   *)
(*      modified on Wed Mar 25 23:06:56 1992 by steveg                       *)
(*      modified on Fri Mar 20 10:37:36 PST 1992 by jdd                      *)
(*      modified on Thu Jul 11 16:02:24 PDT 1991 by mhb                      *)
(*      modified on Thu Jan 31 15:26:52 PST 1991 by chan                     *)
(*      modified on Tue May 15 17:30:13 PDT 1990 by mcjones                  *)

MODULE VTView;

IMPORT Axis, Font, Rd, Rect, RefList, RefListSort, PaintOp, Palette, Point,
       Pts, ScrnFont, Thread, VBT, VTDef, VTVirtual, VTReal,
       VTCaret;

TYPE
  Pixels = VTDef.Pixels;
  RealLines = VTDef.RealLines;
  VirtualLines = VTDef.VirtualLines;
  VScreenFont = VTDef.VScreenFont;

PROCEDURE New (         vt      : T;
                        vbt     : VBT.T;
               READONLY full    : Rect.T;
               READONLY vOptions: VOptions;
                        start   : I         ): View
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    WITH leading = Pts.ToScreenPixels (
                     vbt, vOptions.leadingPts, Axis.T.Ver),
         vScreenFont = MakeVScreenFont (vOptions.vFontxxx, vbt, leading),
         box         = vScreenFont.vScreenFont.box,
         view = NEW (View, vt := vt, vbt := vbt, vOptions := vOptions,
                     vScreenFont := vScreenFont,
                     lineSpacing := box.south - box.north + leading) DO
      view.real.line := NEW (RealLines, 1);
      view.real.blocks.block := NEW (VTDef.BlockArray, 1);
      view.virtual.line := NEW (VirtualLines, 1);
      view.newVirtual.line := NEW (VirtualLines, 1);
      SetLocation (view, full, full);
      VTReal.Init (view);
      VTVirtual.Init (view, start);
      LOCK vt.caret.mutex DO
        VTCaret.InitInView (view);
        view.next := vt.views;
        view.previous := NIL;
        IF vt.views # NIL THEN vt.views.previous := view; END;
        vt.views := view;
      END;
      RETURN view
    END
  END New;

VAR
  vFontList      : RefList.T := NIL;
  vScreenFontList: RefList.T := NIL;
  vFontMutex              := NEW(MUTEX);
  (* vFontQ: ObjectCleanUp.Q; *)
  timeToCleanup := NEW(Thread.Condition);
  handouts      := 0;

CONST
  handoutMax = 100;             (* After 100 handouts, call cleanup *)
  handoutMin = 10;              (* vFonts that haven't been handed out at
                                   least 10 times get removed from the
                                   cache. *)

PROCEDURE MakeVFont (         font     : Font.T;
                     READONLY printable: SET OF CHAR;
                              whiteTabs: BOOLEAN      ): VFont =
  VAR vL: RefList.T;
  BEGIN
    LOCK vFontMutex DO
      vL := vFontList;
      WHILE vL # NIL DO
        WITH v = NARROW (vL.head, VFont) DO
          IF v.vFont.font.fnt = font.fnt AND v.vFont.printable = printable
               AND v.vFont.whiteTabs = whiteTabs THEN
            INC (v.handedOut);
            INC (handouts);
            IF handouts > handoutMax THEN
              Thread.Signal (timeToCleanup);
              handouts := 0
            END;
            RETURN v
          END
        END;
        vL := vL.tail;
      END;
      WITH vFont = NEW (VFont, handedOut := 0) DO
        vFont.vFont.font := font;
        vFont.vFont.printable := printable;
        vFont.vFont.whiteTabs := whiteTabs;
        vFontList := RefList.Cons (vFont, vFontList);
        RETURN vFont
      END
    END
  END MakeVFont;

PROCEDURE VFontCleanUpThread (<* UNUSED *> cl: Thread.Closure): REFANY =
  VAR
    vfl: RefList.T;
    n              := 0;
  BEGIN
    LOOP                         (* forever *)
      LOCK vFontMutex DO
        Thread.Wait (vFontMutex, timeToCleanup);
        vfl := RefListSort.SortD (vFontList, CompareHandouts);
        WHILE vfl # NIL AND NARROW (vfl.head, VFont).handedOut < handoutMin DO
          INC (n);
          vfl := vfl.tail        (* remove this from the cache *)
        END;
        vFontList := RefList.ReverseD (vfl) (* popular ones near the front *)
      END
    END;
  END VFontCleanUpThread;

PROCEDURE CompareHandouts (xx, yy: REFANY): [-1 .. 1] =
  VAR
    x: VFont := xx;
    y: VFont := yy;
  BEGIN
    IF x.handedOut < y.handedOut THEN
      RETURN -1
    ELSIF x.handedOut = y.handedOut THEN
      RETURN 0
    ELSE
      RETURN 1
    END
  END CompareHandouts;

PROCEDURE MakeVOptions (vFont: VFont;
                        leftMargin, rightMargin, turnMargin, topMargin,
                          leading: Points;
                        whiteBlack, whiteStroke: ColorScheme;
                        leftOffset             : Points;
                        wrap                   : BOOLEAN;
                        eob                    : BOOLEAN;
                        intervalStylePrecedence: IntervalStylePrecedence):
  VOptions RAISES {} =
  VAR vOptions: VOptions;
  BEGIN
    vOptions.vFontxxx := vFont;
    vOptions.leftMarginPts := leftMargin;
    vOptions.rightMarginPts := rightMargin;
    vOptions.turnMarginPts := turnMargin;
    vOptions.topMarginPts := topMargin;
    vOptions.leadingPts := leading;
    vOptions.whiteBlack := whiteBlack;
    vOptions.whiteStroke := whiteStroke;
    vOptions.leftOffsetPts := leftOffset;
    vOptions.wrap := wrap;
    vOptions.eob := eob;
    vOptions.intervalStylePrecedence := intervalStylePrecedence;
    RETURN vOptions;
  END MakeVOptions;

PROCEDURE Close (view: View) RAISES {} =
  BEGIN
    WITH z_111 = view^ DO
      LOCK z_111.vt.caret.mutex DO
        IF z_111.previous = NIL THEN
          z_111.vt.views := z_111.next;
        ELSE
          z_111.previous.next := z_111.next;
        END;
        IF z_111.next # NIL THEN
          z_111.next.previous := z_111.previous;
        END;
        (* DISPOSE (view); *)
      END;
    END;
  END Close;

PROCEDURE Move (view: View; READONLY full, saved: Rect.T; scroll: BOOLEAN)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    oFull, oClip, okClip: Rect.T;
    delta               : Point.T;
    f                   : Rect.Partition;
  BEGIN
    oFull := view.rect.full;
    oClip :=
      Rect.Meet (
        Rect.Meet (view.rect.clip, saved),
        Rect.MoveEdge (view.rect.full, Rect.Edge.S,
                       view.rect.text.south - view.rect.full.south));
    delta := Point.Sub (Rect.NorthWest (full), Rect.NorthWest (oFull));
    SetLocation (view, full, Rect.Move (view.rect.bad, delta));
    IF view.rect.full.east - view.rect.full.west = oFull.east - oFull.west
      THEN
      VTVirtual.Resize (view, view.nLines);
      IF (scroll OR ((delta.h = 0) AND (delta.v = 0)))
           AND NOT Rect.IsEmpty (oClip) THEN
        VTCaret.Deactivate (view);
        TRY
          VTReal.Resize (view, view.nLines);
          okClip :=
            Rect.Meet (
              Rect.Meet (full, Rect.MoveEdge (full, Rect.Edge.S,
                                              view.rect.text.south
                                                - view.rect.full.south)),
              Rect.Move (oClip, delta));
          VBT.Scroll (view.vbt, okClip, delta, PaintOp.Copy);
          Rect.Factor (full, okClip, f, 0, 0);
          VTReal.Bad (view, f [0]);
          VTReal.Bad (view, f [1]);
          VTReal.Bad (view, f [3]);
          VTReal.Bad (view, f [4]);
        FINALLY
          VTCaret.Reactivate (view);
        END;
      ELSE
        VTReal.Bad (view, view.rect.clip);
      END;
    ELSE
      VTVirtual.Bad (view);
      VTReal.Bad (view, view.rect.clip);
    END;
  END Move;

PROCEDURE Rescreen (view: View; <* UNUSED *> READONLY cd: VBT.RescreenRec) =
  VAR
    leading := Pts.ToScreenPixels (
                 view.vbt, view.vOptions.leadingPts, Axis.T.Ver);
  BEGIN
    view.vScreenFont :=
      MakeVScreenFont (view.vOptions.vFontxxx, view.vbt, leading);
    view.lineSpacing :=
      view.vScreenFont.vScreenFont.box.south
        - view.vScreenFont.vScreenFont.box.north + leading
  END Rescreen;

(* UTILITY ROUTINES *)

PROCEDURE SetPixelOptions (VAR vo: VOptions; v: VBT.T) =
  BEGIN
    vo.leftMargin := Pts.ToScreenPixels (v, vo.leftMarginPts, Axis.T.Hor);
    vo.rightMargin :=
      Pts.ToScreenPixels (v, vo.rightMarginPts, Axis.T.Hor);
    vo.turnMargin := Pts.ToScreenPixels (v, vo.turnMarginPts, Axis.T.Hor);
    vo.topMargin := Pts.ToScreenPixels (v, vo.topMarginPts, Axis.T.Ver);
    vo.leading := Pts.ToScreenPixels (v, vo.leadingPts, Axis.T.Ver);
    vo.leftOffset := Pts.ToScreenPixels (v, vo.leftOffsetPts, Axis.T.Hor);
  END SetPixelOptions;

PROCEDURE SetLocation (view: View; READONLY full, bad: Rect.T) RAISES {} =
  VAR
    nLines0, nLines1: INTEGER;
    newRealLine     : RealLines;
    newVirtualLine  : VirtualLines;
    text            : Rect.T;
  BEGIN
    WITH vo = view.vOptions DO
      SetPixelOptions (vo, view.vbt);
      view.rect.full := full;
      view.rect.clip := full;
      nLines0 := view.nLines;
      nLines1 := MAX (0, (full.south - (full.north + vo.topMargin))
                           DIV view.lineSpacing);
      view.nLines := nLines1;
      IF nLines1 > LAST (view.real.line^) THEN
        newRealLine := NEW (RealLines, nLines1 + 1);
        FOR i := 0 TO LAST (view.real.line^) DO
          newRealLine [i] := view.real.line [i];
        END;
        view.real.line := newRealLine;
        view.real.blocks.block :=
          NEW (VTDef.BlockArray, nLines1 + 1);
        newVirtualLine := NEW (VirtualLines, nLines1 + 1);
        FOR i := 0 TO LAST (view.virtual.line^) DO
          newVirtualLine [i] := view.virtual.line [i];
        END;
        view.virtual.line := newVirtualLine;
        newVirtualLine := NEW (VirtualLines, nLines1 + 1);
        FOR i := 0 TO LAST (view.newVirtual.line^) DO
          newVirtualLine [i] := view.newVirtual.line [i];
        END;
        view.newVirtual.line := newVirtualLine;
      END;
      text := Rect.FromEdges (
                view.rect.full.west + vo.leftMargin + vo.turnMargin,
                view.rect.full.east - vo.rightMargin - vo.turnMargin,
                view.rect.full.north + vo.topMargin,
                view.rect.full.north + vo.topMargin
                  + view.nLines * view.lineSpacing);
      view.rect.textClip := Rect.Meet (text, view.rect.clip);
      IF view.vOptions.wrap THEN
        view.rect.text :=
          Rect.FromEdges (
            text.west - vo.leftOffset, text.east, text.north, text.south);
      ELSE
        view.rect.text :=
          Rect.FromEdges (text.west - vo.leftOffset, LAST (Pixels) DIV 2,
                          text.north, text.south);
      END;
      view.lineWidth := view.rect.text.east - view.rect.text.west;
      view.rect.bad := Rect.Meet (bad, view.rect.clip)
    END
  END SetLocation;

PROCEDURE MakeVScreenFont (vFont: VFont; vbt: VBT.T; leading: CARDINAL):
  VScreenFont RAISES {} =
  VAR
    vScreenFont: VScreenFont;
    metrics    : ScrnFont.Metrics;
  PROCEDURE Find (vFont: VFont; metrics: ScrnFont.Metrics): VScreenFont =
    BEGIN
      VAR list := vScreenFontList;
      BEGIN
        WHILE list # NIL DO
          VAR x: VScreenFont := list.head;
          BEGIN
            list := list.tail;
            IF x.vScreenFont.vFont = vFont AND x.vScreenFont.metrics = metrics THEN
              RETURN x
            END
          END
        END
      END;
      RETURN NIL
    END Find;
  BEGIN
    metrics := FontMetrics (vbt, vFont.vFont.font);
    IF metrics = NIL THEN RETURN MakeBadVScreenFont (vFont); END;

    IF (metrics.maxBounds.boundingBox.south
          - metrics.maxBounds.boundingBox.north) + leading < 2 THEN
      RETURN MakeBadVScreenFont (vFont);
    END;

    LOCK vFontMutex DO
      vScreenFont := Find (vFont, metrics);
      IF vScreenFont # NIL THEN RETURN vScreenFont; END;
    END;

    vScreenFont := UncachedMakeVScreenFont (vFont, metrics);

    LOCK vFontMutex DO
      IF Find (vFont, metrics) = NIL THEN
        vScreenFontList := RefList.Cons (vScreenFont, vScreenFontList);
      END;
    END;

    RETURN vScreenFont;
  END MakeVScreenFont;

PROCEDURE UncachedMakeVScreenFont (vFont: VFont; metrics: ScrnFont.Metrics):
  VScreenFont RAISES {} =
  VAR
    vScreenFont: VScreenFont;
    bsWidth    : INTEGER;
  BEGIN
    vScreenFont := NEW(VScreenFont);
    vScreenFont.vScreenFont.vFont := vFont;
    vScreenFont.vScreenFont.box := metrics.maxBounds.boundingBox;
    vScreenFont.vScreenFont.paintOpaque :=
      metrics.selfClearing
        AND NOT (metrics.leftKerning OR metrics.rightKerning);
    FOR c := FIRST(CHAR) TO LAST(CHAR) DO
      vScreenFont.vScreenFont.width[c] := 0;
    END;
    vScreenFont.vScreenFont.defined := SET OF CHAR{};
    FOR i := metrics.firstChar TO MIN(metrics.lastChar, ORD(LAST(CHAR))) DO
      VAR c := VAL(i, CHAR);
      BEGIN
        IF c IN vFont.vFont.printable THEN
          IF metrics.charMetrics # NIL THEN
            vScreenFont.vScreenFont.width[c] :=
              metrics.charMetrics[i - metrics.firstChar].printWidth;
          ELSE
            vScreenFont.vScreenFont.width[c] :=
              metrics.maxBounds.printWidth;
          END;
          IF vScreenFont.vScreenFont.width[c] # 0 THEN
            vScreenFont.vScreenFont.defined :=
              vScreenFont.vScreenFont.defined + SET OF CHAR{c}
          END;
        END;
      END;
    END;
    IF SET OF CHAR{' ', '\\', '0'.. '9'} - vScreenFont.vScreenFont.defined
         # SET OF CHAR{} THEN
      RETURN MakeBadVScreenFont(vFont);
    END;
    bsWidth := vScreenFont.vScreenFont.width['\\'];
    FOR c := FIRST(CHAR) TO LAST(CHAR) DO
      IF NOT (c IN vScreenFont.vScreenFont.defined) THEN
        vScreenFont.vScreenFont.width[c] :=
          bsWidth + vScreenFont.vScreenFont.width[
                      VAL(ORD(c) DIV 64 + ORD('0'), CHAR)]
            + vScreenFont.vScreenFont.width[
                VAL(ORD(c) DIV 8 MOD 8 + ORD('0'), CHAR)]
            + vScreenFont.vScreenFont.width[
                VAL(ORD(c) MOD 8 + ORD('0'), CHAR)];
      END;
      vScreenFont.vScreenFont.width['\n'] := 1;
      vScreenFont.vScreenFont.defined :=
        vScreenFont.vScreenFont.defined - SET OF CHAR{'\n'};
      IF '\t' IN vFont.vFont.printable THEN
        vScreenFont.vScreenFont.width['\t'] :=
          8 * vScreenFont.vScreenFont.width[' '];
        vScreenFont.vScreenFont.defined :=
          vScreenFont.vScreenFont.defined - SET OF CHAR{'\t'}
      END;
    END;
    vScreenFont.vScreenFont.metrics := metrics;
    RETURN vScreenFont;
  END UncachedMakeVScreenFont;

PROCEDURE MakeBadVScreenFont (vFont: VFont): VScreenFont RAISES {} =
  VAR vScreenFont := NEW(VScreenFont);
  BEGIN
    vScreenFont.vScreenFont.vFont := vFont;
    vScreenFont.vScreenFont.box :=
      Rect.FromEdges(FIRST(INTEGER) DIV 4, LAST(INTEGER) DIV 4,
                     FIRST(INTEGER) DIV 4, LAST(INTEGER) DIV 4);
    FOR c := FIRST(CHAR) TO LAST(CHAR) DO
      vScreenFont.vScreenFont.width[c] :=
        LAST(INTEGER) DIV 4 - FIRST(INTEGER) DIV 4;
    END;
    vScreenFont.vScreenFont.defined :=
      SET OF CHAR{FIRST(CHAR).. LAST(CHAR)};
    vScreenFont.vScreenFont.paintOpaque := TRUE;
    vScreenFont.vScreenFont.metrics := NIL;
    RETURN vScreenFont;
  END MakeBadVScreenFont;

PROCEDURE FontMetrics (v: VBT.T; fnt: Font.T): ScrnFont.Metrics RAISES {} =
  BEGIN
    WITH screentype = VBT.ScreenTypeOf(v) DO
      IF screentype = NIL THEN
        RETURN NIL
      ELSE
        RETURN Palette.ResolveFont(screentype, fnt).metrics
      END
    END
  END FontMetrics;

PROCEDURE Init () RAISES {} =
  BEGIN
    (* -- vFontQ := ObjectCleanUp.NewQ (); ObjectCleanUp.EstablishCleanUp
       (TYPECODE (VFont), 1, vFontQ); -- *)
    EVAL Thread.Fork(NEW(Thread.Closure, apply := VFontCleanUpThread));
  END Init;

BEGIN
  Init();
END VTView.
