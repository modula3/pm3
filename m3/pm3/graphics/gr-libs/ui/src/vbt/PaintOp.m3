(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Jan 25 18:19:39 PST 1993 by msm      *)
(*      modified on Tue Mar 10 19:04:56 1992 by steveg   *)
(*      modified on Mon Feb 24 13:57:21 PST 1992 by muller   *)
(*      modified on Sat Nov  2 17:20:48 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE PaintOp;

IMPORT Palette, PlttFrnds, VBT, ScrnPaintOp, ScreenType, ScrnColorMap, 
  TrestleComm;

TYPE RGBClosure = Palette.OpClosure OBJECT
    rgb: ScrnColorMap.RGB;
    mode: Mode;
    gray: REAL;
    bw: BW
  OVERRIDES
    apply := RGBApply
  END;

PROCEDURE FromRGB (r, g, b: REAL;
                   mode            := Mode.Normal;
                   gray            := -1.0;
                   bw              := BW.UseIntensity): T =
  VAR rgb := ScrnColorMap.RGB{r, g, b};
  BEGIN
    IF gray < 0.0 THEN
      gray := MIN(1.0, MAX(0.0, 0.2390 * r + 0.6860 * g + 0.0750 * b))
    END;
    IF bw = BW.UseIntensity THEN
      IF r = 0.0 AND g = 0.0 AND b = 0.0 THEN
        bw := BW.UseFg
      ELSE
        bw := BW.UseBg
      END
    END;
    LOCK PlttFrnds.con DO
      IF PlttFrnds.con.ops # NIL THEN
        FOR i := 0 TO PlttFrnds.con.nextOp - 1 DO
          TYPECASE PlttFrnds.con.ops[i] OF
            NULL =>              (* skip *)
          | RGBClosure (op) =>
              IF op.rgb = rgb AND op.mode = mode AND op.gray = gray
                   AND op.bw = bw THEN
                RETURN T{i}
              END
          ELSE
          END
        END
      END
    END;
    RETURN Palette.FromOpClosure(NEW(RGBClosure, rgb := rgb, mode := mode,
                                     gray := gray, bw := bw))
  END FromRGB;

PROCEDURE RGBApply(cl: RGBClosure; st: VBT.ScreenType): ScrnPaintOp.T =
<*FATAL ScrnPaintOp.Failure*>
  BEGIN
    TRY
      IF st.cmap # NIL AND st.depth # 1 THEN
        VAR rgb := cl.rgb; gray := cl.gray; pix: ScrnColorMap.Pixel; BEGIN
          IF NOT st.color THEN 
            rgb := ScrnColorMap.RGB{gray, gray, gray}
          END;
          TRY
            pix := st.cmap.standard().fromRGB(rgb, cl.mode)
          EXCEPT
            ScrnColorMap.Failure =>
              TRY
                pix := st.cmap.standard().fromRGB(rgb, Mode.Normal)
              EXCEPT
                ScrnColorMap.Failure => 
		  IF cl.bw = BW.UseBg THEN
                    RETURN Palette.ResolveOp(st, Bg)
		  ELSE
		    RETURN Palette.ResolveOp(st, Fg)
		  END
              END
          END;
          RETURN st.op.opaque(pix)
        END
      ELSE
        IF cl.bw = BW.UseBg THEN
          RETURN Palette.ResolveOp(st, Bg)
        ELSE
          RETURN Palette.ResolveOp(st, Fg)
        END
      END
    EXCEPT
      TrestleComm.Failure => RETURN Palette.ResolveOp(st, Fg)
    END;
  END RGBApply;
        
TYPE 
  PairClosure = Palette.OpClosure OBJECT
    op0, op1: T
  OVERRIDES
    apply := ApplyPair
  END;

PROCEDURE Pair (op0, op1: T): T =
  BEGIN
    LOCK PlttFrnds.con DO
      IF PlttFrnds.con.ops # NIL THEN
        FOR i := 0 TO PlttFrnds.con.nextOp - 1 DO
          TYPECASE PlttFrnds.con.ops[i] OF
            NULL =>              (* skip *)
          | PairClosure (cl) =>
              IF cl.op0 = op0 AND cl.op1 = op1 THEN RETURN T{i} END
          ELSE
          END
        END
      END
    END;
    RETURN Palette.FromOpClosure(NEW(PairClosure, op0 := op0, op1 := op1))
  END Pair;

PROCEDURE ApplyPair(cl: PairClosure; st: VBT.ScreenType): ScrnPaintOp.T =
  VAR sop0 := Palette.ResolveOp(st, cl.op0); 
    sop1 := Palette.ResolveOp(st, cl.op1);
  BEGIN
    TRY
      RETURN st.op.bgfg(sop0, sop1)
    EXCEPT
      ScrnPaintOp.Failure, TrestleComm.Failure => 
        RETURN Palette.ResolveOp(st, Transparent)
    END
  END ApplyPair;
  
TYPE SwapClosure = Palette.OpClosure OBJECT
    fg, bg: T;
  OVERRIDES
    apply := ApplySwap
  END;

PROCEDURE ApplySwap(cl: SwapClosure; st: VBT.ScreenType): ScrnPaintOp.T =
  VAR 
    fg := Palette.ResolveOp(st, cl.fg).pix; 
    bg := Palette.ResolveOp(st, cl.bg).pix;
  BEGIN
    IF fg = -1 OR bg = -1 OR bg = fg THEN 
      RETURN Palette.ResolveOp(st, Transparent)
    ELSE
      TRY
        RETURN st.op.swap(bg, fg)
      EXCEPT
        ScrnPaintOp.Failure, TrestleComm.Failure => 
          RETURN Palette.ResolveOp(st, Transparent)
      END
    END
  END ApplySwap;

PROCEDURE SwapPair(bg, fg: T): T =
  BEGIN
    LOCK PlttFrnds.con DO
      IF PlttFrnds.con.ops # NIL THEN
        FOR i := 0 TO PlttFrnds.con.nextOp - 1 DO
          TYPECASE PlttFrnds.con.ops[i] OF
            NULL =>              (* skip *)
          | SwapClosure (cl) =>
              IF cl.fg = fg AND cl.bg = bg THEN RETURN T{i} END
          ELSE
          END
        END
      END
    END;
    RETURN Palette.FromOpClosure(NEW(SwapClosure, fg := fg, bg := bg));
  END SwapPair;

PROCEDURE MakeColorScheme(bg, fg: T): ColorScheme RAISES {} =
  VAR res:= NEW(ColorScheme); BEGIN
    res.bg := bg;
    res.fg := fg;
    res.bgFg := Pair(bg, fg);
    res.transparentFg := Pair(Transparent, fg);
    res.swap := SwapPair(bg, fg);
    res.bgTransparent := Pair(bg, Transparent);
    res.bgSwap := Pair(bg, res.swap);
    res.fgBg := Pair(fg, bg);
    res.fgTransparent := Pair(fg, Transparent);
    res.fgSwap := Pair(fg, res.swap);
    res.transparentBg := Pair(Transparent, bg);
    res.transparentSwap := Pair(Transparent, res.swap);
    res.swapBg := Pair(res.swap, bg);
    res.swapFg := Pair(res.swap, fg);
    res.swapTransparent := Pair(res.swap, Transparent);
    RETURN res
  END MakeColorScheme;

PROCEDURE MakeColorQuad(bg, fg: T): ColorQuad RAISES {} =
  VAR res:= NEW(ColorQuad); BEGIN
    res.bg := bg;
    res.fg := fg;
    res.bgFg := Pair(bg, fg);
    res.transparentFg := Pair(Transparent, fg);
    RETURN res
  END MakeColorQuad;

BEGIN
  bgFg := NEW(ColorScheme, bgFg := BgFg, bg := Bg, fg := Fg,
    transparentFg := TransparentFg, swap:= Swap, bgTransparent :=
    BgTransparent, bgSwap := BgSwap, fgBg := FgBg, fgTransparent :=
    FgTransparent, fgSwap := FgSwap, transparentBg := TransparentBg,
    transparentSwap := TransparentSwap, swapBg := SwapBg, swapFg :=
    SwapFg, swapTransparent := SwapTransparent)
END PaintOp.
