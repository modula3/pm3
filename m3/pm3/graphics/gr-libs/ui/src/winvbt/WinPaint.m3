(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:26:41 PDT 1996 by najork                   *)
(*       Created on Mon Jun 26 09:33:27 PDT 1995 by najork                   *)


UNSAFE MODULE WinPaint;   (* unsafe because of M3toC *)


IMPORT Batch, BatchRep, BatchUtil, Ctypes, M3toC, PaintExt, PaintPrivate, Path,
       PathPrivate, Point, PolyRegion, Rect, Region, ScrnFont, Trapezoid, 
       Trestle, TrestleImpl, VBT, VBTRep, WinBase, WinContext, WinDef, WinGDI,
       WinScreenType, WinScreenTypePrivate, WinScrnFont, WinScrnPaintOp, 
       WinScrnPixmap, Word;

TYPE 
  PC = PaintPrivate.PaintCommand;

CONST
  (** False = 0; **)
  True  = 1;
  
  ComSize = ADRSIZE (PaintPrivate.CommandRec);

VAR
  Chicago            := IsChicago ();
  Bug95_SetDIBits    := FALSE; (* Chicago; *)
  Bug95_PatternBrush := Chicago;

PROCEDURE PaintBatch (self: Trestle.T; v: VBT.T; ba: Batch.T;  hdc: WinDef.HDC) =
  VAR
    cmdP  := LOOPHOLE (ADR (ba.b[0]), PaintPrivate.CommandPtr);
    endP  : PaintPrivate.CommandPtr := ba.next;
    st    : WinScreenType.T := v.st;
  BEGIN
    IF ba.clip.west >= ba.clip.east OR st = NIL THEN
      Batch.Free (ba);
      RETURN;
    END;
    IF ba.clipped = BatchUtil.ClipState.Unclipped THEN
      BatchUtil.Clip (ba);
    END;
    LOCK self DO
      WHILE cmdP < endP DO
        CASE cmdP.command OF
        | PC.TintCom => 
          cmdP := TintCom (cmdP, endP, hdc, st);
        | PC.TextureCom => 
          cmdP := TextureCom (cmdP, endP, hdc, st);
        | PC.PixmapCom => 
          cmdP := PixmapCom (cmdP, endP, hdc, st);
        | PC.ScrollCom => 
          cmdP := ScrollCom (cmdP, hdc, st);
        | PC.TrapCom => 
          cmdP := TrapCom (cmdP, endP, hdc, st);
        | PC.TextCom => 
          cmdP := TextCom (cmdP, cmdP, endP, hdc, st);
        | PC.ExtensionCom =>
          cmdP := ExtensionCom (cmdP, endP, hdc, self, st);
        | PC.RepeatCom => 
          INC (cmdP, ComSize);
        ELSE
          EXIT;
        END
      END;
      Batch.Free(ba);
    END;
  END PaintBatch;


(*****************************************************************************)
(* Painting Tints                                                            *)
(*****************************************************************************)


PROCEDURE TintCom (cmdP, endP: PaintPrivate.CommandPtr;
                   hdc       : WinDef.HDC;
                   st        : WinScreenType.T): PaintPrivate.CommandPtr =
  BEGIN
    WITH op   = LOOPHOLE (cmdP, PaintPrivate.TintPtr)^,
         ctxt = WinContext.PushTint (hdc, st, op.op) DO
      FillRect (hdc, op.clip);
      INC (cmdP, ADRSIZE (op));
      WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO
        FillRect (hdc, cmdP.clip);
        INC (cmdP, ComSize);
      END;
      WinContext.Pop (ctxt);
    END;
    RETURN cmdP;
  END TintCom;


PROCEDURE FillRect (hdc: WinDef.HDC; READONLY r: Rect.T) =
  VAR
    rc    : WinDef.RECT;
    pen   : WinDef.HPEN;
    oldPen: WinDef.HPEN;
    status: WinDef.BOOL;
  BEGIN
    IF r.west < r.east THEN
      rc := FromRect(r);

      (*
       * One would assume that 
       *     EVAL WinUser.FillRect (hdc, ADR(rc), hbr);
       * should be sufficient here. However, "WinUser.FillRect" ignores the
       * current raster operation mode for some reason.
       *)

      (* Load an invisible pen into the DC *)
      oldPen := WinGDI.SelectObject (hdc, 
                                     WinGDI.GetStockObject (WinGDI.NULL_PEN));
      <* ASSERT oldPen # NIL *>

      (* "WinGDI.Rectangle" uses both the current pen and the current brush *)
      status := WinGDI.Rectangle (hdc, r.west, r.north, r.east+1, r.south+1);
      <* ASSERT status = True *>

      pen := WinGDI.SelectObject (hdc, oldPen);
      <* ASSERT pen # NIL *>
    END;
  END FillRect;


(********************
(*
 * Debugging gear
 *)
PROCEDURE StrokeRect (hdc: WinDef.HDC; READONLY r: Rect.T) =
  VAR
    rc    : WinDef.RECT;
    oldBr : WinDef.HBRUSH;
    oldPen: WinDef.HPEN;
    status: WinDef.BOOL;
  BEGIN
    IF r.west < r.east THEN
      rc := FromRect(r);

      oldPen := WinGDI.SelectObject (hdc, 
                                     WinGDI.GetStockObject (WinGDI.BLACK_PEN));
      <* ASSERT oldPen # NIL *>
      oldBr := WinGDI.SelectObject (hdc, 
                                    WinGDI.GetStockObject (WinGDI.NULL_BRUSH));
      <* ASSERT oldBr # NIL *>
      (* "WinGDI.Rectangle" uses both the current pen and the current brush *)
      status := WinGDI.Rectangle (hdc, r.west, r.north, r.east+1, r.south+1);
      <* ASSERT status = True *>
      oldBr := WinGDI.SelectObject (hdc, oldBr);
      oldPen := WinGDI.SelectObject (hdc, oldPen);
    END;
  END StrokeRect;
*************)


(************
(*
 * More debugging gear
 *)
PROCEDURE MarkPoint (hdc: WinDef.HDC; READONLY a: Point.T) =
  VAR
    oldPen: WinDef.HGDIOBJ;
  BEGIN
    oldPen := WinGDI.SelectObject (hdc, 
                                   WinGDI.GetStockObject (WinGDI.BLACK_PEN));
    <* ASSERT oldPen # NIL *>
    DrawLine(hdc, Point.T{a.h - 2, a.v}, Point.T{a.h + 2, a.v});
    DrawLine(hdc, Point.T{a.h, a.v - 2}, Point.T{a.h, a.v + 2});
    oldPen := WinGDI.SelectObject (hdc, oldPen);
  END MarkPoint;
***********)

(*****************************************************************************)
(* Painting textures                                                         *)
(*****************************************************************************)


PROCEDURE TextureCom (cmdP, endP: PaintPrivate.CommandPtr;
                      hdc       : WinDef.HDC;
                      st        : WinScreenType.T): PaintPrivate.CommandPtr =
  VAR
    fastPath : BOOLEAN;
    status   : WinDef.BOOL;
    comdc    : WinDef.HDC;
    bitmap   : WinDef.HBITMAP;
    oldBitmap: WinDef.HBITMAP;
    pm       : PaintPrivate.Pixmap;
    delta    : Point.T;
    pst      : WinScreenType.T;
    color    : WinDef.COLORREF;
    brush    : WinDef.HBRUSH;
    auxBrush : WinDef.HBRUSH;
    brop     : INTEGER;
    frop     : INTEGER;
    pat0     : INTEGER;
    pat1     : INTEGER;
  BEGIN
    WITH op = LOOPHOLE (cmdP, PaintPrivate.PixmapPtr)^ DO

      IF Bug95_PatternBrush AND NOT PixmapIs8x8 (op.pm, st) THEN
        RETURN ChicagoTextureCom (cmdP, endP, hdc, st);
      END;

      (* First, determine if we can use a fast path.  For Windows NT, the fast
         path is to fill the rectangle directly using a pattern brush.  The 
         slow path is to copy the texture onto a memory device context, and 
         then to repeatedly bit-blit this memory device context onto "hdc". *)

      IF op.op >= 0 AND st.optable # NIL AND op.op < NUMBER(st.optable^) THEN
        WITH tbl = st.optable[op.op] DO
          fastPath := (tbl.bop.mode = tbl.fop.mode);
        END;
      ELSE
        fastPath := TRUE;
      END;

      IF NOT fastPath THEN

        (* Create a compatible device context *)
        comdc := WinGDI.CreateCompatibleDC (hdc);
        (* Create a bitmap that can hold the rectangle covered by op.clip *)
        bitmap := WinGDI.CreateCompatibleBitmap (hdc, 
                                                 op.clip.east - op.clip.west, 
                                                 op.clip.south - op.clip.north);
        (* Select the bitmap into "comdc". *)
        oldBitmap := WinGDI.SelectObject (comdc, bitmap);
        
        (* Map point ("op.clip.west","op.clip.north") of page space to point 
           (0,0) of device space. Since the device is a bitmap of width 
           "op.clip.east - op.clip.west" and height "op.clip.south - 
           op.clip.north", the rectangle "op.clip" of page space is mapped 
           onto the device. *)
        status := WinGDI.SetWindowOrgEx (comdc, op.clip.west, op.clip.north, NIL);
        status := WinGDI.SetViewportOrgEx (comdc, 0, 0, NIL);
        
        (* I dabbled a bit around with "SetWorldTransform", but could 
           not get it to work. Anyways, "SetWordTransform" is supported 
           under NT, but not under Chicago. *)
        
        delta := op.delta;
        IF delta # Point.Origin THEN
          WITH pmb = WinScrnPixmap.PixmapDomain (st, pm) DO
            IF NOT Rect.IsEmpty (pmb) THEN
              delta := Rect.Mod (delta, pmb);
            END;
          END;
        END;

        pm := op.pm;
        IF pm < 0 THEN
          pm := WinScrnPixmap.SolidPixmap - pm;
          pst := st.bits;
        ELSE
          pst := st;
        END;

        IF op.op >= 0 AND st.optable # NIL AND op.op < NUMBER(st.optable^) AND
          pst.pmtable # NIL AND pm < NUMBER (pst.pmtable^) THEN
          WITH tbl = st.optable[op.op] DO
            
            (* Select the pixmap into a pattern brush *)
            brush := WinGDI.CreatePatternBrush (pst.pmtable[pm].hbmp);
            <* ASSERT brush # NIL *>
            
            (* Set the pattern brush origin. The Windows way to do this is 
               confusing in two respects: (1) One has to set the origin BEFORE
               selecting the brush into the device context, and (2) the origin
               is specified in device space, not in world/page space. *)
            
            status := WinGDI.SetBrushOrgEx (comdc, 
                                            delta.h - op.clip.west, 
                                            delta.v - op.clip.north, 
                                            NIL); 
            <* ASSERT status = True *>
            
            auxBrush := WinGDI.SelectObject (comdc, brush);
            <* ASSERT auxBrush # NIL *>
            
            (* In Windows, '0' pixels of the bitmap in the pattern brush are 
               drawn in the current text color, so the text color should be 
               "tbl.bop.col". '1' pixels are drawn in the current background 
               color, so this color should be "tbl.fop.col". Counterintuive? 
               Well, after all, this is Windows! *)
            
            (* Draw the pixels which are 0 in "pst.pmtable[pm].hbmp" as black 
               (all 0's), and the pixels which are 1 as white (all 1's) into 
               "comdc". *)
            
            (* Setting the colors of comdc seems to have no effect. *)
            color := WinGDI.SetTextColor (comdc, WinGDI.RGB(0,0,0));
            <* ASSERT color # WinGDI.CLR_INVALID *>
            color := WinGDI.SetBkColor (comdc, WinGDI.RGB(255,255,255));
            <* ASSERT color # WinGDI.CLR_INVALID *>
           
            color := WinGDI.SetTextColor (hdc, WinGDI.RGB(0,0,0));
            <* ASSERT color # WinGDI.CLR_INVALID *>
            color := WinGDI.SetBkColor (hdc, WinGDI.RGB(255,255,255));
            <* ASSERT color # WinGDI.CLR_INVALID *>

            brop := tbl.brop3;
            frop := tbl.frop3;

            pat0 := tbl.bop.col;
            pat1 := tbl.fop.col;

          END;
        ELSE
          brop := 0;
          frop := 0;
        END;

        (* Fill comdc, using the pattern brush *)
        FillRect (comdc, op.clip);

        (* STILL TO DO: delete "brush" *)

        BitBltFill (hdc, pat0, brop, op.clip, comdc);
        BitBltFill (hdc, pat1, frop, op.clip, comdc);
        INC (cmdP, ADRSIZE(op));

        WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO
          BitBltFill (hdc, pat0, brop, cmdP.clip, comdc);
          BitBltFill (hdc, pat1, frop, cmdP.clip, comdc);
          INC (cmdP, ComSize);
        END;
        
        status := WinGDI.DeleteDC (comdc);
        <* ASSERT status = True *>
        status := WinGDI.DeleteObject (bitmap);
        <* ASSERT status = True *>
        
      ELSE (* fastPath = TRUE *)

        WITH ctxt = WinContext.PushTexture (hdc, st, op.op, op.pm, op.delta) DO

          FillRect (hdc, op.clip);
          INC (cmdP, ADRSIZE(op));
          WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO
            FillRect (hdc, cmdP.clip);
            INC (cmdP, ComSize);
          END;

          WinContext.Pop (ctxt);
        END;

      END;
    END;
        
    RETURN cmdP;
  END TextureCom;



PROCEDURE PixmapIs8x8 (pm: PaintPrivate.Pixmap; st: WinScreenType.T): BOOLEAN =
  BEGIN
    IF pm < 0 THEN
      pm := WinScrnPixmap.SolidPixmap - pm;
      st := st.bits;
    END;
    IF st.pmtable # NIL AND pm < NUMBER (st.pmtable^) THEN
      WITH rect = st.pmtable[pm].domain DO
        RETURN Rect.VerSize (rect) = 8 AND Rect.HorSize (rect) = 8;
      END;
    ELSE
      RETURN TRUE;
    END;
  END PixmapIs8x8;


PROCEDURE ChicagoTextureCom (cmdP, endP: PaintPrivate.CommandPtr;
                             hdc       : WinDef.HDC;
                             st        : WinScreenType.T): PaintPrivate.CommandPtr =
  VAR
    dci   : INTEGER;
    status: WinDef.BOOL;
  BEGIN
    WITH op = LOOPHOLE (cmdP, PaintPrivate.PixmapPtr)^ DO

      dci := WinGDI.SaveDC (hdc);
      <* ASSERT dci # 0 *>

      ClipToRect (hdc, op.clip);
      ChicagoFill (hdc, st, op.clip, op.op, op.pm, op.delta, TRUE);

      status := WinGDI.RestoreDC (hdc, -1);
      <* ASSERT status = True *>

      INC (cmdP, ADRSIZE(op));
      WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO

        dci := WinGDI.SaveDC (hdc);
        <* ASSERT dci # 0 *>

        ClipToRect (hdc, cmdP.clip);
        ChicagoFill (hdc, st, cmdP.clip, op.op, op.pm, op.delta, TRUE);

        status := WinGDI.RestoreDC (hdc, -1);
        <* ASSERT status = True *>

        INC (cmdP, ComSize);
      END;

    END;
    RETURN cmdP;
  END ChicagoTextureCom;

PROCEDURE ClipToRect (hdc: WinDef.HDC; clip: Rect.T) =
  VAR
    hrgn  : WinDef.HRGN;
    status: WinDef.BOOL;
  BEGIN
    hrgn := WinGDI.CreateRectRgn (clip.west, clip.north, 
                                  clip.east, clip.south);
    <* ASSERT hrgn # NIL *>

    status := WinGDI.SelectClipRgn (hdc, hrgn);
    <* ASSERT status # WinGDI.ERROR *>

    status := WinGDI.DeleteObject (hrgn);
    <* ASSERT status = True *>
  END ClipToRect;


PROCEDURE ChicagoFill (hdc  : WinDef.HDC;
                       st   : WinScreenType.T;
                       rect : Rect.T;
                       op   : PaintPrivate.PaintOp;
                       pm   : PaintPrivate.Pixmap;
                       delta: Point.T;
                       tile : BOOLEAN) =
  
  PROCEDURE BitBlt (hdc   : WinDef.HDC;
                    col   : WinDef.COLORREF;
                    rop   : WinDef.DWORD;
                    rect  : Rect.T;
                    delta : Point.T;
                    comdc : WinDef.HDC;
                    width : INTEGER;
                    height: INTEGER;
                    tile  : BOOLEAN) =
    VAR
      oldBrush: WinDef.HBRUSH;
      solBrush: WinDef.HBRUSH;
      status  : WinDef.BOOL;
    BEGIN
      IF rop # 0 THEN
        solBrush := WinGDI.CreateSolidBrush (col);
        <* ASSERT solBrush # NIL *>
        oldBrush := WinGDI.SelectObject (hdc, solBrush);
        <* ASSERT oldBrush # NIL *>
        
        WITH west = rect.west - (rect.west - delta.h) MOD width,
             north = rect.north - (rect.north - delta.v) MOD height DO
          IF tile THEN
            FOR x := west TO rect.east - 1 BY width DO
              FOR y := north TO rect.south - 1 BY height DO
                
                status := WinGDI.BitBlt (hdc, x, y, width, height, 
                                         comdc, 0, 0, rop);
                <* ASSERT status = True *>
              END;
            END;
          ELSE
            status := WinGDI.BitBlt (hdc, west, north, width, height, 
                                     comdc, 0, 0, rop);
            <* ASSERT status = True *>
          END;
        END;
        
        oldBrush := WinGDI.SelectObject (hdc, oldBrush);
        <* ASSERT oldBrush = solBrush *>
        status := WinGDI.DeleteObject (solBrush);
        <* ASSERT status = True *>
      END;
    END BitBlt;


  VAR
    pst     : WinScreenType.T;
    comdc   : WinDef.HDC;
    oldBmp  : WinDef.HBITMAP;
    hbmp    : WinDef.HBITMAP;
    status  : WinDef.BOOL;
    bmi     : WinGDI.BITMAPINFO;
    k       := 0;
  BEGIN
    IF delta # Point.Origin THEN
      WITH pmb = WinScrnPixmap.PixmapDomain (st, pm) DO
        IF NOT Rect.IsEmpty (pmb) THEN
          delta := Rect.Mod (delta, pmb);
        END;
      END;
    END;

    IF pm < 0 THEN
      pm := WinScrnPixmap.SolidPixmap - pm;
      pst := st.bits;
    ELSE
      pst := st;
    END;

    IF op >= 0 AND st.optable # NIL AND op < NUMBER(st.optable^) AND
       pst.pmtable # NIL AND pm < NUMBER (pst.pmtable^) THEN

      WITH tbl    = st.optable[op],
           spm    = pst.pmtable[pm],
           height = Rect.VerSize (spm.domain),
           width  = Rect.HorSize (spm.domain),
           pixels = NEW (REF ARRAY OF WinGDI.RGBQUAD, height * width) DO

        comdc := WinGDI.CreateCompatibleDC (hdc);
        <* ASSERT comdc # NIL *>

        hbmp := WinGDI.CreateCompatibleBitmap (hdc, width, height);
        <* ASSERT hbmp # NIL *>

        WITH h = bmi.bmiHeader DO
          h.biSize := BYTESIZE(WinGDI.BITMAPINFOHEADER);
          h.biWidth := width;
          h.biHeight := height;
          h.biPlanes := 1;
          h.biBitCount := BITSIZE (WinDef.COLORREF);
          h.biCompression := WinGDI.BI_RGB;
        END;

        status := WinGDI.GetDIBits (comdc, 
                                    spm.hbmp,      
                                    0,             (* start at scan line 0 *)
                                    height,        (* copy "height" lines *)
                                    ADR(pixels[0]),(* into "pixels" *)
                                    ADR (bmi),
                                    WinGDI.DIB_RGB_COLORS);
        <* ASSERT status = height *>
        
        (* On NT, we can use "SetDIBits" to copy "pixels" into "hbmp".
           On Windows 95, the bitmap comes out black. So, we copy the 
           pixels individually, using "SetPixelV". This works fine 
           both on NT and on Windows 95. 
           
           Note that the spec of "SetDIBits" says that "hbmp" must not be
           selected into "comdc", while the spec of "SetPixelV" says that
           it must be. *)

        IF NOT Bug95_SetDIBits THEN
          status := WinGDI.SetDIBits (comdc,
                                      hbmp,
                                      0,      
                                      height, 
                                      ADR(pixels[0]),
                                      ADR (bmi),
                                      WinGDI.DIB_RGB_COLORS);
          <* ASSERT status = height *>

          oldBmp := WinGDI.SelectObject (comdc, hbmp);
          <* ASSERT oldBmp # NIL *>
            
        ELSE (* Bug95_SetDIBits = TRUE *)
          
          oldBmp := WinGDI.SelectObject (comdc, hbmp);
          <* ASSERT oldBmp # NIL *>
          
          k := 0;
          FOR y := height - 1 TO 0 BY -1 DO
            FOR x := 0 TO width - 1 DO
              WITH p = pixels[k],
                   col = WinGDI.RGB (p.rgbRed, p.rgbGreen, p.rgbBlue) DO
                status := WinGDI.SetPixelV (comdc, x, y, col);
                <* ASSERT status = True *>
                INC (k);
              END;
            END;
          END;
            
        END;

        BitBlt (hdc, tbl.bop.col, tbl.brop3, rect, delta, 
                comdc, width, height, tile);
        BitBlt (hdc, tbl.fop.col, tbl.frop3, rect, delta, 
                comdc, width, height, tile);

        status := WinGDI.DeleteDC (comdc);
        <* ASSERT status = True *>
        status := WinGDI.DeleteObject (hbmp);
        <* ASSERT status = True *>
      END;
    ELSE
      (* "op" not in "st.optable", or "pm" not in "pst.pmtable" - do nothing *)
    END;
  END ChicagoFill;

(*****************************************************************************)
(* Painting pixmaps                                                          *)
(*****************************************************************************)

(* For now, I try to treat pixmaps just like textures. This might not work 
   for color pixmaps, and might not work for every PaintOp. 

   This code has not been tested for every possible PaintOp and for color.
   It seems to work with PaintOp.BgFg and with PaintOp.TransparentFg.
   I should run a fullsuite of tests once I got rudimentary color working.

   Note that the Win32 specification states that WinGDI.SetBrushOrgEx works
   only for x and y coordinates between 0 and 7, and that I assume it to 
   work for arbitrary coordinates. Under NT, this seems to be ok, but there
   is no guarantee that it will work under Windows 95. *)


PROCEDURE PixmapCom (cmdP, endP: PaintPrivate.CommandPtr;
                     hdc       : WinDef.HDC;
                     st        : WinScreenType.T): PaintPrivate.CommandPtr =
  VAR
    fastPath : BOOLEAN;
    status   : WinDef.BOOL;
    comdc    : WinDef.HDC;
    bitmap   : WinDef.HBITMAP;
    oldBitmap: WinDef.HBITMAP;
    pm       : PaintPrivate.Pixmap;
    delta    : Point.T;
    pst      : WinScreenType.T;
    apm      : PaintPrivate.Pixmap;
    color    : WinDef.COLORREF;
    brush    : WinDef.HBRUSH;
    auxBrush : WinDef.HBRUSH;
    oldBrush : WinDef.HBRUSH;
    brop     : INTEGER;
    frop     : INTEGER;
    pat0     : INTEGER;
    pat1     : INTEGER;
  BEGIN
    WITH op = LOOPHOLE (cmdP, PaintPrivate.PixmapPtr)^ DO

      IF Bug95_PatternBrush AND NOT PixmapIs8x8 (op.pm, st) THEN
        RETURN ChicagoPixmapCom (cmdP, endP, hdc, st);
      END;

      IF op.op >= 0 AND st.optable # NIL AND op.op < NUMBER(st.optable^) THEN
        WITH tbl = st.optable[op.op] DO
          brop := tbl.brop3;
          frop := tbl.frop3;
          fastPath := (tbl.bop.mode = tbl.fop.mode);
        END;
      ELSE
        fastPath := TRUE;
      END;


      IF NOT fastPath THEN
        
        (* Create a compatible device context *)
        comdc := WinGDI.CreateCompatibleDC (hdc);
        (* Create a bitmap that can hold the rectangle covered by op.clip *)
        bitmap := WinGDI.CreateCompatibleBitmap (hdc, 
                                                 op.clip.east - op.clip.west, 
                                                 op.clip.south - op.clip.north);
        (* Select the bitmap into "comdc". *)
        oldBitmap := WinGDI.SelectObject (comdc, bitmap);
        
        (* Map point ("op.clip.west","op.clip.north") of page space to point 
           (0,0) of device space. Since the device is a bitmap of width 
           "op.clip.east - op.clip.west" and height "op.clip.south - 
           op.clip.north", the rectangle "op.clip" of page space is mapped 
           onto the device. *)
        status := WinGDI.SetWindowOrgEx (comdc, op.clip.west, op.clip.north, NIL);
        status := WinGDI.SetViewportOrgEx (comdc, 0, 0, NIL);
        
        (* I dabbled a bit around with "SetWorldTransform", but could 
           not get it to work. Anyways, "SetWordTransform" is supported 
           under NT, but not under Chicago. *)
        
        (* Select the pixmap into a pattern brush *)
        pm := op.pm;  apm := pm;
        delta := op.delta;
        IF pm < 0 THEN
          pm := WinScrnPixmap.SolidPixmap - pm;
          pst := st.bits;
        ELSE
          pst := st;
        END;
        IF delta # Point.Origin THEN
          WITH pmb = WinScrnPixmap.PixmapDomain (st, apm) DO
            IF NOT Rect.IsEmpty (pmb) THEN
              delta := Rect.Mod (delta, pmb);
            END;
          END;
        END;
        IF op.op >= 0 AND st.optable # NIL AND op.op < NUMBER(st.optable^) AND
          pst.pmtable # NIL AND pm < NUMBER (pst.pmtable^) THEN
          WITH tbl = st.optable[op.op] DO
            
            brush := WinGDI.CreatePatternBrush (pst.pmtable[pm].hbmp);
            <* ASSERT brush # NIL *>
            
            (* Set the pattern brush origin. The Windows way to do this is 
               confusing in two respects: (1) One has to set the origin BEFORE
               selecting the brush into the device context, and (2) the origin
               is specified in device space, not in world/page space. *)
            
            status := WinGDI.SetBrushOrgEx (comdc, 
                                            delta.h - op.clip.west, 
                                            delta.v - op.clip.north, 
                                            NIL); 
            <* ASSERT status = True *>
            
            auxBrush := WinGDI.SelectObject (comdc, brush);
            <* ASSERT auxBrush # NIL *>
            
            (* In Windows, '0' pixels of the bitmap in the pattern brush are 
               drawn in the current text color, so the text color should be 
               "tbl.bop.col". '1' pixels are drawn in the current background 
               color, so this color should be "tbl.fop.col". Counterintuive? 
               Well, after all, this is Windows! *)
            
            (* Draw the pixels which are 0 in "pst.pmtable[pm].hbmp" as black 
               (all 0's), and the pixels which are 1 as white (all 1's) into 
               "comdc". *)
            
            (* Setting the colors of comdc seems to have no effect. *)
            color := WinGDI.SetTextColor (comdc, WinGDI.RGB(0,0,0));
            <* ASSERT color # WinGDI.CLR_INVALID *>
            color := WinGDI.SetBkColor (comdc, WinGDI.RGB(255,255,255));
            <* ASSERT color # WinGDI.CLR_INVALID *>
           
            color := WinGDI.SetTextColor (hdc, WinGDI.RGB(0,0,0)); 
            <* ASSERT color # WinGDI.CLR_INVALID *>
            color := WinGDI.SetBkColor (hdc, WinGDI.RGB(255,255,255));
            <* ASSERT color # WinGDI.CLR_INVALID *>

            pat0 := tbl.bop.col;
            pat1 := tbl.fop.col;

          END;
        ELSE
          brop := 0;
          frop := 0;
        END;

        (* Fill comdc, using the pattern brush *)
        FillRect (comdc, op.clip);

        oldBrush := WinGDI.GetCurrentObject (hdc, WinGDI.OBJ_BRUSH);

        BitBltFill (hdc, pat0, brop, op.clip, comdc);
        BitBltFill (hdc, pat1, frop, op.clip, comdc);
        INC (cmdP, ADRSIZE(op));

        WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO
          BitBltFill (hdc, pat0, brop, cmdP.clip, comdc);
          BitBltFill (hdc, pat1, frop, cmdP.clip, comdc);
          INC (cmdP, ComSize);
        END;
        
        (* Clean up. *)
        brush := WinGDI.SelectObject (hdc, oldBrush);
        <* ASSERT brush # NIL *>
        status := WinGDI.DeleteObject (brush);
        <* ASSERT status = True *>
        
        status := WinGDI.DeleteDC (comdc);
        <* ASSERT status = True *>
        status := WinGDI.DeleteObject (bitmap);
        <* ASSERT status = True *>
        
      ELSE (* fastPath = TRUE *)
        
        WITH ctxt = WinContext.PushTexture (hdc, st, op.op, op.pm, op.delta) DO
          FillRect (hdc, op.clip);
          INC (cmdP, ADRSIZE(op));
          WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO
            FillRect (hdc, cmdP.clip);
            INC (cmdP, ComSize);
          END;
          WinContext.Pop (ctxt);
        END;
      END;
    END;

    RETURN cmdP;
  END PixmapCom;


PROCEDURE ChicagoPixmapCom (
                cmdP, endP: PaintPrivate.CommandPtr;
                hdc       : WinDef.HDC;
                st        : WinScreenType.T): PaintPrivate.CommandPtr =
  VAR
    dci   : INTEGER;
    status: WinDef.BOOL;
  BEGIN
    WITH op = LOOPHOLE (cmdP, PaintPrivate.PixmapPtr)^ DO

      dci := WinGDI.SaveDC (hdc);
      <* ASSERT dci # 0 *>

      ClipToRect (hdc, op.clip);
      ChicagoFill (hdc, st, op.clip, op.op, op.pm, op.delta, FALSE);

      status := WinGDI.RestoreDC (hdc, -1);
      <* ASSERT status = True *>

      INC (cmdP, ADRSIZE(op));
      WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO

        dci := WinGDI.SaveDC (hdc);
        <* ASSERT dci # 0 *>

        ClipToRect (hdc, cmdP.clip);
        ChicagoFill (hdc, st, cmdP.clip, op.op, op.pm, op.delta, FALSE);

        status := WinGDI.RestoreDC (hdc, -1);
        <* ASSERT status = True *>

        INC (cmdP, ComSize);
      END;

    END;
    RETURN cmdP;
  END ChicagoPixmapCom;


PROCEDURE BitBltFill (hdc   : WinDef.HDC;
                      col   : WinDef.COLORREF;
                      rop   : WinDef.DWORD;
                      rect  : Rect.T;
                      comdc : WinDef.HDC) =
  VAR
    oldBrush: WinDef.HBRUSH;
    solBrush: WinDef.HBRUSH;
    status  : WinDef.BOOL;
  BEGIN
    IF rop # 0 THEN
      solBrush := WinGDI.CreateSolidBrush (col);
      <* ASSERT solBrush # NIL *>
      oldBrush := WinGDI.SelectObject (hdc, solBrush);
      <* ASSERT oldBrush # NIL *>
            
      status := WinGDI.BitBlt (hdc, rect.west, rect.north,
                               rect.east - rect.west, 
                               rect.south - rect.north, 
                               comdc, rect.west, rect.north, rop);
      <* ASSERT status = True *>
            
      oldBrush := WinGDI.SelectObject (hdc, oldBrush);
      <* ASSERT oldBrush = solBrush *>
      status := WinGDI.DeleteObject (solBrush);
      <* ASSERT status = True *>
    END;
  END BitBltFill;


(*****************************************************************************)
(* Scrolling                                                                 *)
(*****************************************************************************)


(* So far, I only handle "cmdP.op = Paint.Copy".  I'm not quite sure what
 * it means to apply general paint ops to a source whose depth is typically 
 * not 1. 
 *)

PROCEDURE ScrollCom (cmdP: PaintPrivate.CommandPtr;
                     hdc : WinDef.HDC;
                     st  : WinScreenType.T): PaintPrivate.CommandPtr =
  VAR
    trop := 16_00AA0029;  (* Ternary raster op code for NO-OP *)
  BEGIN
    WITH op = LOOPHOLE (cmdP, PaintPrivate.ScrollPtr)^ DO
      IF op.op >= 0 AND st.optable # NIL AND op.op < NUMBER (st.optable^) THEN
        WITH tbl = st.optable[op.op] DO
          IF tbl.bop.mode = WinScrnPaintOp.Mode.Copy THEN
            trop := tbl.brop3;
          END;
        END;
      END;

      INC (cmdP, ADRSIZE (op));
      IF CopyRectWithinDC (hdc, trop, op.clip, op.delta) THEN
(*
 * At this point, the xvbt counterpart has the following code:
 *
 *      XScrollQueue.Insert (ur.scrollQ, op^);
 *      IF Region.OverlapRect (Rect.Sub (op.clip, op.delta), ur.badR)
 *           AND NOT Region.SubsetRect (op.clip, ur.badR) THEN
 *        ur.badR := Region.Join (Region.MeetRect (op.clip, 
 *                                                 Region.Add (ur.badR, 
 *                                                             op.delta)), 
 *                                ur.badR)
 *      END;
 *)
      END;
    END;
    RETURN cmdP;
  END ScrollCom;


PROCEDURE CopyRectWithinDC (         hdc  : WinDef.HDC; 
                                     trop : WinDef.DWORD;
                            READONLY clip : Rect.T;
                            READONLY delta: Point.T): BOOLEAN =
  VAR
    status: WinDef.BOOL;
  BEGIN
    IF clip.west < clip.east + 1 AND clip.north < clip.south + 1 THEN
      status := WinGDI.BitBlt (hdc, 
                               clip.west, 
                               clip.north,
                               clip.east - clip.west,
                               clip.south - clip.north,
                               hdc,
                               clip.west - delta.h,

                               clip.north - delta.v,
                               trop);
      <* ASSERT status = True *>
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END CopyRectWithinDC;


(*****************************************************************************)
(* Painting Trapezoids                                                       *)
(*****************************************************************************)


PROCEDURE TrapCom (cmdP, endP: PaintPrivate.CommandPtr;
                   hdc       : WinDef.HDC;
                   st        : WinScreenType.T): PaintPrivate.CommandPtr =
  BEGIN
    WITH op   = LOOPHOLE (cmdP, PaintPrivate.TrapPtr)^,
         ctxt = WinContext.PushTexture (hdc, st, op.op, op.pm, op.delta) DO

      IF op.m1.n < 0 THEN
        op.m1.n := -op.m1.n;
        op.m1.d := -op.m1.d;
      ELSIF op.m1.n = 0 THEN
        INC (cmdP, ADRSIZE(op));
        RETURN cmdP;
      END;
      IF op.m2.n < 0 THEN
        op.m2.n := -op.m2.n;
        op.m2.d := -op.m2.d;
      ELSIF op.m2.n = 0 THEN
        INC (cmdP, ADRSIZE(op));
        RETURN cmdP;
      END;

      Trap (hdc, op, op.clip);
      INC (cmdP, ADRSIZE(op));

      WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO
        Trap (hdc, op, cmdP.clip);
        INC (cmdP, ComSize);
      END;

      WinContext.Pop (ctxt);
    END;
    RETURN cmdP;
  END TrapCom;


PROCEDURE Trap (         hdc : WinDef.HDC;
                READONLY tr  : PaintPrivate.TrapRec;
                READONLY clip: Rect.T) =

  PROCEDURE HW (READONLY m: Trapezoid.Rational;
                READONLY p: Point.T;
                         v: INTEGER): INTEGER =
    (* Return ceiling of the h-coordinate of the intersection of the
       trapezoid edge determined by (m, p) with the horizontal line at height
       v. *)
    BEGIN
      RETURN p.h + (m.d * (v - p.v) + m.n - 1) DIV m.n;
    END HW;

  PROCEDURE HF (READONLY m: Trapezoid.Rational;
                READONLY p: Point.T;
                         v: INTEGER): INTEGER =
    (* Return fractional part of (ceiling - actual) of intersection above *)
    BEGIN
      RETURN -m.d * (v - p.v) MOD m.n;
    END HF;

  VAR
    vlo, vhi, hw1, hw2, hf1, hf2, mw1, mw2, mf1, mf2, lft, rit: INTEGER;
    empty                                                     : BOOLEAN;
  BEGIN
    IF clip.west >= clip.east THEN 
      RETURN;
    END;
    vlo := clip.north;
    vhi := clip.south;
    IF tr.m1.d = 0 AND tr.m2.d = 0 THEN
      FillRect (hdc, 
                Rect.Meet (clip, Rect.FromEdges (tr.p1.h, tr.p2.h, vlo, vhi)));
      RETURN;
    END;
    hw1 := HW (tr.m1, tr.p1, vlo);
    IF hw1 >= clip.east AND HW (tr.m1, tr.p1, vhi - 1) >= clip.east THEN
      RETURN;
    END;
    hw2 := HW (tr.m2, tr.p2, vlo);
    IF hw2 <= clip.west AND HW (tr.m2, tr.p2, vhi - 1) <= clip.west THEN
      RETURN;
    END;
    hf1 := HF (tr.m1, tr.p1, vlo);
    hf2 := HF (tr.m2, tr.p2, vlo);
    mw1 := tr.m1.d DIV tr.m1.n;
    mf1 := tr.m1.d MOD tr.m1.n;
    mw2 := tr.m2.d DIV tr.m2.n;
    mf2 := tr.m2.d MOD tr.m2.n;
    empty := TRUE;           (* set to false as soon as something is painted *)
    WHILE vlo # vhi DO
      lft := MAX (hw1, clip.west);
      rit := MIN (hw2, clip.east);
      IF lft < rit THEN
        FillRect (hdc, Rect.FromEdges (lft, rit, vlo, vlo + 1));
        empty := FALSE;
      ELSIF lft > rit AND NOT empty THEN
        (* Generated some painting and then found [lft ..  rit) empty by more 
           than one pixel; hence all the remaining lines will be empty, hence:
         *)
        RETURN;
      END;
      (* Advance to next scan line: *)
      INC (vlo);
      INC (hw1, mw1);
      DEC (hf1, mf1);
      IF hf1 < 0 THEN 
        INC (hf1, tr.m1.n); 
        INC (hw1) 
      END;
      INC (hw2, mw2);
      DEC (hf2, mf2);
      IF hf2 < 0 THEN 
        INC (hf2, tr.m2.n); 
        INC (hw2);
      END;
    END;
  END Trap;


(*****************************************************************************)
(* Painting Text                                                             *)
(*****************************************************************************)


PROCEDURE TextCom (cmd       : PaintPrivate.CommandPtr;
                   pAdr, endP: PaintPrivate.CommandPtr;
                   hdc       : WinDef.HDC;
                   st        : WinScreenType.T): PaintPrivate.CommandPtr =
  TYPE
    Mode = {PaintBackground, LeaveBackground};
  VAR
    pr        : PolyRegion.T;
    brush, oldBrush : WinDef.HBRUSH;
    oldFont   : WinDef.HFONT;
    oldColor  : WinDef.COLORREF; 
    oldBgColor: WinDef.COLORREF;
    oldBgMode : Ctypes.int;
    mode      : Mode;
    status    : WinDef.BOOL;
  BEGIN
    WITH op      = LOOPHOLE (cmd, PaintPrivate.TextPtr),
         clipped = PaintPrivate.Prop.Clipped IN op.props DO
      
      (* This chunk of code replaces XGC.ResolveTextGC.
       * Unresolved: 
       *   - mode determination: In xvbt, the mode depends on the "fill_style"
       *     of the XScrnTpRep.OpRecord: "X.FillOpaqueStippled" sets the mode
       *     to "UseImageString" (ie "PaintBackground"), otherwise it is
       *     is "UseDrawString" (ie "LeaveBackground").
       *   - raster operations for text
       *)
      oldFont := WinGDI.SelectObject (hdc, WinScrnFont.FromFont (op.fnt));
      <* ASSERT oldFont # NIL *>

      IF op.op >= 0 AND st.optable # NIL AND op.op < NUMBER(st.optable^) THEN
        WITH tbl = st.optable[op.op] DO
          (* The brush is used for erasing the background *)
          brush := WinGDI.CreateSolidBrush (tbl.bop.col);
          <* ASSERT brush # NIL *>
          oldBrush := WinGDI.SelectObject (hdc, brush);
          <* ASSERT oldBrush # NIL *>
          oldColor := WinGDI.SetTextColor (hdc, tbl.fop.col);
          <* ASSERT oldColor # WinGDI.CLR_INVALID *>
          
          IF FALSE THEN
            oldBgColor := WinGDI.SetBkColor (hdc, tbl.bop.col);
            <* ASSERT oldBgColor # WinGDI.CLR_INVALID *>
            oldBgMode := WinGDI.SetBkMode (hdc, WinGDI.OPAQUE);
            <* ASSERT oldBgMode # 0 *>
            mode := Mode.PaintBackground;
          ELSE
            oldBgMode := WinGDI.SetBkMode (hdc, WinGDI.TRANSPARENT);
            <* ASSERT oldBgMode # 0 *>
            mode := Mode.LeaveBackground;
          END;
        END;
      ELSE
        (* don't draw anything *)
      END;
      
      WITH subbed = (mode = Mode.PaintBackground)
           AND PaintPrivate.Prop.FontSub IN op.props DO
        INC (pAdr, op.szOfRec * ADRSIZE(Word.T));
        IF NOT clipped THEN
          IF op.clip.west < op.clip.east THEN
            IF subbed THEN 
              FillRect (hdc, op.clip)
            END;
            PaintString(hdc, st, op)
          END
        ELSE
          pr := PolyRegion.Empty;
          PolyRegion.JoinRect (pr, op.clip);
          WHILE  pAdr < endP AND pAdr.command = PC.RepeatCom DO
            IF PolyRegion.OverlapRect (pr, pAdr.clip) THEN
              WITH rgn = PolyRegion.ToRegion (pr) DO
                IF NOT Region.IsEmpty (rgn) THEN
                  SetClipRegion (hdc, rgn);
                  IF subbed THEN 
                    FillRect (hdc, rgn.r) 
                  END;
                  PaintString (hdc, st, op);
                  UnsetClipRegion (hdc);
                END
              END;
              pr := PolyRegion.Empty
            END;
            PolyRegion.JoinRect (pr, pAdr.clip);
            INC (pAdr, ComSize);
          END;
          WITH rgn = PolyRegion.ToRegion (pr) DO
            IF NOT Region.IsEmpty (rgn) THEN
              SetClipRegion (hdc, rgn); 
              IF subbed THEN 
                FillRect (hdc, rgn.r) 
              END;
              PaintString (hdc, st, op);
              UnsetClipRegion (hdc);
            END;
          END;
        END;
      END;
    END;

    (* Free up things *)
    IF brush # NIL THEN
      oldBrush := WinGDI.SelectObject (hdc, oldBrush);
      <* ASSERT oldBrush = brush *>
      status := WinGDI.DeleteObject (brush);
      <* ASSERT status = True *>
    END;

    oldFont := WinGDI.SelectObject (hdc, oldFont);
    <* ASSERT oldFont # NIL *>

    RETURN pAdr;
  END TextCom;


CONST
  ValidRect = Rect.T{west := -32768, east := 32768, north := -32768,
                     south := 32768};


PROCEDURE PaintString (hdc: WinDef.HDC; 
                       st : WinScreenType.T;
                       op : PaintPrivate.TextPtr) =

  PROCEDURE FontIdToScrnFont (st: WinScreenType.T; id: INTEGER): ScrnFont.T =
    BEGIN
      FOR i := FIRST(st.fonts^) TO LAST(st.fonts^) DO
        IF st.fonts[i].id = id THEN
          RETURN st.fonts[i];
        END;
      END;
      <* ASSERT FALSE *>  
    END FontIdToScrnFont;

  VAR
    i     := 0;
    newi  : INTEGER;
    dlp   : UNTRACED REF VBT.Displacement := 
                                     op + ADRSIZE(PaintPrivate.TextRec);
    endp  : UNTRACED REF VBT.Displacement := 
                                     dlp + ADRSIZE(VBT.Displacement) * op.dlsz;
    txtp  := LOOPHOLE (endp, Ctypes.char_star);
    blank := M3toC.TtoS(" ");
    delta : Ctypes.int;
    status: Ctypes.int;
  BEGIN
    WITH sz = op.txtsz, 
         ascent = FontIdToScrnFont (st, op.fnt).metrics.ascent,
         pt = Point.T {op.refpt.h, op.refpt.v - ascent} DO

      (* If the string is empty, or the text is of-screen, exit *)
      IF sz = 0 OR NOT Rect.Member (pt, ValidRect) THEN 
        RETURN;
      END;

      (* Set the current position, and tell windows to move the current 
         position upon each call to "TextOut" and "ExtTextOut". *)
      status := WinGDI.SetTextAlign (hdc, WinGDI.TA_UPDATECP);
      <* ASSERT status # WinGDI.GDI_ERROR *>
      status := WinGDI.MoveToEx (hdc, pt.h, pt.v, NIL);
      <* ASSERT status = True *>

      WHILE i < sz DO

        delta := 0;
        WHILE dlp # endp AND dlp.index = i DO
          INC (delta, dlp.dh);
          dlp := dlp + ADRSIZE (VBT.Displacement);
        END;

        (* Emit a blank character of width "delta" *)
        IF delta > 0 THEN
          status := WinGDI.ExtTextOut(hdc, 0, 0, 0, NIL, blank, 1, ADR(delta));
          <* ASSERT status = True *>
        END;

        IF dlp = endp OR dlp.index >= sz THEN
          newi := sz;
        ELSE
          newi := dlp.index
        END;

        (* Draw characters "i" to "newi" - 1 *)
        status := WinGDI.TextOut (hdc, 0, 0, txtp + i, newi - i);
        <* ASSERT status = True *>
        i := newi;
      END;
    END;
  END PaintString;


(*****************************************************************************)
(* Extensions: Stroking and filling polygons; drawing simple lines.          *)
(*                                                                           *)
(* The xvbt version also handles pictures                                    *)
(*****************************************************************************)


PROCEDURE ExtensionCom (cmdP, endP: PaintPrivate.CommandPtr;
                        hdc       : WinDef.HDC;
                        trsl      : Trestle.T;
                        st        : WinScreenType.T): PaintPrivate.CommandPtr =
  <* FATAL Path.Malformed *>
  VAR
    op := LOOPHOLE (cmdP, PaintPrivate.ExtensionPtr);
  BEGIN
    INC (cmdP, op.szOfRec * ADRSIZE(Word.T));
    CASE op.subCommand OF
    | PaintExt.FillCommand, PaintExt.StrokeCommand, PaintExt.LineCommand =>
      VAR
        fillP   := LOOPHOLE (op, PaintExt.FillPtr);
        strokeP := LOOPHOLE (op, PaintExt.StrokePtr);
        lineP   := LOOPHOLE (op, PaintExt.LinePtr);
        pathP   : PaintExt.PathPtr;
        path    : Path.T;
        pr      : PolyRegion.T;
        ctxt    : WinContext.T;
      BEGIN
        IF op.subCommand = PaintExt.LineCommand THEN
          ctxt := WinContext.PushStroke (
                      hdc, st, op.op, op.pm,
                      Point.Add(op.delta, lineP.delta), 
                      lineP.width, lineP.end, VBT.JoinStyle.Round);
          IF op.delta # Point.Origin THEN
            lineP.p := Point.Add(lineP.p, op.delta);
            lineP.q := Point.Add(lineP.q, op.delta)
          END
        ELSE
          IF op.subCommand = PaintExt.FillCommand THEN
            pathP := ADR(fillP.path);
            ctxt := WinContext.PushFill (
                        hdc, st, op.op, op.pm,
                        Point.Add(op.delta, fillP.delta), 
                        fillP.wind);
          ELSIF op.subCommand = PaintExt.StrokeCommand THEN
            pathP := ADR(strokeP.path);
            ctxt := WinContext.PushStroke (
                        hdc, st, op.op, op.pm,
                        Point.Add(op.delta, strokeP.delta),
                        strokeP.width, strokeP.end, strokeP.join);
          END;
          path := NEW(Path.T);
          path.curveCount := pathP.curveCount;
          path.start := pathP + ADRSIZE(pathP^);
          path.next := cmdP;
          path.end := cmdP;
          path.current := cmdP;
          IF op.delta # Point.Origin THEN
            path := Path.Translate(path, op.delta);
          END;
          IF path.curveCount # 0 THEN 
            path := Path.Flatten(path);
          END;
        END;
        pr := PolyRegion.Empty;
        PolyRegion.JoinRect(pr, op.clip);
        WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO
          IF PolyRegion.OverlapRect(pr, cmdP.clip) THEN
            WITH rgn = PolyRegion.ToRegion(pr) DO
              IF NOT Region.IsEmpty(rgn) THEN
                SetClipRegion (hdc, rgn);
                IF op.subCommand = PaintExt.LineCommand THEN
                  DrawLine (hdc, lineP.p, lineP.q);
                ELSIF op.subCommand = PaintExt.FillCommand THEN
                  FillPath(trsl, hdc, path)
                ELSE
                  StrokePath(trsl, hdc, path)
                END;
                UnsetClipRegion (hdc);
              END
            END;
            pr := PolyRegion.Empty
          END;
          PolyRegion.JoinRect(pr, cmdP.clip);
          INC (cmdP, ComSize);
        END;
        WITH rgn = PolyRegion.ToRegion(pr) DO
          IF NOT Region.IsEmpty(rgn) THEN
            SetClipRegion (hdc, rgn);
            IF op.subCommand = PaintExt.LineCommand THEN
              DrawLine (hdc, lineP.p, lineP.q);
            ELSIF op.subCommand = PaintExt.FillCommand THEN
              FillPath(trsl, hdc, path)
            ELSE
              StrokePath(trsl, hdc, path)
            END;
            UnsetClipRegion (hdc);
          END
        END;

        WinContext.Pop (ctxt);
      END;
    | PaintExt.PictureCommand =>
      <* ASSERT FALSE *>  (* pictures are not implemented in WinTrestle *)
    ELSE
      (* skip all "repeat" commands *)
      WHILE cmdP < endP AND cmdP.command = PC.RepeatCom DO
        INC(cmdP, ComSize);
      END;
    END;

    RETURN cmdP;
  END ExtensionCom;


(* This function could move into a module "WinWrap" *)

PROCEDURE DrawLine (hdc: WinDef.HDC; a, b: Point.T) =
  VAR 
    points := ARRAY [0..1] OF WinDef.POINT {
                                  WinDef.POINT {a.h, a.v},
                                  WinDef.POINT {b.h, b.v}};
    status: WinDef.BOOL;
  BEGIN
    status := WinGDI.Polyline (hdc, ADR(points[0]), 2);
    <* ASSERT status = True *>
  END DrawLine;


TYPE
  StrokeMap = Path.MapObject OBJECT
    trsl: Trestle.T;
    hdc : WinDef.HDC;
    a   : Points;
    n   : CARDINAL := 0;
  OVERRIDES
    line  := StrokeLine;
    move  := StrokeMove;
    close := StrokeLine
  END;
  Points = UNTRACED REF ARRAY OF WinDef.POINT;


PROCEDURE StrokePath (trsl: Trestle.T; hdc: WinDef.HDC; path: Path.T) =
  VAR
    sm := NEW(StrokeMap, trsl := trsl, hdc := hdc, a := NEW (Points, 50));
    <*FATAL Path.Malformed*>
  BEGIN
    Path.Map (path, sm);
    IF sm.n # 0 THEN 
      EmitStroke (sm) 
    END;
    DISPOSE (sm.a);
  END StrokePath;


PROCEDURE StrokeMove (self: StrokeMap; READONLY p: Point.T) =
  BEGIN
    IF self.n # 0 THEN 
      EmitStroke (self) 
    END;
    self.a[0].x := p.h;
    self.a[0].y := p.v;
    self.n := 1
  END StrokeMove;


PROCEDURE StrokeLine (                    self: StrokeMap;
                      <*UNUSED*> READONLY p   : Point.T;
                                 READONLY q   : Point.T    ) =
  VAR 
    m := NUMBER(self.a^);
  BEGIN
    IF self.n = m THEN
      VAR 
        newa := NEW(Points, 2 * m);
      BEGIN
        SUBARRAY (newa^, 0, m) := self.a^;
        DISPOSE (self.a);
        self.a := newa;
      END;
    END;
    self.a[self.n].x := q.h;
    self.a[self.n].y := q.v;
    INC (self.n)
  END StrokeLine;


PROCEDURE EmitStroke (sm: StrokeMap) =
  VAR
    status: WinDef.BOOL;
  BEGIN
    IF sm.n = 1 THEN 
      sm.a[1] := sm.a[0]; 
      sm.n := 2 
    END;
    status := WinGDI.Polyline (sm.hdc, ADR(sm.a[0]), sm.n);
    <* ASSERT status = True *>
    sm.n := 0;
  END EmitStroke;


TYPE
  FillMap = Path.MapObject OBJECT
    trsl         : Trestle.T;
    a            : Points;
    n            : CARDINAL := 0;
    origin, start: Point.T;
  OVERRIDES
    line  := FillLine;
    move  := FillMove;
    close := FillLine
  END;


PROCEDURE FillPath (trsl: Trestle.T; hdc: WinDef.HDC; path: Path.T) =
  <*FATAL Path.Malformed*>
  VAR 
    sm := NEW(FillMap, trsl := trsl, a := NEW(Points, 50));
    status: WinDef.BOOL;
  BEGIN
    TRY
      Path.Map (path, sm);
      IF sm.n # 0 THEN
        FillMove (sm, sm.start);
        (* We leave the "polygon fill mode" at its default value of "WINDING".
           This means that complex polygons can contain "islands". *)
        status := WinGDI.Polygon (hdc, ADR(sm.a[0]), sm.n);
        <* ASSERT status = True *>
      END;
    FINALLY
      DISPOSE (sm.a);
    END;
  END FillPath;


PROCEDURE FillMove (self: FillMap; READONLY p: Point.T) =
  BEGIN
    IF self.n = 0 THEN
      self.origin := p
    ELSE
      FillLine (self, Point.Origin, self.start);
      FillLine (self, self.start, self.origin)
    END;
    FillLine (self, self.origin, p);
    self.start := p
  END FillMove;


PROCEDURE FillLine (                    self: FillMap;
                    <*UNUSED*> READONLY p   : Point.T;
                               READONLY q   : Point.T  ) =
  VAR 
    m := NUMBER(self.a^);
  BEGIN
    IF self.n = m THEN
      VAR 
        newa := NEW (Points, 2 * m);
      BEGIN
        SUBARRAY (newa^, 0, m) := self.a^;
        DISPOSE (self.a);
        self.a := newa
      END
    END;
    self.a[self.n].x := q.h;
    self.a[self.n].y := q.v;
    INC(self.n)
  END FillLine;


(*****************************************************************************)
(* Auxiliary procedures                                                      *)
(*****************************************************************************)


PROCEDURE FromRect (READONLY r: Rect.T): WinDef.RECT =
  BEGIN
    RETURN WinDef.RECT {left   := r.west,
                        right  := r.east,
                        top    := r.north,
                        bottom := r.south};
  END FromRect;

PROCEDURE EmptyRegion (): WinDef.HRGN =
  VAR
    rgn := WinGDI.CreateRectRgn (1,1,1,1);
  BEGIN
    <* ASSERT rgn # NIL *>
    RETURN rgn;
  END EmptyRegion;

PROCEDURE FromRegion (READONLY rgn: Region.T): WinDef.HRGN =
  VAR
    hrgn := EmptyRegion();
    rr   := EmptyRegion();
    status: Ctypes.int;
  BEGIN
    WITH rects = Region.ToRects (rgn) DO
      FOR i := FIRST(rects^) TO LAST (rects^) DO
        WITH r  = rects[i] DO
          status := WinGDI.SetRectRgn (rr, r.west, r.north, r.east, r.south);
          <* ASSERT status = True *>
          status := WinGDI.CombineRgn (hrgn, hrgn, rr, WinGDI.RGN_OR);
          <* ASSERT status # WinGDI.ERROR *>
        END;
      END;
      status := WinGDI.DeleteObject (rr);
      <* ASSERT status = True *>
    END;
    RETURN hrgn;
  END FromRegion;


PROCEDURE SetClipRegion (hdc: WinDef.HDC; rgn: Region.T) =
  VAR
    hrgn := FromRegion(rgn);
    status: Ctypes.int;
  BEGIN
    status := WinGDI.SelectClipRgn (hdc, hrgn);
    <* ASSERT status # WinGDI.ERROR *>
    status := WinGDI.DeleteObject (hrgn);
    <* ASSERT status = True *>
  END SetClipRegion;


PROCEDURE UnsetClipRegion (hdc: WinDef.HDC) =
  VAR
    status: Ctypes.int;
  BEGIN
    status := WinGDI.SelectClipRgn (hdc, NIL);
    <* ASSERT status # WinGDI.ERROR *>
  END UnsetClipRegion;


PROCEDURE IsChicago (): BOOLEAN =
  VAR
    os_version : WinBase.OSVERSIONINFO;
    status : WinDef.BOOL;
  BEGIN
    os_version.dwOSVersionInfoSize := BYTESIZE (os_version);
    status := WinBase.GetVersionEx (ADR (os_version));
    <* ASSERT status = True *>
    RETURN os_version.dwPlatformId = WinBase.VER_PLATFORM_WIN32_WINDOWS;
  END IsChicago;


BEGIN
END WinPaint.
