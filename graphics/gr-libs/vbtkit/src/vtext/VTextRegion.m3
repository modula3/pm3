(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Sun Mar 21 16:29:16 PST 1993 by meehan *)
(*      modified On Tue Jun 16 13:12:31 PDT 1992 by muller *)
(*      Modified On Fri Dec 21 14:07:33 1990 by jdd *)
(*      Modified On Tue May 15 17:34:03 PDT 1990 by mcjones *)

(* Region-management operations *)

MODULE VTextRegion;

IMPORT Rd, Rect, Thread, VBT;
IMPORT VTDef, VTReal, VTView, VTVirtual;

CONST
  MinRegionHeight = 2;
  DividerHeight = 1;
  (* SplitRegion splits one region into two, one above the other. *)


PROCEDURE SplitRegion (vtext : T;
                       r     : Region;
                       ht0   : Pixels;
                       scroll: BOOLEAN  := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i                  : CARDINAL;
    ht1, south0, north1: Pixels;
    newRect            : Rect.T;
  BEGIN
    WITH z_157 = vtext^ DO
      IF (z_157.regionMax = LAST (Region)) OR (r > z_157.regionMax) THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      ht1 := z_157.region [r].height - ht0 - DividerHeight;
      IF ((ht0 - z_157.topMargin) DIV z_157.lineSpacing < MinRegionHeight)
           OR ((ht1 - z_157.topMargin) DIV z_157.lineSpacing
                 < MinRegionHeight) THEN
        RAISE VTDef.Error (ErrorCode.IllegalCoord);
      END;
      south0 := z_157.region [r].north + ht0;
      north1 := south0 + DividerHeight;
      FOR z_158 := z_157.regionMax TO r + 1 BY -1 DO
        i := z_158;
        z_157.region [i + 1] := z_157.region [i];
      END;
      z_157.regionMax := z_157.regionMax + 1;
      newRect :=
        Rect.MoveEdge (z_157.region [r].view.rect.full, Rect.Edge.S,
                       z_157.region [r].view.rect.full.north + ht0
                         - z_157.region [r].view.rect.full.south);
      VTView.Move (z_157.region [r].view, newRect,
                   z_157.region [r].view.rect.clip, scroll);
      CopyOut (vtext, r);
      IF z_157.region [r].view.virtual.dirty THEN
        VTVirtual.UpdateView (z_157.region [r].view);
      END;
      SetupRegion (
        vtext, r + 1, north1, ht1,
        MIN (z_157.region [r].view.virtual.line [
               z_157.region [r].view.virtual.lines].virtualLine.from,
             z_157.vt.length));
      z_157.dividersDirty := TRUE;
    END;
  END SplitRegion;

PROCEDURE MergeRegion (vtext: T; i, j: Region; scroll: BOOLEAN := TRUE)
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    r      : Region;
    newRect: Rect.T;
  BEGIN
    WITH z_159 = vtext^ DO
      IF (MAX (i, j) > z_159.regionMax) OR (ABS (i - j) # 1) THEN
        RAISE VTDef.Error (ErrorCode.IllegalRegion);
      END;
      newRect :=
        Rect.FromEdges (z_159.west, z_159.west + z_159.width,
                        z_159.region [MIN (i, j)].view.rect.full.north,
                        z_159.region [MAX (i, j)].view.rect.full.south);
      VTView.Move (z_159.region [j].view, newRect,
                   z_159.region [j].view.rect.clip, scroll);
      CopyOut (vtext, j);
      VTView.Close (z_159.region [i].view);
      z_159.regionMax := z_159.regionMax - 1;
      FOR z_160 := i TO z_159.regionMax DO
        r := z_160;
        z_159.region [r] := z_159.region [r + 1];
      END;
    END;
  END MergeRegion;

PROCEDURE Move (         vtext             : T;
                READONLY newRect, savedRect: Rect.T;
                READONLY dividers          : ARRAY OF Pixels;
                         scroll            : BOOLEAN         )
  RAISES {VTDef.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  PROCEDURE OverlapsNext (r: Region): BOOLEAN RAISES {} =
    BEGIN
      RETURN MAX (n [r], vtext.region [r + 1].view.rect.full.north)
               < MIN (s [r], vtext.region [r + 1].view.rect.full.south);
    END OverlapsNext;
  VAR
    r, r0        : INTEGER;               (* Region *)
    rr           : Region;
    n, s         : ARRAY Region OF Pixels;
    dividerHeight: Pixels;
  BEGIN
    IF newRect.south > newRect.north THEN
      dividerHeight := DividerHeight;
    ELSE
      dividerHeight := 0;
    END;
    WITH z_161 = vtext^ DO
      IF NUMBER (dividers) < z_161.regionMax THEN
        RAISE VTDef.Error (ErrorCode.IllegalDividers);
      END;
      n [0] := newRect.north;
      FOR z_162 := 0 TO z_161.regionMax - 1 DO
        r := z_162;
        s [r] := dividers [r];
        n [r + 1] := dividers [r] + dividerHeight;
      END;
      s [z_161.regionMax] := newRect.south;
      FOR z_163 := 0 TO z_161.regionMax DO
        r := z_163;
        IF n [r] > s [r] THEN
          RAISE VTDef.Error (ErrorCode.IllegalCoord);
        END;
      END;
      z_161.north := newRect.north; (* old-style *)
      z_161.height := newRect.south - newRect.north; (* old-style *)
      z_161.west := newRect.west; (* old-style *)
      z_161.width := newRect.east - newRect.west; (* old-style *)
      z_161.left := newRect.west + z_161.leftMargin + z_161.turnMargin;
      (* old-style *)
      r := 0;
      (* blt the regions around *)
      WHILE r <= z_161.regionMax DO
        WHILE (r <= z_161.regionMax)
                AND ((r = z_161.regionMax) OR NOT OverlapsNext (r)) DO
          VTView.Move (z_161.region [r].view,
                       Rect.FromEdges (z_161.west,
                                       z_161.west + z_161.width, n [r],
                                       s [r]), savedRect, scroll);
          CopyOut (vtext, r);
          r := r + 1;
        END;
        r0 := r;
        WHILE (r <= z_161.regionMax)
                AND ((r = z_161.regionMax) OR OverlapsNext (r)) DO
          r := r + 1;
        END;
        FOR z_164 := r - 1 TO r0 BY -1 DO
          rr := z_164;
          VTView.Move (z_161.region [rr].view,
                       Rect.FromEdges (
                         z_161.west, z_161.west + z_161.width, n [rr],
                         s [rr]), savedRect, scroll);
          CopyOut (vtext, rr);
        END;
      END;
      z_161.dividersDirty := TRUE;
    END;
  END Move;

PROCEDURE SetupRegion (vtext     : T;
                       r         : Region;
                       n         : Pixels;
                       h         : CARDINAL;
                       startIndex: I         )
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR newRect: Rect.T;
  BEGIN
    WITH z_165 = vtext^ DO
      newRect :=
        Rect.FromEdges (z_165.west, z_165.west + z_165.width, n, n + h);
      z_165.region [r].view := VTView.New (z_165.vt, z_165.vbt, newRect,
                                           z_165.vOptions, startIndex);
      CopyOut (vtext, r);
    END;
  END SetupRegion;

PROCEDURE Bad (vtext: T; READONLY where: Rect.T) RAISES {} =
  (* Bad: some portion of the vtext needs to be cleared and redrawn. *)
  VAR r: Region;
  BEGIN
    WITH z_166 = vtext^ DO
      FOR z_167 := 0 TO z_166.regionMax DO
        r := z_167;
        VTReal.Bad (z_166.region [r].view, where);
      END;
      z_166.dividersDirty := TRUE;
    END;
  END Bad;

PROCEDURE UpdateDividers (vtext: T) RAISES {} =
  VAR r: Region;
  BEGIN
    WITH z_168 = vtext^ DO
      IF z_168.dividersDirty THEN
        FOR z_169 := 0 TO z_168.regionMax - 1 DO
          r := z_169;
          VBT.PaintTint (z_168.vbt,
                         Rect.FromEdges (z_168.west, z_168.west + z_168.width,
                                         z_168.region[r].north
                                         + z_168.region[r].height,
                                         z_168.region[r + 1].north),
                         z_168.vOptions.whiteStroke.fg);
        END;
        z_168.dividersDirty := FALSE;
      END;
    END;
  END UpdateDividers;
  
(* UTILITY *)

PROCEDURE CopyOut (vtext: T; r: Region) RAISES {} =
  BEGIN
    WITH z_170 = vtext.region [r] DO
      z_170.north := z_170.view.rect.full.north;
      z_170.height := z_170.view.rect.full.south - z_170.north;
      z_170.top := z_170.view.rect.text.north;
      z_170.nLines := z_170.view.nLines;
    END;
  END CopyOut;

BEGIN
END VTextRegion.
