(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue Jan 31 10:00:13 PST 1995 by kalsow   *)
(*      modified on Fri Jul  8 17:10:24 PDT 1994 by msm      *)
(*      modified on Sat Apr  2 16:44:54 PST 1994 by heydon   *)
(*      modified on Mon Oct  4 11:36:26 PDT 1993 by sfreeman *)
(* modified on Tue Mar 10 19:04:35 1992 by steveg *)
(* modified on Mon Feb 24 13:56:44 PST 1992 by muller *)
(* modified on Sat Nov 16 15:44:03 PST 1991 by gnelson *)
<*PRAGMA LL*>

UNSAFE MODULE BatchUtil EXPORTS BatchRep, BatchUtil;

IMPORT Batch, PaintPrivate, Point, Rect, Word, PictureRep;

TYPE PC = PaintPrivate.PaintCommand;

PROCEDURE GetClip (ba: Batch.T): Rect.T =
  BEGIN
    RETURN ba.clip
  END GetClip;

PROCEDURE GetClipState (ba: Batch.T): ClipState =
  BEGIN
    RETURN ba.clipped
  END GetClipState;

PROCEDURE GetLength (ba: Batch.T): CARDINAL =
  BEGIN
    RETURN (ba.next - ADR(ba.b[0])) DIV ADRSIZE(Word.T)
  END GetLength;

PROCEDURE Copy (ba: Batch.T): Batch.T =
  VAR
    len := GetLength(ba);
    res := Batch.New(len);
  BEGIN
    SUBARRAY(res.b^, 0, len) := SUBARRAY(ba.b^, 0, len);
    res.clip := ba.clip;
    res.clipped := ba.clipped;
    res.scrollSource := ba.scrollSource;
    res.next := ADR(res.b[0]) + (ba.next - ADR(ba.b[0]));
    res.firstSingle := res.next;
    res.containsPicture := ba.containsPicture;
    IF res.containsPicture THEN PictureRep.IncrementBatch(res); END;
    RETURN res
  END Copy;

PROCEDURE Meet (ba: Batch.T; READONLY clip: Rect.T) =
  BEGIN
    IF NOT Rect.Subset(ba.clip, clip) THEN
      ba.clip := Rect.Meet(ba.clip, clip);
      ba.clipped := ClipState.Unclipped
    END
  END Meet;

<* UNUSED *> PROCEDURE Verify (ba: Batch.T) =
  VAR
    p         : PaintPrivate.CommandPtr;
    start, end: INTEGER;
  BEGIN
    IF ba = NIL THEN RETURN END;
    start := 0;
    end := (ba.next - ADR(ba.b[0])) DIV ADRSIZE(Word.T);
    WHILE start # end DO
      p := LOOPHOLE(ADR(ba.b[start]), PaintPrivate.CommandPtr);
      INC(start, PaintPrivate.CommandLength(p));
      CASE p.command OF
      | PC.TextCom =>
          WITH pText = LOOPHOLE(p, PaintPrivate.TextPtr) DO
            IF pText.szOfRec < ADRSIZE(PaintPrivate.TextRec) THEN
              Crash()
            END
          END;
      | PC.ExtensionCom =>
          WITH pExtension = LOOPHOLE(p, PaintPrivate.ExtensionPtr) DO
            IF pExtension.szOfRec < ADRSIZE(PaintPrivate.ExtensionRec) THEN
              Crash()
            END
          END;
      ELSE                       (*skip*)
      END
    END
  END Verify;

PROCEDURE Clip (ba: Batch.T) =
  BEGIN
    IF ba.clipped = ClipState.Unclipped THEN
      ClipSub(ba.clip, ba.b^, 0, GetLength(ba));
      ba.clipped := ClipState.Clipped
    END
  END Clip;

PROCEDURE ClipSub (READONLY clip   : Rect.T;
                   VAR      ba     : ARRAY OF Word.T;
                            st, len: INTEGER          ) =
  VAR
    start, end: INTEGER;
    p         : PaintPrivate.CommandPtr;
    cw                                  := clip.west;
    ce                                  := clip.east;
    cn                                  := clip.north;
    cs                                  := clip.south;
  BEGIN
    start := st;
    end := st + len;
    WHILE start < end DO
      p := LOOPHOLE(ADR(ba[start]), PaintPrivate.CommandPtr);
      INC(start, PaintPrivate.CommandLength(p));
      VAR
        pw := p.clip.west;
        pe := p.clip.east;
        pn := p.clip.north;
        ps := p.clip.south;
      BEGIN
        IF pw < cw OR pe > ce OR pn < cn OR ps > cs THEN
          IF p.command = PC.TextCom THEN
            WITH t = LOOPHOLE(p, PaintPrivate.TextPtr) DO
              t.props :=
                t.props + PaintPrivate.Props{PaintPrivate.Prop.Clipped}
            END
          END;
          IF pw < cw THEN p.clip.west := cw END;
          IF pe > ce THEN p.clip.east := ce END;
          IF pn < cn THEN p.clip.north := cn END;
          IF ps > cs THEN p.clip.south := cs END;
          IF (p.clip.west >= p.clip.east) OR (p.clip.north >= p.clip.south) THEN
            p.clip := Rect.Empty
          END
        END
      END
    END
  END ClipSub;

TYPE RectPtr = UNTRACED REF Rect.T;

PROCEDURE ClipSubAndTighten (READONLY    clip        : Rect.T;
                             VAR         ba          : ARRAY OF Word.T;
                                         st, len     : INTEGER;
                             VAR (*out*) scrollSource: Rect.T           ):
  Rect.T =
  VAR
    start, end: INTEGER;
    p         : PaintPrivate.CommandPtr;
    join      : Rect.T;
    firstTime : BOOLEAN;
    clipPtr   : RectPtr;
    joinPtr   : RectPtr;
    clipped   : BOOLEAN;
  BEGIN
    firstTime := TRUE;
    clipPtr := ADR(clip);
    joinPtr := ADR(join);
    scrollSource := Rect.Empty;
    start := st;
    end := st + len;
    WHILE start < end DO
      p := LOOPHOLE(ADR(ba[start]), PaintPrivate.CommandPtr);
      INC(start, PaintPrivate.CommandLength(p));
      IF p.command = PC.TextCom THEN
        WITH pText = LOOPHOLE(p, PaintPrivate.TextPtr) DO
          clipped := FALSE;
          IF p.clip.west < clipPtr.west THEN
            clipped := TRUE;
            p.clip.west := clipPtr.west
          END;
          IF p.clip.east > clipPtr.east THEN
            clipped := TRUE;
            p.clip.east := clipPtr.east
          END;
          IF p.clip.north < clipPtr.north THEN
            clipped := TRUE;
            p.clip.north := clipPtr.north
          END;
          IF p.clip.south > clipPtr.south THEN
            clipped := TRUE;
            p.clip.south := clipPtr.south
          END;
          IF clipped THEN
            pText.props := pText.props +
              PaintPrivate.Props{PaintPrivate.Prop.Clipped}
          END
        END;
      ELSE
        IF p.clip.west < clipPtr.west THEN p.clip.west := clipPtr.west END;
        IF p.clip.east > clipPtr.east THEN p.clip.east := clipPtr.east END;
        IF p.clip.north < clipPtr.north THEN
          p.clip.north := clipPtr.north
        END;
        IF p.clip.south > clipPtr.south THEN
          p.clip.south := clipPtr.south
        END;
      END;
      (* Normalize p.clip; join := Rect.Join(join, p.clip): *)
      IF (p.clip.west >= p.clip.east) OR (p.clip.north >= p.clip.south) THEN
        p.clip := Rect.Empty
      ELSIF firstTime THEN
        join := p.clip;
        firstTime := FALSE
      ELSE
        IF joinPtr.west > p.clip.west THEN joinPtr.west := p.clip.west END;
        IF joinPtr.east < p.clip.east THEN joinPtr.east := p.clip.east END;
        IF joinPtr.north > p.clip.north THEN
          joinPtr.north := p.clip.north
        END;
        IF joinPtr.south < p.clip.south THEN
          joinPtr.south := p.clip.south
        END
      END;
      IF p.command = PC.ScrollCom THEN
        WITH pScroll = LOOPHOLE(p, PaintPrivate.ScrollPtr) DO
          scrollSource :=
            Rect.Join(scrollSource,
                      Rect.Move(pScroll.clip, Point.Minus(pScroll.delta)))
        END
      END
    END;
    IF NOT firstTime THEN RETURN join ELSE RETURN Rect.Empty END
  END ClipSubAndTighten;

PROCEDURE Tighten (ba: Batch.T) =
  BEGIN
    IF ba.clipped = ClipState.Unclipped THEN
      ba.clip := ClipSubAndTighten(
                   ba.clip, ba.b^, 0, GetLength(ba), ba.scrollSource)
    ELSIF ba.clipped = ClipState.Clipped THEN
      TightenSub(ba.b^, 0, GetLength(ba), ba.clip)
    END;
    ba.clipped := ClipState.Tight
  END Tighten;

PROCEDURE TightenSub (VAR           btch   : ARRAY OF Word.T;
                                    st, len: INTEGER;
                      VAR (* out *) clip   : Rect.T           ) =
  VAR
    start, end : INTEGER;
    p          : PaintPrivate.CommandPtr;
    clipIsEmpty: BOOLEAN;
  BEGIN
    clipIsEmpty := TRUE;         (* logically *)
    start := st;
    end := st + len;
    WHILE start < end DO
      p := LOOPHOLE(ADR(btch[start]), PaintPrivate.CommandPtr);
      INC(start, PaintPrivate.CommandLength(p));
      WITH r = p.clip DO
        IF r.west < r.east THEN
          IF clipIsEmpty THEN
            clip := p.clip;
            clipIsEmpty := FALSE
          ELSE
            (* join of two non-empty rectangles *)
            IF r.west < clip.west THEN clip.west := r.west END;
            IF r.east > clip.east THEN clip.east := r.east END;
            IF r.north < clip.north THEN clip.north := r.north END;
            IF r.south > clip.south THEN clip.south := r.south END;
          END
        END
      END
    END;
    IF clipIsEmpty THEN clip := Rect.Empty END
  END TightenSub;

PROCEDURE Translate (ba: Batch.T; READONLY delta: Point.T) =
  BEGIN
    TranslateSub(ba.b^, 0, GetLength(ba), delta);
    ba.clip := Rect.Move(ba.clip, delta);
    ba.scrollSource := Rect.Move(ba.scrollSource, delta)
  END Translate;

PROCEDURE TranslateSub (VAR      btch   : ARRAY OF Word.T;
                                 st, len: INTEGER;
                        READONLY delta  : Point.T          ) =
  VAR
    start, end: INTEGER;
    p         : PaintPrivate.CommandPtr;
  BEGIN
    start := st;
    end := st + len;
    WHILE start < end DO
      p := LOOPHOLE(ADR(btch[start]), PaintPrivate.CommandPtr);
      INC(start, PaintPrivate.CommandLength(p));
      p.clip := Rect.Move(p.clip, delta);
      CASE p.command OF
        PC.TextureCom, PC.PixmapCom =>
          WITH pTexture = LOOPHOLE(p, PaintPrivate.PixmapPtr) DO
            pTexture.delta := Point.Add(pTexture.delta, delta)
          END
      | PC.TextCom =>
          WITH pText = LOOPHOLE(p, PaintPrivate.TextPtr) DO
            pText.refpt := Point.Add(pText.refpt, delta)
          END
      | PC.TrapCom =>
          WITH pTrap = LOOPHOLE(p, PaintPrivate.TrapPtr) DO
            pTrap.delta := Point.Add(pTrap.delta, delta);
            pTrap.p1 := Point.Add(pTrap.p1, delta);
            pTrap.p2 := Point.Add(pTrap.p2, delta);
          END
      | PC.ExtensionCom =>
          WITH pExtension = LOOPHOLE(p, PaintPrivate.ExtensionPtr) DO
            pExtension.delta := Point.Add(pExtension.delta, delta)
          END
      ELSE
      END
    END
  END TranslateSub;

(* VAR buffer: Wr.T;

   PROCEDURE Parse(ba: Batch.T): Text.T; CONST RepeatFormat = "Repeat: w
   %-4d e %-4d n %-4d s %-4d\n"; TintFormat = "PaintTint: w %-4d e %-4d n
   %-4d s %-4d\n"; TextureFormat = "PaintTexture: w %-4d e %-4d n %-4d s
   %-4d\n"; TextFormat = "PaintText: w %-4d e %-4d n %-4d s %-4d\n";
   BitmapFormat = "PaintBitmap: w %-4d e %-4d n %-4d s %-4d\n"; TrapFormat
   = "PaintTrap: w %-4d e %-4d n %-4d s %-4d\n"; ExtensionFormat =
   "PaintExtension: w %-4d e %-4d n %-4d s %-4d\n"; ScrollFormat = "Scroll:
   w %-4d e %-4d n %-4d s %-4d\n"; VAR start, end: INTEGER; p:
   PaintPrivate.CommandPtr; pTint: PaintPrivate.TintPtr; pTexture:
   PaintPrivate.TexturePtr; pPixmap: PaintPrivate.PixmapPtr; pText:
   PaintPrivate.TextPtr; pScroll: PaintPrivate.ScrollPtr; pTrap:
   PaintPrivate.TrapPtr; pExtension: PaintPrivate.ExtensionPtr; BEGIN start
   := 0; end := (ba.next - ADR(ba.b^[0])) DIV ADRSIZE(Word.T); WHILE start
   # end DO p := ADR(ba.b^[start]); INC(start,
   PaintPrivate.CommandLength(p)); CASE p.command OF RepeatCom: WITH p.clip
   DO PRINTF(buffer, RepeatFormat, west, east, north, south) END; |
   TintCom: pTint := LOOPHOLE(p, PaintPrivate.TintPtr); WITH pTint.clip DO
   PRINTF(buffer, TintFormat, west, east, north, south) END; | TextureCom,
   PixmapCom: pTexture := LOOPHOLE(p, PaintPrivate.PixmapPtr); WITH
   pTexture.clip DO PRINTF(buffer, TextureFormat, west, east, north, south)
   END; | TextCom: pText := LOOPHOLE(p, PaintPrivate.TextPtr); WITH
   pText.clip DO PRINTF(buffer, TextFormat, west, east, north, south) END;
   | ScrollCom: pScroll := LOOPHOLE(p, PaintPrivate.ScrollPtr); WITH
   pScroll.clip DO PRINTF(buffer, ScrollFormat, west, east, north, south)
   END; | TrapCom: pTrap := LOOPHOLE(p, PaintPrivate.TrapPtr); WITH
   pTrap.clip DO PRINTF(buffer, TrapFormat, west, east, north, south) END;
   | ExtensionCom: pExtension := LOOPHOLE(p, PaintPrivate.ExtensionPtr);
   WITH pExtension.clip DO PRINTF(buffer, ExtensionFormat, west, east,
   north, south) END; ELSE ASSERT(FALSE, "Unimplemented operation") END
   END; RETURN Wr.ToText(buffer) END Parse; *)

PROCEDURE ByteSwap (<*UNUSED*> ba: Batch.T) RAISES {} =
  BEGIN
    Crash();
  END ByteSwap;

PROCEDURE Succ (ba: Batch.T; cptr: PaintPrivate.CommandPtr):
  PaintPrivate.CommandPtr =
  BEGIN
    IF cptr = NIL THEN
      RETURN LOOPHOLE(ADR(ba.b[0]), PaintPrivate.CommandPtr)
    END;
    INC(cptr, PaintPrivate.CommandLength(cptr) * ADRSIZE(Word.T));
    IF cptr = ba.next THEN RETURN NIL END;
    RETURN cptr
  END Succ;

PROCEDURE SetPicture (ba: Batch.T) =
  BEGIN
    ba.containsPicture := TRUE;
  END SetPicture;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN
END BatchUtil.
