(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue Jan 31 09:09:48 PST 1995 by kalsow *)
(*      modified on Mon Nov 22 13:57:33 PST 1993 by steveg *)
(*      modified on Fri Nov  6 19:55:56 PST 1992 by msm    *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XScrnCrsr;

IMPORT Cursor, Rect, ScrnCursor, ScrnPixmap, Text, TrestleComm, X, XClient,
       XClientF, XCursors, XScreenType, TrestleOnX, XScrnPxmp, ScreenType;

REVEAL T = T_Pub BRANDED OBJECT cursorGC, cursorGC2: X.GC := NIL;  END;

TYPE
  CursorOracle = ScrnCursor.Oracle OBJECT
                   st: XScreenType.T;
                 OVERRIDES
                   load    := CursorRegister;
                   list    := CursorList;
                   lookup  := CursorLookup;
                   builtIn := CursorBuiltIn
                 END;
  XCursor = ScrnCursor.T OBJECT
            OVERRIDES
              unload   := CursorUnregister;
              localize := CursorLocalize
            END;

PROCEDURE NewOracle (scrn: XScreenType.T): ScrnCursor.Oracle =
  BEGIN
    RETURN NEW(CursorOracle, st := scrn)
  END NewOracle;

PROCEDURE NullCursor (dpy: X.DisplayStar; w: X.Drawable): X.Cursor
  RAISES {TrestleComm.Failure} <* LL.sup = trsl, such that trsl.dpy = dpy *> =
  VAR
    rgb               := X.XColor{0, 0, 0, 0, 0, 0};
    zero: X.Pixmap;
    gc  : X.GC;
    gcv : X.XGCValues;
    res : X.Cursor;
  BEGIN
    TRY
      zero := X.XCreatePixmap(dpy, w, 1, 1, 1);
      gcv.function := X.GXclear;
      gc := X.XCreateGC(dpy, zero, X.GCFunction, ADR(gcv));
      X.XFillRectangle(dpy, zero, gc, 0, 0, 1, 1);
      X.XFreeGC(dpy, gc);
      res :=
        X.XCreatePixmapCursor(dpy, zero, zero, ADR(rgb), ADR(rgb), 0, 0);
      X.XFreePixmap(dpy, zero);
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
    RETURN res
  END NullCursor;

PROCEDURE CursorRegister (                    orc: CursorOracle;
                                     READONLY c  : ScrnCursor.Raw;
                          <*UNUSED*>          nm : TEXT             := NIL):
  ScrnCursor.T RAISES {TrestleComm.Failure} =
  VAR
    gcv   : X.XGCValues;
    res                 := NEW(XCursor);
    fg, bg: X.XColor;
  BEGIN
    TRY
    IF (c.plane1 = NIL) OR (c.plane2 = NIL) OR (c.plane1.depth # 1)
         OR (c.plane2.depth # 1) OR Rect.IsEmpty(c.plane1.bounds)
         OR (c.plane1.bounds # c.plane2.bounds) THEN
      RETURN ScrnCursor.DontCare
    END;
    WITH st   = orc.st,
         trsl = st.trsl,
         dpy  = trsl.dpy DO
      TrestleOnX.Enter(trsl);
      TRY
        WITH mask   = XScrnPxmp.PixmapFromRaw(st.bits, c.plane1),
             source = XScrnPxmp.PixmapFromRaw(st.bits, c.plane2)  DO
          IF st.cursorGC = NIL THEN
            gcv.function := X.GXor;
            gcv.graphics_exposures := X.False;
            st.cursorGC := X.XCreateGC(dpy, mask, X.GCFunction, ADR(gcv))
          END;
          IF st.cursorGC2 = NIL THEN
            gcv.function := X.GXorInverted;
            gcv.graphics_exposures := X.False;
            st.cursorGC2 := X.XCreateGC(dpy, mask, X.GCFunction, ADR(gcv))
          END;
          X.XCopyArea(dpy, source, mask, st.cursorGC, 0, 0,
                      Rect.HorSize(c.plane1.bounds),
                      Rect.VerSize(c.plane1.bounds), 0, 0);
          IF st.color THEN
            fg.red := ROUND(FLOAT(16_ffff) * c.color1.r);
            fg.green := ROUND(FLOAT(16_ffff) * c.color1.g);
            fg.blue := ROUND(FLOAT(16_ffff) * c.color1.b);
            bg.red := ROUND(FLOAT(16_ffff) * c.color2.r);
            bg.green := ROUND(FLOAT(16_ffff) * c.color2.g);
            bg.blue := ROUND(FLOAT(16_ffff) * c.color2.b)
          ELSIF st.depth # 1 THEN
            fg.red := ROUND(FLOAT(16_ffff) * c.color1.gray);
            fg.green := fg.red;
            fg.blue := fg.red;
            bg.red := ROUND(FLOAT(16_ffff) * c.color2.gray);
            bg.green := bg.red;
            bg.blue := bg.red
          ELSE
            IF c.color1.bw = Cursor.BW.UseBg THEN
              fg.red := 16_ffff;
              (* gross hack for broken Macintosh XServer *)
              X.XCopyArea(dpy, mask, source, st.cursorGC2, 0, 0,
                          Rect.HorSize(c.plane1.bounds),
                          Rect.VerSize(c.plane1.bounds), 0, 0);
            ELSE
              fg.red := 0
            END;
            fg.green := fg.red;
            fg.blue := fg.red;
            IF c.color2.bw = Cursor.BW.UseBg THEN
              bg.red := 16_ffff
            ELSE
              bg.red := 0
            END;
            bg.green := bg.red;
            bg.blue := bg.red
          END;
          res.id :=
            X.XCreatePixmapCursor(dpy, source, mask, ADR(fg), ADR(bg),
                                  c.hotspot.h - c.plane1.bounds.west,
                                  c.hotspot.v - c.plane1.bounds.north);
          X.XFreePixmap(dpy, mask);
          X.XFreePixmap(dpy, source)
        END
      FINALLY
        TrestleOnX.Exit(trsl)
      END
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
    RETURN res
  END CursorRegister;

PROCEDURE CursorList (<*UNUSED*> orc       : CursorOracle;
                      <*UNUSED*> pat       : TEXT;
                      <*UNUSED*> maxResults: CARDINAL       := 1):
  REF ARRAY OF TEXT =
  BEGIN
    RETURN NIL
  END CursorList;

PROCEDURE CursorLookup (orc: CursorOracle; name: TEXT): ScrnCursor.T
  RAISES {TrestleComm.Failure} =
  VAR
    trsl: XClient.T := orc.st.trsl;
    res             := NEW(XCursor);
  BEGIN
    TRY
    FOR i := 0 TO LAST(XCursors.Names) DO
      IF Text.Equal(name, XCursors.Names[i]) THEN
        TrestleOnX.Enter(trsl);
        TRY
          res.id := X.XCreateFontCursor(trsl.dpy, 2 * i);
          RETURN res
        FINALLY
          TrestleOnX.Exit(trsl)
        END
      END
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
    RETURN NIL
  END CursorLookup;

PROCEDURE CursorBuiltIn (orc: CursorOracle; cs: Cursor.Predefined):
  ScrnCursor.T =
  VAR xid: X.Cursor;
  BEGIN
    WITH st   = orc.st,
         trsl = st.trsl,
         dpy  = trsl.dpy DO
      TRY
        TrestleOnX.Enter(trsl);
        TRY
          CASE cs OF
            Cursor.DontCare.cs => RETURN ScrnCursor.DontCare
          | Cursor.TextPointer.cs =>
              xid := X.XCreateFontCursor(dpy, 68 (*X.XC_left_ptr*))
          | Cursor.NotReady.cs =>
              xid := X.XCreateFontCursor(dpy, 150 (*X.XC_watch*))
          ELSE
            xid := X.None
          END
        FINALLY
          TrestleOnX.Exit(trsl)
        END
      EXCEPT
        X.Error, TrestleComm.Failure => RETURN ScrnCursor.DontCare
      END
    END;
    RETURN NEW(XCursor, id := xid)
  END CursorBuiltIn;

PROCEDURE CursorLocalize (<*UNUSED*> cs: XCursor): ScrnCursor.Raw
  RAISES {ScrnCursor.Failure} =
  BEGIN
    RAISE ScrnCursor.Failure
  END CursorLocalize;

PROCEDURE CursorUnregister (<*UNUSED*> cs: XCursor) =
  BEGIN
  END CursorUnregister;

BEGIN
END XScrnCrsr.

