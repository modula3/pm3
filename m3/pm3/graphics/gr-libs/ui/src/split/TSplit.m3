(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Wed Mar  8 18:16:38 PST 1995 by msm      *)
(*      modified on Mon Feb 24 13:54:48 PST 1992 by muller   *)
(*      modified on Sun Nov 10 19:41:25 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE TSplit;

IMPORT VBT, Split, ProperSplit, VBTClass, Point, Rect,
  Axis, Region;

REVEAL 
  Private = ProperSplit.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT 
    current: VBT.T := NIL;
    fickle: BOOLEAN;
  OVERRIDES
    reshape := Reshape;
    repaint := Repaint;
    redisplay := Redisplay;
    shape := Shape;
    replace := Replace;
    insert := Insert;
    locate := Locate;
    newShape := NewShape;
    axisOrder := AxisOrder;
    init := Be
  END;

PROCEDURE Be(v: T; fickle: BOOLEAN): T = 
  BEGIN v.fickle := fickle; RETURN v END Be;

PROCEDURE Shape(v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange RAISES {} =
  BEGIN
    IF (v.fickle AND v.current = NIL) OR v.succ(NIL) = NIL THEN 
      RETURN VBT.DefaultShape
    ELSIF v.fickle THEN
      RETURN VBTClass.GetShape(v.current, ax, n)
    ELSE
      VAR 
        ch := v.succ(NIL); 
        sh := VBT.SizeRange{lo := 0, pref := 0, hi := LAST(INTEGER)}; 
      BEGIN 
        WHILE ch # NIL DO
          VAR shP := VBTClass.GetShape(ch, ax, n); BEGIN
            sh.lo := MAX(sh.lo, shP.lo);
            sh.hi := MIN(sh.hi, shP.hi);
            sh.pref := MAX(sh.pref, shP.pref);
            ch := v.succ(ch)
          END
        END;
        sh.hi := MAX(sh.hi, sh.lo + 1);
        sh.pref := MIN(sh.pref, sh.hi - 1);
        RETURN sh
      END
    END
  END Shape;

PROCEDURE Cons(ch0, ch1, ch2, ch3, ch4: VBT.T := NIL; fickle := TRUE): T =
  <*FATAL Split.NotAChild*>
  VAR v := NEW(T);
  BEGIN
    EVAL Be(v, fickle);
    Split.AddChild(v, ch0, ch1, ch2, ch3, ch4);
    IF ch0 # NIL THEN SetCurrent(v, ch0) END;
    RETURN v
  END Cons;

PROCEDURE Redisplay(v: T) RAISES {} =
  BEGIN
    IF (v.current # NIL) AND NOT Rect.Equal(v.current.domain, v.domain) THEN
      VBTClass.Reshape(v.current, v.domain, v.domain);
    END
  END Redisplay;

PROCEDURE SetCurrent(v: T; ch: VBT.T) RAISES {Split.NotAChild} =
  BEGIN
    IF ch = v.current THEN RETURN END;
    IF (ch # NIL) AND (ch.parent # v) THEN RAISE Split.NotAChild END;
    IF v.fickle THEN
      IF ch = NIL OR v.current = NIL OR
       VBTClass.GetShapes(ch) # VBTClass.GetShapes(v.current, FALSE)
      OR ch.axisOrder() # v.current.axisOrder()
      THEN
        VBT.NewShape(v)
      END
    ELSIF ch # NIL AND AxisOrder(v) # ch.axisOrder() 
       OR ch = NIL AND AxisOrder(v) # ProperSplit.T.axisOrder(v) THEN
      VBT.NewShape(v)
    END;
    IF v.current # NIL THEN
      VBTClass.Reshape(v.current, Rect.Empty, Rect.Empty)
    END;
    v.current := ch;
    VBT.Mark(v);
    VBTClass.LocateChanged(v)
  END SetCurrent;

PROCEDURE GetCurrent(v: T): VBT.T =
  BEGIN
    RETURN v.current;
  END GetCurrent;

PROCEDURE Insert (v: T; pred, ch: VBT.T) =
  BEGIN
    Public.insert(v, pred, ch);
    LOCK ch DO VBTClass.SetShortCircuit(ch) END;
    IF NOT v.fickle THEN VBT.NewShape(v) END
  END Insert;

PROCEDURE Replace (v: T; ch, new: VBT.T) RAISES {} =
  VAR chsh := VBTClass.GetShapes(ch, FALSE);
  BEGIN
    IF v.current = ch THEN v.current := new END;
    Public.replace(v, ch, new);
    IF new # NIL THEN LOCK new DO VBTClass.SetShortCircuit(new) END END;
    IF NOT v.fickle
         OR v.current = new
              AND (new = NIL OR VBTClass.GetShapes(new, FALSE) # chsh) THEN
      VBT.NewShape(v)
    END
  END Replace;
  
PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  BEGIN
    IF v.current # NIL THEN
      VBTClass.Reshape(v.current, cd.new, cd.saved)
    END
  END Reshape;

PROCEDURE Repaint(v: T; READONLY badR: Region.T) RAISES {} =
  BEGIN
    IF v.current # NIL THEN
      VBTClass.Repaint(v.current, badR)
    END
  END Repaint;

PROCEDURE Locate(
    v: T;
    <*UNUSED*> READONLY pt: Point.T;
    VAR rect: Rect.T): VBT.T RAISES {} =
  BEGIN
    IF v.current # NIL THEN
      rect := v.current.domain;
      RETURN v.current
    ELSE
      rect := Rect.Empty;
      RETURN NIL
    END
  END Locate;

PROCEDURE NewShape(v: T; ch: VBT.T) RAISES {} =
  BEGIN
    IF NOT v.fickle OR v.current = ch THEN VBT.NewShape(v) END
  END NewShape;

PROCEDURE AxisOrder(v: T): Axis.T = 
  BEGIN
    IF v.current = NIL THEN 
      RETURN ProperSplit.T.axisOrder(v)
    ELSE
      RETURN v.current.axisOrder()
    END
  END AxisOrder;
  
BEGIN
END TSplit.
