(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: ETAgent.m3, by cgn, Tue Apr 21 22:00:25 1987 *)
(* Last modified on Fri Oct  2 11:39:24 PDT 1992 by msm     *)
(*      modified on Tue Apr  7 18:12:16 1992 by steveg  *)
(*      modified on Mon Feb 24 13:53:05 PST 1992 by muller  *)
(*      modified on Sun Nov 10 19:21:05 PST 1991 by gnelson *)
<*PRAGMA LL*>

MODULE ETAgent;

IMPORT VBT, Filter, FilterClass, SelectQueue, VBTClass, Thread, Word;

TYPE SelectionRec = RECORD
    v: VBT.T;
    ts: VBT.TimeStamp
  END;

TYPE SelArray = REF ARRAY OF SelectionRec;

REVEAL
  T = Filter.T BRANDED OBJECT <* LL.sup = SELF *>
    owners: SelArray := NIL;
    lost, forgery := SelectQueue.Empty;
    forked, covered := FALSE;
  OVERRIDES
    acquire := Acquire;
    release := Release;
    forge := Forge;
    read := Read;
    write := Write;
    readUp := ReadUp;
    writeUp := WriteUp;
    put := Put;
    misc := MiscCode;
    key := KeyCode;
    mouse := Mouse;
    init := Be
  END;

PROCEDURE Be(v: T; ch: VBT.T): Filter.T = BEGIN 
  v.owners := NEW(SelArray, 0);
  EVAL Filter.T.init(v, ch);
  RETURN v
END Be;

PROCEDURE New(ch: VBT.T): T = 
  BEGIN 
    RETURN Be(NEW(T), ch)
  END New;

TYPE LostClosure = Thread.SizedClosure OBJECT
    v: T;
  OVERRIDES
    apply := DeliverLost
  END;

PROCEDURE DeliverLost(self: LostClosure): REFANY RAISES {} =
  <*FATAL SelectQueue.Exhausted*>
  VAR v := self.v; rec: SelectQueue.Elem; 
  BEGIN
    LOCK VBT.mu DO
      LOOP
        LOCK v DO
          IF SelectQueue.IsEmpty(v.lost) THEN v.forked := FALSE; EXIT END;
          rec := SelectQueue.Remove(v.lost)
        END;
        VBTClass.Misc(rec.v, rec.cd)
      END
    END;
    RETURN NIL
  END DeliverLost;

PROCEDURE GetSel(v: T; sel: VBT.Selection): SelectionRec =
<* LL.sup = v *>
  BEGIN
    ExtendSel(v.owners, sel);
    RETURN v.owners[sel.sel]
  END GetSel;
  
PROCEDURE Lose(v: T; sel: VBT.Selection): BOOLEAN <* LL.sup = v *> =
  (* Enqueue a lost code to the owner of sel, if any *)
  VAR ch := GetSel(v, sel).v; BEGIN
    IF ch = NIL THEN RETURN FALSE END;
    v.owners[sel.sel].v := NIL;
    SelectQueue.Insert(v.lost, SelectQueue.Elem{ch, 
      VBT.MiscRec{VBT.Lost, VBT.NullDetail, 0, sel}});
    IF NOT v.forked AND NOT v.covered THEN
      v.forked := TRUE;
      EVAL Thread.Fork(NEW(LostClosure, v:= v, stackSize := 20000))
    END;
    RETURN TRUE
  END Lose;

PROCEDURE CompareTimeStamp(t1, t2: VBT.TimeStamp): INTEGER =
(* Return something <, =, or > zero according as t1 < t2, 
   t1 = t2, t1 > t2. *)
  BEGIN
    RETURN Word.Minus(t1, t2) 
  END CompareTimeStamp;

PROCEDURE Acquire(
  v: T; 
  <*UNUSED*>ch:VBT.T; w: VBT.T; 
  s: VBT.Selection; 
  ts: VBT.TimeStamp) 
  RAISES {VBT.Error} =
  BEGIN (* LL < v *)
    LOCK v DO
      IF v.parent = NIL THEN 
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
      ELSIF GetSel(v, s).v # NIL 
        AND CompareTimeStamp(v.owners[s.sel].ts, ts) > 0
        OR ts = 0
      THEN
        RAISE VBT.Error(VBT.ErrorCode.EventNotCurrent)
      ELSE
        IF NOT Lose(v, s) THEN v.parent.acquire(v, v, s, ts) END;
        v.owners[s.sel].v := w;
        v.owners[s.sel].ts := ts
      END
    END
  END Acquire;

PROCEDURE Release(
  v: T; 
  <*UNUSED*>ch:VBT.T; w: VBT.T; 
  s: VBT.Selection) RAISES {} =
  BEGIN (* LL < v *)
    LOCK v DO
      IF v.parent # NIL AND w = GetSel(v,s).v AND Lose(v, s) THEN
        v.parent.release(v, v, s) 
      END
    END
  END Release;

PROCEDURE Forge(
  v: T; 
  <*UNUSED*>ch:VBT.T; w: VBT.T; 
  type: VBT.MiscCodeType;
  READONLY detail: ARRAY [0..1] OF INTEGER)
  RAISES {VBT.Error} =
  BEGIN (* LL < v *)
    LOCK v DO
      IF v.parent = NIL THEN 
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
      ELSE
        v.parent.forge(v, v, VBT.TrestleInternal, VBT.NullDetail);
        SelectQueue.Insert(v.forgery, SelectQueue.Elem{w, 
          VBT.MiscRec{type, detail, 0, VBT.Forgery}})
      END
    END
  END Forge;

PROCEDURE Read(
  v: T; 
  s: VBT.Selection; 
  tc: CARDINAL)
  : VBT.Value RAISES {VBT.Error}  =
  VAR owner: VBT.T; BEGIN 
    LOCK v DO owner := GetSel(v, s).v END;
    IF owner = NIL THEN 
      RAISE VBT.Error(VBT.ErrorCode.UnownedSelection)
    ELSE
      RETURN owner.read(s, tc)
    END
  END Read;

PROCEDURE Write(
  v: T; 
  s: VBT.Selection; 
  val: VBT.Value;
  tc: CARDINAL) 
  RAISES {VBT.Error} =
  VAR owner: VBT.T; BEGIN 
    LOCK v DO owner := GetSel(v, s).v END;
    IF owner = NIL THEN 
      RAISE VBT.Error(VBT.ErrorCode.UnownedSelection)
    ELSE
      owner.write(s, val, tc)
    END
  END Write;

PROCEDURE ReadUp(
  v: T; 
  <*UNUSED*> ch, w: VBT.T; 
  s: VBT.Selection; 
  ts: VBT.TimeStamp; 
  tc: CARDINAL)
  : VBT.Value RAISES {VBT.Error}  =
  VAR p: VBT.Split; owner: VBT.T; 
  BEGIN 
    LOCK v DO p := v.parent; owner := GetSel(v,s).v END;
    IF owner # NIL THEN 
      RETURN owner.read(s, tc)
    ELSIF p = NIL THEN
      RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    ELSE
      RETURN p.readUp(v, v, s, ts, tc)
    END 
  END ReadUp;

PROCEDURE WriteUp(
  v: T; 
  <*UNUSED*> ch, w: VBT.T; 
  s: VBT.Selection; 
  ts: VBT.TimeStamp; 
  val: VBT.Value;
  tc: CARDINAL) 
  RAISES {VBT.Error} =
  VAR p: VBT.Split; owner: VBT.T;
  BEGIN 
    LOCK v DO p := v.parent; owner := GetSel(v,s).v END;
    IF owner # NIL THEN 
      owner.write(s, val, tc)
    ELSIF p = NIL THEN
      RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    ELSE
      p.writeUp(v, v, s, ts, val, tc)
    END 
  END WriteUp;


PROCEDURE FlushQueue(v: T; VAR q: SelectQueue.T) =
  <*FATAL SelectQueue.Exhausted*>
  VAR rec: SelectQueue.Elem; BEGIN
    LOOP
      LOCK v DO
        IF SelectQueue.IsEmpty(q) THEN RETURN END;
        rec := SelectQueue.Remove(q)
      END;
      VBTClass.Misc(rec.v, rec.cd)
    END
  END FlushQueue;

PROCEDURE MiscCode(v: T; READONLY cd: VBT.MiscRec) =
  <*FATAL SelectQueue.Exhausted*>
  VAR elem: SelectQueue.Elem; 
  BEGIN
    LOCK v DO v.covered := TRUE END;
    TRY
      FlushQueue(v, v.lost);
      IF cd.selection = VBT.Forgery
         AND cd.type = VBT.TrestleInternal 
      THEN
        LOCK v DO 
          IF SelectQueue.IsEmpty(v.forgery) THEN RETURN END;
          elem := SelectQueue.Remove(v.forgery) 
        END;
        elem.cd.time := cd.time;
        VBTClass.Misc(elem.v, elem.cd)
      ELSIF cd.type = VBT.Deleted OR cd.type = VBT.Disconnected THEN
        ReleaseSelections(v);
        Filter.T.misc(v, cd)
      ELSIF cd.type = VBT.Lost THEN
        EVAL Lose(v, cd.selection)
      ELSE
        Filter.T.misc(v, cd) 
      END
    FINALLY
      LOCK v DO v.covered := FALSE END;
      FlushQueue(v, v.lost)
    END
  END MiscCode;

PROCEDURE ReleaseSelections (v: T) =
  BEGIN
    FOR s := FIRST(v.owners^) TO LAST(v.owners^) DO
      EVAL Lose(v, VBT.Selection{s})
    END;
    FlushQueue(v, v.lost);
    FlushQueue(v, v.forgery);
  END ReleaseSelections;

PROCEDURE ExtendSel(VAR sa: SelArray; s: VBT.Selection) =
  VAR n := NUMBER(sa^); na: SelArray; BEGIN
    IF s.sel > LAST(sa^) THEN
      na := NEW(SelArray, MAX(2*n, s.sel+1)); 
      SUBARRAY(na^, 0, n) := sa^;
      FOR i := n TO LAST(na^) DO na[i] := SelectionRec{NIL, 0} END;
      sa := na
    END
  END ExtendSel;

PROCEDURE KeyCode(v: T; READONLY cd: VBT.KeyRec) =
  VAR owner: VBT.T; BEGIN
    FlushQueue(v, v.lost);
    LOCK v DO 
      v.covered := TRUE;
      ExtendSel(v.owners, VBT.KBFocus);
      owner := v.owners[VBT.KBFocus.sel].v 
    END;
    IF owner # NIL THEN VBTClass.Key(owner, cd) END;
    LOCK v DO v.covered := FALSE END;
    FlushQueue(v, v.lost)
  END KeyCode;

PROCEDURE Put(
  v: T;
  <*UNUSED*>ch, w: VBT.T;
  s: VBT.Selection;
  ts: VBT.TimeStamp;
  type: VBT.MiscCodeType;
  READONLY detail: ARRAY [0..1] OF INTEGER) RAISES {VBT.Error} =
  BEGIN
    LOCK v DO
      IF v.parent = NIL THEN RAISE VBT.Error(VBT.ErrorCode.Uninstalled) END;
      IF GetSel(v,s).v # NIL THEN
        IF ts = 0 OR CompareTimeStamp(ts, v.owners[s.sel].ts) < 0 THEN
          RAISE VBT.Error(VBT.ErrorCode.EventNotCurrent)
        END;
        SelectQueue.Insert(v.lost, SelectQueue.Elem{v.owners[s.sel].v, 
          VBT.MiscRec{type, detail, ts, s}});
        IF NOT v.forked AND NOT v.covered THEN
          v.forked := TRUE;
          EVAL Thread.Fork(NEW(LostClosure, v:= v, stackSize := 20000))
        END;
      ELSE
        v.parent.put(v, v, s, ts, type, detail)
      END
    END
  END Put;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    FlushQueue(v, v.lost);
    LOCK v DO v.covered := TRUE END;
    Filter.T.mouse(v, cd);
    LOCK v DO v.covered := FALSE END;
    FlushQueue(v, v.lost)
  END Mouse;

BEGIN END ETAgent.
