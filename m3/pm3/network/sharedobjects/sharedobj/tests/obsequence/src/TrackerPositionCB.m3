(*
 * TrackerPositionCB.m3 -- the autogenerated callback object
 *)

MODULE TrackerPositionCB EXPORTS TrackerPositionCB, TrackerPositionProxy;

IMPORT TrackerPosition, SharedObjRep, SharedObjStubLib,
       WeakRef, WeakerRef, WeakRefList, WeakRefListFuncs;

REVEAL 
  T = Public BRANDED OBJECT 
    obj: TrackerPosition.T;
  OVERRIDES
    init := InitCallback;
    pre_set := Pre_set;
    pre_anyChange := Pre_anyChange;
    post_set := Post_set;
    post_anyChange := Post_anyChange;
  END;

PROCEDURE InitCallback(self: T; obj: TrackerPosition.T): T =
  VAR
    wref := NEW(WeakerRef.T, weakRef := WeakRef.FromRef(self, CleanupCB), 
                ready := TRUE);
  BEGIN
    self.obj := obj;
    IF MkProxyCB # NIL AND self.proxy = NIL THEN
      MkProxyCB (self);
    END;

    SharedObjStubLib.AcquireWriteLock(obj);
    TRY
      obj.callbacks := WeakRefList.Cons(wref, obj.callbacks);
    FINALLY
      SharedObjStubLib.ReleaseWriteLock(obj);
    END;
    RETURN self;
  END InitCallback; 

PROCEDURE CleanupCB (READONLY wref: WeakRef.T; ref: REFANY) =
  VAR
    cb             := NARROW(ref, T);
    weakerRef := NEW(WeakerRef.T, weakRef := wref);
  BEGIN
    SharedObjStubLib.AcquireWriteLock(cb.obj);
    TRY
      (* Callback is gone, so delete it. *)
      EVAL WeakRefListFuncs.DeleteD(cb.obj.callbacks, weakerRef);
    FINALLY
      SharedObjStubLib.ReleaseWriteLock(cb.obj);
    END;
  END CleanupCB;

PROCEDURE Pre_anyChange(self: T; READONLY obj: TrackerPosition.T) =
  BEGIN
    (* Default does nothing. *)
    IF self.proxy # NIL THEN
      NARROW (self.proxy, CallbackProxy).pre_anyChange (obj);
    END;
  END Pre_anyChange;

PROCEDURE Pre_set(self: T; READONLY obj: TrackerPosition.T; 
                  READONLY val: TrackerPosition.Data): BOOLEAN =
  BEGIN
    (* Default calls proxy or does nothing. *)
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, CallbackProxy).pre_set (obj, val);
    END;
    RETURN FALSE;
  END Pre_set; 

PROCEDURE Post_anyChange(self: T; READONLY obj: TrackerPosition.T) =
  BEGIN
    (* Default does nothing. *)
    IF self.proxy # NIL THEN
      NARROW (self.proxy, CallbackProxy).post_anyChange (obj);
    END;
  END Post_anyChange;

PROCEDURE Post_set(self: T; READONLY obj: TrackerPosition.T; 
                  READONLY val: TrackerPosition.Data): BOOLEAN =
  BEGIN
    (* Default calls proxy or does nothing. *)
    IF self.proxy # NIL THEN
      RETURN NARROW (self.proxy, CallbackProxy).post_set (obj, val);
    END;
    RETURN FALSE;
  END Post_set; 

BEGIN
END TrackerPositionCB.
