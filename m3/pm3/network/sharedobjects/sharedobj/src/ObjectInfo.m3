(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Wed Jun 28 19:12:54 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  3 13:12:34 1996
 * Update Count    : 13
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 22:00:47  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/26 00:04:04  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.2  2001/12/02 13:41:16  wagner
 * add copyright notes, fix overrides for cm3, and make everything compile(except tests)
 *
 * added: sharedobj/COPYRIGHT-COLUMBIA
 * added: sharedobj/src/COPYRIGHT-COLUMBIA
 * modified: sharedobj/src/LocalObjectSpace.i3
 * modified: sharedobj/src/LocalObjectSpace.m3
 * modified: sharedobj/src/Message.i3
 * modified: sharedobj/src/Message.m3
 * modified: sharedobj/src/ObjCopy.i3
 * modified: sharedobj/src/ObjCopy.m3
 * modified: sharedobj/src/ObjectInfo.i3
 * modified: sharedobj/src/ObjectInfo.m3
 * modified: sharedobj/src/ObjectSpace.i3
 * modified: sharedobj/src/ObjectSpace_FindObjCallBack_v1.i3
 * modified: sharedobj/src/ObjectSpace_FindObjCallBack_v1.m3
 * modified: sharedobj/src/ObjectSpace_FindObjCallBack_v2.i3
 * modified: sharedobj/src/ObjectSpace_FindObjCallBack_v2.m3
 * modified: sharedobj/src/ObjectSpace_T_v1.i3
 * modified: sharedobj/src/ObjectSpace_T_v1.m3
 * modified: sharedobj/src/ObjectSpace_T_v2.i3
 * modified: sharedobj/src/ObjectSpace_T_v2.m3
 * modified: sharedobj/src/SharedObj.i3
 * modified: sharedobj/src/SharedObjError.i3
 * modified: sharedobj/src/SharedObjF.i3
 * modified: sharedobj/src/SharedObjF.m3
 * modified: sharedobj/src/SharedObjRT.i3
 * modified: sharedobj/src/SharedObjRT.m3
 * modified: sharedobj/src/SharedObjRTF.i3
 * modified: sharedobj/src/SharedObjRep.i3
 * modified: sharedobj/src/SharedObjRep.m3
 * modified: sharedobj/src/SharedObjStubLib.i3
 * modified: sharedobj/src/SharedObjStubLib.m3
 * modified: sharedobj/src/SpaceConn.i3
 * modified: sharedobj/src/SpaceConn.m3
 * modified: sharedobj/src/WeakerRef.i3
 * modified: sharedobj/src/WeakerRef.m3
 * modified: sharedobj/src/m3makefile
 * modified: sharedobj/src/m3overrides
 * modified: sharedobj/tests/netobjtest/src/m3makefile
 * modified: sharedobj/tests/obsequence/src/m3makefile
 * modified: sharedobj/tests/tracker/src/m3makefile
 *
 * Revision 1.1.1.1  2001/12/02 13:14:14  wagner
 * Blair MacIntyre's sharedobj package
 *
 * Revision 1.3  1997/01/23 15:27:14  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.2  1996/11/22 19:00:52  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

MODULE ObjectInfo;

IMPORT EventNumber, Fmt, EventConnList, EventWireRep, SpaceConn,
       ObjCopyList, Thread;

PROCEDURE Init(self: T; seqNo: EventNumber.T): T =
  BEGIN
    self.seqNo := EventNumber.New(seqNo);
    self.waiting := NEW(REF ARRAY OF Blocked, 1);
    self.waiting[0].used := FALSE;
    self.waiting[0].cv := NEW(Thread.Condition);
    self.waiting[0].en := EventNumber.New();
    RETURN self;
  END Init;

(* A method call will need to be able to block if this host is not the
   sequencer.  For now we will always pick a slot for it in the array,
   but eventually may optimize this.   Probably won't matter, though.
   The majority of objects will not have concurrent writers, so there
   will only be one entry in the table, so this will be fast. *)

PROCEDURE PickThreadSlot(self: T) : CARDINAL =
  BEGIN
    (* Look for an unused one *)
    FOR i := FIRST(self.waiting^) TO LAST(self.waiting^) DO
      IF self.waiting[i].used = FALSE THEN
        (* Use it! *)
        self.waiting[i].used := TRUE;
        RETURN i;
      END;
    END;
    (* Add one on to the end. *)
    WITH num = NUMBER(self.waiting^), 
         newBlocked = NEW(REF ARRAY OF Blocked, num+1) DO
      SUBARRAY(newBlocked^, 0, num) := self.waiting^;
      self.waiting := newBlocked;
      self.waiting[num].used := TRUE;
      self.waiting[num].cv := NEW(Thread.Condition);
      self.waiting[num].en := EventNumber.New();
      RETURN num;
    END;
  END PickThreadSlot;

PROCEDURE ReleaseThreadSlot(self: T; id: CARDINAL) =
  BEGIN
    self.waiting[id].used := FALSE;
  END ReleaseThreadSlot;

PROCEDURE ToText(o: T): TEXT =
  VAR ret : TEXT;
      count: INTEGER := 0;
  BEGIN
    ret := "{" & EventWireRep.ToText(o.wrep) & ")seq(" &
               o.seqNo.fmt(10) & ")conns(";
    IF o.conns # NIL THEN
      ret := ret & Fmt.Int(EventConnList.Length(o.conns)) & ")flags(";
    ELSE
      ret := ret & "0)flags(";
    END;
    IF o.isStandalone THEN
      ret := ret & "s1";
    ELSE
      ret := ret & "s0";
    END;
    IF o.isOwner THEN
      ret := ret & "o1";
    ELSE
      ret := ret & "o0";
    END;
    IF o.isLocker THEN
      ret := ret & "l1";
    ELSE
      ret := ret & "l0";
    END;
    IF o.hasCopy THEN
      ret := ret & "c1)";
    ELSE
      ret := ret & "c0)";
    END;
    IF o.sequencer # NIL THEN
      ret := ret & "seq(" & SpaceConn.ToText(o.sequencer)&")";
    END;
    IF o.clients # NIL THEN
      ret := ret & "clients(" & Fmt.Int(ObjCopyList.Length(o.clients)) & ")";
    ELSE
      ret := ret & "clients(0)";
    END;
    IF o.fastClients # NIL THEN
      ret := ret & "fastC(" & Fmt.Int(ObjCopyList.Length(o.fastClients)) & ")";
    ELSE
      ret := ret & "fastC(0)";
    END;
    ret := ret & "waiting(";
    FOR i := FIRST(o.waiting^) TO LAST(o.waiting^) DO
      IF o.waiting[i].used THEN
        INC(count);
      END;
    END;
    RETURN ret & Fmt.Int(count) & "}";
  END ToText;

BEGIN
END ObjectInfo.
