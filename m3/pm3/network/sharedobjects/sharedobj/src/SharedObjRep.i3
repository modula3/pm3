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
 * Created On      : Mon Apr 17 13:49:21 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Mon Dec  2 21:27:36 1996
 * Update Count    : 160
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 22:00:49  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/26 00:04:08  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.2  2001/12/02 13:41:17  wagner
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
 * Revision 1.5  1997/01/23 15:27:19  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.4  1996/11/22 19:02:39  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

(* The module "SharedRep" provides the part of the implementation of shared
   objects "SharedObj.T" that is independent of any particular shared
   object.  It is not intended that any client accesses any of the fields
   of the SharedObj.T directly.  Only the generated code references this
   interface.

   *)

INTERFACE SharedObjRep;

IMPORT SharedObj, RdWrMutex, Thread, EventWireRep, EventCounter,
       WeakRefList, Wr, Rd, Event, EventStubLib, ThreadF;

TYPE WireRep = EventWireRep.T;
(* The wire representation of our objects is represented using the
   WireRep provided by the event package. *)

REVEAL
  SharedObj.T = SharedObj.Public BRANDED SharedObj.Brand OBJECT 
      (* Is it ok, or has an update raised a fatal error? *)
      ok      : BITS 1 FOR BOOLEAN := TRUE;

      (* Who is performing an update right now? *)
      updating: ThreadF.Id := -1;

      (* A shareable lock. *)
      mu    : RdWrMutex.T := NIL;

      (* The wire representation of the object. *)
      wrep  : WireRep := EventWireRep.NullT;

      (* Current sequence number. *)
      seqNoCnt: EventCounter.T := NIL;

      (* How fast do we require updates? *)
      timeliness: SharedObj.Timeliness := 0;

      (* the currently registered callbacks on this object.  Stored 
         as "WeakRef.T"s *)
      callbacks: WeakRefList.T := NIL;
    METHODS
      applyUpdate(ev: Event.T; h: EventStubLib.Handle) 
        RAISES {SharedObj.Error, SharedObj.Fatal, Event.Error,
                Rd.Failure, Wr.Failure, Thread.Alerted};
    END;

(* for debugging. *)
PROCEDURE ToText(self: SharedObj.T): TEXT;

END SharedObjRep.
