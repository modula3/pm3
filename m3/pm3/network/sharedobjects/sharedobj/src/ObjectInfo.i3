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
 * Created On      : Tue Apr 25 13:50:10 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 14:00:34 1996
 * Update Count    : 64
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
 * Revision 1.2  1996/11/22 19:00:38  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE ObjectInfo;

IMPORT SharedObj, ObjCopy, SharedObjRep, ObjCopyList, WeakRef,
       EventConnList, Thread, SpaceConn, EventNumber; 

CONST Brand = "Shared Object Info";

TYPE
  T  = Thread.Mutex OBJECT
             (* The wire representation of the object. *)
             wrep  : SharedObjRep.WireRep;

             (* Used by all hosts. *)
             seqNo     : SharedObj.SequenceNumber;
             conns     : EventConnList.T            := NIL;

             (* Used by hosts that have a copy of the object. *)
             isStandalone: BITS 1 FOR BOOLEAN := TRUE;
             isOwner     : BITS 1 FOR BOOLEAN := FALSE;
             isLocker    : BITS 1 FOR BOOLEAN := FALSE;
             hasCopy     : BITS 1 FOR BOOLEAN := FALSE;
             obj         : WeakRef.T;

             (* Used by the sequencers. *)
             lock      : ObjCopy.T                  := NIL;
             owner     : ObjCopy.T                  := NIL;
             sequencer : SpaceConn.T                := NIL;
             clients   : ObjCopyList.T              := NIL;

             (* Different meaning on sequencers. *)
             fastClients: ObjCopyList.T             := NIL;

             (* threads that sent messages to be sequenced *)
             waiting: REF ARRAY OF Blocked := NIL;
           METHODS
             init (seqNo: SharedObj.SequenceNumber := NIL): T := Init;
             pickThreadSlot(): CARDINAL := PickThreadSlot;
             releaseThreadSlot(id: CARDINAL) := ReleaseThreadSlot;
          END;

TYPE 
  Blocked = RECORD
    used: BOOLEAN;
    en: EventNumber.T := NIL;
    cv: Thread.Condition;
  END;
    
(* The information maintained by the runtime system for each shared
   object it knows about.  The "seqNo" is the highest
   sequence number seen in, or assigned to, any update event.  If this
   host is forwarding to any other host, either because its a
   sequencer or because it is doing fast updates, "conns" contains the
   list of "Conn.T"s to use as a parameter to "EventPort.T.mcast()".

   If there is a copy of the object on this host, "obj" is a weak
   reference to the object itself.  The following flags define the
   state of this copy of the object: "isStandalone" indicates if the
   object exists on any other machines or just this one, "isOwner"
   indicates if this machine "owns" the objects and "isLocker" is true
   if someone has locked the object through this copy.

   If the host is the sequencer for this group, it can also have the
   following additional fields set. "lock" is the the copy that is
   locking the object and "owner" is the copy that owns the object, if
   either of those conditions are true.  If some other sequencer is
   doing the sequencing for this object, "sequencer" refers to it.
   "clients" is a list of clients.

   "fastClients" is the list of clients that are getting timely
   updates.  On a sequencer, this is the list that has been delegated
   to the "owner."  Thus, "conns" corresponds to
   "clients-fastClients".  On the owner, this is the list we are
   sending to.  In this case, "conns" corresponds to "fastClients".

 *)

PROCEDURE Init(self: T; seqNo: SharedObj.SequenceNumber): T;
PROCEDURE PickThreadSlot(self: T) : CARDINAL;
PROCEDURE ReleaseThreadSlot(self: T; id: CARDINAL);

(* Generate a text version for debugging. *)
PROCEDURE ToText(objInfo: T): TEXT;

END ObjectInfo.
