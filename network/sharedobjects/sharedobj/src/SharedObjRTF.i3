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
 * Created On      : Wed Sep 13 11:19:30 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  3 13:16:24 1996
 * Update Count    : 46
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
 * Revision 1.3  1997/01/23 15:27:18  bm
 * Lot's of little bug fixes.
 *
 * Revision 1.2  1996/11/22 19:02:26  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE SharedObjRTF;

IMPORT AtomList, Thread, ObjectInfo, SharedObjRep, Debug,
       Message, SpaceConn, ObjectSpace, Event, EventSeq, WeakRef,
       SharedObj; 

TYPE
  Op = { MethodCall };

(* "Op" indicates the message type and is used the event type in the
   event package, as follows:

   "MethodCall" indicates a method invocation.  A header containing
   (in order) the wirerep of the object, the sequence number of the
   update call and an identifier for the calling thread is at the
   start of the event.  These are followed by the method number and
   the arguments of the call, which are read by the appropriate
   applyUpdate() method. *)

PROCEDURE GetObjInfo(wrep: SharedObjRep.WireRep; 
                     VAR objInfo: ObjectInfo.T): BOOLEAN; 
PROCEDURE GetObjRef(objInfo: ObjectInfo.T): SharedObj.T;
PROCEDURE SequenceMsg(m: Message.T): BOOLEAN 
  RAISES {SharedObj.Error, Thread.Alerted};
PROCEDURE CheckForLastClient(objInfo: ObjectInfo.T);
PROCEDURE RegisterObject(obj: SharedObj.T; wrep: SharedObjRep.WireRep; 
                         sequencer: SpaceConn.T; standAlone: BOOLEAN; 
                         seqNo: SharedObj.SequenceNumber): ObjectInfo.T;
PROCEDURE AddClient(objInfo: ObjectInfo.T; conn: SpaceConn.T) 
  RAISES {SharedObj.Error};
PROCEDURE DeleteClient(objInfo: ObjectInfo.T; conn: SpaceConn.T) 
  RAISES {SharedObj.Error};
PROCEDURE WaitForSequencer() RAISES {Thread.Alerted};
PROCEDURE GetDfltSequencer(): ObjectSpace.T RAISES {Thread.Alerted};
PROCEDURE GetSequencer(wrep: SharedObjRep.WireRep): ObjectSpace.T
  RAISES {SharedObj.Error}; 
PROCEDURE SequencerFailed(seq: SpaceConn.T; ec: AtomList.T);

PROCEDURE EventPortConnect(conn: SpaceConn.T) RAISES {SharedObj.Error};
PROCEDURE EventPortDisconnect (conn: SpaceConn.T): EventSeq.T 
  RAISES {SharedObj.Error};

PROCEDURE SequenceCall(obj: SharedObj.T; ev: Event.T; thread: INTEGER; 
                       objInfo: ObjectInfo.T; en: SharedObj.SequenceNumber;
                       VAR alerted: BOOLEAN) 
  RAISES {SharedObj.Error, Thread.Alerted};

PROCEDURE CleanupSharedObj(READONLY w: WeakRef.T; r: REFANY);

PROCEDURE ObjTblToText(): TEXT;

VAR
  objTblMu: MUTEX; (* A mutex for the object table. *)

  (* For debugging.  Messages are turned off by default. *)
  debug: Debug.T;
  debug_level := 0;

  zeroSeqNo: SharedObj.SequenceNumber;
  oneSeqNo: SharedObj.SequenceNumber;

  localSpace: ObjectSpace.Local := NIL;

END SharedObjRTF.
