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
 * Created On      : Mon May 29 15:33:17 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 13:59:34 1996
 * Update Count    : 28
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
 * Revision 1.1.3.1  2003/01/26 00:04:03  hosking
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
 * Revision 1.2  1996/11/22 18:59:38  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE Message;

IMPORT Event, ObjectInfo, EventStubLib;

CONST Brand = "Message";

TYPE 
  T = REF RECORD 
    next: T := NIL;

    ev: Event.T;
    h: EventStubLib.Handle;
    thread: INTEGER;
    objInfo: ObjectInfo.T; 
  END;

(* The Message structure contains all the information carried around
   by a message on a particular host.  "ev" is the event received from
   the Event package.  "h" is the handle to be used to read additional
   data out of the event.  "thread" is an identifier for the thread
   that initiated the call that resulted in this event, and is used to
   wake up that thread when the sequenced event is returned to that
   space.  *)

PROCEDURE Equal(k1, k2: T): BOOLEAN;

(* Create a text version of the message for printing during debugging. *)
PROCEDURE ToText(m: T): TEXT;

END Message.   
