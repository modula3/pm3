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
 * Created On      : Wed Jun  7 16:53:58 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:44:47 1996
 * Update Count    : 5
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 22:00:42  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/26 01:11:34  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.2  2001/12/02 00:20:37  wagner
 * add copyright notes, fix overrides for cm3, and make everything compile
 *
 * added: events/COPYRIGHT-COLUMBIA
 * added: events/src/COPYRIGHT-COLUMBIA
 * modified: events/src/Event.i3
 * modified: events/src/Event.m3
 * modified: events/src/EventConn.i3
 * modified: events/src/EventConn.m3
 * modified: events/src/EventCounter.i3
 * modified: events/src/EventCounter.m3
 * modified: events/src/EventHandle.i3
 * modified: events/src/EventIO.i3
 * modified: events/src/EventNumber.i3
 * modified: events/src/EventNumber.m3
 * modified: events/src/EventNumberF.i3
 * modified: events/src/EventPort.i3
 * modified: events/src/EventPort.m3
 * modified: events/src/EventProtocol.i3
 * modified: events/src/EventRd.i3
 * modified: events/src/EventRd.m3
 * modified: events/src/EventSpaceID.i3
 * modified: events/src/EventSpaceID.m3
 * modified: events/src/EventStubLib.i3
 * modified: events/src/EventStubLib.m3
 * modified: events/src/EventWireRep.i3
 * modified: events/src/EventWireRep.m3
 * modified: events/src/EventWr.i3
 * modified: events/src/EventWr.m3
 * modified: events/src/EventWrF.i3
 * modified: events/src/HostInfo.i3
 * modified: events/src/HostInfo.m3
 * modified: events/src/RdWrMutex.i3
 * modified: events/src/RdWrMutex.m3
 * modified: events/src/Work.i3
 * modified: events/src/WorkerPool.i3
 * modified: events/src/WorkerPool.m3
 * modified: events/src/Zombie.i3
 * modified: events/src/m3makefile
 * modified: events/src/m3overrides
 *
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.2  1996/11/21 22:44:52  bm
 * fixed header
 *
 * 
 * HISTORY
 * - based on SpaceID from the netobj package.
 *)
(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* EventSpaceID.i3 *)
(* Last modified on Mon Jul 19 13:50:29 PDT 1993 by wobber  *)
(*      modified on Thu Nov 19 21:28:19 1992 by gnelson *)
(*      modified on Wed Jun 10 16:56:10 PDT 1992 by owicki *)

(* The "EventSpaceID" interface is used to generate values which uniquely 
   identify address space instances across space and time. *)

INTERFACE EventSpaceID;

IMPORT Fingerprint;

CONST Brand = "EventSpaceID";

TYPE T = Fingerprint.T;

(* A "EventSpaceID.T" is a value which is sufficiently distinct to
   identify the address space which generated it among the set of all
   such address spaces.  Each value contains both an address and
   a time component, but the exact format is implementation dependent.
*)

PROCEDURE Mine() : T;

(* "Mine" returns the "T" value for the current address space.  It is
    distinct from all other such values in other address spaces.

   Implementation notes:

   The current implementation generates unique values by concatentating
   the local hardware Ethernet address and the real time clock.

   Any given implementation should be able to support aggregate
   "EventSpaceID.T" generation at sustained rate of at least one per second
   per Ethernet address.
   
   More detailed specification as to how values are generated
   is required to ensure uniqueness across multiple implementations.
*)

PROCEDURE ToText(id: T): TEXT;

(* "ToText" returns a textual representation of "T".  This is intended
   to be used for debugging purposes. 
*)

END EventSpaceID.
