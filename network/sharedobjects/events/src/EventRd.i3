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
 * Created On      : Tue May 23 17:51:38 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Jun 18 18:59:44 1998
 * Update Count    : 18
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
 * Revision 1.3  1998/07/02 21:41:11  bm
 * small bug fixes
 *
 * Revision 1.2  1996/11/21 22:40:24  bm
 * fixed header
 *
 * 
 * HISTORY
 *   Based on TextRd from the m3 distribution.
 *
 * TODO
 * - allow an EventRd to be written, perhaps by handing out an EventWr
 *   on it.
 *)
(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Sep 27 14:35:11 PDT 1993 by mcjones    *)
(*      modified on Thu May 20 15:21:48 PDT 1993 by swart      *)
(*      modified on Tue Jan 28 12:09:43 PST 1992 by kalsow     *)
(*      modified on Thu Nov  2 18:13:15 1989 by muller         *)

(* An "EventRd.T", or event reader, is a reader that delivers the
   characters of an "event" supplied when the reader was created.  Event
   readers are seekable, non-intermittent, and never raise "Alerted". 
 *)

INTERFACE EventRd;

IMPORT Rd, Thread, EventWr;

CONST Brand = "Event Reader";

TYPE
  T <: Public;
  Public = Rd.T OBJECT 
    extMu: Thread.Mutex;
  METHODS
    init(e: EventWr.T): T 
  END;

(* The call "rd.init(e)" initializes "rd" as a seekable,
   non-intermittent reader with:
| len(rd) = Wr.Length(e)
| src(rd) = `characters of e`
| cur(rd) = 0

   It is a checked runtime error if e has not been properly
   initialized. 
  
   extMu is a secondary external mutex that can be used for 2 level
   locking, for the situation when you need to lock a sequence of
   calls to other functions that lock the reader within them
   (preventing you from locking it yourself).
*)

PROCEDURE New(e: EventWr.T := NIL): T;
(* Equivalent to "NEW(T).init(e)". *)

PROCEDURE FromRd(rd: Rd.T; erd: T := NIL): T RAISES
  {Rd.Failure, Thread.Alerted}; 
(* Creates an "EventRd.T" from a normal reader "rd" using the remaining
   data in "rd".  If "erd" is non-NIL, reuse it instead of allocating
   a new one. *)

(* Convert the "rd" back into an "EventWr.T". *)
PROCEDURE ToWr(rd: T) : EventWr.T;

END EventRd.
