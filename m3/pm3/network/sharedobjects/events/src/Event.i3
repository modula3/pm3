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
 * Created On      : Wed Jun  7 15:56:08 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:31:54 1996
 * Update Count    : 33
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
 * Revision 1.1.3.1  2003/01/26 01:11:32  hosking
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
 * Revision 1.2  1997/01/23 15:26:33  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

INTERFACE Event;

IMPORT EventRd, AtomList, EventSpaceID, EventNumber, EventConn, TimeStamp;
FROM EventProtocol IMPORT MsgHeader, ID, StubProtocol;

CONST Brand = "Event";

TYPE
  T <: Public;
  Public = OBJECT
    (* for creating a list of them *)
    next: T := NIL;

    (* Event data *)
    hdr: MsgHeader;
    from: EventSpaceID.T;
    ts: TimeStamp.T;
    prot: StubProtocol;
    num: EventNumber.T;

    (* who sent this to us.  NIL means we created it! *)
    sender: EventConn.T := NIL;

    (* the reader for an event *)
    rd: EventRd.T;
  METHODS
    (* Initialize an event. *)
    init (id: ID; eventProt: StubProtocol; num: EventNumber.T);

    (* Keep track of the number of references to an event.  The Event
       module may cache a certain number of event structures for reuse
       by "New". A new, or freshly initialized event, has a reference
       count of 1.  When the reference count drops to 0 the event may
       be reused. *)
    addRef ();
    dropRef ();
  END;

(* "hdr" contains the header that is sent out at the start of
   the event.  "rep" contains the "DataRep" part of "hdr".  "prot"
   contains the "prot" part of "rep", converted to local byte
   ordering. "rd" contains the bulk of the event.  "ts" is the
   timestamp of event creation. *)

(* Get an event structure (possibly from the cache list) *)
PROCEDURE New (): T;

PROCEDURE ToText(ev: T): TEXT;
(* "ToText" creates a text version of ev, suitable for use during
   debugging. *)

EXCEPTION Error(AtomList.T);

END Event.
