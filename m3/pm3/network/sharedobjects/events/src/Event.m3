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
 * Created On      : Thu Jun  8 23:04:24 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  3 13:14:02 1996
 * Update Count    : 47
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
 * Revision 1.2  1997/01/23 15:26:34  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

MODULE Event;
IMPORT EventSpaceID, EventNumber, EventNumberF, TimeStamp;
FROM EventProtocol IMPORT StubProtocol, ID, NativeRep;
IMPORT Fmt, IO, ParseParams, Stdio;

REVEAL
  T = Public BRANDED "Event v1.0" OBJECT
    (* for reference counting *)
    count : INTEGER := 1;
  OVERRIDES
    init := Init;
    addRef := AddRef;
    dropRef := DropRef;
  END;

VAR 
  freeEvent: T := NIL;
  mu: MUTEX;

PROCEDURE New (): T = 
  VAR ev: T;
  BEGIN
    LOCK mu DO
      IF freeEvent # NIL THEN
        IF debug THEN
          IO.Put("Event.New: reusing event " & ToText(freeEvent) & "\n");
        END;
        ev := freeEvent;
        freeEvent := ev.next;
        ev.count := 1;
        ev.next := NIL;
      ELSE
        ev := NEW(T);
        ev.rd := NIL;
        ev.num := EventNumber.New();
      END;
    END;
    RETURN ev;
  END New;

PROCEDURE Init (ev: T; id: ID; eventProt: StubProtocol; num: EventNumber.T) = 
  BEGIN
    ev.count := 1;

    ev.num := ev.num.init(num);
    ev.hdr.rep := NativeRep;
    ev.from := EventSpaceID.Mine();
    ev.ts := TimeStamp.New();

    ev.hdr.rep.id := id;
    ev.hdr.prot := eventProt;
    ev.hdr.numHi := ev.num.hi;
    ev.hdr.numLo := ev.num.lo;
    ev.prot := eventProt;
  END Init;

PROCEDURE DropRef(ev: T) =
  BEGIN
    LOCK mu DO
      DEC(ev.count);
      IF debug THEN
        IO.Put("Event.DropRef: dropping ref to " & ToText(ev) & "\n");
      END;
      IF ev.count > 0 THEN RETURN END;

      IF debug THEN
        IO.Put("Event.DropRef: added to free list\n");
      END;
      ev.next := freeEvent;
      ev.sender := NIL;
      freeEvent := ev;
    END;
  END DropRef;

PROCEDURE AddRef(ev: T) =
  BEGIN
    LOCK mu DO
      INC(ev.count);
      IF debug THEN
        IO.Put("Event.AddRef: adding ref to " & ToText(ev) & "\n");
      END;
    END;
  END AddRef;

PROCEDURE ToText(ev: T): TEXT =
  BEGIN
    WITH hdr = ev.hdr, rep = hdr.rep DO
      RETURN "{(" & Fmt.Int(ev.count) & ")" & 
             Fmt.Int(rep.id, 16) & "+" & Fmt.Int(rep.intFmt, 16) &
             "+" & Fmt.Int(rep.floatFmt, 16) & "+" &
             Fmt.Int(rep.charSet, 16) & "/" & Fmt.Int(ev.prot) & 
             "/" & EventSpaceID.ToText(ev.from) & "/" &
             ev.num.fmt(10) & "}";
    END;
  END ToText;

VAR
  debug: BOOLEAN := FALSE;

BEGIN
  mu := NEW(MUTEX);
  debug := NEW(ParseParams.T).init(Stdio.stderr).keywordPresent("@EVdebug");
END Event.
