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
 * Created On      : Wed Jun  7 16:54:33 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:45:18 1996
 * Update Count    : 6
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
 * Revision 1.2  1996/11/21 22:45:23  bm
 * fixed header
 *
 * 
 * HISTORY
 * - based on SpaceID from the netobj package.
 *)
(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* EventSpaceID.m3 *)
(* Last modified on Mon Jul 19 15:04:37 PDT 1993 by wobber *)
(*      modified on Fri Aug  7 15:00:56 PDT 1992 by evers  *)

UNSAFE MODULE EventSpaceID;

IMPORT Fingerprint, TimeStamp, Fmt;

VAR myT: T;

PROCEDURE Mine() : T = BEGIN RETURN myT; END Mine;

PROCEDURE ComputeFP() : T =
  VAR ts := TimeStamp.New();
  BEGIN
    RETURN Fingerprint.FromChars(
             LOOPHOLE(ts, ARRAY [0..15] OF CHAR), Fingerprint.OfEmpty);
  END ComputeFP;
   
PROCEDURE ToText(id: T): TEXT =
  VAR t: TEXT := Fmt.Int(id.byte[FIRST(id.byte)]);
  BEGIN
    FOR i := FIRST(id.byte)+1 TO LAST(id.byte) DO
      t := t & "." & Fmt.Int(id.byte[i]);
    END;
    RETURN t;
  END ToText;

BEGIN
  myT := ComputeFP();
END EventSpaceID.
