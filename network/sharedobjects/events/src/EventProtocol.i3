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
 * Created On      : Wed May 24 12:51:10 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:39:41 1996
 * Update Count    : 49
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
 * Revision 1.3  1996/11/21 22:39:45  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

INTERFACE EventProtocol;

TYPE
  Byte8 = BITS 8 FOR [0..255];
  DataRep = RECORD
    id, intFmt, floatFmt, charSet: Byte8;
  END;

  Int32 =  BITS 32 FOR [-16_7FFFFFFF-1..16_7FFFFFFF];
  Cardinal32 =  BITS 32 FOR [0..16_7FFFFFFF]; 
  (* I lose half the numbers here, but that's ok. *)
  Word32 = Int32;    

  StubProtocol = Int32;
  ID = Byte8;

(* The type "DataRep" describes the format used to encode characters,
   integers, and floating point numbers in network data.  This is the
   same as the network object StubLib interface, so see it for a
   description of these fields.   The "id" field is used to specify
   the type of the event.

   The type "StubProtocol" indicates the version of the stub compiler
   used to generate a particular stub.  Multiple stubs for the same
   sort of event can coexist within the same program (for example, 
   the outputs of different stub compilers).\ttindex{EventStubLib.StubProtocol}

   The ID type is the type used outside this interface for
   specifying the event type.
 
*)

(* This header appears in all events.  The "intFmt", "floatFmt", and
   "charSet" fields of a "Header" indicate the native data representation
   of the sender.   For a header "hdr", "VAL(hdr.id, Op)" indicates
   the event type and is dependent on the package using it. 
*)

  MsgHeader = RECORD
    rep: DataRep;
    prot: StubProtocol;
    numLo: Word32;
    numHi: Word32;
  END;

(* A 128-bit "MsgHeader" prefixes every event.  
   It is 64-bit aligned so that data streams will be aligned
   correctly in memory on 64-bit machines.
*)

VAR (*CONST*) NativeRep: DataRep;

(* "NativeRep" is a runtime constant that describes the native format
   of the current environment.
   \ttindex{EventStubLib.DataRep}\ttindex{EventStubLib.NativeRep}

*)
END EventProtocol.
