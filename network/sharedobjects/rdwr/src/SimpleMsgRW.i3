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
 * Created On      : Wed Apr 19 10:16:48 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:48:28 1997
 * Update Count    : 5
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 22:00:45  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/26 01:20:25  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.2  2001/12/02 00:35:21  wagner
 * add copyright notes and fix overrides for cm3
 *
 * added: rdwr/COPYRIGHT-COLUMBIA
 * added: rdwr/src/COPYRIGHT-COLUMBIA
 * added: rdwr/src/m3overrides
 * modified: rdwr/src/RdWrPipe.i3
 * modified: rdwr/src/RdWrPipe.m3
 * modified: rdwr/src/SimpleMsgRW.i3
 * modified: rdwr/src/SimpleMsgRW.m3
 * modified: rdwr/src/TeeWr.i3
 * modified: rdwr/src/TeeWr.m3
 *
 * Revision 1.1.1.1  2001/12/02 00:29:10  wagner
 * Blair MacIntyre's rdwr library
 *
 * Revision 1.2  1997/08/11 20:36:22  bm
 * Various fixes
 *
 * 
 * HISTORY
 *
 * Originally was the ConnMsgRW module in the TCP package. 
 * A small number of modifications allowed it to be adapted to sit on
 * generic Rd.T and Wr.T objects, instead of ConnFD.T objects used by
 * TCP.  The burning question is, why wasn't this done before???
 *
 * The original header in ConnMsgRW.i3 is here: *)
(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Dec  2 16:40:57 PST 1992 by wobber *)

INTERFACE SimpleMsgRW;

IMPORT Rd, Wr, MsgRd, MsgWr;

PROCEDURE NewRd(rd: Rd.T) : MsgRd.T;
   (* produces a message reader from a generic Rd.T *)

PROCEDURE NewWr(wr: Wr.T) : MsgWr.T;
   (* produces a message writer from a generic Wr.T *)

END SimpleMsgRW.

