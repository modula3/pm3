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
 * Created On      : Mon Feb 20 17:43:14 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:47:31 1997
 * Update Count    : 8
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
 * Revision 1.1.3.1  2003/01/26 01:20:23  hosking
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
 * Revision 1.2  1997/08/11 20:36:20  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

(* The FileRdWr module sets up and returns a Rd/Wr pair which are
   linked together. Anything written to the writer is immediately
   available to the reader.  *)

INTERFACE RdWrPipe;

IMPORT Rd, Wr;

CONST
  (* the default size of the shared buffer *)
  BufferSize = 1024;

PROCEDURE New(VAR rd: Rd.T; VAR wr: Wr.T; buff_size: CARDINAL :=
  BufferSize; nm : TEXT := NIL);
(* Returns a read and writer which are connected together. *)

PROCEDURE ResetRdCounter(rd: Rd.T);
(* Reset the cur, lo and hi pointers, to allow this to read more
   characters than LAST(CARDINAL).  Should be called periodically. *)

PROCEDURE ResetWrCounter(wr: Wr.T);
(* Reset the cur, lo and hi pointers, to allow this to write more
   characters than LAST(CARDINAL).  Should be called periodically. *)

END RdWrPipe.
