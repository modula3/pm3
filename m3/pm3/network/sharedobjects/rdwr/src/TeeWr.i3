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
 * Created On      : Wed Mar  1 20:04:22 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sat Aug  9 13:49:32 1997
 * Update Count    : 12
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
 * Revision 1.2  1997/08/11 20:36:24  bm
 * Various fixes
 *
 * 
 * HISTORY
 *)

INTERFACE TeeWr;

IMPORT Text, Wr, Thread;

TYPE
  T <: Public;
  Public = Wr.T OBJECT 
  METHODS 
    init (): T;

    (* add the named writer to the output list. *)
    tee (name: Text.T; wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted};
    
    (* remove a named writer from the output list. *)
    untee (name: Text.T): Wr.T RAISES {Wr.Failure, Thread.Alerted};
  END;

  (* An initialized TeeWr.T returned by NEW(T).init() is an 
     output stream with which copies all it's output to all of it's
     output writers. If there are no writers currently on the output
     list, the TeeWr.T behaves like a NullWr.T  *)

END TeeWr. 
