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
 * Created On      : Mon May 15 17:26:50 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Nov 27 19:23:10 1996
 * Update Count    : 23
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  2003/04/08 22:00:43  hosking
 * Merge of PM3 with Persistent M3 and CM3 release 5.1.8
 *
 * Revision 1.1.3.1  2003/01/26 01:11:36  hosking
 * Import of CM3 5.1.8
 *
 * Revision 1.2  2001/12/02 00:20:38  wagner
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
 * Revision 1.4  1997/01/23 15:26:41  bm
 * Lots of little bug fixes.
 *
 * Revision 1.3  1996/11/21 22:51:14  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

(* A "RdWrMutex.T" is a mutex that supports being locked by a single
   "writer" or by multiple "readers".  The mutex has the following
   properties: 
   \begin{itemize}
   \item The locks are associated with the thread that calls
   "acquireRead" or "acquireWrite".  Attempting to release the mutex
   with a different thread is a runtime error.
   \item It is safe to reacquire a held lock, as long as the lock is
   released the same number of times, with reads and writes released
   in the same order.  This includes any combination or acquiring read
   and write locks.
   \item If the owner of a read lock attempts to reacquire a write
   lock on the mutex, it will block until all other read locks have
   been released.
   \end{itemize}

   The "wait" method can be called when a read lock, a write lock or
   neither has been acquired.  All locks will be released while
   sleeping on the condition variable "cv."  It is assumed the
   argument "mu" is held before "wait" is called.

   Here's an idea to make them more fair.  The only potential
   unfairness is that if a reader holds a "RdWrMutex.T," a writer
   could block indefinately as long as at least one reader keeps the
   lock.   If, however, we order the queue waiting for the lock in a
   first-come-first-server manner, and only let multiple readers in if
   one of the readers at the front of the queue still has the lock, we
   will be more fair.  When we say "at the front of the queue," we
   mean those readers that aquired the read lock before the first
   writer that is waiting for it.
*)

INTERFACE RdWrMutex;

IMPORT AtomList, Thread;

EXCEPTION Error(AtomList.T);

TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(): T;
    acquireRead();
    acquireWrite();
    releaseRead();
    releaseWrite();

    wait(mu: Thread.Mutex; cv: Thread.Condition);
  END;

END RdWrMutex.
