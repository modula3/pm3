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
 * Created On      : Wed May 10 15:36:50 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Oct 23 11:41:07 1997
 * Update Count    : 30
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
 * Revision 1.2  2003/01/26 01:17:22  hosking
 * Merge with CM3 5.1.8.
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
 * Revision 1.4  1997/10/24 19:31:35  bm
 * Added the ability to flush the readers and worker pool.
 *
 * Revision 1.3  1997/02/17 16:22:43  bm
 * fixed comment
 *
 * Revision 1.2  1997/01/23 15:26:43  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

(* A simple interface to create a pool of workers. A worker is a
   thread which handles pieces of "work" which are supplied as
   subclasses of "Work.T" objects.  The subtypes provide a "handle()"
   method for each unit of work.  Since a unit of "Work.T" is an
   object, it can contain any other data or methods needed to
   accomplish its task.

   After a pool of workers has been initialized, simply add a unit of
   work to be performed by calling "add(work)".  An idle thread will
   handle it, or a new thread will be created if none are idle, and
   there are less than maxThreads already performing work.  There will
   be at most maxIdleThreads threads idle at any given time.  By
   default, the maximum number of idle threads is a function of the
   maximum number of threads.  Increasing this will increase both
   memory overhead (by having idle threads tie up memory) and (in the
   current implementation) CPU overhead because all idle threads are
   awakened when new work arrives.  "stackSize" is the stackSize of
   the worker threads.  The default is the system default (usually
   3000).  If your threads run out of space, you may want to increase
   this.

   If a worker thread needs to remove itself from the pool of workers,
   either permanently or temporarily (perhaps because it will be
   blocking for an unexpectedly large period of time), it should call
   "stealWorker()."  The thread will be removed from the list of
   workers, allowing another thread to be added to the pool.  When a
   stolen thread finishes its work, it will either be destroyed or
   added to the idle queue, depending on the number of threads left
   around.  If is ok, but meaningless, for a non-worker thread to call
   "stealWorker()."  The return value is true is the thread was a
   worker thread, false otherwise.

   Call flush() when you want to pause until all the work has been
   done.   You can wake a blocked thread by alerting it.

   Call finish() when you wish to wait for all the worker threads to
   finish what they are doing.
*)

INTERFACE WorkerPool;

IMPORT Work, Thread;

TYPE
  T <: Public;
  Public = Private OBJECT METHODS
    init(maxThreads: CARDINAL := 4;
         maxIdleThreads: INTEGER := -1; 
         stackSize: CARDINAL := 0): T;
    add(work: Work.T);
    stealWorker(): BOOLEAN;
    flush() RAISES {Thread.Alerted};
    finish();
  END;
  Private <: <*TRANSIENT*> ROOT;

END WorkerPool.
