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
 * Created On      : Thu May 11 11:11:52 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Tue Dec  3 11:37:24 1996
 * Update Count    : 38
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
 * Idea taking from a C version of similar idea in MUTS, written by
 *     Robbert van Renesse, Brad Glade, Cornell University 
 *
 * This module implements event counters.
 * Event counters are like MUTEXs, but can only be grabbed in a certain
 * order.  Furthermore, other threads are allowed to wait for a certain
 * event values to be reached without incrementing the counter.  
 * Finally, instead of blocking, a thread can leave an event
 * associated with an event value on the internal queue, which can be
 * processed .
 *
 * The lock associated with an EventCount is a RdWrMutex, and is
 * passed in via the "init()" method. 
 *
 * The intended use for event counters is to syncronize threads that are
 * handling numbered events that may arrive out of order.
 * The wait() method allows additional threads to wait until after a
 * certain numbered event has finished.  These "wait()"ing threads
 * will run after the event value has been reached and 
 * do not lock the counter.  This implies two things:
 *   - the scheduler may end up running these threads at some arbitrary
 *     time, so a thread that waits for the event counter to reach value N
 *     should not depend on it being N when it is run.
 *   - these threads should not call release(), since they haven't
 *     acquired()d the lock.
 *
 * The "init()" method should be called to initialize a new event
 * counter.  By default, the counter is initialized to zero. 
 * The "acquire(value: EventNumber.T)" method is like Thread.Acquire(), but
 * blocks until the mutex is free and the event counter reaches the
 * given value.  If multiple attempts are made to acquire the same
 * value, all but one will raise the "Duplicate" exception when the counter
 * reaches the given value (duplicates will not be checked until that
 * point).  It is undefined which one will acquire the lock, and which
 * will receive an exception.  "tryAcquire()" is similar to acquire,
 * but will not block.  If the lock cannot be acquired immediately, it
 * will return FALSE.  Otherwise the lock is acquired and
 * "tryAcquire()" returns TRUE.
 *
 * "enqueueAction(value: EventNumber.T; handler: Handler)" adds an
 * action to the EventCount queue that will be exectuted with the
 * counter reaches "value."  An enqueued action is semantically
 * similar to a thread acquiring the lock, executing the
 * action itself when the acquire succeeds, and then releasing the
 * lock.  However, the action will be executed by the thread that
 * releases the lock when it is at "value-1."  If the enqueued action
 * can be executed (no other thread or action has acquired the lock at
 * the requested value), "handler.handle()" will be called to execute
 * the action.  If the action cannot be executed because a duplicate
 * value has already been acquired, "handler.duplicate()" will be called.
 *
 * The "release()" method is similar to Thread.Release(), but 
 * increments the event counter in addition to releasing the lock.
 * The "wait(value: Integer.T)" method causes the threads to
 * wait until the event counter is "release()"d by the thread that
 * acquire()d it with the same value, incrementing the counter past the
 * value. 
 *
 * If the event number overflows its maximum value, "release()" raises
 * "Overflow".
 *
 * The "set(value: EventNumber.T)" is similar to acquire except that
 * it sets the value of the counter to value.  However, set can never
 * lower the value of the counter, and an "Invalid" exception will be
 * raised if this is attempted.  A call to "set()" will lock the
 * counter and then change it.  If a set of clients are already
 * waiting to "acquire()" the next "N" values of the counter, some "M"
 * of them, "M <= N", may acquire the counter before "set()".  Once
 * "set()" acquires the counter, it is set to the new value and all
 * clients that were blocked attempting to acquire some value less
 * than this new value are unblocked with a "Duplicate" exception. All
 * clients that were blocked waiting for some value less than this new
 * value are unblocked.  
 *)

INTERFACE EventCounter;

IMPORT RdWrMutex, EventNumber;

EXCEPTION Duplicate;
(* EXCEPTION Overflow;  *)
EXCEPTION Invalid;

TYPE
  Handler = OBJECT METHODS 
    handle() := DefaultHandlerHandle; 
    duplicate() := DefaultHandlerDuplicate;
  END;

  T <: Public;
  Public = OBJECT METHODS
    init(mu: RdWrMutex.T; value: EventNumber.T := NIL): T;
    acquire(value: EventNumber.T) RAISES {Duplicate};
    tryAcquire(value: EventNumber.T): BOOLEAN RAISES {Duplicate};
    enqueueAction(value: EventNumber.T; handler: Handler) RAISES {Duplicate};
    wait(value: EventNumber.T);
    release();
    value(): EventNumber.T;
    set(value: EventNumber.T) RAISES {Invalid};
  END;

PROCEDURE New(mu: RdWrMutex.T; t: EventNumber.T := NIL): T;
  (* Same as "NEW(T).init(t)" *)

PROCEDURE DefaultHandlerHandle(self: Handler);
PROCEDURE DefaultHandlerDuplicate(self: Handler);
  (* These routines return immediately. *)

PROCEDURE ToText(self: T): TEXT;
  (* For debugging, return a textual representation of the
     EventCounter state. *)

END EventCounter.
