(*| Copyright (C) 1990, Digital Equipment Corporation       *)
(*| All rights reserved.                                    *)
(*| See the file COPYRIGHT for a full description.          *)
(*|                                                         *)
(*| Portions Copyright 1996-2000, Critical Mass, Inc.       *)
(*| See file COPYRIGHT-CMASS for details.                   *)
(*|                                                         *)
(*| Portions Copyright 1998-2002, Purdue Research           *)
(*| Foundation                                              *)
(*|                                                         *)
(*| Last modified on Wed Oct 12 14:30:51 PDT 1994 by kalsow *)
(*|      modified on Tue Jun  1 13:03:23 PDT 1993 by muller *)
(*|      modified on Tue Mar  9 08:44:09 PST 1993 by jdd    *)

(* "RTHeapRep" is a private, implementation-dependent extension to
   "RTAllocator", "RTCollector", and "RTHeap". *)

UNSAFE INTERFACE RTHeapRep;

(* This interface provides low-level access to the storage allocator and
   garbage collector.  Some items here should be made private or moved
   elsewhere. *)

IMPORT RT0, RTHeapDep, RTHeapDB;
FROM RT0 IMPORT Typecode;

(* The allocator and collector maintain two heaps of objects.  One heap is
   "traced" (its objects are collected); the other is "untraced".

   The allocator for the untraced heap is simply "malloc".  Unless
   explicitly noted, all procedures and variables here are for the traced
   heap.  Unless explicitly noted, none of the variables in this interface
   are writable. *)

(****** PAGES ******)

(* The (traced) heap consists of a number of aligned pages, divided among
   three spaces: Free, Previous, and Current.  All other pages in the
   address space are in the "Unallocated" space.  Pages are numbered 0, 1,
   2, ....  The pagesize used is fixed; if incremental or generational
   collection is to be allowed, it must be at least the VM page size.

   The global variable p0 and p1 hold the bounds of the heap pages: only
   pages in the range [p0, p1) are in a space other than Unallocated.  For
   these pages, the array "desc" holds more information; desc[p - p0] holds
   state for page "p". *)

CONST
  BytesPerPage    = RTHeapDep.BytesPerPage;
  LogBytesPerPage = RTHeapDep.LogBytesPerPage;
  AdrPerPage      = RTHeapDep.AdrPerPage;
  LogAdrPerPage   = RTHeapDep.LogAdrPerPage;

TYPE Page = RTHeapDep.Page;

CONST
  Nil: Page = 0;                 (* page 0 cannot be part of the traced
                                    heap *)

VAR p0, p1: Page := Nil;

VAR
  desc  : UNTRACED REF ARRAY OF Desc;
  writer: UNTRACED REF ARRAY OF RTHeapDB.Txn;
  reader: UNTRACED REF ARRAY OF RTHeapDB.Txn;
  map   : UNTRACED REF ARRAY OF RTHeapDB.DBPage;

VAR max_heap: INTEGER := -1;
(** If "max_heap" is non-negative, the traced heap will not be
    extended beyond "max_heap" bytes.  If "max_heap" is
    negative, the traced heap will be allowed to grow until the
    underlying OS refuses to provide more memory.  *)

TYPE
  Desc = RECORD
           space     : BITS 2 FOR Space;
           generation: BITS 1 FOR Generation;
           pure      : BITS 1 FOR BOOLEAN;
           note      : BITS 3 FOR Note;
           gray      : BITS 1 FOR BOOLEAN;
           mode      : BITS 2 FOR Mode;
           continued : BITS 1 FOR BOOLEAN;
           resident  : BITS 1 FOR BOOLEAN;
           dirty     : BITS 1 FOR BOOLEAN;
           link: BITS BITSIZE(ADDRESS) - LogAdrPerPage FOR Page := Nil;
         END;

TYPE Mode = { NoAccess, ReadOnly, ReadWrite };
CONST
  Readable = ARRAY Mode OF BOOLEAN { FALSE, TRUE, TRUE };
  Writable = ARRAY Mode OF BOOLEAN { FALSE, FALSE, TRUE };

TYPE Space = {Unallocated, Free, Previous, Current};

(* Each page has a short note attached, describing why it is in its current
   state.  This is usually used for performance monitoring. *)

TYPE
  Notes = SET OF Note;
  Note = {OlderGeneration,       (* page promoted to current space because
                                    it it contained the older generation
                                    from the previous space *)
          AmbiguousRoot,         (* page promoted to current space because
                                    of a possible reference from a thread
                                    state *)
          Large,                 (* page promoted to current space because
                                    it contains a single accessible object,
                                    so no garbage would be collected by
                                    copying the object *)
          Persistent,            (* page promoted to current space because
                                    it is persistent *)
          Frozen,                (* page contains frozen ref *)
          Allocated,             (* page was allocated in current space *)
          Copied};               (* page contains elements that were copied
                                    from previous space *)

(* The collector can be generational; the heap is divided into two
   generations. *)

TYPE Generation = {Older, Younger};

VAR
  allocatedPages: CARDINAL := 0; (* the number of pages in the Free,
                                    Previous, or Current spaces *)

(****** HEAP OBJECTS ******)

(* An object is a contiguous array of words on the heap.  The first word of
   an object is its header.  The object's body begins at the second word,
   its address is the object's REF.  All object bodies are aligned.

   "Small" objects never cross a page boundary.  "Large" objects are larger
   than a page; they span multiple contiguous pages.  For large objects,
   pages following the first are marked "continued".  The large object is
   the only object on its pages; it starts at the beginning of its first
   page, and no other objects follow it on its last page.

   Special "filler" objects are used to exactly fill out the end of a page
   of small objects, or to fill space between small objects when they
   cannot exactly follow the previous object because of alignment
   restrictions.  There are 1-word and multi-word filler objects.  The
   beginning of a page is always adequate alignment, so a filler object
   need never begin a page. *)

TYPE
  Header = RT0.RefHeader;
  RefHeader = UNTRACED REF Header;

CONST
  Fill_1_type: Typecode = LAST(Typecode); (* 1 word filler *)
  FillHeader1: Header = Header{typecode := Fill_1_type, forwarded := FALSE};

CONST
  Fill_N_type: Typecode = LAST(Typecode) - 1;
  FillHeaderN: Header = Header{typecode := Fill_N_type, forwarded := FALSE};
(* multi-word filler, the second word is the total size of the object,
   in bytes *)

(****** OPEN ARRAYS ******)

(* An open array object with N open dimensions contains a header, then a
   pointer to the first data element, then N integers that hold the
   dimensions. *)

TYPE UnsafeArrayShape = UNTRACED REF ARRAY [0 .. (*N-1*) 999] OF INTEGER;

PROCEDURE UnsafeGetShape (    r          : REFANY;
                          VAR nDimensions: INTEGER;
                          VAR s          : UnsafeArrayShape);
(* if r is a reference to an open array, the number of open dimensions,
   nDimensions, and size of each dimension, s, is returned.  The array's
   shape vector is valid as long as r exists.  If r is not a reference to
   an open array, nDimensions = 0 and s is undefined.  It is an unchecked
   runtime error to modify s^, to free s, or to use it after r has been
   garbage collected. *)

(****** LOW-LEVEL ALLOCATOR/COLLECTOR *****)

PROCEDURE AllocUntraced (size: INTEGER): ADDRESS;
(* Return the address of "size" bytes of untraced, un-zeroed storage,
   if possible.  Otherwise, return "NIL".  *)

PROCEDURE AllocTraced (size, alignment: CARDINAL;  VAR pool: AllocPool): ADDRESS;
(* Return the address of "size" bytes of traced storage on an
   "alignment" byte boundary from the allocation pool "pool".
   The storage is not zeroed.  If the request cannot be satisfied,
   "NIL" is returned.  LL >= RTOS.LockHeap. *)

(* Objects in the traced heap are allocated from one of three "pools".
   A pool is collection of pages with similar properties.  The "newPool"
   contains NEWed objects.  The "pureCopy" pool contains objects that
   were copied by the collector into new space, but that contain no
   internal REFs.  Similarly, the "impureCopy" pool is for copied
   objects that contain REFs.  *)

TYPE
  AllocPool = RECORD
    desc       : Desc;           (* descriptor for new pages in this pool *)
    notAfter   : Notes;          (* if possible avoid following these pages *)
    page       : Page    := Nil; (* current allocation page of the pool *)
    stack      : Page    := Nil; (* linked list of new pages from this pool *)
    next       : ADDRESS := NIL; (* address of next available byte *)
    limit      : ADDRESS := NIL; (* address of first unavailable byte *)
    n_small    : INTEGER := 0;   (* # of "small" pages allocated via this pool *)
    n_big      : INTEGER := 0;   (* # of "big" and "continued" pages allocated *)
    txn: RTHeapDB.Txn := NIL;
    db: RTHeapDB.DB := NIL;
  END;

VAR (* LL >= RTOS.HeapLock *)
  newPool := AllocPool {
    desc := Desc {space := Space.Current, generation := Generation.Younger,
                  pure := FALSE, note := Note.Allocated, gray := FALSE,
                  mode := Mode.ReadWrite, continued := FALSE,
                  resident := TRUE, dirty := TRUE },
    notAfter := Notes {Note.Copied} };

VAR (* LL >= RTOS.HeapLock *)
  newTransient := AllocPool {
    desc := Desc {space := Space.Current, generation := Generation.Younger,
                  pure := FALSE, note := Note.Allocated, gray := FALSE,
                  mode := Mode.ReadWrite, continued := FALSE,
                  resident := TRUE, dirty := TRUE },
    notAfter := Notes {Note.Copied} };

VAR (* LL >= RTOS.HeapLock *)
  pureCopy := AllocPool {
    desc := Desc {space := Space.Current, generation := Generation.Younger,
                  pure := TRUE, note := Note.Copied, gray := FALSE,
                  mode := Mode.ReadWrite, continued := FALSE,
                  resident := TRUE, dirty := FALSE },
    notAfter := Notes {Note.Allocated} };

VAR (* LL >= RTOS.HeapLock *)
  pureTransient := AllocPool {
    desc := Desc {space := Space.Current, generation := Generation.Younger,
                  pure := TRUE, note := Note.Copied, gray := FALSE,
                  mode := Mode.ReadWrite, continued := FALSE,
                  resident := TRUE, dirty := FALSE },
    notAfter := Notes {Note.Allocated} };

VAR (* LL >= RTOS.HeapLock *)
  pureStabilize := AllocPool {
    desc := Desc {space := Space.Current, generation := Generation.Younger,
                  pure := TRUE, note := Note.Copied, gray := TRUE,
                  mode := Mode.ReadWrite, continued := FALSE,
                  resident := TRUE, dirty := FALSE },
    notAfter := Notes {Note.Allocated} };

VAR (* LL >= RTOS.HeapLock *)
  impureCopy := AllocPool {
    desc := Desc {space := Space.Current, generation := Generation.Younger,
                  pure := FALSE, note := Note.Copied, gray := TRUE,
                  mode := Mode.ReadWrite, continued := FALSE,
                  resident := TRUE, dirty := TRUE },
    notAfter := Notes {Note.Allocated} };

VAR (* LL >= RTOS.HeapLock *)
  impureTransient := AllocPool {
    desc := Desc {space := Space.Current, generation := Generation.Younger,
                  pure := FALSE, note := Note.Copied, gray := TRUE,
                  mode := Mode.ReadWrite, continued := FALSE,
                  resident := TRUE, dirty := TRUE },
    notAfter := Notes {Note.Allocated} };

VAR (* LL >= RTOS.HeapLock *)
  impureStabilize := AllocPool {
    desc := Desc {space := Space.Current, generation := Generation.Younger,
                  pure := FALSE, note := Note.Copied, gray := TRUE,
                  mode := Mode.ReadWrite, continued := FALSE,
                  resident := TRUE, dirty := TRUE },
    notAfter := Notes {Note.Allocated} };

PROCEDURE GetTraced (defn: ADDRESS; VAR pool: AllocPool): REFANY;

(****** MODULE OBJECTS ******)

(* A Modula-3 object is appears to the collector like any other object.
   The first word of its body is a pointer to its method list. *)

CONST MethodListOffset = 0;      (* byte offset in the object's body *)

(* RegisterFinalCleanup is available for low-level cleanup by the thread
   package.  If "r" is registered for cleanup, then just before "r" is
   freed, the cleanup procedure "p" is called.  This procedure is allowed
   to dereference "r" to copy out data, not including traced references.

   Although the thread package could use ordinary weak refs, this operation
   is easy to provide and is a little more efficient.  The thread package
   cannot use weak refs straight on public types because its clients should
   remain free to use weak refs on subtypes. *)

PROCEDURE RegisterFinalCleanup (r: REFANY; p: PROCEDURE (r: REFANY));

(****** COLLECTOR STATUS AND CONTROL ******)

(* There are various status variables. *)

VAR collections := 0;            (* the number of collections begun *)

VAR
  disableCount: CARDINAL := 0;   (* how many more Disables than Enables *)
  disableMotionCount: CARDINAL := 0; (* how many more DisableMotions than
                                        EnableMotions *)
  disableVMCount: CARDINAL := 0; (* how many more DisableVMs than
                                    EnableVMs *)

PROCEDURE Crash (): BOOLEAN;
(* Crash is called by the runtime when the program is about to crash.  When
   Crash returns, the entire heap is readable, and no further heap objects
   will move or be protected.  Crash attempts to finish the current
   collection.  If Crash returns TRUE, the current collection, if any,
   successfully completed. *)

TYPE
  MonitorClosure <: <*TRANSIENT*> ROOT OBJECT
                    METHODS
                      before ();
                      after  ();
                    END;

PROCEDURE RegisterMonitor (cl: MonitorClosure);
(* Before each collection, the collector calls all registered 'before'
   procedures; after each collection, the collector calls all registered
   'after' procedures. *)

PROCEDURE UnregisterMonitor (cl: MonitorClosure);
(* removes m's procedures from the registered set. *)

PROCEDURE InvokeMonitors (before: BOOLEAN);
(* called by the collector to trigger the registered monitors.
   If "before" is "TRUE", the "before" methods are called, otherwise
   the "after" methods are called. *)

(*** VM support ***)

PROCEDURE Fault (addr: ADDRESS; mode := Mode.NoAccess): BOOLEAN;

(* Fault is called from the RTHeapDep when a VM fault occurs.  If Fault
   returns TRUE, protection has been changed and the operation should be
   retried.  If Fault returns FALSE, the faulting address is not part of
   the traced heap, and the fault should be treated as an error. *)

(****** DEBUGGING ******)

(* There are various routines for collecting or printing out information on
   the objects on the heap. *)

TYPE
  RefVisitor = <*TRANSIENT*> ROOT OBJECT
               METHODS
                 visit (tc: Typecode; r: REFANY; size: CARDINAL): BOOLEAN;
                 (* returns TRUE to continue *)
               END;

PROCEDURE VisitAllRefs (proc: RefVisitor);
(* Visit all the traced references in the heap, and call proc.visit for
   each one of them.  Garbage collection is disabled during that visit and
   you should refrain from allocating memory in proc. *)

(****** INITIALIZATION ******)

PROCEDURE Init();
(* MUST be called to initialize allocator/collector state *)

END RTHeapRep.
