(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Feb  7 16:32:39 PST 1997 by heydon   *)
(*      modified on Fri Apr 22 12:17:25 PDT 1994 by wobber   *)

INTERFACE SmallDB;

IMPORT AtomList, OSError, Rd, Wr;

(* This package will maintain a copy of a data structure on secondary
   storage, and will update the secondary storage as updates are made to
   the data structure. A client can use this package to ensure that the
   current value of the data structure can be recovered after any crash.
   This package is efficient: the cost of recording an update is about
   one disk write. If you are interested in simply maintaining a stable
   version of a single object type, you should probably use the generic
   "Stable" interface instead.

   The secondary storage strategy is to record values in files using a
   representation of the caller's choosing. Two sorts of files are kept:
   snapshots and a log. At any instant, one snapshot is "current". The
   log consists of a sequence of updates that have occurred since the
   current snapshot was taken. The current stable state is the value of
   the current snapshot, as modified by the sequence of updates in the
   log. From time to time, the client of this package instructs the
   package to make a new snapshot and clear the log. This package
   arranges disk writes such that updates are stable and atomic: no
   update is lost, and each update either is recorded completely in the
   log or not at all. Making a new snapshot is also atomic.

   Normal use for maintaing a database is as follows. First, the client
   calls the "New" procedure to create a new instance "t" of a
   "SmallDB.T". The client then calls "t.recover()" to recover the latest
   version of his data structure (or to create a new one if one doesn't
   yet exist). The client maintains his data structure in memory, as
   usual. As updates happen to the structure, the client informs this
   package by calling "t.update(value)", where "value" is an arbitrary,
   client-defined reference representing the update. Periodically, the
   client calls "t.snapshot(value)", where "value" is the data structure
   to be recorded. The more often "t.snapshot" is called, the less time
   future "t.recover" operations will take.

   Each database is associated with its own directory in the filesystem;
   this directory is the first argument to "New". The second argument to
   "New" is a value of type "SmallDB.Closure", which defines methods for
   creating new instances of the client's data structure and for reading
   and writing both snapshots of the client's data structure and updates
   of that data structure to disk. This "Closure" argument becomes
   associated with the returned "T" object; many of a "T"'s methods are
   implemented by calls to its associated "Closure" object (as described
   below).

   NB: The types "T" and "Closure" share methods with the same name. Keep
   in mind that the client is responsible for implementing the "Closure"
   methods, but should only call the "T" methods. To implement the
   "Closure" methods, you may find it helpful to use the "Pickle"
   interface.
*)

EXCEPTION Failed(AtomList.T);
(* This exception is raised when an operation fails. The argument to the
   exception is a list of error messages describing the failure. *)

PROCEDURE New(dir: TEXT; cl: Closure; pad := TRUE): T
  RAISES {OSError.E, Failed};
(* Returns a "T" for the database maintained in the directory "dir" and
   with associated "SmallDB.Closure" "cl". The client must supply a value
   "cl" that is a subtype of "SmallDB.Closure" for creating new instances
   of the client data structure, and for reading/writing snapshots and
   updates of the data structure to disk.

   Raises "OSError.E" if the directory doesn't exist or is inaccessible.
   If the directory exists but contains no backing files, creates files
   corresponding to a new object with no updates.

   If "pad", then updates are padded to disk page boundaries. *)

TYPE
  T <: Public;
  Public = OBJECT METHODS
    recover(): REFANY RAISES {OSError.E, Failed};
    update(value: REFANY; forceToDisk: BOOLEAN := TRUE) RAISES {OSError.E};
    snapshot(value: REFANY) RAISES {OSError.E};
    close() RAISES {OSError.E};
    snapshotBytes(): CARDINAL;
    logBytes(): CARDINAL;
    status(): TEXT;
  END;

(* A "T" is a handle on an open stable storage directory. Values of type
   "T" should only be created by calls to the "New" procedure above.

   Each value "t" of type "T" has an associated closure of type "Closure"
   denoted "cl(t)". Its methods are as follows:

   "t.recover()":
       Return the REFANY recorded in the current snapshot,
       as recovered by calling "cl(t).recover" and then
       subsequently invoking "cl(t).readUpdate" to apply any
       logged updates to the state. In the event that this
       is a new database, instead return the REFANY returned
       by calling "cl(t).new".

   "t.update(value, forceToDisk)":
       Record the update "value" in the log file for "t" by calling
       "cl(t).logUpdate". This method must not be called until
       after "t.recover()" has been invoked. If "NOT forceToDisk",
       the update is buffered until the buffer is full.

   "t.snapshot(value)":
       Record "value" as the current snapshot by invoking
       "cl(t).snapshot", and then empty the log.

   "t.close()":
       Close "t"'s associated stable storage directory in an
       orderly manner.

   "t.snapshotBytes()":
       Return the size of the current snapshot file.

   "t.logBytes()":
       Return the size of the log file.

   "t.status()":
       Return human readable status information about the
       database "t".
*)

TYPE
  Closure = OBJECT METHODS
    new(): REFANY RAISES {Failed};
    recover(rd: Rd.T): REFANY RAISES {Failed, Rd.Failure};
    snapshot(wr: Wr.T; r: REFANY) RAISES {Wr.Failure};
    readUpdate(rd: Rd.T; state: REFANY): REFANY RAISES {Failed, Rd.Failure};
    logUpdate(wr: Wr.T; r: REFANY) RAISES {Wr.Failure};
  END;

(* Each client must implement a closure object.  The readers and writers
   passed to the closure object methods will not raise "Thread.Alerted".
   The methods are to be implemented as follows:

   "cl.new()":
       No database exists; create and return a new instance of
       the desired in-core data structure, or raise "Failed".

   "cl.recover(rd)":
       Read from "rd" the client-specific representation of a database
       snapshot. Decode this representation and return the resulting
       "REFANY".
 
   "cl.snapshot(wr, r)":
       Write a client-specific representation of the data structure
       "r" to "wr".

   "cl.readUpdate(rd, state)":
       Read a stably logged update from "rd", apply it to the current
       snapshot value "state", and return an updated value of "state".

   "cl.logUpdate(wr, r)":
       Write an update "r" to "wr" to be recorded in stable storage.
*)


END SmallDB.
