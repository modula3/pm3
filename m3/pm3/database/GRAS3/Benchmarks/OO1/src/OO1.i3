INTERFACE OO1;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.2  1998/03/17 14:13:30  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.1  1997/02/20 16:08:47  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT BenchmarkLog, Thread;

(** Implementation of th OO1 benchmark as reported in [1] for GRAS(3)
    Parts are nodes with 4 attributes

    Part = RECORD
             id: CARDINAL; -- node number
             type: ARRAY [0..9] OF CHAR;
             x, y: INTEGER;
             build: Date;
           END;

    Date = RECORD
             year, month, day: INTEGER;
             hour, minute, second: INTEGER;
           END;

    Connection = RECORD
                   to, from: INTEGER; -- Part ids
                   type: ARRAY [0..9] OF CHAR;
                   length: INTEGER;
                 END;

    [1] R.G.G. Cattell and J. Skeen, "Object Operations Benchmark",
        ACM Transactions on Database Systems, 1992, vol. 17, no. 1, pp 1--31

*)

TYPE
  Benchmark = {Load, Lookup, Traverse, ReverseTraverse, Insert};
  Suite = SET OF Benchmark;
    (* The OO1 benchmark has 5 phases.  Load builds up the database, Lookup
       chooses some parts randomly by number and reads their attributes,
       Traverse performs a depth first traversal on a subset of the parts,
       ReverseTraverse does the same, but traverses in counterdirection of
       the connections, and insert adds some parts to the database. *)

  GraphType = {Persistent, ChgMgmt, Typed};
    (* Gras has a layerd architecture.  The simplest graph form are
       persistent graphs.  These are directed node and edge labeled graphs.
       Following are change management graphs, allowing for undo/redo
       operations, and finally typed graphs that provide a more controlled
       typing of nodes, edges and attributes.  The benchmark is implemented
       for each of these graph types. *)

  ReportProc = PROCEDURE ();
  
PROCEDURE Init (reportFinish, reportError: ReportProc);
  (* Pre: $TRUE$. *)
  (* Post: $State' = Created$. *)
  (* Before starting the benchmark, Init must be called to install a log
     into which output will be written. *)

PROCEDURE Start (log                  : BenchmarkLog.T;
                 type                 : GraphType;
                 local                : BOOLEAN;
                 suite                : Suite;
                 parts                : CARDINAL;
                 simpleconnects, quick: BOOLEAN         )
  RAISES {StateError, Thread.Alerted};
  (* Pre: $State \in \{Created | Interrupted\}$. *)
  (* Post: State' = Running$. *)
  (* This starts the benchmark from the beginning.  If it was interrupted,
     all state information will be reset.  When the benchmark is complete,
     state changes to finished. *)

PROCEDURE Stop () RAISES {StateError};
  (* Pre: $State = Running$. *)
  (* Post: State' = Interrupted$. *)

PROCEDURE Continue () RAISES {StateError};
  (* Pre: $State = Interrupted$. *)
  (* Post: State' = Running$. *)
  (* Continue execution after interrupt. *)

PROCEDURE Quit () RAISES {StateError};
  (* Pre: $State \in \{Finished | Interrupted\}$. *)
  (* Post: State' = Finished. *)
  (* End benchmark.  If it was interrupted, all information we be
     deleted. *)

EXCEPTION StateError;

END OO1.
