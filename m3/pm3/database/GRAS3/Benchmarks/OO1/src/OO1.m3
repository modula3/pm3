MODULE OO1;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.7  1998/08/03 09:45:43  roland
    Changed parameter 'nameserver' to 'agent' according to gras3 convention.
    Thread stack in OO1 set higher.

    Revision 1.6  1998/03/18 09:26:55  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.5  1998/03/17 14:13:31  kluck
    Necessary adaptions to use local graphs. (MK)


    Revision 1.5 1997/10/23 m.kluck
    OpenModes implemented

    Revision 1.4  1997/10/14 09:16:43  roland
    Merge of HiGRAS and Main branch.

    Revision 1.2.2.2  1997/07/21 10:53:47  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.2.2.1  1997/06/19 14:38:28  roland
    Branch HiGRAS implements OO1 for HiGRAS. TypegGraph-Implementation of
    OO1Graph added.

    Revision 1.2  1997/02/26 16:18:59  roland
    Batchbetrieb realisiert. Weitere Kommandozeilenoptionen fuer
    Cachegroesse und Nameserver.

    Revision 1.1  1997/02/20 16:08:48  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT Thread, BenchmarkLog, BenchmarkAux, OO1Graph, Time, Random, Access,
       Fmt, BenchmarkSupport;
IMPORT CardSet;
IMPORT PersistentOO1Graph, ChgMgmtOO1Graph, TypedOO1Graph;

TYPE
  State =
    {Created, Forked, Running, Interrupted, Restart, Finished, Exiting};

TYPE
  OO1State = RECORD
               graph           : OO1Graph.T;
               graphopen       : BOOLEAN            := FALSE;
               graphtype       : GraphType;
               local           : BOOLEAN;
               log             : BenchmarkLog.T;
               thread          : Thread.T;
               state           : State              := State.Created;
               phase           : Benchmark;
               contCond        : Thread.Condition;
               statelock       : MUTEX;
               simplecon, quick: BOOLEAN;
               parts           : CARDINAL;
               suite           : Suite;
               counter         : CARDINAL;
               reportFinish    : ReportProc;
               reportError     : ReportProc;
             END;


VAR bm: OO1State;

CONST
  ConnectsPerPart   = 3;
  MaxNodesTraversed = 3280.0D0;

VAR
  rnd: Random.Default := NEW(Random.Default).init(fixed := TRUE);
  typeString := OO1Graph.TypeString{
                  'p', 'a', 'r', 't', '-', 't', 'y', 'p', 'e', '?'};
  conString := OO1Graph.TypeString{
                 'c', 'o', 'n', 'n', 'e', 'c', 't', 'i', 'o', 'n'};
  date: OO1Graph.Date;

  OrigPath: TEXT := "oo1Original";

PROCEDURE ThreadApply (<* UNUSED *> thread: Thread.Closure): REFANY =
  BEGIN
    LOCK bm.statelock DO
      TRY
        Thread.AlertWait(bm.statelock, bm.contCond);
      EXCEPT
        Thread.Alerted => RETURN NIL;
      END;
    END;
    WHILE bm.state # State.Exiting DO
      TRY
        Perform();
        (* Benchmark completed.  Do as if just created. *)
        bm.reportFinish();
        LOCK bm.statelock DO
          bm.state := State.Forked;
          Thread.AlertWait(bm.statelock, bm.contCond);
        END;
      EXCEPT
        Thread.Alerted =>
          EVAL Thread.TestAlert();
          LOCK bm.statelock DO
            bm.state := State.Interrupted;
            TRY
              bm.log.writeTime();
              bm.log.write("Interrupted\n");
              Thread.AlertWait(bm.statelock, bm.contCond);
            EXCEPT
              Thread.Alerted => EVAL Thread.TestAlert()
            END;
          END;
      END;
    END;
    RETURN NIL;
  END ThreadApply;

PROCEDURE Init (rf, re: ReportProc) =
  BEGIN
    bm.reportFinish := rf;
    bm.reportError := re;
    bm.statelock := NEW(MUTEX);
    bm.contCond := NEW(Thread.Condition);
    IF bm.state = State.Created THEN
      bm.thread :=
        Thread.Fork(NEW(Thread.SizedClosure,
                        stackSize := MIN(100000, Thread.GetDefaultStackSize()),
                        apply := ThreadApply));
      bm.state := State.Forked;
    END;
  END Init;

PROCEDURE Start (log                  : BenchmarkLog.T;
                 graphtype            : GraphType;
                 local                : BOOLEAN;
                 suite                : Suite;
                 parts                : CARDINAL;
                 simpleconnects, quick: BOOLEAN         )
  RAISES {StateError, Thread.Alerted} =
  BEGIN
    bm.log := log;
    IF suite = Suite{} THEN
      bm.log.write("You must at least specify one benchmark action!\n");
    ELSE
      LOCK bm.statelock DO
        IF bm.state # State.Forked AND bm.state # State.Interrupted THEN
          RAISE StateError
        END;
        bm.graphtype := graphtype;
        bm.local := local;
        bm.suite := suite;
        bm.parts := parts;
        bm.simplecon := simpleconnects;
        bm.quick := quick;
        bm.phase := Benchmark.Load;
        WHILE NOT bm.phase IN suite DO INC(bm.phase) END;
        IF bm.state = State.Forked THEN
          bm.state := State.Running;
        ELSE
          bm.state := State.Restart;
        END;
        bm.log.write("Database type: ");
        CASE bm.graphtype OF
          GraphType.Persistent => bm.log.write("PersistentGraph\n");
        | GraphType.ChgMgmt => bm.log.write("ChgMgmtGraph\n");
        | GraphType.Typed => bm.log.write("TypedGraph\n");
        END;
        IF NOT bm.local THEN
          bm.log.write("Graph is REMOTE\n");
        ELSE
          bm.log.write("Graph is LOCAL\n");
        END;
        IF bm.simplecon THEN
          bm.log.write("Connection type simple.\n");
        ELSE
          bm.log.write("Connection type attributed.\n");
        END;
        IF bm.quick THEN
          bm.log.write("Few large transactions.\n");
        ELSE
          bm.log.write("Many small transactions.\n");
        END;
        Thread.Signal(bm.contCond);
      END;
    END;
  END Start;

PROCEDURE Stop () RAISES {StateError} =
  BEGIN
    LOCK bm.statelock DO
      IF bm.state # State.Running THEN RAISE StateError; END;
      Thread.Alert(bm.thread);
    END;
  END Stop;

PROCEDURE Continue () RAISES {StateError} =
  BEGIN
    LOCK bm.statelock DO
      IF bm.state # State.Interrupted THEN RAISE StateError; END;
      bm.state := State.Running;
      Thread.Signal(bm.contCond);
    END;
  END Continue;

PROCEDURE Quit () RAISES {StateError} =
  BEGIN
    LOCK bm.statelock DO
      IF bm.state = State.Running OR bm.state = State.Restart THEN
        RAISE StateError;
      END;
      bm.state := State.Exiting;
    END;
    Thread.Signal(bm.contCond);
    EVAL Thread.Join(bm.thread);
  END Quit;

PROCEDURE PhaseNeedsExecution (phase: Benchmark): BOOLEAN =
  BEGIN
    RETURN phase IN bm.suite AND phase = bm.phase;
  END PhaseNeedsExecution;

PROCEDURE Perform () RAISES {Thread.Alerted} =
  BEGIN
    BenchmarkAux.ClearSystemCaches(bm.log);
    LOCK bm.statelock DO
      IF bm.state = State.Restart THEN
        CloseGraph();
        bm.state := State.Running
      END;
    END;
    TRY
      IF PhaseNeedsExecution(Benchmark.Load) THEN
        BuildUp();
        INC(bm.phase);
      END;
      IF PhaseNeedsExecution(Benchmark.Lookup) THEN
        MeasurePhase(Benchmark.Lookup, "Lookup", measureOpen := FALSE);
        INC(bm.phase);
      END;
      IF PhaseNeedsExecution(Benchmark.Traverse) THEN
        MeasurePhase(Benchmark.Traverse, "Traversal", measureOpen := FALSE);
        INC(bm.phase);
      END;
      IF PhaseNeedsExecution(Benchmark.ReverseTraverse) THEN
        MeasurePhase(Benchmark.ReverseTraverse, "Reverse Traversal",
                     measureOpen := FALSE);
        INC(bm.phase);
      END;
      IF PhaseNeedsExecution(Benchmark.Insert) THEN
        MeasurePhase(Benchmark.Insert, "Insert", measureOpen := FALSE);
      END;
    EXCEPT
      Error => bm.reportError(); RAISE Thread.Alerted;
    END;
    bm.log.writeTime();
  END Perform;

PROCEDURE MeasurePhase (Phase: Benchmark; Name: TEXT; measureOpen: BOOLEAN)
  RAISES {Thread.Alerted, Error} =
  VAR
    coldTime, avgTime: Time.T;
    warmTime         : ARRAY [1 .. 10] OF Time.T;
    min              : INTEGER;
    factor           : LONGREAL;
  BEGIN
    (* One cold run with cleared system caches + 10 warm runs *)
    BenchmarkAux.ClearSystemCaches(bm.log);
    bm.log.writeEvent("Performing " & Name & "...");
    IF Phase = Benchmark.ReverseTraverse THEN
      bm.log.write(
        "Times are normalized: "
          & "measuredTime * MaxNodesTraversed / NodesTraversed\n");
    END;
    IF measureOpen THEN coldTime := Time.Now(); END;
    OpenGraph();
    IF NOT measureOpen THEN coldTime := Time.Now(); END;

    CASE Phase OF
    | Benchmark.Load =>
      (* empty *)

    | Benchmark.Lookup => PerformLookup(); factor := 1.0D0;

    | Benchmark.Traverse =>
        PerformTraversal(forward := TRUE, NodesTraversed := factor);
        factor := MaxNodesTraversed / factor;

    | Benchmark.ReverseTraverse =>
        PerformTraversal(forward := FALSE, NodesTraversed := factor);
        factor := MaxNodesTraversed / factor;

    | Benchmark.Insert => PerformInsert(); factor := 1.0D0;
    END;

    IF NOT measureOpen THEN
      coldTime := BenchmarkSupport.Interval(coldTime);
    ELSE
      CloseGraph();
      coldTime := BenchmarkSupport.Interval(coldTime);
    END;
    bm.log.write(
      "Measured time for " & Name & " cold: "
        & BenchmarkSupport.TimeToString(coldTime * factor) & " sec\n");
    bm.log.writeEvent("Cold run for " & Name & " finished");

    FOR i := 1 TO 10 DO
      IF measureOpen THEN
        warmTime[i] := Time.Now();
        OpenGraph();
      ELSE
        warmTime[i] := Time.Now();
      END;

      CASE Phase OF
      | Benchmark.Load =>
        (* empty *)

      | Benchmark.Lookup => PerformLookup(); factor := 1.0D0;

      | Benchmark.Traverse =>
          PerformTraversal(forward := TRUE, NodesTraversed := factor);
          factor := MaxNodesTraversed / factor;

      | Benchmark.ReverseTraverse =>
          PerformTraversal(forward := FALSE, NodesTraversed := factor);
          factor := MaxNodesTraversed / factor;

      | Benchmark.Insert => PerformInsert(); factor := 1.0D0
      END;

      IF NOT measureOpen THEN
        warmTime[i] := BenchmarkSupport.Interval(warmTime[i]) * factor;
      ELSE
        CloseGraph();
        warmTime[i] := BenchmarkSupport.Interval(warmTime[i]) * factor;
      END;
      bm.log.write("*");
    END;

    IF NOT measureOpen THEN CloseGraph(); END;


    bm.log.write("\nMeasured times for " & Name & " warm:\n");
    min := 1;
    avgTime := 0.0D0;
    FOR i := 1 TO 10 DO
      bm.log.write(BenchmarkSupport.TimeToString(warmTime[i]) & " ");
      IF warmTime[i] < warmTime[min] THEN min := i END;
      avgTime := avgTime + warmTime[i];
    END;
    bm.log.write(
      "\nMinimum: " & BenchmarkSupport.TimeToString(warmTime[min]));
    bm.log.write(
      "\nAverage: " & BenchmarkSupport.TimeToString(avgTime / 10.0D0)
        & "\n");
    bm.log.writeEvent("Warm runs for " & Name & " finished\n");
  END MeasurePhase;

PROCEDURE OpenGraph () RAISES {Error, Thread.Alerted} =
  BEGIN
    IF NOT bm.graphopen THEN
      TRY
        CASE bm.graphtype OF
          GraphType.Persistent =>
            bm.graph := NEW(PersistentOO1Graph.T).open(
                          OrigPath, access := Access.Mode.ReadWriteShared,
                          new := FALSE, local := bm.local,
                          simpleConnects := bm.simplecon);
        | GraphType.ChgMgmt =>
            bm.graph :=
              NEW(ChgMgmtOO1Graph.T).open(
                OrigPath, bm.local, access := Access.Mode.ReadWriteShared,
                new := FALSE, simpleConnects := bm.simplecon);
        | GraphType.Typed =>
            bm.graph :=
              NEW(TypedOO1Graph.T).open(
                OrigPath, bm.local, access := Access.Mode.ReadWriteShared,
                new := FALSE, simpleConnects := bm.simplecon);
        ELSE
          bm.log.write("Benchmark not implemented for this graph type.\n");
          RAISE Error;
        END;
        bm.graphopen := TRUE;
      EXCEPT
        OO1Graph.Failure (description) =>
          bm.log.write("Error: Cannot open graph! (" & description & ")\n");
          RAISE Error;

      END;
    END;
  END OpenGraph;


PROCEDURE CloseGraph () =
  BEGIN
    IF bm.graphopen THEN bm.graph.close(); bm.graphopen := FALSE; END;
  END CloseGraph;

PROCEDURE ErrAbort () RAISES {Thread.Alerted} =
  BEGIN
    TRY
      bm.graph.abortTransaction();
    EXCEPT
      OO1Graph.Failure =>        (* ignore *)
    END;
  END ErrAbort;


EXCEPTION Error;

PROCEDURE BuildUp () RAISES {Error, Thread.Alerted} =
  VAR measuredTime: Time.T;

  PROCEDURE CreateGraph () RAISES {Error, Thread.Alerted} =
    BEGIN
      TRY
        IF NOT bm.graphopen THEN
          CASE bm.graphtype OF
            GraphType.Persistent =>
              bm.graph :=
                NEW(PersistentOO1Graph.T).open(
                  OrigPath, Access.Mode.ReadWriteShared, new := TRUE,
                  local := bm.local, simpleConnects := bm.simplecon);
          | GraphType.ChgMgmt =>
              bm.graph :=
                NEW(ChgMgmtOO1Graph.T).open(
                  OrigPath, bm.local, Access.Mode.ReadWriteShared,
                  new := TRUE, simpleConnects := bm.simplecon);
          | GraphType.Typed =>
              bm.graph :=
                NEW(TypedOO1Graph.T).open(
                  OrigPath, bm.local, Access.Mode.ReadWriteShared,
                  new := TRUE, simpleConnects := bm.simplecon);
          ELSE
            bm.log.write(
              "Benchmark not implemented for this graph type.\n");
            RAISE Error;
          END;
          bm.graphopen := TRUE;
        END;
      EXCEPT
      | OO1Graph.Failure (msg) =>
          bm.log.write("Cannot create graph: " & msg & "\n");
          RAISE Error;
      END;
    END CreateGraph;


  PROCEDURE CreateNodes () RAISES {Error, Thread.Alerted} =
    VAR nn: CARDINAL;
    BEGIN
      (* Create N nodes with node numbers ranging from 1 to N *)
      bm.log.write("Creating parts...\n");
      FOR i := 1 TO bm.parts DO
        TRY
          IF NOT bm.quick THEN bm.graph.beginTransaction(); END;
          (* create node *)
          nn := bm.graph.createPart(i);
          IF nn # i THEN
            IF NOT bm.quick THEN bm.graph.abortTransaction() END;
            RAISE Error;
          END;

          (* construct attribute value *)
          typeString[9] := VAL(rnd.integer(48, 57), CHAR);
          date.year := rnd.integer(1985, 1995);
          date.month := rnd.integer(1, 12);
          date.day := rnd.integer(1, 31);
          (* Yes, I do know that the number of days in a month depends upon
             the year and the month itself.  Though, who cares?  This is a
             benchmark, NOT business software! *)
          date.hour := rnd.integer(0, 23);
          date.minute := rnd.integer(0, 59);
          date.second := rnd.integer(0, 59);

          (* store atttribute value *)
          bm.graph.putPartAttributes(i, typeString, rnd.integer(0, 99999),
                                     rnd.integer(0, 99999), date);

          (* producing some output to provide user response *)
          (* bm.log.write("."); *)
          IF i MOD (bm.parts DIV 10) = 0 THEN
            bm.log.write(Fmt.Int(i) & "\n");
            IF bm.quick THEN
              bm.graph.commitTransaction();
              bm.graph.beginTransaction();
            END;
          END;

          IF NOT bm.quick THEN bm.graph.commitTransaction(); END;
        EXCEPT
          OO1Graph.Failure (msg) =>
            IF NOT bm.quick THEN ErrAbort(); END;
            bm.log.write("Error building graph: " & msg & ".\n");
            RAISE Error;
        END;
      END;
    END CreateNodes;

  PROCEDURE CreateConnections () RAISES {Error, Thread.Alerted} =
    VAR cpart: INTEGER;
    BEGIN
      bm.log.write("Drawing connections...\n");

      FOR i := 1 TO bm.parts DO
        TRY
          IF NOT bm.quick THEN bm.graph.beginTransaction(); END;
          FOR j := 1 TO ConnectsPerPart DO
            IF rnd.integer(1, 10) > 1 THEN
              (* 90% of time create connection to the closest 1% of
                 parts *)
              cpart := i + rnd.integer(1, bm.parts DIV 100) - 1
                         - bm.parts DIV 200;
              (* "double up" at the ends so stay in part id range *)
              IF cpart < bm.parts DIV 200 THEN
                cpart := cpart + bm.parts DIV 200
              END;
              IF cpart > bm.parts - bm.parts DIV 200 THEN
                cpart := cpart - bm.parts DIV 200
              END;
            ELSE
              cpart := rnd.integer(1, bm.parts);
            END;
            bm.graph.connect(i, cpart, conString, rnd.integer(1, 1000));
          END;
          (* bm.log.write("."); *)
          IF i MOD (bm.parts DIV 10) = 0 THEN
            bm.log.write(Fmt.Int(i) & "\n");
            IF bm.quick THEN
              bm.graph.commitTransaction();
              bm.graph.beginTransaction();
            END;
          END;

          IF NOT bm.quick THEN bm.graph.commitTransaction(); END;
        EXCEPT
          OO1Graph.Failure (msg) =>
            IF NOT bm.quick THEN ErrAbort(); END;
            bm.log.write("Error building graph: " & msg & ".\n");
            RAISE Error;
        END;
      END;
    END CreateConnections;

  (* BuildUp *)
  BEGIN
    TRY
      bm.log.writeEvent("BuildUp started");
      bm.log.write(
        "Building database with " & Fmt.Int(bm.parts) & " parts and "
          & Fmt.Int(bm.parts * ConnectsPerPart) & " connects.\n");
      measuredTime := Time.Now();

      CreateGraph();
      TRY
        IF bm.quick THEN bm.graph.beginTransaction(); END;
        CreateNodes();
        CreateConnections();
        IF bm.quick THEN bm.graph.commitTransaction(); END;
      EXCEPT
        OO1Graph.Failure (msg) =>
          IF NOT bm.quick THEN ErrAbort(); END;
          bm.log.write("Error building graph: " & msg & ".\n");
          RAISE Error;
      | Error => IF bm.quick THEN ErrAbort(); END; RAISE Error;
      END;

      bm.log.writeEvent("BuildUp endend");
      measuredTime := BenchmarkSupport.Interval(measuredTime);
      bm.log.write(
        "Measured time for Buildup: "
          & BenchmarkSupport.TimeToString(measuredTime) & " sec\n");

      (* All done, now close the graph. *)
      bm.log.writeEvent("CheckIn");
      CloseGraph();
    EXCEPT
      Thread.Alerted => CloseGraph(); RAISE Thread.Alerted;
    | Error => CloseGraph(); RAISE Error;
    END;
  END BuildUp;

(* Lookup: Generate 1000 random part id's and fetch the corresponding parts
   from the database.  For each part, call a null procedure written in any
   host programming language, passing passing the x,y position and type of
   the part. *)
PROCEDURE PerformLookup () RAISES {Error, Thread.Alerted} =
  VAR
    part : CARDINAL;
    x, y : INTEGER;
    type : OO1Graph.TypeString;
    build: OO1Graph.Date;

  PROCEDURE NullProc (<* UNUSED *>          p   : CARDINAL;
                      <* UNUSED *> READONLY t   : OO1Graph.TypeString;
                      <* UNUSED *>          x, y: INTEGER              ) =
    BEGIN
      (* empty *)
    END NullProc;

  (* PerformLookup *)
  BEGIN
    TRY
      IF bm.quick THEN bm.graph.beginTransaction(); END;
      FOR i := 1 TO 1000 DO
        IF NOT bm.quick THEN bm.graph.beginTransaction(); END;
        part := rnd.integer(1, bm.parts);
        bm.graph.getPartAttributes(part, type, x, y, build);
        NullProc(part, type, x, y);
        IF NOT bm.quick THEN bm.graph.commitTransaction(); END;

        (* bm.log.write("."); *)
      END;
      IF bm.quick THEN bm.graph.commitTransaction(); END;
    EXCEPT
    | OO1Graph.Failure (msg) =>
        ErrAbort();
        bm.log.write("Lookup error: " & msg & ".\n");
        RAISE Error;
    END;
  END PerformLookup;


(* Traversal: Find all parts connected to a randomly selected part, or to a
   part connected to it, and so on, up to seven hops (total of 3280 parts,
   with possible duplicates).  For each part, call a null programming
   language procedure with the value of the x and y fields and the part
   type.  Also measure time for reverse traversal, swapping "from" and "to"
   directions, to compare results obtained.*)
PROCEDURE PerformTraversal (forward: BOOLEAN; VAR NodesTraversed: LONGREAL)
  RAISES {Error, Thread.Alerted} =
  VAR
    part : CARDINAL;
    x, y : INTEGER;
    type : OO1Graph.TypeString;
    build: OO1Graph.Date;

  PROCEDURE NullProc (<* UNUSED *>          p   : CARDINAL;
                      <* UNUSED *> READONLY t   : OO1Graph.TypeString;
                      <* UNUSED *>          x, y: INTEGER              ) =
    BEGIN
      (* empty *)
    END NullProc;


  (* perform a depth first traversal for depth of level *)
  PROCEDURE Traverse (    part          : CARDINAL;
                          level         : CARDINAL;
                      VAR NodesTraversed: LONGREAL  )
    RAISES {Error, Thread.Alerted} =
    VAR
      nextNodes: CardSet.T := CardSet.New();
      next     : CARDINAL;
      found    : BOOLEAN   := FALSE;
    BEGIN
      TRY
        IF forward THEN
          nextNodes := bm.graph.targets(part);
          (*
          IF nextNodes.card() # ConnectsPerPart THEN
            bm.log.write("Node " & Fmt.Int(part) & " has only "
                           & Fmt.Int(nextNodes.card()) & " successors.\n");
          END;
          *)
        ELSE
          nextNodes := bm.graph.sources(part);
        END;

        nextNodes.loop();
        next := nextNodes.get(found);
        WHILE found DO
          bm.graph.getPartAttributes(next, type, x, y, build);
          NullProc(next, type, x, y);
          NodesTraversed := NodesTraversed + 1.0D0;
          IF level > 1 THEN Traverse(next, level - 1, NodesTraversed); END;
          next := nextNodes.get(found);
        END;
        nextNodes.dispose();
      EXCEPT
        OO1Graph.Failure (msg) =>
          bm.log.write("Traverse error: " & msg & ".\n");
          RAISE Error;
      END;
    END Traverse;

  (* PerformTraversal *)
  BEGIN
    TRY
      bm.graph.beginTransaction();
      part := rnd.integer(1, bm.parts);
      NodesTraversed := 1.0D0;
      bm.graph.getPartAttributes(part, type, x, y, build);
      NullProc(part, type, x, y);

      Traverse(part, 7, NodesTraversed);
      bm.graph.commitTransaction();
    EXCEPT
    | Error => ErrAbort(); RAISE Error;
    | Thread.Alerted => ErrAbort(); RAISE Thread.Alerted;
    | OO1Graph.Failure (msg) =>
        ErrAbort();
        bm.log.write("Traverse error: " & msg & ".\n");
        RAISE Error;
    END
  END PerformTraversal;

(* Insert: Insert 100 new parts and three connections from each to other,
   randomly selected parts.  Time must be included to update indices or
   other access structures used in the execution of Lookup and Traverse.
   Call a null programming language porcedure to obtain the x,y position
   for each insert.  Commit changes to the disk.*)
PROCEDURE PerformInsert () RAISES {Thread.Alerted, Error} =
  VAR
    x, y       : INTEGER;
    part, cpart: CARDINAL;

  PROCEDURE NullProc (<* UNUSED *> VAR x, y: INTEGER) =
    BEGIN
      (* empty *)
    END NullProc;

  (* PerformInsert *)
  BEGIN
    x := rnd.integer(0, 99999);
    y := rnd.integer(0, 99999);

    TRY
      IF bm.quick THEN bm.graph.beginTransaction(); END;
      FOR i := bm.parts + 1 TO bm.parts + 100 DO
        IF NOT bm.quick THEN bm.graph.beginTransaction(); END;

        part := bm.graph.createPart(i);
        NullProc(x, y);
        bm.graph.putPartAttributes(part, typeString, x, y, date);
        FOR j := 1 TO ConnectsPerPart DO
          cpart := rnd.integer(1, bm.parts);
          bm.graph.connect(part, cpart, conString, rnd.integer(1, 1000));
        END;
        IF NOT bm.quick THEN bm.graph.commitTransaction(); END;
        (* bm.log.write("+"); *)
      END;
      IF bm.quick THEN bm.graph.commitTransaction(); END;
    EXCEPT
    | OO1Graph.Failure (msg) =>
        IF NOT bm.quick THEN ErrAbort(); END;
        bm.log.write("Insert error: " & msg & ".\n");
        RAISE Error;
    END;
  END PerformInsert;

BEGIN
END OO1.
