MODULE Multi EXPORTS Main, Multi;

IMPORT IO, Params, Text, OO7, CRandom, Ctypes, ODMG, Scan, Lex, Fmt,
       FloatMode, SetParams, Globals, Transaction, Database, Utime, Uresource,
       Support, Stdio, Process, RefList, Module, Thread, Rd, ThreadF;
FROM IO IMPORT OpenRead, Put, PutInt;
FROM Support IMPORT ComputeUserTime, ComputeSystemTime, ComputeWallClockTime;
FROM BenchParams IMPORT NumNewCompParts;
FROM VarParams
IMPORT TotalAtomicParts, NumAssmPerAssm, NumAssmLevels, TotalModules,
       TotalCompParts;
FROM SetParams IMPORT Error;
FROM Lex IMPORT Int, Match, Skip;

(*
//////////////////////////////////////////////////////////////////
// Assumes a configuration file of the following format, repeated
// as necessary per client thread:
//
// ClientId	n1
// ReadPriv	n2
// ReadShar	n3
// WritePriv	n4
// WriteShar	n5
// RepeatCount	n6
// SleepTime	n7
//
// where n1 through n7 are integers.  The order of
// parameters is critical!  This may repeat as many times as
// the number of clients in the process.
//////////////////////////////////////////////////////////////////
*)

VAR
  startUsage, endUsage: Uresource.struct_rusage;
  startWallTime, StartWallTime, endWallTime, EndWallTime: Utime.struct_timeval;
  tz: Utime.struct_timezone;
  totalWarmTime, TotalWarmTime: LONGREAL;
  userTime, UserTime, systemTime, SystemTime: LONGREAL;
  aborts, Aborts: INTEGER;

(*
  ///////////////////////////////////////////////////////////////////////////
  // ParseCommandLine parses the original shell call to "bench", determining 
  // which operation to run, how many times to run it, and whether the
  // individual runs are distinct transactions or are lumped together
  // into one large transaction.
  //////////////////////////////////////////////////////////////////////////
*)
PROCEDURE ParseCommandLine (VAR txnCount: INTEGER;
                            VAR chain: BOOLEAN) =
  CONST
    usage =
      " <configFileName> <clients> <count> <clientFileName> <xacts> [-d|-w]\n" &
      " | <configFileName> -r\n" &
      "<xacts> = [chain|nochain]\n" &
      "\t(all repetitions done in one or many distinct transactions)\n";

  BEGIN 
    Put("Call was: ");
    FOR foo := 0 TO Params.Count-1 DO           
      Put(Params.Get(foo)); Put("  ");
    END;
    Put("\n");

    IF Text.Equal(Params.Get(Params.Count-1), "-r") THEN
      reset := TRUE;
      IF Params.Count < 2 THEN
        Put("Usage: ", Stdio.stderr);
        Put(Params.Get(0), Stdio.stderr);
        Put(usage, Stdio.stderr);
        Process.Exit(1);
      END;
      RETURN;
    END;

    IF Text.Equal(Params.Get(Params.Count-1), "-d") THEN
      Globals.debugMode := TRUE;
    ELSIF Text.Equal(Params.Get(Params.Count-1), "-w") THEN
      writeAtomicParts := TRUE;
    END;
    IF Params.Count < 6 THEN
      Put("Usage: ", Stdio.stderr);
      Put(Params.Get(0), Stdio.stderr);
      Put(usage, Stdio.stderr);
      Process.Exit(1);
    END;

    TRY
      clientCount := Scan.Int(Params.Get(2));
    EXCEPT
    | FloatMode.Trap, Lex.Error =>
      Put("Usage: ", Stdio.stderr);
      Put(Params.Get(0), Stdio.stderr);
      Put(usage, Stdio.stderr);
      Process.Exit(1);
    END;

    IF clientCount <= 0 THEN
      Put("Usage: ", Stdio.stderr);
      Put(Params.Get(0), Stdio.stderr);
      Put(usage, Stdio.stderr);
      Process.Exit(1);
    END;

    TRY
      txnCount := Scan.Int(Params.Get(3));
    EXCEPT
    | FloatMode.Trap, Lex.Error =>
      Put("Usage: ", Stdio.stderr);
      Put(Params.Get(0), Stdio.stderr);
      Put(usage, Stdio.stderr);
      Process.Exit(1);
    END;

    IF txnCount <= 0 THEN
      Put("Usage: ", Stdio.stderr);
      Put(Params.Get(0), Stdio.stderr);
      Put(usage, Stdio.stderr);
      Process.Exit(1);
    END;

    IF Text.Equal(Params.Get(5), "chain") THEN 
      chain := TRUE;
    END;
  END ParseCommandLine;

<*FATAL SetParams.Error*>
<*FATAL Globals.Error*>
<*FATAL Database.NotFound*>
<*FATAL Database.Opened*>
<*FATAL Database.Closed*>
<*FATAL Transaction.InProgress*>
<*FATAL Transaction.NotInProgress*>

TYPE BasicOp = { readPriv, readShar, writePriv, writeShar };
TYPE Client = Thread.Closure OBJECT
  id: INTEGER;
  repeatCount: INTEGER;
  sleepTime: LONGREAL;
  op: ARRAY[0..99] OF BITS 2 FOR BasicOp;
OVERRIDES
  apply := ClientThread;
END;

VAR
  mutex := NEW(MUTEX);
  threadsReady := 0;
  threadsDone := 0;
  ready := NEW(Thread.Condition);
  start := NEW(Thread.Condition);
  finish := NEW(Thread.Condition);

PROCEDURE ClientThread (closure: Thread.Closure): REFANY =
  VAR
    self := NARROW(closure, Client);
    random: ARRAY[1..256] OF CHAR;
    state := CRandom.State(random);
    myAborts: INTEGER;
    tr := NEW(Transaction.T);
    iter := 0;
    rand: INTEGER;
    count := 0;
    wr: Transaction.T;

  PROCEDURE ClientTransaction(moduleId: INTEGER; op: OO7.BenchmarkOp;
                              state: Ctypes.void_star;
                              writer: Transaction.T): INTEGER =
    VAR
      moduleH: Module.T;
      moduleName: TEXT;
    BEGIN
      moduleName := Fmt.F("Module %08s", Fmt.Int(moduleId));
      IF Globals.debugMode THEN
        Put("Traversing Module= "); Put(moduleName); Put("\n");
      END;
      VAR ref: REFANY;
      BEGIN
        IF NOT Globals.ModuleIdx.get(moduleName, ref) THEN
          Put("ERROR: Unable to access ", Stdio.stderr);
          Put(moduleName, Stdio.stderr);
          Put(".\n", Stdio.stderr);
          tr.abort();
          Process.Exit(1);
        END;
        moduleH := ref;
      END;
      RETURN moduleH.traverseRandom(op, state, writer);
    END ClientTransaction;

  BEGIN
    ThreadF.SuspendOthers();
    VAR oldstate := CRandom.initstate(self.id, state, 256);
    BEGIN
      EVAL CRandom.setstate(oldstate);
    END;
    ThreadF.ResumeOthers();

    LOOP
      (* wait for starting gun *)
      myAborts := 0;
      LOCK mutex DO
        INC(threadsReady);
        IF threadsReady = threadCount THEN Thread.Signal(ready) END;
        Thread.Wait(mutex, start);
      END;

      LOOP
        TRY
          (* Start a new transaction if either this is the first iteration
             of a multioperation transaction or we we are running each
             operation as a separate transaction *)
          IF NOT chain OR myAborts > 0 OR iter = 0 THEN
            tr.begin();
          ELSE
            (* do nothing *)
          END;

          (* set random seed so "hot" runs are truly hot *)
          ThreadF.SuspendOthers();
          VAR oldstate := CRandom.setstate(state);
          BEGIN
            CRandom.srandom(self.id);
            EVAL CRandom.setstate(oldstate);
          END;
          ThreadF.ResumeOthers();

          FOR i := 0 TO self.repeatCount-1 DO
            ThreadF.SuspendOthers();
            VAR oldstate := CRandom.setstate(state);
            BEGIN
              rand := CRandom.random() MOD 100;
              EVAL CRandom.setstate(oldstate);
            END;
            ThreadF.ResumeOthers();
            
            CASE self.op[rand] OF
            | BasicOp.readPriv =>
              count := ClientTransaction(self.id, OO7.BenchmarkOp.Trav1, state,
                                         NIL);
            | BasicOp.readShar =>
              count := ClientTransaction(TotalModules,
                                         OO7.BenchmarkOp.Trav1,  state, NIL);
            | BasicOp.writePriv =>
              count := ClientTransaction(self.id,
                                         OO7.BenchmarkOp.Trav2b, state, NIL);
            | BasicOp.writeShar =>
              IF writeAtomicParts THEN
                wr := tr;
              END;
              count := ClientTransaction(TotalModules,
                                         OO7.BenchmarkOp.Trav2b, state, wr);
            END;

            IF Globals.debugMode THEN
              Put("Visited="); PutInt(count); Put("\n");
            END;
            IF self.sleepTime # 0.0D0 THEN Thread.Pause(self.sleepTime) END;
          END;

          (* Commit the current transaction if
             we are running the last iteration
             or running a multitransaction test and not chaining
             Chain the tx if we are chaining and not on
             the last iteration *)
          IF NOT chain OR iter = txnCount-1 THEN
            tr.commit();
          ELSE
            tr.chain();
          END;
          EXIT;
        EXCEPT
        | Thread.Aborted =>
          INC(myAborts);
          Thread.Pause(FLOAT(myAborts, LONGREAL) * 0.05D0);
        END;
      END;

      LOCK mutex DO
        INC(threadsDone);
        INC(aborts, myAborts);
        IF threadsDone = threadCount THEN Thread.Signal(finish) END;
      END;

      INC(iter);
    END;
  END ClientThread;

PROCEDURE MinTime(localTime: Utime.struct_timeval;
                  VAR globalTime: Utime.struct_timeval) =
  BEGIN
    IF globalTime.tv_sec = 0 AND globalTime.tv_usec = 0
      OR localTime.tv_sec < globalTime.tv_sec
      OR (localTime.tv_sec = globalTime.tv_sec
      AND localTime.tv_usec < globalTime.tv_usec)
     THEN
      globalTime := localTime;
    END;
  END MinTime;

PROCEDURE MaxTime(localTime: Utime.struct_timeval;
                  VAR globalTime: Utime.struct_timeval) =
  BEGIN
    IF globalTime.tv_sec = 0 AND globalTime.tv_usec = 0
      OR localTime.tv_sec > globalTime.tv_sec
      OR (localTime.tv_sec = globalTime.tv_sec
      AND localTime.tv_usec > globalTime.tv_usec)
     THEN
      globalTime := localTime;
    END;
  END MaxTime;

VAR
  db := ODMG.Open("OO7");
  txnCount := 1;
  chain := FALSE;
  writeAtomicParts := FALSE;
  threadCount := 0;
  clientCount := 0;
  main := NEW(Transaction.T);
  reset: BOOLEAN;
BEGIN
  (* See if debug mode is desired, see which operation to run,
     and how many times to run it. *)
  ParseCommandLine(txnCount, chain);

  IF reset THEN ipc_reset(); Process.Exit(0) END;

  TRY
    main.begin();

    (* initialize parameters for benchmark. *)
    SetParams.FromFile(Params.Get(1));
    Globals.nextAtomicId := TotalAtomicParts + 1;
    Globals.nextCompositeId := TotalCompParts + 1;

    Globals.InitGlobals(db);

    main.commit();
  EXCEPT
  | Thread.Aborted => Put("Aborted\n", Stdio.stderr);
  END;

  (* fork clients *)
  Thread.IncDefaultStackSize(4 * Thread.GetDefaultStackSize());
  VAR
    configFileName := Params.Get(4);
    configFile := OpenRead(configFileName);
    clientId: INTEGER;
  PROCEDURE GetParam(param: TEXT): INTEGER RAISES { Error } =
    VAR value: INTEGER;
    BEGIN
      TRY
        Skip(configFile);
        Match(configFile, param);
        value := Int(configFile);
      EXCEPT
      | Rd.Failure, Lex.Error, Thread.Alerted, FloatMode.Trap =>
        Put("Error reading configuration parameter ", Stdio.stderr);
        Put(param, Stdio.stderr); Put("\n", Stdio.stderr);
        RAISE Error;
      END;
      Put(param); Put(" = "); PutInt(value); Put(".\n");
      RETURN value;
    END GetParam;
  BEGIN
    IF configFile = NIL THEN
      Put("Couldn't open config file: ", Stdio.stderr);
      Put(configFileName, Stdio.stderr); Put("\n", Stdio.stderr);
      RAISE Error;
    END;

    (* Get client parameters *)
    REPEAT
      clientId := GetParam("ClientId");
      INC(threadCount);
      IF clientId >= TotalModules THEN
        Put("More clients than modules", Stdio.stderr);
        Process.Exit(1);
      END;
      VAR
        rdPriv := GetParam("ReadPriv");
        rdShar := GetParam("ReadShar");
        wrPriv := GetParam("WritePriv");
        wrShar := GetParam("WriteShar");
        client := NEW(Client,
                      id := clientId,
                      repeatCount := GetParam("RepeatCount"),
                      sleepTime :=
                          FLOAT(GetParam("SleepTime"), LONGREAL) /
        FLOAT(1000, LONGREAL));
        i := 0;
      BEGIN
        IF rdPriv + rdShar + wrPriv + wrShar # 100 THEN
          Put("Bad basic operation vector: (", Stdio.stderr);
          PutInt(rdPriv, Stdio.stderr); Put(", ", Stdio.stderr);
          PutInt(rdShar, Stdio.stderr); Put(", ", Stdio.stderr);
          PutInt(wrPriv, Stdio.stderr); Put(", ", Stdio.stderr);
          PutInt(wrShar, Stdio.stderr); Put(")\n", Stdio.stderr);
          Process.Exit(1);
        END;
        FOR j := 1 TO rdPriv DO
          client.op[i] := BasicOp.readPriv;
          INC(i);
        END;
        FOR j := 1 TO rdShar DO
          client.op[i] := BasicOp.readShar;
          INC(i);
        END;
        FOR j := 1 TO wrPriv DO
          client.op[i] := BasicOp.writePriv;
          INC(i);
        END;
        FOR j := 1 TO wrShar DO
          client.op[i] := BasicOp.writeShar;
          INC(i);
        END;
        <* ASSERT i = 100 *>
        EVAL Thread.Fork(client);
      END;
      TRY
        Skip(configFile);
      EXCEPT
      | Rd.Failure, Thread.Alerted =>
        Put("Error skipping to next parameter\n", Stdio.stderr);
      END;
    UNTIL IO.EOF(configFile);
  END;
  IF chain AND threadCount > 1 THEN
    Put("Cannot run multiple chained clients\n", Stdio.stderr);
    Process.Exit(1);
  END;

  (* Compute structural info needed by the update operations,
     since these operations need to know which id's should
     be used next. *)
  VAR
    baseCnt := NumAssmPerAssm;
    complexCnt := 1;
  BEGIN
    FOR i := 1 TO NumAssmLevels-2 DO
      baseCnt := baseCnt * NumAssmPerAssm;
      INC(complexCnt, complexCnt * NumAssmPerAssm);
    END;
    Globals.nextBaseAssemblyId := TotalModules * baseCnt + 1;
    Globals.nextComplexAssemblyId := TotalModules * complexCnt + 1;
    Globals.nextAtomicId := TotalAtomicParts + 1;
    Globals.nextCompositeId := TotalCompParts + 1;
  END;

  (* needed for insert and delete tests *)
  Globals.shared_cp  := NEW(REF ARRAY OF RefList.T,
                            TotalCompParts + NumNewCompParts + 1);
  Globals.private_cp := NEW(REF ARRAY OF RefList.T,
                            TotalCompParts + NumNewCompParts + 1);

  IF NOT chain AND threadCount < clientCount THEN
    ipc_open(clientCount, threadCount)
  END;

  FOR iter := 0 TO txnCount-1 DO

    aborts := 0;

    (* wait for all the threads to be ready *)
    LOCK mutex DO
      WHILE threadsReady # threadCount DO Thread.Wait(mutex, ready) END;
      threadsReady := 0;
    END;

    IF NOT chain THEN
      LOOP
        TRY
          main.begin();
          Globals.ResetTimes();
          main.commit();
          EXIT;
        EXCEPT
        | Thread.Aborted =>
        END;
      END;
      IF threadCount < clientCount THEN ipc_start(threadCount) END;
    END;
    
    Put("RUNNING OO7 BENCHMARK OPERATION, ");
    PutInt(threadCount); Put(" thread(s)");
    Put(", iterations = "); PutInt(txnCount); Put(".\n");

    (* get wall clock time *)
    EVAL Utime.gettimeofday(startWallTime, tz);

    (* get starting usage values. *)
    EVAL Uresource.getrusage(Uresource.RUSAGE_SELF, startUsage);
    
    Thread.Broadcast(start);

    LOCK mutex DO
      WHILE threadsDone # threadCount DO Thread.Wait(mutex, finish) END;
      threadsDone := 0;
    END;

    (* get final usage values. *)
    EVAL Uresource.getrusage(Uresource.RUSAGE_SELF, endUsage);     

    (* get wall clock time *)
    EVAL Utime.gettimeofday(endWallTime, tz);

    (* compute processor usage *)
    userTime := ComputeUserTime(startUsage, endUsage);
    systemTime := ComputeSystemTime(startUsage, endUsage);

    IF NOT chain THEN
      LOOP
        TRY
          main.begin();
          Globals.UserTime^ := Globals.UserTime^ + userTime;
          Globals.SystemTime^ := Globals.SystemTime^ + systemTime;
          INC(Globals.Aborts^, aborts);
          MinTime(startWallTime, Globals.StartWallTime^);
          MaxTime(endWallTime, Globals.EndWallTime^);
          main.commit();
          EXIT;
        EXCEPT
        | Thread.Aborted =>
        END;
      END;
      IF threadCount < clientCount THEN ipc_stop(threadCount) END;
      LOOP
        TRY
          main.begin();
          Aborts := Globals.Aborts^;
          StartWallTime := Globals.StartWallTime^;
          EndWallTime := Globals.EndWallTime^;
          UserTime := Globals.UserTime^;
          SystemTime := Globals.SystemTime^;
          main.commit();
          EXIT;
        EXCEPT
        | Thread.Aborted =>
        END;
      END;
    ELSE
      Aborts := aborts;
      StartWallTime := startWallTime;
      EndWallTime := endWallTime;
      UserTime := userTime;
      SystemTime := systemTime;
    END;

    (* compute and report wall clock time *)
    Put("PM3");
    Put(", iteration= "); PutInt(iter);
    Put(", elapsedTime= ");
    Put(Fmt.LongReal(ComputeWallClockTime(StartWallTime, EndWallTime)));
    Put(" seconds (");
    Put(Fmt.LongReal(ComputeWallClockTime(startWallTime, endWallTime)));
    Put(")\n");

    (* Compute and report CPU time. *)
    Put("CPU time: ");
    Put(Fmt.LongReal(UserTime+SystemTime));
    Put(" (");
    Put(Fmt.LongReal(userTime+systemTime));
    Put(").\n(");
    Put(Fmt.LongReal(UserTime));
    Put(" seconds user, ");
    Put(Fmt.LongReal(SystemTime));
    Put(" seconds system.) (");
    Put(Fmt.LongReal(userTime));
    Put(", ");
    Put(Fmt.LongReal(systemTime));
    Put(")\n");

    Put("maximum resident set size: "); PutInt(endUsage.ru_maxrss);
    Put("\nintegral resident set size: "); PutInt(endUsage.ru_idrss);
    Put("\npage faults not requiring physical I/O: ");
    PutInt(endUsage.ru_minflt - startUsage.ru_minflt);
    Put("\npage faults requiring physical I/O: ");
    PutInt(endUsage.ru_majflt - startUsage.ru_majflt);
    Put("\nswaps: ");
    PutInt(endUsage.ru_nswap - startUsage.ru_nswap);
    Put("\nblock input operations: ");
    PutInt(endUsage.ru_inblock - startUsage.ru_inblock);
    Put("\nblock output operations: ");
    PutInt(endUsage.ru_oublock - startUsage.ru_oublock);
    Put("\nmessages sent: ");
    PutInt(endUsage.ru_msgsnd - startUsage.ru_msgsnd);
    Put("\nmessages received: ");
    PutInt(endUsage.ru_msgrcv - startUsage.ru_msgrcv);
    Put("\nsignals received: ");
    PutInt(endUsage.ru_nsignals - startUsage.ru_nsignals);
    Put("\nvoluntary context switches: ");
    PutInt(endUsage.ru_nvcsw - startUsage.ru_nvcsw);
    Put("\ninvoluntary context switches: ");
    PutInt(endUsage.ru_nivcsw - startUsage.ru_nivcsw);
    Put("\naborts: "); PutInt(Aborts); Put(" ("); PutInt(aborts); Put(")");
    Put("\n\n");

    IF iter = 1 THEN
      TotalWarmTime := ComputeWallClockTime(StartWallTime, EndWallTime);
      totalWarmTime := ComputeWallClockTime(startWallTime, endWallTime);
    ELSIF iter > 1 AND iter <= txnCount-2 THEN
      TotalWarmTime :=
          TotalWarmTime + ComputeWallClockTime(StartWallTime, EndWallTime);
      totalWarmTime :=
          totalWarmTime + ComputeWallClockTime(startWallTime, endWallTime);
    END;

    IF (txnCount > 2) AND (iter = txnCount-2) THEN
      (* compute average hot time for 2nd through n-1 th iterations *)
      Put("PM3");
      Put(", average hot elapsedTime=");
      Put(Fmt.LongReal(TotalWarmTime / FLOAT(txnCount-2, LONGREAL)));
      Put(" seconds (");
      Put(Fmt.LongReal(totalWarmTime / FLOAT(txnCount-2, LONGREAL)));
      Put(")\n");
    END;

    Database.Checkpoint();
  END;

  IF NOT chain AND threadCount < clientCount THEN ipc_close(threadCount) END;

  (*
    //////////////////////////////////////////////////////////////////
    //
    // Shutdown 
    //
    //////////////////////////////////////////////////////////////////
  *)
  db.close();
END Multi.
