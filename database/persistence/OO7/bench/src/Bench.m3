MODULE Bench EXPORTS Bench, Main;

IMPORT IO, Params, Text, OO7, CRandom, AtomicPart, ODMG, Scan, Lex, Fmt,
       FloatMode, SetParams, Globals, Transaction, Database, Utime, Uresource,
       Support, Stdio, Process, RefList, Module, BenchParams, VarParams;
FROM IO IMPORT Put, PutInt;
FROM Support IMPORT ComputeUserTime, ComputeSystemTime, ComputeWallClockTime;

VAR     
  startUsage, endUsage: Uresource.struct_rusage;
  startWallTime, endWallTime, startWarmTime: Utime.struct_timeval;
  tz: Utime.struct_timezone;

(*
  ///////////////////////////////////////////////////////////////////////////
  // ParseCommandLine parses the original shell call to "bench", determining 
  // which operation to run, how many times to run it, and whether the
  // individual runs are distinct transactions or are lumped together
  // into one large transaction.
  //////////////////////////////////////////////////////////////////////////
*)
PROCEDURE ParseCommandLine (VAR opIndex, repeatCount: INTEGER;
                            VAR whichOp: OO7.BenchmarkOp;
                            VAR manyXACTS: BOOLEAN) =
  CONST
    usage1 =
      " <configFileName> <count> <op> <xacts> [-d]\n" &
      "<count> = number of times to repeat <op>.\n";
    usage2 =
      "Currently supported <op>s: t1, t1ww, t2a, t2b, t2c, t3a, t3b, t3c\n" &
      "\t t4, t5do, t5undo, t6, t7, t8, t9, t10\n" &
      "\t q1, q2, q3, q4, q5, q6, q7, q8\n" &
      "\t i, d, r1, r2, wu\n";
    usage3 =
      "<xacts> = [single|one|many]\n" &
      "\t(all repetitions done in one or many distinct transactions)\n";
  BEGIN 
    Put("Call was: ");
    FOR foo := 0 TO Params.Count-1 DO           
      Put(Params.Get(foo)); Put("  ");
    END;
    Put("\n");
    opIndex := 3;

    IF Text.Equal(Params.Get(Params.Count-1), "-d") THEN
      Globals.debugMode := TRUE;
    END;
    IF Params.Count < 5 THEN
      Put("Usage: ", Stdio.stderr);
      Put(Params.Get(0), Stdio.stderr);
      Put(usage1, Stdio.stderr);
      Put(usage2, Stdio.stderr);
      Put(usage3, Stdio.stderr);
      Process.Exit(1);
    END;

    TRY
      repeatCount := Scan.Int(Params.Get(2));
    EXCEPT
    | FloatMode.Trap, Lex.Error =>
      Put("Usage: ", Stdio.stderr);
      Put(Params.Get(0), Stdio.stderr);
      Put(usage1, Stdio.stderr);
      Process.Exit(1);
    END;

    IF repeatCount <= 0 THEN
      Put("Usage: ", Stdio.stderr);
      Put(Params.Get(0), Stdio.stderr);
      Put(usage1, Stdio.stderr);
      Put(usage2, Stdio.stderr);
      Put(usage3, Stdio.stderr);
      Process.Exit(1);
    END;

    IF Text.Equal(Params.Get(4), "many") THEN 
      manyXACTS := TRUE;
    END;
    
    WITH op = Params.Get(opIndex) DO
      IF Text.Equal(op, "t1") THEN
        whichOp := OO7.BenchmarkOp.Trav1;
      ELSIF Text.Equal(op, "t1ww") THEN
        whichOp := OO7.BenchmarkOp.Trav1WW;
        TRY
          IF Globals.debugMode THEN
            AtomicPart.WorkAmount := Scan.Int(Params.Get(Params.Count-2));
          ELSE
            AtomicPart.WorkAmount := Scan.Int(Params.Get(Params.Count-1));
          END;
        EXCEPT
        | FloatMode.Trap, Lex.Error =>
          Put("Usage: ", Stdio.stderr);
          Put(Params.Get(0), Stdio.stderr);
          Put(usage1, Stdio.stderr);
          Process.Exit(1);
        END;
        Put("WorkAmount = "); PutInt(AtomicPart.WorkAmount); Put("\n");
      ELSIF Text.Equal(op, "t2a") THEN
        whichOp := OO7.BenchmarkOp.Trav2a;
      ELSIF Text.Equal(op, "t2b") THEN
        whichOp := OO7.BenchmarkOp.Trav2b;
      ELSIF Text.Equal(op, "t2c") THEN 
        whichOp := OO7.BenchmarkOp.Trav2c;
      ELSIF Text.Equal(op, "t3a") THEN
        whichOp := OO7.BenchmarkOp.Trav3a;
      ELSIF Text.Equal(op, "t3b") THEN
        whichOp := OO7.BenchmarkOp.Trav3b;
      ELSIF Text.Equal(op, "t3c") THEN
        whichOp := OO7.BenchmarkOp.Trav3c;
      ELSIF Text.Equal(op, "t4") THEN
        whichOp := OO7.BenchmarkOp.Trav4;
      ELSIF Text.Equal(op, "t5do") THEN
        whichOp := OO7.BenchmarkOp.Trav5do;
      ELSIF Text.Equal(op, "t5undo") THEN
        whichOp := OO7.BenchmarkOp.Trav5undo;
      ELSIF Text.Equal(op, "t6") THEN
        whichOp := OO7.BenchmarkOp.Trav6;
      ELSIF Text.Equal(op, "t7") THEN
        whichOp := OO7.BenchmarkOp.Trav7;
      ELSIF Text.Equal(op, "t8") THEN
        whichOp := OO7.BenchmarkOp.Trav8;
      ELSIF Text.Equal(op, "t9") THEN 
        whichOp := OO7.BenchmarkOp.Trav9;
      ELSIF Text.Equal(op, "t10") THEN
        whichOp := OO7.BenchmarkOp.Trav10;
      ELSIF Text.Equal(op, "q1") THEN
        whichOp := OO7.BenchmarkOp.Query1;
      ELSIF Text.Equal(op, "q2") THEN
        whichOp := OO7.BenchmarkOp.Query2;
      ELSIF Text.Equal(op, "q3") THEN
        whichOp := OO7.BenchmarkOp.Query3;
      ELSIF Text.Equal(op, "q4") THEN
        whichOp := OO7.BenchmarkOp.Query4;
      ELSIF Text.Equal(op, "q5") THEN
        whichOp := OO7.BenchmarkOp.Query5;
      ELSIF Text.Equal(op, "q6") THEN
        whichOp := OO7.BenchmarkOp.Query6;
      ELSIF Text.Equal(op, "q7") THEN
        whichOp := OO7.BenchmarkOp.Query7;
      ELSIF Text.Equal(op, "q8") THEN
        whichOp := OO7.BenchmarkOp.Query8;
      ELSIF Text.Equal(op, "i") THEN
        whichOp := OO7.BenchmarkOp.Insert;
      ELSIF Text.Equal(op, "d") THEN
        whichOp := OO7.BenchmarkOp.Delete;
      ELSIF Text.Equal(op, "r1") THEN
        whichOp := OO7.BenchmarkOp.Reorg1;
      ELSIF Text.Equal(op, "r2") THEN
        whichOp := OO7.BenchmarkOp.Reorg2;
      ELSIF Text.Equal(op, "wu") THEN
        whichOp := OO7.BenchmarkOp.WarmUpdate;
      ELSE
        Put("ERROR: Illegal OO7 operation specified.\n", Stdio.stderr);
        Put(usage2, Stdio.stderr);
        Process.Exit(1);
      END
    END;
  END ParseCommandLine;

<*FATAL SetParams.Error*>
<*FATAL Globals.Error*>
<*FATAL Database.NotFound*>
<*FATAL Database.Opened*>
<*FATAL Database.Closed*>
<*FATAL Transaction.InProgress*>
<*FATAL Transaction.NotInProgress*>

VAR
  db := ODMG.Open("OO7");
  tr := NEW(Transaction.T);
  moduleH: Module.T;
  resultText: TEXT;                      (* buffer to hold result of        *)
					 (* operation for printing outside  *)
					 (* of timing region.               *)
  moduleName: TEXT;
  opIndex := 2;
  repeatCount := 1;
  whichOp := OO7.BenchmarkOp.Trav1;
  manyXACTS := FALSE;
BEGIN
  tr.begin();

  (* See if debug mode is desired, see which operation to run,
     and how many times to run it. *)
  ParseCommandLine(opIndex, repeatCount, whichOp, manyXACTS);

  (* initialize parameters for benchmark. *)
  SetParams.FromFile(Params.Get(1));
  Globals.nextAtomicId := VarParams.TotalAtomicParts + 1;
  Globals.nextCompositeId := VarParams.TotalCompParts + 1;

  Globals.InitGlobals(db);

  tr.commit();

  (* Compute structural info needed by the update operations,
     since these operations need to know which id's should
     be used next. *)
  VAR
    baseCnt := VarParams.NumAssmPerAssm;
    complexCnt := 1;
  BEGIN
    FOR i := 1 TO VarParams.NumAssmLevels-2 DO
      baseCnt := baseCnt * VarParams.NumAssmPerAssm;
      INC(complexCnt, complexCnt * VarParams.NumAssmPerAssm);
    END;
    Globals.nextBaseAssemblyId := VarParams.TotalModules * baseCnt + 1;
    Globals.nextComplexAssemblyId := VarParams.TotalModules * complexCnt + 1;
    Globals.nextAtomicId := VarParams.TotalAtomicParts + 1;
    Globals.nextCompositeId := VarParams.TotalCompParts + 1;
  END;

  (* needed for insert and delete tests *)
  Globals.shared_cp  := NEW(REF ARRAY OF RefList.T,
                            VarParams.TotalCompParts + BenchParams.NumNewCompParts + 1);
  Globals.private_cp := NEW(REF ARRAY OF RefList.T,
                            VarParams.TotalCompParts + BenchParams.NumNewCompParts + 1);

  (* Actually run the darn thing. *)
  FOR iter := 0 TO repeatCount-1 DO
    (*
      //////////////////////////////////////////////////////////////////
      // Run an OO7 Benchmark Operation
      //
      //////////////////////////////////////////////////////////////////
    *)

    Put("RUNNING OO7 BENCHMARK OPERATION "); Put(Params.Get(opIndex));
    Put(", iteration = "); PutInt(iter); Put(".\n");

    (* get wall clock time *)
    EVAL Utime.gettimeofday(startWallTime, tz);

    (* get starting usage values. *)
    EVAL Uresource.getrusage(Uresource.RUSAGE_SELF, startUsage);

    (* Start a new transaction if either this is the first iteration
       of a multioperation transaction or we we are running each
       operate as a separate transaction *)

    IF iter = 0 THEN
      tr.begin();
    ELSIF manyXACTS THEN
      IF Globals.chain_tx THEN
        (* do nothing *)
      ELSE
        tr.begin();
      END;
    END;

    (* set random seed so "hot" runs are truly hot *)
    CRandom.srandom(1);

    (* Use random module for the operation *)
    (* moduleId := (CRandom.random() MOD VarParams.TotalModules) + 1 *)
    FOR moduleId := 1 TO VarParams.TotalModules DO
      moduleName := Fmt.F("Module %08s", Fmt.Int(moduleId));
      Put("Traversing Module= "); Put(moduleName); Put("\n");
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

      (* Perform the requested operation on the chosen module *)
      VAR
        count := 0;
      BEGIN
        CASE (whichOp) OF
        | OO7.BenchmarkOp.Trav1 =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 1 DFS visited %s atomic parts.\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav1WW =>
          AtomicPart.RealWork := TRUE;
          whichOp := OO7.BenchmarkOp.Trav1; (* so traverse methods don't complain *)
          count := moduleH.traverse(whichOp);
          whichOp := OO7.BenchmarkOp.Trav1WW;  (* for next (hot) traversal *)
          resultText :=
              Fmt.F("Traversal 1WW DFS visited %s atomic parts.\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav2a =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 2A swapped %s pairs of (X,Y) coordinates.\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav2b =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 2B swapped %s pairs of (X,Y) coordinates.\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav2c =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 2C swapped %s pairs of (X,Y) coordinates.\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav3a =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 3A toggled %s dates.\n", Fmt.Int(count));
        | OO7.BenchmarkOp.Trav3b =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 3B toggled %s dates.\n", Fmt.Int(count));
        | OO7.BenchmarkOp.Trav3c =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 3C toggled %s dates.\n", Fmt.Int(count));
        | OO7.BenchmarkOp.Trav4 =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 4: %s instances of the character found\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav5do =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 5(DO): %s string replacements performed\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav5undo=>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 5(UNDO): %s string replacements performed\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav6 =>
          count := moduleH.traverse(whichOp);
          resultText :=
              Fmt.F("Traversal 6: visited %s atomic part roots.\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav7 =>
          count := traverse7();
          resultText :=
              Fmt.F("Traversal 7: found %s assemblies using random atomic part.\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav8 =>
          count := moduleH.scanManual();
          resultText :=
              Fmt.F("Traversal 8: found %s occurrences of character in manual.\n",
                    Fmt.Int(count));
        | OO7.BenchmarkOp.Trav9 =>
          count := moduleH.firstLast();
          resultText :=
              Fmt.F("Traversal 9: match was %s.\n", Fmt.Int(count));
        | OO7.BenchmarkOp.Trav10 =>
          (* run traversal #1 on every module. *)
          count := 0;
          whichOp := OO7.BenchmarkOp.Trav1;  (* so object methods don't complain *)
          FOR moduleId := 1 TO VarParams.TotalModules DO
            moduleName := Fmt.F("Module %08s", Fmt.Int(moduleId));
            VAR ref: REFANY;
            BEGIN
              IF NOT Globals.ModuleIdx.get(moduleName, ref) THEN
                Put("ERROR: t10 Unable to access ", Stdio.stderr);
                Put(moduleName, Stdio.stderr);
                Put(".\n", Stdio.stderr);
                tr.abort();
                Process.Exit(1);
              END;
              moduleH := ref;
            END;
            INC(count, moduleH.traverse(whichOp));
          END;
          resultText :=
              Fmt.F("Traversal 10 visited %s atomic parts in %s modules.\n",
                    Fmt.Int(count),
                    Fmt.Int(VarParams.TotalModules));
          whichOp := OO7.BenchmarkOp.Trav10;  (* for next time around *)

        | OO7.BenchmarkOp.Query1 =>
          count := query1();
          resultText :=
              Fmt.F("Query one retrieved %s atomic parts.\n",
                    Fmt.Int(count));

        | OO7.BenchmarkOp.Query2 =>
          count := query2();
          resultText :=
              Fmt.F("Query two retrieved %s qualifying atomic parts.\n",
                    Fmt.Int(count));

        | OO7.BenchmarkOp.Query3 =>
          count := query3();
          resultText :=
              Fmt.F("Query three retrieved %s qualifying atomic parts.\n",
                    Fmt.Int(count));

        | OO7.BenchmarkOp.Query4 =>
          count := query4();
          resultText :=
              Fmt.F("Query four retrieved %s (document, base assembly) pairs.\n",
                    Fmt.Int(count));

        | OO7.BenchmarkOp.Query5 =>
          count := query5();
          resultText :=
              Fmt.F("Query five retrieved %s out-of-date base assemblies.\n",
                    Fmt.Int(count));

        | OO7.BenchmarkOp.Query6 =>
          count := query6();
          resultText :=
              Fmt.F("Query six retrieved %s out-of-date assemblies.\n",
                    Fmt.Int(count));

        | OO7.BenchmarkOp.Query7 =>
          count := query7();
          resultText :=
              Fmt.F("Query seven iterated through %s atomic parts.\n",
                    Fmt.Int(count));

        | OO7.BenchmarkOp.Query8 =>
          count := query8();
          resultText :=
              Fmt.F("Query eight found %s atomic part/document matches.\n",
                    Fmt.Int(count));

        | OO7.BenchmarkOp.Insert =>
          insert1();
          resultText :=
              Fmt.F("Inserted %s composite parts (a total of %s atomic parts.)\n",
                    Fmt.Int(BenchParams.NumNewCompParts),
                    Fmt.Int(BenchParams.NumNewCompParts * VarParams.NumAtomicPerComp));

        | OO7.BenchmarkOp.Delete =>
          delete1();
          resultText :=
              Fmt.F("Deleted %s composite parts (a total of %s atomic parts.)\n",
                    Fmt.Int(BenchParams.NumNewCompParts),
                    Fmt.Int(BenchParams.NumNewCompParts * VarParams.NumAtomicPerComp));

        | OO7.BenchmarkOp.WarmUpdate =>
          (* first do the t1 traversal to warm the cache *)
          count := moduleH.traverse(OO7.BenchmarkOp.Trav1);
          (* then call T2 to do the update *)
          count := moduleH.traverse(OO7.BenchmarkOp.Trav2a);
          resultText :=
              Fmt.F("Warm update swapped %s pairs of (X,Y) coordinates.\n",
                    Fmt.Int(count));
        ELSE
          Put("Sorry, that operation isn't available yet.\n", Stdio.stderr);
          tr.abort();
          Process.Exit(1);
        END;
        Put("Visited="); PutInt(count); Put("\n");
      END;

      (* Commit the current transaction if
         we are running the last iteration
         or running a multitransaction test and not chaining
         Chain the tx if we are chaining and not on
         the last iteration *)
      IF iter = repeatCount-1 THEN
        tr.commit();
      ELSIF manyXACTS THEN
        IF Globals.chain_tx THEN
          tr.chain();
        ELSE
          tr.commit();
        END
      END
    END;

    (* compute and report wall clock time *)
    EVAL Utime.gettimeofday(endWallTime, tz);
    Put("PM3, operation= "); Put(Params.Get(opIndex));
    Put(", iteration= "); PutInt(iter);
    Put(", elapsedTime= ");
    Put(Fmt.LongReal(ComputeWallClockTime(startWallTime, endWallTime)));
    Put(" seconds\n");
    IF (iter = 1) THEN
      startWarmTime := startWallTime;
    END;

    (* Compute and report CPU time. *)
    EVAL Uresource.getrusage(Uresource.RUSAGE_SELF, endUsage);
    Put(resultText);
    Put("CPU time: ");
    Put(Fmt.LongReal(
            ComputeUserTime(startUsage, endUsage) +
            ComputeSystemTime(startUsage, endUsage)));
    Put(".\n(");
    Put(Fmt.LongReal(ComputeUserTime(startUsage, endUsage)));
    Put(" seconds user, ");
    Put(Fmt.LongReal(ComputeSystemTime(startUsage, endUsage)));
    Put(" seconds system.)\n");

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
    Put("\n\n");

    IF (repeatCount > 2) AND (iter = repeatCount-2) THEN
      (* compute average hot time for 2nd through n-1 th iterations *)
      Put("PM3, operation="); Put(Params.Get(opIndex));
      Put(", average hot elapsedTime=");
      Put(Fmt.LongReal(
              ComputeWallClockTime(startWarmTime, endWallTime) /
              FLOAT(repeatCount-2, LONGREAL)));
      Put(" seconds\n");
    END;
  END;

  (*
    //////////////////////////////////////////////////////////////////
    //
    // Shutdown 
    //
    //////////////////////////////////////////////////////////////////
  *)
  db.close();
END Bench. 
