MODULE GenDB EXPORTS Main;

IMPORT Params, Module, Fmt, CompositePart, Globals, SetParams, ODMG,
       Transaction, Utime, Uresource, Support, RefList, Database, Stdio,
       Process, Text, VarParams;
FROM IO IMPORT Put, PutInt;
FROM Support IMPORT ComputeWallClockTime, ComputeUserTime, ComputeSystemTime;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Do Benchmark Data Generation
  //
  //////////////////////////////////////////////////////////////////
*)

VAR
  startWallTime, endWallTime: Utime.struct_timeval;
  startUsage, endUsage: Uresource.struct_rusage;
  tz: Utime.struct_timezone;

<*FATAL Globals.Error*>
<*FATAL SetParams.Error*>

PROCEDURE gendb () =
  VAR
    id: INTEGER;
    moduleH: Module.T;
    cp: CompositePart.T;
    db := ODMG.Open("OO7");
    tr := NEW(Transaction.T);
    
  BEGIN
    tr.begin();

    (* reinitialize these globals *)
    Globals.nextAtomicId := 1;
    Globals.nextCompositeId := 1;
    Globals.nextComplexAssemblyId := 1;
    Globals.nextBaseAssemblyId := 1;
    Globals.nextModuleId := 1;

    (* create all the indices needed *)
    Globals.InitGlobals(db);

    Put("TotalCompParts = "); PutInt(VarParams.TotalCompParts); Put("\n");

    Globals.shared_cp  := NEW(REF ARRAY OF RefList.T, VarParams.TotalCompParts + 1);
    Globals.private_cp := NEW(REF ARRAY OF RefList.T, VarParams.TotalCompParts + 1);

    (* First generate the desired number of modules *)
    WHILE Globals.nextModuleId <= VarParams.TotalModules DO
      VAR id := Globals.nextModuleId;
      BEGIN
        INC(Globals.nextModuleId);
        moduleH := NEW(Module.T).init(id);
        (* add module to index *)
        EVAL Globals.ModuleIdx.put(Fmt.F("Module %08s", Fmt.Int(id)), moduleH);
        (* add module to extent *)
        Globals.AllModules.addhi(moduleH);

        IF Globals.chain_tx THEN
          tr.chain();
        ELSE
          tr.commit();
          tr.begin();
        END

      END
    END;

    Put("generated all the modules, now generating the comp parts\n");
    (* now generate the composite parts *)

    VAR
      startTime, endTime: Utime.struct_timeval;
    BEGIN
      EVAL Utime.gettimeofday(startTime, tz);
      WHILE Globals.nextCompositeId <= VarParams.TotalCompParts DO
        id := Globals.nextCompositeId;
        INC(Globals.nextCompositeId);
        cp := NEW(CompositePart.T).init(id);

        (* add composite part to its index *)
        EVAL Globals.CompPartIdx.put(id, cp);

        IF Globals.nextCompositeId MOD 25 = 0 THEN
          Put("progress : nextCompositeId=");
          PutInt(Globals.nextCompositeId);
          Put("\n");
          IF Globals.nextCompositeId MOD VarParams.NumCompPerModule = 0 THEN
            EVAL Utime.gettimeofday(endTime, tz);
            Put("Elapsed Time for Local Creation = ");
            Put(Fmt.LongReal(ComputeWallClockTime(startTime, endTime)));
            Put("\n");

            EVAL Utime.gettimeofday(startTime, tz);
            IF Globals.chain_tx THEN
              tr.chain();
            ELSE
              tr.commit();
            END;
            EVAL Utime.gettimeofday(endTime, tz);
            Put("Elapsed Time for Local Commit = ");
            Put(Fmt.LongReal(ComputeWallClockTime(startTime, endTime)));
            Put("\n");

            EVAL Utime.gettimeofday(startTime, tz);
            IF NOT Globals.chain_tx THEN
              tr.begin();
            END;
            Put("intermediate commit done\n");
          END
        END
      END
    END;

    (* print out some useful information about what happened *)

    Put("=== DONE CREATING DATABASE, TOTALS WERE ===");
    Put("\n    # atomic parts\t\t");
    PutInt(Globals.nextAtomicId-1);
    Put("\n    # composite parts\t\t");
    PutInt(Globals.nextCompositeId-1);
    Put("\n    # complex assemblies\t");
    PutInt(Globals.nextComplexAssemblyId-1);
    Put("\n    # base assemblies\t\t");
    PutInt(Globals.nextBaseAssemblyId-1);
    Put("\n    # modules\t\t\t");
    PutInt(Globals.nextModuleId-1);
    Put("\n");

    (*
      //////////////////////////////////////////////////////////////////
      //
      // Shutdown DB
      //
      //////////////////////////////////////////////////////////////////
    *)

    tr.commit();
    db.close();

    (* compute and report wall clock time *)
    EVAL Utime.gettimeofday(endWallTime, tz);
    Put("Wall-Clock time to generate database: ");
    Put(Fmt.LongReal(ComputeWallClockTime(startWallTime, endWallTime)));
    Put(" seconds.\n");

    EVAL Uresource.getrusage(Uresource.RUSAGE_SELF, endUsage);
    Put("(CPU: ");
    Put(Fmt.LongReal(
            ComputeUserTime(startUsage, endUsage) +
            ComputeSystemTime(startUsage, endUsage)));
    Put(" seconds.)\n");
  END gendb;

<*FATAL Database.NotFound*>
<*FATAL Database.Exists*>
<*FATAL Database.Opened*>
<*FATAL Database.Closed*>
<*FATAL Transaction.InProgress*>
<*FATAL Transaction.NotInProgress*>

BEGIN
  IF Params.Count < 2 THEN
    Put("Usage: ", Stdio.stderr);
    Put(Params.Get(0), Stdio.stderr);
    Put(" <configFileName> [-d]\n", Stdio.stderr);
    Process.Exit(1);
  END;
  (* See if debug mode is desired *)
  IF Params.Count = 3 AND Text.Equal(Params.Get(2), "-d") THEN
    Globals.debugMode := TRUE;
  END;

  SetParams.FromFile(Params.Get(1));

  (* get wall clock time *)
  EVAL Utime.gettimeofday(startWallTime, tz);

  (* get starting usage values. *)
  EVAL Uresource.getrusage(Uresource.RUSAGE_SELF, startUsage);

  (* generate the database *)
  ODMG.Create("OO7");
  gendb();
END GenDB.
