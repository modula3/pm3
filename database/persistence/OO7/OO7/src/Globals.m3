MODULE Globals;

IMPORT Text, Fmt, GenParams, VarParams, ODMG, Transaction, Stdio, RefSeq,
       IntRefBPlusTree, TextRefBPlusTree, IntRefBagBPlusTree, Utime;
FROM IO IMPORT Put;

PROCEDURE InitGlobals (db: ODMG.T) RAISES { Error } =
  BEGIN
    TRY

      TRY
        AtomicPartIdx   := db.lookup("AtomicPartIdx");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        AtomicPartIdx   := NEW(IntRefBPlusTree.T).init();
        db.bind(AtomicPartIdx, "AtomicPartIdx");
      END;

      TRY
        CompPartIdx     := db.lookup("CompPartIdx");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        CompPartIdx     := NEW(IntRefBPlusTree.T).init();
        db.bind(CompPartIdx, "CompPartIdx");
      END;

      TRY
        DocumentIdx     := db.lookup("DocumentIdx");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        DocumentIdx     := NEW(TextRefBPlusTree.T).init();
        db.bind(DocumentIdx, "DocumentIdx");
      END;

      TRY
        DocumentIdIdx   := db.lookup("DocumentIdIdx");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        DocumentIdIdx   := NEW(IntRefBPlusTree.T).init();
        db.bind(DocumentIdIdx, "DocumentIdIdx");
      END;

      TRY
        BaseAssemblyIdx := db.lookup("BaseAssemblyIdx");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        BaseAssemblyIdx := NEW(IntRefBPlusTree.T).init();
        db.bind(BaseAssemblyIdx, "BaseAssemblyIdx");
      END;

      TRY
        ModuleIdx       := db.lookup("ModuleIdx");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        ModuleIdx       := NEW(TextRefBPlusTree.T).init();
        db.bind(ModuleIdx, "ModuleIdx");
      END;

      TRY
        BuildDateIdx  := db.lookup("BuildDateIdx");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        BuildDateIdx  := NEW(IntRefBagBPlusTree.T).init();
        db.bind(BuildDateIdx, "BuildDateIdx");
      END;

      TRY
        AllModules      := db.lookup("AllModules");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        AllModules      := NEW(RefSeq.T).init(VarParams.TotalModules);
        db.bind(AllModules, "AllModules");
      END;

      TRY
        ClientsReady    := db.lookup("ClientsReady");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        ClientsReady    := NEW(REF INTEGER);
        ClientsReady^   := 0;
        db.bind(ClientsReady, "ClientsReady");
      END;

      TRY
        ClientsDone     := db.lookup("ClientsDone");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        ClientsDone     := NEW(REF INTEGER);
        ClientsDone^    := 0;
        db.bind(ClientsDone, "ClientsDone");
      END;


      TRY
        TotalAborts    := db.lookup("TotalAborts");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        TotalAborts    := NEW(REF INTEGER);
        db.bind(TotalAborts, "TotalAborts");
      END;

      TRY
        StartUser     := db.lookup("StartUser");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        StartUser     := NEW(REF Utime.struct_timeval);
        db.bind(StartUser, "StartUser");
      END;

      TRY
        StartSystem   := db.lookup("StartSystem");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        StartSystem   := NEW(REF Utime.struct_timeval);
        db.bind(StartSystem, "StartSystem");
      END;

      TRY
        StartWallTime     := db.lookup("StartWallTime");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        StartWallTime     := NEW(REF Utime.struct_timeval);
        db.bind(StartWallTime, "StartWallTime");
      END;

      TRY
        EndUser       := db.lookup("EndUser");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        EndUser       := NEW(REF Utime.struct_timeval);
        db.bind(EndUser, "EndUser");
      END;

      TRY
        EndSystem     := db.lookup("EndSystem");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        EndSystem     := NEW(REF Utime.struct_timeval);
        db.bind(EndSystem, "EndSystem");
      END;

      TRY
        EndWallTime     := db.lookup("EndWallTime");
      EXCEPT
      | ODMG.ObjectNameNotFound =>
        EndWallTime     := NEW(REF Utime.struct_timeval);
        db.bind(EndWallTime, "EndWallTime");
      END;

    EXCEPT
    | ODMG.ObjectNameNotUnique =>
      Put("Object name not unique.\n", Stdio.stderr);
      RAISE Error;
    | Transaction.NotInProgress =>
      Put("Transaction not in progress.\n", Stdio.stderr);
      RAISE Error;
    END;
  END InitGlobals;


PROCEDURE ResetTimes()=
  BEGIN
    TotalAborts^          := 0;
    StartUser^            := Utime.struct_timeval{tv_sec := 0, tv_usec := 0};
    StartSystem^          := Utime.struct_timeval{tv_sec := 0, tv_usec := 0};
    StartWallTime^        := Utime.struct_timeval{tv_sec := 0, tv_usec := 0};
    EndUser^              := Utime.struct_timeval{tv_sec := 0, tv_usec := 0};
    EndSystem^            := Utime.struct_timeval{tv_sec := 0, tv_usec := 0};
    EndWallTime^          := Utime.struct_timeval{tv_sec := 0, tv_usec := 0};
  END ResetTimes;

BEGIN
  FOR i := 0 TO GenParams.NumTypes - 1 DO
    Text.SetChars(types[i], Fmt.F("type%03s", Fmt.Int(i)));
  END
END Globals.
