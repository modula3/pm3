MODULE Support;

IMPORT Utime, Uresource, OO7, Globals;
FROM IO IMPORT Put, PutInt;

PROCEDURE JoinDoNothing (id1, id2: INTEGER) =
  BEGIN
    IF Globals.debugMode THEN
      Put("==> JoinDoNothing(id1 = "); PutInt(id1);
      Put(", id2 = "); PutInt(id2); Put(".)\n");
    END
  END JoinDoNothing;

PROCEDURE ComputeWallClockTime(READONLY start, end: Utime.struct_timeval):
  LONGREAL =
  VAR
    seconds := FLOAT(end.tv_sec - start.tv_sec, LONGREAL);
    useconds := FLOAT(end.tv_usec - start.tv_usec, LONGREAL);
  BEGIN
    IF useconds < 0.0D0 THEN
      useconds := 1.0D6 + useconds;
      seconds := seconds - 1.0D0;
    END;
    RETURN seconds + useconds / 1.0D6;
  END ComputeWallClockTime;

PROCEDURE ComputeUserTime(READONLY start, end: Uresource.struct_rusage):
  LONGREAL =
  VAR
    seconds := FLOAT(end.ru_utime.tv_sec - start.ru_utime.tv_sec, LONGREAL);
    useconds := FLOAT(end.ru_utime.tv_usec - start.ru_utime.tv_usec, LONGREAL);
  BEGIN
    IF useconds < 0.0D0 THEN
      useconds := 1.0D6 + useconds;
      seconds := seconds - 1.0D0;
    END;
    RETURN seconds + useconds / 1.0D6;
  END ComputeUserTime;

PROCEDURE ComputeSystemTime(READONLY start, end: Uresource.struct_rusage):
  LONGREAL =
  VAR
    seconds := FLOAT(end.ru_stime.tv_sec - start.ru_stime.tv_sec, LONGREAL);
    useconds := FLOAT(end.ru_stime.tv_usec - start.ru_stime.tv_usec, LONGREAL);
  BEGIN
    IF useconds < 0.0D0 THEN
      useconds := 1.0D6 + useconds;
      seconds := seconds - 1.0D0;
    END;
    RETURN seconds + useconds / 1.0D6;
  END ComputeSystemTime;

PROCEDURE PrintOp(op: OO7.BenchmarkOp) =
  BEGIN
    CASE op OF
    | OO7.BenchmarkOp.Trav1      => Put("Trav1");
    | OO7.BenchmarkOp.Trav1WW    => Put("Trav1WW");
    | OO7.BenchmarkOp.Trav2a     => Put("Trav2a");
    | OO7.BenchmarkOp.Trav2b     => Put("Trav2b");
    | OO7.BenchmarkOp.Trav2c     => Put("Trav2c");
    | OO7.BenchmarkOp.Trav3a     => Put("Trav3a");
    | OO7.BenchmarkOp.Trav3b     => Put("Trav3b");
    | OO7.BenchmarkOp.Trav3c     => Put("Trav3c");
    | OO7.BenchmarkOp.Trav4      => Put("Trav4");
    | OO7.BenchmarkOp.Trav5do    => Put("Trav5do");
    | OO7.BenchmarkOp.Trav5undo  => Put("Trav5undo");
    | OO7.BenchmarkOp.Trav6      => Put("Trav6");
    | OO7.BenchmarkOp.Trav7      => Put("Trav7");
    | OO7.BenchmarkOp.Trav8      => Put("Trav8");
    | OO7.BenchmarkOp.Trav9      => Put("Trav9");
    | OO7.BenchmarkOp.Trav10     => Put("Trav10");
    | OO7.BenchmarkOp.Query1     => Put("Query1");
    | OO7.BenchmarkOp.Query2     => Put("Query2");
    | OO7.BenchmarkOp.Query3     => Put("Query3");
    | OO7.BenchmarkOp.Query4     => Put("Query4");
    | OO7.BenchmarkOp.Query5     => Put("Query5");
    | OO7.BenchmarkOp.Query6     => Put("Query6");
    | OO7.BenchmarkOp.Query7     => Put("Query7");
    | OO7.BenchmarkOp.Query8     => Put("Query8");
    | OO7.BenchmarkOp.Insert     => Put("Insert");
    | OO7.BenchmarkOp.Delete     => Put("Delete");
    | OO7.BenchmarkOp.Reorg1     => Put("Reorg1");
    | OO7.BenchmarkOp.Reorg2     => Put("Reorg2");
    | OO7.BenchmarkOp.WarmUpdate => Put("WarmUpdate");
    | OO7.BenchmarkOp.MultiTrav1 => Put("MultiTrav1");
    | OO7.BenchmarkOp.MultiTrav2 => Put("MultiTrav2");
    | OO7.BenchmarkOp.MultiTrav3 => Put("MultiTrav3");
    | OO7.BenchmarkOp.MultiTrav4 => Put("MultiTrav4");
    | OO7.BenchmarkOp.MultiTrav5 => Put("MultiTrav5");
    | OO7.BenchmarkOp.MultiTrav6 => Put("MultiTrav6");
    END
  END PrintOp;

BEGIN
END Support.
