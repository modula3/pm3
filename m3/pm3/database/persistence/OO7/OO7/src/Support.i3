INTERFACE Support;

IMPORT Utime, Uresource, OO7;

PROCEDURE JoinDoNothing(id1, id2: INTEGER);

PROCEDURE ComputeWallClockTime(READONLY startWallTime: Utime.struct_timeval; 
                               READONLY endWallTime: Utime.struct_timeval):
  LONGREAL;

PROCEDURE ComputeUserTime(READONLY startUsage: Uresource.struct_rusage; 
                          READONLY endUsage: Uresource.struct_rusage):
  LONGREAL;

PROCEDURE ComputeSystemTime(READONLY startUsage: Uresource.struct_rusage; 
                            READONLY endUsage: Uresource.struct_rusage):
  LONGREAL;

PROCEDURE PrintOp(op: OO7.BenchmarkOp);

END Support.
