INTERFACE BenchParams;

(* parameters for controlling benchmark operations *)
CONST

  UpdateRepeatCnt = 4;			 (* repeat count for repeated       *)
					 (* updates                         *)

  Query1RepeatCnt = 10;			 (* repeat count for query #1       *)
					 (* lookus                          *)

  Query2Percent = 1;			 (* selected % for query #2 lookups *)

  Query3Percent = 10;			 (* selected % for query #3 lookups *)

  Query4RepeatCnt = 10;			 (* repeat count for query #4       *)
					 (* lookups                         *)

  NumNewCompParts = 10;			 (* # new composite parts for       *)
					 (* updates                         *)

  BaseAssmUpdateCnt = 10;		 (* # base assemblies used in       *)
					 (* updates                         *)

  MultiBenchMPL = 4;			 (* # multiuser benchmark processes *)

END BenchParams.
