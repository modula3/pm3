MODULE Query1 EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Query #1 - randomly choose "Query1RepeatCnt" atomic parts by 
  // lookup on their id field.  An index can be used for the
  // actual lookup.  
  //
  /////////////////////////////////////////////////////////////////
*)

IMPORT CRandom, VarParams, Globals, Stdio, AtomicPart;
FROM IO IMPORT Put, PutInt;
FROM BenchParams IMPORT Query1RepeatCnt;

PROCEDURE query1(): INTEGER =
  VAR atomH: AtomicPart.T;
  BEGIN
    (* set random seed so "hot" runs are truly hot *)
    CRandom.srandom(1);

    (* now randomly select parts via partId index and process them *)
    FOR i := 1 TO Query1RepeatCnt DO
      (* generate part id and lookup part *)
      VAR
        partId := (CRandom.random() MOD VarParams.TotalAtomicParts) + 1;
        ref: REFANY;
      BEGIN
        IF Globals.debugMode THEN
          Put("    In Query1, partId = "); PutInt(partId); Put(":\n");
        END;
        IF Globals.AtomicPartIdx.get(partId, ref) THEN
          atomH := ref;
          (* process part by calling the null procedure *)
          atomH.doNothing();
        ELSE
          Put("ERROR: Unable to find atomic part ", Stdio.stderr);
          PutInt(partId, Stdio.stderr);
          Put("\n", Stdio.stderr);
        END
      END
    END;
    RETURN Query1RepeatCnt;
  END query1;

BEGIN
END Query1.
