MODULE Query4 EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Query #4 - randomly choose "Query4RepeatCnt" document objects by 
  // lookup on their title field.  An index can be used for the
  // actual lookup.  
  //
  /////////////////////////////////////////////////////////////////
*)

IMPORT CRandom, VarParams, Globals, Fmt, Stdio, Process, BaseAssembly,
       Document;
FROM IO IMPORT Put;
FROM BenchParams IMPORT Query4RepeatCnt;
FROM VarParams IMPORT TotalCompParts;

PROCEDURE query4(): INTEGER =
  VAR
    docH: Document.T;
    ba: BaseAssembly.T;
    count := 0;
  BEGIN
    (* now randomly select a document via its title index
       and trace a path to the base assemblies that use
       its associated composite part (and then
       repeat this process the desired number of times) *)

    (* set random seed so "hot" runs are truly hot *)
    CRandom.srandom(1);

    FOR i := 1 TO Query4RepeatCnt DO
      (* generate random document title and lookup document *)
      VAR
        compPartId := (CRandom.random() MOD TotalCompParts) + 1;
        title := Fmt.F("Composite Part %08s", Fmt.Int(compPartId));
        ref: REFANY;
      BEGIN
        IF NOT Globals.DocumentIdx.get(title, ref) THEN
          Put("ERROR: Unable to find document called \"", Stdio.stderr);
          Put(title, Stdio.stderr); Put("\"\n", Stdio.stderr);
          Process.Exit(1);
        END;
        docH := ref;

        IF Globals.debugMode THEN
          Put("   In Query4, document title = \""); Put(title); Put("\"\n");
        END;

        (* now walk back up the path(s) to the associated base assemblies
           (based on private uses of the composite part, at least for now) *)
        VAR baseI := docH.part.usedInPriv;
        BEGIN
          WHILE baseI # NIL DO
            ba := baseI.head; baseI := baseI.tail;
            ba.doNothing();
            INC(count);
          END
        END
      END;
    END;
    RETURN count;
  END query4;
    
BEGIN
END Query4.
