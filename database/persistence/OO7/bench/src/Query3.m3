MODULE Query3 EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Query #3 - 10% selection on AtomicParts via build date (the
  // most recent 10% of AtomicParts.)   The actual percentage
  // is determined by the constant Query3Percent.  This may
  // be varied as we gain experience with the benchmark - the
  // goal is to check out if the "optimizer" is smart enough
  // to properly choose between B+ tree and scan. 
  //
  /////////////////////////////////////////////////////////////////
*)

IMPORT Globals, AtomicPart, RefBag;
FROM IO IMPORT Put, PutInt;
FROM BenchParams IMPORT Query3Percent;
FROM GenParams IMPORT MinAtomicDate, MaxAtomicDate;

PROCEDURE query3(): INTEGER =
  VAR
    ap: AtomicPart.T;
    (* choose a range of dates with the appropriate selectivity *)
    dateRange := (Query3Percent * (MaxAtomicDate - MinAtomicDate)) DIV 100;
    lowerDate := MaxAtomicDate - dateRange;
    count := 0;
  BEGIN
    IF Globals.debugMode THEN
      Put("    Running Query3 with dateRange = "); PutInt(dateRange);
      Put(", lowerDate = "); PutInt(lowerDate); Put("\n");
    END;
    
    (* iterate over all atomic parts in the design library (since this
       release of Objectivity/DB does not include B+ trees) looking for
       atomic parts that were built recently enough to be of interest *)
    VAR
      DateRetrieve := Globals.BuildDateIdx.iterateOrdered();
      bag: RefBag.T;
      date: INTEGER;
    BEGIN
      DateRetrieve.seek(lowerDate);
      (* upper range is end of index? *)
      WHILE DateRetrieve.next(date, bag) AND date <= MaxAtomicDate DO
        VAR partI := bag.iterate(); ref: REFANY;
        BEGIN
          WHILE partI.next(ref) DO
            ap := ref;
            IF Globals.debugMode THEN
              Put("    In Query3, partId = "); PutInt(ap.id);
              Put(", buildDate = "); PutInt(ap.buildDate); Put("\n");
            END;

            (* process qualifying parts by calling the null procedure *)
            IF ap.buildDate >= lowerDate THEN
              ap.doNothing();
              INC(count);
            ELSE (* shouldn't happen *)
              Put("error:    In Query3, partId = "); PutInt(ap.id);
              Put(", buildDate = "); PutInt(ap.buildDate);
              Put(", indexval "); PutInt(date);
              Put(" lowerDate "); PutInt(lowerDate); Put("\n");
            END
          END
        END
      END
    END;
    RETURN count;
  END query3;
  
BEGIN
END Query3.
