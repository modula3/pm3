MODULE Query2 EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Query #2 - 1% selection on AtomicParts via build date (the
  // most recent 1% of AtomicParts.)  A B+ tree index is used
  //
  /////////////////////////////////////////////////////////////////
*)

IMPORT Globals, AtomicPart, RefBag;
FROM IO IMPORT Put, PutInt;
FROM BenchParams IMPORT Query2Percent;
FROM GenParams IMPORT MinAtomicDate, MaxAtomicDate;

PROCEDURE query2(): INTEGER =
  VAR
    ap: AtomicPart.T;
    (* choose a range of dates with the appropriate selectivity *)
    dateRange := (Query2Percent * (MaxAtomicDate - MinAtomicDate)) DIV 100;
    lowerDate := MaxAtomicDate - dateRange;
    count := 0;
  BEGIN
    IF Globals.debugMode THEN
      Put("    Running Query2 with dateRange = "); PutInt(dateRange);
      Put(", lowerDate = "); PutInt(lowerDate); Put("\n");
    END;

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
              Put("    In Query2, partId = "); PutInt(ap.id);
              Put(", buildDate = "); PutInt(ap.buildDate); Put("\n");
            END;

            (*  process qualifying parts by calling the null procedure *)
            IF ap.buildDate >= lowerDate THEN
              ap.doNothing();
              INC(count);
            ELSE (* shouldn't happen *)
              Put("error:    In Query2, partId = "); PutInt(ap.id);
              Put(", buildDate = "); PutInt(ap.buildDate);
              Put(", indexval "); PutInt(date);
              Put(" lowerDate "); PutInt(lowerDate); Put("\n");
            END
          END
        END
      END
    END;
    RETURN count;
  END query2;
  
BEGIN
END Query2.
