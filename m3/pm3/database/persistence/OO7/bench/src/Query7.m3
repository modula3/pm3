MODULE Query7 EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Query #7 --- iterate through all atomic parts.  Checks scan
  // speed.  Some duplication with other queries, especially in
  // systems that lack indices and force the other queries to be
  // done BYTESIZE iterating through sets.
  //
  //////////////////////////////////////////////////////////////////
*)

IMPORT  Globals, AtomicPart;
FROM IO IMPORT Put, PutInt;

PROCEDURE query7(): INTEGER =
  VAR
    ap: AtomicPart.T;
    count := 0;
    AtomScan := Globals.AtomicPartIdx.iterate(); ref: REFANY;
    id: INTEGER;
  BEGIN
    WHILE AtomScan.next(id, ref) DO
      ap := ref;
      IF Globals.debugMode THEN
        Put("    In Query7, partId = "); PutInt(ap.id); Put(":\n");
      END;
      ap.doNothing();
      INC(count);
    END;
    RETURN count;
  END query7;

BEGIN
END Query7.
