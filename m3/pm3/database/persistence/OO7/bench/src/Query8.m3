MODULE Query8 EXPORTS Bench;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Query #8 --- value join between Documents and AtomicParts
  // on Document.id and AtomicPart.docId.  This is a key - foreign
  // key join.  Algorithm is nested loops with index on document id field
  //
  // The main drawback is that we have no semantics whatsoever for
  // this query.
  //
  //////////////////////////////////////////////////////////////////
*)

IMPORT Globals, AtomicPart, Document;
FROM IO IMPORT Put, PutInt;
FROM Support IMPORT JoinDoNothing;

PROCEDURE query8(): INTEGER =
  VAR
    ap: AtomicPart.T;
    doc: Document.T;
    id: INTEGER;
    count := 0;
    AtomScan := Globals.AtomicPartIdx.iterate();
    ref: REFANY;
  BEGIN
    WHILE AtomScan.next(id, ref) DO
      ap := ref;
      IF Globals.DocumentIdIdx.get(ap.docId, ref) THEN
        doc := ref;
        (* found a matching pair of tuples *)
        IF Globals.debugMode THEN
          Put("In Query8, atomic part "); PutInt(ap.id);
          Put(" matches doc "); PutInt(doc.id); Put(".\n");
        END;            
        JoinDoNothing(ap.id, doc.id);
        INC(count);
      END
    END;
    RETURN count;
  END query8;

BEGIN
END Query8.
