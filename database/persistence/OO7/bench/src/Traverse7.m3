MODULE Traverse7 EXPORTS Bench;

(*
  /////////////////////////////////////////////////////////////////
  //
  // Traversal #7.  Pick a random atomic part; traverse from the 
  // part to its containing composite part P.  From P, traverse
  // backwards up to all base assemblies referencing P; for each
  // such base assembly, traverse upward through the assembly
  // hierarchy until the root is reached.  This traversal coupled
  // with traversal #1 ensures
  // that all pointers are bi-directional.
  //
  /////////////////////////////////////////////////////////////////
*)

IMPORT CRandom, Globals, Stdio, Process, AtomicPart;
FROM VarParams IMPORT TotalAtomicParts;
FROM IO IMPORT Put, PutInt;

PROCEDURE traverse7 (): INTEGER =
  VAR
    partId: INTEGER;
    atomH: AtomicPart.T;
  BEGIN
    (* get access to the design library containing atomic parts *)
       
    (* prepare to select a random atomic part via the id index
       now randomly select a part and traverse upwards from there *)
       
    (* set random seed so "hot" runs are truly hot *)
    CRandom.srandom(1);

    partId := (CRandom.random() MOD TotalAtomicParts) + 1;
    IF Globals.debugMode THEN
      Put("  In Traverse7, starting at atomic part "); PutInt(partId);
      Put("\n");
    END;
    VAR ref: REFANY;
    BEGIN
      IF NOT Globals.AtomicPartIdx.get(partId, ref) THEN
        Put("ERROR: Unable to find atomic part ", Stdio.stderr);
        PutInt(partId, Stdio.stderr);
        Put("\n", Stdio.stderr);
        Process.Exit(1);
      END;
      atomH := ref;
    END;
    RETURN atomH.partOf.traverse7();
  END traverse7;

BEGIN
END Traverse7.
