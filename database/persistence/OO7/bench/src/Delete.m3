MODULE Delete EXPORTS Bench;

IMPORT Globals, CompositePart;
FROM BenchParams IMPORT NumNewCompParts;
FROM IO IMPORT Put, PutInt;
FROM Stdio IMPORT stderr;

(*
  //////////////////////////////////////////////////////////////////
  //
  // Delete - Delete NumNewCompParts randomly chosen composite parts. 
  //
  //////////////////////////////////////////////////////////////////
*)

PROCEDURE delete1() =
  VAR
    compId: INTEGER;
    compH, compH2: CompositePart.T;
  BEGIN
    FOR i := 1 TO NumNewCompParts DO
      compId := Globals.nextCompositeId;
      INC(Globals.nextCompositeId);

      IF Globals.debugMode THEN
        Put("In Delete, deleting composite part ");
        PutInt(compId);
        Put("\n");
      END;

      VAR ref: REFANY;
      BEGIN
        IF Globals.CompPartIdx,get(compId, ref) THEN
          compH := ref;
          IF Globals.CompPartIdx.delete(compId, ref) THEN
            compH2 := ref;
            IF compH2 != compH THEN
              Put("index error in deleting composite part from its idx",
                  stderr);
            END;
          END;
          compH.delete();
        ELSE
          Put("ERROR: Unable to locate composite part ", stderr);
          PutInt(compId, stderr);
          Put("\n", stderr);
        END
      END
    END
  END delete1;

BEGIN
END Delete.
