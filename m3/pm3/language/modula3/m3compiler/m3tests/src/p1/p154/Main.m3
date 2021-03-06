(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 

(* simple SCANF tests on RdV *)

MODULE Main;

IMPORT Test, TextRd (*RdV*);

PROCEDURE DoIt () =
  VAR i1, i2: INTEGER;
    rd := TextRd.New(*RdV.FromText*) (" 31 \n 42 53 64 Hello 75");
  BEGIN
    SCANF(rd, "%d", i1);
    Test.checkI (i1, 31);
    SCANF(rd, "%d %d", i1, i2);
    Test.checkI (i1, 42);
    Test.checkI (i2, 53);
    TRY
      SCANF( rd,  "%d%d", i1, i2 );
      Test.check (FALSE);
    EXCEPT RdV.ScanFailed =>
      Test.checkI (i1, 64);
    END;
  END DoIt;

BEGIN
  DoIt();
END Main.
