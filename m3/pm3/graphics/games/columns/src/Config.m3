(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Tue Feb 11 17:40:41 PST 1992 by muller         *)
(*      modified on Fri Dec 22 15:14:18 1989 by kalsow         *)

MODULE Config;

PROCEDURE New3 (x0,y0, x1,y1, x2,y2: INTEGER): Piece =
  VAR p := NEW (Piece, 3);
  BEGIN
    WITH a = p[0] DO  a.h := x0;  a.v := y0  END;
    WITH a = p[1] DO  a.h := x1;  a.v := y1  END;
    WITH a = p[2] DO  a.h := x2;  a.v := y2  END;
    RETURN p;
  END New3;

PROCEDURE New4 (x0,y0, x1,y1, x2,y2, x3, y3: INTEGER): Piece =
  VAR p := NEW (Piece, 4);
  BEGIN
    WITH a = p[0] DO  a.h := x0;  a.v := y0  END;
    WITH a = p[1] DO  a.h := x1;  a.v := y1  END;
    WITH a = p[2] DO  a.h := x2;  a.v := y2  END;
    WITH a = p[3] DO  a.h := x3;  a.v := y3  END;
    RETURN p;
  END New4;

BEGIN
END Config.
