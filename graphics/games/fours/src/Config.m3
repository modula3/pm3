(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jun 14 08:21:47 PDT 1995 by kalsow     *)

MODULE Config;

PROCEDURE New (nSquares: [2..5]): T =
  VAR t := NEW (T);
  BEGIN
    CASE nSquares OF
    | 2 => InitTwos (t);
    | 3 => InitThrees (t);
    | 4 => InitFours (t);
    | 5 => InitFives (t);
    END;
    RETURN t;
  END New;

PROCEDURE InitTwos (t: T) =
  CONST NTiles = 2;  MaxSpan = 3;
  BEGIN
    t.name    := "Twos";
    t.nRows   := 18;
    t.nCols   := 9;
    t.nPieces := 6;
    t.nTiles  := NTiles;
    t.delay   := 0.08d0;
    t.pieces  := NEW (PieceList, t.nPieces);

    (* build the piece descriptions *)
    WITH z = t.pieces[0] DO
      BuildRotation (z[0], NTiles, MaxSpan, 0,  0, 110);
      BuildRotation (z[1], NTiles, MaxSpan, 0,  0, 100,100);
      z [2] := z [0];
      z [3] := z [1];
    END;

    WITH z = t.pieces[1] DO
      BuildRotation (z[0], NTiles, MaxSpan, 0, -1, 100,000,100);
      BuildRotation (z[1], NTiles, MaxSpan, -1, 0, 101);
      z [2] := z [0];
      z [3] := z [1];
    END;

    WITH z = t.pieces[2] DO
      BuildRotation (z[0], NTiles, MaxSpan, 0,  0, 100,010);
      BuildRotation (z[1], NTiles, MaxSpan, 0,  0, 010,100);
      z [2] := z [0];
      z [3] := z [1];
    END;

    WITH z = t.pieces[3] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 100,001, 0);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 010,000,100);
      z [2] := z [0];
      z [3] := z [1];
    END;

    WITH z = t.pieces[4] DO
      BuildRotation (z[0], NTiles, MaxSpan,  0, -1, 100,000,010);
      BuildRotation (z[1], NTiles, MaxSpan, -1,  0, 001,100,0);
      z [2] := z [0];
      z [3] := z [1];
    END;

    WITH z = t.pieces[5] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1, -1, 100,000,001);
      BuildRotation (z[1], NTiles, MaxSpan, -1, -1, 001,000,100);
      z [2] := z [0];
      z [3] := z [1];
    END;
  END InitTwos;

PROCEDURE InitThrees (t: T) =
  CONST NTiles = 3;  MaxSpan = 3;
  BEGIN
    t.name    := "Threes";
    t.nRows   := 18;
    t.nCols   := 9;
    t.nPieces := 2;
    t.nTiles  := NTiles;
    t.delay   := 0.08d0;
    t.pieces  := NEW (PieceList, 2);

    (* build the piece descriptions *)
    WITH z = t.pieces[0] DO
      BuildRotation (z[0], NTiles, MaxSpan,  0,  0, 110,100);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 100,110);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 010,110);
      BuildRotation (z[3], NTiles, MaxSpan, -1,  0, 110,010);
    END;

    WITH z = t.pieces[1] DO
      BuildRotation (z[0], NTiles, MaxSpan,  0, -1, 100,100,100);
      BuildRotation (z[1], NTiles, MaxSpan, -1,  0, 111);
      z [2] := z [0];
      z [3] := z [1];
    END;

  END InitThrees;

PROCEDURE InitFours (t: T) =
  CONST NTiles = 4;  MaxSpan = 4;
  BEGIN
    t.name    := "Fours";
    t.nRows   := 20;
    t.nCols   := 10;
    t.nPieces := 7;
    t.nTiles  := NTiles;
    t.delay   := 0.12d0;
    t.pieces  := NEW (PieceList, 7);

    (* build the piece descriptions *)
    WITH z = t.pieces[0] DO
      BuildRotation (z[0], NTiles, MaxSpan, 0, 0, 1100,1100);
      z [1] := z [0];
      z [2] := z [0];
      z [3] := z [0];
    END;

    WITH z = t.pieces[1] DO
      BuildRotation (z[0], NTiles, MaxSpan,  0, -2, 1000,1000,1000,1000);
      BuildRotation (z[1], NTiles, MaxSpan, -2,  0, 1111);
      z [2] := z [0];
      z [3] := z [1];
    END;

    WITH z = t.pieces[2] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 1110,0100);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 1000,1100,1000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 0100,1110);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 0100,1100,0100);
    END;

    WITH z = t.pieces[3] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 1110,1000);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 1000,1000,1100);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 0010,1110);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 1100,0100,0100);
    END;

    WITH z = t.pieces[4] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 1110,0010);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 1100,1000,1000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 1000,1110);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 0100,0100,1100);
    END;

    WITH z = t.pieces[5] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 0110,1100);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 1000,1100,0100);
      z [2] := z[0];
      z [3] := z[1];
    END;

    WITH z = t.pieces[6] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 1100,0110);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 0100,1100,1000);
      z [2] := z [0];
      z [3] := z [1];
    END;
  END InitFours;

PROCEDURE InitFives (t: T) =
  CONST NTiles = 5;  MaxSpan = 5;
  BEGIN
    t.name    := "Fives";
    t.nRows   := 24;
    t.nCols   := 12;
    t.nPieces := 18;
    t.nTiles  := NTiles;
    t.delay   := 0.12d0;
    t.pieces  := NEW (PieceList, 18);

    (* build the piece descriptions *)
    WITH z = t.pieces[0] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1, -1, 10000,11000,01100);
      BuildRotation (z[1], NTiles, MaxSpan, -1, -1, 00100,01100,11000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 11000,01100,00100);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 01100,11000,10000);
    END;

    WITH z = t.pieces[1] DO
      BuildRotation (z[0], NTiles, MaxSpan, -2, -2, 00100,00100,00100,00100,00100);
      BuildRotation (z[1], NTiles, MaxSpan, -2,  0, 11111);
      z [2] := z [0];
      z [3] := z [1];
    END;

    WITH z = t.pieces[2] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 11110,10000);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -2, 10000,10000,10000,11000);
      BuildRotation (z[2], NTiles, MaxSpan, -2, -1, 00010,11110);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 11000,01000,01000,01000);
    END;

    WITH z = t.pieces[3] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 11110,01000);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -2, 10000,10000,11000,10000);
      BuildRotation (z[2], NTiles, MaxSpan, -2, -1, 00100,11110);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 01000,11000,01000,01000);
    END;

    WITH z = t.pieces[4] DO
      BuildRotation (z[0], NTiles, MaxSpan, -2,  0, 11110,00100);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 10000,11000,10000,10000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 01000,11110);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -2, 01000,01000,11000,01000);
    END;

    WITH z = t.pieces[5] DO
      BuildRotation (z[0], NTiles, MaxSpan, -2,  0, 11110,00010);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 11000,10000,10000,10000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 10000,11110);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -2, 01000,01000,01000,11000);
    END;

    WITH z = t.pieces[6] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 11100,11000);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 10000,11000,11000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 01100,11100);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 11000,11000,01000);
    END;

    WITH z = t.pieces[7] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 11100,01100);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 11000,11000,10000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 11000,11100);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 01000,11000,11000);
    END;

    WITH z = t.pieces[8] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 01110,11000);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -2, 10000,10000,11000,01000);
      BuildRotation (z[2], NTiles, MaxSpan, -2, -1, 00110,11100);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 10000,11000,01000,01000);
    END;

    WITH z = t.pieces[9] DO
      BuildRotation (z[0], NTiles, MaxSpan, -2,  0, 11100,00110);
      BuildRotation (z[1], NTiles, MaxSpan,  0,  0, 01000,11000,10000,10000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 11000,01110);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -2, 01000,01000,11000,10000);
    END;

    WITH z = t.pieces[10] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1, -1, 11100,10000,10000);
      BuildRotation (z[1], NTiles, MaxSpan, -1, -1, 10000,10000,11100);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 00100,00100,11100);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 11100,00100,00100);
    END;

    WITH z = t.pieces[11] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 11100,01000,01000);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 10000,11100,10000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -2, 01000,01000,11100);
      BuildRotation (z[3], NTiles, MaxSpan, -2, -1, 00100,11100,00100);
    END;

    WITH z = t.pieces[12] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1,  0, 11100,10100);
      BuildRotation (z[1], NTiles, MaxSpan,  0, -1, 11000,10000,11000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 10100,11100);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 11000,01000,11000);
    END;

    WITH z = t.pieces[13] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1, -1, 00100,11100,10000);
      BuildRotation (z[1], NTiles, MaxSpan, -1, -1, 11000,01000,01100);
      z [2] := z [0];
      z [3] := z [1];
    END;

    WITH z = t.pieces[14] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1, -1, 01000,11100,10000);
      BuildRotation (z[1], NTiles, MaxSpan, -1, -1, 01000,11000,01100);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 00100,11100,01000);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 11000,01100,01000);
    END;

    WITH z = t.pieces[15] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1, -1, 01000,11100,01000);
      z [1] := z [0];
      z [2] := z [0];
      z [3] := z [0];
    END;

    WITH z = t.pieces[16] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1, -1, 10000,11100,01000);
      BuildRotation (z[1], NTiles, MaxSpan, -1, -1, 01000,01100,11000);
      BuildRotation (z[2], NTiles, MaxSpan, -1, -1, 01000,11100,00100);
      BuildRotation (z[3], NTiles, MaxSpan, -1, -1, 01100,11000,01000);
    END;

    WITH z = t.pieces[17] DO
      BuildRotation (z[0], NTiles, MaxSpan, -1, -1, 10000,11100,00100);
      BuildRotation (z[1], NTiles, MaxSpan, -1, -1, 01100,01000,11000);
      z [2] := z [0];
      z [3] := z [1];
    END;
  END InitFives;
 
PROCEDURE BuildRotation (VAR r               : RotatedPieceMap;
                             nTiles, maxSpan : INTEGER;
                             hmin, vmin      : INTEGER;
                             x0,x1,x2,x3,x4  : INTEGER := 0) =
  VAR cnt := 0;  lines: ARRAY [0..4] OF INTEGER;
  BEGIN
    lines[0] := x0;
    lines[1] := x1;
    lines[2] := x2;
    lines[3] := x3;
    lines[4] := x4;

    r.voffset := 99999;
    r.hoffset := 99999;
    r.tiles   := NEW (TileList, nTiles);

    FOR x := FIRST (lines) TO LAST (lines) DO
      BuildRow (r, maxSpan, hmin, vmin + x, lines[x], cnt);
    END;
    <* ASSERT cnt = nTiles *>
  END BuildRotation;

PROCEDURE BuildRow (VAR r          : RotatedPieceMap;
                        maxSpan    : INTEGER;
                        hmin, vmin : INTEGER;
                        line       : INTEGER;
                    VAR n          : INTEGER) =
  BEGIN
    FOR i := maxSpan-1 TO 0 BY -1 DO
      IF ((line MOD 10) # 0) THEN
        r.tiles[n].h := hmin + i;
	r.tiles[n].v := vmin;
	r.voffset  := MIN (r.voffset, vmin);
	r.hoffset  := MIN (r.hoffset, hmin + i);
	INC (n);
      END;
      line := line DIV 10;
    END;
  END BuildRow;

BEGIN
END Config.
