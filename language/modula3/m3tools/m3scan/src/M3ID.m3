(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3ID.m3                                               *)
(* Last modified on Fri May 28 09:52:06 PDT 1993 by kalsow     *)

UNSAFE MODULE M3ID;

IMPORT M3Token;
TYPE TK = M3Token.T;

(* Note: the module is unsafe because we use "p^[i]" instead
   of "x[i]", but since "p^" is a fixed length array we avoid
   the runtime subscript checks. *)

PROCEDURE Classify (READONLY x: ARRAY OF CHAR): M3Token.T =
  VAR p : UNTRACED REF ARRAY [0..8] OF CHAR := ADR (x[0]);
  VAR n := NUMBER (x);  class := TK.Ident;
  BEGIN
    CASE p[0] OF
    | 'A' =>
              IF    (n = 3)
                AND (p[1] = 'N')
                AND (p[2] = 'D') THEN class := TK.And;
              ELSIF (n = 3)
                AND (p[1] = 'N')
                AND (p[2] = 'Y') THEN class := TK.Any;
              ELSIF (n = 5)
                AND (p[1] = 'R')
                AND (p[2] = 'R')
                AND (p[3] = 'A')
                AND (p[4] = 'Y') THEN class := TK.Array;
              ELSIF (n = 2)
                AND (p[1] = 'S') THEN class := TK.As;
              END;
    | 'B' =>
              IF    (n = 5)
                AND (p[1] = 'E')
                AND (p[2] = 'G')
                AND (p[3] = 'I')
                AND (p[4] = 'N') THEN class := TK.Begin;
              ELSIF (n = 4)
                AND (p[1] = 'I')
                AND (p[2] = 'T')
                AND (p[3] = 'S') THEN class := TK.Bits;
              ELSIF (n = 7)
                AND (p[1] = 'R')
                AND (p[2] = 'A')
                AND (p[3] = 'N')
                AND (p[4] = 'D')
                AND (p[5] = 'E')
                AND (p[6] = 'D') THEN class := TK.Branded;
              ELSIF (n = 2)
                AND (p[1] = 'Y') THEN class := TK.By;
              END;
    | 'C' =>
              IF    (n = 4)
                AND (p[1] = 'A')
                AND (p[2] = 'S')
                AND (p[3] = 'E') THEN class := TK.Case;
              ELSIF (n = 5)
                AND (p[1] = 'O')
                AND (p[2] = 'N')
                AND (p[3] = 'S')
                AND (p[4] = 'T') THEN class := TK.Const;
              END;
    | 'D' =>
              IF    (n = 3)
                AND (p[1] = 'I')
                AND (p[2] = 'V') THEN class := TK.Div;
              ELSIF (n = 2)
                AND (p[1] = 'O') THEN class := TK.Do;
              END;
    | 'E' =>
              IF    (n = 4)
                AND (p[1] = 'L')
                AND (p[2] = 'S')
                AND (p[3] = 'E') THEN class := TK.Else;
              ELSIF (n = 5)
                AND (p[1] = 'L')
                AND (p[2] = 'S')
                AND (p[3] = 'I')
                AND (p[4] = 'F') THEN class := TK.Elsif;
              ELSIF (n = 3)
                AND (p[1] = 'N')
                AND (p[2] = 'D') THEN class := TK.End;
              ELSIF (n = 4)
                AND (p[1] = 'V')
                AND (p[2] = 'A')
                AND (p[3] = 'L') THEN class := TK.Eval;
              ELSIF (n = 6)
                AND (p[1] = 'X')
                AND (p[2] = 'C')
                AND (p[3] = 'E')
                AND (p[4] = 'P')
                AND (p[5] = 'T') THEN class := TK.Except;
              ELSIF (n = 9)
                AND (p[1] = 'X')
                AND (p[2] = 'C')
                AND (p[3] = 'E')
                AND (p[4] = 'P')
                AND (p[5] = 'T')
                AND (p[6] = 'I')
                AND (p[7] = 'O')
                AND (p[8] = 'N') THEN class := TK.Exception;
              ELSIF (n = 4)
                AND (p[1] = 'X')
                AND (p[2] = 'I')
                AND (p[3] = 'T') THEN class := TK.Exit;
              ELSIF (n = 7)
                AND (p[1] = 'X')
                AND (p[2] = 'P')
                AND (p[3] = 'O')
                AND (p[4] = 'R')
                AND (p[5] = 'T')
                AND (p[6] = 'S') THEN class := TK.Exports; 
              END;
    | 'F' =>
              IF    (n = 7)
                AND (p[1] = 'I')
                AND (p[2] = 'N')
                AND (p[3] = 'A')
                AND (p[4] = 'L')
                AND (p[5] = 'L')
                AND (p[6] = 'Y') THEN class := TK.Finally;
              ELSIF (n = 3)
                AND (p[1] = 'O')
                AND (p[2] = 'R') THEN class := TK.For;
              ELSIF (n = 4)
                AND (p[1] = 'R')
                AND (p[2] = 'O')
                AND (p[3] = 'M') THEN class := TK.From;
              END;
    | 'G' =>
              IF    (n = 7)
                AND (p[1] = 'E')
                AND (p[2] = 'N')
                AND (p[3] = 'E')
                AND (p[4] = 'R')
                AND (p[5] = 'I')
                AND (p[6] = 'C') THEN class := TK.Generic;
              END;
    | 'I' =>
              IF    (n = 2)
                AND (p[1] = 'F') THEN class := TK.If;
              ELSIF (n = 6)
                AND (p[1] = 'M')
                AND (p[2] = 'P')
                AND (p[3] = 'O')
                AND (p[4] = 'R')
                AND (p[5] = 'T') THEN class := TK.Import;
              ELSIF (n = 2)
                AND (p[1] = 'N') THEN class := TK.In;
              ELSIF (n = 9)
                AND (p[1] = 'N')
                AND (p[2] = 'T')
                AND (p[3] = 'E')
                AND (p[4] = 'R')
                AND (p[5] = 'F')
                AND (p[6] = 'A')
                AND (p[7] = 'C')
                AND (p[8] = 'E') THEN class := TK.Interface;
              END;
    | 'L' =>
              IF    (n = 4)
                AND (p[1] = 'O')
                AND (p[2] = 'C')
                AND (p[3] = 'K') THEN class := TK.Lock;
              ELSIF (n = 4)
                AND (p[1] = 'O')
                AND (p[2] = 'O')
                AND (p[3] = 'P') THEN class := TK.Loop;
              END;
    | 'M' =>
              IF    (n = 7)
                AND (p[1] = 'E')
                AND (p[2] = 'T')
                AND (p[3] = 'H')
                AND (p[4] = 'O')
                AND (p[5] = 'D')
                AND (p[6] = 'S') THEN class := TK.Methods;
              ELSIF (n = 3)
                AND (p[1] = 'O')
                AND (p[2] = 'D') THEN class := TK.Mod;
              ELSIF (n = 6)
                AND (p[1] = 'O')
                AND (p[2] = 'D')
                AND (p[3] = 'U')
                AND (p[4] = 'L')
                AND (p[5] = 'E') THEN class := TK.Module;
              END;
    | 'N' =>
              IF    (n = 3)
                AND (p[1] = 'O')
                AND (p[2] = 'T') THEN class := TK.Not;
              END;
    | 'O' =>
              IF    (n = 6)
                AND (p[1] = 'B')
                AND (p[2] = 'J')
                AND (p[3] = 'E')
                AND (p[4] = 'C')
                AND (p[5] = 'T') THEN class := TK.Object;
              ELSIF (n = 2)
                AND (p[1] = 'F') THEN class := TK.Of;
              ELSIF (n = 2)
                AND (p[1] = 'R') THEN class := TK.Or;
              ELSIF (n = 9)
                AND (p[1] = 'V')
                AND (p[2] = 'E')
                AND (p[3] = 'R')
                AND (p[4] = 'R')
                AND (p[5] = 'I')
                AND (p[6] = 'D')
                AND (p[7] = 'E')
                AND (p[8] = 'S') THEN class := TK.Overrides;
              END;
    | 'P' =>
              IF    (n = 9)
                AND (p[1] = 'R')
                AND (p[2] = 'O')
                AND (p[3] = 'C')
                AND (p[4] = 'E')
                AND (p[5] = 'D')
                AND (p[6] = 'U')
                AND (p[7] = 'R')
                AND (p[8] = 'E') THEN class := TK.Procedure;
              END;
    | 'R' =>
              IF    (n = 5)
                AND (p[1] = 'A')
                AND (p[2] = 'I')
                AND (p[3] = 'S')
                AND (p[4] = 'E') THEN class := TK.Raise;
              ELSIF (n = 6)
                AND (p[1] = 'A')
                AND (p[2] = 'I')
                AND (p[3] = 'S')
                AND (p[4] = 'E')
                AND (p[5] = 'S') THEN class := TK.Raises;
              ELSIF (n = 8)
                AND (p[1] = 'E')
                AND (p[2] = 'A')
                AND (p[3] = 'D')
                AND (p[4] = 'O')
                AND (p[5] = 'N')
                AND (p[6] = 'L')
                AND (p[7] = 'Y') THEN class := TK.Readonly;
              ELSIF (n = 6)
                AND (p[1] = 'E')
                AND (p[2] = 'C')
                AND (p[3] = 'O')
                AND (p[4] = 'R')
                AND (p[5] = 'D') THEN class := TK.Record;
              ELSIF (n = 3)
                AND (p[1] = 'E')
                AND (p[2] = 'F') THEN class := TK.Ref;
              ELSIF (n = 6)
                AND (p[1] = 'E')
                AND (p[2] = 'P')
                AND (p[3] = 'E')
                AND (p[4] = 'A')
                AND (p[5] = 'T') THEN class := TK.Repeat;
              ELSIF (n = 6)
                AND (p[1] = 'E')
                AND (p[2] = 'T')
                AND (p[3] = 'U')
                AND (p[4] = 'R')
                AND (p[5] = 'N') THEN class := TK.Return;
              ELSIF (n = 6)
                AND (p[1] = 'E')
                AND (p[2] = 'V')
                AND (p[3] = 'E')
                AND (p[4] = 'A')
                AND (p[5] = 'L') THEN class := TK.Reveal;
              END;
    | 'S' =>
              IF    (n = 3)
                AND (p[1] = 'E')
                AND (p[2] = 'T') THEN class := TK.Set;
              END;
    | 'T' =>
              IF    (n = 4)
                AND (p[1] = 'H')
                AND (p[2] = 'E')
                AND (p[3] = 'N') THEN class := TK.Then;
              ELSIF (n = 2)
                AND (p[1] = 'O') THEN class := TK.To;
              ELSIF (n = 3)
                AND (p[1] = 'R')
                AND (p[2] = 'Y') THEN class := TK.Try;
              ELSIF (n = 4)
                AND (p[1] = 'Y')
                AND (p[2] = 'P')
                AND (p[3] = 'E') THEN class := TK.Type;
              ELSIF (n = 8)
                AND (p[1] = 'Y')
                AND (p[2] = 'P')
                AND (p[3] = 'E')
                AND (p[4] = 'C')
                AND (p[5] = 'A')
                AND (p[6] = 'S')
                AND (p[7] = 'E') THEN class := TK.Typecase;
              END;
    | 'U' =>
              IF    (n = 6)
                AND (p[1] = 'N')
                AND (p[2] = 'S')
                AND (p[3] = 'A')
                AND (p[4] = 'F')
                AND (p[5] = 'E') THEN class := TK.Unsafe;
              ELSIF (n = 5)
                AND (p[1] = 'N')
                AND (p[2] = 'T')
                AND (p[3] = 'I')
                AND (p[4] = 'L') THEN class := TK.Until;
              ELSIF (n = 8)
                AND (p[1] = 'N')
                AND (p[2] = 'T')
                AND (p[3] = 'R')
                AND (p[4] = 'A')
                AND (p[5] = 'C')
                AND (p[6] = 'E')
                AND (p[7] = 'D') THEN class := TK.Untraced;
              END;
    | 'V' =>
              IF    (n = 5)
                AND (p[1] = 'A')
                AND (p[2] = 'L')
                AND (p[3] = 'U')
                AND (p[4] = 'E') THEN class := TK.Value;
              ELSIF (n = 3)
                AND (p[1] = 'A')
                AND (p[2] = 'R') THEN class := TK.Var;
              END;
    | 'W' =>
              IF    (n = 5)
                AND (p[1] = 'H')
                AND (p[2] = 'I')
                AND (p[3] = 'L')
                AND (p[4] = 'E') THEN class := TK.While;
              ELSIF (n = 4)
                AND (p[1] = 'I')
                AND (p[2] = 'T')
                AND (p[3] = 'H') THEN class := TK.With;
              END;

    ELSE (* class := TK.Ident *)
    END;
    RETURN class;
  END Classify;

BEGIN
END M3ID.
