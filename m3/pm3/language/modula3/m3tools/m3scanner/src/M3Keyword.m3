(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Keyword.m3                                          *)
(* Last modified on Fri May 28 09:52:06 PDT 1993 by kalsow     *)

UNSAFE MODULE M3Keyword;

FROM M3Scanner IMPORT TK, TK_Ident,
  TK_And, TK_Any, TK_Array, TK_As, TK_Begin, TK_Bits, TK_Branded,
  TK_By, TK_Case, TK_Const, TK_Div, TK_Do, TK_Else, TK_Elsif, TK_End,
  TK_Eval, TK_Except, TK_Exception, TK_Exit, TK_Exports, TK_Finally,
  TK_For, TK_From, TK_Generic, TK_If, TK_Import, TK_In, TK_Interface,
  TK_Lock, TK_Loop, TK_Methods, TK_Mod, TK_Module, TK_Not, TK_Object,
  TK_Of, TK_Or, TK_Overrides, TK_Procedure, TK_Raise, TK_Raises,
  TK_Readonly, TK_Record, TK_Ref, TK_Repeat, TK_Return, TK_Reveal,
  TK_Set, TK_Then, TK_To, TK_Try, TK_Type, TK_Typecase, TK_Unsafe,
  TK_Until, TK_Untraced, TK_Value, TK_Var, TK_While, TK_With;

(* Note: the module is unsafe because we use "p^[i]" instead
   of "x[i]", but since "p^" is a fixed length array we avoid
   the runtime subscript checks. *)

PROCEDURE Classify (READONLY x: ARRAY OF CHAR): TK =
  VAR p : UNTRACED REF ARRAY [0..8] OF CHAR := ADR (x[0]);
  VAR n := NUMBER (x);  class := TK_Ident;
  BEGIN
    CASE p[0] OF
    | 'A' =>
              IF    (n = 3)
                AND (p[1] = 'N')
                AND (p[2] = 'D') THEN class := TK_And;
              ELSIF (n = 3)
                AND (p[1] = 'N')
                AND (p[2] = 'Y') THEN class := TK_Any;
              ELSIF (n = 5)
                AND (p[1] = 'R')
                AND (p[2] = 'R')
                AND (p[3] = 'A')
                AND (p[4] = 'Y') THEN class := TK_Array;
              ELSIF (n = 2)
                AND (p[1] = 'S') THEN class := TK_As;
              END;
    | 'B' =>
              IF    (n = 5)
                AND (p[1] = 'E')
                AND (p[2] = 'G')
                AND (p[3] = 'I')
                AND (p[4] = 'N') THEN class := TK_Begin;
              ELSIF (n = 4)
                AND (p[1] = 'I')
                AND (p[2] = 'T')
                AND (p[3] = 'S') THEN class := TK_Bits;
              ELSIF (n = 7)
                AND (p[1] = 'R')
                AND (p[2] = 'A')
                AND (p[3] = 'N')
                AND (p[4] = 'D')
                AND (p[5] = 'E')
                AND (p[6] = 'D') THEN class := TK_Branded;
              ELSIF (n = 2)
                AND (p[1] = 'Y') THEN class := TK_By;
              END;
    | 'C' =>
              IF    (n = 4)
                AND (p[1] = 'A')
                AND (p[2] = 'S')
                AND (p[3] = 'E') THEN class := TK_Case;
              ELSIF (n = 5)
                AND (p[1] = 'O')
                AND (p[2] = 'N')
                AND (p[3] = 'S')
                AND (p[4] = 'T') THEN class := TK_Const;
              END;
    | 'D' =>
              IF    (n = 3)
                AND (p[1] = 'I')
                AND (p[2] = 'V') THEN class := TK_Div;
              ELSIF (n = 2)
                AND (p[1] = 'O') THEN class := TK_Do;
              END;
    | 'E' =>
              IF    (n = 4)
                AND (p[1] = 'L')
                AND (p[2] = 'S')
                AND (p[3] = 'E') THEN class := TK_Else;
              ELSIF (n = 5)
                AND (p[1] = 'L')
                AND (p[2] = 'S')
                AND (p[3] = 'I')
                AND (p[4] = 'F') THEN class := TK_Elsif;
              ELSIF (n = 3)
                AND (p[1] = 'N')
                AND (p[2] = 'D') THEN class := TK_End;
              ELSIF (n = 4)
                AND (p[1] = 'V')
                AND (p[2] = 'A')
                AND (p[3] = 'L') THEN class := TK_Eval;
              ELSIF (n = 6)
                AND (p[1] = 'X')
                AND (p[2] = 'C')
                AND (p[3] = 'E')
                AND (p[4] = 'P')
                AND (p[5] = 'T') THEN class := TK_Except;
              ELSIF (n = 9)
                AND (p[1] = 'X')
                AND (p[2] = 'C')
                AND (p[3] = 'E')
                AND (p[4] = 'P')
                AND (p[5] = 'T')
                AND (p[6] = 'I')
                AND (p[7] = 'O')
                AND (p[8] = 'N') THEN class := TK_Exception;
              ELSIF (n = 4)
                AND (p[1] = 'X')
                AND (p[2] = 'I')
                AND (p[3] = 'T') THEN class := TK_Exit;
              ELSIF (n = 7)
                AND (p[1] = 'X')
                AND (p[2] = 'P')
                AND (p[3] = 'O')
                AND (p[4] = 'R')
                AND (p[5] = 'T')
                AND (p[6] = 'S') THEN class := TK_Exports; 
              END;
    | 'F' =>
              IF    (n = 7)
                AND (p[1] = 'I')
                AND (p[2] = 'N')
                AND (p[3] = 'A')
                AND (p[4] = 'L')
                AND (p[5] = 'L')
                AND (p[6] = 'Y') THEN class := TK_Finally;
              ELSIF (n = 3)
                AND (p[1] = 'O')
                AND (p[2] = 'R') THEN class := TK_For;
              ELSIF (n = 4)
                AND (p[1] = 'R')
                AND (p[2] = 'O')
                AND (p[3] = 'M') THEN class := TK_From;
              END;
    | 'G' =>
              IF    (n = 7)
                AND (p[1] = 'E')
                AND (p[2] = 'N')
                AND (p[3] = 'E')
                AND (p[4] = 'R')
                AND (p[5] = 'I')
                AND (p[6] = 'C') THEN class := TK_Generic;
              END;
    | 'I' =>
              IF    (n = 2)
                AND (p[1] = 'F') THEN class := TK_If;
              ELSIF (n = 6)
                AND (p[1] = 'M')
                AND (p[2] = 'P')
                AND (p[3] = 'O')
                AND (p[4] = 'R')
                AND (p[5] = 'T') THEN class := TK_Import;
              ELSIF (n = 2)
                AND (p[1] = 'N') THEN class := TK_In;
              ELSIF (n = 9)
                AND (p[1] = 'N')
                AND (p[2] = 'T')
                AND (p[3] = 'E')
                AND (p[4] = 'R')
                AND (p[5] = 'F')
                AND (p[6] = 'A')
                AND (p[7] = 'C')
                AND (p[8] = 'E') THEN class := TK_Interface;
              END;
    | 'L' =>
              IF    (n = 4)
                AND (p[1] = 'O')
                AND (p[2] = 'C')
                AND (p[3] = 'K') THEN class := TK_Lock;
              ELSIF (n = 4)
                AND (p[1] = 'O')
                AND (p[2] = 'O')
                AND (p[3] = 'P') THEN class := TK_Loop;
              END;
    | 'M' =>
              IF    (n = 7)
                AND (p[1] = 'E')
                AND (p[2] = 'T')
                AND (p[3] = 'H')
                AND (p[4] = 'O')
                AND (p[5] = 'D')
                AND (p[6] = 'S') THEN class := TK_Methods;
              ELSIF (n = 3)
                AND (p[1] = 'O')
                AND (p[2] = 'D') THEN class := TK_Mod;
              ELSIF (n = 6)
                AND (p[1] = 'O')
                AND (p[2] = 'D')
                AND (p[3] = 'U')
                AND (p[4] = 'L')
                AND (p[5] = 'E') THEN class := TK_Module;
              END;
    | 'N' =>
              IF    (n = 3)
                AND (p[1] = 'O')
                AND (p[2] = 'T') THEN class := TK_Not;
              END;
    | 'O' =>
              IF    (n = 6)
                AND (p[1] = 'B')
                AND (p[2] = 'J')
                AND (p[3] = 'E')
                AND (p[4] = 'C')
                AND (p[5] = 'T') THEN class := TK_Object;
              ELSIF (n = 2)
                AND (p[1] = 'F') THEN class := TK_Of;
              ELSIF (n = 2)
                AND (p[1] = 'R') THEN class := TK_Or;
              ELSIF (n = 9)
                AND (p[1] = 'V')
                AND (p[2] = 'E')
                AND (p[3] = 'R')
                AND (p[4] = 'R')
                AND (p[5] = 'I')
                AND (p[6] = 'D')
                AND (p[7] = 'E')
                AND (p[8] = 'S') THEN class := TK_Overrides;
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
                AND (p[8] = 'E') THEN class := TK_Procedure;
              END;
    | 'R' =>
              IF    (n = 5)
                AND (p[1] = 'A')
                AND (p[2] = 'I')
                AND (p[3] = 'S')
                AND (p[4] = 'E') THEN class := TK_Raise;
              ELSIF (n = 6)
                AND (p[1] = 'A')
                AND (p[2] = 'I')
                AND (p[3] = 'S')
                AND (p[4] = 'E')
                AND (p[5] = 'S') THEN class := TK_Raises;
              ELSIF (n = 8)
                AND (p[1] = 'E')
                AND (p[2] = 'A')
                AND (p[3] = 'D')
                AND (p[4] = 'O')
                AND (p[5] = 'N')
                AND (p[6] = 'L')
                AND (p[7] = 'Y') THEN class := TK_Readonly;
              ELSIF (n = 6)
                AND (p[1] = 'E')
                AND (p[2] = 'C')
                AND (p[3] = 'O')
                AND (p[4] = 'R')
                AND (p[5] = 'D') THEN class := TK_Record;
              ELSIF (n = 3)
                AND (p[1] = 'E')
                AND (p[2] = 'F') THEN class := TK_Ref;
              ELSIF (n = 6)
                AND (p[1] = 'E')
                AND (p[2] = 'P')
                AND (p[3] = 'E')
                AND (p[4] = 'A')
                AND (p[5] = 'T') THEN class := TK_Repeat;
              ELSIF (n = 6)
                AND (p[1] = 'E')
                AND (p[2] = 'T')
                AND (p[3] = 'U')
                AND (p[4] = 'R')
                AND (p[5] = 'N') THEN class := TK_Return;
              ELSIF (n = 6)
                AND (p[1] = 'E')
                AND (p[2] = 'V')
                AND (p[3] = 'E')
                AND (p[4] = 'A')
                AND (p[5] = 'L') THEN class := TK_Reveal;
              END;
    | 'S' =>
              IF    (n = 3)
                AND (p[1] = 'E')
                AND (p[2] = 'T') THEN class := TK_Set;
              END;
    | 'T' =>
              IF    (n = 4)
                AND (p[1] = 'H')
                AND (p[2] = 'E')
                AND (p[3] = 'N') THEN class := TK_Then;
              ELSIF (n = 2)
                AND (p[1] = 'O') THEN class := TK_To;
              ELSIF (n = 3)
                AND (p[1] = 'R')
                AND (p[2] = 'Y') THEN class := TK_Try;
              ELSIF (n = 4)
                AND (p[1] = 'Y')
                AND (p[2] = 'P')
                AND (p[3] = 'E') THEN class := TK_Type;
              ELSIF (n = 8)
                AND (p[1] = 'Y')
                AND (p[2] = 'P')
                AND (p[3] = 'E')
                AND (p[4] = 'C')
                AND (p[5] = 'A')
                AND (p[6] = 'S')
                AND (p[7] = 'E') THEN class := TK_Typecase;
              END;
    | 'U' =>
              IF    (n = 6)
                AND (p[1] = 'N')
                AND (p[2] = 'S')
                AND (p[3] = 'A')
                AND (p[4] = 'F')
                AND (p[5] = 'E') THEN class := TK_Unsafe;
              ELSIF (n = 5)
                AND (p[1] = 'N')
                AND (p[2] = 'T')
                AND (p[3] = 'I')
                AND (p[4] = 'L') THEN class := TK_Until;
              ELSIF (n = 8)
                AND (p[1] = 'N')
                AND (p[2] = 'T')
                AND (p[3] = 'R')
                AND (p[4] = 'A')
                AND (p[5] = 'C')
                AND (p[6] = 'E')
                AND (p[7] = 'D') THEN class := TK_Untraced;
              END;
    | 'V' =>
              IF    (n = 5)
                AND (p[1] = 'A')
                AND (p[2] = 'L')
                AND (p[3] = 'U')
                AND (p[4] = 'E') THEN class := TK_Value;
              ELSIF (n = 3)
                AND (p[1] = 'A')
                AND (p[2] = 'R') THEN class := TK_Var;
              END;
    | 'W' =>
              IF    (n = 5)
                AND (p[1] = 'H')
                AND (p[2] = 'I')
                AND (p[3] = 'L')
                AND (p[4] = 'E') THEN class := TK_While;
              ELSIF (n = 4)
                AND (p[1] = 'I')
                AND (p[2] = 'T')
                AND (p[3] = 'H') THEN class := TK_With;
              END;

    ELSE (* class := TK_Ident *)
    END;
    RETURN class;
  END Classify;

BEGIN
END M3Keyword.
