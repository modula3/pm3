INTERFACE xMat;
(*Copyright (c) 1996, m3na project
  
Abstract: Direct access to matrix functions

2/17/96  Harry George    Convert from OO to ADT
*)
FROM xUtils IMPORT Error,Err;
FROM xReal64 IMPORT REAL64;
FROM xVect IMPORT Vector;
IMPORT Fmt;
(*==========================*)
(*-----------------*)
TYPE
(*
|   text form: "M2x6{
                V6{a00,a01,a02,a03,a04,a05},
|               V6{a10,a11,a12,a13,a14,a15}
|               }"
*)
  Matrix  = REF ARRAY OF ARRAY OF REAL64;

PROCEDURE new(m,n:CARDINAL):Matrix; (*make new mxn matrix*)
PROCEDURE copy(mat:Matrix):Matrix;
PROCEDURE lex(str:TEXT):Matrix RAISES {Error};
PROCEDURE fmt(mat:Matrix;
              style:=Fmt.Style.Fix;
              prec:=2):TEXT;
    
PROCEDURE Zero(mat:Matrix);                              (*set to zeros*)
PROCEDURE One (mat:Matrix) RAISES {Error};               (*set to identity*)

PROCEDURE add(mat1,mat2:Matrix):Matrix RAISES {Error};   (*mat1  +mat2*)
PROCEDURE sub(mat1,mat2:Matrix):Matrix RAISES {Error};   (*mat1 - mat2*)
PROCEDURE mul(mat1,mat2:Matrix):Matrix RAISES {Error};   (*mat1 * mat2*)
PROCEDURE mulV(A:Matrix;b:Vector):Vector RAISES {Error}; (*A * b*)
PROCEDURE transpose(mat:Matrix):Matrix;                  (*mat^T*)
(*==========================*)
END xMat.
