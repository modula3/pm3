INTERFACE xVect;
(*Copyright (c) 1996, m3na project
  
Abstract: Vector math

2/17/96  Harry George    Covert from Objects to ADT's
*)
FROM xReal64 IMPORT REAL64;
FROM xUtils IMPORT Error,Err;
IMPORT Fmt;
(*==========================*)
TYPE
  (*text form: "V6{a0,a1,a2,a3,a4,a5}"*)
  Vector  = REF ARRAY OF REAL64;

PROCEDURE new(n:CARDINAL):Vector; (*make new nx1 Vector*)
PROCEDURE copy(v:Vector):Vector;
PROCEDURE lex(str:TEXT):Vector RAISES {Error};
PROCEDURE fmt(v:Vector;
            style:=Fmt.Style.Fix;
            prec:=2):TEXT;
    
PROCEDURE Zero(v:Vector);                   (*set to zero*)
    (*NOTE: you should make unit vectors as needed*)
    
PROCEDURE abs(v:Vector):REAL64;                      (*|v|*)
PROCEDURE add(v1,v2:Vector):Vector RAISES {Error};   (*v1+v2*)
PROCEDURE sub(v1,v2:Vector):Vector RAISES {Error};   (*v1-v2*)
PROCEDURE scale(v:Vector; factor:REAL64);            (*v1:=v1*factor*)
PROCEDURE dot(v1,v2:Vector):REAL64 RAISES {Error};   (*v1 dot v2*)
PROCEDURE cross(v1,v2:Vector):Vector RAISES {Error}; (*v1 x v2*)       
(*==========================*)
END xVect.
