INTERFACE xPoly;
(*Copyright (c) 1996, m3na project
  
Abstract: Direct access to Polynomial functions

2/3/96   Harry George    Initial version
2/17/96  Harry George    Convert from OO to ADT
*)

FROM xUtils IMPORT Error,Err;
FROM xReal64 IMPORT REAL64;
IMPORT Fmt,xReal64 AS R,xComplex AS C;
(*==========================*)

TYPE
  (*interpretation is: a[0] + a[1]*x + a[2]* x^2...a[n]*x^n *)
  (*text form is: Poly4{a0,a1,a2,a3} *)
  
  Poly = BRANDED "Poly"  REF ARRAY OF REAL64;
  cPoly =BRANDED "cPoly" REF ARRAY OF C.COMPLEX;

PROCEDURE new(n:CARDINAL):Poly;    (*make a poly for a0..an*)
PROCEDURE copy(p:Poly):Poly;       (*copy p to a new poly*)
PROCEDURE lex(str:TEXT):Poly;
PROCEDURE fmt(p:Poly;
              style:Fmt.Style:=Fmt.Style.Fix;
              prec:CARDINAL:=1):TEXT;

PROCEDURE Zero(p:Poly);          (*set p to zeros*)
PROCEDURE One (p:Poly);          (*set p to 1*)
    
PROCEDURE eval(p:Poly;           (*eval this polynomial*)
               x:REAL64          (*at this point*)
               ):REAL64;
PROCEDURE add(p1,p2:Poly):Poly;  (*return p1+p2*)
PROCEDURE sub(p1,p2:Poly):Poly;  (*return p1-p2*)
PROCEDURE mul(p1,p2:Poly):Poly;  (*return p1*p2*)
PROCEDURE div(p1,p2:Poly;        (*compute p1/p2 *) 
              VAR q,r:Poly);     (*giving quotient q with remainder r*)
PROCEDURE deflate(p:Poly;        (*divide this polynomial*)
                  c:REAL64;      (* by (x-c) *)
                  VAR rem:REAL64);(*leaving remainder -- possibly 0*)
PROCEDURE deriv(p:Poly;          (*eval this polynomial*)
                x:REAL64;        (*at this point*)
               pd:R.Array;       (*returning p(x), p'(x)...*)
               nd:CARDINAL       (*for up to nd derivatives*)
                ) RAISES {Error};
          (*raises:
               Err.bad_size if nd>NUMBER(pd)+1
          *)
(*==========================*)
END xPoly.
