INTERFACE xRoot;
(*Copyright (c) 1996, m3na project
  
Abstract: Direct access to root finders

1/28/96  Harry George    Initial version
*)

FROM xUtils IMPORT Error;
FROM xReal64 IMPORT REAL64,Ftn,Array;
IMPORT xComplex AS C;
(*==========================*)
(*=====================*)
(* Quadratics          *)
(*=====================*)
(*------------------*)
PROCEDURE quadreal (a,b,c:REAL64;             (*coefs*)
                    VAR alpha,beta:C.COMPLEX; (*alpha +/- beta format*)
                    VAR x1,x2:C.COMPLEX);     (*root format*)  
(*Given a*x^2+b*x+c=0, solve for x.*)
(*------------------*)
PROCEDURE quadcmpx(a,b,c:C.COMPLEX;          (*coefs*)
                   VAR alpha,beta:C.COMPLEX; (*alpha +/- beta format*)
                   VAR x1,x2:C.COMPLEX);     (*results*)  
(*Given a*x^2+b*x+c=0, solve for x.*)




(*=====================*)
(* Non-linear Ftns     *)
(*=====================*)
(*----------------*)
PROCEDURE bracket_out(func:Ftn;      (*find brackets for this function*)
                VAR x1,x2:REAL64;    (*starting with these points*)
                maxiter:CARDINAL:=55 (*growing maxiter times*)
                ):BOOLEAN RAISES {Error}; (*true if successful*)
(*Given x1,x2, search for points (returned in x1, x2) for which
func(x1) is opposite sign from func(x2).  Grow outward from
the original x1,x2 by golden ratio, for geometric growth.
Return true if a good x1,x2 can be found before getting to
maxiter, else return false. 

requires: x1<x2.
*)

(*----------------*)
PROCEDURE bracket_in(func:Ftn;       (*find brackets for this function*)
                VAR x1,x2:REAL64;    (*starting with these points*)
                n:CARDINAL;          (*using n segments*)
                xb1,xb2:Array;       (*returning pairs here*)
                VAR nb:CARDINAL      (*with this count of valid pairs*)
                ):BOOLEAN RAISES {Error}; (*true if successful*)
(*Break up the x1..x2 range into n segments.  Search for pairs which
allow bracketing.  Save up to nb of these pairs.

requires: x1<x2,  nb<n.
*)

(*------------------*)
PROCEDURE bisect(func:Ftn;           (*find root of this function*)
                 x1,x2:REAL64;       (*between these brackets*)
                 tol:REAL64;         (*to within +/- tolerance*)
                 maxiter:=45         (*but no more than maxiter cuts*)
                 ):REAL64 RAISES {Error}; (*returning the root*)
(*Given brackets x1,x2, find a root via bisection, and refine it
to within +/- tol
*)
(*------------------*)
PROCEDURE brent(func:Ftn;          (*find a root of this function*)
               x1,x2:REAL64;       (*between these bracket points*)
               tol:REAL64;         (*to this tolerance*)
               maxiter:=100        (*with <= maxiter iterations*)
               ):REAL64 RAISES {Error};
(*Use Brent's algorithm to find the real root between the
bracket points.  x1 and x2 must be of opposite signs.
*)
(*---------------------*)
PROCEDURE newtraph(
                 func:PROCEDURE(x:REAL64; VAR f,df:REAL64); (*this ftn*)
                 x1,x2:REAL64;    (*bracketed by these points*)
                 xtol:REAL64;     (*find root to this precision of x*)
                 maxiter:=25      (*with no more than maxiter loops*)
                 ):REAL64 RAISES {Error};  (*returning root*)
(*Given a function which returns both f(x) and df(x),
and brackets x1 and x2, find the root to xtol precision.
Works via newton-raphson and bisection.
*)
(*==========================*)
END xRoot.
