MODULE xRoot;
(*Copyright (c) 1996, Harry George
  
Abstract: Implementation of Root.

1/28/96    Harry George    Initial version, from earlier work
2/17/96    Harry George    Converted from OO to ADT format
*)
FROM xUtils IMPORT Error,Err;
IMPORT xReal64   AS R,
       xComplex  AS C;
FROM xReal64 IMPORT
  REAL64,Ftn,Array,Zero,Half,One,TINY,EPS;

CONST Module = "xRoot.";
(*==========================*)
(*=====================*)
(* Quadratics          *)
(*=====================*)

(*---------------------*)
PROCEDURE quadreal (a,b,c:REAL64;             (*coefs*)
                    VAR alpha,beta:C.COMPLEX; (*alpha +/- beta format*)
                    VAR x1,x2:C.COMPLEX)=     (*root format*)  
(*Given a*x^2+b*x+c=0, solve for x.*)
VAR
  disc,q,q1,q2:REAL64;  
BEGIN
  disc:=b*b-4.0D0*a*c;
  IF disc<Zero THEN
    (*use the complex version*)
    quadcmpx(C.COMPLEX{re:=a,im:=Zero},
             C.COMPLEX{re:=b,im:=Zero},
             C.COMPLEX{re:=c,im:=Zero},
             alpha,beta,x1,x2);
    RETURN;
  END;

  q:=-R.Half*(b+R.sgn(b)*R.sqrt(disc));
  q1:=q/a;
  q2:=c/q;
  x1   :=C.COMPLEX{re:=q1,im:=R.Zero};
  x2   :=C.COMPLEX{re:=q2,im:=R.Zero};
  alpha:=C.COMPLEX{re:=0.5D0*(q1+q2),im:=R.Zero};
  beta :=C.COMPLEX{re:=0.5D0*(q1-q2),im:=R.Zero};
END quadreal;
(*---------------------*)
PROCEDURE quadcmpx (a,b,c:C.COMPLEX;          (*coefs*)
                    VAR alpha,beta:C.COMPLEX; (*alpha +/- beta format*)
                    VAR x1,x2:C.COMPLEX)=     (*results*)  
(*Given a*x^2+b*x+c=0, solve for x.*)
CONST
  c4 =C.COMPLEX{re:= 4.0D0,im:=Zero};
  c05=C.COMPLEX{re:=-0.5D0,im:=Zero};
VAR
  disc,disc_sqrt,q:C.COMPLEX;  
BEGIN
  disc:=C.sub(C.mul(b,b),C.mul(c4,C.mul(a,c)));
  disc_sqrt:=C.sqrt(disc);

  (*---set sign of sqrt via NR92 eqn 5.6.6---*)
  IF C.mul(C.conj(b),disc_sqrt).re<Zero THEN
    disc_sqrt.re:=-disc_sqrt.re;
  END;

  (*---calculate per NR92 eqn 5.6.4, 5.6.5.---*)  
  q:=C.mul(c05,C.add(b,disc_sqrt));
  x1:=C.div(q,a);
  x2:=C.div(c,q);
  alpha:=C.scale(C.add(x1,x2),R.Half);
  beta :=C.scale(C.sub(x1,x2),R.Half);
END quadcmpx;

(*=====================*)
(* Non-linear Ftns     *)
(*=====================*)
(*----------------*)
PROCEDURE bracket_out(func:Ftn;      (*find brackets for this function*)
                VAR x1,x2:REAL64;    (*starting with these points*)
                maxiter:CARDINAL:=55 (*growing maxiter times*)
                ):BOOLEAN RAISES {Error}= (*true if successful*)
(*Given x1,x2, search for points (returned in x1, x2) for which
func(x1) is opposite sign from func(x2).  Grow outward from
the original x1,x2 by golden ratio, for geometric growth.
Return true if a good x1,x2 can be found before getting to
maxiter, else return false. 

requires: x1<x2.
*)
CONST
  ftn = Module & "bracket_out";
VAR
  f1,f2,diff:REAL64;
BEGIN
  IF x2-x1 < TINY THEN
    (*need x1<x2*)
    RAISE Error(Err.out_of_range);
  END;
  (*---initialize---*)
  f1:=func(x1); f2:=func(x2);
  (*---loop to completion---*)
  FOR i:=1 TO maxiter DO
    (*---check exit criteria---*)
    IF (f1<Zero AND f2>Zero) OR (f1>Zero AND f2<Zero) THEN
      RETURN TRUE;
    END;
    diff:=x2-x1;
    (*---grow the smallest one---*)
    IF ABS(f1) < ABS(f2) THEN
      x1:=x1-R.Golden*diff; (*x1 gets more negative*)
      f1:=func(x1);      
    ELSE
      x2:=x2+R.Golden*diff; (*x2 gets more positive*)
      f2:=func(x2);
    END;
  END;
  RETURN FALSE;
END bracket_out;
(*----------------*)
PROCEDURE bracket_in(func:Ftn;       (*find brackets for this function*)
                VAR x1,x2:REAL64;    (*starting with these points*)
                n:CARDINAL;          (*using n segments*)
                xb1,xb2:Array;       (*returning pairs here*)
                VAR nb:CARDINAL      (*with this count of valid pairs*)
                ):BOOLEAN RAISES {Error}= (*true if successful*)
(*Break up the x1..x2 range into n segments.  Search for pairs which
allow bracketing.  Save up to nb of these pairs.

requires: x1<x2,  nb<n.
*)
CONST ftn = Module & "bracket_in";
VAR
  h,x,xh,y,yh:REAL64;
  maxnb:=nb;
  result:BOOLEAN:=FALSE;
BEGIN
  IF x1>=x2 THEN
    (*need x1<x2*)
    RAISE Error(Err.out_of_range);
  END;
  IF (nb < 1) OR (nb>NUMBER(xb1^)) OR (nb> NUMBER(xb2^)) THEN
    (*need 1 <= nb <= size of xb1 and xb2*)
    RAISE Error(Err.out_of_range);
  END;
  
  h:=(x2-x1)/FLOAT(n,REAL64);
  x:=x1; y:=func(x); nb:=0;
  FOR i:=1 TO n DO
    xh:=x+h; yh:=func(xh);
    IF (y<Zero AND yh>Zero) OR (y>Zero AND yh<Zero) THEN
      xb1[nb]:=x; xb2[nb]:=xh; INC(nb);
      IF nb=maxnb THEN EXIT; END;
    END;
    x:=xh; y:=yh;
  END;  
  RETURN nb>0;
END bracket_in;

(*----------------*)
PROCEDURE bisect(func:Ftn;           (*find root of this function*)
                 x1,x2:REAL64;       (*between these brackets*)
                 tol:REAL64;         (*to within +/- tolerance*)
                 maxiter:=45         (*but no more than maxiter cuts*)
                 ):REAL64 RAISES {Error} = (*returning the root*)
(*Given brackets x1,x2, find a root via bisection, and refine it
to within +/- tol
*)
CONST
  ftn = Module & "bisect";
VAR
  h,x,y1,y2,y:REAL64;
BEGIN
  (*---check preconditions---*)
  IF x2-x1 < TINY THEN
    (*need x1<x2*)
    RAISE Error(Err.out_of_range);
  END;
  y1:=func(x1); y2:=func(x2);
  IF (y1>Zero AND y2>Zero) OR (y1<Zero AND y2<Zero) THEN
    (*x1 and x2 do not bracket a root*)
    RAISE Error(Err.not_bracketed);
  END;
  
  (*---initialize---*)
  IF y1>Zero THEN
    x:=x2; h:=x1-x2;
  ELSE
    x:=x1; h:=x2-x1;
  END;
  h:=h*Half; x:=x+h;

  (*---loop for maxiter or exit conditions---*)
  FOR i:=1 TO maxiter DO
    y:=func(x);
    IF y=Zero THEN
      RETURN x;
    ELSIF y<Zero THEN
      x:=x+h;
    ELSE
      x:=x-h;
    END;
    IF ABS(h)<tol THEN RETURN x; END;
    h:=h*Half;
  END;
  RAISE Error(Err.not_converging);
  RETURN x;
END bisect;
(*----------------*)
PROCEDURE brent(func:Ftn;           (*find a root of this function*)
                x1,x2:REAL64;       (*between these bracket points*)
                tol:REAL64;         (*to this tolerance*)
                maxiter:=100        (*with <= maxiter iterations*)
                ):REAL64 RAISES {Error}=
(*Use Brent's algorithm to find the real root between the
bracket points.  x1 and x2 must be of opposite signs.
*)
CONST
  ftn = Module & "brent";
VAR
  a,b,c,fa,fb,fc,diffnext,diff2,delta:REAL64;
  min1,min2,tolnext:REAL64;
  r,s,t,p,q:REAL64;
BEGIN
  (*---check for quick victory---*)
  a:=x1; fa:=func(a);
  IF ABS(fa)<TINY THEN RETURN a; END;
  b:=x2; fb:=func(b);
  IF ABS(fb)<TINY THEN RETURN b; END;

  (*---check for bad bracketing---*)
  IF fa*fb>Zero THEN
    (*x1 and x2 do not bracket root*)
    RAISE Error(Err.not_bracketed);
  END;

  (*---set c at a---*)
  c:=x1;  fc:=fa;

  (*---loop---*)
  FOR i:=1 TO maxiter DO
    (*---establish preconditions for loop---*)
    (*1. a and c are same side, with b opposite*)
    IF (fb<Zero AND fc<Zero) OR (fb>Zero AND fc>Zero) THEN
      c:=a; fc:=fa;
    END;
    (*2. fb is smallest of the three*)
    IF ABS(fc)<ABS(fb) THEN
      (*use the smallest one for b*)
      (*and keep c with a*)
      a:=b; fa:=fb;
      b:=c; fb:=fc;
      c:=a; fc:=fa;
    END;

    (*---check for convergence---*)
    (*1. check for quick victory*)
    IF ABS(fb)<TINY THEN RETURN b; END;
    (*2. get estimate of length if we go again*)
    diffnext:=Half*(c-b);  diff2:=2.0D0*diffnext;
    (*3. get practical tolerance*)
    (*the idea is to do worst case machine tol or requested tol*)
    (*where one typically swamps the other*)
    tolnext:=EPS*ABS(b)+Half*tol;    
    (*4. check estimate for being too small*)
    IF ABS(diffnext)<tolnext THEN RETURN b; END;

    (*---ready for another attempt---*)
    IF ABS(a-b) >=tolnext AND ABS(fa)>ABS(fb) THEN
      (*---try for quadratic---*)
      (*1. build p and q, using reduction if possible*)
      s:=fb/fa;
      IF a=c THEN (*reduces to linear*)
        p:=diff2*s;
        q:=One-s;
      ELSE
        r:=fb/fc; t:=fa/fc;
        p:=s*(t*(r-t)*diff2 - (One-r)*(b-a));
        q:=(t-One)*(r-One)*(s-One);
      END;
      (*2. need p < q *)
      min1:=1.5D0*diff2*q-ABS(tolnext*q);
      min2:=ABS(diff2*q);
      IF p < MIN(min1,min2) THEN
        (*ok to interpolate*)
        delta:=p/q; 
      ELSE
        (*use bisection*)
        delta:=diffnext;
      END;
    ELSE (*bad candidate for quadratic, need bisection*)
      delta:=diffnext;
    END;

    (*---have diff, so use it---*)
    (*1. save old b as new a*)
    a:=b; fa:=fb;
    (*2. get new b*)
    b:=b+delta;  fb:=func(b);        
  END;
  RAISE Error(Err.not_converging);
  RETURN x2;      
END brent;

(*---------------------*)
PROCEDURE newtraph(
                 func:PROCEDURE(x:REAL64; VAR f,df:REAL64); (*this ftn*)
                 x1,x2:REAL64;    (*bracketed by these points*)
                 xtol:REAL64;     (*find root to this x precision*)
                 maxiter:=25      (*with no more than maxiter loops*)
                 ):REAL64 RAISES {Error}=  (*returning root*)
(*Given a function which returns both f(x) and df(x),
and brackets x1 and x2, find the root to xtol precision.
Works via newton-raphson and bisection.

*)
CONST ftn = Module & "newtraph";
VAR
  f1,f2,f,df,delta,root,rootnext:REAL64;
  a,b,tmp:REAL64;
BEGIN
  IF x1>=x2 THEN
    (*need x1<x2*)
    RAISE Error(Err.not_bracketed);
  END;
  func(x1,f1,df); IF f1=Zero THEN RETURN x1; END;
  func(x2,f2,df); IF f2=Zero THEN RETURN x2; END;
  IF (f1<Zero AND f2<Zero) OR (f1>Zero AND f2>Zero) THEN
    RAISE Error(Err.not_bracketed);
  END;

  (*---orient for fa<0, fb>0---*)
  IF f1<Zero THEN
    a:=x1; b:=x2;
  ELSE
    a:=x2; b:=x1;
  END;
  
  (*---init and loop---*)
  root:=a;
  func(root,f,df); 
  FOR i:=1 TO maxiter DO
    IF f=Zero THEN RETURN root; END;
    IF df = Zero THEN df:=TINY; END;
    delta:=f/df;
    rootnext:=root-delta; 
    IF (a-rootnext)*(rootnext-b) >= Zero THEN
      (*in bounds and fast enough for newton-raphson*)
      root:=rootnext;
    ELSE
      (*out of bounds, need bisect*)
      tmp:=root;
      root:=Half*(a+b);
      delta:=root-tmp;
    END;
    IF ABS(delta)<xtol THEN RETURN root; END;
    func(root,f,df);
    IF f<Zero THEN
      a:=root;
    ELSE
      b:=root;
    END;
  END;
  RAISE Error(Err.not_converging);
  RETURN Zero;
END newtraph;
(*==========================*)
BEGIN
END xRoot.
