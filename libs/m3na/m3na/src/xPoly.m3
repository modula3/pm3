MODULE xPoly;
(*Copyright (c) 1995, Harry George
  
Abstract: Polynomials.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)
FROM xUtils IMPORT Error,Err;
IMPORT Fmt,Wr,TextWr;
IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64;

CONST Module = "xPoly.";

(*--------------------*)
PROCEDURE new( 
               n:CARDINAL):Poly=
BEGIN
  RETURN NEW(Poly,n+1);
END new;
(*--------------------*)
PROCEDURE copy( 
               p:Poly):Poly=
VAR
  n:=NUMBER(p^); n1:=0; nn:=n-1;
  tmp:=NEW(Poly,n);
BEGIN
  tmp^:=p^;
  RETURN tmp;
END copy;

(*--------------------*)
PROCEDURE lex( 
               str:TEXT):Poly=
BEGIN
  RAISE Error(Err.not_implemented);
END lex;

(*----------------------*)
PROCEDURE fmt( 
               p:Poly;
               style:Fmt.Style:=Fmt.Style.Fix;
               prec:CARDINAL:=1
               ):TEXT=
(*Generate a text object for the polynomial poly, in form:
 Poly3{a0,a1,a2}
*)
VAR
  n:=NUMBER(p^); n1:=0; nn:=n-1;
  wr:=NEW(TextWr.T).init(); 
BEGIN
  Wr.PutText(wr,"Poly"
     & Fmt.Int(n) & "{");
  FOR i:=n1 TO nn DO
    Wr.PutText(wr,R.fmt(p[i],style,prec));
    IF i#nn THEN Wr.PutText(wr,", "); END;
  END;
  Wr.PutText(wr,"}");
  RETURN TextWr.ToText(wr);
END fmt;
(*--------------------*)
PROCEDURE Zero( 
               p:Poly)=
VAR
  n:=NUMBER(p^); n1:=0; nn:=n-1;
BEGIN
  FOR i:=n1 TO nn DO
    p[i]:=R.Zero;
  END;
END Zero;
(*--------------------*)
PROCEDURE One( 
               p:Poly)=
VAR
  n:=NUMBER(p^); n0:=0; nn:=n-1;
BEGIN
  p[0]:=R.One;
  FOR i:=n0+1 TO nn DO
    p[i]:=R.Zero;
  END;
END One;
(*--------------------*)
PROCEDURE eval( 
                p:Poly;
                x:REAL64
                ):REAL64=
VAR
  n:=NUMBER(p^); n0:=0; nn:=n-1;
  tmp:=p[nn];
BEGIN
  FOR i:=nn-1 TO 1 BY -1 DO
    tmp:=p[i]+x*tmp;
  END;
  tmp:=p[0]+tmp;
  RETURN tmp;
END eval;

(*-----------------*)
PROCEDURE add( 
               p1,p2:Poly):Poly=
VAR
  p1n:=NUMBER(p1^); p1nn:=p1n-1;
  p2n:=NUMBER(p2^); p2nn:=p2n-1;
  maxn:=MAX(p1n,p2n);
  p:=NEW(Poly,maxn);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO p[i]:=p1[i]+p2[i]; END;
    FOR i:=p2nn+1 TO p1nn DO p[i]:=p1[i];       END;
  ELSE
    FOR i:=0 TO p1nn      DO p[i]:=p1[i]+p2[i]; END;
    FOR i:=p1nn+1 TO p2nn DO p[i]:=      p2[i]; END;
  END;
  RETURN p;
END add;
(*-----------------*)
PROCEDURE sub( 
               p1,p2:Poly):Poly=
VAR
  p1n:=NUMBER(p1^); p1nn:=p1n-1;
  p2n:=NUMBER(p2^); p2nn:=p2n-1;
  maxn:=MAX(p1n,p2n);
  p:=NEW(Poly,maxn);
BEGIN
  IF p1nn>=p2nn THEN
    FOR i:=0 TO p2nn      DO p[i]:=p1[i]-p2[i]; END;
    FOR i:=p2nn+1 TO p1nn DO p[i]:=p1[i];       END;
  ELSE
    FOR i:=0 TO p1nn      DO p[i]:=p1[i]-p2[i]; END;
    FOR i:=p1nn+1 TO p2nn DO p[i]:=      p2[i]; END;
  END;
  RETURN p;
END sub;

(*---------------------*)
PROCEDURE mul( 
               p1,p2:Poly):Poly=
VAR
  p1n:=NUMBER(p1^); p2n:=NUMBER(p2^);
  pn:=p1n+p2n-1; p0:=0; pnn:=pn-1;
  p:=NEW(Poly,pn);
BEGIN
  FOR i:=p0 TO pnn DO p[i]:=R.Zero; END;

  FOR i:=0 TO p1n-1 DO
    FOR j:=0 TO p2n-1 DO
      p[i+j]:=p[i+j]+p1[i]*p2[j];
    END;
  END;
  RETURN p;
END mul;

(*---------------------*)
PROCEDURE div( 
               p1,p2:Poly;
           VAR q,r:Poly)=
CONST ftn = Module & "div";
VAR
  p1n:=NUMBER(p1^); p10:=FIRST(p1^); p1nn:=LAST(p1^); 
  p2n:=NUMBER(p2^); p20:=FIRST(p2^); p2nn:=LAST(p2^);
  qtmp,p2max:REAL64;
  qn,q0,qnn,qi,ri2:CARDINAL;
BEGIN
  (*---copy numerator into r---*)
  r:=NEW(Poly,p1n); r^:=p1^;

  (*---check for quick exit---*)
  IF p1nn<p2nn THEN
    (*can't do any divides at all*)
    q:=NEW(Poly,1); q[0]:=R.Zero;
    RETURN;
  END;

  (*---setup quotient---*)
  qn:=p1n-p2n+1;
  q:=NEW(Poly,qn); q0:=FIRST(q^); qnn:=LAST(q^);

  (*---find the dominant denominator term---*)
  p2max:=p2[p2nn];


  (*---compute---*)
  qi:=qnn+1;
  FOR ri:=p1nn TO (p1nn-qnn) BY-1 DO
    DEC(qi);
    qtmp:=r[ri]/p2max;
    q[qi]:=qtmp;
    ri2:=ri+1;
    FOR p2i:=p2nn TO p20 BY -1 DO
      DEC(ri2);
      r[ri2]:=r[ri2]-qtmp*p2[p2i];
    END;
  END;
END div;

(*-----------------------*)
PROCEDURE deflate( 
                   p:Poly;
                   c:REAL64;
                   VAR rem:REAL64)=
VAR
  pnn:=LAST(p^);
  b,psave:REAL64;
BEGIN
  b:=p[pnn]; psave:=p[pnn-1]; p[pnn-1]:=b;
  FOR i:=pnn-2 TO 1 BY -1 DO
    b:=psave+c*b;
    psave:=p[i]; p[i]:=b;
  END;
  rem:=p[0]+c*p[1];
END deflate;

(*---------------------*)
PROCEDURE deriv( 
                 p:Poly;      (*evaluate the poly with these coefs*)
                 x:REAL64;    (*at this point*)
                 pd:R.Array;  (*returning p(x), p'(x)...*)
                 nd:CARDINAL  (*for up to nd derivatives*)
                 ) RAISES {Error}=
(*Given a poly with coefs p, find the value at x as pd[0],
and nd more derivatives as pd[1]..pd[nd].

raises:
   Err.bad_size if nd>NUMBER(pd)+1 
*)
VAR
  pn:=NUMBER(p^); p0:=FIRST(p^); pnn:=LAST(p^);
  pdn:=nd+1; pd0:=0; pdnn:=nd; (*may be using part of pd vector*)
BEGIN
  IF nd>NUMBER(pd^)+1 OR nd>pnn THEN
    RAISE Error(Err.bad_size);
  END;

  (*---initialize f(x) and clear f'(x), f"(x)...---*)
  pd[0]:=p[pnn];
  FOR i:=1 TO pdnn DO pd[i]:=R.Zero; END;
  
  (*---collect the raw values---*)
  FOR i:=pnn-1 TO p0 BY -1 DO
    FOR j:=pdnn TO 1 BY -1 DO
      pd[j]:=pd[j-1]+x*pd[j];
    END;
    pd[0]:=pd[0]*x+p[i];
  END;

  (*---fix the factorials---*) 
  FOR i:=0 TO pdnn DO
    pd[i]:=R.factorial(i)*pd[i];
  END;
 
END deriv; 

(*==========================*)
BEGIN
END xPoly.
