MODULE xInteger;
(*Copyright (c) 1996, m3na project
  
Abstract: Integers

2/17/96  Harry George    Initial version
*)
IMPORT Word, Fmt,Wr,TextWr;
FROM xUtils IMPORT debug;


CONST Module = "xIntegers.";
(*==========================*)
(*----------------------*)
PROCEDURE fmtArray(READONLY a:Array;
                   base      :CARDINAL:=10;
                   cellwidth :CARDINAL:=4;
                   linewidth :CARDINAL:=60):TEXT=
VAR
  wr:=TextWr.New();
  n:=NUMBER(a^); n1:=0; nn:=n-1;
  currwidth:=0;
BEGIN
  Wr.PutText(wr,"A" & Fmt.Int(n) & "{");
  FOR i:=n1 TO nn DO
    Wr.PutText(wr,Fmt.Pad(Fmt.Int(a[i],base:=base),cellwidth)); 
    IF i#nn THEN Wr.PutText(wr,", "); END;
    INC(currwidth,cellwidth+2);
    IF currwidth>linewidth THEN
      Wr.PutText(wr,"\n   ");
      currwidth:=0;
    END;
  END;
  Wr.PutText(wr,"}\n");
  RETURN TextWr.ToText(wr);  
END fmtArray;

(*============================*)
(* Factoring                  *)
(*============================*)  

(*------------------------*)
PROCEDURE factor(n:CARDINAL;   (*factor this number*)
                 VAR p,m:Array (*giving primes and multiplicity*)
                 ):CARDINAL=   (*and count of factors*)
(*e.g., factor(24) gives 2^3 * 3^1 or:
   p=[2,3]
   m=[3,1]
   return=2;
p and m are created by the procedure.
*)
CONST ftn = Module & "factor";
CONST MAXFACTOR = 30;
VAR
  i:CARDINAL;
  tmp:=n;
  ndx:=0;
BEGIN
  p:=NEW(Array,MAXFACTOR);
  m:=NEW(Array,MAXFACTOR);
  i:=2;
  WHILE i<=tmp DO
    IF isprime(i) AND (tmp MOD i = 0) THEN
      p[ndx]:=i; m[ndx]:=0;
      REPEAT
        tmp:=tmp DIV i;
        INC(m[ndx]);
      UNTIL tmp MOD i # 0;
      INC(ndx);
    END;
    INC(i);
  END;
  RETURN ndx;
END factor;
(*----------------------*)
PROCEDURE gcd(u,v:CARDINAL):CARDINAL=
(*returns the greatest common denominator for n1 and n2.*)
(*use Euclid's algorithm*)
VAR
  tmp:CARDINAL;
BEGIN
  IF u<=1 OR v<=1 THEN RETURN 1; END;
  REPEAT
    IF u<v THEN
      tmp:=u; u:=v; v:=tmp;
    END;
    u:=u MOD v;
  UNTIL u=0;
  RETURN v;
END gcd;      


(*============================*)
(* Integer Approximations     *)
(*============================*)  
(*----------------------*)
PROCEDURE sqrt(N:[0..1073741823]):CARDINAL=
(*returns integer sqrt of n*)
(*from P. Heinrich, "A Fast Integer Square Root",
Dr. Dobbs, Apr 1996, pp 113-114*)
CONST ftn = Module & "sqrt";
VAR
  u,v,u2,n:Word.T;
  vbit:CARDINAL;
BEGIN
  (*---check quick victory---*)
  IF N<2 THEN RETURN N; END;

  (*---find highest bit---*)
  u:=N; vbit:=0;
  LOOP
    u:=Word.RightShift(u,2);
    IF u=0 THEN EXIT; END;
    INC(vbit);
  END;

  (*---use vbit to make v---*)
  v:=Word.LeftShift(1,vbit);

  (*---u is approx v---*)
  u:=v;
  (*---u^2 is shifted twice as far as u---*)
  u2:=Word.LeftShift(u,vbit);

  (*---move vbit toward 0, recalculating u as we go---*)
  WHILE vbit>0 DO
    DEC(vbit);
    v:=Word.RightShift(v,1);

    (*---build the current n---*)
    (*---1. get v*(2u+v)---*)
    n:=Word.LeftShift(u+u+v,vbit);
    (*---2. add the u^2 term---*)
    INC(n,u2);

    (*---are we big enough yet?---*)
    IF n<=N THEN
      (*---new u is (u+v)---*)
      INC(u,v);
      (*---current best estimate of u^2---*)
      u2:=n;
    END;
  END;
  RETURN u;
END sqrt;

(*============================*)
(* CORDIC Functions           *)
(*============================*)  

(*----------------*)
PROCEDURE sin_cos(theta:Cordic;    (*given this angle*)
                  VAR s,c:INTEGER)= (*return sin and cos*)
CONST
  ftn = Module & "sin_cos";

  AtanTbl = ARRAY [0..CordicBits] OF CARDINAL{
    32768,19344,10221,5188,2604,1303,652,326,163,81,41,20,10,5,3,1,1};
  n1=FIRST(AtanTbl); nn=LAST(AtanTbl);

  Quad0Boundary = CordicBase*0;
  Quad1Boundary = CordicBase*1;
  Quad2Boundary = CordicBase*2;
  Quad3Boundary = CordicBase*3;
  Quad4Boundary = CordicBase*4;
  
VAR
  z:INTEGER;
  x:INTEGER:=39796;  (*initialize here to overcome the expansion*)
  y:INTEGER:=0;
  xtmp,quadrant:INTEGER;
BEGIN
  (*---find quadrant---*)
  IF    theta < Quad1Boundary THEN
    quadrant:=1; z:=theta - Quad0Boundary;
  ELSIF theta < Quad2Boundary THEN
    quadrant:=2; z:=Quad2Boundary - theta;
  ELSIF theta < Quad3Boundary THEN
    quadrant:=3; z:=theta - Quad2Boundary;
  ELSE (*known to be < Quad4Boundary, due to typechecking*)
    quadrant:=4; z:=Quad4Boundary - theta;
  END;

  (*---negate z so we can go toward 0---*)
  z:=-z;
  (*---compute---*)
  FOR i:=n1 TO nn DO
    IF z<0 THEN (*counter-clockwise*)
      xtmp:=x - Word.RightShift(y,i);
      y   :=y + Word.RightShift(x,i);
      x   :=xtmp;
      INC(z,AtanTbl[i]);
    ELSE        (*clockwise*)
      xtmp:=x + Word.RightShift(y,i);
      y   :=y - Word.RightShift(x,i);
      x   :=xtmp;
      DEC(z,AtanTbl[i]);
    END;
  END;

  (*---resolve quadrants---*)
  CASE quadrant OF
  | 1 => c:= x; s:= y;
  | 2 => c:=-x; s:= y;
  | 3 => c:=-x; s:=-y;
  | 4 => c:= x; s:=-y;
  END;
END sin_cos;
 
(*==========================*)
BEGIN
END xInteger.
