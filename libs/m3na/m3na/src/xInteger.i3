INTERFACE xInteger;
(*Copyright (c) 1996, m3na project
  
Abstract: Bits and Bytes

2/17/96  Harry George    Initial version
*)

IMPORT Fmt,Word;

(*==========================*)
TYPE
  Card8  = BITS  8 FOR [0..16_FF];
  Card16 = BITS 16 FOR [0..16_FFFF];
  Card32 = Word.T;  (*use Word's operators*)

  Int8   = BITS  8 FOR [-16_80..16_7F];
  Int16  = BITS 16 FOR [-16_8000..16_7FFF];
  Int32  = BITS 32 FOR [FIRST(INTEGER)..LAST(INTEGER)];

(*============================*)
(* Handy collectors           *)
(*============================*)
TYPE Array = REF ARRAY OF INTEGER;


(*============================*)
(* Functions                  *)
(*============================*)  
CONST fmt = Fmt.Int;

PROCEDURE fmtArray(READONLY a:Array;
                   base      :CARDINAL:=10;
                   cellwidth :CARDINAL:=4;
                   linewidth :CARDINAL:=60):TEXT;



(*============================*)
(* Factoring                  *)
(*============================*)  
(*-----------------*)
PROCEDURE isprime(n:CARDINAL):BOOLEAN;
(*is this number a prime number?*)

PROCEDURE factor(n:CARDINAL;   (*factor this number*)
                 VAR p,m:Array (*giving primes and multiplicity*)
                 ):CARDINAL;   (*and count of factors*)
(*e.g., factor(24) gives 2^3 * 3^1 or:
   p=[2,3]
   m=[3,1]
   return=2;
p and m are created by the procedure.
*)

PROCEDURE gcd(u,v:CARDINAL):CARDINAL;
(*returns the greatest common denominator for u and v.*)

(*============================*)
(* Integer Approximations     *)
(*============================*)  

PROCEDURE sqrt(N:[0..1073741823]):CARDINAL;
(*returns integer sqrt of N.*)

(*============================*)
(* CORDIC Functions           *)
(*============================*)  
CONST
  CordicBits = 16;
  CordicBase = 65536;  (*2^CordicBits*)
  CordicHalf = CordicBase DIV 2;
  RadToCordic= 4.172151340188181D+4; (*0..+pi/2-->cordicbase*)
  CordicToReal=1.52587890625D-5;      (*cordicbase -->0..1*)

TYPE
  Cordic= [0..CordicBase*4];

PROCEDURE sin_cos(theta:Cordic;     (*given this angle*)   
                  VAR s,c:INTEGER); (*return sin and cos*)
(*E.g.:
  theta:=ROUND(theta_in_radians*RadToCordic);
  sin_cos(theta:=theta,s:=s,c:=c);
  sin_in_radians:=FLOAT(s,REAL64)*UnitPerCordic;
  cos_in_radians:=FLOAT(c,REAL64)*UnitPerCordic;
Of course, in real life you wouldn't be moving in and out
of floating point.  theta would be computed in cordics to begin with.
Thus 100*sin(theta) is obtained by:
  sin_cos(theta:=theta,s:=s,c:=c);
  answer:=Word.RightShift(100*s + CordicHalf),CordicBits); 
*) 
(*==========================*)
END xInteger.
