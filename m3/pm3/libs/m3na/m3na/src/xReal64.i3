INTERFACE xReal64;
(*The interface is Public Domain.
  The supporting implementations are copyrighted,
  but may be used free of charge so long as
  appropriate credit is given.  

  WARNING:  USE AT YOUR OWN RISK.
  The authors accept no responsibility for
  the accuracy, appropriateness or fitness for
  use of any of this material.

Abstract:  This is a Modula-3 rendition of a collection
           of numerical analysis routines.         

12/13/95   Harry George      Initial version
1/22/96    Harry George      Change to m3na project
2/17/96    Harry George      Convert to separate Real* modules
*)
FROM xUtils IMPORT Error;
IMPORT Math,Fmt;

(*=================*)
TYPE
  REAL64 = LONGREAL;  (*IEEE 64-bit real*)
  T      = REAL64;    (*for use as R.T*)
  Ftn    = PROCEDURE(x:REAL64):REAL64;
  
CONST
  (*---distinguished elements---*)
  Zero      = 0.0D0;
  Half      = 0.5D0;
  One       = 1.0D0;
  Two       = 2.0D0;
  SqrtTwo   = 1.414213562373095D0;
  LnTwo     = 0.693147180559945D0;  (*ln(2) *)
   
  Pi        = 3.141592653589793D0;
  TwoPi     = 6.283185307179586D0;
  OneOverPi = 0.318309886183791D0;
  TwoOverPi = 0.636619772367581D0;
  FourOverPi= 1.273239544735163D0;
  PiOverTwo = 1.570796326794897D0;
  PiOverThree=1.047197551196598D0;
  PiOverFour= 0.785398163397448D0;
  PiOverSix = 0.523598775598299D0;

  LnPi      = 1.144729885849400D0;  (*ln(pi) *)
  
  E         = 2.718281828459045D0;  (*natural log base "e"*)
  EulersConst=0.577215664901532D0;  (*Euler's constant "gamma"*)
  Golden    = 1.618033988749894D0;  (*golden ratio*)
  DegPerRad = 57.29577951308232D0;  (*degrees per radian*)
  RadPerDeg = 0.017453292519943D0;  (*radians per degree*)
 
  (*---boundaries for precision testing---*)
  TINY= 1.0D-300; (*nearly 0.0*)
  HUGE= 1.0D+300; (*nearly infinite*)
  EPS = 1.0D-17;  (*approx relative machine precision*)  

(*============================*)
(* Handy collectors           *)
(*============================*)
TYPE Array = REF ARRAY OF REAL64;
  
(*============================*)
(* Functions                  *)
(*============================*)
CONST fmt = Fmt.LongReal;

(*---- Exponential and Logarithm functions ----*)
CONST exp = Math.exp ;
(*returns e^x*)

(*????????????
CONST expm1 = Math.expm1 ;
(*returns e^(x-1) *)
????????*)

CONST log = Math.log ;
(*returns ln(x) *)

CONST log10 = Math.log10 ;
(*returns log10(x) *)

(*???????????????
CONST log1p = Math.log1p ;
(*returns ln(1+x) *)
?????????????*)

CONST pow =Math.pow;
(*returns x^y *)

CONST sqrt = Math.sqrt ;
(*returns square root of x*)

(*---- Trigonometric functions ----*)
CONST cos = Math.cos ;
(*returns the cosine of x radians. *)

CONST sin = Math.sin ;
(*returns the sine of x radians. *)

CONST tan = Math.tan ;
(*returns the tangent of x radians. *)

CONST acos = Math.acos ;
(*returns the arc cosine of x in radians. *)

CONST asin = Math.asin ;
(*returns the arc sine of x in radians. *)

CONST atan = Math.atan ;
(*returns the arc tangent of x in radians. *)

CONST atan2 = Math.atan2;
(*returns the arc tangent of y/x in radians. *)


(*---- Hyperbolic trigonometric functions ----*)

CONST cosh = Math.cosh ;
(*returns the hyperbolic cosine of x. *)

CONST sinh = Math.sinh ;
(*returns the hyperbolic sine of x. *)

CONST tanh = Math.tanh ;
(*returns the hyperbolic tangent of x. *)

(*????????????????????
CONST acosh = Math.acosh ;
(*returns the inverse hyperbolic cosine of x *)

CONST asinh = Math.asinh ;
(*returns the inverse hyperbolic sine of x *)

CONST atanh = Math.atanh ;
(*returns the inverse hyperbolic tangent of x *)
?????????*)


(*============================*)
(* Other Functions            *)
(*============================*)
PROCEDURE sgn(a:REAL64):REAL64;
(*IF a >=0.0 THEN RETURN 1.0 ELSE RETURN -1.0; END:*)

PROCEDURE factorial(n:CARDINAL):REAL64;
(*return n! as a real*)

PROCEDURE ln_factorial(n:CARDINAL):REAL64;
(*returns ln(n!) as a real*)

PROCEDURE gamma(z:REAL64):REAL64;
(*returns gamma(z)*)

PROCEDURE ln_gamma(z:REAL64):REAL64;
(*returns ln(gamma(z))*)


PROCEDURE binomial(n,k:CARDINAL):REAL64 RAISES {Error};
(*returns binomial coefficient for "n over k"*)

PROCEDURE gamma_p(a,x:REAL64):REAL64 RAISES {Error};
(*returns incomplete gamma P(a,x)=gamma(a,x)/Gamma(a)*)

PROCEDURE gamma_q(a,x:REAL64):REAL64 RAISES {Error};
(*returns incomplete gamma Q(a,x)=Gamma(a,x)/Gamma(a)*)
(*also, Q(a,x)=1-P(a,x) *)

(*Notes for in-lines:
|1. Cumulative Poisson Probability:
|   Px(<k)=probability that the number of events will be
|   between 0 and k-1 inclusive, given mean=x.
|     Px(<k)=gamma_q(k,x)
|2. Chi-Square Probability:
|   P(X2|df)=probability that observed chi-square should be
|   less than X2, given df degrees of freedom. 
|     P(X2|df)=gamma_p(df/2.0,X2/2.0); P(0|df)=0, P(inf|df)=1
|   Complementary form:
|     Q(X2|df)=gamma_q(df/2.0,X2/2.0); Q(0|df)=1, Q(inf|df)=0
*)

PROCEDURE erf(x:REAL64):REAL64 RAISES {Error};
(*returns error function of x*)

PROCEDURE erfc(x:REAL64):REAL64 RAISES {Error};
(*returns 1-erf(x) *)

PROCEDURE beta(z,w:REAL64):REAL64;
(*returns gamma(z)*gamma(w)/gamma(z+w)*)

PROCEDURE betai(a,b,x:REAL64):REAL64 RAISES {Error};
(*returns incomplete beta Ix(a,b) *)
(*Notes for in-lines:
|1. Student's t-test distribution for df degrees of freedom is
|     A(t|df) = 1.0-betai(df/2,1/2,df/(df+t^2))
|   In other words, big A means t should probably be smaller
|2. F-test distribution for df1 and df2 degrees of freedom is
|     A(F|df1,df2) = betai(df1/2,df2/2,df2/(df2+df1*F))
|    
|3. Cumulative binomial probability for event which has
|   probability p of occurring in each trial,
|   having the event occur k or moe times in n trials is
|     P(= betai(k,n-k+1,p)
*)


(*========================*)
END xReal64.
