INTERFACE xReal32;
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

IMPORT Fmt;

(*=================*)
TYPE
  REAL32 = REAL;    (*IEEE 32-bit real*)
  T      = REAL32;  (*for use as R.T*)
  Ftn    = PROCEDURE(x:REAL32):REAL32;
  
CONST
  (*---distinguished elements---*)
  Zero      = 0.0E0;
  Half      = 0.5E0;
  One       = 1.0E0;
  Two       = 2.0E0;
  SqrtTwo   = 1.414213562373095E0;
  LnTwo     = 0.693147180559945E0;  (*ln(2) *)
   
  Pi        = 3.141592653589793E0;
  TwoPi     = 6.283185307179586E0;
  OneOverPi = 0.318309886183791E0;
  TwoOverPi = 0.636619772367581E0;
  FourOverPi= 1.273239544735163E0;
  LnPi      = 1.144729885849400E0;  (*ln(pi) *)
  
  E         = 2.718281828459045E0;  (*natural log base "e"*)
  EulersConst=0.577215632901532E0;  (*Euler's constant "gamma"*)
  Golden    = 1.618033988749894E0;  (*golden ratio*)
  DegPerRad = 57.29577951308232E0;  (*degrees per radian*)
  RadPerDeg = 0.017453292519943E0;  (*radians per degree*)
 
  (*---boundaries for precision testing---*)
  TINY= 1.0E-34; (*nearly 0.0*)
  HUGE= 1.0E+34; (*nearly infinite*)
  EPS = 1.0E-7;  (*approx relative machine precision*)  

(*============================*)
(* Handy collectors           *)
(*============================*)
TYPE Array = REF ARRAY OF REAL32;
  
(*============================*)
(* Functions                  *)
(*============================*)
CONST fmt = Fmt.Real;

(*---- Exponential and Logarithm functions ----*)
PROCEDURE exp (x: REAL32): REAL32;
(*returns e^x*)

(*????????????
PROCEDURE expm1 (x: REAL32): REAL32;
(*returns e^(x-1) *)
????????*)

PROCEDURE log (x: REAL32): REAL32;
(*returns ln(x) *)

PROCEDURE log10 (x: REAL32): REAL32;
(*returns log10(x) *)

(*???????????????
PROCEDURE log1p (x: REAL32): REAL32;
(*returns ln(1+x) *)
?????????????*)

PROCEDURE pow (x, y: REAL32): REAL32;
(*returns x^y *)

PROCEDURE sqrt (x: REAL32): REAL32;
(*returns square root of x*)

(*---- Trigonometric functions ----*)
PROCEDURE cos (x: REAL32): REAL32;
(*returns the cosine of x radians. *)

PROCEDURE sin (x: REAL32): REAL32;
(*returns the sine of x radians. *)

PROCEDURE tan (x: REAL32): REAL32;
(*returns the tangent of x radians. *)

PROCEDURE acos (x: REAL32): REAL32;
(*returns the arc cosine of x in radians. *)

PROCEDURE asin (x: REAL32): REAL32;
(*returns the arc sine of x in radians. *)

PROCEDURE atan (x: REAL32): REAL32;
(*returns the arc tangent of x in radians. *)

PROCEDURE atan2 (y, x: REAL32): REAL32;
(*returns the arc tangent of y/x in radians. *)


(*---- Hyperbolic trigonometric functions ----*)

PROCEDURE cosh (x: REAL32): REAL32;
(*returns the hyperbolic cosine of x. *)

PROCEDURE sinh (x: REAL32): REAL32;
(*returns the hyperbolic sine of x. *)

PROCEDURE tanh (x: REAL32): REAL32;
(*returns the hyperbolic tangent of x. *)

(*????????????????????
PROCEDURE acosh (x: REAL32): REAL32;
(*returns the inverse hyperbolic cosine of x *)

PROCEDURE asinh (x: REAL32): REAL32;
(*returns the inverse hyperbolic sine of x *)

PROCEDURE atanh (x: REAL32): REAL32;
(*returns the inverse hyperbolic tangent of x *)
?????????*)


(*============================*)
(* Other Functions            *)
(*============================*)

(*========================*)
END xReal32.
