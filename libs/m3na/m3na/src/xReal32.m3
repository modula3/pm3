MODULE xReal32;
(*
Abstract: This is a 32-bit rendition of the basic
          functions in Math.
          Initially implemented a wrappers on Math, but
          need an assembler rendition.

12/15/95  Harry George   Initial version, via wrappers on Math
*)

IMPORT Math;
FROM xReal64 IMPORT REAL64;
  
(*---- Exponential and Logarithm functions ----*)
(*----------------*)
PROCEDURE exp (x: REAL32): REAL32=
(* returns E^x. *)
BEGIN
  RETURN FLOAT(Math.exp(FLOAT(x,REAL64)),REAL32);
END exp;

(*----------------*)
(* linker can't find it
PROCEDURE expm1 (x: REAL32): REAL32=
(* returns (E^x)-1, even for small x. *)
BEGIN
  RETURN FLOAT(Math.expm1(FLOAT(x,REAL64)),REAL32);
END expm1;
*)
(*----------------*)
PROCEDURE log (x: REAL32): REAL32=
(* returns the natural logarithm of x (base E). *)
BEGIN
  RETURN FLOAT(Math.log(FLOAT(x,REAL64)),REAL32);
END log;

(*----------------*)
PROCEDURE log10 (x: REAL32): REAL32=
(* returns the base 10 logarithm of x. *)
BEGIN
  RETURN FLOAT(Math.log10(FLOAT(x,REAL64)),REAL32);
END log10;

(*----------------*)
(* linker can't find it
PROCEDURE log1p (x: REAL32): REAL32=
(* returns log(1+x), even for small x. *)
BEGIN
  RETURN FLOAT(Math.log1p(FLOAT(x,REAL64)),REAL32);
END log1p;
*)

(*----------------*)
PROCEDURE pow (x, y: REAL32): REAL32=
(* returns x^y. *)
BEGIN
  RETURN FLOAT(Math.pow(FLOAT(x,REAL64),
                        FLOAT(y,REAL64)),REAL32);
END pow;

(*----------------*)
PROCEDURE sqrt (x: REAL32): REAL32=
(* returns the square root of x. *)
BEGIN
  RETURN FLOAT(Math.sqrt(FLOAT(x,REAL64)),REAL32);
END sqrt;


(*---- Trigonometric functions ----*)

(*----------------*)
PROCEDURE cos (x: REAL32): REAL32=
(* returns the cosine of x radians. *)
BEGIN
  RETURN FLOAT(Math.cos(FLOAT(x,REAL64)),REAL32);
END cos;

(*----------------*)
PROCEDURE sin (x: REAL32): REAL32=
(* returns the sine of x radians. *)
BEGIN
  RETURN FLOAT(Math.sin(FLOAT(x,REAL64)),REAL32);
END sin;

(*----------------*)
PROCEDURE tan (x: REAL32): REAL32=
(* returns the tangent of x radians. *)
BEGIN
  RETURN FLOAT(Math.tan(FLOAT(x,REAL64)),REAL32);
END tan;

(*----------------*)
PROCEDURE acos (x: REAL32): REAL32=
(* returns the arc cosine of x in radians. *)
BEGIN
  RETURN FLOAT(Math.acos(FLOAT(x,REAL64)),REAL32);
END acos;

(*----------------*)
PROCEDURE asin (x: REAL32): REAL32=
(* returns the arc sine of x in radians. *)
BEGIN
  RETURN FLOAT(Math.asin(FLOAT(x,REAL64)),REAL32);
END asin;

(*----------------*)
PROCEDURE atan (x: REAL32): REAL32=
(* returns the arc tangent of x in radians. *)
BEGIN
  RETURN FLOAT(Math.atan(FLOAT(x,REAL64)),REAL32);
END atan;

(*----------------*)
PROCEDURE atan2 (y, x: REAL32): REAL32=
(* returns the arc tangent of y/x in radians. *)
BEGIN
  RETURN FLOAT(Math.atan2(FLOAT(y,REAL64),
                          FLOAT(x,REAL64)),REAL32);
END atan2;


(*---- Hyperbolic trigonometric functions ----*)

(*----------------*)
PROCEDURE cosh (x: REAL32): REAL32=
(* returns the hyperbolic cosine of x. *)
BEGIN
  RETURN FLOAT(Math.cosh(FLOAT(x,REAL64)),REAL32);
END cosh;

(*----------------*)
PROCEDURE sinh (x: REAL32): REAL32=
(* returns the hyperbolic sine of x. *)
BEGIN
  RETURN FLOAT(Math.sinh(FLOAT(x,REAL64)),REAL32);
END sinh;

(*----------------*)
PROCEDURE tanh (x: REAL32): REAL32=
(* returns the hyperbolic tangent of x. *)
BEGIN
  RETURN FLOAT(Math.tanh(FLOAT(x,REAL64)),REAL32);
END tanh;


(*---linker can't find them----
(*----------------*)
PROCEDURE acosh (x: REAL32): REAL32=
(* returns the inverse hyperbolic cosine of x *)
BEGIN
  RETURN FLOAT(Math.acosh(FLOAT(x,REAL64)),REAL32);
END acosh;

(*----------------*)
PROCEDURE asinh (x: REAL32): REAL32=
(* returns the inverse hyperbolic sine of x *)
BEGIN
  RETURN FLOAT(Math.asinh(FLOAT(x,REAL64)),REAL32);
END asinh;

(*----------------*)
PROCEDURE atanh (x: REAL32): REAL32=
(* returns the inverse hyperbolic tangent of x *)
BEGIN
  RETURN FLOAT(Math.atanh(FLOAT(x,REAL64)),REAL32);
END atanh;

*)

(*============================*)
BEGIN
END xReal32.


