(* Copyright (C) 1989, 1992, Digital Equipment Corporation    *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Thu Feb 10 14:03:56 PST 1994 by kalsow    *)
(*      modified on Wed May 19 13:07:16 PDT 1993 by muller    *)
(*      modified on Wed Jan 27 10:46:09 PST 1993 by burrows   *)
(*      modified on Sat Aug 12 15:13:23 1989 by ellis         *)

UNSAFE MODULE Poly;

(* This module provides polynomials of degree 64 with
   coefficients in the field Z(2).

   The coefficients of a polynominal are in reverse (VAX) order:
      P(x) = x^64 + c[0]*x^63 + c[1]*x^62 + c[2]*x^61 + ... + c[62]*x + c[63]

   The leading coefficient is not stored in the 64 bit
   representation of the basis polynomial.  All other
   polynomials are residues MOD the basis, hence they
   don't have an x^64 term.

   Here's the scoop:
     Most Significant -- Least Significant
        c[0]         ....   c[63]
         b0          ....    b7
         i0          ....    i1
*)

IMPORT Word;

FROM PolyBasis IMPORT
  poly64, poly72, poly80, poly88, power, power8, power16, power24;

CONST SigBits = 16_ffffffff; (* mask to grab 32 significant bits *)

TYPE IntBytes   = RECORD b0, b1, b2, b3: Byte END;
TYPE IntPtr     = UNTRACED REF Int32;
TYPE DoublePoly = ARRAY [0..3] OF Int32;

VAR init_done     := FALSE;
VAR little_endian := FALSE;
VAR big_endian    := FALSE;

PROCEDURE Sum (READONLY p, q: T) : T =
  VAR r : T;
  BEGIN
    r[0] := Word.Xor (p[0], q[0]);
    r[1] := Word.Xor (p[1], q[1]);
    RETURN r;
  END Sum;

PROCEDURE Product (READONLY p, q: T) : T =
  (* returns (p * q MOD PolyBasis.P) *)
  VAR
    temp, accum : DoublePoly;  z: Word.T;
  BEGIN
    (* zero the temporaries *)
    temp[0]:=0;  temp[1]:=0;  temp[2]:=0;  temp[3]:=0;
    accum := temp;

    (* form the 128 bit product in accum *)
    temp[2] := p[0]; temp[3] := p[1];
    FOR j := 1 TO 0 BY -1 DO
      z := q[j];
      FOR i := 31 TO 0 BY -1 DO
        IF Word.And (z, Word.LeftShift (1, i)) # 0 THEN
	  DoubleINC (accum, temp);
        END;
        DoubleTimesX (temp);
      END;
    END;

    (* collapse and return the accumulated result MOD P *)
    RETURN  ComputeMod (ZERO, ADR (accum), BYTESIZE (accum));
  END Product;

PROCEDURE ComputeMod (READONLY t: T;  addr: ADDRESS;  len: INTEGER): T =
(* This procedure assumes that the 'len' bytes beginning at address
   'addr' define a polynomial, A(x), of degree '(8*len)'.
   The procedure returns '(init*x^(8*len) + A(x)) MOD PolyBasis.P' *)
  VAR j, k: INTEGER;  result := t;
  BEGIN
    IF (NOT init_done) THEN FindByteOrder () END;

    (* word align the source pointer *)
    j := LOOPHOLE (addr, INTEGER) MOD 4;
    IF (len >= 4) AND (j # 0) THEN
      j := 4 - j;
      result := ExtendBytes (result, addr, j);
      INC (addr, j);
      DEC (len, j);
    END;

    (* compute the bulk of the result a word at a time *)
    IF (len >= 4) THEN
      j := len MOD 4;
      k := len - j;
      IF (little_endian)
        THEN result := ExtendWords_LE (result, addr, k);
        ELSE result := ExtendWords_BE (result, addr, k);
      END;
      INC (addr, k);
      len := j;
    END;

    (* finish up the last few bytes *)
    IF (len > 0) THEN
      result := ExtendBytes (result, addr, len);
    END;

    RETURN result;
  END ComputeMod;

PROCEDURE Fix32 (x: Word.T): Int32 =
  (* return the sign-extended bottom 32 bits of 'x' *)
  CONST
    Sign = 16_80000000;
    SignExtend = Word.LeftShift (Word.Not (0), 31);
  BEGIN
    IF Word.And (x, Sign) = 0
      THEN RETURN Word.And (x, SigBits);
      ELSE RETURN Word.Or (SignExtend, Word.And (x, SigBits));
    END;
  END Fix32;

PROCEDURE ExtendBytes (READONLY t: T;  addr: ADDRESS;  len: [1..3]): T =
  VAR
    cp     : UNTRACED REF ARRAY [0..3] OF Byte := addr;
    n_bits : [8..24] := 8 * len;
    x_bits : [8..24] := 32 - n_bits;
    t0     : INTEGER := Word.And (t[0], SigBits);
    t0_x   : INTEGER := Word.LeftShift  (t0, x_bits);
    t0_n   : INTEGER := Word.RightShift (t0, n_bits);
    t1     : INTEGER := Word.And (t[1], SigBits);
    t1_x   : INTEGER := Word.LeftShift  (t1, x_bits);
    t1_n   : INTEGER := Word.RightShift (t1, n_bits);
    tmp    : RECORD i: INTEGER;  b: ARRAY [0..3] OF Byte; END;
    result : T;
  BEGIN
    result[0] := Fix32 (t0_x);
    result[1] := Fix32 (Word.Xor (t0_n, t1_x));
    CASE len OF
    | 1 =>
           tmp.b[0] := Word.Extract (t1_n,  0, 8);
           tmp.b[1] := Word.Extract (t1_n,  8, 8);
           tmp.b[2] := Word.Extract (t1_n, 16, 8);
           tmp.b[3] := cp[0];
    | 2 =>
           tmp.b[0] := Word.Extract (t1_n,  0, 8);
           tmp.b[1] := Word.Extract (t1_n,  8, 8);
           tmp.b[2] := cp[0];
           tmp.b[3] := cp[1];
    | 3 =>
           tmp.b[0] := Word.Extract (t1_n,  0, 8);
           tmp.b[1] := cp[0];
           tmp.b[2] := cp[1];
           tmp.b[3] := cp[2];
    END;
    IF (little_endian)
      THEN RETURN ExtendWords_LE (result, ADR (tmp.b[0]), BYTESIZE (tmp.b));
      ELSE RETURN ExtendWords_BE (result, ADR (tmp.b[0]), BYTESIZE (tmp.b));
    END;
  END ExtendBytes;

PROCEDURE ExtendWords_LE (READONLY p: T;  source: ADDRESS;  len: INTEGER): T =
  VAR
    ip  : IntPtr := source;
    tmp : IntBytes;
    p0  : INTEGER := p[0];
    p1  : INTEGER := p[1];
  BEGIN
    WHILE (len > 0) DO
      (* split the low-order bytes *)
      LOOPHOLE (tmp, Int32) := p0;

      (* compute the new result *)
      p0 := Word.Xor (p1,  Word.Xor (Word.Xor (poly88[tmp.b0][0],
                                               poly80[tmp.b1][0]),
                                     Word.Xor (poly72[tmp.b2][0],
                                               poly64[tmp.b3][0])));
      p1 := Word.Xor (ip^, Word.Xor (Word.Xor (poly88[tmp.b0][1],
                                               poly80[tmp.b1][1]),
                                     Word.Xor (poly72[tmp.b2][1],
                                               poly64[tmp.b3][1])));
      DEC (len, BYTESIZE (Int32));
      INC (ip, ADRSIZE (ip^));
    END;
    RETURN T {p0, p1};
  END ExtendWords_LE;

PROCEDURE ExtendWords_BE (READONLY p: T;  source: ADDRESS;  len: INTEGER): T =
  VAR
    ip  : IntPtr := source;
    tmp : IntBytes;
    p0  := p[0];
    p1  := p[1];
    x, y: Int32;
  BEGIN
    WHILE (len > 0) DO
      (* byte swap the input word -- inline *)
      y := ip^;
      LOOPHOLE (x, IntBytes).b0 := LOOPHOLE (y, IntBytes).b3;
      LOOPHOLE (x, IntBytes).b1 := LOOPHOLE (y, IntBytes).b2;
      LOOPHOLE (x, IntBytes).b2 := LOOPHOLE (y, IntBytes).b1;
      LOOPHOLE (x, IntBytes).b3 := LOOPHOLE (y, IntBytes).b0;

      (* split the low-order bytes *)
      LOOPHOLE (tmp, Int32) := p0;

      (* compute the new result *)
      p0 := Word.Xor (p1, Word.Xor (Word.Xor (poly88[tmp.b3][0],
                                              poly80[tmp.b2][0]),
                                    Word.Xor (poly72[tmp.b1][0],
                                              poly64[tmp.b0][0])));
      p1 := Word.Xor (x,  Word.Xor (Word.Xor (poly88[tmp.b3][1],
                                              poly80[tmp.b2][1]),
                                    Word.Xor (poly72[tmp.b1][1],
                                              poly64[tmp.b0][1])));
      DEC (len, BYTESIZE (Int32));
      INC (ip, ADRSIZE (ip^));
    END;
    RETURN T {p0, p1};
  END ExtendWords_BE;

PROCEDURE Power (d: Card32) : T =
  (* returns x^d MOD PolyBasis.P *)
  VAR i : [0..15];  b0, b1, b2, b3: Byte;
  BEGIN
    IF (NOT init_done) THEN FindByteOrder () END;
    IF (little_endian) THEN
      b0 := LOOPHOLE (d, IntBytes).b0;
      b1 := LOOPHOLE (d, IntBytes).b1;
      b2 := LOOPHOLE (d, IntBytes).b2;
      b3 := LOOPHOLE (d, IntBytes).b3;
    ELSE
      b3 := LOOPHOLE (d, IntBytes).b0;
      b2 := LOOPHOLE (d, IntBytes).b1;
      b1 := LOOPHOLE (d, IntBytes).b2;
      b0 := LOOPHOLE (d, IntBytes).b3;
    END;

    (* find the non-zero bytes of 'd' *)
    i := 0;
    IF (b0 # 0) THEN i := 1; END;
    IF (b1 # 0) THEN INC (i,2); END;
    IF (b2 # 0) THEN INC (i,4); END;
    IF (b3 # 0) THEN INC (i,8); END;

    CASE i OF
    | 0  => RETURN ONE;
    | 1  => RETURN power [b0]
    | 2  => RETURN power8 [b1];
    | 3  => RETURN Product (power8 [b1], power [b0]);
    | 4  => RETURN power16 [b2];
    | 5  => RETURN Product (power16 [b2], power [b0]);
    | 6  => RETURN Product (power16 [b2], power8 [b1]);
    | 7  => RETURN Product (power16 [b2], Product (power8 [b1], power [b0]));
    | 8  => RETURN power24 [b3]
    | 9  => RETURN Product (power24 [b3], power [b0]);
    | 10 => RETURN Product (power24 [b3], power8 [b1]);
    | 11 => RETURN Product (power24 [b3], Product (power8 [b1], power [b0]));
    | 12 => RETURN Product (power24 [b3], power16 [b2]);
    | 13 => RETURN Product (power24 [b3], Product (power16 [b2], power [b0]));
    | 14 => RETURN Product (power24 [b3], Product(power16 [b2], power8 [b1]));
    | 15 => RETURN Product (Product (power24 [b3], power16 [b2]),
                            Product (power8 [b1], power [b0]));
    END;
  END Power;

PROCEDURE TimesX (READONLY p : T) : T =
  (* return (p * X^1) *)
  VAR result : T;
  BEGIN
    result[0] := Word.RightShift (p[0], 1);
    IF Word.And (p[1], 1) # 0 THEN
      result[0] := Word.Or (result[0], FIRST (Int32))
    END;
    result[1] := Word.RightShift (p[1], 1);
    RETURN result;
  END TimesX;

PROCEDURE DoubleINC (VAR a,b : DoublePoly) =
  (*  a := a + b *)
  BEGIN
    a[0] := Word.Xor (a[0], b[0]);
    a[1] := Word.Xor (a[1], b[1]);
    a[2] := Word.Xor (a[2], b[2]);
    a[3] := Word.Xor (a[3], b[3]);
  END DoubleINC;

PROCEDURE DoubleTimesX (VAR a : DoublePoly) =
  (* a := a*x  *)
  BEGIN
    a[0] := Word.RightShift (a[0], 1); 
    IF Word.And (a[1], 1) # 0 THEN a[0] := Word.Or (a[0], FIRST (Int32)); END;
    a[1] := Word.RightShift (a[1], 1);    
    IF Word.And (a[2], 1) # 0 THEN a[1] := Word.Or (a[1], FIRST (Int32)); END;
    a[2] := Word.RightShift (a[2], 1); 
    IF Word.And (a[3], 1) # 0 THEN a[2] := Word.Or (a[2], FIRST (Int32)); END;
    a[3] := Word.RightShift (a[3], 1);
  END DoubleTimesX;

(******** == Word.LeftShift 
PROCEDURE LSL (a, cnt : INTEGER) : INTEGER =
  (* return 'a' shifted 'cnt' bits to the left, zero filling from the right *)
  BEGIN
    RETURN Word.Shift (a, cnt);
  END LSL;
******)

(******* == Word.RightShift (a,cnt) = Word.Shift(a,-cnt) = LSR(a,cnt)
PROCEDURE LSR (a, cnt : INTEGER) : INTEGER =
  (* return 'a' shifted 'cnt' bits to the right, zero filling from the left *)
  BEGIN
    RETURN Word.Shift (a, -cnt);
  END LSR;
*******)

(********
PROCEDURE ByteSwap (x: INTEGER): INTEGER =
  VAR z: INTEGER;
  BEGIN
    LOOPHOLE (z, IntBytes).b0 := LOOPHOLE (x, IntBytes).b3;
    LOOPHOLE (z, IntBytes).b1 := LOOPHOLE (x, IntBytes).b2;
    LOOPHOLE (z, IntBytes).b2 := LOOPHOLE (x, IntBytes).b1;
    LOOPHOLE (z, IntBytes).b3 := LOOPHOLE (x, IntBytes).b0;
    RETURN z;
  END ByteSwap;
*********)

PROCEDURE FindByteOrder () =
  VAR 
    i  : Int32 := 16_12345678;
    x  := LOOPHOLE (i, IntBytes);
    a0 := Word.Extract (i,  0, 8);
    a1 := Word.Extract (i,  8, 8);
    a2 := Word.Extract (i, 16, 8);
    a3 := Word.Extract (i, 24, 8);
  BEGIN
    IF (a0 = x.b0) AND (a1 = x.b1) AND (a2 = x.b2) AND (a3 = x.b3) THEN
      little_endian := TRUE;
    ELSIF (a0 = x.b3) AND (a1 = x.b2) AND (a2 = x.b1) AND (a3 = x.b0) THEN
      big_endian := TRUE;
    ELSE (* unsupported byte ordering ... *)
      (* Process.Crash ("Poly.FindByteOrder: unknown byte order"); *)
      <*ASSERT FALSE*>
    END;
    init_done := TRUE;
  END FindByteOrder;

PROCEDURE ToBytes (READONLY t: T;  VAR b: Bytes) =
  (* generate the bytes in little-endian order *)
  BEGIN
    b[0] := Word.Extract (t[0],  0, 8);
    b[1] := Word.Extract (t[0],  8, 8);
    b[2] := Word.Extract (t[0], 16, 8);
    b[3] := Word.Extract (t[0], 24, 8);
    b[4] := Word.Extract (t[1],  0, 8);
    b[5] := Word.Extract (t[1],  8, 8);
    b[6] := Word.Extract (t[1], 16, 8);
    b[7] := Word.Extract (t[1], 24, 8);
  END ToBytes;

PROCEDURE FromBytes (READONLY b: Bytes;  VAR t: T) =
  (* assume the bytes are in little-endian order *)
  BEGIN
    t[0] := Fix32 (Word.Or (Word.Or (                b[0],
                                     Word.LeftShift (b[1], 8)),
                            Word.Or (Word.LeftShift (b[2], 16),
                                     Word.LeftShift (b[3], 24))));
    t[1] := Fix32 (Word.Or (Word.Or (                b[4],
                                     Word.LeftShift (b[5], 8)),
                            Word.Or (Word.LeftShift (b[6], 16),
                                     Word.LeftShift (b[7], 24))));
  END FromBytes;

BEGIN
  <* ASSERT BITSIZE (Int32) = 32 AND BITSIZE (CHAR) = 8 *>
END Poly.

