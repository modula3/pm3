MODULE xWordEx;
(*Copyright (c) 1996, m3na project
  
Abstract: Extensions to Word module

3/16/96  Warren Smith   Initial version
*)
IMPORT Word;

CONST Module = "xWordEx.";
(*==========================*)
TYPE
  Byte = [0 .. 255];

CONST
  HalfLength = Word.Size DIV 2; 
  LeastSignifHalfMask = Word.Minus(Word.LeftShift(1, HalfLength), 1);

  (** We assume Word.Size is divisible by 8, as it is on every machine
   * I've ever heard of. That is anyway enforced by an ASSERT at bottom. *)

BytePopCount = ARRAY Byte OF [0..8]{
0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8 };

IndexOfLSBit = ARRAY Byte OF [0..8]{
8, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0 };

IndexOfMSBit = ARRAY Byte OF [-1 .. 7]{
-1, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7 };


(*---------------------------------------*)
(** A workaround of the absence of this primitive in the Modula-3
 * Word library (as well as many high level languages). *)

PROCEDURE PlusWithCarry(VALUE x,y : Word.T; VAR carry: BOOLEAN) : Word.T =
  VAR
    sum : Word.T;
  BEGIN
    IF carry THEN
      x := Word.Plus(x, 1);
      carry := (x=0);
      (*ELSE carry := FALSE; is not needed since happens automatically *)
    END;
    IF Word.GT(x,y) THEN (* swap x,y if nec. so that we have x<=y *)
      WITH t=x DO
        x := y; y := t;
      END;
    END;
    sum := Word.Plus(x,y);
    carry := carry OR Word.LT(sum, y);
    RETURN sum;
  END PlusWithCarry;

(*-------------------------------------------*)
(** A workaround of the absence of this primitive in the Modula-3
 * Word library (as well as many high level languages). *)

PROCEDURE MinusWithBorrow(VALUE x,y : Word.T; VAR borrow: BOOLEAN) : Word.T =
  VAR
    diff : Word.T;
  BEGIN
    IF borrow THEN
      x := Word.Plus(x, 1);
      borrow := (x=0);
      (*ELSE borrow := FALSE; is not needed since happens automatically *)
    END;
    diff := Word.Minus(x,y);
    borrow := borrow OR Word.GT(diff, x);
    RETURN diff;
  END MinusWithBorrow;

(*---------------------------------------------*)
(** A workaround of the absence of this primitive in the Modula-3
 * Word library (as well as many high level languages). Perhaps
 * also a version LeftShiftWithCarry, specializing to sh=1, should
 * be provided, to allow extra closeness to the hardware. *)

PROCEDURE LeftShiftWithProbscosis(VALUE x : Word.T;
                                  VALUE sh : CARDINAL;
                                  VAR probscosis : Word.T) : Word.T =
  VAR
    y : Word.T;
  BEGIN
    <* ASSERT sh<Word.Size *>
    y := Word.LeftShift(x, sh);
    y := Word.Or(y, probscosis);
    probscosis := Word.RightShift(x, Word.Size - sh);
    (** Now do not need to mask off sh low-order bits 
     * to get final probscosis: 
     *    probscosis := Word.And( probscosis, 
     *                       Word.Minus( Word.LeftShift(1, sh), 1 ) );
     ***************************************************)
    RETURN y;
  END LeftShiftWithProbscosis;


(*-----------------------------------------------*)
(** A workaround of the absence of this primitive in the Modula-3
 * Word library (as well as many high level languages). Perhaps
 * also a version RightShiftWithCarry, specializing to sh=1, should
 * be provided. *)

PROCEDURE RightShiftWithProbscosis(VALUE x : Word.T;
                                  VALUE sh : CARDINAL;
                                  VAR probscosis : Word.T) : Word.T =
  VAR
    y : Word.T;
  BEGIN
    <* ASSERT sh<Word.Size *>
    y := Word.RightShift(x, sh);
    y := Word.Or(y, probscosis);
    probscosis := Word.LeftShift(x, Word.Size - sh);
    (** Now do not need to mask off sh high-order bits to get
     * final probscosis: 
     *    probscosis := Word.And( probscosis, 
     * Word.LeftShift( Word.Minus(Word.LeftShift(1,sh),1), Word.Size-sh ) );
     ****************************************************)
    RETURN y;
  END RightShiftWithProbscosis;

(*-----------------------------------------*)
(** A workaround of the absence of this primitive in the Modula-3
 * Word library (as well as many high level languages): *)

PROCEDURE DoubleLengthMultiply(VALUE x,y : Word.T;
                            VAR lo, hi : Word.T ) =
  BEGIN
    lo := Word.Times(x,y);
    hi := HighTimes(x,y);
  END DoubleLengthMultiply;

(*-------------------------------------------------*)
(* Returns the "hi" word in DoubleLengthMultiply(x,y, lo,hi). *)

PROCEDURE HighTimes(VALUE x,y : Word.T) : Word.T =
  VAR
    hi, xtop, ytop, xbot, ybot : Word.T;
  BEGIN
    xtop := Word.RightShift( x, HalfLength );
    ytop := Word.RightShift( y, HalfLength );
    xbot := Word.And( x, LeastSignifHalfMask );
    ybot := Word.And( y, LeastSignifHalfMask );
    
    hi := Word.Plus( Word.Times(xtop, ybot),
                     Word.Times(xbot, ytop) );

    hi := Word.RightShift( Word.Plus(hi, 
                                      Word.RightShift(
                                         Word.Times(xbot, ybot),
                                         HalfLength )),
                           HalfLength );

    hi := Word.Plus( Word.Times(xtop, ytop),  hi );
    RETURN hi;
  END HighTimes;

(*--------------------------------------------*)
(** I have not written a DoubleLengthDivide workaround,
 * but this absence is not so serious since many bignum
 * packages implement division via multiplication
 * and Newton algorithm, and modulus via division,
 * multiplication and subtraction. *)

PROCEDURE PopCount(x : Word.T) : [0 .. Word.Size] =
  VAR
    j, sum : [0 .. Word.Size];
  BEGIN
    j := Word.Size - 8;
    sum := BytePopCount[ Word.RightShift(x, j) ];
    REPEAT
      DEC(j,8);
      sum := sum + 
           BytePopCount[ Word.And(Word.RightShift(x, j),2_11111111) ];
    UNTIL j=0;
    RETURN sum;
  END PopCount;

(*------------------------------------------*)
(** Returns the index (in [0..Word.Size-1]) of the least significant
 * bit of x that is 1. But if x=0, returns -1. *)

PROCEDURE FindLeastSignifBit(x: Word.T) : [-1 .. Word.Size-1] =
  VAR
    j := 0;
    y : Word.T;
  BEGIN
    IF x=0 THEN RETURN -1; END;
    y := Word.And(x,2_11111111);
    WHILE y=0 AND j<Word.Size-8 DO
      INC(j,8);
      y := Word.And( Word.RightShift(x,j), 2_11111111 );
    END;
    RETURN j + IndexOfLSBit[y];
  END FindLeastSignifBit;

(*-------------------------------------------*)
(** Returns the index (in [0..Word.Size-1]) of the most significant
 * bit of x that is 1. But if x=0, returns Word.Size. *)

PROCEDURE FindMostSignifBit(x: Word.T) : [0 .. Word.Size] =
  VAR
    j := Word.Size - 8;
    y : Byte;
  BEGIN
    IF x=0 THEN RETURN Word.Size; END;
    REPEAT
      y := Word.And(Word.RightShift(x, j), 2_11111111);
      IF y#0 THEN RETURN j + IndexOfMSBit[y]; END;
      DEC(j,8);
    UNTIL j=0;
    y := Word.And(x, 2_11111111);
    <* ASSERT y#0 *>
    RETURN IndexOfMSBit[y];
  END FindMostSignifBit;
(*===============================================*)
PROCEDURE Test() =
BEGIN
  <* ASSERT Word.Size MOD 8 = 0 *>

  (** And now a few tests to be sure it works... *)

  <* ASSERT PopCount(254784321) = 16 *>
  <* ASSERT PopCount(16_f) = 4 *>
  <* ASSERT PopCount(16_ffffffff) = 32 *>
  <* ASSERT PopCount(16_e0000000) = 3 *>
  <* ASSERT PopCount(16_00050000) = 2 *>
  <* ASSERT PopCount( LeastSignifHalfMask ) = HalfLength *>

  <* ASSERT FindMostSignifBit(16_00050000) = 4*4+2  *>
  <* ASSERT FindMostSignifBit(16_00500000) = 5*4+2  *>
  <* ASSERT FindMostSignifBit(16_f0005000) = 31  *>
  <* ASSERT FindMostSignifBit(16_1) = 0  *>
  <* ASSERT FindMostSignifBit(16_0) = Word.Size  *>
  <* ASSERT FindMostSignifBit( LeastSignifHalfMask ) = HalfLength-1 *>

  <* ASSERT FindLeastSignifBit(16_00050000) = 4*4  *>
  <* ASSERT FindLeastSignifBit(16_00500000) = 5*4  *>
  <* ASSERT FindLeastSignifBit(16_f0005000) = 3*4  *>
  <* ASSERT FindLeastSignifBit(16_1) = 0  *>
  <* ASSERT FindLeastSignifBit(16_0) = -1  *>
  <* ASSERT FindLeastSignifBit(16_80000000) = 31  *>
  <* ASSERT FindLeastSignifBit( LeastSignifHalfMask ) = 0 *>

  <* ASSERT HighTimes(3, 2) = 0 *>
  <* ASSERT HighTimes(16_ffff0000, 16_aa230000) = 16_aa2255dd OR Word.Size#32 *>
  <* ASSERT HighTimes(16_ffff, 16_aa23) = 0 OR Word.Size#32 *>
  <* ASSERT HighTimes(16_ffff0000, 16_0000aa23) = 16_0000aa22 OR Word.Size#32 *>
  <* ASSERT HighTimes(16_0000ffff, 16_aa230000) = 16_0000aa22 OR Word.Size#32 *>

  <* ASSERT HighTimes(16_f2fb341, 16_2c3e7e12) =16_29fe7d7 OR Word.Size#32 *>
  (* routine returns 29fe7d6 !! *)
END Test;

(*==========================*)
BEGIN
END xWordEx.
