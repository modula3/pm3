(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 *
 * This file is released under the same conditions as Pickle.m3. See COPYRIGHT.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Sun Jul 23 00:17:49 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov  7 13:32:13 1997
 * Update Count    : 188
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.1  1998/02/26 16:37:33  dagenais
 * Enhanced pickler which allows communicating pickles between machines
 * with different endianess.
 *
 * 
 * HISTORY
 *)

UNSAFE MODULE ConvertPacking;

IMPORT RTTipe, RTPacking, PklAction, PklActionSeq, Thread, Wr, Rd, Swap, Word,
       IO, Fmt, PackingTbl, PackingTypeCode;

VAR packingCache: PackingTbl.T := NEW(PackingTbl.Default).init();

REVEAL ReadVisitor = RVPublic BRANDED "Packing Read Visitor 1.0" OBJECT END;
REVEAL WriteVisitor = WVPublic BRANDED "Packing Write Visitor 1.0" OBJECT END;

REVEAL T = Public BRANDED "ConvertPacking 1.0" OBJECT
      prog: PklActionSeq.T;
      kind: Kind;
      fromOffset: INTEGER := 0;
      toOffset: INTEGER := 0;
      fromSize: INTEGER := 0;
      toSize: INTEGER := 0;
      fromTipe: RTTipe.T;
      toTipe : RTTipe.T;
      from: RTPacking.T;
      to: RTPacking.T;
      nDim, fromEltPack, toEltPack: INTEGER := 0;

      writer: T;
    METHODS
      extractSwap (x: Word.T; i, n: CARDINAL; 
                   size: INTEGER): Word.T := ExtractSwap;

      buildOne(fromTipe: RTTipe.T; toTipe: RTTipe.T) RAISES {Error} := 
          BuildOne; 

      addCopy(length: INTEGER) := AddCopy;
      addCopy32to64(length: INTEGER; signed: BOOLEAN) := AddCopy32to64;
      addCopy64to32(length: INTEGER) := AddCopy64to32;

      addPackedSwapFirstField(fieldsize: INTEGER) :=
          AddPackedSwapFirstField;
      addPackedSwapNextField(fieldsize: INTEGER; offset: INTEGER) :=
          AddPackedSwapNextField;
      addPackedSwapArray(length, numElts, fieldsize, wordsize:
          INTEGER) := AddPackedSwapArray; 

      addSkipFrom(length: INTEGER) := AddSkipFrom;
      addSkipTo(length: INTEGER) := AddSkipTo;
      addSkipOrCopy(length: INTEGER) := AddSkipOrCopy;
      addSkip(fromDiff, toDiff: INTEGER) := AddSkip;
      addSwap16(length: INTEGER) := AddSwap16;
      addSwap32(length: INTEGER) := AddSwap32;
      addSwap64(length: INTEGER) := AddSwap64;
      addSwap32to64(length: INTEGER; signed: BOOLEAN) := AddSwap32to64;
      addSwap64to32(length: INTEGER) := AddSwap64to32;
      addRef(type: RefType) := AddRef;
      addDone() := AddDone;

      buildSuper(typecode: INTEGER; 
                 VAR fromSize, fromAlign, toSize, toAlign: INTEGER) 
        RAISES {Error} := BuildSuper;
      buildFields(fromField: RTTipe.Field; fromSize: INTEGER;
                  toField: RTTipe.Field; toSize: INTEGER) 
        RAISES {Error} := BuildFields;

      appendProg(other: T) RAISES {Error} := AppendProg;
    OVERRIDES
      init := Init;
      convertRead := Convert;
      write := Write;
      print := Print;
      printProgram := PrintProgram;
      getDim := GetDim;
    END;

CONST SignExt32 = ARRAY [0..1] OF Swap.Int32 {0, -1};

TYPE
  Int8 = BITS 8 FOR [-16_80 .. 16_7F];

(* Based on Word.Extract, but extracts from a word of size bytes with
   the opposite endian to this machine.
   Take n bits from x, with bit i as the least significant bit, and return them
   as the least significant n bits of a word whose other bits are 0. A checked
   runtime error if n + i > Word.Size. *)
PROCEDURE ExtractSwap (self: T; x: Word.T; i, n: CARDINAL; 
                       size: INTEGER): Word.T =
  VAR xb := LOOPHOLE(x, ARRAY [0..BYTESIZE(Word.T)-1] OF Int8);
      to: Word.T := 0;
      bit: INTEGER := 0;
  BEGIN
    (* March through the bytes from the input word backward, since
       that gives us the bytes in forward order from the other
       endian. Start with the byte containing the first part of the
       field to be extracted. *)
(*
    WITH last = size - 1 - (i DIV 8) DO
      i := i MOD 8;

      FOR b := last TO FIRST(xb) BY -1 DO
*)
    WITH first = i DIV 8 DO
      i := i MOD 8;

      IF NOT self.from.little_endian THEN
        bit := n;
      END;

      FOR b := first TO size-1 DO
        (* If we want some data from this *)
        WITH len = MIN(n, 8 - i) DO
          (* Get the appropriate part of the byte *)
          IF self.from.little_endian THEN
            to := Word.Insert(to, Word.Extract(xb[b], i, len), bit, len);
            INC(bit, len);
          ELSE
            DEC(bit, len);
            to := Word.Insert(to, Word.Extract(xb[b], 8-i-len, len), bit, len);
          END;
          (* set i to 0 after the first pass. *)
          i := 0;

          (* keep track of how much we have left to get *)
          DEC(n, len);
          IF n = 0 THEN
            RETURN to;
          END;
        END;
      END;
    END;
    RETURN to;
  END ExtractSwap;

CONST MAXLEN = 65536;
TYPE  BigBuf = ARRAY [0..MAXLEN-1] OF CHAR;
TYPE  BufPtr = UNTRACED REF BigBuf;

PROCEDURE ReadData(v: ReadVisitor;  dest: ADDRESS; len: INTEGER) 
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    WHILE len >= MAXLEN DO
      v.readData(LOOPHOLE(dest, BufPtr)^);
      INC(dest, MAXLEN);  DEC(len, MAXLEN);
    END;
    IF len > 0 THEN
      v.readData (SUBARRAY(LOOPHOLE (dest, BufPtr)^, 0, len));
    END;
  END ReadData;

PROCEDURE Convert(self: T; dest: ADDRESS; v: ReadVisitor; 
                  number: INTEGER := 1): ADDRESS RAISES 
          {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR t: ARRAY [0..7] OF CHAR;
      repetition: INTEGER := 1;

  BEGIN
    IF self.prog.size() = 2 THEN
      (* We have a one step program (the step plus the DONE step), so
         lets just do it in one shot. *)
      repetition := number;
      number := 1;
    END;
    FOR n := 1 TO number DO
      FOR i := 0 TO self.prog.size() - 1 DO
        WITH elem = self.prog.get(i), 
             length = elem.length * repetition DO
          CASE elem.kind OF
          | PklAction.Kind.Copy => 
            ReadData(v, dest, length);
            INC(dest, length);
          | PklAction.Kind.SwapPacked => 
            WITH nelem = NARROW(elem, PklAction.SwapPacked) DO
              FOR i := 1 TO length DO
                VAR from: Word.T := 0;
                    to: Word.T := 0;
                    bit: INTEGER := 0;
                    wb := LOOPHOLE(ADR(from), UNTRACED REF 
                                   ARRAY [0..BYTESIZE(Word.T)-1] OF CHAR);
                BEGIN
                  v.readData(SUBARRAY(wb^, 0, nelem.size));
                  
                  (** IO.Put("Extracting from: " &
                    Fmt.Pad(Fmt.Unsigned(from, 2), nelem.size * 8, '0') & "\n"); **)

                  IF self.from.little_endian THEN
                    FOR j := FIRST(nelem.field^) TO LAST(nelem.field^) DO
                      WITH extract = self.extractSwap(from, bit, 
                                                      nelem.field[j], 
                                                      nelem.size) DO
                        (** IO.Put(" Extracted field: " &
                          Fmt.Pad(Fmt.Unsigned(extract, 2),
                                  nelem.field[j], '0') & "\n"); **)

                        to := Word.Insert(to, extract,
                                          Word.Size-bit-nelem.field[j], 
                                          nelem.field[j]);

                        (** IO.Put(" Into field: " &
                          Fmt.Pad(Fmt.Unsigned(to, 2), Word.Size, '0') & "\n"); **)

                        INC(bit, nelem.field[j]);
                      END;
                    END;
                  ELSE
                    FOR j := FIRST(nelem.field^) TO LAST(nelem.field^) DO
                      WITH extract = self.extractSwap(from, bit, 
                                                      nelem.field[j], 
                                                      nelem.size) DO
                        (** IO.Put(" Extracted field: " &
                          Fmt.Pad(Fmt.Unsigned(extract, 2),
                                  nelem.field[j], '0') & "\n"); **)

                        to := Word.Insert(to, extract,
                                          bit, nelem.field[j]);

                        (** IO.Put(" Into field: " &
                          Fmt.Pad(Fmt.Unsigned(to, 2), nelem.size * 8, '0') & "\n"); **)

                        INC(bit, nelem.field[j]);
                      END;
                    END;
                  END;
                  (* Copy the bytes *)
                  SUBARRAY(LOOPHOLE(dest, BufPtr)^, 0, nelem.size) :=
                    SUBARRAY(LOOPHOLE(ADR(to), BufPtr)^, 0, nelem.size);
                END;
                INC(dest, nelem.size);
              END;
            END;
          | PklAction.Kind.SkipFrom =>
            v.skipData(length);
          | PklAction.Kind.SkipTo =>
            INC(dest, length);
          | PklAction.Kind.Skip =>
            v.skipData(length);
            INC(dest, length);
          | PklAction.Kind.Swap16 =>
            ReadData(v, dest, length*2);
            FOR i := 1 TO length DO
              WITH int16 = LOOPHOLE(dest, UNTRACED REF Swap.Int16) DO
                int16^ := Swap.Swap2(int16^);
              END;
              INC(dest, 2);
            END;
          | PklAction.Kind.Swap32 =>
            ReadData(v, dest, length*4);
            FOR i := 1 TO length DO
              WITH int32 = LOOPHOLE(dest, UNTRACED REF Swap.Int32) DO
                int32^ := Swap.Swap4(int32^);
              END;
              INC(dest, 4);
            END;
          | PklAction.Kind.Swap64 =>
            ReadData(v, dest, length*8);
            FOR i := 1 TO length DO
              WITH int64 = LOOPHOLE(dest, UNTRACED REF Swap.Int64On32) DO
                int64^ := Swap.Swap8(int64^);
              END;
              INC(dest, 8);
            END;
          | PklAction.Kind.Copy32to64, PklAction.Kind.Swap32to64 =>
            WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
              FOR i := 1 TO length DO
                v.readData(SUBARRAY(t, 0, 4));
                WITH int32 = LOOPHOLE(ADR(t[0]), UNTRACED REF Swap.Int32)^,
                     int64 = LOOPHOLE(dest, UNTRACED REF Swap.Int64On32) DO
                  (* First, put it in a 64 bit word appropriate to the 
                     sending machine. *)
                  IF self.from.little_endian THEN
                    int64.a := int32;
                    IF nelem.signed THEN
                      int64.b := SignExt32[Word.Extract(ORD(t[3]), 7, 1)];
                    ELSE
                      int64.b := 0;
                    END;
                  ELSE
                    int64.b := int32;
                    IF nelem.signed THEN
                      int64.a := SignExt32[Word.Extract(ORD(t[0]), 7, 1)];
                    ELSE
                      int64.a := 0;
                    END;
                  END;
                  (* Now, swap it if need be. *)
                  IF elem.kind = PklAction.Kind.Swap32to64 THEN
                    int64^ := Swap.Swap8(int64^);
                  END;
                END;
                INC(dest, 8);
              END;
            END;
          | PklAction.Kind.Copy64to32, PklAction.Kind.Swap64to32 =>
            FOR i := 1 TO length DO
              v.readData(t);
              WITH int64 = LOOPHOLE(ADR(t[0]), UNTRACED REF Swap.Int64On32)^,
                   int32 = LOOPHOLE(dest, UNTRACED REF Swap.Int32) DO
                (* First, get the lower 32 bits as perceived on the the 
                   sending machine. *)
                IF self.from.little_endian THEN
                  int32^ := int64.a;
                  IF int64.b # 0 AND int64.b # -1 THEN
                    RAISE Error("Data value too big.");
                  END;
                ELSE
                  int32^ := int64.b;
                  IF int64.a # 0 AND int64.a # -1 THEN
                    RAISE Error("Data value too big.");
                  END;
                END;

                (* Now, swap it if need be. *)
                IF elem.kind = PklAction.Kind.Swap64to32 THEN
                  int32^ := Swap.Swap4(int32^);
                END;
              END;
              INC(dest, 4);
            END;
          | PklAction.Kind.ReadRef =>
            WITH nelem = NARROW(elem, PklAction.Ref) DO
              FOR i := 1 TO length DO
                WITH ref = LOOPHOLE(dest, UNTRACED REF REFANY) DO
                  ref^ := v.readRef(nelem.refType);
                END;
                INC(dest, self.to.word_size DIV 8);
              END;
            END;
          | PklAction.Kind.Done =>
          END;
        END;
      END;
    END;
    RETURN dest;
  END Convert;
 
PROCEDURE WriteData(v: WriteVisitor;  src: ADDRESS;  len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    WHILE len >= MAXLEN DO
      v.writeData(LOOPHOLE(src, BufPtr)^);
      INC(src, MAXLEN);  DEC(len, MAXLEN);
    END;
    IF (len > 0) THEN
      v.writeData(SUBARRAY(LOOPHOLE(src, BufPtr)^, 0, len));
    END;
  END WriteData;
 
PROCEDURE Write(self: T; src: ADDRESS; v: WriteVisitor; 
                number: INTEGER := 1): ADDRESS RAISES 
          {Error, Wr.Failure, Thread.Alerted} =
  VAR repetition: INTEGER := 1;

  BEGIN
    IF self.writer # NIL THEN
      RETURN self.writer.write(src, v, number);
    END;

    IF self.prog.size() = 2 THEN
      (* We have a one step program (the step plus the DONE step), so
         lets just do it in one shot. *)
      repetition := number;
      number := 1;
    END;
    FOR n := 1 TO number DO
      FOR i := 0 TO self.prog.size() - 1 DO
        WITH elem = self.prog.get(i),
             length = elem.length * repetition DO
          CASE elem.kind OF
          | PklAction.Kind.Copy => 
            WriteData (v, src, length);
            INC(src, length);

          | PklAction.Kind.Skip =>
            v.skipData(length);
            INC(src, length);

          | PklAction.Kind.ReadRef =>
            WITH nelem = NARROW(elem, PklAction.Ref) DO
              FOR i := 1 TO length DO
                WITH ref = LOOPHOLE(src, UNTRACED REF REFANY) DO
                  v.writeRef(nelem.refType, ref^);
                END;
                INC(src, self.to.word_size DIV 8);
              END;
            END;

          | PklAction.Kind.Done =>
          | PklAction.Kind.SwapPacked => 
            RAISE Error("PklAction.Kind.SwapPacked called during write?");
          | PklAction.Kind.SkipFrom =>
            RAISE Error("PklAction.Kind.SkipFrom called during write?");
          | PklAction.Kind.SkipTo =>
            RAISE Error("PklAction.Kind.SkipTo called during write?");
          | PklAction.Kind.Swap16 =>
            RAISE Error("PklAction.Kind.Swap16 called during write?");
          | PklAction.Kind.Swap32 =>
            RAISE Error("PklAction.Kind.Swap32 called during write?");
          | PklAction.Kind.Swap64 =>
            RAISE Error("PklAction.Kind.Swap64 called during write?");
          | PklAction.Kind.Copy32to64 =>
            RAISE Error("PklAction.Kind.Copy32to64 called during write?");
          | PklAction.Kind.Swap32to64 =>
            RAISE Error("PklAction.Kind.Swap32to64 called during write?");
          | PklAction.Kind.Copy64to32 =>
            RAISE Error("PklAction.Kind.Copy64to32 called during write?");
          | PklAction.Kind.Swap64to32 =>
            RAISE Error("PklAction.Kind.Swap64to32 called during write?");

          END;
        END;
      END;
    END;
    RETURN src;
  END Write;
 
PROCEDURE AppendProg(self: T; other: T) RAISES {Error} =
  VAR nelem: PklAction.T;
  BEGIN
    FOR i := 0 TO other.prog.size() - 1 DO
      WITH elem = other.prog.get(i) DO
        CASE elem.kind OF
        | PklAction.Kind.Copy => 
          nelem := NEW(PklAction.T, kind := elem.kind, length := elem.length);
          INC(self.fromOffset, elem.length*8);
          INC(self.toOffset, elem.length*8);
        | PklAction.Kind.SwapPacked => 
          WITH t = NARROW(elem, PklAction.SwapPacked),
               nt = NEW(PklAction.SwapPacked, 
                        kind := t.kind, length := t.length, size := t.size,
                        field := NEW(REF ARRAY OF CARDINAL, 
                                     NUMBER(t.field^))) DO
            FOR i := FIRST(nt.field^) TO LAST(nt.field^) DO
              nt.field[i] := t.field[i];
            END;
            nelem := nt;
            INC(self.fromOffset, elem.length*8*t.size);
            INC(self.toOffset, elem.length*8*t.size);
          END;
        | PklAction.Kind.SkipFrom =>
          nelem := NEW(PklAction.T, kind := elem.kind, length := elem.length);
          INC(self.fromOffset, elem.length*8);
        | PklAction.Kind.SkipTo =>
          nelem := NEW(PklAction.T, kind := elem.kind, length := elem.length);
          INC(self.toOffset, elem.length*8);
        | PklAction.Kind.Skip =>
          nelem := NEW(PklAction.T, kind := elem.kind, length := elem.length);
          INC(self.fromOffset, elem.length*8);
          INC(self.toOffset, elem.length*8);
        | PklAction.Kind.Swap16 =>
          nelem := NEW(PklAction.T, kind := elem.kind, length := elem.length);
          INC(self.fromOffset, elem.length*16);
          INC(self.toOffset, elem.length*16);
        | PklAction.Kind.Swap32 =>
          nelem := NEW(PklAction.T, kind := elem.kind, length := elem.length);
          INC(self.fromOffset, elem.length*32);
          INC(self.toOffset, elem.length*32);
        | PklAction.Kind.Swap64 =>
          nelem := NEW(PklAction.T, kind := elem.kind, length := elem.length);
          INC(self.fromOffset, elem.length*64);
          INC(self.toOffset, elem.length*64);
        | PklAction.Kind.Copy32to64, PklAction.Kind.Swap32to64 =>
          WITH t = NARROW(elem, PklAction.Copy32to64) DO
            nelem := NEW(PklAction.Copy32to64, kind := t.kind, length := t.length,
                         signed := t.signed);
          END;
          INC(self.fromOffset, elem.length*32);
          INC(self.toOffset, elem.length*64);
        | PklAction.Kind.Copy64to32, PklAction.Kind.Swap64to32 =>
          nelem := NEW(PklAction.T, kind := elem.kind, length := elem.length);
          INC(self.fromOffset, elem.length*64);
          INC(self.toOffset, elem.length*32);
        | PklAction.Kind.ReadRef =>
          WITH t = NARROW(elem, PklAction.Ref) DO
            nelem := NEW(PklAction.Ref, kind := t.kind, length := t.length,
                         refType := t.refType);
          END;
          INC(self.fromOffset, elem.length*self.from.word_size);
          INC(self.toOffset, elem.length*self.to.word_size);
        | PklAction.Kind.Done =>
          RETURN;
        END;
        self.prog.addhi(nelem);
      END;
    END;
    RAISE Error("Invalid conversion program.");
  END AppendProg;
 
PROCEDURE GetHiKind(prog: PklActionSeq.T; kind: PklAction.Kind;
                    VAR elem: PklAction.T) : BOOLEAN =
  BEGIN
    IF prog.size() > 0 THEN
      elem := prog.gethi();
      IF elem.kind = kind THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END GetHiKind;

PROCEDURE AddCopy(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    INC(self.toOffset, length);
    IF GetHiKind(self.prog, PklAction.Kind.Copy, elem) THEN
      INC(elem.length, length DIV 8);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.Copy, 
                        length := length DIV 8));
  END AddCopy;

PROCEDURE AddPackedSwapFirstField(self: T; fieldsize: INTEGER) =
  BEGIN
    (* We'll copy the minimum number of whole bytes. *)
    WITH length = RoundUp(fieldsize, 8) DO
      INC(self.fromOffset, length);
      INC(self.toOffset, length);
      WITH elem = NEW(PklAction.SwapPacked, kind := PklAction.Kind.SwapPacked, 
                      length := 1, size := length DIV 8,
                      field := NEW(REF ARRAY OF CARDINAL, 1)) DO
        elem.field[0] := fieldsize;
        self.prog.addhi(elem);
      END;
    END;
  END AddPackedSwapFirstField;

PROCEDURE AddPackedSwapNextField(self: T; fieldsize: INTEGER; 
                                 offset: INTEGER) =
  VAR elem: PklAction.T;
      total: CARDINAL;
  BEGIN
    WITH ret = GetHiKind(self.prog, PklAction.Kind.SwapPacked, elem) DO
      (* The last entry _must_ be one of these. *)
      <* ASSERT ret *>
      WITH nelem = NARROW(elem, PklAction.SwapPacked) DO
        total := 0;
        FOR i := FIRST(nelem.field^) TO LAST(nelem.field^) DO
          INC(total, nelem.field[i]);
        END;
        (* They should be packed, so our offset should equal the 
           total thus far.  We can't check it, but we can at least
           check the bits line up in the current byte. *)
        <* ASSERT (total MOD 8) = (offset MOD 8) *>

        INC(total, fieldsize);

        (* The max total size of a packed set of fields is the word
           size!  This is guaranteed because a packed field cannot
           span a word boundary, and we only call this function when a
           packed field starts within a byte.  If a packed field
           starts on a byte boundary, we start a new set of packed
           fields. *)
        <* ASSERT total <= self.from.word_size *>

        (* Make sure the size in nelem is correct, and make sure the
           the fromOffset and toOffsets are updated to include any new
           bytes *)
        total := RoundUp(total, 8);
        WITH bytes = (total DIV 8) - nelem.size DO
          IF bytes > 0 THEN
            INC(self.fromOffset, bytes*8);
            INC(self.toOffset, bytes*8);
            INC(nelem.size, bytes);
          END;
        END;

        WITH new_field = NEW(REF ARRAY OF CARDINAL, 
                             NUMBER(nelem.field^) + 1) DO
          SUBARRAY(new_field^, 0, NUMBER(nelem.field^)) := nelem.field^;
          nelem.field := new_field;
        END;
        nelem.field[LAST(nelem.field^)] := fieldsize;
      END;
    END;
  END AddPackedSwapNextField;

PROCEDURE AddPackedSwapArray(self: T; length: INTEGER; 
                             numElts: INTEGER; fieldsize: INTEGER;
                             wordsize: INTEGER) =
  VAR count := wordsize DIV fieldsize;
  BEGIN
    (* We will build a packing for at most "count" fields.  Packed
       arrays must not have elements that span word boundaries, so 
       if "numElts > count" then "count * fieldsize = wordsize" *)
    <* ASSERT (numElts <= count) OR (count * fieldsize = wordsize) *>

    INC(self.fromOffset, length);
    INC(self.toOffset, length);

    WITH len       = numElts DIV count,
         size      = wordsize DIV 8,
         extraCnt  = numElts MOD count,   
         extraSize = (length MOD wordsize) DIV 8 DO

      (* First, we add an element to handle the bulk of the array
         element swapping when the array spans more than one word.
         It takes care of the first len*count elements which occupy
         the first len words. *)
      IF len > 0 THEN
        WITH nelem = NEW(PklAction.SwapPacked, 
                         kind := PklAction.Kind.SwapPacked, 
                         length := len, size := size, 
                         field := NEW(REF ARRAY OF CARDINAL, count)) DO
          FOR i := FIRST(nelem.field^) TO LAST(nelem.field^) DO
            nelem.field[i] := fieldsize;
          END;
          self.prog.addhi(nelem);
        END;
      END;

      (* Next, if the number of array elements does not fit exactly in
         full words, add one more element to the end which handles the
         extra fields.  *)
      IF extraSize > 0 THEN
        WITH nelem = NEW(PklAction.SwapPacked, 
                         kind := PklAction.Kind.SwapPacked, 
                         length := 1, size := extraSize,
                         field := NEW(REF ARRAY OF CARDINAL, extraCnt)) DO
          FOR i := FIRST(nelem.field^) TO LAST(nelem.field^) DO
            nelem.field[i] := fieldsize;
          END;
          self.prog.addhi(nelem);
        END;
      END;
    END;
  END AddPackedSwapArray; 

PROCEDURE AddCopy32to64(self: T; length: INTEGER; signed: BOOLEAN) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    INC(self.toOffset, length*2);
    IF GetHiKind(self.prog, PklAction.Kind.Copy32to64, elem) THEN
      WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
        IF nelem.signed = signed THEN
          INC(elem.length, length DIV 32);
          RETURN;
        END;
      END;
    END;
    self.prog.addhi(NEW(PklAction.Copy32to64, kind := PklAction.Kind.Copy32to64, 
                        length := length DIV 32, signed := signed));
  END AddCopy32to64;

PROCEDURE AddCopy64to32(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    <* ASSERT length MOD 2 = 0 *>
    INC(self.toOffset, length DIV 2);
    IF GetHiKind(self.prog, PklAction.Kind.Copy64to32, elem) THEN
      INC(elem.length, length DIV 64);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.Copy64to32,
                        length := length DIV 64));
  END AddCopy64to32;

PROCEDURE AddSkipFrom(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    IF GetHiKind(self.prog, PklAction.Kind.SkipFrom, elem) THEN
      INC(elem.length, length DIV 8);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.SkipFrom, 
                        length := length DIV 8));
  END AddSkipFrom;

PROCEDURE AddSkipTo(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.toOffset, length);
    IF GetHiKind(self.prog, PklAction.Kind.SkipTo, elem) THEN
      INC(elem.length, length DIV 8);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.SkipTo, 
                        length := length DIV 8));
  END AddSkipTo;

PROCEDURE AddSkipOrCopy(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    INC(self.toOffset, length);
    IF GetHiKind(self.prog, PklAction.Kind.Copy, elem) THEN
      INC(elem.length, length DIV 8);
      RETURN;
    ELSIF GetHiKind(self.prog, PklAction.Kind.Skip, elem) THEN
      INC(elem.length, length DIV 8);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.Skip, 
                        length := length DIV 8));
  END AddSkipOrCopy;

PROCEDURE AddSkip(self: T; fromDiff, toDiff: INTEGER) =
  BEGIN
    IF fromDiff = toDiff THEN
      IF fromDiff > 0 THEN
        self.addSkipOrCopy(fromDiff);
      END;
    ELSE
      WITH bothPad = MIN(toDiff, fromDiff) DO
        IF bothPad > 0 THEN
          self.addSkipOrCopy(bothPad);
        END;
        DEC(fromDiff, bothPad);
        DEC(toDiff, bothPad);
        IF fromDiff > 0 THEN
          self.addSkipFrom(fromDiff);
        END;
        IF toDiff > 0 THEN
          self.addSkipTo(toDiff);
        END;
      END;
    END;
  END AddSkip;

PROCEDURE AddSwap16(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    INC(self.toOffset, length);
    IF GetHiKind(self.prog, PklAction.Kind.Swap16, elem) THEN
      INC(elem.length, length DIV 16);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.Swap16, 
                        length := length DIV 16));
  END AddSwap16;

PROCEDURE AddSwap32(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    INC(self.toOffset, length);
    IF GetHiKind(self.prog, PklAction.Kind.Swap32, elem) THEN
      INC(elem.length, length DIV 32);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.Swap32, 
                        length := length DIV 32));
  END AddSwap32;

PROCEDURE AddSwap64(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    INC(self.toOffset, length);
    IF GetHiKind(self.prog, PklAction.Kind.Swap64, elem) THEN
      INC(elem.length, length DIV 64);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.Swap64, 
                        length := length DIV 64));
  END AddSwap64;

PROCEDURE AddSwap32to64(self: T; length: INTEGER; signed: BOOLEAN) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    INC(self.toOffset, length*2);
    IF GetHiKind(self.prog, PklAction.Kind.Swap32to64, elem) THEN
      WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
        IF nelem.signed = signed THEN
          INC(elem.length, length DIV 32);
          RETURN;
        END;
      END;
    END;
    self.prog.addhi(NEW(PklAction.Copy32to64, kind := PklAction.Kind.Swap32to64, 
                        length := length DIV 32, signed := signed));
  END AddSwap32to64;

PROCEDURE AddSwap64to32(self: T; length: INTEGER) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, length);
    <* ASSERT length MOD 2 = 0 *>
    INC(self.toOffset, length DIV 2);
    IF GetHiKind(self.prog, PklAction.Kind.Swap64to32, elem) THEN
      INC(elem.length, length DIV 64);
      RETURN;
    END;
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.Swap64to32, 
                        length := length DIV 64));
  END AddSwap64to32;

PROCEDURE AddRef(self: T; type: RefType) =
  VAR elem: PklAction.T;
  BEGIN
    INC(self.fromOffset, self.from.word_size);
    INC(self.toOffset, self.to.word_size);
    IF GetHiKind(self.prog, PklAction.Kind.ReadRef, elem) THEN
      WITH nelem = NARROW(elem, PklAction.Ref) DO
        IF nelem.refType = type THEN
          INC(elem.length);
          RETURN;
        END;
      END;
    END;
    self.prog.addhi(NEW(PklAction.Ref, kind := PklAction.Kind.ReadRef, 
                        length := 1, refType := type));
  END AddRef;

PROCEDURE AddDone(self: T) =
  BEGIN
    self.prog.addhi(NEW(PklAction.T, kind := PklAction.Kind.Done));
  END AddDone;

PROCEDURE New(typecode: INTEGER; from: RTPacking.T; to: RTPacking.T; 
              VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER): T 
  RAISES {Error} =
  VAR key := PackingTypeCode.T{from := RTPacking.Encode(from), 
                               to := RTPacking.Encode(to),
                               tc := typecode};
      ref : REFANY; 
  BEGIN
    (* If we've already building this converter, return it. *)
    IF packingCache.get(key, ref) THEN
      WITH ret = NARROW(ref, T) DO
        ret.getDim(nDim, fromEltPack, toEltPack);
        RETURN ret;
      END;
    END;

    RETURN NEW(T).init(typecode, from, to, nDim, fromEltPack, toEltPack);
  END New;

PROCEDURE GetDim(self: T; VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER) =
  BEGIN
    nDim := self.nDim;
    fromEltPack := self.fromEltPack;
    toEltPack := self.toEltPack;
  END GetDim;

PROCEDURE RoundUp (size, alignment: INTEGER): INTEGER =
  BEGIN
    IF (alignment = 0) THEN RETURN size;
    ELSE RETURN ((size + alignment - 1) DIV alignment) * alignment END;
  END RoundUp;

PROCEDURE Init(self: T; typecode: INTEGER; from: RTPacking.T; 
               to: RTPacking.T; 
               VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER): T
  RAISES {Error} =
  VAR key := PackingTypeCode.T{from := RTPacking.Encode(from), 
                               to := RTPacking.Encode(to),
                               tc := typecode};
      ref : REFANY; 
  BEGIN
    (* If we've already building this converter, return it.  We've
       still wasted a NEW() to get here, but better than recomputing
       everything! *)
    IF packingCache.get(key, ref) THEN
      WITH ret = NARROW(ref, T) DO
        ret.getDim(nDim, fromEltPack, toEltPack);
        RETURN ret;
      END;
    END;

    (* Get the fromTipe data structure and check for a few errors. *)
    self.fromTipe := RTTipe.Get(typecode, from);
    IF self.fromTipe = NIL THEN
      RAISE Error("\"from\" RTTipe.T is NIL.");
    END;

    self.toTipe := RTTipe.Get(typecode, to);
    IF self.toTipe = NIL THEN
      RAISE Error("\"to\" RTTipe.T is NIL.");
    END;

    (* OpenArrays need to be handled outside of the convert routine.  A
       conversion is therefore built for the type of the array element,
       and that conversion should be applied the correct number of
       times when the array dimmensions are known. *) 
    TYPECASE self.fromTipe OF
    | RTTipe.OpenArray(oa) =>
      WITH oa2 = NARROW(self.toTipe, RTTipe.OpenArray) DO
        self.nDim := oa.n_dimensions;
        self.fromEltPack := oa.elt_pack;
        self.toEltPack := oa2.elt_pack;
        self.fromTipe := oa.element;
        self.toTipe := oa2.element;
      END;
    ELSE
      self.nDim := 0;
      self.fromEltPack := 0;
      self.toEltPack := 0;
    END;
    nDim := self.nDim;
    fromEltPack := self.fromEltPack;
    toEltPack := self.toEltPack;

    (* We don't know how to do floating point conversions. *)
    IF from.float # to.float THEN
      RAISE Error("Data format (float) not handled.");
    END;

    IF from.word_size < 32 OR to.word_size < 32 THEN
      RAISE Error("Data format (word size) not handled.");
    END;

    self.from := from;
    self.to := to;
    self.prog := NEW(PklActionSeq.T).init();

    self.kind := GetKind(from, to);

    (* Build the tipe conversion for the top level  *)
    self.buildOne(self.fromTipe, self.toTipe);

    (* Figure out the allocated size. *)
    IF self.nDim > 0 THEN
      (* Open arrays will actually store the data for their elemnent
         here, so we don't adjust that. *)
      self.fromSize := self.fromTipe.size;
      self.toSize := self.toTipe.size;
    ELSE
      (* If this is not an open array, we need to heed what it says in
         in RTType.FixSizes().  A comment points out that
         all the REF types dataSizes are a multiple of "header" words,
         where the size of "header" is the word size of the machine.
         So, we will ensure that any conversion copies a multiple of
         word_size if it is a reftype.  *)

      (* Get the size of the data of the referent.  If it's an object,
         we want the size of the fields.  Otherwise, the size is what we
         want. *)


      self.fromSize := self.fromOffset;
      self.toSize := self.toOffset;
(*
      TYPECASE self.fromTipe OF
      | RTTipe.Object(o) =>
        WITH o2 = NARROW(self.toTipe, RTTipe.Object) DO
          self.fromSize := o.field_size;
          self.toSize := o2.field_size;
        END;
      ELSE
        self.fromSize := self.fromTipe.size;
        self.toSize := self.toTipe.size;
      END;
*)
      WITH newFromSize = RoundUp(self.fromSize, self.from.word_size),
           newToSize = RoundUp(self.toSize, self.to.word_size),
           fromDiff = newFromSize - self.fromSize,
           toDiff = newToSize - self.toSize DO
	<* ASSERT fromDiff >= 0 *>
	<* ASSERT toDiff >= 0 *>
  
        self.addSkip(fromDiff, toDiff);
      END;
    END;
    self.addDone();

    (* the write method is only geared towards "converting" between
       exactly the same machine. *)
    IF self.from # self.to THEN
      VAR wnDim, wfromEltPack, wtoEltPack: INTEGER;
      BEGIN
        self.writer := New(typecode, to, to, wnDim, 
                           wfromEltPack, wtoEltPack);
      END;
    ELSE
      self.writer := NIL;
    END;

    EVAL packingCache.put(key, self);
(*
    BasicTipeToText(self.fromTipe);
    IO.Put(", total size: " & Fmt.Int(self.fromSize) & "\n");
    BasicTipeToText(self.toTipe);
    IO.Put(", total size: " & Fmt.Int(self.toSize) & "\n");
*)
    RETURN self;
  END Init;

PROCEDURE GetKind(from: RTPacking.T; to: RTPacking.T): Kind =
  BEGIN
    IF from.word_size = to.word_size THEN
      IF from.little_endian = to.little_endian THEN
        RETURN Kind.Copy;
      ELSE
        RETURN Kind.Swap;
      END;
    ELSE
      IF from.little_endian = to.little_endian THEN
        IF from.word_size = 32 THEN
          RETURN Kind.Copy32to64;
        ELSE
          RETURN Kind.Copy64to32;
        END;
      ELSE
        IF from.word_size = 32 THEN
          RETURN Kind.Swap32to64;
        ELSE
          RETURN Kind.Swap64to32;
        END;
      END;
    END;
  END GetKind;

PROCEDURE BuildSuper(self: T; typecode: INTEGER; 
                     VAR fromSize, fromAlign, toSize, toAlign: INTEGER)
  RAISES {Error} =
  VAR nDim, fromEltPack, toEltPack: INTEGER;
      superConverter := New(typecode, self.from, self.to, nDim, 
                            fromEltPack, toEltPack);
      superFromTipe, superToTipe: RTTipe.Object;
  BEGIN
    (* Get the fromTipe data structure and check for a few errors. *)
    (*
    superFromTipe := RTTipe.Get(typecode, self.from);
    IF superFromTipe = NIL THEN
      RAISE Error("supertype \"from\" RTTipe.T is NIL.");
    END;

    superToTipe := RTTipe.Get(typecode, self.to);
    IF superToTipe = NIL THEN
      RAISE Error("supertype \"to\" RTTipe.T is NIL.");
    END;
    self.buildOne(superFromTipe, superToTipe);
    *)

    (* Add the supertype conversion program to ours. *)
    self.appendProg(superConverter);

    (* Return the proper allocated size. *)
    fromSize := superConverter.fromSize;
    toSize := superConverter.toSize;

    (* Coerce to RTTipe.Objects, which they must be. *)
    superFromTipe := superConverter.fromTipe;
    superToTipe := superConverter.toTipe;
    fromAlign := superFromTipe.field_align;
    toAlign := superToTipe.field_align;
  END BuildSuper; 

PROCEDURE BuildOne(self: T; fromTipe: RTTipe.T; 
                   toTipe: RTTipe.T) RAISES {Error} =
  BEGIN
    (* When we start, we assume that the offsets are positioned
       correctly for these tipes in the data objects. *)
    
    (* Check some obvious conditions. *)
    <* ASSERT fromTipe.kind = toTipe.kind *>

    (* In fact, these next two aren' always correct because of the
       weird way objects are laid out. *)
    (* ASSERT self.fromOffset MOD fromTipe.align = 0 *)
    (* ASSERT self.toOffset MOD toTipe.align = 0 *)

    TYPECASE fromTipe OF
    | RTTipe.Builtin, RTTipe.Enum, RTTipe.Set, RTTipe.Subrange =>
      CASE fromTipe.kind OF
      | RTTipe.Kind.Address => self.addRef(RefType.UntracedRef);
      | RTTipe.Kind.Proc => self.addRef(RefType.Proc);
      | RTTipe.Kind.Refany => self.addRef(RefType.Ref);
      | RTTipe.Kind.Null => 
        CASE self.kind OF
	| Kind.Copy, Kind.Swap =>
	  <* ASSERT fromTipe.size = toTipe.size *>
	  self.addCopy(fromTipe.size);
	| Kind.Copy32to64, Kind.Swap32to64 =>
	  <* ASSERT fromTipe.size = 32 *> 
	  <* ASSERT toTipe.size = 64 *> 
	  self.addCopy32to64(32, FALSE);
	| Kind.Copy64to32, Kind.Swap64to32 =>
	  <* ASSERT fromTipe.size = 64 *> 
	  <* ASSERT toTipe.size = 32 *> 
	  self.addCopy64to32(64);
	END;
      | RTTipe.Kind.Integer, RTTipe.Kind.Boolean, RTTipe.Kind.Cardinal,
        RTTipe.Kind.Char, RTTipe.Kind.Enum, RTTipe.Kind.Set,
        RTTipe.Kind.Subrange => 
        WITH signed = (fromTipe.kind = RTTipe.Kind.Integer) DO
	  CASE self.kind OF
	  | Kind.Copy =>
	    <* ASSERT fromTipe.size = toTipe.size *>
	    self.addCopy(fromTipe.size);
	  | Kind.Swap =>
	    <* ASSERT fromTipe.size = toTipe.size *>
	    CASE fromTipe.size OF
	    | 8 => self.addCopy(8);
	    | 16 => self.addSwap16(16);
	    | 32 => self.addSwap32(32);
	    | 64 => self.addSwap64(64);
            ELSE
              RAISE Error("Should not get here: 2");
	    END;
	  | Kind.Copy32to64 =>
	    IF fromTipe.size = toTipe.size THEN
	      self.addCopy(fromTipe.size);
	    ELSE
	      self.addCopy32to64(32, signed);
	    END;
	  | Kind.Copy64to32 =>
            IF fromTipe.size = toTipe.size THEN
	      self.addCopy(fromTipe.size);
	    ELSE
	      self.addCopy64to32(64);
	    END;
	  | Kind.Swap32to64 =>
	    CASE fromTipe.size OF
	    | 8 => <* ASSERT toTipe.size = 8 *> self.addCopy(8);
	    | 16 => <* ASSERT toTipe.size = 16 *> self.addSwap16(16);
	    | 32 => 
	      <* ASSERT toTipe.size >= 32 *> 
	      IF toTipe.size = 32 THEN
		self.addSwap32(32);
	      ELSE
		self.addSwap32to64(32, signed);
	      END;
	    | 64 => RAISE Error("Should not get here: 1");
            ELSE
              RAISE Error("Should not get here: 3");
	    END;
	  | Kind.Swap64to32 =>
	    CASE fromTipe.size OF
	    | 8 => <* ASSERT toTipe.size = 8 *> self.addCopy(8);
	    | 16 => <* ASSERT toTipe.size = 16 *> self.addSwap16(16);
	    | 32 => <* ASSERT toTipe.size = 32 *> self.addSwap32(32);
	    | 64 => <* ASSERT toTipe.size = 32 *> self.addSwap64to32(64);
            ELSE
              RAISE Error("Should not get here: 4");
	    END;
	  END;
        END;
      | RTTipe.Kind.Extended, RTTipe.Kind.Longreal =>
        <* ASSERT toTipe.size = 64 *> 
        <* ASSERT fromTipe.size = 64 *> 
        CASE self.kind OF
	| Kind.Copy, Kind.Copy32to64, Kind.Copy64to32 =>
	  self.addCopy(64);
	| Kind.Swap, Kind.Swap32to64, Kind.Swap64to32 =>
	  self.addSwap64(64);
	END;
      | RTTipe.Kind.Real => 
        <* ASSERT toTipe.size = 32 *> 
        <* ASSERT fromTipe.size = 32 *> 
        CASE self.kind OF
	| Kind.Copy, Kind.Copy32to64, Kind.Copy64to32 =>
	  self.addCopy(32);
	| Kind.Swap, Kind.Swap32to64, Kind.Swap64to32 =>
	  self.addSwap32(32);
	END;
      ELSE
        RAISE Error("Builtin but not builtin.");
      END;

    | RTTipe.Packed(p) => 
      (* We should only get here if we have a reference to a packed
         element, since packed fields are handled in BuildFields.
         We can ignore the parent type and just create a single entry
         packedswap or a byte copy. *)
      <* ASSERT fromTipe.size = toTipe.size *> 
      CASE self.kind OF
      | Kind.Copy, Kind.Copy32to64, Kind.Copy64to32 =>
        WITH bytesNeeded = RoundUp(p.size, 8) DO
          self.addCopy(bytesNeeded);
        END;
      | Kind.Swap, Kind.Swap32to64, Kind.Swap64to32 =>
        self.addPackedSwapFirstField(p.size);
      END;

    | RTTipe.Array(a) =>
      WITH a2 = NARROW(toTipe, RTTipe.Array) DO
        IF a.element.kind = RTTipe.Kind.Packed THEN
          IF a.element.size = 8 THEN
            (* An array of packed elements that fit exactly in a byte.  We can
               just copy this on any machine! *)
            self.addCopy(a.size);
          ELSE
            (* On same endian machines, we can just copy these bytes. *)
            CASE self.kind OF
            | Kind.Copy, Kind.Copy32to64, Kind.Copy64to32 =>
              self.addCopy(a.size);
            | Kind.Swap =>
              self.addPackedSwapArray(a.size, a.n_elts, a.element.size, 
                                      self.from.word_size);
            | Kind.Swap32to64, Kind.Swap64to32 =>
              (* Must pack properly into the smaller 32 bit words *)
              self.addPackedSwapArray(a.size, a.n_elts, a.element.size,
                                      32);
            END;
          END;
        ELSE
          (* Since array elements must fit in totally
             continguous memory, we don't need to check the offsets
             when we return from buildOne *)
          FOR i := 1 TO a.n_elts DO
            self.buildOne(a.element, a2.element);
          END;
        END;
      END;

    | RTTipe.Object(o) =>
      WITH o2 = NARROW(toTipe, RTTipe.Object) DO
        <* ASSERT o.super # NIL *>
        <* ASSERT o2.super # NIL *>
        <* ASSERT o.super.typecode = o2.super.typecode *>
        (* Get the conversion program for the supertype, and append it
           onto the end of ours.  Note that we assume that we are
           operating on the data addresses of the objects, as returned
           by RTHeap.GetDataAdr().  Not the actual object address.
           This address is BYTESIZE(ADDRESS) after the actual start of
           the allocated space. *) 

        VAR fromSize, fromAlign, toSize, toAlign: INTEGER;
            fromPad, toPad: INTEGER := 0;
        BEGIN
          IF o.super.typecode = TYPECODE(ROOT) THEN
            fromSize := 0;
            toSize := 0;
            fromAlign := o.align;
            toAlign := o2.align;
          ELSIF o.super.typecode = TYPECODE(UNTRACED ROOT) THEN
            RAISE Error("UNTRACED ROOT passed to ConvertPacking");
          ELSE
            self.buildSuper(o.super.typecode, fromSize, fromAlign,
                            toSize, toAlign); 
          END;
          
          (* If the alignment increases from a supertype to a subtype,
             we may have to add some padding in the data fields.  Note
             that we have to take into account the real allocated
             space to figure the current alignment, so add that extra
             BITSIZE(ADDRESS) to the beginning. *)
          WITH fromDiff = o.field_align - fromAlign  DO
            IF fromDiff > 0 THEN
              fromPad := (fromSize + self.from.word_size) MOD o.field_align;
              IF fromPad > 0 THEN
                fromPad := o.field_align - fromPad;
              END;
            END;
          END;
          WITH toDiff = o2.field_align - toAlign DO
            IF toDiff > 0 THEN
              toPad := (toSize + self.to.word_size) MOD o2.field_align;
              IF toPad > 0 THEN
                toPad := o2.field_align - toPad;
              END;
            END;
          END;

          (* If we need to pad them, do it. *)
          self.addSkip(fromPad, toPad);
        END;
        (* Since a conversion program guarantees it reads and writes
           the proper number of bytes, we can now just continue with
           this objects fields. *)

        self.buildFields(o.fields, o.field_size, o2.fields, o2.field_size);
      END;

    | RTTipe.Record(r) =>
      WITH r2 = NARROW(toTipe, RTTipe.Record) DO
        self.buildFields(r.fields, r.size, r2.fields, r2.size);
      END;

    | RTTipe.Ref(r) =>
      IF r.traced THEN
        self.addRef(RefType.Ref);
      ELSE
        self.addRef(RefType.UntracedRef);
      END;
    | RTTipe.OpenArray => RAISE Error("OpenArray within BuildOne?");
    ELSE
    END;
  END BuildOne;

PROCEDURE BuildFields(self: T; 
                      fromField: RTTipe.Field; fromSize: INTEGER;
                      toField: RTTipe.Field; toSize: INTEGER) 
    RAISES {Error} =
 VAR fromOffset := self.fromOffset;
     toOffset := self.toOffset;
  BEGIN
    WHILE fromField # NIL DO
      <* ASSERT fromField.type.kind = toField.type.kind *>
      (* Check that the last field advanced the data pointers
         correctly. *)
      WITH fromDiff = fromOffset + fromField.offset - self.fromOffset,
           toDiff = toOffset + toField.offset - self.toOffset DO
        <* ASSERT fromDiff > -8 *>
        <* ASSERT toDiff > -8 *>
        IF fromDiff < 0 THEN
          (* A data difference < 0 is ok if this field is packed *)
          <* ASSERT fromField.type.kind = RTTipe.Kind.Packed *>
          <* ASSERT toField.type.kind = RTTipe.Kind.Packed *>
          CASE self.kind OF
          | Kind.Copy, Kind.Copy32to64, Kind.Copy64to32 =>
            (* If this field overflows the current packed byte, copy
               the next ones as required. *)
            WITH spaceNeeded = fromField.type.size + fromDiff DO
              IF spaceNeeded > 0 THEN
                WITH bytesNeeded = RoundUp(spaceNeeded, 8) DO
                  self.addCopy(bytesNeeded);
                END;
              END;
            END;
          | Kind.Swap, Kind.Swap32to64, Kind.Swap64to32 =>
            self.addPackedSwapNextField(fromField.type.size,
                                        fromField.offset);
          END;
        ELSE
          (* If we need to add a skip, do it *)
          self.addSkip(fromDiff, toDiff);

          IF fromField.type.kind = RTTipe.Kind.Packed THEN
            (* We can ignore the word size here, size the size is
               explicit in the packing and will be the same on both
               machines, so no conversion is required. *)
            <* ASSERT fromField.type.size = toField.type.size *>
            <* ASSERT toField.type.kind = RTTipe.Kind.Packed *>
            CASE self.kind OF
            | Kind.Copy, Kind.Copy32to64, Kind.Copy64to32 =>
              WITH bytesNeeded = RoundUp(fromField.type.size, 8) DO
                self.addCopy(bytesNeeded);
              END;
            | Kind.Swap, Kind.Swap32to64, Kind.Swap64to32 =>
              <* ASSERT fromField.offset MOD 8 = 0 *>
              self.addPackedSwapFirstField(fromField.type.size);
            END;
          ELSIF fromField.type.kind = RTTipe.Kind.Object THEN
            (* Treat objects, when in a field, as references.
               Otherwise we get an infinite loop trying to expand things. *)
            self.addRef(RefType.Ref);
          ELSE
            self.buildOne(fromField.type, toField.type);
          END;
        END;
      END;

      fromField := fromField.next;
      toField := toField.next;
    END;

    (* Now that it's done, check if there is any space after the last
       field that needs to be accounted for. *)
    WITH fromDiff = fromOffset + fromSize - self.fromOffset,
         toDiff = toOffset + toSize - self.toOffset DO
      <* ASSERT fromDiff >= 0 *>
      <* ASSERT toDiff >= 0 *>
      self.addSkip(fromDiff, toDiff);
    END;
  END BuildFields;

(***************************************************************
 * For printing 
 ***************************************************************)

PROCEDURE PrintPacking(packing: RTPacking.T) =
  BEGIN
    IO.Put(" word_size: " & Fmt.Unsigned(packing.word_size, 10) & "\n");
    IO.Put(" max_align: " & Fmt.Unsigned(packing.max_align, 10) & "\n");
    IO.Put(" struct_align: " & Fmt.Unsigned(packing.struct_align, 10) & "\n");
    CASE packing.float OF
    | RTPacking.FloatKind.IEEE => 
      IO.Put(" float_kind: IEEE\n");
    | RTPacking.FloatKind.VAX => 
      IO.Put(" float_kind: VAX\n");
    | RTPacking.FloatKind.other => 
      IO.Put(" float_kind: other\n");
    END;
    IO.Put(" little_endian: " & Fmt.Bool(packing.little_endian) & "\n");
  END PrintPacking;

PROCEDURE KindToText(kind: RTTipe.Kind) =
  BEGIN
    CASE kind OF
    | RTTipe.Kind.Address => IO.Put( "Address");
    | RTTipe.Kind.Array => IO.Put( "Array");
    | RTTipe.Kind.Boolean => IO.Put( "Boolean");
    | RTTipe.Kind.Cardinal => IO.Put( "Cardinal");
    | RTTipe.Kind.Char => IO.Put( "Char");
    | RTTipe.Kind.Enum => IO.Put( "Enum");
    | RTTipe.Kind.Extended => IO.Put( "Extended");
    | RTTipe.Kind.Integer => IO.Put( "Integer");
    | RTTipe.Kind.Longreal => IO.Put( "Longreal");
    | RTTipe.Kind.Null => IO.Put( "Null");
    | RTTipe.Kind.Object => IO.Put( "Object");
    | RTTipe.Kind.OpenArray => IO.Put( "OpenArray");
    | RTTipe.Kind.Packed => IO.Put( "Packed");
    | RTTipe.Kind.Proc => IO.Put( "Proc");
    | RTTipe.Kind.Real => IO.Put( "Real");
    | RTTipe.Kind.Record => IO.Put( "Record");
    | RTTipe.Kind.Ref => IO.Put( "Ref");
    | RTTipe.Kind.Refany => IO.Put( "Refany");
    | RTTipe.Kind.Set => IO.Put( "Set");
    | RTTipe.Kind.Subrange => IO.Put( "Subrange");
    | RTTipe.Kind.UntracedRef => IO.Put( "UntracedRef");
    END;
  END KindToText; 

PROCEDURE BasicTipeToText(tipe: RTTipe.T) =
  BEGIN
    IO.Put( "kind: " );
    KindToText(tipe.kind);
    IO.Put( ", size: " & Fmt.Int(tipe.size DIV 8) & "+" & 
      Fmt.Int(tipe.size MOD 8) & ", align: " & Fmt.Int(tipe.align));
  END BasicTipeToText; 

PROCEDURE FieldsToText(f: RTTipe.Field; pre: TEXT; 
                       packing: RTPacking.T) = 
  BEGIN
    IF f = NIL THEN
      IO.Put( pre & "<NO FIELDS>");
      RETURN;
    END;
    IO.Put(pre & "offset: " & Fmt.Int(f.offset DIV 8) & "+" &
        Fmt.Int(f.offset MOD 8) & "\n");
    TipeToText(f.type, "  " & pre, packing);
    f := f.next;
    WHILE f # NIL DO
      IO.Put("\n" & pre & "offset: " & Fmt.Int(f.offset DIV 8) & "+" &
        Fmt.Int(f.offset MOD 8) & "\n");
      TipeToText(f.type, "  " & pre, packing);
      f := f.next;
    END;
  END FieldsToText;
  
PROCEDURE TipeToText(tipe: RTTipe.T; pre: TEXT; packing: RTPacking.T) =
  BEGIN
    IF tipe = NIL THEN
      IO.Put(pre & "<NIL>");
      RETURN;
    END;
    IO.Put(pre);
    BasicTipeToText(tipe);

    TYPECASE tipe OF
    | RTTipe.Builtin =>
      IO.Put(" (Builtin)");
    | RTTipe.Array(a) =>
      IO.Put(" (packed elements " & Fmt.Int(a.elt_pack) & ") [1.." & 
             Fmt.Int(a.n_elts) & "] of \n");
      TipeToText(a.element, pre & "  ", packing);
    | RTTipe.Enum(e) =>
      IO.Put(" of " & Fmt.Int(e.n_elts) & " elements");
    | RTTipe.Object(o) =>
      IO.Put(":\n" & pre & "  Super Type = ");
      IF o.super # NIL THEN
        IO.Put("typecode " & Fmt.Int(o.super.typecode) & "\n");
        TipeToText(RTTipe.Get(o.super.typecode, packing),
                   pre & "    ", packing);
      ELSE
        IO.Put(" <NIL> type?\n");
      END;
      IO.Put("\n" & pre & "  Fields.  size: " & 
        Fmt.Int(o.field_size DIV 8) & "+" & 
        Fmt.Int(o.field_size MOD 8) & ", align: " & 
        Fmt.Int(o.field_align) & "\n");
      FieldsToText(o.fields, "    " & pre, packing);
    | RTTipe.OpenArray(oa) =>
      IO.Put(" in " & Fmt.Int(oa.n_dimensions) & 
        " dimensions, packed " & Fmt.Int(oa.elt_pack) & " of \n"); 
      TipeToText(oa.element, pre & "  ", packing);
    | RTTipe.Packed(p) =>
      IO.Put(" in " & Fmt.Int(p.n_bits) & " bits of\n");
      TipeToText(p.base, "  " & pre, packing);
    | RTTipe.Record(r) =>
      IO.Put(" of:\n");
      FieldsToText(r.fields, "  " & pre, packing);
    | RTTipe.Ref(r) =>
      IO.Put(" (traced: " & Fmt.Bool(r.traced) & ", uid: " &
             Fmt.Int(r.uid) & ") to ");
      IF r.self # NIL THEN
        IO.Put("typecode " & Fmt.Int(r.self.typecode) & "\n");
      ELSE
        IO.Put("<NIL> type?\n");
      END;
    | RTTipe.Set(s) =>
      IO.Put(" of " & Fmt.Int(s.n_elts) & " elements");
    | RTTipe.Subrange(s) =>
      IO.Put(" from " & Fmt.Int(s.min) & " to " & Fmt.Int(s.max));
    ELSE
    END;
  END TipeToText; 

PROCEDURE Print(self: T) =
  BEGIN
    IO.Put("Converting from packing:\n");
    PrintPacking(self.from);
    IO.Put("to packing:\n");
    PrintPacking(self.to);
    IO.Put("Converting from RTTipe:\n");
    TipeToText(self.fromTipe, "  ", self.from);
    IO.Put("\nto RTTipe:\n");
    TipeToText(self.toTipe, "  ", self.to);
    self.printProgram();
  END Print;

PROCEDURE PrintProgram(self: T) =
  VAR fromSize := 0;
      toSize := 0;
  BEGIN    
    IO.Put("\nDoing it in " & Fmt.Int(self.prog.size()) & " step(s):\n");
    FOR i := 0 TO self.prog.size() - 1 DO
      WITH elem = self.prog.get(i) DO
        CASE elem.kind OF
        | PklAction.Kind.Copy => 
          IO.Put(" Copy " & Fmt.Int(elem.length) & " byte(s).\n");
          INC(fromSize, elem.length);
          INC(toSize, elem.length);
        | PklAction.Kind.SwapPacked => 
          WITH nelem = NARROW(elem, PklAction.SwapPacked) DO
            IO.Put(" Copy and swap " & Fmt.Int(elem.length) & 
              " unit(s) of " & Fmt.Int(nelem.size) & 
              " byte(s) with packed bitfields: ");
            FOR i := FIRST(nelem.field^) TO LAST(nelem.field^) DO
              IO.Put(Fmt.Int(nelem.field[i]) & " ");
            END;
          END;
          IO.Put("\n");
          INC(fromSize, elem.length);
          INC(toSize, elem.length);
        | PklAction.Kind.SkipFrom =>
          IO.Put(" Skip " & Fmt.Int(elem.length) & " src byte(s).\n");
          INC(fromSize, elem.length);
        | PklAction.Kind.SkipTo =>
          IO.Put(" Skip " & Fmt.Int(elem.length) & " dst byte(s).\n");
          INC(toSize, elem.length);
        | PklAction.Kind.Skip =>
          IO.Put(" Skip " & Fmt.Int(elem.length) & " src and dst byte(s).\n");
          INC(fromSize, elem.length);
          INC(toSize, elem.length);
        | PklAction.Kind.Swap16 =>
          IO.Put(" Swap " & Fmt.Int(elem.length) & " 16-bit word(s).\n");
          INC(fromSize, elem.length*2);
          INC(toSize, elem.length*2);
        | PklAction.Kind.Swap32 =>
          IO.Put(" Swap " & Fmt.Int(elem.length) & " 32-bit word(s).\n");
          INC(fromSize, elem.length*4);
          INC(toSize, elem.length*4);
        | PklAction.Kind.Swap64 =>
          IO.Put(" Swap " & Fmt.Int(elem.length) & " 64-bit word(s).\n");
          INC(fromSize, elem.length*8);
          INC(toSize, elem.length*8);
        | PklAction.Kind.Copy32to64 =>
          IO.Put(" Copy " & Fmt.Int(elem.length) & " 32 to 64-bit ");
          WITH nelem = NARROW(elem, PklAction.Copy32to64) DO
            IF nelem.signed THEN
              IO.Put("signed ");
            ELSE
              IO.Put("unsigned ");
            END;
          END;
          IO.Put("word(s).\n");
          INC(fromSize, elem.length*4);
          INC(toSize, elem.length*8);
        | PklAction.Kind.Copy64to32 =>
          IO.Put(" Copy " & Fmt.Int(elem.length) & " 64 to 32-bit word(s).\n");
          INC(fromSize, elem.length*8);
          INC(toSize, elem.length*4);
        | PklAction.Kind.Swap32to64 =>
          IO.Put(" Copy and Swap " & Fmt.Int(elem.length) & 
            " 32 to 64-bit words.\n");
          INC(fromSize, elem.length*4);
          INC(toSize, elem.length*8);
        | PklAction.Kind.Swap64to32 =>
          IO.Put(" Copy and Swap " & Fmt.Int(elem.length) & 
            " 64 to 32-bit words.\n"); 
          INC(fromSize, elem.length*8);
          INC(toSize, elem.length*4);
        | PklAction.Kind.ReadRef =>
          IO.Put(" Read " & Fmt.Int(elem.length));
          WITH nelem = NARROW(elem, PklAction.Ref) DO
            CASE nelem.refType OF
            | PklAction.RefType.Ref => IO.Put(" traced");
            | PklAction.RefType.UntracedRef => IO.Put(" untraced");
            | PklAction.RefType.Proc => IO.Put(" procedure");
            END;
          END;
          IO.Put(" references(s).\n");
          INC(fromSize, elem.length*(self.from.word_size DIV 8));
          INC(toSize, elem.length*(self.to.word_size DIV 8));
        | PklAction.Kind.Done =>
          IO.Put("Copied " & Fmt.Int(fromSize) & " bytes to " &
            Fmt.Int(toSize) & "\n");
        END;
      END;
    END;
  END PrintProgram;

BEGIN
  (* This will only work on machines with 8 bit chars right now. *)
  <* ASSERT BITSIZE(CHAR) = 8 *>
END ConvertPacking.
