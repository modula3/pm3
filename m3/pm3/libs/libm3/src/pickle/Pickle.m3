(* Copyright 1992 Digital Equipment Corporation             *)
(* Distributed only by permission.                          *)
(* Last modified on Thu Jan 26 13:56:19 PST 1995 by kalsow  *)
(*      modified on Tue Mar  1 16:17:32 PST 1994 by wobber  *)
(*      modified on Mon Mar  8 12:43:59 PST 1993 by mjordan *)
(*      modified on Tue Jan 26 16:21:22 1993 by birrell *)

(* This module implements Pickles.  See the interface for
   documentation *)

UNSAFE MODULE Pickle;

IMPORT Rd, RT0, RTAllocator, RTCollector, RTHeap, RTHeapRep, RTType, RTTypeFP,
       RTTypeMap, Thread, UnsafeWr, Word, Wr, Fingerprint;

(* *)
(* Syntax of a pickle, and constants pertaining thereto *)
(* *)

(* Pickle::      Header Value Trailer
   Header::      Header1                - verify it's a pickle
                 Header2                - verify it's a pickle
                 Version                - version of this syntax
                 CharRep                - character encoding
                 Endian                 - byte order
                 IntSize                - size of an INTEGER
                 RealRep                - format of REAL and such
   CharRep::     CharRepISOLatin1
   Endian::      LittleEndian | BigEndian
   IntSize::     IntSize16 | IntSize32 | IntSize64
   RealRep::     RealRepNative
   Value::       '0'                    - r=NIL
                 '1' 4-byte-integer     - pickle-relative index of "r" (0 based)
                 '2' 8-bytes              - COMPATIBILITY, old fingerprint
                 '3' scInt acInt Contents - COMPATIBILITY, old value
                 - '4' is not used, but occurs in Trailer as blunder check.
                 '5' sc Contents        - an actual value
   FingerPrint:: bytes                  - 8 bytes, details t.b.d.
   sc::          LocalCode              - typecode of Special
                                          writing the value
   Contents::    ( bytes | Value )*     - Special.write output
   Trailer::     Trailer1 Trailer2      - verify the parse was OK

   - Note that "LocalCode" may occur within the bytes written by a
     special. E.g. the root special does this to record the allocated
     type of the value. It is written by WriteType and parsed by ReadType.

   LocalCode::   0 8-bytes |            - first occurence of a fingerprint.
                 [1..254] |             - subsequent occurrence, n IN [1..254].
                 255 4-byte-integer     - subsequent occurence, other cases.
   *)

CONST
  Header1 = '&';
  Header2 = '%';
  Version = '2';                       (* current syntax *)
  OldVersion = '1';                    (* old FP and ac syntax *)
  CharRepISOLatin1 = 'I';              (* 8-bit ISO Latin 1 *)
  LittleEndian = 'L';                  (* l.s. byte first *)
  BigEndian = 'B';                     (* m.s. byte first *)
  IntSize16 = '4';                     (* 16 bit INTEGER *)
  IntSize32 = '5';                     (* 32 bit INTEGER *)
  IntSize64 = '6';                     (* 64 bit INTEGER *)
  RealRepNative = 'N';                 (* whatever the writing
                                          host used *)
  Trailer1 = '4';                      (* # main cases *)
  Trailer2 = '\n';                     (* keeps editors happy *)


(* *)
(* Constants, types and revelations *)
(* *)

TYPE
  RefTable = REF ARRAY OF RECORD
      (* hash table keyed by REFANY, yields index in pickle *)
      r: REFANY := NIL;        (* the Ref *)
      index: INTEGER := 0;     (* pickle-relative index of this
                                  ref, 0-based *)
      nextUsed: INTEGER := 0;  (* index in this table of next
                                  used entry *)
    END;
  RefArray = REF ARRAY OF REFANY;
    (* array indexed by index in pickle, yields REFANY *)
  TypeTable = REF ARRAY OF INTEGER;
    (* indexed by RTType.TypeCode, yields pickle-relative
       typecode *)
    (* Or indexed by pickle-relative typecode, yields
       RTTypes.TypeCode *)
  SpecialTable = REF ARRAY OF Special;
    (* indexed by RTType.TypeCode, yields Special for nearest
       super-type *)

REVEAL
  Writer = WriterPublic BRANDED "Pickle.Writer 1.0" OBJECT
      level := 0;
      refCount: INTEGER;         (* count of refs written in this pickle *)
      firstUsed: INTEGER;        (* index in "refs" of first used entry *)
      refs: RefTable := NIL;     (* hash table of refs in this pickle *)
      tcCount: INTEGER;          (* count of typecodes in this pickle *)
      tcToPkl: TypeTable := NIL; (* process TC -> pickle TC *)
      pklToTC: TypeTable := NIL; (* pickle TC -> process TC, for erasing tcToPkl *)
      nextAddr: ADDRESS;         (* Used within RootSpecialWrite *)
      collisions: INTEGER := 0;  (* Performance measure *)
      visitor: WriteVisitor := NIL;
    OVERRIDES
      write := WriteRef;
      writeType := WriteType;
      writeInt := WriteInt;
    END;

  Reader = ReaderPublic BRANDED "Pickle.Reader 1.0" OBJECT
      level := 0;
      acPending := 0;            (* COMPATIBILITY - OldVersion *)
      refCount: INTEGER;         (* count of refs read in this pickle *)
      tcCount: INTEGER;          (* count of typecodes in this pickle *)
      refs: RefArray := NIL;     (* array of refs in this pickle *)
      pklToTC: TypeTable := NIL; (* pickle TC -> process TC *)
      nextAddr: ADDRESS;         (* Used within RootSpecialWrite *)
      visitor: ReadVisitor := NIL;
    OVERRIDES
      read := ReadRef;
      readType := ReadType;
      readInt := ReadInt;
      noteRef := NoteRef;
    END;

  Special = SpecialPublic BRANDED "Pickle.Special 1.0" OBJECT
    OVERRIDES
      write := RootSpecialWrite;
      read := RootSpecialRead;
    END;

TYPE
  WriteVisitor = RTTypeMap.Visitor OBJECT
      writer: Writer := NIL;
    OVERRIDES
      apply := VisitWrite;
    END;

  ReadVisitor = RTTypeMap.Visitor OBJECT
      reader: Reader := NIL;
    OVERRIDES
      apply := VisitRead;
    END;

CONST
  RefFields = RTTypeMap.Mask { RTTypeMap.Kind.Ref,
                               RTTypeMap.Kind.UntracedRef,
                               RTTypeMap.Kind.Proc };

TYPE (* for binary I/O loopholes *)
  CharInt32 = ARRAY [0..3] OF CHAR;  (* 32 bits only *)
  CharFP    = ARRAY [0..BYTESIZE(Fingerprint.T)-1] OF CHAR;
  ToChars   = UNTRACED REF ARRAY [0..100000000] OF CHAR; (* for misc. data *)

TYPE
  HC = { h1, h2, v, c, e, i, r }; (* the chars in a pickle header *)
  HT = { t1, t2 };                (* the chars in a pickle trailer *)
  Header = ARRAY HC OF CHAR;      (* a pickle header string *)
  Trailer = ARRAY HT OF CHAR;     (* a pickle trailer string *)

CONST
  InitRefCapacity = 99;           (* Init size of {Reader,Writer}.refs *)
  InitTypeCapacity = 99;          (* Init size of *.pklToTC *)


(* *)
(* Global variables (gasp!); initialized in main body *)
(* *)

VAR specialsMu := NEW(MUTEX);
VAR specials: SpecialTable;       (* LL >= specialsMu *)
VAR myHeader: Header;             (* header for pickles we write *)
VAR myTrailer: Trailer;           (* trailer for pickles we write *)
VAR nullReaderRef: REF INTEGER;   (* null value for reader.refs entries *)


(* *)
(* Top-level sugar: Write and Read *)
(* *)

PROCEDURE Write(wr: Wr.T; r: REFANY)
        RAISES { Error, Wr.Failure, Thread.Alerted } =
  BEGIN
    NEW (Writer, wr := wr).write(r);
  END Write;

PROCEDURE Read(rd: Rd.T): REFANY
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    RETURN NEW (Reader, rd := rd).read();
  END Read;


(* *)
(* Writer methods and subroutines *)
(* *)

PROCEDURE Hash(r: REFANY; table: RefTable): INTEGER =
  (* Considerations for the hash function:
        - use positive numbers
        - multiply to avoid collisions between equally
          distributed patterns
        - don't overflow 31-bit positive numbers
        - keep it efficient
        - result is in [0..LAST(table)]
  *)
  BEGIN
    RETURN ((Word.And(LOOPHOLE(r, Word.T), 16_7FFFFFFF) DIV
             65536) * (LOOPHOLE(r, Word.T) MOD 65536)
           ) MOD NUMBER(table^)
  END Hash;

PROCEDURE ExtendWriterRefs(writer: Writer) =
    (* Make writer.refs bigger *)
    VAR old := writer.refs;
    VAR oldFirst := writer.firstUsed;
    VAR h: INTEGER;
  BEGIN
    writer.firstUsed := -1;
    writer.refs := NEW(RefTable, NUMBER(writer.refs^) * 2 - 1);
    WHILE oldFirst >= 0 DO
      (* for each entry in "old" *)
      WITH oldEntry = old[oldFirst] DO
        h := Hash(oldEntry.r, writer.refs);
        LOOP
          WITH entry = writer.refs[h] DO
            IF entry.r = NIL THEN
              entry.r := oldEntry.r;
              entry.index := oldEntry.index;
              entry.nextUsed := writer.firstUsed;
              writer.firstUsed := h;
              EXIT
            ELSE
              <* ASSERT(entry.r # oldEntry.r) *>
              INC(h);
              IF h >= NUMBER(writer.refs^) THEN h := 0 END;
            END;(*IF*)
          END;(*WITH*)
        END;(*LOOP*)
        oldFirst := oldEntry.nextUsed;
      END;(*WITH oldEntry*)
    END;(*WHILE*)
  END ExtendWriterRefs;

PROCEDURE WriteRef(writer: Writer; r: REFANY)
        RAISES { Error, Wr.Failure, Thread.Alerted } =
    VAR h: INTEGER;
    VAR sp: Special;
  BEGIN
    IF writer.level = 0 THEN
      (* Start of a pickle; do the overhead and call ourselves
         recursively *)
      IF writer.visitor = NIL THEN
        writer.visitor := NEW (WriteVisitor, writer := writer);
      END;
      IF writer.refs = NIL THEN
        (* deferred allocation *)
        writer.refs := NEW(RefTable, InitRefCapacity * 2 - 1);
        WITH refs = writer.refs^ DO
          FOR i := 0 TO LAST(refs) DO refs[i].r := NIL END;
        END;
        writer.firstUsed := -1;
        writer.tcCount := 0; (* prevent excessive initialization *)
        writer.tcToPkl := NEW(TypeTable, RTType.MaxTypecode()+1);
        writer.pklToTC := NEW(TypeTable, InitTypeCapacity);
        WITH tcs = writer.tcToPkl^ DO
          FOR i := 0 TO LAST(tcs) DO tcs[i] := 0 END;
        END;
      END;
      (* (Re-)initialize the writer's state *)
      <*ASSERT (writer.firstUsed = -1)*>
          (* cleaned up last time *)
      writer.refCount := 0;
      FOR i := 1 TO writer.tcCount DO writer.tcToPkl[writer.pklToTC[i]] := 0 END;
      writer.tcCount := 0;
      writer.collisions := 0;
      Wr.PutString(writer.wr, myHeader);
      RTCollector.DisableMotion();
      INC(writer.level);
      TRY
        WriteRef(writer, r);
      FINALLY
        RTCollector.EnableMotion();
        DEC(writer.level);
        (* Flush the refs table now, to encourage the garbage
           collector. It must be flushed before re-use anyway *)
        h := writer.firstUsed;
        WHILE h >= 0 DO
          WITH entry = writer.refs[h] DO
            entry.r := NIL;
            h := entry.nextUsed;
          END;
        END;
        writer.firstUsed := -1;
      END;(*TRY*)
      Wr.PutString(writer.wr, myTrailer);
    ELSE
      (* Normal case: level#0 *)
      IF r = NIL THEN
        Wr.PutChar(writer.wr, '0');
      ELSE
        (* check refTable *)
        (* The following loop includes the entire hash table
           implementation. Considerations for the hash table
           algorithm:
             - mostly, we're adding new entries; looking up old
               entries is rare
             - we need to flush the table completely once per
               pickle
             - beware of relocating garbage collectors
           *)
        h := Hash(r, writer.refs);
        LOOP
          WITH entry = writer.refs[h] DO
            IF entry.r = NIL THEN
              (* virgin: insert in hash table *)
              entry.r := r;
              entry.index := writer.refCount;
              entry.nextUsed := writer.firstUsed;
                (* for fast flushing *)
              writer.firstUsed := h;
              INC(writer.refCount);
              IF writer.refCount * 2 > NUMBER(writer.refs^) THEN
                ExtendWriterRefs(writer);
              END;
              (* CAUTION: don't use "entry" after here, because
                 it might be invalidated by extendWriterRefs *)
              LOCK specialsMu DO
                sp := specials[TYPECODE(r)];
              END;
              LOCK writer.wr DO
                UnsafeWr.FastPutChar(writer.wr, '5');
              END;
              writer.writeType(sp.sc);
              sp.write(r, writer);
              EXIT
            ELSIF entry.r = r THEN
              (* recycled *)
              LOCK writer.wr DO
                UnsafeWr.FastPutChar(writer.wr, '1');
              END;
              writer.writeInt(entry.index);
              EXIT
            ELSE
              (* Hash collision *)
              INC(h);
              IF h >= NUMBER(writer.refs^) THEN h := 0 END;
              INC(writer.collisions);
            END;
          END;(*WITH*)
        END;(*LOOP*)
      END;(*r#NIL*)
    END;(*IF level=0 THEN ELSE*)
  END WriteRef;

PROCEDURE WriteType(writer: Writer; tc: INTEGER)
        RAISES { Wr.Failure, Thread.Alerted } =
    VAR fp: Fingerprint.T;
  BEGIN
    WITH pickleTC = writer.tcToPkl[tc] DO
      IF pickleTC = 0 THEN
        INC(writer.tcCount);
        pickleTC := writer.tcCount;
        writer.pklToTC[writer.tcCount] := tc;
        fp := RTTypeFP.ToFingerprint(tc);
        LOCK writer.wr DO
          UnsafeWr.FastPutChar(writer.wr, VAL(0, CHAR));
          UnsafeWr.FastPutString(writer.wr, LOOPHOLE(fp, CharFP));
        END;
      ELSIF pickleTC < 255 THEN
        LOCK writer.wr DO
          UnsafeWr.FastPutChar(writer.wr, VAL(pickleTC, CHAR));
        END;
      ELSE
        LOCK writer.wr DO
          UnsafeWr.FastPutChar(writer.wr, VAL(255, CHAR));
        END;
        writer.writeInt(pickleTC);
      END;
    END;
  END WriteType;

PROCEDURE WriteInt(writer: Writer; i: INTEGER)
        RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    LOCK writer.wr DO
      UnsafeWr.FastPutString(
        writer.wr, LOOPHOLE(ADR(i), UNTRACED REF CharInt32)^);
    END;
  END WriteInt;

(* *)
(* Reader methods and subroutines *)
(* *)

PROCEDURE ExtendReaderTypes(reader: Reader) =
  (* Extend reader.pklToTC *)
    VAR old := reader.pklToTC;
  BEGIN
    reader.pklToTC := NEW(TypeTable, NUMBER(reader.pklToTC^) * 2);
    SUBARRAY(reader.pklToTC^, 0, NUMBER(old^)) := old^;
  END ExtendReaderTypes;

PROCEDURE GetBinaryInt(rd: Rd.T): INTEGER
        RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    VAR i: INTEGER := 0;
  BEGIN
    IF Rd.GetSub(rd,
      LOOPHOLE (ADR(i), UNTRACED REF CharInt32)^) # BYTESIZE(CharInt32) THEN
        RAISE Rd.EndOfFile
    END;
    RETURN i;
  END GetBinaryInt;

PROCEDURE ReadFP(reader: Reader): TypeCode
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    (* Read a fingerprint (8 bytes), store in reader.pklToTC,
       and return typecode *)
    VAR fp: Fingerprint.T; tc: TypeCode;
  BEGIN
    IF Rd.GetSub(reader.rd, LOOPHOLE(fp, CharFP)) # BYTESIZE(CharFP) THEN
      RAISE Rd.EndOfFile
    END;
    INC(reader.tcCount);
    IF reader.tcCount >= NUMBER(reader.pklToTC^) THEN
      ExtendReaderTypes(reader);
    END;
    tc := RTTypeFP.FromFingerprint(fp);
    IF tc = RTType.NoSuchType THEN
      RAISE Error(
             "Can't read pickle (type not known in this program)")
    END;
    reader.pklToTC[reader.tcCount] := tc;
    RETURN tc
  END ReadFP;

PROCEDURE TCFromIndex(reader: Reader; index: INTEGER): TypeCode
      RAISES { Error } =
  BEGIN
    IF index > reader.tcCount THEN
      RAISE Error("Malformed pickle (TC index too large)")
    END;
    RETURN reader.pklToTC[index]
  END TCFromIndex;

PROCEDURE InvokeSpecial(reader: Reader; sc: TypeCode): REFANY
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    VAR sp: Special; r: REFANY; id: RefID;
  BEGIN
    LOCK specialsMu DO
      sp := specials[sc];
    END;
    IF sp.sc # sc THEN
      RAISE Error("Can't read pickle (Special not defined)")
    END;
    IF reader.refCount >= NUMBER(reader.refs^) THEN
      VAR old := reader.refs;
      BEGIN
        reader.refs := NEW(RefArray, NUMBER(reader.refs^) * 2);
        SUBARRAY(reader.refs^, 0, NUMBER(old^)) := old^;
      END;
    END;
    id := reader.refCount;
    reader.refs[id] := nullReaderRef;
    INC(reader.refCount);
    INC(reader.level);
    r := sp.read(reader, id);
    DEC(reader.level);
    reader.noteRef(r, id);
    RETURN r
  END InvokeSpecial;

PROCEDURE ReadRef(reader: Reader): REFANY
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    VAR r: REFANY; (* the result *)
    VAR repCase: CHAR;
  BEGIN
    IF reader.level = 0 THEN
      (* Start of a pickle *)
      IF reader.visitor = NIL THEN
        reader.visitor := NEW (ReadVisitor, reader := reader);
      END;
      IF reader.refs = NIL THEN
        (* deferred allocation *)
        reader.refs := NEW(RefArray, InitRefCapacity);
        reader.pklToTC := NEW(TypeTable, InitTypeCapacity);
      END;
      (* (Re-)initialize the reader's state *)
      reader.refCount := 0;
      reader.tcCount := 0;
      (* Header *)
      VAR headerChars: Header;
      BEGIN
        IF Rd.GetSub(reader.rd, headerChars) # NUMBER(HC) THEN
          RAISE Rd.EndOfFile
        ELSIF (headerChars[HC.h1] # myHeader[HC.h1]) OR
                (headerChars[HC.h2] # myHeader[HC.h2]) THEN
          RAISE Error("Malformed pickle (wrong signature)")
        ELSIF (headerChars[HC.v] # myHeader[HC.v]) AND
              (headerChars[HC.v] # OldVersion) THEN
          RAISE Error("Can't read pickle (wrong version)")
        ELSIF headerChars[HC.c] # myHeader[HC.c] THEN
          RAISE Error("Can't read pickle (char rep)")
        ELSIF headerChars[HC.e] # myHeader[HC.e] THEN
          RAISE Error("Can't read pickle (endian)")
(*
        ELSIF headerChars[HC.i] # myHeader[HC.i] THEN
          RAISE Error("Can't read pickle (INTEGER size)")
*)
        ELSIF headerChars[HC.r] # myHeader[HC.r] THEN
          RAISE Error("Can't read pickle (REAL rep)")
        END;
      END;
    END;
    LOOP
      (* COMPATIBILITY: OldVersion uses '2 fingerprint' *)
      repCase := Rd.GetChar(reader.rd);
      IF repCase # '2' THEN EXIT END;
      EVAL ReadFP(reader)
    END;
    IF repCase = '0' THEN
      r := NIL;
    ELSIF repCase = '1' THEN
      VAR refIndex := reader.readInt();
      BEGIN
        IF refIndex >= reader.refCount THEN
          RAISE Error("Malformed pickle (ref index too large)")
        END;
        r := reader.refs[refIndex];
      END;
    ELSIF repCase = '3' THEN
      (* COMPATIBILITY: OldVersion uses 3 sc ac contents *)
      VAR sc := GetBinaryInt(reader.rd);
      BEGIN
        reader.acPending := GetBinaryInt(reader.rd);
        r := InvokeSpecial(reader, TCFromIndex(reader, sc));
        reader.acPending := 0;
      END
    ELSIF repCase = '5' THEN
      r := InvokeSpecial(reader, reader.readType());
    ELSE
      RAISE Error("Malformed pickle (unknown switch)")
    END;
    IF reader.level = 0 THEN
      IF (Rd.GetChar(reader.rd) # Trailer1) OR
              (Rd.GetChar(reader.rd) # Trailer2) THEN
        RAISE Error("Malformed pickle (wrong trailer)")
      END;
      (* flush the ref table to encourage the garbage collector *)
      WITH refs = reader.refs^ DO
        FOR i := 0 TO reader.refCount-1 DO refs[i] := NIL END;
      END;
    END;
    RETURN r
  END ReadRef;

PROCEDURE ReadType(reader: Reader): TypeCode
      RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    VAR c: CHAR; ac: INTEGER;
  BEGIN
    IF reader.acPending # 0 THEN
      (* COMPATIBILITY *)
      ac := reader.acPending; reader.acPending := 0;
      RETURN TCFromIndex(reader, ac);
    ELSE
      c := Rd.GetChar(reader.rd);
      IF ORD(c) = 0 THEN
        RETURN ReadFP(reader)
      ELSIF ORD(c) < 255 THEN
        RETURN TCFromIndex(reader, ORD(c));
      ELSE
        RETURN TCFromIndex(reader, reader.readInt());
      END;
    END;
  END ReadType;

PROCEDURE ReadInt(reader: Reader): INTEGER
        RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN GetBinaryInt(reader.rd)
  END ReadInt;

PROCEDURE NoteRef(reader: Reader; ref: REFANY; id: RefID) =
  BEGIN
    WITH rr = reader.refs[id] DO
      <* ASSERT (rr = nullReaderRef) OR (rr = ref) *>
      rr := ref;
    END;
  END NoteRef;
  

(* *)
(* Specials, including RootSpecial *)
(* *)

EXCEPTION DuplicateSpecial; <* FATAL DuplicateSpecial *>

PROCEDURE RegisterSpecial(sp: Special) =
  BEGIN
    LOCK specialsMu DO
      IF specials[sp.sc].sc = sp.sc THEN
        RAISE DuplicateSpecial;
      END;
      FOR i := 0 TO LAST(specials^) DO
        IF (i#RT0.NilTypecode) AND RTType.IsSubtype(i,sp.sc) THEN
          (* i is a sub-type of this special *)
          IF (specials[i].sc = RT0.NilTypecode) OR
                      RTType.IsSubtype(sp.sc, specials[i].sc) THEN
            (* previous special for i isn't more specific than
               sp.sc *)
            specials[i] := sp;
          END;
        END;
      END;
    END;
  END RegisterSpecial;

PROCEDURE VisitWrite(v: WriteVisitor; field: ADDRESS; kind: RTTypeMap.Kind)
  RAISES ANY =
  (* Call-back from RTType.Visit for RootSpecialWrite *)
  VAR writer := v.writer;
  BEGIN
    (* write data fields preceding the ref *)
    IF field # writer.nextAddr THEN
      Wr.PutString(writer.wr,
                   SUBARRAY(LOOPHOLE(writer.nextAddr, ToChars)^,
                                     0, field - writer.nextAddr));
    END;
    IF kind = RTTypeMap.Kind.Ref THEN
      writer.write(LOOPHOLE(field, UNTRACED REF REFANY)^);
    ELSE
      (* Other REF fields, including procedures, are discarded on
         write *)
    END;
    writer.nextAddr := field + ADRSIZE(ADDRESS);
  END VisitWrite;

PROCEDURE RootSpecialWrite(<*UNUSED*> sp: Special;
                           r: REFANY; writer: Writer)
        RAISES { Error <*NOWARN*>, Wr.Failure, Thread.Alerted } =
    VAR nDim: INTEGER;
    VAR shape: UNTRACED REF ARRAY [0..999] OF INTEGER;
    VAR limit: ADDRESS;
  BEGIN
    writer.writeType(TYPECODE(r));
    RTHeapRep.UnsafeGetShape (r, nDim, shape);
    FOR i := 0 TO nDim-1 DO
      writer.writeInt(shape[i]);
    END;
    writer.nextAddr := RTHeap.GetDataAdr(r);
    limit := writer.nextAddr + RTHeap.GetDataSize(r);
    <*FATAL ANY*> BEGIN
      RTTypeMap.WalkRef(r, RefFields, writer.visitor);
    END;
    (* Write remainder of the data fields *)
    IF limit # writer.nextAddr THEN
      Wr.PutString(writer.wr,
                   SUBARRAY(LOOPHOLE(writer.nextAddr, ToChars)^,
                                      0, limit-writer.nextAddr));
    END;
  END RootSpecialWrite;

PROCEDURE VisitRead(v: ReadVisitor; field: ADDRESS; kind: RTTypeMap.Kind)
  RAISES ANY =
  (* Call-back from RTType.Visit for RootSpecialRead *)
  VAR reader := v.reader;
  BEGIN
    (* read data fields preceding the ref *)
    EVAL Rd.GetSub(reader.rd,
                   SUBARRAY(LOOPHOLE(reader.nextAddr, ToChars)^,
                                     0, field - reader.nextAddr));
    IF kind = RTTypeMap.Kind.Ref THEN
      LOOPHOLE(field, UNTRACED REF REFANY)^ := reader.read();
    ELSE
      (* Other REF fields, including procedures, are discarded on
         write *)
      LOOPHOLE(field, UNTRACED REF REFANY)^ := NIL;
    END;
    reader.nextAddr := field + ADRSIZE(ADDRESS);
  END VisitRead;

PROCEDURE RootSpecialRead(<*UNUSED*> sp: Special;
                          reader: Reader; id: RefID): REFANY
    RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
    VAR nDim: INTEGER;
    VAR shape: ARRAY [0..10] OF INTEGER;
    VAR limit: ADDRESS;
    VAR r: REFANY;
    VAR ac := reader.readType();
  BEGIN
    nDim := RTType.GetNDimensions(ac);
    IF nDim > 0 THEN
      FOR i := 0 TO nDim-1 DO
        shape[i] := reader.readInt();
      END;
      r := RTAllocator.NewTracedArray(ac, SUBARRAY(shape, 0, nDim));
    ELSE
      r := RTAllocator.NewTraced(ac);
    END;
    reader.noteRef(r, id);
    reader.nextAddr := RTHeap.GetDataAdr(r);
    limit := reader.nextAddr + RTHeap.GetDataSize(r);
    <*FATAL ANY*> BEGIN
      RTTypeMap.WalkRef(r, RefFields, reader.visitor);
    END;
    (* Read remainder of the data fields *)
    EVAL Rd.GetSub(reader.rd,
                   SUBARRAY(LOOPHOLE(reader.nextAddr, ToChars)^,
                                     0, limit-reader.nextAddr));
    RETURN r
  END RootSpecialRead;


(* *)
(* Initialization *)
(* *)

PROCEDURE InitHeader() =
    VAR test: BITS 16 FOR [0..32767];
    TYPE EndianTest = ARRAY[0..1] OF BITS 8 FOR [0..255];
  BEGIN
    myHeader[HC.h1] := Header1;
    myHeader[HC.h2] := Header2;
    myHeader[HC.v] := Version;
    myHeader[HC.c] := CharRepISOLatin1;
    test := 1;
    IF LOOPHOLE(test, EndianTest)[0] = 1 THEN
      myHeader[HC.e] := LittleEndian
    ELSE
      myHeader[HC.e] := BigEndian;
    END;
    IF BITSIZE(INTEGER) = 16 THEN
      myHeader[HC.i] := IntSize16
    ELSIF BITSIZE(INTEGER) = 32 THEN
      myHeader[HC.i] := IntSize32
    ELSIF BITSIZE(INTEGER) = 64 THEN
      myHeader[HC.i] := IntSize64
    ELSE
      myHeader[HC.i] := '?'
    END;
    myHeader[HC.r] := RealRepNative;
    myTrailer[HT.t1] := Trailer1;
    myTrailer[HT.t2] := Trailer2;
  END InitHeader;

PROCEDURE InitSpecials() =
    VAR theRootSpecial: Special;
  BEGIN
    theRootSpecial := NEW(Special, sc := RT0.NilTypecode);
    specials := NEW(SpecialTable, RTType.MaxTypecode()+1);
    FOR i := 0 TO LAST(specials^) DO
      specials[i] := theRootSpecial;
    END;
  END InitSpecials;

BEGIN

  InitHeader();
  InitSpecials();
  nullReaderRef := NEW(REF INTEGER);
  
END Pickle.
