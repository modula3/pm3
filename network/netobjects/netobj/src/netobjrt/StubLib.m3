(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* StubLib.m3 *)
(* Last modified on Thu Sep  5 11:34:46 PDT 1996 by wobber *)
(*      modified on Tue Jan 31 08:47:30 PST 1995 by kalsow *)
(*      modified on Wed Feb 10 17:10:17 PST 1993 by owicki *)

UNSAFE MODULE StubLib;
   (* unsafe because of marshalling code *)
   
IMPORT NetObj, NetObjRep, NetObjRT, Pickle, Protocol, Transport,
       TransportUtils, Voucher, WireRep;
IMPORT Atom, AtomList, Rd, RTType, Wr, Text, TextF, Thread,
       RdClass, WrClass, UnsafeRd, UnsafeWr, FloatMode, Swap;

FROM Protocol IMPORT MsgHeader, CallHeader, Op;

REVEAL RdClass.Private <: MUTEX;
REVEAL WrClass.Private <: MUTEX;

(* most if not all of the following could be inline in stub code *)

(* Since clients of "Conn" must avoid accessing them concurrently,
   we operate on the embedded streams without locking them.
*)

TYPE ObjectStack = RECORD
    pos: CARDINAL := 0;
    objs: REF ARRAY OF NetObj.T := NIL;
  END;
  
CONST DefaultObjStackSize = 8;

REVEAL
  Conn = Transport.Conn BRANDED OBJECT
    objStack: ObjectStack := ObjectStack {};
    inObj: BOOLEAN := FALSE;
  END;

(* The field "t.objStack" is used to record the set of network objects
   marshalled during any single method invocation (at either client or
   server).  This record is required for cleanup at the termination of
   the call.   The "t.objStack" field is managed by the network object
   runtime and should not be modified by any transport implementation. *)

REVEAL
  Transport.T = TransportUtils.Public BRANDED OBJECT
  OVERRIDES
    serviceCall := ServiceCall;
  END;
  
(* Pickle.Reader and Pickle.Writer subtypes and free list headers *)

TYPE SpecWr = Pickle.Writer OBJECT
  c: Conn;
  next: SpecWr;
  END;

TYPE SpecRd = Pickle.Reader OBJECT
  c: Conn;
  rep: DataRep;
  next: SpecRd;
  END;

VAR mu: MUTEX;
    freeWr: SpecWr;
    freeRd: SpecRd;


PROCEDURE ServiceCall(<*UNUSED*> tt: Transport.T; c: Conn) : BOOLEAN
    RAISES {Thread.Alerted} =
  BEGIN
      TRY
        VAR
          dispatcher: Dispatcher;
          obj: NetObj.T;
          rd := c.rd;
          h := LOOPHOLE(ADR(rd.buff[rd.st+rd.cur-rd.lo]),
                                     UNTRACED REF CallHeader);
        BEGIN
          IF rd.hi - rd.cur < BYTESIZE(CallHeader) OR
                     h.hdr.private # ORD(Op.MethodCall) THEN
               RaiseUnmarshalFailure();
          END;
          INC(rd.cur, BYTESIZE(CallHeader));
          IF h.hdr.intFmt # NativeRep.intFmt THEN
            IF NOT NativeEndian(h.hdr) THEN
              h.prot := Swap.Swap4(h.prot);
            END;
          END;
          obj := NetObjRT.FindTarget(h.obj, h.prot, dispatcher);
          TRY
            c.objStack.pos := 0;
            dispatcher(c, obj, h.hdr, h.prot);
            IF (c.objStack.pos # 0) THEN
              c.wr.nextMsg();
              IF NOT rd.nextMsg() OR rd.hi - rd.cur < BYTESIZE(MsgHeader) THEN
                RETURN FALSE;
              END;
              VAR hh := LOOPHOLE(ADR(rd.buff[rd.st+rd.cur-rd.lo]),
                                              UNTRACED REF MsgHeader)^;
              BEGIN
                INC(rd.cur, BYTESIZE(MsgHeader));
                IF hh.hdr.private # ORD(Op.ResultAck) THEN RETURN FALSE; END;
              END;
            ELSE
              c.wr.nextMsg();
            END;
          FINALLY
            IF (c.objStack.pos # 0) THEN
              NetObjRT.Unpin(SUBARRAY(c.objStack.objs^, 0, c.objStack.pos));
              FOR i := 0 TO c.objStack.pos-1 DO
                c.objStack.objs[i] := NIL;
              END;
            END;
          END;
        END;
      EXCEPT
      | Rd.Failure, Wr.Failure => RETURN FALSE;
      | NetObj.Error(ec) =>
          TRY
            (* this test checks whether we have started marshalling results *)
            IF c.wr.cur = 0 THEN
              VAR wr := c.wr;
                  h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]),
                                             UNTRACED REF MsgHeader);
              BEGIN
                h.hdr := NativeRep;
                h.hdr.private := ORD(Op.CallFailed);
                INC(wr.cur, BYTESIZE(MsgHeader));
              END;
              OutRef(c, ec);
            END;
            c.wr.nextMsg();
          EXCEPT
          | Wr.Failure => RETURN FALSE;
          END;
      END;
    RETURN TRUE;
  END ServiceCall;
  

(* exports to StubLib *)

PROCEDURE StartCall(obj: NetObj.T; stubProt: StubProtocol) : Conn
    RAISES {NetObj.Error, Thread.Alerted} =
  VAR c := NARROW(obj.r, Transport.Location).new();
  BEGIN
    c.objStack.pos := 0;
    c.inObj := FALSE;
    VAR wr := c.wr;
        h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]),
                                 UNTRACED REF CallHeader);
    BEGIN
      <* ASSERT (wr.hi - wr.cur >= BYTESIZE(CallHeader)) *>
      INC(wr.cur, BYTESIZE(CallHeader));
      h.hdr := NativeRep;
      h.hdr.private := ORD(Op.MethodCall);
      h.prot := stubProt;
      h.obj := obj.w;
    END;
    RETURN c;
  END StartCall;
  
PROCEDURE AwaitResult(c: Conn) : DataRep
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure, Thread.Alerted} =
  VAR h: MsgHeader;
      rd := c.rd;
  BEGIN
    c.wr.nextMsg();
    TRY
      IF NOT rd.nextMsg() OR rd.hi - rd.cur < BYTESIZE(MsgHeader) THEN
        RaiseUnmarshalFailure();
      END;
      h := LOOPHOLE(ADR(rd.buff[rd.st+rd.cur-rd.lo]), UNTRACED REF MsgHeader)^;
      INC(rd.cur, BYTESIZE(MsgHeader));
    EXCEPT
    | Thread.Alerted => RAISE NetObj.Error(AtomList.List1(NetObj.Alerted));
    END;
    CASE h.hdr.private OF
    | ORD(Op.Return) =>
    | ORD(Op.CallFailed) => 
        RAISE NetObj.Error(InRef(c, h.hdr, TYPECODE(AtomList.T)));
    ELSE
      RaiseUnmarshalFailure();
    END;
    RETURN h.hdr;
  END AwaitResult;

PROCEDURE EndCall(c: Conn; reUse: BOOLEAN)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    TRY
      IF c.objStack.pos # 0 THEN
        NetObjRT.Unpin(SUBARRAY(c.objStack.objs^, 0, c.objStack.pos));
        FOR i := 0 TO c.objStack.pos-1 DO
          c.objStack.objs[i] := NIL;
        END;
      END;
      IF reUse AND c.inObj (* OR NOT UnsafeRd.FastEOF(c.rd) *) THEN
        VAR wr := c.wr;
            h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]),
                                       UNTRACED REF MsgHeader);
        BEGIN
          h.hdr := NativeRep;
          h.hdr.private := ORD(Op.ResultAck);
          INC(wr.cur, BYTESIZE(MsgHeader));
          wr.nextMsg();
        END;
      END;
    FINALLY
      c.loc.free(c, reUse);
    END;
  END EndCall;
  
PROCEDURE StartResult(c: Conn) =
  VAR wr := c.wr;
      h := LOOPHOLE(ADR(wr.buff[wr.st+wr.cur-wr.lo]),
                                 UNTRACED REF MsgHeader);
  BEGIN
    h.hdr := NativeRep;
    h.hdr.private := ORD(Op.Return);
    INC(wr.cur, BYTESIZE(MsgHeader));
  END StartResult;

PROCEDURE InChars(c: Conn; rep: DataRep; VAR arr: ARRAY OF CHAR)
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    IF rep.charSet # NativeRep.charSet THEN
      RaiseError(NetObj.UnsupportedDataRep);
    END;
    IF c.rd.getSub(arr) # NUMBER(arr) THEN
      RaiseUnmarshalFailure();
    END;
  END InChars;

PROCEDURE OutChars(c: Conn; READONLY arr: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    c.wr.putString(arr);
  END OutChars;

PROCEDURE InBytes(c: Conn; VAR arr: ARRAY OF Byte8)
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR p := LOOPHOLE(ADR(arr[0]), UNTRACED REF ARRAY [0..65335] OF CHAR);
  BEGIN
    IF c.rd.getSub(SUBARRAY(p^, 0, NUMBER(arr))) # NUMBER(arr) THEN
      RaiseUnmarshalFailure();
    END;
  END InBytes;

PROCEDURE OutBytes(c: Conn; READONLY arr: ARRAY OF Byte8)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR p := LOOPHOLE(ADR(arr[0]), UNTRACED REF ARRAY [0..65335] OF CHAR);
  BEGIN
    c.wr.putString(SUBARRAY(p^, 0, NUMBER(arr)));
  END OutBytes;

CONST
  BigEndianFmt   = 16;
  IntFmt32Little = 0;
  IntFmt64Little = 1;
  IntFmt32Big    = BigEndianFmt;
  IntFmt64Big    = BigEndianFmt + 1;

  FloatIEEE      = 0;
  FloatOther     = 1;

TYPE
  Int64 = ARRAY [0..1] OF Int32;

(* this code is integer-length dependent *)
(* we also rely on the invariant that MsgRd/MsgWr will
   provide contiguous 8-byte chunks at proper alignment ..
   as long as there is no intervening flush *)

PROCEDURE InInteger(c: Conn; 
                    rep: DataRep;
                    min := FIRST(INTEGER);
                    max := LAST(INTEGER)): INTEGER
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR i: INTEGER;
  BEGIN
    IF rep.intFmt = NativeRep.intFmt THEN
      i := LOOPHOLE(AlignRd(c.rd, BYTESIZE(INTEGER)), UNTRACED REF INTEGER)^;
      INC(c.rd.cur, BYTESIZE(INTEGER));
    ELSE
      CASE rep.intFmt OF
      | IntFmt32Little, IntFmt32Big =>
          VAR ii: Int32 :=
             LOOPHOLE(AlignRd(c.rd, BYTESIZE(Int32)), UNTRACED REF Int32)^;              BEGIN
            INC(c.rd.cur, BYTESIZE(Int32));
            IF NOT NativeEndian(rep) THEN ii := Swap.Swap4(ii); END;
            i := ii;
          END;
      | IntFmt64Little =>
          (* this can only be 64 -> 32 bit conversion *)
          (* no 64 -> 64 bit byte swap at this point in time *)
          VAR
            ip := LOOPHOLE(AlignRd(c.rd, BYTESIZE(Int64)), UNTRACED REF Int64);
          BEGIN
            INC(c.rd.cur, BYTESIZE(Int64));
            IF NativeEndian(rep) THEN
              i := ip[0];
            ELSE
              i := Swap.Swap4(ip[0]);
            END;
            (* Don't need to swap ip[1] to do this check, since -1 and
               0 are the same regardless *)
            IF (i < 0 AND ip[1] # -1) OR (i >= 0 AND ip[1] # 0) THEN
              RaiseError(NetObj.UnsupportedDataRep);
            END;
          END;
      ELSE
          RaiseError(NetObj.UnsupportedDataRep);
      END;
    END;
    IF i < min OR i > max THEN RaiseUnmarshalFailure(); END;
    RETURN i;
  END InInteger;

PROCEDURE InInt32(c: Conn; 
                    rep: DataRep;
                    min := FIRST(Int32);
                    max := LAST(Int32)): Int32
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR i: Int32;
  BEGIN
    IF rep.intFmt = NativeRep.intFmt THEN
      i := LOOPHOLE(AlignRd(c.rd, BYTESIZE(Int32)), UNTRACED REF Int32)^;
      INC(c.rd.cur, BYTESIZE(Int32));
    ELSE
      CASE rep.intFmt OF
      | IntFmt32Little, IntFmt32Big, IntFmt64Little =>
          i := LOOPHOLE(AlignRd(c.rd, BYTESIZE(Int32)), UNTRACED REF Int32)^;
          INC(c.rd.cur, BYTESIZE(Int32));
          IF NOT NativeEndian(rep) THEN i := Swap.Swap4(i); END;
      ELSE
          RaiseError(NetObj.UnsupportedDataRep);
      END;
    END;
    IF i < min OR i > max THEN RaiseUnmarshalFailure(); END;
    RETURN i;
  END InInt32;

PROCEDURE AlignRd(rd: Rd.T; nb: CARDINAL) : ADDRESS
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR diff := rd.cur MOD nb;
      res: ADDRESS;
  BEGIN
    (* here we rely on the alignment invariants of MsgRd.T *)
    IF diff # 0 THEN
      VAR n := rd.cur + nb - diff; BEGIN
        IF n > rd.hi THEN RaiseUnmarshalFailure(); END;
        rd.cur := n;
      END;
    END;
    IF rd.cur = rd.hi THEN EVAL rd.seek(rd.cur, FALSE); END;
    IF rd.hi - rd.cur < nb THEN RaiseUnmarshalFailure(); END;
    res := ADR(rd.buff[rd.st + rd.cur - rd.lo]);
    RETURN res;
  END AlignRd;

  (*
    A MsgRd fragment must be 64-bit aligned.  Fragments of types call, return,
    or call-failed, must either be a multiple of 8 bytes in length, or else
    contain the end-of-message.    MsgRd buffers must be 64-bit aligned in
    length.
   *)

PROCEDURE OutInteger(c: Conn; i: INTEGER)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR ip := LOOPHOLE(AlignWr(c.wr, BYTESIZE(INTEGER)), UNTRACED REF INTEGER);
  BEGIN
    ip^ := i;
    INC(c.wr.cur, BYTESIZE(INTEGER));
  END OutInteger;

PROCEDURE OutInt32(c: Conn; i: Int32)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR ip := LOOPHOLE(AlignWr(c.wr, BYTESIZE(Int32)), UNTRACED REF Int32);
  BEGIN
    ip^ := i;
    INC(c.wr.cur, BYTESIZE(Int32));
  END OutInt32;

PROCEDURE AlignWr(wr: Wr.T; align: CARDINAL) : ADDRESS
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR diff := wr.cur MOD align;
      res: ADDRESS;
  BEGIN
    (* here we rely on the alignment invariants of MsgWr.T *)
    IF diff # 0 THEN INC(wr.cur, align-diff); END;
    IF wr.cur = wr.hi THEN wr.seek(wr.cur); END;
    res := ADR(wr.buff[wr.st + wr.cur - wr.lo]);
    RETURN res;
  END AlignWr;

  (*
    A MsgWr fragment must be 64-bit aligned.  Fragments of types call, return,
    or call-failed, must either be a multiple of 8 bytes in length, or else
    contain the end-of-message.  MsgWr buffers must be 64-bit aligned in
    length.
   *)

PROCEDURE InByte(c: Conn; 
                 max := LAST(Byte8)): Byte8
     RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR b: Byte8;
  BEGIN
    TRY
      b := LOOPHOLE(UnsafeRd.FastGetChar(c.rd), Byte8);
    EXCEPT
    | Rd.EndOfFile => RaiseUnmarshalFailure();
    END;
    IF b > max THEN
      RaiseUnmarshalFailure();
    END;
    RETURN b
  END InByte;

PROCEDURE OutByte(c: Conn; b: Byte8)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    UnsafeWr.FastPutChar(c.wr, LOOPHOLE(b, CHAR));
  END OutByte;

TYPE MSpec = {Pickle, Text, NetObj, Reader, Writer, Texts};

PROCEDURE InRef(c: Conn; rep: DataRep; tc: INTEGER): REFANY
     RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR r: REFANY;
      srd: SpecRd;  
  BEGIN
    CASE InByte(c) OF
    | ORD(MSpec.Pickle) => 
      TRY
        srd := NewRd(c, rep);
        VAR ok := FALSE; BEGIN
          TRY
            r := srd.read();
            ok := TRUE;
          FINALLY
            IF ok THEN FreeRd(srd); END;
          END;
        END;
      EXCEPT
      | Rd.EndOfFile => RaiseUnmarshalFailure();
      | Pickle.Error(cause) =>
          RAISE NetObj.Error(
            AtomList.List2(UnmarshalFailure, Atom.FromText(cause)));
      END;
      IF tc # -1 AND NOT RTType.IsSubtype(TYPECODE(r), tc) THEN
        RaiseUnmarshalFailure();
      END;      
    | ORD(MSpec.Text) => r := InText(c, rep);
    | ORD(MSpec.NetObj) => r := InObject(c, tc);
    | ORD(MSpec.Reader) => r := InReader(c);
    | ORD(MSpec.Writer) => r := InWriter(c);
    | ORD(MSpec.Texts) => r := InTexts(c, rep);
    ELSE RaiseUnmarshalFailure();
    END;
    RETURN r;
  END InRef;

PROCEDURE OutRef(c: Conn; r: REFANY)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    TYPECASE r OF
    | TEXT(x) => OutByte(c, ORD(MSpec.Text)); OutText(c, x);
    | NetObj.T(x) => OutByte(c, ORD(MSpec.NetObj)); OutObject(c, x);
    | Rd.T(x) => OutByte(c, ORD(MSpec.Reader)); OutReader(c, x);
    | Wr.T(x) => OutByte(c, ORD(MSpec.Writer)); OutWriter(c, x);
    | REF ARRAY OF TEXT(x) => OutByte(c, ORD(MSpec.Texts)); OutTexts(c, x);
    ELSE 
      OutByte(c, ORD(MSpec.Pickle)); 
      TRY
        VAR swr := NewWr(c); ok := FALSE; BEGIN
          TRY
            swr.write(r);
            ok := TRUE;
          FINALLY
            IF ok THEN FreeWr(swr); END;
          END;
        END;
      EXCEPT
      | Pickle.Error(cause) =>
          RAISE Wr.Failure(AtomList.List1(Atom.FromText(cause)));
      END;
    END;
  END OutRef;

PROCEDURE InCardinal(c: Conn; rep: DataRep;
     lim: CARDINAL := LAST(CARDINAL)): CARDINAL
     RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN InInteger(c, rep, 0, lim);
  END InCardinal;

PROCEDURE OutCardinal(c: Conn; card: CARDINAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    OutInteger(c, card);
  END OutCardinal;

PROCEDURE InReal(c: Conn; rep: DataRep): REAL
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR i: REAL;
  BEGIN
    IF rep.floatFmt # NativeRep.floatFmt THEN
      RaiseError(NetObj.UnsupportedDataRep);
    END;
    IF c.rd.getSub(
        LOOPHOLE(i, ARRAY [0..BYTESIZE(REAL)-1] OF CHAR)) # BYTESIZE(REAL) THEN
      RaiseUnmarshalFailure();
    END;
    IF NOT NativeEndian(rep) THEN i := SwapReal(i); END;
    RETURN i;
  END InReal;

PROCEDURE OutReal(c: Conn; i: REAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    c.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(REAL)-1] OF CHAR));
  END OutReal;

PROCEDURE InLongreal(c: Conn; rep: DataRep): LONGREAL
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR i: LONGREAL;
  BEGIN
    IF rep.floatFmt # NativeRep.floatFmt THEN
      RaiseError(NetObj.UnsupportedDataRep);
    END;
    IF c.rd.getSub(
        LOOPHOLE(i, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR)) #
                       BYTESIZE(LONGREAL) THEN
      RaiseUnmarshalFailure();
    END;
    IF NOT NativeEndian(rep) THEN i := SwapLongReal(i); END;
    RETURN i;
  END InLongreal;

PROCEDURE OutLongreal(c: Conn; i: LONGREAL)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    c.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR));
  END OutLongreal;

PROCEDURE InExtended(c: Conn; rep: DataRep): EXTENDED
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN LOOPHOLE(InLongreal(c, rep), EXTENDED);
  END InExtended;

PROCEDURE OutExtended(c: Conn; i: EXTENDED)
   RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    c.wr.putString(LOOPHOLE(i, ARRAY [0..BYTESIZE(EXTENDED)-1] OF CHAR));
  END OutExtended;

PROCEDURE InBoolean(c: Conn) : BOOLEAN
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR res: BOOLEAN;
  BEGIN
    TRY
      res := UnsafeRd.FastGetChar(c.rd) # '\000';
    EXCEPT
    | Rd.EndOfFile => RaiseUnmarshalFailure();
    END;
    RETURN res;
  END InBoolean;

PROCEDURE OutBoolean(c: Conn; bool: BOOLEAN)
    RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF bool THEN
      UnsafeWr.FastPutChar(c.wr, '\001');
    ELSE
      UnsafeWr.FastPutChar(c.wr, '\000');
    END;
  END OutBoolean;

PROCEDURE InText(c: Conn; rep: DataRep) : TEXT
   RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR len: INTEGER;
  VAR text: TEXT;
  BEGIN
    len := InInt32(c, rep);
    IF len = -1 THEN
      RETURN NIL;
    ELSIF len < 0 THEN
      RaiseUnmarshalFailure();
    ELSE
      text := NEW(TEXT, len+1);
      InChars(c, rep, SUBARRAY(text^, 0, len));
      text[len] := '\000';
    END;
    RETURN text;
  END InText;

PROCEDURE OutText(c: Conn; text: TEXT)
   RAISES {Wr.Failure, Thread.Alerted} =
  VAR len: INTEGER;
  BEGIN
    IF text # NIL THEN
      len := Text.Length(text);
    ELSE
      len := -1;
    END;
    OutInt32(c, len);
    IF len > 0 THEN OutChars(c, SUBARRAY(text^, 0, len)); END;
  END OutText;

PROCEDURE InTexts(c: Conn; rep: DataRep) : REF ARRAY OF TEXT
   RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR n: CARDINAL;
  VAR rt: REF ARRAY OF TEXT;
  BEGIN
    n := InInt32(c, rep, 0);
    IF n = 0 THEN
      RETURN NIL;
    END;
    rt := NEW(REF ARRAY OF TEXT, n);
    IF n > 0 THEN
      FOR i := 0 TO n-1 DO rt[i] := InText(c, rep); END;
    END;
    RETURN rt;
  END InTexts;

PROCEDURE OutTexts(c: Conn; rt: REF ARRAY OF TEXT)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR n: CARDINAL;
  BEGIN
    IF (rt = NIL) THEN n := 0 ELSE n := NUMBER(rt^); END;
    OutInt32(c, n);
    IF n > 0 THEN
      FOR i := 0 TO n-1 DO OutText(c, rt[i]); END;
    END;
  END OutTexts;

TYPE
  VT = Voucher.T OBJECT
    stream: REFANY;
  OVERRIDES
    claimRd := ClaimRd;
    claimWr := ClaimWr;
  END;

(*
PROCEDURE CleanupVT(<*UNUSED*> READONLY w: WeakRef.T; r: REFANY) =
  VAR v := NARROW(r, VT);
    <*FATAL Thread.Alerted*>
  BEGIN
    TRY
      TYPECASE v.stream OF
      | Rd.T(rd) => Rd.Close(rd);
      | Wr.T(wr) => Wr.Close(wr);
      ELSE
      END;
    EXCEPT
    | Rd.Failure, Wr.Failure =>
    END;
  END CleanupVT;
*)

PROCEDURE InReader(c: Conn) : Rd.T
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR obj: NetObj.T := NIL;
      rd: Rd.T := NIL;
  BEGIN
    obj := InObject(c);
    TYPECASE obj OF
    | NULL =>
    | Voucher.T(v) =>  rd := v.claimRd();
    ELSE
        RaiseUnmarshalFailure();
    END;
    RETURN rd;
  END InReader;

PROCEDURE OutReader(c: Conn; rd: Rd.T)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR obj: NetObj.T := NIL;
  BEGIN
    IF rd # NIL THEN
      obj := NEW(VT, stream := rd);
      (* EVAL WeakRef.FromRef(obj, CleanupVT); *)
    END;
    OutObject(c, obj);
  END OutReader;

PROCEDURE InWriter(c: Conn) : Wr.T
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR obj: NetObj.T := NIL;
      wr: Wr.T := NIL;
  BEGIN
    obj := InObject(c);
    TYPECASE obj OF
    | NULL => 
    | Voucher.T(v) =>  wr := v.claimWr();
    ELSE
        RaiseUnmarshalFailure();
    END;
    RETURN wr;
  END InWriter;

PROCEDURE OutWriter(c: Conn; wr: Wr.T)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR obj: NetObj.T := NIL;
  BEGIN
    IF wr # NIL THEN
      obj := NEW(VT, stream := wr);
      (* EVAL WeakRef.FromRef(obj, CleanupVT); *)
    END;
    OutObject(c, obj);
  END OutWriter;

PROCEDURE ClaimRd(v: VT) : Rd.T =
  BEGIN
    TYPECASE v.stream OF
    | Rd.T(rd) => RETURN rd;
    ELSE RETURN NIL;
    END;
  END ClaimRd;
  
PROCEDURE ClaimWr(v: VT) : Wr.T =
  BEGIN
    TYPECASE v.stream OF
    | Wr.T(wr) => RETURN wr;
    ELSE RETURN NIL;
    END;
  END ClaimWr;

CONST Align64 = 8;   (* to achieve 64-bit alignment *)

PROCEDURE OutObject (c: Conn; o: NetObj.T)
    RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    (* we believe that 16-byte records are 32-bit aligned!! *)
    wr := c.wr;
    pwrep := LOOPHOLE(AlignWr(wr, Align64),
                               UNTRACED REF WireRep.T);
  BEGIN
    IF wr.hi - wr.cur >= BYTESIZE(WireRep.T) THEN
      IF o = NIL THEN
        pwrep^ := WireRep.NullT;
      ELSE
        pwrep^ := NetObjRT.InsertAndPin(o);
      END;
      INC(wr.cur, BYTESIZE(WireRep.T));
    ELSE
      IF o = NIL THEN
        OutBytes(c, WireRep.NullT.byte);
      ELSE
        OutBytes(c, NetObjRT.InsertAndPin(o).byte);
      END;
    END;
    VAR s := c.objStack.objs; BEGIN
      IF s = NIL THEN
        s := NEW(REF ARRAY OF NetObj.T, DefaultObjStackSize);
        c.objStack.objs := s;
      ELSIF c.objStack.pos = NUMBER(s^) THEN
        s := NEW(REF ARRAY OF NetObj.T, 2 * c.objStack.pos);
        SUBARRAY(s^, 0, c.objStack.pos) := c.objStack.objs^;
        c.objStack.objs := s;
      END;
      s[c.objStack.pos] := o;
      INC(c.objStack.pos);
    END;
  END OutObject;

PROCEDURE InObject (c: Conn; tc: INTEGER := -1): NetObj.T
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted} =
  VAR
    o: NetObj.T;
    rd := c.rd;
    pwrep := LOOPHOLE(AlignRd(rd, Align64),
                               UNTRACED REF WireRep.T);
  BEGIN
    IF rd.hi - rd.cur >= BYTESIZE(WireRep.T) THEN
      INC(rd.cur, BYTESIZE(WireRep.T));
      IF pwrep^ = WireRep.NullT THEN RETURN NIL END;
      o := NetObjRT.Find(pwrep^, c.loc);
    ELSE
      VAR w: WireRep.T; BEGIN
        InBytes(c, w.byte);
        IF w = WireRep.NullT THEN RETURN NIL END;
        o := NetObjRT.Find(w, c.loc);
      END;
    END;
    IF tc # -1 AND NOT RTType.IsSubtype(TYPECODE(o), tc) THEN
      RaiseUnmarshalFailure();
    END;
    c.inObj := TRUE;
    RETURN o;
  END InObject;

 
(* Procedures for Pickling -- free list management *)

PROCEDURE NewWr(c: Conn): SpecWr = 
  VAR pwr: SpecWr;
  BEGIN
    LOCK mu DO
      IF freeWr # NIL THEN
        pwr := freeWr; freeWr := freeWr.next;
      ELSE
        pwr := NEW(SpecWr, next:=NIL);
      END;
    END;
    pwr.wr := c.wr;
    pwr.c := c;
    RETURN pwr
  END NewWr;

PROCEDURE FreeWr(pwr: SpecWr) =
  BEGIN
    LOCK mu DO
      pwr.next := freeWr;
      pwr.c := NIL;
      pwr.wr := NIL;
      freeWr := pwr;
    END;
  END FreeWr;

PROCEDURE NewRd(c: Conn; rep: DataRep): SpecRd = 
  VAR prd: SpecRd;
  BEGIN
    LOCK mu DO
      IF freeRd # NIL THEN
        prd := freeRd; freeRd := freeRd.next;
      ELSE
        prd := NEW(SpecRd, next:=NIL);
      END;
    END;
    prd.rd := c.rd;
    prd.c := c;
    prd.rep := rep;
    RETURN prd
  END NewRd;

PROCEDURE FreeRd(prd: SpecRd) =
  BEGIN
    LOCK mu DO
      prd.c := NIL;
      prd.rd := NIL;
      prd.next := freeRd;
      freeRd := prd;
    END;
  END FreeRd;

(* Pickle Special routines *)

PROCEDURE OutSpecial(self: Pickle.Special; 
                 r: REFANY; 
                 writer: Pickle.Writer) 
  RAISES  {Pickle.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    TYPECASE writer OF
    | SpecWr =>
        VAR c := NARROW(writer, SpecWr).c; BEGIN
          OutRef(c, r);  
        END;
     ELSE 
      TYPECASE r OF
      | NetObj.T(x) =>
          IF NOT ISTYPE(x.r, Transport.Location) THEN
            (* This will gratuitously pickle the ExportInfo ref
               embedded in x.r.  It would be better to exclude this
               if and when possible, but it shouldn't hurt for now. *)
            Pickle.Special.write(self, r, writer);
          ELSE
            RAISE Pickle.Error("Can't pickle a surrogate object");
          END;
      ELSE RAISE Pickle.Error("Can't Pickle Rd.T or Wr.T");
      END;
    END;
  END OutSpecial;

PROCEDURE InSpecial(self: Pickle.Special; 
                reader: Pickle.Reader;
                id: Pickle.RefID): REFANY
  RAISES { Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    TRY
      TYPECASE reader OF
      | SpecRd =>
          VAR c := NARROW(reader, SpecRd).c; BEGIN
            RETURN InRef(c, NARROW(reader, SpecRd).rep, self.sc);
          END;
      ELSE
        TYPECASE Pickle.Special.read(self, reader, id) OF
        | NetObj.T(x) =>
            x.w := WireRep.NullT;
            x.r := NIL;
            RETURN x;
        ELSE RAISE Pickle.Error("Can't Unpickle Rd.T or Wr.T");
        END;
      END;
    EXCEPT
      NetObj.Error(cause) => RAISE Pickle.Error(Atom.ToText(cause.head));
    END;
  END InSpecial;

PROCEDURE RaiseUnmarshalFailure() RAISES {NetObj.Error} =
  BEGIN
    RaiseError(UnmarshalFailure);
  END RaiseUnmarshalFailure;

PROCEDURE RaiseCommFailure(ec: AtomList.T) RAISES {NetObj.Error} =
  BEGIN
    RAISE NetObj.Error(AtomList.Cons(NetObj.CommFailure, ec));
  END RaiseCommFailure;

PROCEDURE RaiseError(a: Atom.T) RAISES {NetObj.Error} =
  BEGIN
    RAISE NetObj.Error(AtomList.List1(a));
  END RaiseError;

(*
PROCEDURE Swap32(i: Int32) : Int32 =
  VAR res: Int32;
  BEGIN
    WITH p = LOOPHOLE(ADR(i), UNTRACED REF ARRAY [0..3] OF Byte8) DO
      WITH r = LOOPHOLE(ADR(res), UNTRACED REF ARRAY [0..3] OF Byte8) DO
        r[0] := p[3];
        r[1] := p[2];
        r[2] := p[1];
        r[3] := p[0];
      END;
    END;
    RETURN res;
  END Swap32;
*)

PROCEDURE SwapReal(i: REAL) : REAL =
  BEGIN
    RETURN LOOPHOLE(Swap.Swap4(LOOPHOLE(i, Int32)), REAL);
  END SwapReal;

TYPE LR = RECORD a, b: Int32; END;

PROCEDURE SwapLongReal(i: LONGREAL) : LONGREAL =
  VAR res: LONGREAL;
  BEGIN
    WITH p = LOOPHOLE(ADR(i), UNTRACED REF LR) DO
      WITH r = LOOPHOLE(ADR(res), UNTRACED REF LR) DO
        r.a := Swap.Swap4(p.b);
        r.b := Swap.Swap4(p.a);
      END;
    END;
    RETURN res;
  END SwapLongReal;

PROCEDURE NativeEndian(rep: DataRep) : BOOLEAN =
  BEGIN
    RETURN (rep.intFmt >= BigEndianFmt) = (Swap.endian = Swap.Endian.Big);
  END NativeEndian;

PROCEDURE ChooseIntFmt(): Byte8 =
  BEGIN
    IF BYTESIZE(INTEGER) = 8 THEN
      IF Swap.endian = Swap.Endian.Little THEN
        RETURN IntFmt64Little;
      ELSE
        RETURN IntFmt64Big;
      END;
    ELSE
      IF Swap.endian = Swap.Endian.Little THEN
        RETURN IntFmt32Little;
      ELSE
        RETURN IntFmt32Big;
      END;
    END;
  END ChooseIntFmt;

PROCEDURE ChooseFloatFmt(): Byte8 =
  BEGIN
    IF FloatMode.IEEE THEN
      RETURN FloatIEEE;
    ELSE
      RETURN FloatOther;
    END;
  END ChooseFloatFmt;

BEGIN
  NativeRep := DataRep{private := 0,
                       intFmt := ChooseIntFmt(),
                       charSet := 0,
                       floatFmt := ChooseFloatFmt()};
  UnmarshalFailure := Atom.FromText("NetObj.UnmarshalFailure");

  (* Initialization for Pickle specials and free list *)
  mu := NEW(MUTEX);
  Pickle.RegisterSpecial(
      NEW(Pickle.Special, sc:= TYPECODE(NetObj.T),
                          write := OutSpecial,
                          read := InSpecial));
  Pickle.RegisterSpecial(
      NEW(Pickle.Special, sc:= TYPECODE(Rd.T),
                          write := OutSpecial,
                          read := InSpecial));
  Pickle.RegisterSpecial(
      NEW(Pickle.Special, sc:= TYPECODE(Wr.T),
                          write := OutSpecial,
                          read := InSpecial));

END StubLib.
