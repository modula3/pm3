(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Created by Carsten Weich                                    *)
(*                                                             *)
(* Last modified on Thu Jan 19 13:13:18 PST 1995 by kalsow     *)
(*      modified on Tue Sep 27 19:18:12 PDT 1994 by weich      *)

UNSAFE MODULE StableLog;

IMPORT Rd, Wr, WrClass, RdClass, Text, TextF, 
       Thread, Pickle, 
       StableError, RdUtils, Word;

REVEAL
  WrClass.Private <: MUTEX;
  RdClass.Private <: MUTEX;

<*FATAL Thread.Alerted*>

CONST
  CallMark     = 27353;  (* this is Gregs birthday... *)
  EndCallMark  = 30965;  (* ...Carstens birthday...*)

  Mtext        = ORD('t');
  Mpickle      = ORD('P');

PROCEDURE OutCall (log: Wr.T; procId: CARDINAL) =
  BEGIN
    OutInteger(log, CallMark);
    OutInteger(log, procId)
  END OutCall;

PROCEDURE OutCallEndMark (log: Wr.T) =
  BEGIN
    OutInteger(log, EndCallMark)
  END OutCallEndMark;

PROCEDURE InCall (log: Rd.T; max: CARDINAL): CARDINAL RAISES {Error} =
  BEGIN
    IF InInteger(log) # CallMark THEN RAISE Error END;
    VAR res:= InInteger(log); BEGIN
      IF res > max OR res < 0 THEN RAISE Error END;
      RETURN res
    END
  END InCall;

PROCEDURE CheckCallEndMark (log: Rd.T): BOOLEAN =
  BEGIN
    TRY
      RETURN EndCallMark = InInteger(log)
    EXCEPT
      Error => RETURN FALSE
    END
  END CheckCallEndMark;

PROCEDURE OutRef(log: Wr.T; r: REFANY) =
  BEGIN
    TYPECASE r OF
    | TEXT(x) => OutInteger(log, Mtext); OutText(log, x);
    ELSE
      TRY
        OutInteger(log, Mpickle);
        Pickle.Write(log, r)
      EXCEPT
        Wr.Failure (err) =>
        StableError.Halt(
            "Cannot write to logfile: " & RdUtils.FailureText(err))
      | Pickle.Error (msg) =>
        StableError.Halt("Cannot write to logfile: Pickle error: " & msg)
      END
    END
  END OutRef;

PROCEDURE InRef(log: Rd.T): REFANY RAISES {Error} =
  VAR r: REFANY;
      code:= InInteger(log);
  BEGIN
    IF code = Mpickle THEN
      TRY
        r:= Pickle.Read(log)
      EXCEPT
      | Pickle.Error, Rd.EndOfFile => RAISE Error
      | Rd.Failure (err) =>
        StableError.Halt(
            "Can not read log file: " & RdUtils.FailureText(err))
      END
    ELSIF code = Mtext THEN
      r:= InText(log)
    ELSE
      RAISE Error
    END;
    RETURN r
  END InRef;


(* Procedures for generic logging of procedure parameters *)

PROCEDURE OutChar (log: Wr.T; c: CHAR) =
  BEGIN
    TRY
      Wr.PutChar(log, c)
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutChar;

PROCEDURE OutChars (log: Wr.T; READONLY chars: ARRAY OF CHAR) =
  VAR n:= NUMBER(chars) - NUMBER(chars) MOD BYTESIZE(Word.T);
  BEGIN
    TRY
      Wr.PutString(log, SUBARRAY(chars, 0, n));
      FOR i:= n TO LAST(chars) DO
        Wr.PutChar(log, chars[i])
      END
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutChars;

(* "OutInteger(log, i)" skips to the next word aligned byte in log,
   and then stores "i" by loopholing. See "AlignWr".
*)
PROCEDURE OutInteger (log: Wr.T; i: INTEGER) =
  BEGIN
    LOCK log DO
      VAR
        ip := LOOPHOLE(AlignWr(log, BYTESIZE(INTEGER)),
                       UNTRACED REF INTEGER);
      BEGIN
        ip^ := i;
        INC(log.cur, BYTESIZE(INTEGER))
      END
    END (*LOCK*)
  END OutInteger;

PROCEDURE OutCardinal (log: Wr.T; card: CARDINAL) =
  BEGIN
    OutInteger(log, card)
  END OutCardinal;

PROCEDURE OutBoolean (log: Wr.T; bool: BOOLEAN) =
  BEGIN
    TRY
      Wr.PutChar(log, VAL(ORD(bool), CHAR))
    EXCEPT
      Wr.Failure (err) =>
        StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END OutBoolean;

PROCEDURE OutReal (log: Wr.T; r: REAL) =
  BEGIN
    LOCK log DO
      VAR
        rp := LOOPHOLE(AlignWr(log, BYTESIZE(REAL)),
                       UNTRACED REF REAL);
      BEGIN
        rp^ := r;
        INC(log.cur, BYTESIZE(REAL))
      END
    END
  END OutReal;

PROCEDURE OutLongreal (log: Wr.T; r: LONGREAL) =
  BEGIN
    LOCK log DO
      VAR
        rp := LOOPHOLE(AlignWr(log, BYTESIZE(LONGREAL)),
                       UNTRACED REF LONGREAL);
      BEGIN
        rp^ := r;
        INC(log.cur, BYTESIZE(LONGREAL))
      END
    END
  END OutLongreal;

PROCEDURE OutExtended (log: Wr.T; r: EXTENDED) =
  BEGIN
    LOCK log DO
      VAR
        rp := LOOPHOLE(AlignWr(log, BYTESIZE(EXTENDED)),
                       UNTRACED REF EXTENDED);
      BEGIN
        rp^ := r;
        INC(log.cur, BYTESIZE(EXTENDED))
      END
    END
  END OutExtended;

PROCEDURE OutText(log: Wr.T; text: TEXT) =
  VAR len: INTEGER;
  BEGIN
    IF text # NIL THEN
      len := Text.Length(text)
    ELSE
      len := -1
    END;
    OutInteger(log, len);
    IF len > 0 THEN OutChars(log, SUBARRAY(text^, 0, len)) END
  END OutText;

PROCEDURE AlignWr(wr: Wr.T; align: CARDINAL) : ADDRESS =
  VAR diff := wr.cur MOD align;
      res: ADDRESS;
  BEGIN
    TRY
      (* here we rely on the alignment invariants *)
      IF diff # 0 THEN INC(wr.cur, align-diff) END;
      IF wr.cur = wr.hi THEN wr.seek(wr.cur) END;
      res := ADR(wr.buff[wr.st + wr.cur - wr.lo]);
      RETURN res
    EXCEPT
      Wr.Failure (err) =>
        <*NOWARN*> StableError.Halt(
          "Cannot write to logfile: " & RdUtils.FailureText(err))
    END
  END AlignWr;


(* The following procedures are provided in support of generic reading the
   log. *)

PROCEDURE InChar (log: Rd.T): CHAR RAISES {Error} =
  BEGIN
    TRY
      RETURN Rd.GetChar(log)
    EXCEPT
      Rd.EndOfFile => RAISE Error
    | Rd.Failure (err) =>
        <*NOWARN*> StableError.Halt(
          "Can not read log file: " & RdUtils.FailureText(err))
    END
  END InChar;

PROCEDURE InCharsLen (log: Rd.T): CARDINAL RAISES {Error} =
  BEGIN
    RETURN InInteger(log)
  END InCharsLen;

PROCEDURE InChars (    log  : Rd.T;
                   VAR chars: ARRAY OF CHAR)
  RAISES {Error} =
  VAR n:= NUMBER(chars) - NUMBER(chars) MOD BYTESIZE(Word.T);
  BEGIN
    TRY
      IF Rd.GetSub(log, SUBARRAY(chars, 0, n)) # n THEN
        RAISE Error
      END;
      FOR i:= n TO LAST(chars) DO
        chars[i]:= Rd.GetChar(log)
      END
    EXCEPT
      Rd.EndOfFile => RAISE Error
    | Rd.Failure (err) =>
        StableError.Halt("Can not read log file: "
                           & RdUtils.FailureText(err))
    END
  END InChars;

PROCEDURE InInteger (log: Rd.T;
                     min         := FIRST(INTEGER);
                     max         := LAST(INTEGER)   ):
  INTEGER RAISES {Error} =
  BEGIN
    LOCK log DO
      VAR i: INTEGER;
      BEGIN
        i := LOOPHOLE(AlignRd(log, BYTESIZE(INTEGER)),
                      UNTRACED REF INTEGER)^;
        INC(log.cur, BYTESIZE(INTEGER));
        IF min <= i AND i <= max THEN
          RETURN i
        ELSE
          RAISE Error
        END (*IF*)
      END
    END (*LOCK*)
  END InInteger;

PROCEDURE InCardinal (log: Rd.T;
                      lim: CARDINAL := LAST(CARDINAL)):
  CARDINAL RAISES {Error} =
  BEGIN
    RETURN InInteger(log, 0, lim)
  END InCardinal;

PROCEDURE InBoolean (log: Rd.T): BOOLEAN RAISES {Error} =
  BEGIN
    TRY
      RETURN Rd.GetChar(log) = VAL(ORD(TRUE), CHAR)
    EXCEPT
      Rd.EndOfFile => RAISE Error
    | Rd.Failure (err) =>
        <*NOWARN*> StableError.Halt(
          "Can not read log file: " & RdUtils.FailureText(err))
    END
  END InBoolean;

PROCEDURE InReal (log: Rd.T): REAL RAISES {Error} =
  BEGIN
    LOCK log DO
      VAR r: REAL;
      BEGIN
        r := LOOPHOLE(AlignRd(log, BYTESIZE(REAL)),
                      UNTRACED REF REAL)^;
        INC(log.cur, BYTESIZE(REAL));
        RETURN r
      END
    END
  END InReal;

PROCEDURE InLongreal (log: Rd.T): LONGREAL RAISES {Error} =
  BEGIN
    LOCK log DO
      VAR r: LONGREAL;
      BEGIN
        r := LOOPHOLE(AlignRd(log, BYTESIZE(LONGREAL)),
                      UNTRACED REF LONGREAL)^;
        INC(log.cur, BYTESIZE(LONGREAL));
        RETURN r
      END
    END
  END InLongreal;

PROCEDURE InExtended (log: Rd.T): EXTENDED RAISES {Error} =
  BEGIN
    LOCK log DO
      VAR r: EXTENDED;
      BEGIN
        r := LOOPHOLE(AlignRd(log, BYTESIZE(EXTENDED)),
                      UNTRACED REF EXTENDED)^;
        INC(log.cur, BYTESIZE(EXTENDED));
        RETURN r
      END
    END
  END InExtended;

PROCEDURE InText(log: Rd.T) : TEXT
   RAISES {Error} =
  VAR len: INTEGER;
      text: TEXT;
  BEGIN
    len := InInteger(log);
    IF len = -1 THEN
      RETURN NIL
    ELSIF len < 0 THEN
      RAISE Error
    ELSE
      text := NEW(TEXT, len+1);
      InChars(log, SUBARRAY(text^, 0, len));
      text[len] := '\000';
      RETURN text
    END
  END InText;

PROCEDURE AlignRd(rd: Rd.T; nb: CARDINAL) : ADDRESS
    RAISES {Error} =
  VAR diff := rd.cur MOD nb;
      res: ADDRESS;
  BEGIN
    (* here we rely on the alignment invariants *)
    IF diff # 0 THEN
      VAR n := rd.cur + nb - diff; BEGIN
        IF n > rd.hi THEN RAISE Error END;
        rd.cur := n;
      END;
    END;
    IF rd.cur = rd.hi THEN
      TRY
        VAR sres:= rd.seek(rd.cur, FALSE); BEGIN
          IF sres = RdClass.SeekResult.Eof THEN
            RAISE Error
          END
        END
      EXCEPT
      | Rd.Failure (err) =>
        StableError.Halt(
            "Can not read log file: " & RdUtils.FailureText(err))
      END (*TRY*)
    END; (*IF*)
    res := ADR(rd.buff[rd.st + rd.cur - rd.lo]);
    RETURN res
  END AlignRd;

BEGIN
END StableLog.
