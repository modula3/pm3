(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* ListPkl.m3 *)
(* Last modified on Tue Mar  1 16:24:38 PST 1994 by wobber *)
(*      modified on Tue Sep  1 15:37:17 PDT 1992 by evers  *)

(* Iterative List Special to prevent small stacks from 
   overflowing on RefList.Ts of length >~ 25. *)

UNSAFE MODULE ListPkl;

IMPORT Pickle, PickleStubs, Rd, RefList, Thread, Wr;

FROM Pickle IMPORT Error, RefID, Special, Reader, Writer;

PROCEDURE ListPklWrite (sp: Special;  r: REFANY;  pwr: Writer)
  RAISES { Error, Wr.Failure, Thread.Alerted } =
  VAR
    l: RefList.T := r;
    len := RefList.Length (l);
  BEGIN
    IF PutSubtypeMark (r, TYPECODE (RefList.T), pwr) THEN
      (* we don't know how to marshal subtypes of RefList.T *)
      Special.write (sp, r, pwr);
    ELSE
      PickleStubs.OutCardinal (pwr, len);
      FOR i := 1 TO len DO
        pwr.write (l.head);
        l := l.tail;
      END;
      <* ASSERT l = NIL *>
    END;
  END ListPklWrite;

PROCEDURE ListPklRead (sp: Special;  prd: Reader;  id: RefID) : REFANY
  RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } = 
  VAR
    len: CARDINAL;
    res, tail: RefList.T;
  BEGIN
    IF GetSubtypeMark (prd) THEN
      (* the writer encountered a subtype of RefList.T *)
      res := Special.read (sp, prd, id);
    ELSE
      len := PickleStubs.InCardinal (prd);
      res := NEW (RefList.T);
      tail := res;
      FOR i := 1 TO len - 1 DO
        tail.head := prd.read ();
        tail.tail := NEW (RefList.T);
        tail := tail.tail;
      END;
      tail.head := prd.read ();
      tail.tail := NIL;
    END;
    RETURN res;
  END ListPklRead;

(*------------------------------------------------------------- internal ---*)

PROCEDURE PutSubtypeMark (r: REFANY;  tcode: INTEGER;  pwr: Writer): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR isSubtype := TYPECODE (r) # tcode;
  BEGIN
    Wr.PutChar (pwr.wr, VAL (ORD (isSubtype), CHAR));
    RETURN isSubtype;
  END PutSubtypeMark;

PROCEDURE GetSubtypeMark (prd: Reader): BOOLEAN
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  CONST TrueMark = VAL (ORD (TRUE), CHAR); 
  BEGIN
    RETURN Rd.GetChar (prd.rd) = TrueMark;
  END GetSubtypeMark;

BEGIN
  Pickle.RegisterSpecial (NEW (Special, sc := TYPECODE (RefList.T),
                               write := ListPklWrite, read  := ListPklRead));
END ListPkl.
