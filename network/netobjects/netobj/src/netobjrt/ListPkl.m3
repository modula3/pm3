(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* ListPkl.m3 *)
(* Last modified on Tue Mar  1 16:24:38 PST 1994 by wobber *)
(*      modified on Tue Sep  1 15:37:17 PDT 1992 by evers  *)


(* Iterative List Pickle.Special to prevent small stacks from 
   overflowing on RefList.Ts of length >~ 25. *)

UNSAFE MODULE ListPkl;

IMPORT RefList, Thread, Rd, Wr, Pickle;

PROCEDURE ListPklWrite (
    sp: Pickle.Special;
    r: REFANY; writer: Pickle.Writer)
    RAISES { Pickle.Error, Wr.Failure, Thread.Alerted } =
  VAR l: RefList.T := r; len := RefList.Length (l);
      isSubtype := TYPECODE(r) # TYPECODE(RefList.T);
  BEGIN
    Wr.PutChar (writer.wr, LOOPHOLE(isSubtype, CHAR));
    IF isSubtype THEN
      (* we don't know how to marshal subtypes of RefList.T *)
      Pickle.Special.write(sp, r, writer);
    ELSE
      writer.writeInt(len);
      FOR i := 1 TO len DO
        writer.write (l.head);
        l := l.tail;
      END;
      <* ASSERT l = NIL *>
    END;
  END ListPklWrite;

PROCEDURE ListPklRead (
    sp: Pickle.Special;
    reader: Pickle.Reader;
    id: Pickle.RefID) : REFANY
    RAISES { Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } = 
  VAR
    len: CARDINAL;
    res, tail: RefList.T;
    isSubtype := LOOPHOLE(Rd.GetChar(reader.rd), BOOLEAN);
  BEGIN
    IF isSubtype THEN
      (* the sender encountered a subtype of RefList.T *)
      res := Pickle.Special.read(sp, reader, id);
    ELSE
      len := reader.readInt();
      IF len < 0 THEN RAISE Pickle.Error("Pickle.Error: negative int"); END;
      res := NEW (RefList.T);
      tail := res;
      FOR i := 1 TO len - 1 DO
        tail.head := reader.read ();
        tail.tail := NEW (RefList.T);
        tail := tail.tail;
      END;
      tail.head := reader.read ();
      tail.tail := NIL;
    END;
    RETURN res;
  END ListPklRead;

BEGIN
  Pickle.RegisterSpecial (NEW (Pickle.Special, sc := TYPECODE (RefList.T),
    write := ListPklWrite,
    read  := ListPklRead));
END ListPkl.
