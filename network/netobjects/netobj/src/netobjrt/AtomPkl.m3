(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* ListPkl.m3 *)
(* Last modified on Thu Apr  1 17:29:02 PST 1993 by wobber *)
(*      modified on Tue Feb 16 23:54:36 PST 1993 by owicki *)
(*      modified on Tue Sep  1 15:37:17 PDT 1992 by evers  *)


(* Atom Pickle.Special *)

MODULE AtomPkl;

IMPORT Atom, Thread, Rd, Wr, Pickle;

PROCEDURE AtomPklWrite (
    <*UNUSED*> sp: Pickle.Special;
    r: REFANY; writer: Pickle.Writer)
    RAISES { Pickle.Error, Wr.Failure, Thread.Alerted } =
  BEGIN
    writer.write(Atom.ToText(r));
  END AtomPklWrite;

PROCEDURE AtomPklRead (
    <*UNUSED*> sp: Pickle.Special;
    reader: Pickle.Reader;
    <*UNUSED*> id: Pickle.RefID) : REFANY
    RAISES { Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    RETURN Atom.FromText(reader.read());
  END AtomPklRead;

BEGIN
  Pickle.RegisterSpecial (NEW (Pickle.Special, sc := TYPECODE (Atom.T),
    write := AtomPklWrite,
    read  := AtomPklRead));
END AtomPkl.
