(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* ListPkl.m3 *)
(* Last modified on Thu Apr  1 17:29:02 PST 1993 by wobber *)
(*      modified on Tue Feb 16 23:54:36 PST 1993 by owicki *)
(*      modified on Tue Sep  1 15:37:17 PDT 1992 by evers  *)

MODULE AtomPkl;

IMPORT Atom, Pickle, Rd, Thread, Wr;

FROM Pickle IMPORT Error, RefID, Special, Reader, Writer;

PROCEDURE AtomPklWrite (<*UNUSED*> sp: Special;  r: REFANY;  pwr: Writer)
  RAISES {Error, Wr.Failure, Thread.Alerted } =
  BEGIN
    pwr.write (Atom.ToText (r));
  END AtomPklWrite;

PROCEDURE AtomPklRead (<*UNUSED*> sp: Special;  prd: Reader;
                       <*UNUSED*> id: RefID): REFANY
  RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  BEGIN
    RETURN Atom.FromText (prd.read ());
  END AtomPklRead;

BEGIN
  Pickle.RegisterSpecial (NEW (Special, sc := TYPECODE (Atom.T),
                               write := AtomPklWrite, read  := AtomPklRead));
END AtomPkl.
