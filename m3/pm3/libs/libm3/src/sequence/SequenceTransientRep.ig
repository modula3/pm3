(* Copyright 1993 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)

(* This interface defines the representation of generic transient
   sequences. *)

GENERIC INTERFACE SequenceTransientRep(Elem, Seq);
(* Where "Seq = Sequence(Elem)". *)

REVEAL Seq.T <: Public;

TYPE
  RefArray = <*TRANSIENT*> REF ARRAY OF Elem.T;
  Public = Seq.Public OBJECT
    elem: RefArray := NIL;
    st: CARDINAL := 0;
    sz: CARDINAL := 0
  END;

(* Element "i" of "s" is stored in

| s.elem[(s.st + i) MOD NUMBER(s.elem^)].

A sequence "s" satisfies the invariant that

| (s.elem # NIL) AND (s.sz <= NUMBER(s.elem^))
| AND (s.size() = s.sz) AND (NUMBER(s.elem^) > 0)
| AND (s.st < NUMBER(s.elem^))

*)

TYPE EArr = ARRAY OF Elem.T;

END SequenceTransientRep.
