(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

(* Last modified on Wed Feb 10 17:14:04 PST 1993 by owicki  *)

(*

  Some common Modula-3 types require special handling during pickling.  This
  module provides that handling for Atom.T.

  Values of type "Atom.T" with any given name are expected to be unique within
  the process.  The pickle specials make sure that this property is
  maintained.

  If a program contains an import of this interface, a "Pickle.Special" with
  "sp.sc = TYPECODE (Atom.T)" will be passed to "Pickle.RegisterSpecial"
  during initialization.  This special pickles references "r" with "TYPECODE
  (r) = TYPECODE (Atom.T)" by passing the text obtained from Atom.Name(r).

*)

INTERFACE AtomPkl;

END AtomPkl.
