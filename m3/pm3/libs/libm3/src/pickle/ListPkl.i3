(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

(* Last modified on Tue Sep  1 15:42:30 PDT 1992 by evers  *)

(*

  Some common Modula-3 types require special handling during pickling.  This
  module provides that handling for RefList.T.

  Values of type "RefList.T" (and any other type derived from "List.ig") may
  contain long chains of nodes which cause the recursive pickler to overflow
  its stack.  The pickle specials iterate over the list without recursing on
  the stack.  Note that subtypes of "RefList.T" are handled by the default
  pickle writers.

  If a program contains an import of this interface, a "Pickle.Special" with
  "sp.sc = TYPECODE (List.T)" will be passed to "Pickle.RegisterSpecial"
  during initialization.  This special pickles references "r" with "TYPECODE
  (r) = TYPECODE (List.T)" iteratively rather than recursively, and is thus
  useful for long lists in small stacks.

*)

INTERFACE ListPkl;

END ListPkl.
