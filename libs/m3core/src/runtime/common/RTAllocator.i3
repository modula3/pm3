(* Copyright (C) 1993, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*| Last modified on Thu May  4 14:00:29 PDT 1995 by kalsow  *)
(*|      modified on Tue Nov  9 12:17:47 PST 1993 by mcjones *)
(*|      modified on Fri Jan 29 09:39:44 PST 1993 by jdd    *)
(*|      modified on Wed Jul 3 04:15:39 1991 by muller      *)

(* "RTAllocator" provides access to the runtime storage allocator.
   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>allocator</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>storage allocator</SPAN>
</SPAN>

   <SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>heap</SPAN>
</SPAN>

*)

INTERFACE RTAllocator;

FROM RTType IMPORT Typecode;

(* Each of the procedures described below allocates and initializes heap
   storage.  Calling any of these procedures with a typecode "tc" that
   names a type "T" is equivalent to calling "NEW" for that type.  It is a
   checked runtime error to pass a typecode that is not proper.  (See
   "RTType" for the definition of proper typecode.) *)

PROCEDURE NewTraced(tc: Typecode): REFANY
  RAISES {OutOfMemory};
(* Return a reference to a freshly allocated and initialized, traced
   referent with typecode "tc".  It is a checked runtime error if "tc" does
   not name a traced reference type other than "REFANY", or if its referent
   is an open array. *)

PROCEDURE NewUntraced(tc: Typecode): ADDRESS
  RAISES {OutOfMemory};
(* Return a reference to a freshly allocated and initialized, untraced
   referent with typecode "tc".  It is a checked runtime error if "tc" does
   not name an untraced reference type other than "ADDRESS", or if it names
   an untraced object type, or if its referent is an open array. *)

PROCEDURE NewUntracedObject(tc: Typecode): UNTRACED ROOT
  RAISES {OutOfMemory};
(* Return a freshly allocated and initialized, untraced object with
   typecode "tc".  It is a checked runtime error if "tc" does not name an
   untraced object type. *)

TYPE Shape = ARRAY OF INTEGER;

PROCEDURE NewTracedArray(tc: Typecode; READONLY s: Shape): REFANY
  RAISES {OutOfMemory};
(* Return a reference to a freshly allocated and initialized, traced open
   array referent with typecode "tc" and sizes "s[0]", ..., "s[LAST(s)]".
   It is a checked runtime error if "tc" does not name a traced reference
   to an open array, or if any "s[i]" is negative, or if "NUMBER(s)" does
   not equal the number of open dimensions of the array. *)

PROCEDURE NewUntracedArray(tc: Typecode; READONLY s: Shape): ADDRESS
  RAISES {OutOfMemory};
(* Return a reference to a freshly allocated and initialized, untraced open
   array referent with typecode "tc" and sizes "s[0]", ..., "s[LAST(s)]".
   It is a checked runtime error if "tc" does not name an untraced
   reference to an open array, or if any "s[i]" is negative, or if
   "NUMBER(s)" does not equal the number of open dimensions of the
   array. *)

PROCEDURE Clone (ref: REFANY): REFANY
  RAISES {OutOfMemory};
(* Return a reference to a freshly allocated and initialized, traced
   reference, object or array.  The new referent will have the same
   runtime type as "ref" and a copy of its contents, "ref^". *)

EXCEPTION OutOfMemory;
(* Raised if the allocator was unable to allocate the requested object *)

(*--------------------------------------------------------- SRC only ---*)

VAR callback: PROCEDURE (ref: REFANY) := NIL;
(* If non-NIL, the allocator calls "callback(r)" just before returning
   a new traced reference "r".  See "RTAllocStats" for an example client. *)

END RTAllocator.
