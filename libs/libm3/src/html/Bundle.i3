(* Copyright (C) 1992, Digital Equipment Corporation                     *)
(* All rights reserved.                                                  *)
(* See the file COPYRIGHT for a full description.                        *)
(* Last modified on Mon Nov  8 16:50:30 PST 1993 by mcjones              *)
(*      modified on Thu Sep 10 20:48:51 PDT 1992 by mhb                  *)

(* A "Bundle.T", or bundle, is a collection of named byte string
   values, where the names and values are represented as "TEXTs".  The
   usefulness of bundles stems from the existence of a program called
   "m3bundle".  This program accepts an arbitrary set of files and
   produces the source code of a Modula-3 procedure that, when
   compiled and executed, returns a bundle containing the contents of
   the original files.
   <SPAN CLASS=INDEX.MARK>
   <SPAN CLASS=INDEX.KEY>m3bundle program</SPAN>
   <SPAN CLASS=INDEX.TEXT><TT>m3bundle</TT> program</SPAN>
   </SPAN>
*)

INTERFACE Bundle;

TYPE T <: REFANY;

PROCEDURE Get(b: T; nm: TEXT): TEXT;
(* If an element of "b" has the name "nm", return its value.
   Otherwise, return "NIL". *)

END Bundle.

(* To call "Bundle.Get", you need a value of type "Bundle.T".  Given a
   collection of files, the program "m3bundle" generates the source
   code of an interface (".i3" file) and a module (".m3" file)
   implementing that interface.  The interface contains a single
   procedure returning a bundle.

   If you want to build a bundle with elements named "e1, ..., en"
   corresponding to values currently in files with pathnames "p1, ...,
   pN", you invoke "m3bundle" as follows:

| m3bundle -name Foo [-element e1 p1]...

   "m3bundle" then produces an interface "Foo.i3" with this format:

| INTERFACE Foo;
| IMPORT Bundle;
| PROCEDURE Get(): Bundle.T;
| END Foo.

   The call "Foo.Get()" returns a bundle "b" such that the call
   "Bundle.Get(b, nm)" returns the contents of file "pi" at the time
   "m3bundle" was invoked if "nm" equals one of the "ei" passed to
   "m3bundle". Otherwise, "Bundle.Get(b, nm)" returns "NIL".

   For more information about "m3bundle", consult its man page or
   other system-specific documentation.

*)
