(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Unit.i3                                               *)
(* Last modified on Wed Jul  6 13:46:07 PDT 1994 by kalsow     *)

INTERFACE Unit;

IMPORT M3Compiler;
FROM M3Driver IMPORT Error;

PROCEDURE ResetPath ();
(* reset the search path to be empty *)

PROCEDURE PushDir (name : TEXT);
(* prepend the directory 'name' to the search path *)

PROCEDURE PushTable (name: TEXT) RAISES {Error};
(* prepend the search table in the file 'name' *)

PROCEDURE GetFullPath (name: TEXT): TEXT;  (* For AST cache *)
(* get the full path location of the compilation unit named "name" *)

PROCEDURE GetRelativePath (name: TEXT): TEXT; (* For AST cache *)
(* return the relative path location of the compilation unit named "name" *)

PROCEDURE Open (name: TEXT; interface,generic: BOOLEAN): M3Compiler.SourceFile;
(* locate and open the compilation unit named "name" *)

PROCEDURE Find (name: TEXT; interface,generic: BOOLEAN): TEXT RAISES {Error};
(* locate the compilation unit named "name" and return its full path name *)

END Unit.
