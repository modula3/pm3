(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Compiler.i3                                         *)
(* Last modified on Tue Dec  6 09:13:12 PST 1994 by kalsow     *)
(*      modified on Sat May 12 07:05:52 1990 by muller         *)

INTERFACE M3Compiler;

(* This module defines the front-end interface of the Modula-3
   compiler.  The compiler is a monitor -- only one compilation
   can be active at a time.
*)

IMPORT File, Fingerprint;
IMPORT M3ID, M3CG;

EXCEPTION
  FrontError;

PROCEDURE ParseImports (READONLY input : SourceFile;
                                 env   : Environment): IDList;
(* Returns the names of the interfaces directly imported by 'input'. *)

PROCEDURE Compile (READONLY input    : SourceFile;
                            env      : Environment;
                   READONLY options  : ARRAY OF TEXT): BOOLEAN RAISES {FrontError};

(* Reads and compiles a Modula-3 unit from "input".
   Evironmental queries and reports are made through "env".
   The listing and diagnostic options of the compiler are set by "options".
   Returns "TRUE" iff the compilation succeeded with no errors.
   It is the caller's responsibility to initialize the Target interface
   prior to calling Compile. *)

PROCEDURE GetImports (interface: REFANY): IDList;

TYPE
  TypeID     = INTEGER;  (* A compiler generated type id. *)
  SourceFile = RECORD  name: TEXT;  contents: File.T  END;
  IDList     = REF RECORD interface: M3ID.T;  next: IDList  END;

TYPE
  Environment = OBJECT METHODS

    report_error (file: TEXT;  line: INTEGER;  msg: TEXT);

    find_source (unit: M3ID.T;  interface, generic: BOOLEAN): SourceFile;

    note_unit (name: M3ID.T;  interface: BOOLEAN);

    note_comment (msg: TEXT);

    note_interface_use (name: M3ID.T;  imported: BOOLEAN);

    note_generic_use (name: M3ID.T);

    note_version_stamp (unit, symbol: M3ID.T;
                        READONLY vs: Fingerprint.T;
                        imported, implemented: BOOLEAN);

    note_opaque (type, super_type: TypeID);

    note_revelation (unit: M3ID.T;  interface: BOOLEAN;
                    lhs, rhs: TypeID;  full, imported: BOOLEAN);

    note_opaque_magic (type, super_type: TypeID;
                       data_size, data_align, method_size : INTEGER);

    find_opaque_magic (type: TypeID;  VAR(*OUT*) super_type: TypeID;
            VAR(*OUT*) data_size, data_align, method_size: INTEGER): BOOLEAN;

    note_ast (unit: M3ID.T;  ast: REFANY);
    find_ast (unit: M3ID.T): REFANY;

    note_type (type: TypeID;  imported: BOOLEAN);

    init_code_generator (): M3CG.T RAISES {FrontError};

    note_webinfo (t: TEXT);
  END;

END M3Compiler.

(* The compiler makes all environmental queries and reports through
   the "env" parameter:

     "report_error" is called to report error and warning messages.

     "find_source" is called to locate the source files needed to
      satisfy "IMPORT"s.

     "note_unit" is called to announce the current unit.  The remainder
      of the "note" calls attach information needed by the linker
      and smart recompilation system to the announced unit.

     "note_comment" attaches a comment to the current unit.

     "note_inteface_use" records the named interface as is either imported
      or exported by the current unit.

     "note_generic_use" records the use of the specified generic unit.

     "note_version_stamp" records the import(export) of "symbol" with
      version stamp "vs" from(to) the interface "unit".

     "note_opaque" records the opaque declaration "type <: super_type"
      in the current unit.

     "note_revelation" records the import(export) of a revelation.
      With "note_opaque" it is used to verify the all opaque types
      are defined and the all compilation units saw a consistent
      set of revelations.

     "note_opaque_magic" announces the size of a previously declared
      opaque type.

     "find_opaque_magic" attempts to locate the size of an opaque type.
      Returns TRUE iff it succeeded in finding the information.

     "note_ast" records an (interface name, ast) pair in the ast
     cache.

     "find_ast" returns the ast cached under the given name.  Returns
     NIL if no such ast exists.

     "init_code_generator" returns the code generator to be used.
     To avoid empty object files, "init_code_generator" isn't called
     until after type checking.
*)

(* The recognized options are:

     -v             verbose mode
     -g             generate debugging information
     -S             don't generate version stamps
     -wX            print warnings at level "X" and above
     -w             don't print warnings  == -w99
     -Z             generate line-based profiling.
     -En            die on the "n-th" error message.
     -NoChecks      disable all runtime checks
     -NoAsserts     disable <*ASSERT*> checks
     -NoNarrowChk   disable narrow checks
     -NoRangeChk    disable subscript and range checks
     -NoReturnChk   disable checks for functions that fail to return values
     -NoCaseChk     disable checks for unhandled CASE selectors
     -NoTypecaseChk disable checks for unhandled TYPECASE selectors
     -NoNilChk      disable explicit NIL checks
     -NoRaisesChk   disable checks for unhandled exceptions
     -InitFloats    initialize all floating point values to zero
     -vsdebug       generate a trace of the fingerprint computations
     -builtins      "compile" and emit the builtin symbols
     -times         print the elapsed time profile
     -load_map      generate the load map comment in the output
     -No_load_map   don't generate the load map comment in the output
*)



