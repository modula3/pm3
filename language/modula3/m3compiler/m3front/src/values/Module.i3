(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Module.def                                            *)
(* Last modified on Mon Aug 29 09:53:53 PDT 1994 by kalsow     *)
(*      modified on Sat Mar 16 01:55:39 1991 by muller         *)

INTERFACE Module;

IMPORT M3ID, Type, Value, Scope, CG, M3Compiler;

TYPE T <: Value.T;

PROCEDURE Parse (interfaceOnly: BOOLEAN := FALSE): T;

PROCEDURE NewDefn (name: TEXT;  safe: BOOLEAN;  syms: Scope.T): T;

PROCEDURE LookUp (name: M3ID.T;  internal: BOOLEAN): T;

PROCEDURE ImportRevelations (t: T;  source: Value.T);

PROCEDURE TypeCheck (t: T;  main: BOOLEAN;  VAR cs: Value.CheckState);

PROCEDURE Compile (t: T);

PROCEDURE IsSafe (): BOOLEAN;
PROCEDURE IsInterface (): BOOLEAN;
PROCEDURE IsExternal (): BOOLEAN;

PROCEDURE GetImports(t: T): M3Compiler.IDList;

PROCEDURE ExportScope (t: T): Scope.T;

PROCEDURE Current (): T;

PROCEDURE Name (t: T): M3ID.T;
PROCEDURE Prefix (t: T): TEXT;
(* t = NIL => use Current *)

PROCEDURE CurrentCounter (): ARRAY [0..4] OF CHAR;
PROCEDURE SetCurrentCounter (c: ARRAY [0..4] OF CHAR);

PROCEDURE Allocate (size, align: INTEGER;
                    tag: TEXT := NIL;  id: M3ID.T := M3ID.NoID): INTEGER;
(* allocate 'size' bits of space with the specified alignment
   in the current module's global data segment.  Return the
   bit offset of allocated data. *)

PROCEDURE GlobalData (t: T): CG.Var;
(* returns 't's global data segment.  If 't' is NIL, returns the
   current module's global dat segment.  *)

PROCEDURE GetTypeInfo (t: T): Type.ModuleInfo;
(* return the global type info for module 't' *)

PROCEDURE VisitImports (v: Visitor);
(* Call 'v(m)' for each interface 'm' imported or exported,
   directly or indirectly, by the current module.  Restrictions:
   'v' must be a top-level procedure and may not call 'VisitImports'
   directly or indirectly.  *)

TYPE Visitor = PROCEDURE (t: T);

PROCEDURE Reset ();
PROCEDURE MakeCurrent (t: T);
(* refresh 't' and its imports for the current compilation *)

END Module.
