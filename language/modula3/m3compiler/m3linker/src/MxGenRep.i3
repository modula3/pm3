(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxGenRep.i3                                           *)
(* Last Modified On Thu Jan 18 15:09:58 EST 1996 By ldd        *)
(*                                                             *)
(* From File: MxGen.m3                                         *)
(* Last Modified On Thu Feb 23 08:53:23 PST 1995 By kalsow     *)
(*      Modified On Fri Jul  2 19:33:09 PDT 1993 By muller     *)

INTERFACE MxGenRep;

IMPORT MxGen, Mx, Wr, IntRefTbl;

TYPE
  Common <: Public;
  Public = MxGen.T OBJECT
    base          : Mx.LinkSet   := NIL;
    errors        : Wr.T         := NIL;
    output        : Wr.T         := NIL;
    failed        : BOOLEAN      := FALSE;
    verbose       : BOOLEAN      := FALSE;
    gui           : BOOLEAN      := FALSE;
    n_modules     : INTEGER      := 0;
    dfs_count     : INTEGER      := 0;
    init_stack    : UnitInfo     := NIL;
    n_opaques     : INTEGER      := 0;
    opaques       : IntRefTbl.T  := NIL; (* type name -> OpaqueInfo *)
    all_opaques   : OpaqueInfo   := NIL;
    builtin_name  : Mx.Name      := 0;
    builtin_unit  : Mx.Unit      := NIL;
    eol           : TEXT         := NIL;
  END;
  (* The 'Common' object contains fields likely to be used by most
     generators. A new generator may be derived either from 'T' or 'Common'.
     The use of 'Common' is recommended where applicable. *)

CONST (* name-mangling done by the compiler & back-end *)
  IR_Prefix = ARRAY BOOLEAN OF TEXT { "MM_", "MI_" };

TYPE
  UnitInfo = REF RECORD
    unit         : Mx.Unit;
    next         : UnitInfo;
    imports      : UnitInfoList := NIL;
    exporters    : UnitInfoList := NIL;
    dfs_id       : INTEGER  := 0;
    low_link     : INTEGER  := 0;
    prev_stack   : UnitInfo := NIL;
    init_started : BOOLEAN  := FALSE;
    stacked      : BOOLEAN  := FALSE;
  END;

TYPE
  UnitInfoList = REF RECORD
    ui   : UnitInfo;
    next : UnitInfoList;
  END;

TYPE
  UnitProc = PROCEDURE (s: Common;  u: Mx.Unit);

TYPE
  OpaqueInfo = REF RECORD
    next   : OpaqueInfo     := NIL;
    type   : Mx.OpaqueType  := NIL;
    t_unit : Mx.Unit        := NIL;
    reveal : Mx.Revelation  := NIL;
    r_unit : Mx.Unit        := NIL;
  END;

PROCEDURE CheckOpaques(s: Common);
PROCEDURE FindBuiltins(s: Common);
PROCEDURE FindUnit(s: Common; intfs: REF ARRAY OF UnitInfo; READONLY name: Mx.Name): UnitInfo;
PROCEDURE ForEachUnit(s: Common; p: UnitProc);
PROCEDURE Err(s: Common; a, b, c, d: TEXT := NIL);
PROCEDURE Out(s: Common; a, b, c, d: TEXT := NIL);
PROCEDURE SortUnits (all_ui: UnitInfo;  n_units: INTEGER): UnitInfo;

END MxGenRep.
