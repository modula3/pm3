(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan  6 13:00:00 1997 by collin@vlsi.polymtl.ca       *)
(*      under a new license:                                                 *)
(*                                                                           *)
(*      Copyright (C) 1997 Jerome Collin                                     *)
(*                                                                           *)
(*      This is a free software; you can redistribute it and/or modify       *)
(*      it under the terms of the GNU General Public License as published    *)
(*      by the Free Software Foundation; either version 2, or                *)
(*      (at your option) any later version.                                  *)
(*                                                                           *)
(*      This software is distributed in the hope that it will be useful,     *)
(*      but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(*      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(*      GNU General Public License for more details.                         *)
(*                                                                           *)
(* modified on Wed Mar 22 08:05:39 PST 1995 by kalsow                        *)
(* modified on Fri Nov 25 11:34:48 PST 1994 by isard                         *)
(* modified on Mon Apr 13 09:55:12 PDT 1992 by muller                        *)

INTERFACE Stackx86;

FROM M3CG IMPORT MType, ZType, Sign, ByteOffset;
FROM M3CG_Ops IMPORT ErrorHandler;

IMPORT M3x86Rep, Codex86, Wrx86;
FROM M3x86Rep IMPORT Operand, OLoc, MVar, Regno, Force, RegSet, FlToInt;
FROM M3x86Rep IMPORT x86Proc, x86Var;

FROM Codex86 IMPORT Op, FOp;

TYPE T <: Public;
TYPE Public = OBJECT
      METHODS
        unlock ();
        clearall ();
        releaseall ();
        lock (r: Regno);
        find (stackp: INTEGER; force: Force; set := RegSet {};
              hintaddr := FALSE);
        freereg (set := RegSet {}): Regno;
        set_reg (stackp: INTEGER; r: Regno);
        dealloc_reg (stackp: INTEGER);
        corrupt (reg: Regno);
        set_fstack (stackp: INTEGER);
        set_mvar (stackp: INTEGER; READONLY mvar: MVar);
        set_imm (stackp, imm: INTEGER);
        loc (stackp: INTEGER): OLoc;
        op (stackp: INTEGER): Operand;
        pos (depth: INTEGER; place: TEXT): INTEGER;
        discard (depth: INTEGER);
        discard_indreg (indreg: Operand);
        set_error_handler (err: ErrorHandler);
        push (READONLY mvar: MVar);
        pushnew (type: MType; force: Force; set := RegSet {});
        pushimm (imm: INTEGER);
        pop (READONLY mvar: MVar);
        doloadaddress (v: x86Var; o: ByteOffset);
        get_addr_in_GOT (op: Operand; VAR indreg: Operand);
        dobin (op: Op; symmetric, overwritesdest: BOOLEAN): BOOLEAN;
        dostoreind (o: ByteOffset; type: MType);
        doumul ();
        doimul ();
        dodiv (a, b: Sign);
        domod (a, b: Sign);
        doneg ();
        doabs ();
        domaxmin (type: ZType; maxmin: MaxMin);
        load_fstacktop();
	prepare_fstack (op: FOp; forcenomem: BOOLEAN);
        fltoint (mode: FlToInt);
        inttoflt ();
        doshift ();
        dorotate ();
        doextract (sign: BOOLEAN);
        doextract_n (sign: BOOLEAN; n: INTEGER);
        doextract_mn (sign: BOOLEAN; m, n: INTEGER);
        doinsert ();
        doinsert_n (n: INTEGER);
        doinsert_mn (m, n: INTEGER);
        swap ();
        doloophole (from, two: ZType);
        doindex_address (shift, size: INTEGER; neg: BOOLEAN);
        docopy (type: MType; overlap: BOOLEAN);
        docopy_n (n: INTEGER; type: MType; overlap: BOOLEAN);
        doimm (op: Op; imm: INTEGER; overwritesdest: BOOLEAN);
        newdest (READONLY op: Operand);
        init ();
        end ();
        set_current_proc (p: x86Proc);
        reg (stackp: INTEGER): Regno;
        lower (reg: Regno): INTEGER;
        set_lower (reg: Regno; low: INTEGER);
        upper (reg: Regno): INTEGER;
        set_upper (reg: Regno; up: INTEGER);
        non_nil (reg: Regno): BOOLEAN;
        set_non_nil (reg: Regno);
      END;

TYPE MaxMin = { Max, Min };

PROCEDURE New (parent: M3x86Rep.U; cg: Codex86.T; debug, pic: BOOLEAN): T;

PROCEDURE Debug (t: T;  tag: TEXT;  wr: Wrx86.T);

END Stackx86.
