(*Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Aug 27 09:35:34 PDT 1996 by najork     *)
(*      modified on Wed Mar 22 09:02:46 PST 1995 by kalsow     *)
(*      modified on Fri Nov 25 11:36:13 PST 1994 by isard      *)

MODULE Stackx86;

IMPORT M3CG, TargetMap, M3CG_Ops, Word, M3x86Rep, Codex86;

FROM TargetMap IMPORT CG_Bytes, CG_Align_bytes;

FROM M3CG IMPORT Type, MType, ZType, Sign, Label, ByteOffset;
FROM M3CG_Ops IMPORT ErrorHandler;

FROM M3x86Rep IMPORT Operand, MVar, Regno, OLoc, VLoc, NRegs, Force;
FROM M3x86Rep IMPORT RegSet, FlToInt, x86Var, x86Proc, NoStore;

FROM Codex86 IMPORT Op, FOp, Cond, revcond, IntnlVar;

REVEAL T = Public BRANDED "Stackx86.T" OBJECT
        cg            : Codex86.T := NIL;
        parent        : M3x86Rep.U := NIL;
        Err           : ErrorHandler := NIL;
        debug         := FALSE;
        stacktop      := 0;
        vstack        : REF ARRAY OF Operand := NIL;
        vstacklimit   := 0;
        reguse        : ARRAY [0 .. NRegs] OF Register;
        current_proc  : x86Proc;
        rmode         : ARRAY FlToInt OF INTEGER;
        lowset_table  : MVar;
        highset_table : MVar;
      OVERRIDES
        init := init;
        end := end;
        set_current_proc := set_current_proc;
        unlock := unlock;
        lock := lock;
        clearall := clearall;
        find := find;
        freereg := freereg;
        set_reg := set_reg;
        dealloc_reg := dealloc_reg;
        corrupt := corrupt;
        set_fstack := set_fstack;
        set_mvar := set_mvar;
        set_imm := set_imm;
        loc := get_loc;
        op := get_op;
        pos := pos;
        discard := discard;
        set_error_handler := set_error_handler;
        push := push;
        pushnew := pushnew;
        pushimm := pushimm;
        pop := pop;
        doloadaddress := doloadaddress;
        dobin := dobin;
        dostoreind := dostoreind;
        doumul := doumul;
        doimul := doimul;
        dodiv := dodiv;
        domod := domod;
        doimm := doimm;
        doneg := doneg;
        doabs := doabs;
        domaxmin := domaxmin;
        fltoint := fltoint;
        inttoflt := inttoflt;
        doshift := doshift;
        dorotate := dorotate;
        doextract := doextract;
        doextract_n := doextract_n;
        doextract_mn := doextract_mn;
        doinsert := doinsert;
        doinsert_n := doinsert_n;
        doinsert_mn := doinsert_mn;
        swap := swap;
        doloophole := doloophole;
        doindex_address := doindex_address;
        newdest := newdest;
        reg := reg;
        lower := lower;
        set_lower := set_lower;
        upper := upper;
        set_upper := set_upper;
        non_nil := non_nil;
        set_non_nil := set_non_nil;
      END;

TYPE
  Register = RECORD
    stackp     : INTEGER := -1;
    last_store : MVar    := NoStore;
    last_imm   : INTEGER := 0;
    lowbound   : INTEGER := FIRST (INTEGER);
    upbound    : INTEGER := LAST (INTEGER);
    imm        : BOOLEAN := FALSE;
    locked     : BOOLEAN := FALSE;
    non_nil    : BOOLEAN := FALSE;
  END;

(*-------------------------------------------- register handling routines ---*)

CONST HighPrec: INTEGER = NRegs * 1000;

PROCEDURE unlock (t: T) =
  VAR flcount := 0;
  BEGIN
    FOR i := 0 TO NRegs DO
      t.reguse[i].locked := FALSE;
    END;

    IF t.debug THEN
      t.cg.wrFlush();

      FOR i := 0 TO NRegs DO
        IF t.reguse[i].stackp # -1 THEN
          <* ASSERT t.vstack[t.reguse[i].stackp].reg = i *>
        END
      END;

      FOR i := 0 TO t.stacktop - 1 DO
        IF t.vstack[i].loc = OLoc.register THEN
          <* ASSERT t.reguse[t.vstack[i].reg].stackp = i *>
        ELSIF t.vstack[i].loc = OLoc.fstack THEN
          INC(flcount);
        END
      END;

      t.cg.assert_fstack(flcount);
    END
  END unlock;

PROCEDURE lock (t: T; r: Regno) =
  BEGIN
      t.reguse[r].locked := TRUE;
  END lock;

PROCEDURE clear (t: T; r: Regno) =
  BEGIN
    <* ASSERT t.reguse[r].stackp = -1 *>
    corrupt(t, r);
    t.reguse[r].locked := FALSE;
  END clear;

PROCEDURE loadreg (t: T; r: Regno; op: Operand) =
  BEGIN
    t.cg.movOp(t.cg.reg[r], op);

    t.reguse[r] := Register { locked := t.reguse[r].locked };

    t.reguse[r].stackp := op.stackp;

    IF op.loc = OLoc.mem THEN
      IF op.mvar.var.stack_temp THEN
        t.parent.free_temp(op.mvar.var);
      ELSE
        t.reguse[r].last_store := op.mvar;
      END
    END;

    IF op.loc = OLoc.imm THEN
      t.reguse[r].last_imm := op.imm;
      t.reguse[r].imm := TRUE;
    END;

    set_reg(t, op.stackp, r);
  END loadreg;

PROCEDURE loadphantom (t: T; r: Regno; stackp: INTEGER) =
  BEGIN
    t.reguse[r].stackp := stackp;
    t.vstack[stackp].loc := OLoc.register;
    t.vstack[stackp].reg := r;
  END loadphantom; 

PROCEDURE copyreg (t: T; stackp: INTEGER; to, from: Regno) =
  BEGIN
    t.reguse[to] := t.reguse[from];
    set_reg(t, stackp, to);
    t.cg.movOp(t.cg.reg[to], t.cg.reg[from]);
  END copyreg;

PROCEDURE movereg (t: T; to, from: Regno) =
  BEGIN
    t.reguse[to] := t.reguse[from];
    t.reguse[from].stackp := -1;
    set_reg(t, t.reguse[to].stackp, to);
    t.cg.movOp(t.cg.reg[to], t.cg.reg[from]);
  END movereg;

PROCEDURE swapreg (t: T; to, from: Regno) =
  VAR tempstack := t.reguse[from].stackp;
      tempstore := t.reguse[from].last_store;
  BEGIN
    t.reguse[from].stackp := t.reguse[to].stackp;
    t.reguse[to].stackp := tempstack;
    t.reguse[from].last_store := t.reguse[to].last_store;
    t.reguse[to].last_store := tempstore;

    IF t.reguse[from].stackp # -1 THEN
      set_reg(t, t.reguse[from].stackp, from);
    END;

    IF tempstack # -1 THEN
      set_reg(t, tempstack, to);
    END;

    t.cg.swapOp(t.cg.reg[to], t.cg.reg[from]);
  END swapreg;

PROCEDURE clearall (t: T) =
  BEGIN
    t.cg.wrFlush();

    FOR i := 0 TO NRegs DO
      clear(t, i);
    END
  END clearall; 

PROCEDURE find (t: T; stackp: INTEGER;
                force: Force := Force.any; set := RegSet {};
                hintaddr := FALSE) =
  (* Find a suitable register to put a stack item in *)
  VAR in, to: Regno;
  BEGIN
    CASE t.vstack[stackp].loc OF
      OLoc.fstack =>
        t.Err("Tried to put a float in an int register in 'find'");
    | OLoc.mem =>
        in := inreg(t, t.vstack[stackp].mvar, set);
    | OLoc.register =>
        in := t.vstack[stackp].reg;
    | OLoc.imm =>
        in := immreg(t, t.vstack[stackp].imm, set);
    END;

    IF t.vstack[stackp].mvar.t = Type.Addr THEN
      hintaddr := TRUE;
    END;

    (* If it is in a register and shoudn't be, move it *)
    IF force = Force.mem AND t.vstack[stackp].loc = OLoc.register THEN
      get_temp(t, stackp, t.vstack[stackp].reg);
      RETURN;
    END;

    (* If it is an immediate value and should be in mem, do it *)
    IF force = Force.mem AND t.vstack[stackp].loc = OLoc.imm THEN
      get_temp(t, stackp, -1, t.vstack[stackp].imm);
      RETURN;
    END;

    (* If it is immediate and doesn't have to be in a register, then
       do nothing *)
    IF t.vstack[stackp].loc = OLoc.imm AND force # Force.anyreg AND
       force # Force.regset THEN
      RETURN;
    END;

    (* If it isn't in a register yet, and it doesn't have to be, do nothing *)
    IF force = Force.any AND in = -1 THEN
      RETURN;
    END;

    IF force = Force.anydword AND in = -1 AND t.vstack[stackp].loc = OLoc.mem
       AND CG_Bytes[t.vstack[stackp].mvar.t] = 4 THEN
      RETURN;
    END;

    (* If it is in a temporary variable and can stay there, leave it *)
    IF force = Force.anytemp AND in = -1 AND t.vstack[stackp].loc = OLoc.mem
       AND t.vstack[stackp].mvar.var.stack_temp THEN
      RETURN;
    END;

    IF t.vstack[stackp].loc = OLoc.mem AND
       CG_Bytes[t.vstack[stackp].mvar.t] = 1 THEN
      force := Force.regset;
      IF set = RegSet {} THEN
        set := RegSet { Codex86.EAX, Codex86.EBX,
                        Codex86.ECX, Codex86.EDX };
      ELSE
        set := set * RegSet { Codex86.EAX, Codex86.EBX,
                              Codex86.ECX, Codex86.EDX };
      END
    END;

    (* If for any reason it isn't in the right register, find the best
       candidate for a register to put it in. *)
    IF (in = -1) OR (force = Force.regset AND (NOT in IN set)) OR
       t.reguse[in].locked OR
       (t.reguse[in].stackp # -1 AND t.reguse[in].stackp # stackp) THEN
      to := pickreg(t, set, hintaddr);

    (* Otherwise, it is in the right place, so leave it *)
    ELSE
      loadphantom(t, in, stackp);
      t.reguse[in].locked := TRUE;
      RETURN;
    END;

    (* If it doesn't have to be in a register, and there are no
       unused registers, do nothing *)
    IF force = Force.any AND t.reguse[to].stackp # -1 THEN
      RETURN;
    END;

    IF force = Force.anydword AND t.reguse[to].stackp # -1 AND
       t.vstack[stackp].loc = OLoc.mem
       AND  CG_Bytes[t.vstack[stackp].mvar.t] = 4 THEN
      RETURN;
    END;

    (* If it is in a temporary variable & can stay there, leave it *)
    IF force = Force.anytemp AND t.reguse[to].stackp # -1
       AND t.vstack[stackp].loc = OLoc.mem
       AND t.vstack[stackp].mvar.var.stack_temp THEN
      RETURN;
    END;

    (* Now we know that we want to put it into 'to' *)

    (* If 'to' is unused, this is easy *)
    IF t.reguse[to].stackp = -1 THEN
      IF in = -1 THEN
        loadreg(t, to, t.vstack[stackp]);
      ELSE
        IF t.reguse[in].stackp = stackp THEN
          movereg(t, to, in);
        ELSE
          copyreg(t, stackp, to, in);
        END
      END;

    ELSE
    (* Otherwise, see if 'in' is used for something other than stackp. If not,
       swap the registers over. If so, force 'to' out. If there is a free
       register, 'to' will be moved into it, otherwise it will be stored to
       memory *)
      IF in = -1 OR
         (t.reguse[in].stackp # -1 AND t.reguse[in].stackp # stackp) THEN
        forceout(t, to);
        IF in = -1 THEN
          loadreg(t, to, t.vstack[stackp]);
        ELSE
          copyreg(t, stackp, to, in);
        END
      ELSE
        swapreg(t, to, in);
        loadphantom(t, to, stackp);
      END
    END;

    t.reguse[to].locked := TRUE;
  END find;

PROCEDURE freereg (t: T; set := RegSet {}): Regno =
  VAR to: Regno;
  BEGIN
    to := pickreg(t, set);
    corrupt(t, to);
    t.reguse[to].locked := TRUE;
    RETURN to;
  END freereg;

PROCEDURE forceout (t: T; r: Regno) =
  VAR dead: Regno;
  BEGIN
    dead := finddead(t);
    IF dead = -1 THEN
      get_temp(t, t.reguse[r].stackp, r);
    ELSE
      movereg(t, dead, r);
    END
  END forceout;

PROCEDURE finddead (t: T): Regno =
  VAR minprec := HighPrec;
      bestreg: Regno := -1;
  BEGIN
    FOR i := 0 TO NRegs DO
      IF (t.reguse[i].stackp = -1) THEN
        WITH prec = precedence(t, i) DO
          IF prec < minprec THEN
            minprec := prec;
            bestreg := i;
          END
        END
      END
    END;
    RETURN bestreg;
  END finddead;

PROCEDURE pickreg (t: T; set: RegSet:= RegSet {}; hintaddr := FALSE): Regno =
  VAR minprec := HighPrec;
      bestreg: Regno := -1;
  BEGIN
    FOR i := 0 TO NRegs DO
      IF set = RegSet {} OR i IN set THEN
        WITH prec = precedence(t, i, hintaddr) DO
          IF prec < minprec THEN
            minprec := prec;
            bestreg := i;
          END
        END
      END
    END;
    <* ASSERT minprec # HighPrec *>
    RETURN bestreg;
  END pickreg;

PROCEDURE inreg (t: T; READONLY v: MVar; set: RegSet:= RegSet {}): Regno =
  VAR minprec := HighPrec * HighPrec;
      prec := 0;
      bestreg: Regno := -1;
      hintaddr := FALSE;
  BEGIN
    IF v.t = Type.Addr THEN
      hintaddr := TRUE;
    END;

    FOR i := 0 TO NRegs DO
      IF t.reguse[i].last_store # NoStore AND
         v = t.reguse[i].last_store THEN
        prec := precedence(t, i);
        IF (set # RegSet {}) AND (NOT i IN set) THEN
          prec := prec * HighPrec;
        END;
        IF prec < minprec THEN
          minprec := prec;
          bestreg := i;
        END
      END
    END;
    RETURN bestreg;
  END inreg;

PROCEDURE immreg (t: T; imm: INTEGER; set: RegSet:= RegSet {}): Regno =
  VAR minprec := HighPrec * HighPrec;
      prec := 0;
      bestreg: Regno := -1;
  BEGIN
    FOR i := 0 TO NRegs DO
      IF t.reguse[i].imm AND
         imm = t.reguse[i].last_imm THEN
        prec := precedence(t, i);
        IF (set # RegSet {}) AND (NOT i IN set) THEN
          prec := prec * HighPrec;
        END;
        IF prec < minprec THEN
          minprec := prec;
          bestreg := i;
        END
      END
    END;
    RETURN bestreg;
  END immreg;

CONST baseprec = ARRAY BOOLEAN, [0 .. NRegs] OF INTEGER
                 { ARRAY [0 .. NRegs] OF INTEGER { 6, 5, 2, 1, HighPrec,
                                                   HighPrec, 3, 4 },
                   ARRAY [0 .. NRegs] OF INTEGER { 6, 5, 4, 3, HighPrec,
                                                   HighPrec, 1, 2 } };

PROCEDURE precedence (t: T; r: Regno; hintaddr := FALSE): INTEGER =
  VAR prec: INTEGER;
  BEGIN
    IF baseprec[hintaddr][r] = HighPrec THEN
      RETURN HighPrec;
    END;
    IF t.reguse[r].locked THEN
      RETURN HighPrec;
    END;
    IF t.reguse[r].stackp # -1 THEN
      prec := 4 * NRegs;
    ELSIF t.reguse[r].last_store # NoStore THEN
      prec := 3 * NRegs;
    ELSIF t.reguse[r].imm THEN
      prec := 2 * NRegs;
    ELSE
      prec := NRegs;
    END;
    prec := prec + baseprec[hintaddr][r];
    RETURN prec;
  END precedence;

(*-------------------------------------------------------- stack routines ---*)

PROCEDURE get_temp (t: T; stackp: INTEGER; r: Regno; imm := 0) =
  BEGIN
    set_mvar(t, stackp, MVar { var := t.parent.declare_temp(4, 4, Type.Int,
                                                            FALSE),
                               o := 0, t := Type.Int } );
    t.vstack[stackp].mvar.var.stack_temp := TRUE;
    IF r = -1 THEN
      t.cg.movImm(t.vstack[stackp], imm);
    ELSE
      t.reguse[r].stackp := -1;
      t.cg.movOp(t.vstack[stackp], t.cg.reg[r]);
    END
  END get_temp;

PROCEDURE sweep (t: T; READONLY mvar: MVar) =
  VAR doneswap := FALSE;
  BEGIN
    FOR i := 0 TO t.stacktop - 1 DO
      IF t.vstack[i].loc = OLoc.mem AND
         t.vstack[i].mvar = mvar THEN
        IF NOT doneswap THEN
          doneswap := TRUE;
          t.cg.pushOp(t.cg.reg[Codex86.EAX]);
        END;
        t.cg.movOp(t.cg.reg[Codex86.EAX], t.vstack[i]);
        set_mvar(t, i, MVar { var := t.parent.declare_temp(4, 4, Type.Int,
                                                           FALSE),
                              o := 0, t := Type.Int } );
        t.vstack[i].mvar.var.stack_temp := TRUE;
        t.cg.movOp(t.vstack[i], t.cg.reg[Codex86.EAX]);
      END
    END;
    IF doneswap THEN
      t.cg.popOp(t.cg.reg[Codex86.EAX]);
    END
  END sweep;

PROCEDURE set_reg (t: T; stackp: INTEGER; r: Regno) =
  BEGIN
    t.vstack[stackp].loc := OLoc.register;
    t.vstack[stackp].reg := r;
    t.reguse[r].stackp := stackp;
  END set_reg;

PROCEDURE dealloc_reg (t: T; stackp: INTEGER) =
  BEGIN
    <* ASSERT t.vstack[stackp].loc = OLoc.register *>
    t.reguse[t.vstack[stackp].reg].stackp := -1;
  END dealloc_reg;

PROCEDURE corrupt (t: T; reg: Regno) =
  BEGIN
    IF t.reguse[reg].stackp # -1 THEN
      forceout(t, reg);
    END;
    t.reguse[reg] := Register { locked := t.reguse[reg].locked };
  END corrupt;

PROCEDURE set_fstack (t: T; stackp: INTEGER) =
  BEGIN
    t.vstack[stackp].loc := OLoc.fstack;
  END set_fstack;

PROCEDURE set_mvar (t: T; stackp: INTEGER; READONLY mvar: MVar) =
  BEGIN
    t.vstack[stackp].loc := OLoc.mem;
    t.vstack[stackp].mvar := mvar;
  END set_mvar;

PROCEDURE set_imm (t: T; stackp, imm: INTEGER) =
  BEGIN
    t.vstack[stackp].loc := OLoc.imm;
    t.vstack[stackp].imm := imm;
  END set_imm;

PROCEDURE get_loc (t: T; stackp: INTEGER): OLoc =
  BEGIN
    RETURN t.vstack[stackp].loc;
  END get_loc;

PROCEDURE get_op (t: T; stackp: INTEGER): Operand =
  BEGIN
    RETURN t.vstack[stackp];
  END get_op;

PROCEDURE pos (t: T; depth: INTEGER; place: TEXT): INTEGER =
  BEGIN
    WITH pos = t.stacktop - 1 - depth DO
      IF pos >= 0 THEN
        RETURN pos;
      ELSE
        t.Err("Stack underflow in " & place);
      END
    END;
    RETURN -1;
  END pos;

PROCEDURE pushimm (t: T; imm: INTEGER) =
  BEGIN
    IF t.stacktop = t.vstacklimit THEN
      expand_stack(t);
    END;

    WITH stack0 = t.vstack[t.stacktop] DO
      stack0.loc := OLoc.imm;
      stack0.imm := imm;
      stack0.stackp := t.stacktop;
    END;

    INC(t.stacktop);
  END pushimm;

PROCEDURE pushnew (t: T; type: MType; force: Force; set := RegSet {}) =
  VAR hintaddr := type = Type.Addr;
  VAR reg := pickreg(t, set, hintaddr);
  BEGIN
    IF t.stacktop = t.vstacklimit THEN
      expand_stack(t);
    END;
    WITH stack0 = t.vstack[t.stacktop] DO
      stack0.stackp := t.stacktop;
      IF type >= Type.Reel AND type <= Type.XReel THEN
        stack0.loc := OLoc.fstack;
      ELSE
        IF force = Force.mem OR 
           (t.reguse[reg].stackp # -1 AND force = Force.any) THEN
          set_mvar(t, t.stacktop,
                   MVar { var :=  t.parent.declare_temp(CG_Bytes[type],
                                                        CG_Align_bytes[type],
                                                        type, FALSE),
                          o := 0, t := type } );
          stack0.mvar.var.stack_temp := TRUE;
        ELSE
          corrupt(t, reg);
          set_reg(t, t.stacktop, reg);
        END
      END
    END;
    INC(t.stacktop);
  END pushnew;

PROCEDURE push (t: T; READONLY mvar: MVar) =
  VAR indreg, destreg: Regno;
  BEGIN
    IF t.stacktop = t.vstacklimit THEN
      expand_stack(t);
    END;

    WITH stack0 = t.vstack[t.stacktop] DO
      stack0.stackp := t.stacktop;
      IF mvar.t >= Type.Reel AND mvar.t <= Type.XReel THEN
        IF mvar.var.loc = VLoc.temp AND mvar.var.parent # t.current_proc THEN
          unlock(t);
          indreg := pickreg(t, RegSet {}, TRUE);
          corrupt(t, indreg);

          t.cg.get_frame(indreg, mvar.var.parent, t.current_proc);
          t.cg.f_loadind(t.cg.reg[indreg], mvar.o + mvar.var.offset, mvar.t);
          stack0.loc := OLoc.fstack;
        ELSE
          stack0.loc := OLoc.fstack;
          t.cg.fstack_push(mvar);
        END
      ELSE
        IF mvar.var.loc = VLoc.temp AND mvar.var.parent # t.current_proc THEN
          unlock(t);
          IF CG_Bytes[mvar.t] = 1 THEN
            destreg := pickreg(t, RegSet { Codex86.EAX, Codex86.EBX,
                                           Codex86.ECX, Codex86.EDX } );
          ELSE
            destreg := pickreg(t, RegSet {}, mvar.t = Type.Addr);
          END;

          corrupt(t, destreg);
          t.reguse[destreg].locked := TRUE;
          indreg := pickreg(t, RegSet {}, TRUE);
          corrupt(t, indreg);

          t.cg.get_frame(indreg, mvar.var.parent, t.current_proc);
          t.cg.load_ind(destreg, t.cg.reg[indreg], mvar.o + mvar.var.offset,
                        mvar.t);
          set_reg(t, t.stacktop, destreg);
          newdest(t, stack0);
        ELSE

          stack0.loc := OLoc.mem;
          stack0.mvar := mvar;
        END
      END
    END;

    INC(t.stacktop);
  END push;

PROCEDURE pop (t: T; READONLY mvar: MVar) =
  VAR indreg: Regno;
  BEGIN
    IF t.stacktop < 1 THEN
      t.Err("Stack underflow in pop");
    END;

    WITH stack0 = t.vstack[t.stacktop - 1] DO
      IF stack0.loc = OLoc.fstack THEN
        IF mvar.var.loc = VLoc.temp AND mvar.var.parent # t.current_proc THEN
          unlock(t);
          indreg := pickreg(t, RegSet {}, TRUE);
          corrupt(t, indreg);
          t.cg.get_frame(indreg, mvar.var.parent, t.current_proc);
          t.cg.f_storeind(t.cg.reg[indreg], mvar.o + mvar.var.offset, mvar.t);

        ELSE
          t.cg.fstack_pop(mvar);
        END
      ELSE
        unlock(t);
        IF CG_Bytes[mvar.t] = 1 AND stack0.loc # OLoc.imm THEN
          find(t, t.stacktop - 1, Force.regset,
               RegSet { Codex86.EAX, Codex86.EBX,
                        Codex86.ECX, Codex86.EDX } );
        ELSE
          find(t, t.stacktop - 1, Force.anyregimm);
        END;

        IF mvar.var.loc = VLoc.temp AND mvar.var.parent # t.current_proc THEN
          indreg := pickreg(t, RegSet {}, TRUE);
          corrupt(t, indreg);
          t.cg.get_frame(indreg, mvar.var.parent, t.current_proc);
          t.cg.store_ind(stack0, t.cg.reg[indreg], mvar.o + mvar.var.offset,
                         mvar.t);
          t.reguse[stack0.reg].stackp := -1;
          corrupt(t, stack0.reg);

        ELSE
          sweep(t, mvar);

          FOR i := 0 TO NRegs DO
            IF t.reguse[i].last_store = mvar THEN
              t.reguse[i].last_store := NoStore;
            END
          END;

          IF stack0.loc = OLoc.register THEN
            t.reguse[stack0.reg].stackp := -1;
            t.reguse[stack0.reg].last_store := mvar;
          END;

          t.cg.movOp(Operand { loc := OLoc.mem, mvar := mvar }, stack0);
          set_mvar(t, t.stacktop - 1, mvar);
        END
      END
    END;

    DEC(t.stacktop);
  END pop;

PROCEDURE doloadaddress (t: T; v: x86Var; o: ByteOffset) =
  BEGIN
    unlock(t);
    pushnew(t, Type.Addr, Force.anyreg);

    WITH stop0 = t.vstack[pos(t, 0, "doloadaddress")] DO
      IF v.loc = VLoc.temp AND v.parent # t.current_proc THEN
        t.cg.get_frame(stop0.reg, v.parent, t.current_proc);
        t.cg.immOp(Op.oADD, t.cg.reg[stop0.reg], o + v.offset);

      ELSE
        t.cg.binOp(Op.oLEA, stop0, Operand {loc := OLoc.mem,
                                            mvar := MVar {var := v, o := o,
                                                          t := Type.Int} } );
      END
    END
  END doloadaddress;

PROCEDURE findbin (t: T; symmetric, overwritesdest: BOOLEAN;
                   VAR dest, src: INTEGER): BOOLEAN =
  VAR reversed := FALSE;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "findbin"), stack1 = pos(t, 1, "findbin") DO
      find(t, stack0, Force.any);

      find(t, stack1, Force.any);
      WITH stop0 = t.vstack[stack0], stop1 = t.vstack[stack1] DO
        IF symmetric THEN
          IF stop0.loc = OLoc.register OR stop1.loc = OLoc.imm OR
             (stop0.loc = OLoc.mem AND stop0.mvar.var.stack_temp AND
              stop1.loc # OLoc.register) THEN
            dest := stack0;
            src := stack1;
            reversed := TRUE;
          ELSE
            dest := stack1;
            src := stack0;
          END
        ELSE
          dest := stack1;
          src := stack0;
        END
      END
    END;

    WITH destop = t.vstack[dest], srcop = t.vstack[src] DO
      IF destop.loc = OLoc.mem AND NOT destop.mvar.var.stack_temp
         AND overwritesdest THEN
        find(t, dest, Force.anyreg);
      END;

      IF destop.loc = OLoc.imm THEN
        find(t, dest, Force.anyreg);
      END;

      IF destop.loc = OLoc.mem AND
         (CG_Bytes[destop.mvar.t] # 4 OR srcop.loc = OLoc.mem) THEN
        find(t, dest, Force.anyreg);
      END;

      IF srcop.loc = OLoc.mem AND CG_Bytes[srcop.mvar.t] # 4 THEN
        find(t, src, Force.anyreg);
      END
    END;

    RETURN reversed;
  END findbin;

PROCEDURE dobin (t: T; op: Op; symmetric, overwritesdest: BOOLEAN): BOOLEAN =
  VAR src, dest: INTEGER;
      reversed: BOOLEAN;
  BEGIN
    reversed := findbin(t, symmetric, overwritesdest, dest, src);

    WITH destop = t.vstack[dest], srcop = t.vstack[src] DO
      t.cg.binOp(op, destop, srcop);

      IF overwritesdest THEN
        newdest(t, destop);
        IF reversed THEN
          swap(t);
        END;
        discard(t, 1);
      ELSE
        discard(t, 2);
      END
    END;

    RETURN reversed;
  END dobin;

PROCEDURE dostoreind (t: T; o: ByteOffset; type: MType) =
  BEGIN
    WITH stack0 = pos(t, 0, "store_indirect"),
         stack1 = pos(t, 1, "store_indirect") DO
      find(t, stack1, Force.any, RegSet {}, TRUE);
      IF CG_Bytes[type] = 1 AND t.vstack[stack0].loc # OLoc.imm THEN
        find(t, stack0, Force.regset, RegSet { Codex86.EAX, Codex86.EBX,
                                               Codex86.ECX, Codex86.EDX } );
      ELSE
        find(t, stack0, Force.anyregimm);
      END;

      IF t.vstack[stack1].loc # OLoc.register THEN
        find(t, stack1, Force.anyreg, RegSet {}, TRUE);
      END;

      t.cg.store_ind(t.vstack[stack0], t.vstack[stack1], o, type);
    END;

    discard(t, 2);
  END dostoreind;

PROCEDURE doumul (t: T) =
  VAR otherop: Operand;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doumul"), stack1 = pos(t, 1, "doumul") DO
      WITH stop0 = t.vstack[stack0], stop1 = t.vstack[stack1] DO
        IF stop0.loc = OLoc.register AND stop0.reg = Codex86.EAX THEN
          lock(t, Codex86.EAX);
          find(t, stack1, Force.anydword);
          otherop := stop1;
        ELSIF stop1.loc = OLoc.register AND stop1.reg = Codex86.EAX THEN
          lock(t, Codex86.EAX);
          find(t, stack0, Force.anydword);
          otherop := stop0;
        ELSIF stop0.loc = OLoc.register THEN
          find(t, stack0, Force.regset, RegSet {Codex86.EAX});
          find(t, stack1, Force.anydword);
          otherop := stop1;
        ELSE
          find(t, stack1, Force.regset, RegSet {Codex86.EAX});
          find(t, stack0, Force.anydword);
          otherop := stop0;
        END
      END;

      IF otherop.loc = OLoc.imm THEN
        IF otherop = t.vstack[stack1] THEN
          find(t, stack1, Force.anyreg);
          otherop := t.vstack[stack1];
        ELSE
          find(t, stack0, Force.anyreg);
          otherop := t.vstack[stack0];
        END
      END;

      IF otherop.loc # OLoc.register OR otherop.reg # Codex86.EDX THEN
        corrupt(t, Codex86.EDX);
      END;

      t.cg.mulOp(otherop);
      IF otherop = t.vstack[stack1] THEN
        swap(t);
      END;

      newdest(t, t.cg.reg[Codex86.EDX]);
      newdest(t, t.cg.reg[Codex86.EAX]);
      discard(t, 1);
    END
  END doumul;

PROCEDURE doimul (t: T) =
  VAR dest, src: Operand;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doimul"), stack1 = pos(t, 1, "doimul") DO
      WITH stop0 = t.vstack[stack0], stop1 = t.vstack[stack1] DO
        find(t, stack1, Force.any);
        IF stop1.loc = OLoc.register THEN
          find(t, stack0, Force.anydword);
          dest := stop1;
          src := stop0;
        ELSE
          find(t, stack0, Force.anyreg);
          find(t, stack1, Force.anydword);
          dest := stop0;
          src := stop1;
          swap(t);
        END
      END;

      t.cg.imulOp(dest, src);
      newdest(t, dest);
      discard(t, 1);
    END
  END doimul;

PROCEDURE dodiv (t: T; a, b: Sign) =
  VAR neglabel: Label;
  BEGIN
    unlock(t);

    corrupt(t, Codex86.EDX);
    lock(t, Codex86.EDX);

    WITH stack0 = pos(t, 0, "dodiv"), stack1 = pos(t, 1, "dodiv") DO
      find(t, stack1, Force.regset, RegSet {Codex86.EAX});

      IF a # Sign.Unknown AND b # Sign.Unknown THEN
        find(t, stack0, Force.anydword);
        IF t.vstack[stack0].loc = OLoc.imm THEN
          find(t, stack0, Force.anyreg);
        END;

        IF a = Sign.Positive THEN
          t.cg.binOp(Op.oXOR, t.cg.reg[Codex86.EDX], t.cg.reg[Codex86.EDX]);
        ELSE
          t.cg.noargOp(Op.oCDQ);
        END;

        IF a = Sign.Positive AND b = Sign.Positive THEN
          t.cg.divOp(t.vstack[stack0]);
        ELSE
          t.cg.idivOp(t.vstack[stack0]);
        END;

        IF (a = Sign.Positive AND b = Sign.Negative) OR
           (a = Sign.Negative AND b = Sign.Positive) THEN
          t.cg.immOp(Op.oCMP, t.cg.reg[Codex86.EDX], 0);

          neglabel := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.E, neglabel);
          t.cg.decOp(t.cg.reg[Codex86.EAX]);

          t.cg.set_label(neglabel);
        END
      ELSE
        find(t, stack0, Force.anyreg);
        t.cg.diffdivOp(t.vstack[stack0], a = Sign.Positive);
      END;
        
      newdest(t, t.vstack[stack1]);
      discard(t, 1);
    END
  END dodiv;

PROCEDURE domod (t: T; a, b: Sign) =
  VAR neglabel: Label;
  BEGIN
    unlock(t);

    corrupt(t, Codex86.EDX);
    lock(t, Codex86.EDX);

    WITH stack0 = pos(t, 0, "domod"), stack1 = pos(t, 1, "domod") DO
      find(t, stack1, Force.regset, RegSet {Codex86.EAX});
      IF (a = Sign.Positive AND b = Sign.Positive) OR
         (a = Sign.Negative AND b = Sign.Negative) THEN
        find(t, stack0, Force.anydword);
        IF t.vstack[stack0].loc = OLoc.imm THEN
          find(t, stack0, Force.anyreg);
        END;
      ELSE
        find(t, stack0, Force.anyreg);
      END;

      IF a # Sign.Unknown AND b # Sign.Unknown THEN
        IF a = Sign.Positive THEN
          t.cg.binOp(Op.oXOR, t.cg.reg[Codex86.EDX], t.cg.reg[Codex86.EDX]);
        ELSE
          t.cg.noargOp(Op.oCDQ);
        END;

        IF a = Sign.Positive AND b = Sign.Positive THEN
          t.cg.divOp(t.vstack[stack0]);
        ELSE
          t.cg.idivOp(t.vstack[stack0]);
        END;

        IF (a = Sign.Positive AND b = Sign.Negative) OR
           (a = Sign.Negative AND b = Sign.Positive) THEN
          t.cg.immOp(Op.oCMP, t.cg.reg[Codex86.EDX], 0);

          neglabel := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.E, neglabel);
          t.cg.binOp(Op.oADD, t.cg.reg[Codex86.EDX], t.vstack[stack0]);

          t.cg.set_label(neglabel);
        END
      ELSE
        t.cg.diffmodOp(t.vstack[stack0], a = Sign.Positive);
      END;

      newdest(t, t.vstack[stack1]);
      dealloc_reg(t, stack1);
      set_reg(t, stack1, Codex86.EDX);
      discard(t, 1);
    END
  END domod;

PROCEDURE doimm (t: T; op: Op; imm: INTEGER; overwritesdest: BOOLEAN) =
  BEGIN
    unlock(t);

    WITH stack0 = pos(t, 0, "doimm"), stop0 = t.vstack[stack0] DO
      IF (stop0.loc = OLoc.mem AND
         ((overwritesdest AND NOT stop0.mvar.var.stack_temp) OR
          CG_Bytes[stop0.mvar.t] = 2 OR
          (CG_Bytes[stop0.mvar.t] = 1 AND (imm >= 16_80 OR imm <= -16_81))))
         OR stop0.loc = OLoc.imm THEN
        find(t, stack0, Force.anyreg);
      ELSE
        find(t, stack0, Force.any);
      END;

      t.cg.immOp(op, stop0, imm);

      IF overwritesdest THEN
        newdest(t, stop0);
      ELSE
        discard(t, 1);
      END
    END
  END doimm;

PROCEDURE doneg (t: T) =
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doneg"), stop0 = t.vstack[stack0] DO
      IF stop0.loc = OLoc.imm THEN
        stop0.imm := -stop0.imm
      ELSE
        find(t, stack0, Force.anytemp);
        t.cg.unOp(Op.oNEG, stop0);

        newdest(t, stop0);
      END
    END
  END doneg;

PROCEDURE doabs (t: T) =
  VAR lab: Label;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doabs"), stop0 = t.vstack[stack0] DO
      IF stop0.loc = OLoc.imm THEN
        stop0.imm := ABS(stop0.imm)
      ELSE
        find(t, stack0, Force.anytemp);

        IF stop0.loc = OLoc.mem THEN
          t.cg.immOp(Op.oCMP, stop0, 0);

          lab := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.GE, lab);
          t.cg.unOp(Op.oNEG, stop0);

          t.cg.set_label(lab);
        ELSE
          t.cg.unOp(Op.oNEG, stop0);

          lab := t.cg.reserve_labels(1, TRUE);

          t.cg.brOp(Cond.G, lab);
          t.cg.unOp(Op.oNEG, stop0);

          t.cg.set_label(lab);
        END;

        newdest(t, stop0);
      END
    END
  END doabs;

PROCEDURE doshift (t: T) =
  VAR ovflshift, leftlab, endlab: Label;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doshift"), stack1 = pos(t, 1, "doshift"),
         stop0 = t.vstack[stack0], stop1 = t.vstack[stack1] DO
      IF stop0.loc = OLoc.imm THEN
        IF stop1.loc = OLoc.imm THEN
          stop1.imm := Word.Shift(stop1.imm, stop0.imm);
        ELSE
          IF stop0.imm # 0 THEN
            find(t, stack1, Force.anytemp);

            IF stop0.imm > 0 THEN
              IF stop0.imm > 31 THEN
                t.cg.binOp(Op.oXOR, stop1, stop1);
              ELSE
                t.cg.immOp(Op.oSAL, stop1, stop0.imm);
              END
            ELSE
              IF stop0.imm < -31 THEN
                t.cg.binOp(Op.oXOR, stop1, stop1);
              ELSE
                t.cg.immOp(Op.oSHR, stop1, -stop0.imm);
              END
            END;

            newdest(t, stop1);
          END
        END
      ELSE
        find(t, stack0, Force.regset, RegSet {Codex86.ECX});

        find(t, stack1, Force.anytemp);
        IF stop1.loc = OLoc.imm THEN
          find(t, stack1, Force.anyreg);
        END;

        t.cg.immOp(Op.oCMP, stop0, 0);

        leftlab := t.cg.reserve_labels(1, TRUE);
        ovflshift := t.cg.reserve_labels(1, TRUE);
        endlab := t.cg.reserve_labels(1, TRUE);

        t.cg.brOp(Cond.GE, leftlab);
        t.cg.unOp(Op.oNEG, stop0);
        t.cg.immOp(Op.oCMP, stop0, 32);
        t.cg.brOp(Cond.GE, ovflshift);
        t.cg.unOp(Op.oSHR, stop1);
        t.cg.brOp(Cond.Always, endlab);
        t.cg.set_label(ovflshift);
        (* .ovflshift *)
        t.cg.binOp(Op.oXOR, stop1, stop1);
        t.cg.brOp(Cond.Always, endlab);
        t.cg.set_label(leftlab);
        (* .leftlab *)
        t.cg.immOp(Op.oCMP, stop0, 32);
        t.cg.brOp(Cond.GE, ovflshift);
        t.cg.unOp(Op.oSAL, stop1);
        t.cg.set_label(endlab);
        (* .endlab  *)

        newdest(t, stop1);
        newdest(t, stop0);
      END;

      discard(t, 1);
    END
  END doshift;

PROCEDURE dorotate (t: T) =
  VAR leftlab, endlab: Label;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "dorotate"), stack1 = pos(t, 1, "dorotate"),
         stop0 = t.vstack[stack0], stop1 = t.vstack[stack1] DO
      IF stop0.loc = OLoc.imm THEN
        IF stop1.loc = OLoc.imm THEN
          stop1.imm := Word.Rotate(stop1.imm, stop0.imm);
        ELSE
          IF stop0.imm # 0 THEN
            find(t, stack1, Force.anytemp);

            IF stop0.imm > 0 THEN
              stop0.imm := Word.And(stop0.imm, 16_1F);
              t.cg.immOp(Op.oROL, stop1, stop0.imm);
            ELSE
              stop0.imm := Word.And(-stop0.imm, 16_1F);
              t.cg.immOp(Op.oROR, stop1, stop0.imm);
            END;

            newdest(t, stop1);
          END
        END
      ELSE
        find(t, stack0, Force.regset, RegSet {Codex86.ECX});

        find(t, stack1, Force.anytemp);
        IF stop1.loc = OLoc.imm THEN
          find(t, stack1, Force.anyreg);
        END;

        t.cg.immOp(Op.oCMP, stop0, 0);

        leftlab := t.cg.reserve_labels(1, TRUE);
        endlab := t.cg.reserve_labels(1, TRUE);

        t.cg.brOp(Cond.GE, leftlab);
        t.cg.unOp(Op.oNEG, stop0);
        t.cg.unOp(Op.oROR, stop1);
        t.cg.brOp(Cond.Always, endlab);
        t.cg.set_label(leftlab);
        (* .leftlab *)
        t.cg.unOp(Op.oROL, stop1);
        t.cg.set_label(endlab);
        (* .endlab  *)

        newdest(t, stop1);
        newdest(t, stop0);
      END;

      discard(t, 1);
    END
  END dorotate;

PROCEDURE doextract (t: T; sign: BOOLEAN) =
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "extract"), stack1 = pos(t, 1, "extract"),
         stack2 = pos(t, 2, "extract"),
         stop0 = t.vstack[stack0], stop1 = t.vstack[stack1],
         stop2 = t.vstack[stack2] DO

      IF stop0.loc = OLoc.imm THEN
        discard(t, 1);
        doextract_n(t, sign, stop0.imm);
        RETURN;
      END;

      (* The register allocation will sometimes do more memory operations
         than necessary, however I thought the time wasted in rare cases
         was worth the easier debugging task, since it was going to be
         really messy to cover all the special cases correctly *)

      IF sign THEN
        find(t, stack0, Force.regset, RegSet { Codex86.ECX });
        find(t, stack1, Force.any);
        find(t, stack2, Force.anyreg);
        IF stop1.loc = OLoc.mem AND CG_Bytes[stop1.mvar.t] < 4 THEN
          find(t, stack1, Force.anyreg);
        END;

        t.cg.binOp(Op.oADD, stop0, stop1);
        t.cg.unOp(Op.oNEG, stop0);
        t.cg.unOp(Op.oSAL, stop2);
        t.cg.binOp(Op.oADD, stop0, stop1);
        t.cg.unOp(Op.oSAR, stop2);

        newdest(t, stop0);

      ELSE
        IF stop1.loc = OLoc.imm THEN
          stop1.imm := Word.And(stop1.imm, 16_1F);
        ELSE
          find(t, stack1, Force.regset, RegSet { Codex86.ECX });
        END;

        find(t, stack0, Force.any);
        find(t, stack2, Force.anyreg);
        IF stop0.loc # OLoc.register THEN
          find(t, stack0, Force.anyreg);
        END;

        IF stop1.loc = OLoc.imm THEN
          t.cg.immOp(Op.oSHR, stop2, stop1.imm);
        ELSE
          t.cg.unOp(Op.oSHR, stop2);
        END;

        t.cg.tableOp(Op.oAND, stop2, stop0, 4, t.lowset_table);
      END;

      newdest(t, stop2);
      discard(t, 2);
    END
  END doextract;

PROCEDURE doextract_n (t: T; sign: BOOLEAN; n: INTEGER) =
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "extract_n"), stack1 = pos(t, 1, "extract_n"),
         stop0 = t.vstack[stack0], stop1 = t.vstack[stack1] DO

      IF stop0.loc = OLoc.imm THEN
        discard(t, 1);
        doextract_mn(t, sign, stop0.imm, n);
        RETURN;
      END;

      IF sign THEN
        corrupt(t, Codex86.ECX);
        t.reguse[Codex86.ECX].locked := TRUE;

        find(t, stack0, Force.any);
        find(t, stack1, Force.anyreg);
        IF stop0.loc = OLoc.mem AND CG_Bytes[stop0.mvar.t] < 4 THEN
          find(t, stack0, Force.anyreg);
        END;

        t.cg.movImm(t.cg.reg[Codex86.ECX], 32 - n);
        t.cg.binOp(Op.oSUB, t.cg.reg[Codex86.ECX], stop0);
        t.cg.unOp(Op.oSAL, stop1);

        IF n < 32 THEN
          t.cg.immOp(Op.oSAR, stop1, 32 - n);
        END
      ELSE
        find(t, stack0, Force.regset, RegSet { Codex86.ECX });
        find(t, stack1, Force.anyreg);

        t.cg.unOp(Op.oSHR, stop1);

        IF n < 32 THEN
          WITH andval = Word.Shift(16_ffffffff, n - 32) DO
            t.cg.immOp(Op.oAND, stop1, andval);
          END
        END
      END;

      newdest(t, stop1);
      discard(t, 1);
    END
  END doextract_n;

PROCEDURE doextract_mn (t: T; sign: BOOLEAN; m, n: INTEGER) =
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "extract_mn"), stop0 = t.vstack[stack0] DO

      IF stop0.loc = OLoc.imm THEN
        stop0.imm := Word.Shift(stop0.imm, -m);
        stop0.imm := Word.And(stop0.imm, Word.Shift(16_FFFFFFFF, n - 32));

        IF sign AND Word.And(stop0.imm, Word.Shift(1, n-1)) # 0 THEN
          stop0.imm := Word.Or(stop0.imm, Word.Shift(16_FFFFFFFF, n));
        END;
        RETURN;
      END;

      IF sign THEN
        find(t, stack0, Force.anyreg);
        IF (m + n) < 32 THEN
          t.cg.immOp(Op.oSAL, stop0, 32 - (m + n));
        END;

        IF n < 32 THEN
          t.cg.immOp(Op.oSAR, stop0, 32 - n);
        END
      ELSE
        find(t, stack0, Force.anyreg);
        IF (m + n) < 32 THEN
          WITH andval = Word.Shift(16_ffffffff, m + n - 32) DO
            t.cg.immOp(Op.oAND, stop0, andval);
          END
        END;

        IF m > 0 THEN
          t.cg.immOp(Op.oSHR, stop0, m);
        END
      END;

      newdest(t, stop0);
    END
  END doextract_mn;

PROCEDURE doinsert (t: T) =
  VAR maskreg: Regno;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "insert"), stack1 = pos(t, 1, "insert"),
         stack2 = pos(t, 2, "insert"), stack3 = pos(t, 3, "insert"),
         stop0 = t.vstack[stack0], stop1 = t.vstack[stack1],
         stop2 = t.vstack[stack2], stop3 = t.vstack[stack3] DO

      IF stop0.loc = OLoc.imm THEN
        discard(t, 1);
        doinsert_n(t, stop0.imm);
        RETURN;
      END;

      IF stop1.loc = OLoc.imm THEN
        stop1.imm := Word.And(stop1.imm, 16_1f);
      ELSE
        find(t, stack1, Force.regset, RegSet { Codex86.ECX });
      END;

      find(t, stack2, Force.any);
      find(t, stack3, Force.any);
      find(t, stack0, Force.anyreg);

      IF stop2.loc # OLoc.register THEN
        find(t, stack2, Force.anyreg);
      END;

      IF stop3.loc # OLoc.register THEN
        find(t, stack3, Force.anyreg);
      END;

      maskreg := pickreg(t);
      corrupt(t, maskreg);

      t.cg.tableOp(Op.oMOV, t.cg.reg[maskreg], stop0, 4, t.lowset_table);
      t.cg.binOp(Op.oAND, stop2, t.cg.reg[maskreg]);

      IF stop1.loc = OLoc.imm THEN
        IF stop1.imm # 0 THEN
          t.cg.immOp(Op.oSAL, stop2, stop1.imm);
          t.cg.immOp(Op.oADD, stop0, stop1.imm);
        END
      ELSE
        t.cg.unOp(Op.oSAL, stop2);
        t.cg.binOp(Op.oADD, stop0, stop1);
      END;

      t.cg.tableOp(Op.oMOV, t.cg.reg[maskreg], stop0, 4, t.lowset_table);

      IF stop1.loc = OLoc.imm THEN
        t.cg.immOp(Op.oXOR, t.cg.reg[maskreg],
                   Word.Shift(16_FFFFFFFF, stop1.imm));
      ELSE
        t.cg.tableOp(Op.oXOR, t.cg.reg[maskreg], stop1, 4, t.highset_table);
      END;

      t.cg.binOp(Op.oAND, stop3, t.cg.reg[maskreg]);
      t.cg.binOp(Op.oOR, stop3, stop2);

      newdest(t, stop0);
      newdest(t, stop2);
      newdest(t, stop3);
      discard(t, 3);
    END
  END doinsert;

PROCEDURE doinsert_n (t: T; n: INTEGER) =
  VAR intable: MVar;
      maskreg: Regno;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "insert"), stack1 = pos(t, 1, "insert"),
         stack2 = pos(t, 2, "insert"),
         stop0 = t.vstack[stack0], stop1 = t.vstack[stack1],
         stop2 = t.vstack[stack2] DO

      IF stop0.loc = OLoc.imm THEN
        discard(t, 1);
        doinsert_mn(t, stop0.imm, n);
        RETURN;
      END;

      find(t, stack0, Force.regset, RegSet { Codex86.ECX });
      find(t, stack2, Force.any);
      find(t, stack1, Force.anyreg);

      IF stop2.loc # OLoc.register THEN
        find(t, stack2, Force.anyreg);
      END;

      maskreg := pickreg(t);
      corrupt(t, maskreg);

      IF n # 32 THEN
        t.cg.immOp(Op.oAND, stop1, Word.Shift(16_ffffffff, n - 32));
      END;

      t.cg.unOp(Op.oSAL, stop1);

      intable := t.lowset_table;
      INC(intable.o, Word.Shift(n*4, 16));
      t.cg.tableOp(Op.oMOV, t.cg.reg[maskreg], stop0, 4, intable);
      t.cg.tableOp(Op.oXOR, t.cg.reg[maskreg], stop0, 4, t.highset_table);
      t.cg.binOp(Op.oAND, stop2, t.cg.reg[maskreg]);
      t.cg.binOp(Op.oOR, stop2, stop1);

      newdest(t, stop1);
      newdest(t, stop2);
      discard(t, 2);
    END
  END doinsert_n;

PROCEDURE doinsert_mn (t: T; m, n: INTEGER) =
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "insert"), stack1 = pos(t, 1, "insert"),
         stop0 = t.vstack[stack0], stop1 = t.vstack[stack1] DO

      find(t, stack1, Force.any);
      find(t, stack0, Force.anyregimm);

      IF stop1.loc = OLoc.mem THEN
        find(t, stack1, Force.anyreg);
      END;

      IF stop0.loc = OLoc.imm THEN
        stop0.imm := Word.And(stop0.imm, Word.Shift(16_FFFFFFFF, n - 32));
        stop0.imm := Word.Shift(stop0.imm, m);
      ELSE
        IF (n + m) < 32 THEN
          t.cg.immOp(Op.oAND, stop0, Word.Shift(16_ffffffff, n - 32));
        END;

        IF m # 0 THEN
          t.cg.immOp(Op.oSAL, stop0, m);
        END
      END;

      WITH mask = Word.Xor(Word.Shift(16_ffffffff, m),
                           Word.Shift(16_ffffffff, m + n - 32)) DO
        IF mask # 16_ffffffff THEN
          IF stop1.loc = OLoc.imm THEN
            stop1.imm := Word.And(stop1.imm, mask);
          ELSE
            t.cg.immOp(Op.oAND, stop1, mask);
          END
        END
      END;

      IF stop1.loc = OLoc.imm THEN
        IF stop0.loc = OLoc.imm THEN
          stop1.imm := Word.Or(stop1.imm, stop0.imm);
        ELSE
          swap(t);
          IF stop0.loc # OLoc.imm OR stop0.imm # 0 THEN
            t.cg.binOp(Op.oOR, stop1, stop0);
          END
        END
      ELSE
        IF stop0.loc # OLoc.imm OR stop0.imm # 0 THEN
          t.cg.binOp(Op.oOR, stop1, stop0);
        END
      END;

      newdest(t, stop0);
      newdest(t, stop1);
      discard(t, 1);
    END
  END doinsert_mn;

PROCEDURE swap (t: T) =
  VAR tmp: Operand;
  BEGIN
    WITH stack0 = pos(t, 0, "swap"), stack1 = pos(t, 1, "swap") DO
      tmp := t.vstack[stack0];
      t.vstack[stack0] := t.vstack[stack1];
      t.vstack[stack1] := tmp;

      t.vstack[stack0].stackp := stack0;
      t.vstack[stack1].stackp := stack1;

      IF t.vstack[stack0].loc = OLoc.register THEN
        <* ASSERT t.reguse[t.vstack[stack0].reg].stackp = stack1 *>
        t.reguse[t.vstack[stack0].reg].stackp := stack0;
      END;

      IF t.vstack[stack1].loc = OLoc.register THEN
        <* ASSERT t.reguse[t.vstack[stack1].reg].stackp = stack0 *>
        t.reguse[t.vstack[stack1].reg].stackp := stack1;
      END;

      IF t.vstack[stack0].loc = OLoc.fstack AND
        t.vstack[stack1].loc = OLoc.fstack THEN
        t.cg.fstack_swap();
      END
    END
  END swap;

PROCEDURE doloophole (t: T; from, two: ZType) =
  BEGIN
      WITH stack0 = pos(t, 0, "doloophole"), stop0 = t.vstack[stack0] DO
        IF from >= Type.Reel AND two < Type.Reel THEN
          <* ASSERT stop0.loc = OLoc.fstack *>
          stop0.loc := OLoc.mem;
          stop0.mvar.var := t.parent.declare_temp(CG_Bytes[two],
                                                  CG_Align_bytes[two], two,
                                                  FALSE);
          stop0.mvar.var.stack_temp := TRUE;
          stop0.mvar.o := 0;
          stop0.mvar.t := from;
          t.cg.fstack_pop(stop0.mvar);
          stop0.mvar.t := two;

        ELSIF from < Type.Reel AND two >= Type.Reel THEN
          IF stop0.loc = OLoc.mem AND CG_Bytes[stop0.mvar.t] # 4 THEN
            unlock(t);
            find(t, stack0, Force.anyreg);
          END;

          IF stop0.loc = OLoc.register OR stop0.loc = OLoc.imm THEN
            find(t, stack0, Force.mem);
          END;
          (******* BOGUS  - WKK 2/7/95 *****************
          find(t, stack0, Force.mem);
          **********************************************)

          <* ASSERT two = Type.Reel *>
          stop0.mvar.t := two;
          t.cg.fstack_push(stop0.mvar, TRUE);
          IF stop0.mvar.var.stack_temp THEN
            t.parent.free_temp(stop0.mvar.var);
          END;
          stop0.loc := OLoc.fstack;
        END
      END
  END doloophole;

PROCEDURE doindex_address (t: T; shift, size: INTEGER; neg: BOOLEAN) =
  VAR imsize: INTEGER;
      muldest: Regno;
  BEGIN
    unlock(t);
    WITH stack0 = pos(t, 0, "doindex_address"),
         stack1 = pos(t, 1, "doindex_address"),
         stop0 = t.vstack[stack0], stop1 = t.vstack[stack1] DO
      find(t, stack0, Force.any);
      find(t, stack1, Force.anyreg, RegSet {}, TRUE);

      IF stop0.loc = OLoc.imm THEN
        stop0.imm := stop0.imm * size;
      ELSE
        IF stop0.loc # OLoc.register AND shift >= 0 THEN
          find(t, stack0, Force.anyreg);
        END;
        IF stop0.loc = OLoc.mem AND shift < 0 AND
          CG_Bytes[stop0.mvar.t] = 1 THEN
          find(t, stack0, Force.anydword);
        END;

        IF shift < 0 THEN
          IF size < 16_80 AND size > -16_81 THEN
            imsize := 1;
          ELSE
            imsize := 4;
          END;

          IF stop0.loc # OLoc.register THEN
            muldest := pickreg(t);
            corrupt(t, muldest);
            t.cg.imulImm(t.cg.reg[muldest], stop0, size, imsize);
            set_reg(t, stack0, muldest);

          ELSE
            t.cg.imulImm(stop0, stop0, size, imsize);
            newdest(t, stop0);
          END

        ELSIF shift > 0 THEN
          t.cg.immOp(Op.oSAL, stop0, shift);
          newdest(t, stop0);
        END
      END;

      IF neg THEN
        t.cg.binOp(Op.oSUB, stop1, stop0);
      ELSE
        t.cg.binOp(Op.oADD, stop1, stop0);
      END;

      newdest(t, stop1);
      discard(t, 1);
    END
  END doindex_address;

TYPE MaxMinRec = RECORD
  regreg, regmem, memreg: Cond;
END;

TYPE MaxMinCond = ARRAY [Type.Word .. Type.Reel] OF MaxMinRec;

CONST maxmincond = ARRAY MaxMin OF MaxMinCond
  { MaxMinCond { MaxMinRec { Cond.A, Cond.AE, Cond.BE },
                 MaxMinRec { Cond.G, Cond.GE, Cond.LE },
                 MaxMinRec { Cond.AE, Cond.AE, Cond.AE } },
    MaxMinCond { MaxMinRec { Cond.B, Cond.BE, Cond.AE },
                 MaxMinRec { Cond.L, Cond.LE, Cond.GE },
                 MaxMinRec { Cond.BE, Cond.BE, Cond.BE } } };

PROCEDURE domaxmin (t: T; type: ZType; maxmin: MaxMin) =
  VAR lab, end: Label;
      src, dest: INTEGER;
      ftop_inmem: BOOLEAN;
      cond: Cond;
  BEGIN
    IF type >= Type.Reel THEN
      t.cg.binFOp(FOp.fCOM, 1);

      ftop_inmem := t.cg.ftop_inmem;

      corrupt(t, Codex86.EAX);
      t.cg.noargFOp(FOp.fNSTSWAX);
      t.cg.noargOp(Op.oSAHF);

      lab := t.cg.reserve_labels(1, TRUE);
      end := t.cg.reserve_labels(1, TRUE);

      cond := maxmincond[maxmin][Type.Reel].regreg;
      IF NOT ftop_inmem THEN
        cond := revcond[cond];
      END;

      t.cg.brOp(cond, lab);
      t.cg.binFOp(FOp.fSTP, 1);

      t.cg.brOp(Cond.Always, end);

      t.cg.set_label(lab);

      t.cg.ftop_inmem := FALSE;
      IF NOT ftop_inmem THEN
        t.cg.f_pushnew(); (* It thinks we have just discarded something
                             from the stack in the previous branch, so we
                             have to fool it into letting us discard it
                             again without getting its stack counts
                             mixed up *)
        t.cg.fstack_discard();
      END;

      t.cg.set_label(end);

    ELSE
      EVAL findbin(t, TRUE, TRUE, dest, src);

      WITH destop = t.vstack[dest], srcop = t.vstack[src] DO
        t.cg.binOp(Op.oCMP, destop, srcop);
        lab := t.cg.reserve_labels(1, TRUE);

        IF destop.loc = OLoc.register OR srcop.loc = OLoc.imm THEN
          IF srcop.loc = OLoc.register OR srcop.loc = OLoc.imm THEN
            t.cg.brOp(maxmincond[maxmin][type].regreg, lab);
          ELSE
            t.cg.brOp(maxmincond[maxmin][type].regmem, lab);
          END;

          t.cg.movOp(destop, srcop);

        ELSE
          t.cg.brOp(maxmincond[maxmin][type].memreg, lab);

          t.cg.movOp(srcop, destop);

        END;
        t.cg.set_label(lab);

        newdest(t, destop);
        IF dest > src THEN
          swap(t);
        END;
      END
    END;

    discard(t, 1);
  END domaxmin;

PROCEDURE fltoint (t: T; mode: FlToInt) =
  VAR status: x86Var;
      statusop, newstat: Operand;
      statreg: Regno;
  BEGIN
    status := t.parent.declare_temp(8, 4, Type.Int, FALSE);

    unlock(t);
    statreg := pickreg(t);
    corrupt(t, statreg);

    t.cg.noargOp(Op.oWAIT);
    t.cg.noargFOp(FOp.fNCLEX);

    statusop := Operand { loc := OLoc.mem,
                          mvar := MVar { var := status, o := 0,
                                         t := Type.Int } };
    newstat := Operand { loc := OLoc.mem,
                         mvar := MVar { var := status, o := 4,
                                         t := Type.Int } };
    t.cg.memFOp(FOp.fSTCW, statusop.mvar);

    t.cg.movOp(t.cg.reg[statreg], statusop);
    t.cg.immOp(Op.oAND, t.cg.reg[statreg], 16_0000F3FF);

    IF t.rmode[mode] # 0 THEN
      t.cg.immOp(Op.oOR, t.cg.reg[statreg], t.rmode[mode]);
    END;

    t.cg.movOp(newstat, t.cg.reg[statreg]);

    t.cg.memFOp(FOp.fLDCW, newstat.mvar);

    discard(t, 1);
    pushnew(t, Type.Int, Force.mem);

    t.cg.memFOp(FOp.fISTP, t.vstack[pos(t, 0, "fltoint")].mvar);

    t.cg.noargOp(Op.oWAIT);
    t.cg.noargFOp(FOp.fNCLEX);
    t.cg.memFOp(FOp.fLDCW, statusop.mvar);

    t.parent.free_temp(status);
  END fltoint;

PROCEDURE inttoflt (t: T) =
  BEGIN
    WITH stack0 = pos(t, 0, "inttoflt"), stop0 = t.vstack[stack0] DO
      IF stop0.loc = OLoc.mem AND CG_Bytes[stop0.mvar.t] # 4 THEN
        unlock(t);
        find(t, stack0, Force.anyreg);
      END;

      IF stop0.loc = OLoc.register OR stop0.loc = OLoc.imm THEN
        find(t, stack0, Force.mem);
      END;

      t.cg.memFOp(FOp.fILD, stop0.mvar);
      IF stop0.mvar.var.stack_temp THEN
        t.parent.free_temp(stop0.mvar.var);
      END;
      stop0.loc := OLoc.fstack;
    END
  END inttoflt;

PROCEDURE newdest (t: T; READONLY op: Operand) =
  BEGIN
    IF op.loc = OLoc.register THEN
      WITH z = t.reguse[op.reg] DO
        z.last_store := NoStore;
        z.upbound    := LAST (INTEGER);
        z.lowbound   := FIRST (INTEGER);
        z.imm        := FALSE;
        z.non_nil    := FALSE;
      END;
    END
  END newdest;

PROCEDURE expand_stack (t: T) =
  VAR newarr := NEW(REF ARRAY OF Operand, t.vstacklimit * 2);
  BEGIN
    FOR i := 0 TO (t.vstacklimit - 1) DO
      newarr[i] := t.vstack[i];
    END;

    t.vstacklimit := t.vstacklimit * 2;
    t.vstack := newarr;
  END expand_stack;

PROCEDURE discard (t: T; depth: INTEGER) =
  BEGIN
    IF depth > t.stacktop THEN
      t.Err("Stack underflow in stack_discard");
    END;
    FOR i := t.stacktop - depth TO t.stacktop - 1 DO
      WITH stackp = t.vstack[i] DO
        CASE stackp.loc OF
          OLoc.mem =>
            IF stackp.mvar.var.stack_temp THEN
              t.parent.free_temp(stackp.mvar.var);
            END
        | OLoc.register =>
            t.reguse[stackp.reg].stackp := -1;
        | OLoc.fstack =>
            (* The discards will have been done elsewhere *)
        | OLoc.imm =>
            (* Nothing to do *)
        END
      END
    END;
    t.stacktop := t.stacktop - depth;
  END discard;

PROCEDURE reg (t: T; stackp: INTEGER): Regno =
  BEGIN
    RETURN t.vstack[stackp].reg;
  END reg;

PROCEDURE lower (t: T; reg: Regno): INTEGER =
  BEGIN
    RETURN t.reguse[reg].lowbound;
  END lower;

PROCEDURE upper (t: T; reg: Regno): INTEGER =
  BEGIN
    RETURN t.reguse[reg].upbound;
  END upper;

PROCEDURE set_lower (t: T; reg: Regno; newlow: INTEGER) =
  BEGIN
    t.reguse[reg].lowbound := newlow;
  END set_lower;

PROCEDURE set_upper (t: T; reg: Regno; newup: INTEGER) =
  BEGIN
    t.reguse[reg].upbound := newup;
  END set_upper;

PROCEDURE non_nil (t: T; reg: Regno): BOOLEAN =
  BEGIN
    RETURN t.reguse[reg].non_nil;
  END non_nil;

PROCEDURE set_non_nil (t: T; reg: Regno) =
  BEGIN
    t.reguse[reg].non_nil := TRUE;
  END set_non_nil;

PROCEDURE set_error_handler (t: T; err: ErrorHandler) =
  BEGIN
    t.Err := err;
  END set_error_handler;

PROCEDURE init (t: T) =
  BEGIN
    t.stacktop := 0;
    t.current_proc := NIL;

    FOR i := 0 TO NRegs DO
      WITH z = t.reguse[i] DO
        z.stackp     := -1;
        z.last_store := NoStore;
        z.upbound    := LAST (INTEGER);
        z.lowbound   := FIRST (INTEGER);
        z.imm        := FALSE;
        z.non_nil    := FALSE;
        z.locked     := FALSE;
      END;
    END;

    t.rmode := ARRAY FlToInt OF INTEGER
      { 16_0000, 16_0400, 16_0800, 16_0F00 };
    t.lowset_table := MVar { var := t.cg.internalvar,
                             o := ORD(IntnlVar.Lowset_table),
                             t := Type.Int };
    t.highset_table := MVar { var := t.cg.internalvar,
                              o := ORD(IntnlVar.Highset_table),
                              t := Type.Int };

  END init;

PROCEDURE end (<*UNUSED*> t: T) =
  BEGIN
  END end;

PROCEDURE set_current_proc (t: T; p: x86Proc) =
  BEGIN
    t.current_proc := p;
  END set_current_proc;

PROCEDURE New (parent: M3x86Rep.U; cg: Codex86.T; debug: BOOLEAN): T =
  VAR stack := NEW(T,
                   parent := parent,
                   cg := cg,
                   debug := debug);
  BEGIN
    stack.vstacklimit := 16;
    stack.vstack := NEW(REF ARRAY OF Operand, stack.vstacklimit);
    RETURN stack;
  END New;

BEGIN
END Stackx86.
