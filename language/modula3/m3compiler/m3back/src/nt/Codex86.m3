(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Aug 27 09:23:28 PDT 1996 by najork     *)
(*      modified on Mon Oct 31 11:00:15 PST 1994 by isard      *)
(*      modified on Fri Nov 19 09:30:31 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

MODULE Codex86;

IMPORT TargetMap, M3x86Rep, M3ID, M3CG_Ops, Word, M3ObjFile, Wrx86, Target;
IMPORT TInt AS TargetInt;

FROM TargetMap IMPORT CG_Bytes;

FROM M3CG IMPORT ByteOffset, ByteSize;
FROM M3CG IMPORT Type, MType, Label, Alignment;
FROM M3CG_Ops IMPORT ErrorHandler;

FROM M3x86Rep IMPORT Operand, MVar, Regno, OLoc, VLoc, x86Var, x86Proc, NRegs;
FROM M3x86Rep IMPORT RegSet;

FROM M3ObjFile IMPORT Seg;

REVEAL T = Public BRANDED "Codex86.T" OBJECT
        parent        : M3x86Rep.U := NIL;
        obj           : M3ObjFile.T := NIL;
        debug         := FALSE;
        Err           : ErrorHandler := NIL;
        opcode        : ARRAY [0 .. NRegs] OF Operand;
        current_proc  : x86Proc;
        textsym       : INTEGER;
        tempsize      := 0;
        temparr       : REF ARRAY OF MVar;
        templimit     := 0;
        fspilltop     := 0;
        fspillhigh    := 0;
        fstackspill   : REF ARRAY OF Operand;
        fspilllimit   := 0;
        fstacksize    := 0;
        fstackloaded  := 0;
        ftop_mem      : MVar;
        labarr        : REF ARRAY OF x86Label;
        lablimit      := 0;
        next_label_id := 0;
        f_litlist     : FLiteral := NIL;
        abscall_list  : AbsCall := NIL;
        internal_list : Internal := NIL;
        flitvar       : x86Var := NIL;
      OVERRIDES
        init := init;
        end := end;
        wrFlush := wrFlush;
        set_obj := set_obj;
        set_current_proc := set_current_proc;
        set_textsym := set_textsym;
        intCall := intCall;
        relCall := relCall;
        absCall := absCall;
        rmCall := rmCall;
        cleanretOp := cleanretOp;
        brOp := brOp;
        setccOp := setccOp;
        noargOp := noargOp;
        noargFOp := noargFOp;
        immFOp := immFOp;
        binFOp := binFOp;
        memFOp := memFOp;
        assert_fstack := assert_fstack;
        f_ensureloaded := f_ensureloaded;
        f_pushnew := f_pushnew;
        f_exitproc := f_exitproc;
        fstack_push := fstack_push;
        fstack_pop := fstack_pop;
        fstack_swap := fstack_swap;
        fstack_discard := fstack_discard;
        f_loadlit := f_loadlit;
        immOp := immOp;
        binOp := binOp;
        tableOp := tableOp;
        swapOp := swapOp;
        movOp := movOp;
        movDummyReloc := movDummyReloc;
        movImm := movImm;
        MOVSWOp := MOVSWOp;
        STOSWOp := STOSWOp;
        CBWOp := CBWOp;
        pushOp := pushOp;
        popOp := popOp;
        decOp := decOp;
        unOp := unOp;
        mulOp := mulOp;
        imulOp := imulOp;
        imulImm := imulImm;
        divOp := divOp;
        idivOp := idivOp;
        diffdivOp := diffdivOp;
        diffmodOp := diffmodOp;
        must_extend := must_extend;
        get_addsize := get_addsize;
        aligned := aligned;
        reserve_labels := reserve_labels;
        set_label := set_label;
        case_jump := case_jump;
        load_ind := load_ind;
        fast_load_ind := fast_load_ind;
        store_ind := store_ind;
        f_loadind := f_loadind;
        f_storeind := f_storeind;
        log_label_init := log_label_init;
        get_frame := get_frame;
        set_error_handler := set_error_handler;
      END;

TYPE FLiteral = REF RECORD
  arr: ARRAY [0 .. 1] OF INTEGER;
  size: INTEGER;
  loc: ByteOffset;
  link: FLiteral;
END;

TYPE Internal = REF RECORD
  ivar: IntnlVar;
  loc: ByteOffset;
  link: Internal;
END;

PROCEDURE intCall (t: T; l: Label) =
  VAR rel: INTEGER;
  BEGIN
    check_label(t, l, "intCall");
    WITH lab = t.labarr[l], curs = t.obj.cursor(Seg.Text) DO
      IF lab.no_address THEN
        rel := 0;
      ELSE
        rel := lab.offset - (curs + 5);
      END;

      Mn(t, "CALL rel32");

      writecode(t, FALSE, 16_E8, 0, FALSE, 0, FALSE, rel, 4, 0, 0);

      IF lab.no_address THEN
        log_unknown_label(t, l, t.obj.cursor(Seg.Text) - 4, FALSE);
      END
    END
  END intCall;

PROCEDURE relCall (t: T; rel: INTEGER) =
  BEGIN
    Mn(t, "CALL rel32");
    writecode(t, FALSE, 16_E8, 0, FALSE, 0, FALSE, rel, 4, 0, 0);
  END relCall;

TYPE AbsCall = REF RECORD
  sym: INTEGER;
  loc: ByteOffset;
  link: AbsCall;
END;

PROCEDURE absCall (t: T; p: x86Proc) =
  BEGIN
    Mn(t, "CALL rel32");
    writecode(t, FALSE, 16_FF, 16_15, TRUE, 0, FALSE, 0, 4, 0, 0);
    t.abscall_list := NEW(AbsCall, loc := t.obj.cursor(Seg.Text) - 4,
                          sym := p.symbol, link := t.abscall_list);
  END absCall;

PROCEDURE rmCall (t: T; READONLY op: Operand) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    <* ASSERT op.loc = OLoc.register OR op.loc = OLoc.mem *>
    Mn(t, "CALL r/m32");
    build_modrm(t, op, t.opcode[2], modrm, disp, dsize);
    writecode(t, FALSE, 16_FF, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);

    IF op.loc = OLoc.mem THEN
      log_global_var(t, op.mvar, -4);
    END
  END rmCall;

PROCEDURE cleanretOp (t: T; psize: INTEGER) =
  BEGIN
    <* ASSERT psize < 16_8000 *>
    writecode(t, FALSE, 16_C2, 0, FALSE, 0, FALSE, 0, 0, psize, 2);
  END cleanretOp;

PROCEDURE brOp (t: T; br: Cond; l: Label) =
  VAR rel: ByteOffset := 0;
  BEGIN
    check_label(t, l, "brOp");
    WITH lab = t.labarr[l], curs = t.obj.cursor(Seg.Text) DO
      IF lab.no_address THEN
        rel := 0;
      ELSE
        rel := lab.offset - (curs + 2);
      END;

      IF rel > 16_7F OR rel < -16_80 OR lab.no_address AND NOT lab.short THEN
        IF lab.no_address THEN
          rel := 0;
        ELSE
          rel := lab.offset - (curs + 5);
        END;

        Mn(t, bropcode[br].name, " rel32");

        IF br # Cond.Always THEN
          DEC(rel);
          writecode(t, FALSE, 16_0F, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
          writecode(t, FALSE, bropcode[br].rel8 + 16_10, 0, FALSE, 0, FALSE,
                    rel, 4, 0, 0);
        ELSE
          writecode(t, FALSE, 16_E9, 0, FALSE, 0, FALSE, rel, 4,
                    0, 0);
        END
      ELSE
        Mn(t, bropcode[br].name, " rel8");
        IF br # Cond.Always THEN
          writecode(t, FALSE, bropcode[br].rel8, 0, FALSE, 0, FALSE, rel, 1,
                    0, 0);
        ELSE
          writecode(t, FALSE, 16_EB, 0, FALSE, 0, FALSE, rel, 1,
                    0, 0);
        END
      END;

      IF lab.no_address THEN
        IF lab.short THEN
          log_unknown_label(t, l, t.obj.cursor(Seg.Text) - 1, FALSE);
        ELSE
          log_unknown_label(t, l, t.obj.cursor(Seg.Text) - 4, FALSE);
        END
      END
    END
  END brOp;

PROCEDURE setccOp (t: T; READONLY op: Operand; cond: Cond) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    <* ASSERT (op.loc = OLoc.register AND
               op.reg IN RegSet { EAX, EBX, ECX, EDX } ) OR
              (op.loc = OLoc.mem AND CG_Bytes[op.mvar.t] = 1) *>
    IF op.loc = OLoc.register THEN
      movImm(t, op, 0);
    END;
    build_modrm(t, op, t.opcode[0], modrm, disp, dsize);
    Mn(t, "SETCC ");
    writecode(t, FALSE, 16_0F, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
    writecode(t, FALSE, condopcode[cond].opc, modrm, TRUE, 0, FALSE, disp,
              dsize, 0, 0);
    IF op.loc = OLoc.mem THEN
      log_global_var(t, op.mvar, -4);
    END
  END setccOp;

PROCEDURE prepare_stack (t: T; op: FOp; forcenomem := FALSE) =
  BEGIN
    WITH opc = fopcode[op] DO
      IF (NOT opc.takesmem) OR forcenomem THEN
        IF (opc.stackin > 0 OR opc.stackdiff # 0) AND t.ftop_inmem THEN
          fstack_loadtop(t);
        END;
        IF opc.stackdiff > 0 THEN
          fstack_ensure(t, opc.stackdiff);
        END;
        fstack_check(t, opc.stackin, "prepare_stack");
      ELSE
        IF t.ftop_inmem THEN
          IF opc.memdiff > 0 THEN
            fstack_ensure(t, opc.memdiff);
          END;
          fstack_check(t, opc.min, "prepare_stack");
        ELSE
          IF opc.stackdiff > 0 THEN
            fstack_ensure(t, opc.stackdiff);
          END;
          fstack_check(t, opc.stackin, "prepare_stack");
        END
      END
    END
  END prepare_stack;

PROCEDURE noargFOp (t: T; op: FOp) =
  BEGIN
    prepare_stack(t, op);
    Mn(t, fopcode[op].name);
    writecode(t, FALSE, fopcode[op].stbase, fopcode[op].stmodrm, TRUE,
              0, FALSE, 0, 0, 0, 0);
    INC(t.fstacksize, fopcode[op].stackdiff);
    INC(t.fstackloaded, fopcode[op].stackdiff);
  END noargFOp;

PROCEDURE immFOp (t: T; op: FOp; im: FIm) =
  BEGIN
    prepare_stack(t, op, TRUE);
    Mn(t, imcode[im].name);
    writecode(t, FALSE, imcode[im].opcode, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
    Mn(t, fopcode[op].name, " ST1");
    writecode(t, FALSE, fopcode[op].stbase, fopcode[op].stmodrm+1, TRUE,
              0, FALSE, 0, 0, 0, 0);
    INC(t.fstacksize, fopcode[op].stackdiff);
    INC(t.fstackloaded, fopcode[op].stackdiff);
  END immFOp;

PROCEDURE binFOp (t: T; op: FOp; st: INTEGER) =
  VAR modrm, disp, dsize, opc: INTEGER;
  BEGIN
    <* ASSERT st < 8 *>
    prepare_stack(t, op);
    IF t.ftop_inmem THEN
      Mn(t, fopcode[op].name, " ST, ");
      IF t.ftop_mem.t = Type.Reel THEN
        IF t.debug THEN
          t.wr.OutT("m32real");
        END;
        opc := fopcode[op].m32;
      ELSE
        IF t.debug THEN
          t.wr.OutT("m64real");
        END;
        opc := fopcode[op].m64;
      END;
      build_modrm(t, Operand {loc := OLoc.mem, mvar := t.ftop_mem},
                  t.opcode[fopcode[op].memop], modrm, disp, dsize);
      writecode(t, FALSE, opc, modrm, TRUE, 0, FALSE, disp, dsize,
                0, 0);
      log_global_var(t, t.ftop_mem, -4);
      INC(t.fstacksize, fopcode[op].stackdiff);
      INC(t.fstackloaded, fopcode[op].memdiff);
      t.ftop_inmem := FALSE;
      RETURN;
    END;
    IF t.debug THEN
      Mn(t, fopcode[op].name, "P ST, ST");
      t.wr.Int(st);
    END;

    writecode(t, FALSE, fopcode[op].stbase, fopcode[op].stmodrm+st, TRUE,
              0, FALSE, 0, 0, 0, 0);
    INC(t.fstacksize, fopcode[op].stackdiff);
    INC(t.fstackloaded, fopcode[op].stackdiff);
  END binFOp;

PROCEDURE memFOp (t: T; op: FOp; mvar: MVar) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    prepare_stack(t, op);

    Mn(t, fopcode[op].name, " m");
    build_modrm(t, Operand {loc := OLoc.mem, mvar := mvar},
                t.opcode[fopcode[op].memop], modrm, disp, dsize);
    writecode(t, FALSE, fopcode[op].m32, modrm, TRUE, 0, FALSE, disp, dsize,
              0, 0);
    log_global_var(t, mvar, -4);
    INC(t.fstacksize, fopcode[op].memdiff);
    INC(t.fstackloaded, fopcode[op].memdiff);
  END memFOp;

PROCEDURE noargOp (t: T; op: Op) =
  BEGIN
    Mn(t, opcode[op].name);
    writecode(t, FALSE, opcode[op].imm32, 0, FALSE, 0, FALSE, 0, 0,
              0, 0);
  END noargOp;

PROCEDURE immOp (t: T; op: Op; READONLY dest: Operand; imm: INTEGER) =
  VAR modrm, disp, dsize: INTEGER;
      imsize := 4;
  BEGIN
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>
    IF imm < 16_80 AND imm > -16_81 THEN
      imsize := 1;
    END;

    IF dest.loc = OLoc.register AND dest.reg = EAX
       AND imsize = 4 THEN
      Mn(t, opcode[op].name, " EAX,Aimm32");
      writecode(t, FALSE, opcode[op].Aimm32, 0, FALSE, 0, FALSE, 0, 0,
                imm, imsize);
    ELSE
      build_modrm(t, dest, t.opcode[opcode[op].immop],
                  modrm, disp, dsize);
      IF imsize = 1 THEN
        IF dest.loc = OLoc.mem AND CG_Bytes[dest.mvar.t] = 1 THEN
          Mn(t, opcode[op].name, " r/m8, imm8");
          writecode(t, FALSE, opcode[op].imm32 - 1, modrm, TRUE, 0, FALSE,
                    disp, dsize, imm, 1);
          log_global_var(t, dest.mvar, -5);
        ELSIF dest.loc = OLoc.mem AND CG_Bytes[dest.mvar.t] = 2 THEN
          Mn(t, opcode[op].name, " r/m16, imm8");
          writecode(t, TRUE, opcode[op].imm8, modrm, TRUE, 0, FALSE,
                    disp, dsize, imm, 1);
          log_global_var(t, dest.mvar, -5);
        ELSE
          Mn(t, opcode[op].name, " r/m32,imm8");
          writecode(t, FALSE, opcode[op].imm8, modrm, TRUE, 0, FALSE,
                    disp, dsize, imm, 1);
          IF dest.loc = OLoc.mem THEN
            log_global_var(t, dest.mvar, -5);
          END
        END
      ELSE
        <* ASSERT dest.loc # OLoc.mem OR CG_Bytes[dest.mvar.t] = 4 *>
        Mn(t, opcode[op].name, " r/m32,imm32");
        writecode(t, FALSE, opcode[op].imm32, modrm, TRUE, 0, FALSE,
                  disp, dsize, imm, 4);
        IF dest.loc = OLoc.mem THEN
          log_global_var(t, dest.mvar, -8);
        END
      END
    END
  END immOp;

PROCEDURE binOp (t: T; op: Op; READONLY dest, src: Operand) =
  VAR modrm, disp, dsize, opc: INTEGER;
      mnemonic: TEXT := NIL;
  BEGIN
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>
    IF src.loc = OLoc.imm THEN
      immOp(t, op, dest, src.imm);

      RETURN;
    END;

    IF dest.loc = OLoc.register THEN
      build_modrm(t, src, dest, modrm, disp, dsize);
      opc := opcode[op].rrm + 1;
      IF src.loc = OLoc.mem THEN
        <* ASSERT CG_Bytes[src.mvar.t] = 4 *>
        mnemonic := "rm32";
      ELSE
        mnemonic := "rr32";
      END
    ELSE
      <* ASSERT src.loc = OLoc.register AND CG_Bytes[src.mvar.t] = 4 *>
      build_modrm(t, dest, src, modrm, disp, dsize);
      opc := opcode[op].rmr + 1;
      mnemonic := "mr32";
    END;
    Mn(t, opcode[op].name, mnemonic);
    varloc(t, dest);
    varloc(t, src);
    writecode(t, FALSE, opc, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    ELSIF src.loc = OLoc.mem THEN      
      log_global_var(t, src.mvar, -4);
    END;
  END binOp;

PROCEDURE tableOp (t: T; op: Op; READONLY dest, index: Operand;
                   scale: INTEGER; table: MVar) =
  VAR offset, modrm, sib, disp, dsize: INTEGER;
      fully_known := FALSE;
  BEGIN
    <* ASSERT dest.loc = OLoc.register AND index.loc = OLoc.register *>

    IF table.var = t.internalvar THEN
      offset := Word.Shift(table.o, -16);
      table.o := Word.And(table.o, 16_FFFF);
    ELSE
      offset := table.o;
    END;

    IF table.var.loc = VLoc.temp THEN
      <* ASSERT table.var.parent = t.current_proc *>
      INC(offset, table.var.offset);
      fully_known := TRUE;
    END;
    IF (NOT fully_known) OR
       (offset > 16_7f) OR (offset < -16_80) THEN
      disp := offset;
      dsize := 4;
      IF NOT fully_known THEN
        modrm := dest.reg*8 + 4;
      ELSE
        modrm := 16_80 + dest.reg*8 + 4;
      END;
    ELSE
      disp := offset;
      dsize := 1;
      modrm := 16_40 + dest.reg*8 + 4;
    END;

    CASE scale OF
      1 => sib := 0;
    | 2 => sib := 16_40;
    | 4 => sib := 16_80;
    | 8 => sib := 16_C0;
    ELSE
      t.Err("tableOp called with invalid scale parameter");
    END;

    INC(sib, index.reg*8);
    INC(sib, 5);

    Mn(t, opcode[op].name, " r32, table[r32*scale]");
    writecode(t, FALSE, opcode[op].rrm+1, modrm, TRUE, sib, TRUE, disp, dsize,
              0, 0);
    log_global_var(t, table, -4);
  END tableOp;

PROCEDURE swapOp (t: T; READONLY dest, src: Operand) =
  VAR modrm, disp, dsize: INTEGER;
      mnemonic: TEXT := NIL;
      otherreg: Regno;
  BEGIN
    <* ASSERT (dest.loc = OLoc.register OR dest.loc = OLoc.mem) AND
              (src.loc = OLoc.register OR src.loc = OLoc.mem) *>
    IF dest.loc = OLoc.register AND src.loc = OLoc.register AND
       (dest.reg = EAX OR src.reg = EAX) THEN
      IF dest.reg = EAX THEN
        otherreg := src.reg;
      ELSE
        otherreg := dest.reg;
      END;
      Mn(t, "XCHG ");
      varloc(t, dest);
      varloc(t, src);
      writecode(t, FALSE, 16_90 + otherreg, 0, FALSE,
                0, FALSE, 0, 0, 0, 0);
      RETURN;
    END;

    IF dest.loc = OLoc.register THEN
      <* ASSERT CG_Bytes[src.mvar.t] = 4 *>
      build_modrm(t, src, dest, modrm, disp, dsize);
      IF src.loc # OLoc.register THEN
        mnemonic := "rm32";
      ELSE
        mnemonic := "rr";
      END
    ELSE
      <* ASSERT src.loc = OLoc.register *>
      <* ASSERT CG_Bytes[dest.mvar.t] = 4 *>
      build_modrm(t, dest, src, modrm, disp, dsize);
      mnemonic := "mr32";
    END;
    Mn(t, "XCHG ", mnemonic);
    varloc(t, dest);
    varloc(t, src);
    writecode(t, FALSE, 16_87, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    ELSIF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4);
    END;
  END swapOp;

PROCEDURE MOVSWOp (t: T) =
  BEGIN
    Mn(t, "MOVSW");
    writecode(t, TRUE, 16_A5, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
  END MOVSWOp;

PROCEDURE STOSWOp (t: T) =
  BEGIN
    Mn(t, "STOSW");
    writecode(t, TRUE, 16_AB, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
  END STOSWOp;

PROCEDURE CBWOp (t: T) =
  BEGIN
    Mn(t, "CBW");
    writecode(t, TRUE, 16_98, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
  END CBWOp;

PROCEDURE movOp (t: T; READONLY dest, src: Operand) =
  VAR modrm, disp, dsize, opcode: INTEGER;
      mnemonic: TEXT := NIL;
      prefix := FALSE;
  BEGIN
    <* ASSERT dest.loc = OLoc.register OR dest.loc = OLoc.mem *>
    IF src.loc = OLoc.imm THEN
      movImm(t, dest, src.imm);
      RETURN;
    END;

    IF dest.loc = OLoc.register AND dest.reg = EAX AND
       src.loc = OLoc.mem AND CG_Bytes[src.mvar.t] = 4 AND
       src.mvar.var.loc = VLoc.global THEN
      opcode := 16_A1;
      mnemonic := "MOV EAX,moffs32";
      Mn(t, mnemonic);
      writecode(t, prefix, opcode, 0, FALSE, 0, FALSE, src.mvar.o, 4, 0, 0);
      log_global_var(t, src.mvar, -4);
      RETURN;
    END;

    IF src.loc = OLoc.register AND src.reg = EAX AND
       dest.loc = OLoc.mem AND dest.mvar.var.loc = VLoc.global THEN
      opcode := 16_A2;
      mnemonic := "MOV moffs";
      get_op_size(t, dest.mvar.t, opcode, prefix);
      Mn(t, mnemonic, ",EAX");
      writecode(t, prefix, opcode, 0, FALSE, 0, FALSE, dest.mvar.o, 4, 0, 0);
      log_global_var(t, dest.mvar, -4);
      RETURN;
    END;

    IF dest.loc = OLoc.register AND src.loc = OLoc.mem AND
       CG_Bytes[src.mvar.t] # 4 THEN
      CASE src.mvar.t OF
        Type.Word_A => opcode := 16_8A;
                       mnemonic := "MOV r32, m8";
                       binOp(t, Op.oXOR, t.reg[dest.reg], t.reg[dest.reg]);
      | Type.Word_B => opcode := 16_8B;
                       prefix := TRUE;
                       mnemonic := "MOV r32, m16";
                       binOp(t, Op.oXOR, t.reg[dest.reg], t.reg[dest.reg]);
      | Type.Int_A  => opcode := 16_BE;
                       mnemonic := "MOVSX r32, m8";
                       writecode(t, FALSE, 16_0F, 0, FALSE, 0, FALSE,
                                 0, 0, 0, 0);
      | Type.Int_B  => opcode := 16_BF;
                       mnemonic := "MOVSX r32, m16";
                       writecode(t, FALSE, 16_0F, 0, FALSE, 0, FALSE,
                                 0, 0, 0, 0);
      ELSE
        t.Err("Unknown type of size other than dword in movOp");
      END;
      build_modrm(t, src, dest, modrm, disp, dsize);
      Mn(t, mnemonic);
      writecode(t, prefix, opcode, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
      log_global_var(t, src.mvar, -4);
      RETURN;
    END;

    IF dest.loc = OLoc.register THEN
      build_modrm(t, src, dest, modrm, disp, dsize);
      opcode := 16_8A;
      IF src.loc # OLoc.register THEN
        mnemonic := "rm";
        get_op_size(t, src.mvar.t, opcode, prefix);
      ELSE
        mnemonic := "rr";
        INC(opcode);
      END
    ELSE
      <* ASSERT src.loc = OLoc.register *>
      build_modrm(t, dest, src, modrm, disp, dsize);
      opcode := 16_88;
      mnemonic := "mr";
      get_op_size(t, dest.mvar.t, opcode, prefix);
    END;
    Mn(t, "MOV ", mnemonic);
    varloc(t, dest);
    varloc(t, src);
    writecode(t, prefix, opcode, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    ELSIF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4);
    END;
  END movOp;

PROCEDURE movDummyReloc(t: T; READONLY dest: Operand; sym: INTEGER) =
  BEGIN
    <* ASSERT dest.loc = OLoc.register *>
    Mn(t, "MOV reg32, imm32");
    writecode(t, FALSE, 16_B8 + dest.reg, 0, FALSE, 0, FALSE, 0, 0, 0, 4);
    t.obj.relocate(t.textsym, t.obj.cursor(Seg.Text) - 4, sym);
  END movDummyReloc;

PROCEDURE movImm (t: T; READONLY dest: Operand; imm: INTEGER) =
  VAR modrm, disp, dsize: INTEGER;
      prefix := FALSE;
      mnemonic := "m";
      opcode := 16_C6;
  BEGIN
    IF dest.loc = OLoc.register THEN
      IF imm = 0 THEN
        binOp(t, Op.oXOR, dest, dest);
      ELSE
        Mn(t, "MOV reg32, imm32");
        writecode(t, FALSE, 16_B8 + dest.reg, 0, FALSE, 0, FALSE,
                  0, 0, imm, 4);
      END
    ELSE
      <* ASSERT dest.loc = OLoc.mem *>
      get_op_size(t, dest.mvar.t, opcode, prefix);
      build_modrm(t, dest, t.opcode[0], modrm, disp, dsize);
      Mn(t, "MOV ", mnemonic, " imm");
      writecode(t, prefix, opcode, modrm, TRUE, 0, FALSE, disp, dsize,
                imm, CG_Bytes[dest.mvar.t]);
      log_global_var(t, dest.mvar, -4 - CG_Bytes[dest.mvar.t]);
    END
  END movImm;

PROCEDURE pushOp (t: T; READONLY src: Operand) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    CASE src.loc OF
      OLoc.imm =>
        Mn(t, "PUSH imm32");
        writecode(t, FALSE, 16_68, 0, FALSE, 0, FALSE, 0, 0, src.imm, 4);
    | OLoc.register =>
        Mn(t, "PUSH r32");
        writecode(t, FALSE, 16_50 + src.reg, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
    | OLoc.mem =>
        <* ASSERT CG_Bytes[src.mvar.t] = 4 *>
        Mn(t, "PUSH m32");
        build_modrm(t, src, t.opcode[6], modrm, disp, dsize);
        writecode(t, FALSE, 16_FF, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
        log_global_var(t, src.mvar, -4);
    ELSE
      t.Err("Tried to push an fstack element to the integer stack");
    END
  END pushOp;

PROCEDURE popOp (t: T; READONLY dest: Operand) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    CASE dest.loc OF
      OLoc.imm =>
        t.Err("Tried to pop into an immediate stack element");
    | OLoc.register =>
        Mn(t, "POP r32");
        writecode(t, FALSE, 16_58 + dest.reg, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
    | OLoc.mem =>
        <* ASSERT CG_Bytes[dest.mvar.t] = 4 *>
        Mn(t, "POP m32");
        build_modrm(t, dest, t.opcode[6], modrm, disp, dsize);
        writecode(t, FALSE, 16_FF, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
        log_global_var(t, dest.mvar, -4);
    ELSE
      t.Err("Tried to pop an fstack element from the integer stack");
    END
  END popOp;

PROCEDURE decOp (t: T; READONLY op: Operand) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    <* ASSERT op.loc = OLoc.mem OR op.loc = OLoc.register *>
    IF op.loc = OLoc.register THEN
      Mn(t, "DEC r32");
      writecode(t, FALSE, 16_48 + op.reg, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
    ELSE
      <* ASSERT op.loc = OLoc.mem AND CG_Bytes[op.mvar.t] = 4 *>
      Mn(t, "DEC m32");
      build_modrm(t, op, t.opcode[1], modrm, disp, dsize);
      writecode(t, FALSE, 16_FF, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
      log_global_var(t, op.mvar, -4);
    END
  END decOp;

PROCEDURE unOp (t: T; op: Op; READONLY dest: Operand) =
  VAR modrm, disp, dsize: INTEGER;
      prefix := FALSE;
      opc := opcode[op].imm32;
      mnemonic := "rm";
  BEGIN
    IF dest.loc = OLoc.mem THEN
      get_op_size(t, dest.mvar.t, opc, prefix);
    ELSE
      <* ASSERT dest.loc = OLoc.register *>
      INC(opc);
      mnemonic := "rm32";
    END;

    build_modrm(t, dest, t.opcode[opcode[op].immop], modrm, disp, dsize);

    Mn(t, opcode[op].name, mnemonic);
    writecode(t, prefix, opc, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);

    IF dest.loc = OLoc.mem THEN
      log_global_var(t, dest.mvar, -4);
    END
  END unOp;

PROCEDURE mulOp (t: T; READONLY src: Operand) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    <* ASSERT src.loc = OLoc.register OR (src.loc = OLoc.mem AND
              CG_Bytes[src.mvar.t] = 4) *>
    build_modrm(t, src, t.opcode[4], modrm, disp, dsize);
    Mn(t, "MUL r32, rm32");
    writecode(t, FALSE, 16_F7, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
    IF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4);
    END
  END mulOp;

PROCEDURE imulOp (t: T; READONLY dest, src: Operand) =
  VAR modrm, disp, dsize: INTEGER;
      imsize := 0;
  BEGIN
    <* ASSERT dest.loc = OLoc.register *>
    <* ASSERT src.loc # OLoc.mem OR CG_Bytes[src.mvar.t] = 4 *>
    IF src.loc = OLoc.imm THEN
      build_modrm(t, t.reg[dest.reg], dest, modrm, disp, dsize);
      Mn(t, "IMUL r32, imm32");
      imsize := 4;
      writecode(t, FALSE, 16_69, modrm, TRUE, 0, FALSE, disp, dsize,
                src.imm, imsize);
    ELSE
      build_modrm(t, src, dest, modrm, disp, dsize);
      Mn(t, "IMUL r32, rm32");
      writecode(t, FALSE, 16_0F, 0, FALSE, 0, FALSE, 0, 0, 0, 0);
      writecode(t, FALSE, 16_AF, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
    END;

    IF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4 - imsize);
    END
  END imulOp;

PROCEDURE imulImm (t: T; READONLY dest, src: Operand; imm, imsize: INTEGER) =
  VAR modrm, disp, dsize, opc: INTEGER;
  BEGIN
    <* ASSERT dest.loc = OLoc.register *>
    <* ASSERT src.loc # OLoc.mem OR CG_Bytes[src.mvar.t] = 4 *>
    build_modrm(t, src, dest, modrm, disp, dsize);
    Mn(t, "IMUL r32, rm32, imm");
    IF imsize = 1 THEN
      opc := 16_6B;
    ELSE
      opc := 16_69;
    END;
    writecode(t, FALSE, opc, modrm, TRUE, 0, FALSE, disp, dsize, imm,
              imsize);
    IF src.loc = OLoc.mem THEN
      log_global_var(t, src.mvar, -4 - imsize);
    END
  END imulImm;

PROCEDURE divOp (t: T; READONLY divisor: Operand) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    <* ASSERT divisor.loc = OLoc.register OR (divisor.loc = OLoc.mem
              AND CG_Bytes[divisor.mvar.t] = 4) *>
    build_modrm(t, divisor, t.opcode[6], modrm, disp, dsize);
    Mn(t, "DIV EAX, rm32");
    writecode(t, FALSE, 16_F7, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
    IF divisor.loc = OLoc.mem THEN
      log_global_var(t, divisor.mvar, -4);
    END
  END divOp;

PROCEDURE idivOp (t: T; READONLY divisor: Operand) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    <* ASSERT divisor.loc = OLoc.register OR (divisor.loc = OLoc.mem
              AND CG_Bytes[divisor.mvar.t] = 4) *>
    build_modrm(t, divisor, t.opcode[7], modrm, disp, dsize);
    Mn(t, "IDIV EAX, rm32");
    writecode(t, FALSE, 16_F7, modrm, TRUE, 0, FALSE, disp, dsize, 0, 0);
    IF divisor.loc = OLoc.mem THEN
      log_global_var(t, divisor.mvar, -4);
    END
  END idivOp;

PROCEDURE diffdivOp (t: T; READONLY divisor: Operand; apos: BOOLEAN) =
  VAR diffsignlab, endlab: Label;
  BEGIN
    <* ASSERT divisor.loc = OLoc.register *>
    diffsignlab := reserve_labels(t, 1, TRUE);
    endlab := reserve_labels(t, 1, TRUE);

    movOp(t, t.reg[EDX], t.reg[EAX]);
    (*                                                     MOV EDX, EAX      *)
    binOp(t, Op.oXOR, t.reg[EDX], divisor);
    (*                                                     XOR EDX, divisor  *)
    brOp(t, Cond.L, diffsignlab);
    (*                                                     JL  diffsignlab   *)
    IF apos THEN
      binOp(t, Op.oXOR, t.reg[EDX], t.reg[EDX]);
    ELSE (*                                                XOR EDX, EDX      *)
      noargOp(t, Op.oCDQ);
    END;
    (*                                                     CDQ               *)
    idivOp(t, divisor);
    (*                                                     IDIV EAX, divisor *)
    brOp(t, Cond.Always, endlab);
    (*                                                     JMP endlab        *)
    set_label(t, diffsignlab);
    (*                                                 .diffsignlab          *)
    noargOp(t, Op.oCDQ);
    (*                                                     CDQ               *)
    idivOp(t, divisor);
    (*                                                     IDIV EAX, divisor *)
    immOp(t, Op.oCMP, t.reg[EDX], 0);
    (*                                                     CMP EDX, #0       *)
    brOp(t, Cond.E, endlab);
    (*                                                     JE  endlab        *)
    decOp(t, t.reg[EAX]);
    (*                                                     DEC EAX           *)
    set_label(t, endlab);
    (*                                                 .endlab               *)
  END diffdivOp;

PROCEDURE diffmodOp (t: T; READONLY divisor: Operand; apos: BOOLEAN) =
  VAR diffsignlab, endlab: Label;
  BEGIN
    <* ASSERT divisor.loc = OLoc.register *>
    diffsignlab := reserve_labels(t, 1, TRUE);
    endlab := reserve_labels(t, 1, TRUE);

    movOp(t, t.reg[EDX], t.reg[EAX]);
    (*                                                     MOV EDX, EAX      *)
    binOp(t, Op.oXOR, t.reg[EDX], divisor);
    (*                                                     XOR EDX, divisor  *)
    brOp(t, Cond.L, diffsignlab);
    (*                                                     JL  diffsignlab   *)
    IF apos THEN
      binOp(t, Op.oXOR, t.reg[EDX], t.reg[EDX]);
    ELSE (*                                                XOR EDX, EDX      *)
      noargOp(t, Op.oCDQ);
    END;
    (*                                                     CDQ               *)
    idivOp(t, divisor);
    (*                                                     IDIV EAX, divisor *)
    brOp(t, Cond.Always, endlab);
    (*                                                     JMP endlab        *)
    set_label(t, diffsignlab);
    (*                                                 .diffsignlab          *)
    noargOp(t, Op.oCDQ);
    (*                                                     CDQ               *)
    idivOp(t, divisor);
    (*                                                     IDIV EAX, divisor *)
    immOp(t, Op.oCMP, t.reg[EDX], 0);
    (*                                                     CMP EDX, #0       *)
    brOp(t, Cond.E, endlab);
    (*                                                     JE  endlab        *)
    binOp(t, Op.oADD, t.reg[EDX], divisor);
    (*                                                     ADD EDX, divisor  *)
    set_label(t, endlab);
    (*                                                 .endlab               *)
  END diffmodOp;

PROCEDURE must_extend (<*UNUSED*> t: T; READONLY src: Operand):
          BOOLEAN =
  BEGIN
    IF src.loc # OLoc.mem THEN
      RETURN FALSE;
    END;
    IF src.mvar.t = Type.Word_A OR src.mvar.t = Type.Word_B OR
       src.mvar.t = Type.Int_A OR src.mvar.t = Type.Int_B THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END
  END must_extend;

PROCEDURE get_addsize (<*UNUSED*> t: T; READONLY op: Operand): INTEGER =
  BEGIN
    IF op.loc # OLoc.mem THEN
      RETURN 0;
    END;

    IF op.mvar.var.loc = VLoc.global THEN
      RETURN 4;
    END;

    WITH offset = op.mvar.o + op.mvar.var.offset DO
      IF offset > 16_7F OR offset < -16_80 THEN
        RETURN 4;
      ELSE
        RETURN 1;
      END
    END
  END get_addsize;

PROCEDURE get_op_size (<*UNUSED*> t: T; type: MType;
                       VAR opcode: INTEGER;
                       VAR prefix: BOOLEAN) =
  BEGIN
    <* ASSERT opcode # -1 *>
    CASE type OF
      Type.Int_A, Type.Word_A =>
        prefix := FALSE;
    | Type.Int_B, Type.Word_B =>
        INC(opcode);
        prefix := TRUE;
    ELSE
        INC(opcode);
        prefix := FALSE;
    END
  END get_op_size;

PROCEDURE build_modrm (t: T; READONLY mem, reg: Operand;
                      VAR modrm, disp, dsize: INTEGER) =
  VAR offset: ByteOffset;
      fully_known := FALSE;
  BEGIN
    <* ASSERT reg.loc = OLoc.register *>
    IF mem.loc = OLoc.register THEN
      disp := 0;
      dsize := 0;
      modrm := 16_C0 + reg.reg*8 + mem.reg;
      RETURN;
    END;

    <* ASSERT mem.loc = OLoc.mem *>

    <* ASSERT CG_Bytes[mem.mvar.t] # 1 OR reg.opcode OR
              reg.reg IN RegSet { EAX, EBX, ECX, EDX } *>

    offset := mem.mvar.o;
    IF mem.mvar.var.loc = VLoc.temp THEN
      <* ASSERT mem.mvar.var.parent = t.current_proc *>
      INC(offset, mem.mvar.var.offset);
      fully_known := TRUE;
    END;
    IF (NOT fully_known) OR
       (offset > 16_7f) OR (offset < -16_80) THEN
      disp := offset;
      dsize := 4;
      IF NOT fully_known THEN
        modrm := reg.reg*8 + 5;
      ELSE
        modrm := 16_80 + reg.reg*8 + EBP;
      END;
    ELSE
      disp := offset;
      dsize := 1;
      modrm := 16_40 + reg.reg*8 + EBP;
    END;
  END build_modrm;

PROCEDURE varloc(t: T; READONLY op: Operand) =
  BEGIN
    IF t.debug THEN
      CASE op.loc OF
        OLoc.fstack => t.wr.OutT(" FST");
      | OLoc.mem => t.wr.VName(op.mvar.var);
      | OLoc.register => t.wr.OutT(" r"); t.wr.OutI(op.reg);
      | OLoc.imm => t.wr.OutT(" i"); t.wr.OutI(op.imm);
      END
    END
  END varloc;

PROCEDURE writecode (t: T; prefix: BOOLEAN; opcode, modrm: INTEGER;
                     mrmpres: BOOLEAN; sib: INTEGER; sibpres: BOOLEAN;
                     disp, dsize, imm, imsize: INTEGER) =
  BEGIN
    IF t.debug THEN
      t.wr.OutT(" ");

      IF prefix THEN
        Byte(t, 16_66);
      END;

      Byte(t, opcode);

      IF mrmpres THEN
        Byte(t, modrm);
      END;

      IF sibpres THEN
        Byte(t, sib);
      END;

      IF dsize # 0 THEN
        Hexbe(t, disp, dsize);
        t.wr.OutT(" ");
      END;

      IF imsize # 0 THEN
        Hexbe(t, imm, imsize);
        t.wr.OutT(" ");
      END;

      t.wr.NL();
    END;

    <* ASSERT dsize = 0 OR dsize = 1 OR dsize = 4 *>
    IF prefix THEN
      t.obj.append(Seg.Text, 16_66, 1);
    END;

    <* ASSERT opcode >= 0 AND opcode <= 255 *>
    t.obj.append(Seg.Text, opcode, 1);

    IF mrmpres THEN
      t.obj.append(Seg.Text, modrm, 1);
    END;

    IF sibpres THEN
      t.obj.append(Seg.Text, sib, 1);
    END;

    IF dsize # 0 THEN
      t.obj.append(Seg.Text, disp, dsize);
    END;

    IF imsize # 0 THEN
      t.obj.append(Seg.Text, imm, imsize);
    END;
  END writecode;

(*--------------------------------------------------------- jump routines ---*)

PROCEDURE case_jump (t: T; index: Operand; READONLY labels: ARRAY OF Label) =
  BEGIN
    <* ASSERT index.loc = OLoc.register *>
    WITH curs = t.obj.cursor(Seg.Text) DO
      writecode(t, FALSE, 16_FF, 16_24, TRUE, 16_85 + index.reg * 8, TRUE,
                curs + 7, 4, 0, 0); (* Jump to abs address indexed by
                                       register 'index'*4 *)
      t.obj.relocate(t.textsym, curs + 3, t.textsym);
      FOR i := 0 TO NUMBER(labels) - 1 DO
        check_label(t, labels[i], "case_jump");
        WITH lab = t.labarr[labels[i]] DO
          IF lab.no_address THEN
            t.obj.append(Seg.Text, 0, 4);
            log_unknown_label(t, labels[i], curs + 7 + i * 4, TRUE);
          ELSE
            t.obj.append(Seg.Text, lab.offset, 4);
            t.obj.relocate(t.textsym, curs + 7 + i * 4, t.textsym);
          END
        END
      END
    END
  END case_jump;

PROCEDURE load_ind (t: T; r: Regno; READONLY ind: Operand; o: ByteOffset;
                    type: MType) =
  VAR opcode := 16_8B;
      mnemonic := "MOV r32, m32";
      modrm, dsize: INTEGER;
      prefix := FALSE;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    IF CG_Bytes[type] # 4 THEN
      CASE type OF
        Type.Word_A => opcode := 16_8A;
                       mnemonic := "MOV r32, m8";
                       binOp(t, Op.oXOR, t.reg[r], t.reg[r]);
      | Type.Word_B => opcode := 16_8B;
                       prefix := TRUE;
                       mnemonic := "MOV r32, m16";
                       binOp(t, Op.oXOR, t.reg[r], t.reg[r]);
      | Type.Int_A  => opcode := 16_BE;
                       mnemonic := "MOVSX r32, m8";
                       writecode(t, FALSE, 16_0F, 0, FALSE, 0, FALSE,
                                 0, 0, 0, 0);
      | Type.Int_B  => opcode := 16_BF;
                       mnemonic := "MOVSX r32, m16";
                       writecode(t, FALSE, 16_0F, 0, FALSE, 0, FALSE,
                                 0, 0, 0, 0);
      ELSE
        t.Err("Unknown type of size other than dword in load_ind");
      END;
    END;
    Mn(t, mnemonic);
    IF o > -16_81 AND o < 16_80 THEN
      modrm := 16_40 + r * 8 + ind.reg;
      dsize := 1;
    ELSE
      modrm := 16_80 + r * 8 + ind.reg;
      dsize := 4;
    END;

    IF ind.reg = ESP THEN
      writecode(t, prefix, opcode, modrm, TRUE, 16_24, TRUE, o, dsize, 0, 0);
    ELSE
      writecode(t, prefix, opcode, modrm, TRUE, 0, FALSE, o, dsize, 0, 0);
    END
  END load_ind;

PROCEDURE fast_load_ind (t: T; r: Regno; READONLY ind: Operand; o: ByteOffset;
                    size: INTEGER) =
  VAR opcode := 16_8B;
      mnemonic := "MOV r32, m32";
      modrm, dsize: INTEGER;
      prefix := FALSE;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    CASE size OF
        1 => opcode := 16_8A;
             mnemonic := "MOV r32, m8";
      | 2 => opcode := 16_8B;
             prefix := TRUE;
             mnemonic := "MOV r32, m16";
      | 4 => opcode := 16_8B;
             mnemonic := "MOV r32, m32";
    ELSE
      t.Err("Illegal size in fast_load_ind");
    END;

    Mn(t, mnemonic);
    IF o > -16_81 AND o < 16_80 THEN
      modrm := 16_40 + r * 8 + ind.reg;
      dsize := 1;
    ELSE
      modrm := 16_80 + r * 8 + ind.reg;
      dsize := 4;
    END;

    IF ind.reg = ESP THEN
      writecode(t, prefix, opcode, modrm, TRUE, 16_24, TRUE, o, dsize, 0, 0);
    ELSE
      writecode(t, prefix, opcode, modrm, TRUE, 0, FALSE, o, dsize, 0, 0);
    END
  END fast_load_ind;

PROCEDURE store_ind (t: T; READONLY val, ind: Operand; o: ByteOffset;
                     type: MType) =
  VAR opcode := 16_88;
      mnemonic := "mr";
      prefix := FALSE;
      modrm, dsize: INTEGER;
      imm := 0;
      immsize := 0;
  BEGIN
    <* ASSERT ind.loc = OLoc.register AND val.loc # OLoc.mem *>
    IF val.loc = OLoc.imm THEN
      opcode := 16_C6;
      imm := val.imm;
      immsize := CG_Bytes[type];
    END;

    get_op_size(t, type, opcode, prefix);
    Mn(t, "MOV ", mnemonic);

    IF o >= -16_80 AND o <= 16_7F THEN
      dsize := 1;
      modrm := 16_40 + ind.reg;
    ELSE
      dsize := 4;
      modrm := 16_80 + ind.reg;
    END;

    IF val.loc # OLoc.imm THEN
      INC(modrm, val.reg * 8);
    END;

    IF ind.reg = ESP THEN
      writecode(t, prefix, opcode, modrm, TRUE, 16_24, TRUE, o, dsize,
                imm, immsize);
    ELSE
      writecode(t, prefix, opcode, modrm, TRUE, 0, FALSE, o, dsize,
                imm, immsize);
    END
  END store_ind;

PROCEDURE f_loadind (t: T; READONLY ind: Operand; o: ByteOffset; type: MType) =
  VAR opcode, modrm, dsize: INTEGER;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    prepare_stack(t, FOp.fLD, TRUE);
    Mn(t, "FLD ");
    IF type = Type.Reel THEN
      IF t.debug THEN
        t.wr.OutT("m32real");
      END;
      opcode := fopcode[FOp.fLD].m32;
    ELSE
      <* ASSERT type = Type.LReel OR type = Type.XReel *>
      IF t.debug THEN
        t.wr.OutT("m64real");
      END;
      opcode := fopcode[FOp.fLD].m64;
    END;
    IF o >= -16_80 AND o <= 16_7F THEN
      dsize := 1;
      modrm := 16_40 + fopcode[FOp.fLD].memop * 8 + ind.reg;
    ELSE
      dsize := 4;
      modrm := 16_80 + fopcode[FOp.fLD].memop * 8 + ind.reg;
    END;
    IF ind.reg = ESP THEN
      writecode(t, FALSE, opcode, modrm, TRUE, 16_24, TRUE, o, dsize, 0, 0);
    ELSE
      writecode(t, FALSE, opcode, modrm, TRUE, 0, FALSE, o, dsize, 0, 0);
    END;

    INC(t.fstacksize);
    INC(t.fstackloaded);
  END f_loadind;

PROCEDURE f_storeind (t: T; READONLY ind: Operand; o: ByteOffset;
                      type: MType) =
  VAR opcode, modrm, dsize: INTEGER;
  BEGIN
    <* ASSERT ind.loc = OLoc.register *>
    fstack_check(t, 1, "f_storeind");
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;
    Mn(t, "FSTP ");
    IF type = Type.Reel THEN
      IF t.debug THEN
        t.wr.OutT("m32real");
      END;
      opcode := fopcode[FOp.fSTP].m32;
    ELSE
      <* ASSERT type = Type.LReel OR type = Type.XReel *>
      IF t.debug THEN
        t.wr.OutT("m64real");
      END;
      opcode := fopcode[FOp.fSTP].m64;
    END;
    IF o >= -16_80 AND o <= 16_7F THEN
      dsize := 1;
      modrm := 16_40 + fopcode[FOp.fSTP].memop * 8 + ind.reg;
    ELSE
      dsize := 4;
      modrm := 16_80 + fopcode[FOp.fSTP].memop * 8 + ind.reg;
    END;
    IF ind.reg = ESP THEN
      writecode(t, FALSE, opcode, modrm, TRUE, 16_24, TRUE, o, dsize, 0, 0);
    ELSE
      writecode(t, FALSE, opcode, modrm, TRUE, 0, FALSE, o, dsize, 0, 0);
    END;

    DEC(t.fstacksize);
    DEC(t.fstackloaded);
  END f_storeind;

(*----------------------------------------------------------- label stuff ---*)

TYPE
  x86Label = RECORD
    offset: ByteOffset := 0;
    no_address := TRUE;
    usage: LabList := NIL;
    short := FALSE;
  END;

TYPE
  LabList = OBJECT
    seg: Seg;
    offs: INTEGER;
    abs: BOOLEAN;
    link: LabList;
  END;

PROCEDURE reserve_labels (t: T; n: INTEGER; short := FALSE): Label =
  VAR lab := t.next_label_id;
  BEGIN
    IF t.next_label_id+n >= t.lablimit THEN
      expand_labels(t);
    END;
    FOR i := lab TO lab + n - 1 DO
      t.labarr[i].no_address := TRUE;
      t.labarr[i].usage := NIL;
      t.labarr[i].short := short;
    END;
    INC(t.next_label_id, n);
    RETURN lab;
  END reserve_labels;

PROCEDURE expand_labels(t: T) =
  VAR newarr := NEW(REF ARRAY OF x86Label, t.lablimit * 2);
  BEGIN
    FOR i := 0 TO t.lablimit - 1 DO
      newarr[i] := t.labarr[i];
    END;
    t.labarr := newarr;
    t.lablimit := t.lablimit * 2;
  END expand_labels;

PROCEDURE log_unknown_label (t: T; l: Label; loc: ByteOffset; abs: BOOLEAN) =
  BEGIN
    check_label(t, l, "log_unknown_label");
    t.labarr[l].usage := NEW(LabList, seg := Seg.Text,
                             offs := loc, abs := abs,
                             link := t.labarr[l].usage);
  END log_unknown_label;

PROCEDURE log_label_init (t: T; var: x86Var; o: ByteOffset; lab: Label) =
  BEGIN
    check_label(t, lab, "log_label_init");

    t.obj.relocate(var.symbol, o, t.textsym);

    IF t.labarr[lab].no_address THEN
      t.labarr[lab].usage := NEW(LabList, seg := Seg.Data,
                                 offs := t.obj.cursor(Seg.Data), abs := TRUE,
                                 link := t.labarr[lab].usage);
      t.obj.append(Seg.Data, 0, 4);
    ELSE
      t.obj.append(Seg.Data, t.labarr[lab].offset, 4);
    END;
  END log_label_init;

PROCEDURE get_frame (t: T; r: Regno; target, current: x86Proc) =
  BEGIN
    IF current = target THEN
      movOp(t, t.reg[r], t.reg[EBP]);
      RETURN;
    END;

    load_ind(t, r, t.reg[EBP], -4, Type.Addr);

    current := current.parent;

    WHILE current # target DO
      load_ind(t, r, t.reg[r], -4, Type.Addr);
      current := current.parent;
    END
  END get_frame;

PROCEDURE set_label (t: T; l: Label; offset := 0) =
  BEGIN
    check_label(t, l, "set_label");
    WITH lab = t.labarr[l] DO
      IF NOT lab.no_address THEN
        t.Err("Duplicate label definition");
      END;
      lab.offset := t.obj.cursor(Seg.Text) + offset;
      lab.no_address := FALSE;
      IF lab.usage # NIL THEN
        fill_in_label_thread(t, lab.usage, lab.offset, lab.short);
        lab.usage := NIL;
      END
    END
  END set_label;

PROCEDURE check_label(t: T; l: Label; place: TEXT) =
  BEGIN
    IF l >= t.next_label_id THEN
      t.Err("Tried to reference unknown label in " & place);
    END
  END check_label;

PROCEDURE fill_in_label_thread (t: T; ptr: LabList; val: INTEGER;
                                short: BOOLEAN) =
  BEGIN
    WHILE ptr # NIL DO
      IF ptr.abs THEN
        t.obj.relocate(t.textsym, ptr.offs, t.textsym);
        t.obj.patch(ptr.seg, ptr.offs, val, 4);
      ELSE
        <* ASSERT ptr.seg = Seg.Text *>
          
        IF short THEN
          <* ASSERT val - (ptr.offs + 1) <= 16_7F AND
                    val - (ptr.offs + 1) >= -16_80 *>
          t.obj.patch(ptr.seg, ptr.offs, val - (ptr.offs + 1), 1);
        ELSE
          t.obj.patch(ptr.seg, ptr.offs, val - (ptr.offs + 4), 4);
        END
      END;
      ptr := ptr.link;
    END;
  END fill_in_label_thread;

(*-------------------------------------------------- floating stack stuff ---*)

PROCEDURE fstack_loadtop (t: T) =
  VAR modrm, disp, dsize, opc: INTEGER;
  BEGIN
    <* ASSERT t.ftop_inmem *>
    fstack_ensure(t, 0); (* ensure will allow an extra space for the item
                            in memory, so height can be 0 not 1 *)
    Mn(t, "FLD ST, ");
    IF t.ftop_mem.t = Type.Reel THEN
      IF t.debug THEN
        t.wr.OutT("m32real");
      END;
      opc := fopcode[FOp.fLD].m32;
    ELSE
      IF t.debug THEN
        t.wr.OutT("m64real");
      END;
      opc := fopcode[FOp.fLD].m64;
    END;
    build_modrm(t, Operand {loc := OLoc.mem, mvar := t.ftop_mem},
                t.opcode[fopcode[FOp.fLD].memop], modrm, disp, dsize);
    writecode(t, FALSE, opc, modrm, TRUE, 0, FALSE, disp, dsize,
              0, 0);
    log_global_var(t, t.ftop_mem, -4);
    t.ftop_inmem := FALSE;
    INC(t.fstackloaded);
  END fstack_loadtop;

PROCEDURE assert_fstack (t: T; count: INTEGER) =
  BEGIN
    <* ASSERT t.fstacksize = count *>
  END assert_fstack;

PROCEDURE f_ensureloaded (t: T) =
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END
  END f_ensureloaded;

PROCEDURE f_exitproc (t: T) =
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;

    <* ASSERT t.fstacksize = 1 *>
    <* ASSERT t.fstackloaded = 1 *>

    t.fstacksize := 0;
    t.fstackloaded := 0;
  END f_exitproc;

PROCEDURE f_pushnew (t: T) =
  BEGIN
    INC(t.fstacksize);
    INC(t.fstackloaded);
  END f_pushnew;

PROCEDURE fstack_push (t: T; READONLY mvar: MVar; nomem := FALSE) =
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;

    t.ftop_inmem := TRUE;
    t.ftop_mem := mvar;
    INC(t.fstacksize);

    IF nomem THEN
      fstack_loadtop(t);
    END
  END fstack_push;

PROCEDURE fstack_pop (t: T; READONLY mvar: MVar) =
  VAR modrm, disp, dsize, opc: INTEGER;
  BEGIN
    IF t.ftop_inmem THEN
      IF mvar = t.ftop_mem THEN
        t.ftop_inmem := FALSE;
        DEC(t.fstacksize);
        RETURN;
      END;
      fstack_loadtop(t);
    END;
    Mn(t, "FSTP ST, ");
    IF mvar.t = Type.Reel THEN
      IF t.debug THEN
        t.wr.OutT("m32real");
      END;
      opc := fopcode[FOp.fSTP].m32;
    ELSE
      <* ASSERT mvar.t = Type.LReel OR mvar.t = Type.XReel *>
      IF t.debug THEN
        t.wr.OutT("m64real");
      END;
      opc := fopcode[FOp.fSTP].m64;
    END;
    build_modrm(t, Operand {loc := OLoc.mem, mvar:= mvar},
                t.opcode[fopcode[FOp.fSTP].memop], modrm, disp, dsize);
    writecode(t, FALSE, opc, modrm, TRUE, 0, FALSE, disp, dsize,
              0, 0);
    log_global_var(t, mvar, -4);
    DEC(t.fstacksize);
    DEC(t.fstackloaded);
    t.ftop_inmem := FALSE;
  END fstack_pop;

PROCEDURE fstack_swap (t: T) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;

    get_temp(t);
    get_temp(t);

    Mn(t, "FLD ST, m80real");
    build_modrm(t, t.fstackspill[t.fspilltop-2], t.opcode[5],
                modrm, disp, dsize);
    writecode(t, FALSE, 16_DB, modrm, TRUE, 0, FALSE, disp, dsize,
              0, 0);

    Mn(t, "FLD ST, m80real");
    build_modrm(t, t.fstackspill[t.fspilltop-1], t.opcode[5],
                modrm, disp, dsize);
    writecode(t, FALSE, 16_DB, modrm, TRUE, 0, FALSE, disp, dsize,
              0, 0);

    DEC(t.fspilltop, 2);
  END fstack_swap;

PROCEDURE fstack_discard (t: T) =
  BEGIN
    fstack_check(t, 1, "fstack_discard");
    IF t.ftop_inmem THEN
      t.ftop_inmem := FALSE;
    ELSE
      binFOp(t, FOp.fFREE, 0);
      noargFOp(t, FOp.fINCSTP);
      DEC(t.fstackloaded);
    END;
    DEC(t.fstacksize);
  END fstack_discard;

PROCEDURE f_loadlit (t: T; READONLY flarr: ARRAY OF INTEGER; type: MType) =
  BEGIN
    IF t.ftop_inmem THEN
      fstack_loadtop(t);
    END;

    t.ftop_inmem := TRUE;
    WITH mvar = t.ftop_mem DO
      mvar.var := t.flitvar;
      mvar.t := type;
      mvar.o := 0;
    END;

    INC(t.fstacksize);

    t.f_litlist := NEW(FLiteral, arr := flarr, size := CG_Bytes[type],
                       loc := 0, link := t.f_litlist);
  END f_loadlit;

PROCEDURE fstack_check (t: T; depth: INTEGER; place: TEXT) =
  BEGIN
    IF t.fstacksize < depth THEN
      t.Err("Floating stack underflow in " & place);
    END;
    IF t.ftop_inmem THEN
      IF t.fstackloaded + 1 < depth THEN
        fstack_wipeup(t, depth-t.fstackloaded-1);
      END
    ELSE
      IF t.fstackloaded < depth THEN
        fstack_wipeup(t, depth-t.fstackloaded);
      END
    END
  END fstack_check;

PROCEDURE fstack_ensure (t: T; height: INTEGER) =
  VAR spill: INTEGER;
  BEGIN
    spill := t.fstackloaded + height - 8;
    IF t.ftop_inmem THEN
      INC(spill);
    END;
    IF spill > 0 THEN
      FOR i := 1 TO spill DO
        noargFOp(t, FOp.fDECSTP);
      END;
      FOR i := 1 TO spill DO
        get_temp(t);
      END;
      t.fstackloaded := t.fstackloaded - spill;
    END
  END fstack_ensure;

PROCEDURE fstack_wipeup(t: T; wipeup: INTEGER) =
  BEGIN
    IF wipeup + t.fstackloaded > 8 THEN
      t.Err("Stack overflow in fstack_wipeup");
    END;
    IF wipeup > t.fspilltop THEN
      t.Err("Not enough spilled fstack elements to replace in fstack_wipeup");
    END;
    FOR i := 1 TO wipeup DO
      retrieve_temp(t);
    END;
    FOR i := 1 TO wipeup DO
      noargFOp(t, FOp.fINCSTP);
    END;
    t.fstackloaded := t.fstackloaded + wipeup;
  END fstack_wipeup;

(*------------------------------------------------------- code writing i/o---*)

PROCEDURE Mn (t: T; mn1, mn2, mn3: TEXT := NIL) =
  BEGIN
    IF t.debug THEN
      Hexbe(t, t.obj.cursor(Seg.Text), 4);
      t.wr.OutT(":");
      IF mn1 # NIL THEN t.wr.OutT(mn1); END;
      IF mn2 # NIL THEN t.wr.OutT(mn2); END;
      IF mn3 # NIL THEN t.wr.OutT(mn3); END;
    END;
  END Mn;

PROCEDURE Hexbe (t: T; val: INTEGER; size: INTEGER) =
  BEGIN
    t.wr.OutT(" ");
    Hexberec(t, val, size);
  END Hexbe;

PROCEDURE Hexberec (t: T; val: INTEGER; size: INTEGER) =
(* Output a hex value as a single number (high byte first)*)
  BEGIN
    <* ASSERT size>0 *>
    IF size # 1 THEN
      Hexbe(t, val DIV 16_100, size-1);
    END;
    Byte(t, Word.And(val,16_ff));
  END Hexberec;

PROCEDURE Byte (t: T; val: INTEGER) =
  CONST Digits = ARRAY [0 .. 15] OF TEXT
    {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A",
     "B", "C", "D", "E", "F"};
  BEGIN
    t.wr.OutT(Digits[val DIV 16_10]);
    t.wr.OutT(Digits[Word.And(val,16_f)]);
  END Byte;

(*--------------------------------------------------- temporary var stuff ---*)

PROCEDURE get_temp (t: T) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    IF t.fspilltop = t.fspilllimit THEN
      expand_spill(t);
    END;
    WITH spill = t.fstackspill[t.fspilltop] DO
      IF t.fspilltop = t.fspillhigh THEN
        spill.loc := OLoc.mem;
        spill.mvar.var := t.parent.declare_temp(10, 4, Type.Void, FALSE);
        INC (t.fspillhigh);
      END;
      Mn(t, "FSTP ST, m80real");
      build_modrm(t, spill, t.opcode[7], modrm, disp, dsize);
      writecode(t, FALSE, 16_DB, modrm, TRUE, 0, FALSE, disp, dsize,
                0, 0);
    END;
    INC(t.fspilltop);
  END get_temp;

PROCEDURE retrieve_temp (t: T) =
  VAR modrm, disp, dsize: INTEGER;
  BEGIN
    <* ASSERT t.fspilltop > 0 *>
    DEC(t.fspilltop);
    WITH spill = t.fstackspill[t.fspilltop] DO
      Mn(t, "FLD ST, m80real");
      build_modrm(t, spill, t.opcode[5], modrm, disp, dsize);
      writecode(t, FALSE, 16_DB, modrm, TRUE, 0, FALSE, disp, dsize,
                0, 0);
    END;
  END retrieve_temp;

PROCEDURE expand_spill (t: T) =
  VAR newspill := NEW(REF ARRAY OF Operand, t.fspilllimit * 2);
  BEGIN
    FOR i := 0 TO t.fspilllimit DO
      newspill[i] := t.fstackspill[i];
    END;
    t.fstackspill := newspill;
    t.fspilllimit := t.fspilllimit * 2;
  END expand_spill;
    
(*------------------------------------------------------- alignment stuff ---*)

PROCEDURE aligned (<*UNUSED*> t: T; READONLY var: MVar;
                   align: Alignment): BOOLEAN =
  BEGIN
    IF Word.And(var.o + var.var.offset, align - 1) = 0 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END
  END aligned;

(*---------------------------------------------- future update list stuff ---*)

PROCEDURE log_global_var (t: T; var: MVar; reltocurs: INTEGER) =
  BEGIN
    IF var.var.loc # VLoc.global THEN
      RETURN;
    END;

    IF var.var = t.flitvar THEN
      <* ASSERT t.f_litlist # NIL AND t.f_litlist.loc = 0 AND
                t.f_litlist.size = CG_Bytes[var.t] AND
                (var.t = Type.Reel OR var.t = Type.LReel OR
                 var.t = Type.XReel) *>
      t.f_litlist.loc := t.obj.cursor(Seg.Text) + reltocurs;
    ELSIF var.var = t.internalvar THEN
      t.internal_list := NEW(Internal, ivar := VAL(var.o, IntnlVar),
                             loc := t.obj.cursor(Seg.Text) + reltocurs,
                             link := t.internal_list);
    ELSE
      t.obj.patch(Seg.Text, t.obj.cursor(Seg.Text) + reltocurs,
                  var.o + var.var.offset, 4);
      t.obj.relocate(t.textsym, t.obj.cursor(Seg.Text) + reltocurs,
                     var.var.symbol);
    END
  END log_global_var;

(*----------------------------------------------------------------- misc. ---*)

PROCEDURE set_error_handler (t: T; err: ErrorHandler) =
  BEGIN
    t.Err := err;
  END set_error_handler;

(*---------------------------------------------------------------------------*)

PROCEDURE init (t: T) =
  BEGIN
    t.tempsize := 0;

    t.fspilltop := 0;

    t.fstacksize := 0;
    t.fstackloaded := 0;

    t.ftop_inmem := FALSE;

    t.next_label_id := 0;

    t.f_litlist := NIL;
    t.abscall_list := NIL;
    t.internal_list := NIL;

    t.flitvar := t.parent.NewVar(Type.Struct, 0, 0, 4);
    t.flitvar.loc := VLoc.global;
    t.internalvar := t.parent.NewVar(Type.Struct, 0, 0, 4);
    t.internalvar.loc := VLoc.global;

    t.current_proc := NIL;

    t.textsym := 0;
  END init;

PROCEDURE end (t: T) =
  BEGIN
    tidy_internals(t);
  END end;

TYPE LocList = REF RECORD
  loc: ByteOffset;
  link: LocList;
END;

PROCEDURE find_flit (<*UNUSED*> t: T; flarr: ARRAY [0 .. 1] OF INTEGER;
                     size: INTEGER;
                     used: FLiteral; VAR loc: ByteOffset): BOOLEAN =
  BEGIN
    WHILE used # NIL DO
      IF flarr[0] = used.arr[0] AND
         (size = 4 OR (flarr[1] = used.arr[1])) THEN
        loc := used.loc;
        RETURN TRUE;
      END;

      used := used.link;
    END;

    RETURN FALSE;
  END find_flit;

PROCEDURE find_abscall (<*UNUSED *> t: T; internal: INTEGER;
                        used: AbsCall; VAR loc: ByteOffset): BOOLEAN =
  BEGIN
    WHILE used # NIL DO
      IF internal = used.sym THEN
        loc := used.loc;
        RETURN TRUE;
      END;

      used := used.link;
    END;

    RETURN FALSE;
  END find_abscall;

PROCEDURE find_internal (<*UNUSED *> t: T; internal: IntnlVar;
                         used: Internal; VAR loc: ByteOffset): BOOLEAN =
  BEGIN
    WHILE used # NIL DO
      IF internal = used.ivar THEN
        loc := used.loc;
        RETURN TRUE;
      END;

      used := used.link;
    END;

    RETURN FALSE;
  END find_internal;

PROCEDURE tidy_internals (t: T) =
  VAR internal_size := 0;
      fl_used: FLiteral;
      abscall_used: AbsCall;
      int_used: Internal;
      fl_locs, abscall_locs, int_locs: LocList;
      intvar: x86Var;
      flptr := t.f_litlist;
      abscallptr := t.abscall_list;
      intptr := t.internal_list;
  BEGIN
    fl_used := log_flit_use(t, internal_size, fl_locs);
    abscall_used := log_abscall_use(t, internal_size, abscall_locs);
    int_used := log_int_use(t, internal_size, int_locs);

    IF internal_size # 0 THEN
      intvar := init_intvar(t, internal_size, fl_used, abscall_used, int_used);

      WHILE flptr # NIL DO
        t.obj.patch(Seg.Text, flptr.loc, fl_locs.loc, 4);
        t.obj.relocate(t.textsym, flptr.loc, intvar.symbol);
        fl_locs := fl_locs.link;
        flptr := flptr.link;
      END;

      <* ASSERT fl_locs = NIL *>

      WHILE abscallptr # NIL DO
        t.obj.patch(Seg.Text, abscallptr.loc, abscall_locs.loc, 4);
        t.obj.relocate(t.textsym, abscallptr.loc, intvar.symbol);
        abscall_locs := abscall_locs.link;
        abscallptr := abscallptr.link;
      END;

      <* ASSERT abscall_locs = NIL *>

      WHILE intptr # NIL DO
        t.obj.patch(Seg.Text, intptr.loc, int_locs.loc, 4);
        t.obj.relocate(t.textsym, intptr.loc, intvar.symbol);
        int_locs := int_locs.link;
        intptr := intptr.link;
      END;

      <* ASSERT int_locs = NIL *>
    END
  END tidy_internals;

PROCEDURE log_flit_use (t: T; VAR internal_size: INTEGER; VAR flloc: LocList):
            FLiteral =
  VAR flptr := t.f_litlist;
      f_lit, f_littail: FLiteral := NIL;
      flloctail: LocList := NIL;
      f_litloc: ByteOffset;
  BEGIN
    WHILE flptr # NIL DO
      IF NOT find_flit(t, flptr.arr, flptr.size, f_lit, f_litloc) THEN
        f_litloc := internal_size;
        IF f_littail = NIL THEN
          f_littail := NEW(FLiteral, arr := flptr.arr, size := flptr.size,
                           loc := f_litloc, link := NIL);
          f_lit := f_littail;
        ELSE
          f_littail.link := NEW(FLiteral, arr := flptr.arr, size := flptr.size,
                                loc := f_litloc, link := NIL);
          f_littail := f_littail.link;
        END;

        INC(internal_size, flptr.size);
      END;

      IF flloctail = NIL THEN
        flloctail := NEW(LocList, loc := f_litloc, link := NIL);
        flloc := flloctail;
      ELSE
        flloctail.link := NEW(LocList, loc := f_litloc, link := NIL);
        flloctail := flloctail.link;
      END;

      flptr := flptr.link;
    END;

    RETURN f_lit;
  END log_flit_use;

PROCEDURE log_abscall_use (t: T; VAR internal_size: INTEGER;
                           VAR abscallloc: LocList):
            AbsCall =
  VAR abscallptr := t.abscall_list;
      abscall, abscalltail: AbsCall := NIL;
      absloctail: LocList := NIL;
      abcloc: ByteOffset;
  BEGIN
    WHILE abscallptr # NIL DO
      IF NOT find_abscall(t, abscallptr.sym, abscall, abcloc) THEN
        abcloc := internal_size;
        IF abscalltail = NIL THEN
          abscalltail := NEW(AbsCall, sym := abscallptr.sym,
                             loc := abcloc, link := NIL);
          abscall := abscalltail;
        ELSE
          abscalltail.link := NEW(AbsCall, sym := abscallptr.sym,
                                loc := abcloc, link := NIL);
          abscalltail := abscalltail.link;
        END;

        INC(internal_size, 4);
      END;

      IF absloctail = NIL THEN
        absloctail := NEW(LocList, loc := abcloc, link := NIL);
        abscallloc := absloctail;
      ELSE
        absloctail.link := NEW(LocList, loc := abcloc, link := NIL);
        absloctail := absloctail.link;
      END;

      abscallptr := abscallptr.link;
    END;

    RETURN abscall;
  END log_abscall_use;

PROCEDURE log_int_use (t: T; VAR internal_size: INTEGER; VAR inloc: LocList):
            Internal =
  VAR intptr := t.internal_list;
      int, inttail: Internal := NIL;
      inloctail: LocList := NIL;
      intloc: ByteOffset;
  BEGIN
    WHILE intptr # NIL DO
      IF NOT find_internal(t, intptr.ivar, int, intloc) THEN
        intloc := internal_size;
        IF inttail = NIL THEN
          inttail := NEW(Internal, ivar := intptr.ivar, loc := intloc,
                         link := NIL);
          int := inttail;
        ELSE
          inttail.link := NEW(Internal, ivar := intptr.ivar,
                              loc := intloc, link := NIL);
          inttail := inttail.link;
        END;

        INC(internal_size, InternalSize[intptr.ivar]);
      END;

      IF inloctail = NIL THEN
        inloctail := NEW(LocList, loc := intloc, link := NIL);
        inloc := inloctail;
      ELSE
        inloctail.link := NEW(LocList, loc := intloc, link := NIL);
        inloctail := inloctail.link;
      END;

      intptr := intptr.link;
    END;

    RETURN int;
  END log_int_use;

PROCEDURE init_intvar (t: T; size: ByteSize; f_lit: FLiteral; abscall: AbsCall;
                       int: Internal):
            x86Var =
  VAR intvar: x86Var;
      tint: Target.Int;
  BEGIN
    intvar := t.parent.declare_global(M3ID.NoID, size, 4,
                                      Type.Struct, 0, FALSE, TRUE);
    t.parent.begin_init(intvar);

    WHILE f_lit # NIL DO
      EVAL TargetInt.FromInt(f_lit.arr[0], tint);
      t.parent.init_int(f_lit.loc, tint, Type.Int);
      IF f_lit.size = 8 THEN
        EVAL TargetInt.FromInt(f_lit.arr[1], tint);
        t.parent.init_int(f_lit.loc + 4, tint, Type.Int);
      END;

      f_lit := f_lit.link;
    END;

    WHILE abscall # NIL DO
      t.parent.init_int(abscall.loc, TargetInt.Zero, Type.Int);
      t.obj.relocate(intvar.symbol, abscall.loc, abscall.sym);
      abscall := abscall.link;
    END;

    WHILE int # NIL DO
      init_internal(t, int.ivar, int.loc);
      int := int.link;
    END;

    t.parent.end_init(intvar);
    RETURN intvar;
  END init_intvar;

CONST InternalSize = ARRAY IntnlVar OF ByteSize
  { 33 * 4, 33 * 4 };

PROCEDURE init_internal(t: T; internal: IntnlVar; o: ByteOffset) =
  VAR tint: Target.Int;
      mask: Word.T;
  BEGIN
    CASE internal OF
      IntnlVar.Lowset_table =>
        mask := 0;
        FOR i := 0 TO 32 DO
          EVAL TargetInt.FromInt(mask, tint);
          mask := Word.Shift(mask, 1);
          mask := Word.Or(mask, 1);
          t.parent.init_int(o + i * 4, tint, Type.Word);
        END;
    | IntnlVar.Highset_table =>
        mask := 16_FFFFFFFF;
        FOR i := 0 TO 32 DO
          EVAL TargetInt.FromInt(mask, tint);
          t.parent.init_int(o + i * 4, tint, Type.Word);
          mask := Word.Shift(mask, 1);
        END
    END
  END init_internal;

PROCEDURE set_current_proc (t: T; p: x86Proc) =
  BEGIN
    t.current_proc := p;

    <* ASSERT t.fspilltop = 0 *>
    t.fspillhigh := 0;
  END set_current_proc;

PROCEDURE set_textsym (t: T; sym: INTEGER) =
  BEGIN
    t.textsym := sym;
  END set_textsym;

PROCEDURE set_obj (t: T; obj: M3ObjFile.T) =
  BEGIN
    t.obj := obj;
  END set_obj;

PROCEDURE wrFlush (t: T) =
  BEGIN
    IF t.debug THEN
      t.wr.Flush();
    END
  END wrFlush;

PROCEDURE New (parent: M3x86Rep.U; wr: Wrx86.T): T =
  VAR code := NEW(T, parent := parent, wr := wr);
  BEGIN
    IF wr # NIL THEN
      code.debug := TRUE;
    END;

    code.templimit := 32;
    code.temparr := NEW(REF ARRAY OF MVar, code.templimit);
    code.fspilllimit := 16;
    code.fstackspill := NEW(REF ARRAY OF Operand, code.fspilllimit);
    code.lablimit := 256;
    code.labarr := NEW(REF ARRAY OF x86Label, code.lablimit);

    FOR i := 0 TO NRegs DO
      code.reg[i].loc := OLoc.register;
      code.reg[i].reg := i;
      code.reg[i].opcode := FALSE;
      code.opcode[i].loc := OLoc.register;
      code.opcode[i].reg := i;
      code.opcode[i].opcode := TRUE;
    END;

    RETURN code;
  END New;

BEGIN
END Codex86.
