(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Oct 11 14:32:39 PDT 1994 by isard      *)
(*      modified on Fri Nov 19 09:30:31 PST 1993 by kalsow     *)
(*      modified on Mon Apr 13 09:55:12 PDT 1992 by muller     *)

INTERFACE Codex86;

IMPORT M3CG, M3ObjFile, TFloat;

FROM M3CG IMPORT MType, Label, ByteOffset, Alignment;
FROM M3CG_Ops IMPORT ErrorHandler;

IMPORT M3x86Rep, Wrx86;

FROM M3x86Rep IMPORT Operand, NRegs, MVar, x86Var, x86Proc, Regno;

TYPE T <: Public;
TYPE Public = OBJECT
        wr         : Wrx86.T := NIL;
        reg        : ARRAY [0 .. NRegs] OF Operand;
        ftop_inmem := FALSE;
      METHODS
        wrFlush ();
        set_obj (obj: M3ObjFile.T);
        set_current_proc (p: x86Proc);
        set_textsym(sym: INTEGER);
        intCall (lab: Label);
        relCall (rel: INTEGER);
        absCall (p: x86Proc);
        rmCall (READONLY op: Operand);
        cleanretOp (psize: INTEGER);
        brOp (br: Cond; l: Label);
        setccOp (READONLY op: Operand; cond: Cond);
        noargOp (op: Op);
        noargFOp (op: FOp);
        immFOp (op: FOp; im: FIm);
        binFOp (op: FOp; st: INTEGER);
        memFOp (op: FOp; mvar: MVar);
        assert_fstack (count: INTEGER);
        f_ensureloaded ();
        f_exitproc ();
        f_pushnew ();
        fstack_push (READONLY mvar: MVar; nomem := FALSE);
        fstack_pop (READONLY mvar: MVar);
        fstack_swap ();
        fstack_discard ();
        f_loadlit (READONLY flarr: FloatBytes; type: MType);
        immOp (op: Op; READONLY dest: Operand; imm: INTEGER);
        binOp (op: Op; READONLY dest, src: Operand);
        tableOp (op: Op; READONLY dest, index: Operand; scale: INTEGER;
                 table: MVar);
        swapOp (READONLY dest, src: Operand);
        movOp (READONLY dest, src: Operand);
        movDummyReloc (READONLY dest: Operand; sym: INTEGER);
        movImm (READONLY dest: Operand; imm: INTEGER);
        MOVSWOp ();
        STOSWOp ();
        CBWOp ();
        pushOp (READONLY src: Operand);
        popOp (READONLY dest: Operand);
        decOp (READONLY op: Operand);
        unOp (op: Op; READONLY dest: Operand);
        mulOp (READONLY src: Operand);
        imulOp (READONLY dest, src: Operand);
        imulImm (READONLY dest, src: Operand; imm, imsize: INTEGER);
        divOp (READONLY divisor: Operand);
        idivOp (READONLY divisor: Operand);
        diffdivOp (READONLY divisor: Operand; apos: BOOLEAN);
        diffmodOp (READONLY divisor: Operand; apos: BOOLEAN);
        must_extend (READONLY operand: Operand): BOOLEAN;
        get_addsize (READONLY Op: Operand): INTEGER;
        aligned (READONLY var: MVar; align: Alignment): BOOLEAN;
        allocate_temp (var: x86Var; size, align: INTEGER);
        reserve_labels (n: INTEGER; short := FALSE): Label;
        set_label (l: Label; offset := 0);
        case_jump (index: Operand; READONLY l: ARRAY OF Label);
        load_ind (r: Regno; READONLY ind: Operand; o: ByteOffset;
                  type: MType);
        fast_load_ind (r: Regno; READONLY ind: Operand; o: ByteOffset;
                       size: INTEGER);
        store_ind (READONLY val, ind: Operand; o: ByteOffset; type: MType);
        f_loadind (READONLY ind: Operand; o: ByteOffset; type: MType);
        f_storeind (READONLY ind: Operand; o: ByteOffset; type: MType);
        log_label_init (var: x86Var; o: ByteOffset; lab: Label);
        get_frame (r: Regno; target, current: x86Proc);
        set_error_handler (err: ErrorHandler);
        init ();
        end ();
      END;

TYPE Cond = { Z, NZ, E, NE, G, GE, L, LE, A, AE, B, BE, Always };

CONST CondName = ARRAY Cond OF TEXT { "Z", "NZ", "E", "NE", "G", "GE",
                                      "L", "LE", "A", "AE", "B", "BE", "*" };

CONST revcond = ARRAY Cond OF Cond
  { Cond.Z, Cond.NZ, Cond.E, Cond.NE, Cond.L, Cond.LE,
    Cond.G, Cond.GE, Cond.B, Cond.BE, Cond.A, Cond.AE, Cond.Always };

CONST unscond = ARRAY Cond OF Cond
  { Cond.Z, Cond.NZ, Cond.E, Cond.NE, Cond.A, Cond.AE,
    Cond.B, Cond.BE, Cond.A, Cond.AE, Cond.B, Cond.BE, Cond.Always };

TYPE
  BrOpCode = RECORD
    name: TEXT;
    rel8: INTEGER;
  END;

CONST bropcode = ARRAY Cond OF BrOpCode
  { BrOpCode { "JE",  16_74 },
    BrOpCode { "JNE", 16_75 },
    BrOpCode { "JE",  16_74 },
    BrOpCode { "JNE", 16_75 },
    BrOpCode { "JG",  16_7F },
    BrOpCode { "JGE", 16_7D },
    BrOpCode { "JL",  16_7C },
    BrOpCode { "JLE", 16_7E },
    BrOpCode { "JA",  16_77 },
    BrOpCode { "JAE", 16_73 },
    BrOpCode { "JB",  16_72 },
    BrOpCode { "JBE", 16_76 },
    BrOpCode { "JMP", -1    } };

TYPE
  CondOpCode = RECORD
    name: TEXT;
    opc: INTEGER;
  END;

CONST condopcode = ARRAY Cond OF CondOpCode
  { CondOpCode { "SETE",  16_94 },
    CondOpCode { "SETNE", 16_95 },
    CondOpCode { "SETE",  16_94 },
    CondOpCode { "SETNE", 16_95 },
    CondOpCode { "SETG",  16_9F },
    CondOpCode { "SETGE", 16_9D },
    CondOpCode { "SETL",  16_9C },
    CondOpCode { "SETLE", 16_9E },
    CondOpCode { "SETA",  16_97 },
    CondOpCode { "SETAE", 16_93 },
    CondOpCode { "SETB",  16_92 },
    CondOpCode { "SETBE", 16_96 },
    CondOpCode { "",      -1    } };

TYPE
  FloatBytes = ARRAY [0..7] OF TFloat.Byte;

TYPE
  FOpCode = RECORD
    name: TEXT;
    m32, m64, memop, stbase, stmodrm,
    stackdiff, memdiff, stackin, min: INTEGER;
    takesmem: BOOLEAN;
  END;

TYPE FOp = { fCOM, fCOMP, fCOMPP, fDECSTP, fINCSTP, fFREE,
             fNCLEX, fABS,
             fCHS, fNSTSWAX, fILD, fISTP, fLDCW, fSTCW,
             fLD, fSTP, fADDP, fSUBP, fSUBPR, fMUL, fDIV,
             fNOP };

CONST fopcode = ARRAY FOp OF FOpCode
  { FOpCode { "FCOM",    -1,    -1,    0, 16_D8, 16_D0, 0,  0,  2, 0, FALSE },
    FOpCode { "FCOM",    16_D8, 16_DC, 3, 16_D8, 16_D8, -2, -1, 2, 1, TRUE  },
    FOpCode { "FCOMP",   -1,    -1,    0, 16_DE, 16_D8, -2, 0,  2, 0, FALSE },
    FOpCode { "FDECSTP", -1,    -1,    0, 16_D9, 16_F6, 0,  0,  0, 0, FALSE },
    FOpCode { "FINCSTP", -1,    -1,    0, 16_D9, 16_F7, 0,  0,  0, 0, FALSE },
    FOpCode { "FFREE",   -1,    -1,    0, 16_DD, 16_C0, 0,  0,  0, 0, FALSE },
    FOpCode { "FNCLEX",  -1,    -1,    0, 16_DB, 16_E2, 0,  0,  0, 0, FALSE },
    FOpCode { "FABS",    -1,    -1,    0, 16_D9, 16_E1, 0,  0,  1, 0, FALSE },
    FOpCode { "FCHS",    -1,    -1,    0, 16_D9, 16_E0, 0,  0,  1, 0, FALSE },
    FOpCode { "FNSTSWAX",-1,    -1,    0, 16_DF, 16_E0, 0,  0,  0, 0, FALSE },
    FOpCode { "FILD",    16_DB, -1,    0, -1,    -1,    1,  1,  0, 0, FALSE },
    FOpCode { "FIST",    16_DB, -1,    3, -1,    -1,    -1, -1, 1, 1, FALSE },
    FOpCode { "FLDCW",   16_D9, -1,    5, -1,    -1,    0,  0,  0, 0, FALSE },
    FOpCode { "FSTCW",   16_D9, -1,    7, -1,    -1,    0,  0,  0, 0, FALSE },
    FOpCode { "FLD",     16_D9, 16_DD, 0, 16_D9, 16_C0, 1,  1,  0, 0, TRUE  },
    FOpCode { "FST",     16_D9, 16_DD, 3, 16_DD, 16_D8, -1, -1, 2, 1, TRUE  },
    FOpCode { "FADD",    16_D8, 16_DC, 0, 16_DE, 16_C0, -1, 0,  2, 1, TRUE  },
    FOpCode { "FSUB",    16_D8, 16_DC, 4, 16_DE, 16_E8, -1, 0,  2, 1, TRUE  },
    FOpCode { "FSUBR",   16_D8, 16_DC, 5, 16_DE, 16_E0, -1, 0,  2, 1, TRUE  },
    FOpCode { "FMUL",    16_D8, 16_DC, 1, 16_DE, 16_C8, -1, 0,  2, 1, TRUE  },
    FOpCode { "FDIV",    16_D8, 16_DC, 6, 16_DE, 16_F8, -1, 0,  2, 1, TRUE  },
    FOpCode { "FNOP",    -1,    -1,    0, -1,    -1,    0,  0,  0, 0, FALSE }};

TYPE
  FImOp = RECORD
    name: TEXT;
    opcode: INTEGER;
  END;

TYPE FIm = { ONE, L2T, L2E, PI, LG2, LN2, Z };

CONST FImName = ARRAY FIm OF TEXT { "1.0", "Log2(10)", "Log2(e)", "pi",
                                    "Log10(2)", "LogE(2)", "0.0" };

CONST imcode = ARRAY FIm OF FImOp
  { FImOp { "FLD1",   16_D9E8 },
    FImOp { "FLDL2T", 16_D9E9 },
    FImOp { "FLDL2E", 16_D9EA },
    FImOp { "FLDPI",  16_D9EB },
    FImOp { "FLDLG2", 16_D9EC },
    FImOp { "FLDLN2", 16_D9ED },
    FImOp { "FLDZ",   16_D9EE } };

TYPE
  OpCode = RECORD
    name: TEXT;
    Aimm32, imm8, imm32, immop, rmr, rrm: INTEGER;
  END;

TYPE Op = { oAND, oXOR, oOR, oMOV,
            oADD, oSUB, oCMP, oNEG, oNOT, oLEA,
            oSAL, oSAR, oSHR, oROL, oROR, oSAHF, oWAIT, oCLD, oSTD,
            oREP, oMOVSB, oMOVSD, oSTOSB, oSTOSD,
            oCWDE, oCDQ, oLEAVE, oRET, oNOP };

CONST opcode = ARRAY Op OF OpCode
  { OpCode { "AND",  16_25, 16_83, 16_81, 4, 16_20, 16_22 },
    OpCode { "XOR",  16_35, 16_83, 16_81, 6, 16_30, 16_32 },
    OpCode { "OR",   16_0D, 16_83, 16_81, 1, 16_08, 16_0A },
    OpCode { "MOV",  -1,    -1,    -1,    0, 16_88, 16_8A },
    OpCode { "ADD",  16_05, 16_83, 16_81, 0, 16_00, 16_02 },
    OpCode { "SUB",  16_2D, 16_83, 16_81, 5, 16_28, 16_2A },
    OpCode { "CMP",  16_3D, 16_83, 16_81, 7, 16_38, 16_3A },
    OpCode { "NEG",  -1,    -1,    16_F6, 3, -1,    -1    },
    OpCode { "NOT",  -1,    -1,    16_F6, 2, -1,    -1    },
    OpCode { "LEA",  -1,    -1,    -1,    0, -1,    16_8C },
    OpCode { "SAL",  -1,    16_C1, 16_D2, 4, -1,    -1    },
    OpCode { "SAR",  -1,    16_C1, 16_D2, 7, -1,    -1    },
    OpCode { "SHR",  -1,    16_C1, 16_D2, 5, -1,    -1    },
    OpCode { "ROL",  -1,    16_C1, 16_D2, 0, -1,    -1    },
    OpCode { "ROR",  -1,    16_C1, 16_D2, 1, -1,    -1    },
    OpCode { "SAHF", -1,    -1,    16_9E, 0, -1,    -1    },
    OpCode { "WAIT", -1,    -1,    16_9B, 0, -1,    -1    },
    OpCode { "CLD",  -1,    -1,    16_FC, 0, -1,    -1    },
    OpCode { "STD",  -1,    -1,    16_FD, 0, -1,    -1    },
    OpCode { "REP",  -1,    -1,    16_F3, 0, -1,    -1    },
    OpCode { "MOVSB",-1,    -1,    16_A4, 0, -1,    -1    },
    OpCode { "MOVSD",-1,    -1,    16_A5, 0, -1,    -1    },
    OpCode { "STOSB",-1,    -1,    16_AA, 0, -1,    -1    },
    OpCode { "STOSD",-1,    -1,    16_AB, 0, -1,    -1    },
    OpCode { "CWDE", -1,    -1,    16_98, 0, -1,    -1    },
    OpCode { "CDQ",  -1,    -1,    16_99, 0, -1,    -1    },
    OpCode { "LEAVE",-1,    -1,    16_C9, 0, -1,    -1    },
    OpCode { "RET"  ,-1,    -1,    16_C3, 0, -1,    -1    },
    OpCode { "NOP",  -1,    -1,    -1,    0, -1,    -1    } };

CONST EAX = 0;
      ECX = 1;
      EDX = 2;
      EBX = 3;
      ESP = 4;
      EBP = 5;
      ESI = 6;
      EDI = 7;

PROCEDURE New (parent: M3x86Rep.U; wr: Wrx86.T): T;

END Codex86.
