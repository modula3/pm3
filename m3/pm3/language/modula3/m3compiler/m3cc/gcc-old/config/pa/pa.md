;;- Machine description for HP PA-RISC architecture for GNU C compiler
;;   Copyright (C) 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
;;   Contributed by the Center for Software Science at the University
;;   of Utah.

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This gcc Version 2 machine description is inspired by sparc.md and
;; mips.md.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Insn type.  Used to default other attribute values.

;; type "unary" insns have one input operand (1) and one output operand (0)
;; type "binary" insns have two input operands (1,2) and one output (0)

(define_attr "type"
  "move,unary,binary,shift,nullshift,compare,load,store,uncond_branch,branch,cbranch,fbranch,call,dyncall,fpload,fpstore,fpalu,fpcc,fpmulsgl,fpmuldbl,fpdivsgl,fpdivdbl,fpsqrtsgl,fpsqrtdbl,multi,milli,parallel_branch"
  (const_string "binary"))

;; Processor type (for scheduling, not code generation) -- this attribute
;; must exactly match the processor_type enumeration in pa.h.
;;
;; FIXME: Add 800 scheduling for completeness?

(define_attr "cpu" "700,7100,7100LC" (const (symbol_ref "pa_cpu_attr")))

;; Length (in # of insns).
(define_attr "length" ""
  (cond [(eq_attr "type" "load,fpload")
	 (if_then_else (match_operand 1 "symbolic_memory_operand" "")
		       (const_int 8) (const_int 4))

	 (eq_attr "type" "store,fpstore")
	 (if_then_else (match_operand 0 "symbolic_memory_operand" "")
		       (const_int 8) (const_int 4))

	 (eq_attr "type" "binary,shift,nullshift")
	 (if_then_else (match_operand 2 "arith_operand" "")
		       (const_int 4) (const_int 12))

	 (eq_attr "type" "move,unary,shift,nullshift")
	 (if_then_else (match_operand 1 "arith_operand" "")
		       (const_int 4) (const_int 8))]

	(const_int 4)))

(define_asm_attributes
  [(set_attr "length" "4")
   (set_attr "type" "multi")])

;; Attributes for instruction and branch scheduling

;; For conditional branches.
(define_attr "in_branch_delay" "false,true"
  (if_then_else (and (eq_attr "type" "!uncond_branch,branch,cbranch,fbranch,call,dyncall,multi,milli,parallel_branch")
		     (eq_attr "length" "4"))
		(const_string "true")
		(const_string "false")))

;; Disallow instructions which use the FPU since they will tie up the FPU
;; even if the instruction is nullified.
(define_attr "in_nullified_branch_delay" "false,true"
  (if_then_else (and (eq_attr "type" "!uncond_branch,branch,cbranch,fbranch,call,dyncall,multi,milli,fpcc,fpalu,fpmulsgl,fpmuldbl,fpdivsgl,fpdivdbl,fpsqrtsgl,fpsqrtdbl,parallel_branch")
		     (eq_attr "length" "4"))
		(const_string "true")
		(const_string "false")))

;; For calls and millicode calls.  Allow unconditional branches in the
;; delay slot.
(define_attr "in_call_delay" "false,true"
  (cond [(and (eq_attr "type" "!uncond_branch,branch,cbranch,fbranch,call,dyncall,multi,milli,parallel_branch")
	      (eq_attr "length" "4"))
	   (const_string "true")
	 (eq_attr "type" "uncond_branch")
	   (if_then_else (ne (symbol_ref "TARGET_JUMP_IN_DELAY")
			     (const_int 0))
			 (const_string "true")
			 (const_string "false"))]
	(const_string "false")))


;; CYGNUS LOCAL mentoropt/law
;; Call delay slot description.
(define_delay (and (eq_attr "type" "call")
		   (eq (symbol_ref "first_reorg_pass") (const_int 0)))
  [(eq_attr "in_call_delay" "true") (nil) (nil)])

;; millicode call delay slot description.  Note it disallows delay slot
;; when TARGET_PORTABLE_RUNTIME is true.
;; CYGNUS LOCAL mentoropt/law
(define_delay (and (eq_attr "type" "milli")
		   (eq (symbol_ref "first_reorg_pass") (const_int 0)))
  [(and (eq_attr "in_call_delay" "true")
	(eq (symbol_ref "TARGET_PORTABLE_RUNTIME") (const_int 0)))
   (nil) (nil)])

;; Unconditional branch, return and other similar instructions.
;; CYGNUS LOCAL mentoropt/law
(define_delay (and (eq_attr "type" "uncond_branch,branch,parallel_branch")
		   (eq (symbol_ref "first_reorg_pass") (const_int 0)))
  [(eq_attr "in_branch_delay" "true") (nil) (nil)])

;; Floating point conditional branch delay slot description and
;; CYGNUS LOCAL mentoropt/law
(define_delay (and (eq_attr "type" "fbranch")
		   (eq (symbol_ref "first_reorg_pass") (const_int 0)))
  [(eq_attr "in_branch_delay" "true")
   (eq_attr "in_nullified_branch_delay" "true")
   (nil)])

;; Integer conditional branch delay slot description.
;; Nullification of conditional branches on the PA is dependent on the
;; direction of the branch.  Forward branches nullify true and
;; backward branches nullify false.  If the direction is unknown
;; then nullification is not allowed.
;; CYGNUS LOCAL mentoropt/law
(define_delay (and (eq_attr "type" "cbranch")
		   (eq (symbol_ref "first_reorg_pass") (const_int 0)))
  [(eq_attr "in_branch_delay" "true")
   (and (eq_attr "in_nullified_branch_delay" "true")
	(attr_flag "forward"))
   (and (eq_attr "in_nullified_branch_delay" "true")
	(attr_flag "backward"))])

;; CYGNUS LOCAL mentoropt/law
;; "delay" slot descriptions for use in the PA specific reorg pass
;; which combines unrelated insns.

;; This one is used to combine a unconditional branch with an insn
;; which loads a register from a small int or other register.
(define_attr "combinable_with_uncond_jump" "false,true"
  (cond [(and (eq_attr "length" "4")
	      (ior (eq (symbol_ref "combinable_add (insn)") (const_int 1))
		   (eq (symbol_ref "combinable_copy (insn)") (const_int 1))))
	   (const_string "true")]
	(const_string "false")))

;; Note that this loses on newer machines for forward
;; unconditional jumps because new machines can multi-issue.
;; The backwards/forward test could be conditional on the
;; scheduling type as well as a "space vs time" flag.
(define_delay (and (eq_attr "type" "uncond_branch")
		   (and (eq (symbol_ref "first_reorg_pass")
			    (const_int 1))
			(eq (symbol_ref "following_call (insn)")
			    (const_int 0))))
  [(and (attr_flag "backward")
	(eq_attr "combinable_with_uncond_jump" "true")) (nil) (nil)])

;; This one is used to combine a a floating pointer multiply
;; with either a floating point add or floating point subtract.
(define_attr "combinable_with_fmpy" "false,true"
  (cond [(and (eq_attr "length" "4")
	      (ior (eq (symbol_ref "combinable_fadd (insn)")
		       (const_int 1))
		   (eq (symbol_ref "combinable_fsub (insn)")
		       (const_int 1))))
	   (const_string "true")]
	(const_string "false")))

(define_delay (and (eq_attr "type" "fpmuldbl,fpmulsgl")
		   (and (eq (symbol_ref "first_reorg_pass") (const_int 1))
			(eq (symbol_ref "combinable_fmpy (insn)")
			    (const_int 1))))
  [(eq_attr "combinable_with_fmpy" "true") (nil) (nil)])
;; END CYGNUS LOCAL

;; Function units of the HPPA. The following data is for the 700 CPUs
;; (Mustang CPU + Timex FPU aka PA-89) because that's what I have the docs for.
;; Scheduling instructions for PA-83 machines according to the Snake
;; constraints shouldn't hurt.

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {issue-delay} [{conflict-list}])

;; The integer ALU.
;; (Noted only for documentation; units that take one cycle do not need to
;; be specified.)

;; (define_function_unit "alu" 1 0
;;  (and (eq_attr "type" "unary,shift,nullshift,binary,move,address")
;;	 (eq_attr "cpu" "700"))
;;  1 0)


;; Memory. Disregarding Cache misses, the Mustang memory times are:
;; load: 2, fpload: 3
;; store, fpstore: 3, no D-cache operations should be scheduled.

(define_function_unit "pa700memory" 1 0
  (and (eq_attr "type" "load,fpload")
       (eq_attr "cpu" "700")) 2 0)
(define_function_unit "pa700memory" 1 0 
  (and (eq_attr "type" "store,fpstore")
       (eq_attr "cpu" "700")) 3 3)

;; The Timex (aka 700) has two floating-point units: ALU, and MUL/DIV/SQRT.
;; Timings:
;; Instruction	Time	Unit	Minimum Distance (unit contention)
;; fcpy		3	ALU	2
;; fabs		3	ALU	2
;; fadd		3	ALU	2
;; fsub		3	ALU	2
;; fcmp		3	ALU	2
;; fcnv		3	ALU	2
;; fmpyadd	3	ALU,MPY	2
;; fmpysub	3	ALU,MPY 2
;; fmpycfxt	3	ALU,MPY 2
;; fmpy		3	MPY	2
;; fmpyi	3	MPY	2
;; fdiv,sgl	10	MPY	10
;; fdiv,dbl	12	MPY	12
;; fsqrt,sgl	14	MPY	14
;; fsqrt,dbl	18	MPY	18

(define_function_unit "pa700fp_alu" 1 0
  (and (eq_attr "type" "fpcc")
       (eq_attr "cpu" "700")) 4 2)
(define_function_unit "pa700fp_alu" 1 0
  (and (eq_attr "type" "fpalu")
       (eq_attr "cpu" "700")) 3 2)
(define_function_unit "pa700fp_mpy" 1 0
  (and (eq_attr "type" "fpmulsgl,fpmuldbl")
       (eq_attr "cpu" "700")) 3 2)
(define_function_unit "pa700fp_mpy" 1 0
  (and (eq_attr "type" "fpdivsgl")
       (eq_attr "cpu" "700")) 10 10)
(define_function_unit "pa700fp_mpy" 1 0
  (and (eq_attr "type" "fpdivdbl")
       (eq_attr "cpu" "700")) 12 12)
(define_function_unit "pa700fp_mpy" 1 0
  (and (eq_attr "type" "fpsqrtsgl")
       (eq_attr "cpu" "700")) 14 14)
(define_function_unit "pa700fp_mpy" 1 0
  (and (eq_attr "type" "fpsqrtdbl")
       (eq_attr "cpu" "700")) 18 18)

;; Function units for the 7100 and 7150.  The 7100/7150 can dual-issue
;; floating point computations with non-floating point computations (fp loads
;; and stores are not fp computations).
;;
;; As with the alpha we multiply the ready delay by two to encourage
;; schedules which will allow the 7100/7150 to dual issue as many instructions
;; as possible.

;; Memory. Disregarding Cache misses, memory loads take two cycles; stores also
;; take two cycles, during which no Dcache operations should be scheduled.
;; Any special cases are handled in pa_adjust_cost.  The 7100, 7150 and 7100LC
;; all have the same memory characteristics if one disregards cache misses.
(define_function_unit "pa7100memory" 1 0
  (and (eq_attr "type" "load,fpload")
       (eq_attr "cpu" "7100,7100LC")) 4 0)
(define_function_unit "pa7100memory" 1 0 
  (and (eq_attr "type" "store,fpstore")
       (eq_attr "cpu" "7100,7100LC")) 4 4)

;; The 7100/7150 has three floating-point units: ALU, MUL, and DIV.
;; Timings:
;; Instruction	Time	Unit	Minimum Distance (unit contention)
;; fcpy		2	ALU	1
;; fabs		2	ALU	1
;; fadd		2	ALU	1
;; fsub		2	ALU	1
;; fcmp		2	ALU	1
;; fcnv		2	ALU	1
;; fmpyadd	2	ALU,MPY	1
;; fmpysub	2	ALU,MPY 1
;; fmpycfxt	2	ALU,MPY 1
;; fmpy		2	MPY	1
;; fmpyi	2	MPY	1
;; fdiv,sgl	8	DIV	8
;; fdiv,dbl	15	DIV	15
;; fsqrt,sgl	8	DIV	8
;; fsqrt,dbl	15	DIV	15

(define_function_unit "pa7100fp_alu" 1 0
  (and (eq_attr "type" "fpcc,fpalu")
       (eq_attr "cpu" "7100")) 4 2)
(define_function_unit "pa7100fp_mpy" 1 0
  (and (eq_attr "type" "fpmulsgl,fpmuldbl")
       (eq_attr "cpu" "7100")) 4 2)
(define_function_unit "pa7100fp_div" 1 0
  (and (eq_attr "type" "fpdivsgl,fpsqrtsgl")
       (eq_attr "cpu" "7100")) 16 16)
(define_function_unit "pa7100fp_div" 1 0
  (and (eq_attr "type" "fpdivdbl,fpsqrtdbl")
       (eq_attr "cpu" "7100")) 30 30)

;; To encourage dual issue we define function units corresponding to
;; the instructions which can be dual issued.    This is a rather crude
;; approximation, the "pa7100nonflop" test in particular could be refined.
(define_function_unit "pa7100flop" 1 1
  (and
    (eq_attr "type" "fpcc,fpalu,fpmulsgl,fpmuldbl,fpdivsgl,fpsqrtsgl,fpdivdbl,fpsqrtdbl")
    (eq_attr "cpu" "7100,7100LC")) 2 2)

(define_function_unit "pa7100nonflop" 1 1
  (and
    (eq_attr "type" "!fpcc,fpalu,fpmulsgl,fpmuldbl,fpdivsgl,fpsqrtsgl,fpdivdbl,fpsqrtdbl")
    (eq_attr "cpu" "7100")) 2 2)


;; Memory subsystem works just like 7100/7150 (except for cache miss times which
;; we don't model here).  

;; The 7100LC has three floating-point units: ALU, MUL, and DIV.
;; Note divides and sqrt flops lock the cpu until the flop is
;; finished.  fmpy and xmpyu (fmpyi) lock the cpu for one cycle.
;; There's no way to avoid the penalty.
;; Timings:
;; Instruction	Time	Unit	Minimum Distance (unit contention)
;; fcpy		2	ALU	1
;; fabs		2	ALU	1
;; fadd		2	ALU	1
;; fsub		2	ALU	1
;; fcmp		2	ALU	1
;; fcnv		2	ALU	1
;; fmpyadd,sgl	2	ALU,MPY	1
;; fmpyadd,dbl	3	ALU,MPY	2
;; fmpysub,sgl	2	ALU,MPY 1
;; fmpysub,dbl	3	ALU,MPY 2
;; fmpycfxt,sgl	2	ALU,MPY 1
;; fmpycfxt,dbl	3	ALU,MPY 2
;; fmpy,sgl	2	MPY	1
;; fmpy,dbl	3	MPY	2
;; fmpyi	3	MPY	2
;; fdiv,sgl	8	DIV	8
;; fdiv,dbl	15	DIV	15
;; fsqrt,sgl	8	DIV	8
;; fsqrt,dbl	15	DIV	15

(define_function_unit "pa7100LCfp_alu" 1 0
  (and (eq_attr "type" "fpcc,fpalu")
       (eq_attr "cpu" "7100LC")) 4 2)
(define_function_unit "pa7100LCfp_mpy" 1 0
  (and (eq_attr "type" "fpmulsgl")
       (eq_attr "cpu" "7100LC")) 4 2)
(define_function_unit "pa7100LCfp_mpy" 1 0
  (and (eq_attr "type" "fpmuldbl")
       (eq_attr "cpu" "7100LC")) 6 4)
(define_function_unit "pa7100LCfp_div" 1 0
  (and (eq_attr "type" "fpdivsgl,fpsqrtsgl")
       (eq_attr "cpu" "7100LC")) 16 16)
(define_function_unit "pa7100LCfp_div" 1 0
  (and (eq_attr "type" "fpdivdbl,fpsqrtdbl")
       (eq_attr "cpu" "7100LC")) 30 30)

;; Define the various functional units for dual-issue.
;; The 7100LC shares the generic "flop" unit specification with the 7100/7150.

;; The 7100LC has two basic integer which allow dual issue of most integer
;; instructions.  This needs further refinement to deal with the nullify,
;; carry/borrow possible the ldw/ldw stw/stw special dual issue cases, and
;; of course it needs to know about hte 2nd alu.
(define_function_unit "pa7100LCnonflop" 1 1
  (and
    (eq_attr "type" "!fpcc,fpalu,fpmulsgl,fpmuldbl,fpdivsgl,fpsqrtsgl,fpdivdbl,fpsqrtdbl,load,fpload,store,fpstore,shift,nullshift")
    (eq_attr "cpu" "7100LC")) 2 2)

(define_function_unit "pa7100LCshifter" 1 1
  (and
    (eq_attr "type" "shift,nullshift")
    (eq_attr "cpu" "7100LC")) 2 2)

(define_function_unit "pa7100LCmem" 1 1
  (and
    (eq_attr "type" "load,fpload,store,fpstore")
    (eq_attr "cpu" "7100LC")) 2 2)


;; Compare instructions.
;; This controls RTL generation and register allocation.

;; We generate RTL for comparisons and branches by having the cmpxx
;; patterns store away the operands.  Then, the scc and bcc patterns
;; emit RTL for both the compare and the branch.
;;

(define_expand "cmpsi"
  [(set (reg:CC 0)
	(compare:CC (match_operand:SI 0 "reg_or_0_operand" "")
		    (match_operand:SI 1 "arith5_operand" "")))]
  ""
  "
{
 hppa_compare_op0 = operands[0];
 hppa_compare_op1 = operands[1];
 hppa_branch_type = CMP_SI;
 DONE;
}")

(define_expand "cmpsf"
  [(set (reg:CCFP 0)
	(compare:CCFP (match_operand:SF 0 "reg_or_0_operand" "")
		      (match_operand:SF 1 "reg_or_0_operand" "")))]
  "! TARGET_SOFT_FLOAT"
  "
{
  hppa_compare_op0 = operands[0];
  hppa_compare_op1 = operands[1];
  hppa_branch_type = CMP_SF;
  DONE;
}")

(define_expand "cmpdf"
  [(set (reg:CCFP 0)
      (compare:CCFP (match_operand:DF 0 "reg_or_0_operand" "")
                    (match_operand:DF 1 "reg_or_0_operand" "")))]
  "! TARGET_SOFT_FLOAT"
  "
{
  hppa_compare_op0 = operands[0];
  hppa_compare_op1 = operands[1];
  hppa_branch_type = CMP_DF;
  DONE;
}")

(define_insn ""
  [(set (reg:CCFP 0)
	(match_operator:CCFP 2 "comparison_operator"
			     [(match_operand:SF 0 "reg_or_0_operand" "fG")
			      (match_operand:SF 1 "reg_or_0_operand" "fG")]))]
  "! TARGET_SOFT_FLOAT"
  "*
{
  rtx next_insn;

  /* See if this is later used in a reversed FP branch.  If so, reverse our
     condition and the branch.  Doing so avoids a useless add,tr. 

     Don't do this if fcmp is in a delay slot since it's too much of a
     headache to track down things on multiple paths.  */
  if (dbr_sequence_length ())
    next_insn = NULL;
  else
    next_insn = NEXT_INSN (insn);
  while (next_insn)
    {
      /* Jumps, calls and labels stop our search.  */
      if (GET_CODE (next_insn) == JUMP_INSN
	  || GET_CODE (next_insn) == CALL_INSN
	  || GET_CODE (next_insn) == CODE_LABEL)
	break;

      /* As does another fcmp insn.  */
      if (GET_CODE (next_insn) == INSN
	  && GET_CODE (PATTERN (next_insn)) == SET
	  && GET_CODE (SET_DEST (PATTERN (next_insn))) == REG
	  && REGNO (SET_DEST (PATTERN (next_insn))) == 0)
	break;

      if (GET_CODE (next_insn) == INSN
	  && GET_CODE (PATTERN (next_insn)) == SEQUENCE)
	next_insn = XVECEXP (PATTERN (next_insn), 0, 0);
      else
	next_insn = NEXT_INSN (next_insn);
    }

  /* Is NEXT_INSN a branch?  */
  if (next_insn
      && GET_CODE (next_insn) == JUMP_INSN)
    {
      rtx pattern = PATTERN (next_insn);

      /* Is it a reversed fp conditional branch (eg uses add,tr) and
	 CCFP dies, then reverse our conditional and the branch to
	 avoid the add,tr.  */
      if (GET_CODE (pattern) == SET
	  && SET_DEST (pattern) == pc_rtx
	  && GET_CODE (SET_SRC (pattern)) == IF_THEN_ELSE
	  && GET_CODE (XEXP (SET_SRC (pattern), 0)) == NE
	  && GET_CODE (XEXP (XEXP (SET_SRC (pattern), 0), 0)) == REG
	  && REGNO (XEXP (XEXP (SET_SRC (pattern), 0), 0)) == 0
	  && GET_CODE (XEXP (SET_SRC (pattern), 1)) == PC
	  && find_regno_note (next_insn, REG_DEAD, 0))

	{
	  rtx tmp;

	  tmp = XEXP (SET_SRC (pattern), 1);
	  XEXP (SET_SRC (pattern), 1) = XEXP (SET_SRC (pattern), 2);
	  XEXP (SET_SRC (pattern), 2) = tmp;
	  INSN_CODE (next_insn) = -1;
	  return \"fcmp,sgl,%y2 %r0,%r1\";
	}
    }
  return \"fcmp,sgl,%Y2 %r0,%r1\";
}"
  [(set_attr "length" "4")
   (set_attr "type" "fpcc")])

(define_insn ""
  [(set (reg:CCFP 0)
	(match_operator:CCFP 2 "comparison_operator"
			     [(match_operand:DF 0 "reg_or_0_operand" "fG")
			      (match_operand:DF 1 "reg_or_0_operand" "fG")]))]
  "! TARGET_SOFT_FLOAT"
  "*
{
  rtx next_insn;

  /* See if this is later used in a reversed FP branch.  If so, reverse our
     condition and the branch.  Doing so avoids a useless add,tr. 

     Don't do this if fcmp is in a delay slot since it's too much of a
     headache to track down things on multiple paths.  */
  if (dbr_sequence_length ())
    next_insn = NULL;
  else
    next_insn = NEXT_INSN (insn);
  while (next_insn)
    {
      /* Jumps, calls and labels stop our search.  */
      if (GET_CODE (next_insn) == JUMP_INSN
	  || GET_CODE (next_insn) == CALL_INSN
	  || GET_CODE (next_insn) == CODE_LABEL)
	break;

      /* As does another fcmp insn.  */
      if (GET_CODE (next_insn) == INSN
	  && GET_CODE (PATTERN (next_insn)) == SET
	  && GET_CODE (SET_DEST (PATTERN (next_insn))) == REG
	  && REGNO (SET_DEST (PATTERN (next_insn))) == 0)
	break;

      if (GET_CODE (next_insn) == INSN
	  && GET_CODE (PATTERN (next_insn)) == SEQUENCE)
	next_insn = XVECEXP (PATTERN (next_insn), 0, 0);
      else
	next_insn = NEXT_INSN (next_insn);
    }

  /* Is NEXT_INSN a branch?  */
  if (next_insn
      && GET_CODE (next_insn) == JUMP_INSN)
    {
      rtx pattern = PATTERN (next_insn);

      /* Is it a reversed fp conditional branch (eg uses add,tr) and
	 CCFP dies, then reverse our conditional and the branch to
	 avoid the add,tr.  */
      if (GET_CODE (pattern) == SET
	  && SET_DEST (pattern) == pc_rtx
	  && GET_CODE (SET_SRC (pattern)) == IF_THEN_ELSE
	  && GET_CODE (XEXP (SET_SRC (pattern), 0)) == NE
	  && GET_CODE (XEXP (XEXP (SET_SRC (pattern), 0), 0)) == REG
	  && REGNO (XEXP (XEXP (SET_SRC (pattern), 0), 0)) == 0
	  && GET_CODE (XEXP (SET_SRC (pattern), 1)) == PC
	  && find_regno_note (next_insn, REG_DEAD, 0))

	{
	  rtx tmp;

	  tmp = XEXP (SET_SRC (pattern), 1);
	  XEXP (SET_SRC (pattern), 1) = XEXP (SET_SRC (pattern), 2);
	  XEXP (SET_SRC (pattern), 2) = tmp;
	  INSN_CODE (next_insn) = -1;
	  return \"fcmp,dbl,%y2 %r0,%r1\";
	}
    }
  return \"fcmp,dbl,%Y2 %r0,%r1\";
}"
  [(set_attr "length" "4")
   (set_attr "type" "fpcc")])

;; scc insns.

(define_expand "seq"
  [(set (match_operand:SI 0 "register_operand" "")
	(eq:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  /* fp scc patterns rarely match, and are not a win on the PA.  */
  if (hppa_branch_type != CMP_SI)
    FAIL;
  /* set up operands from compare.  */
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
  /* fall through and generate default code */
}")

(define_expand "sne"
  [(set (match_operand:SI 0 "register_operand" "")
	(ne:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  /* fp scc patterns rarely match, and are not a win on the PA.  */
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "slt"
  [(set (match_operand:SI 0 "register_operand" "")
	(lt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  /* fp scc patterns rarely match, and are not a win on the PA.  */
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sgt"
  [(set (match_operand:SI 0 "register_operand" "")
	(gt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  /* fp scc patterns rarely match, and are not a win on the PA.  */
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sle"
  [(set (match_operand:SI 0 "register_operand" "")
	(le:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  /* fp scc patterns rarely match, and are not a win on the PA.  */
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sge"
  [(set (match_operand:SI 0 "register_operand" "")
	(ge:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  /* fp scc patterns rarely match, and are not a win on the PA.  */
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sltu"
  [(set (match_operand:SI 0 "register_operand" "")
	(ltu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "register_operand" "")
	(gtu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sleu"
  [(set (match_operand:SI 0 "register_operand" "")
	(leu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sgeu"
  [(set (match_operand:SI 0 "register_operand" "")
	(geu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

;; Instruction canonicalization puts immediate operands second, which
;; is the reverse of what we want.

(define_insn "scc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 3 "comparison_operator"
			   [(match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "arith11_operand" "rI")]))]
  ""
  "com%I2clr,%B3 %2,%1,%0\;ldi 1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn "iorscc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operator:SI 3 "comparison_operator"
				   [(match_operand:SI 1 "register_operand" "r")
				    (match_operand:SI 2 "arith11_operand" "rI")])
		(match_operator:SI 6 "comparison_operator"
				   [(match_operand:SI 4 "register_operand" "r")
				    (match_operand:SI 5 "arith11_operand" "rI")])))]
  ""
  "com%I2clr,%S3 %2,%1,0\;com%I5clr,%B6 %5,%4,%0\;ldi 1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "12")])

;; Combiner patterns for common operations performed with the output
;; from an scc insn (negscc and incscc).
(define_insn "negscc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operator:SI 3 "comparison_operator"
	       [(match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith11_operand" "rI")])))]
  ""
  "com%I2clr,%B3 %2,%1,%0\;ldi -1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

;; Patterns for adding/subtracting the result of a boolean expression from
;; a register.  First we have special patterns that make use of the carry
;; bit, and output only two instructions.  For the cases we can't in
;; general do in two instructions, the incscc pattern at the end outputs
;; two or three instructions.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (leu:SI (match_operand:SI 2 "register_operand" "r")
			 (match_operand:SI 3 "arith11_operand" "rI"))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sub%I3 %3,%2,0\;addc 0,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

; This need only accept registers for op3, since canonicalization
; replaces geu with gtu when op3 is an integer.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (geu:SI (match_operand:SI 2 "register_operand" "r")
			 (match_operand:SI 3 "register_operand" "r"))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sub %2,%3,0\;addc 0,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

; Match only integers for op3 here.  This is used as canonical form of the
; geu pattern when op3 is an integer.  Don't match registers since we can't
; make better code than the general incscc pattern.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (gtu:SI (match_operand:SI 2 "register_operand" "r")
			 (match_operand:SI 3 "int11_operand" "I"))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "addi %k3,%2,0\;addc 0,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn "incscc"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
 	(plus:SI (match_operator:SI 4 "comparison_operator"
		    [(match_operand:SI 2 "register_operand" "r,r")
		     (match_operand:SI 3 "arith11_operand" "rI,rI")])
		 (match_operand:SI 1 "register_operand" "0,?r")))]
  ""
  "@
   com%I3clr,%B4 %3,%2,0\;addi 1,%0,%0
   com%I3clr,%B4 %3,%2,0\;addi,tr 1,%1,%0\;copy %1,%0"
  [(set_attr "type" "binary,binary")
   (set_attr "length" "8,12")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (gtu:SI (match_operand:SI 2 "register_operand" "r")
			  (match_operand:SI 3 "arith11_operand" "rI"))))]
  ""
  "sub%I3 %3,%2,0\;subb %1,0,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "register_operand" "r")
			    (gtu:SI (match_operand:SI 2 "register_operand" "r")
				    (match_operand:SI 3 "arith11_operand" "rI")))
		  (match_operand:SI 4 "register_operand" "r")))]
  ""
  "sub%I3 %3,%2,0\;subb %1,%4,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

; This need only accept registers for op3, since canonicalization
; replaces ltu with leu when op3 is an integer.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (ltu:SI (match_operand:SI 2 "register_operand" "r")
			  (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "sub %2,%3,0\;subb %1,0,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "register_operand" "r")
			    (ltu:SI (match_operand:SI 2 "register_operand" "r")
				    (match_operand:SI 3 "register_operand" "r")))
		  (match_operand:SI 4 "register_operand" "r")))]
  ""
  "sub %2,%3,0\;subb %1,%4,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

; Match only integers for op3 here.  This is used as canonical form of the
; ltu pattern when op3 is an integer.  Don't match registers since we can't
; make better code than the general incscc pattern.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (leu:SI (match_operand:SI 2 "register_operand" "r")
			  (match_operand:SI 3 "int11_operand" "I"))))]
  ""
  "addi %k3,%2,0\;subb %1,0,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "register_operand" "r")
			    (leu:SI (match_operand:SI 2 "register_operand" "r")
				    (match_operand:SI 3 "int11_operand" "I")))
		  (match_operand:SI 4 "register_operand" "r")))]
  ""
  "addi %k3,%2,0\;subb %1,%4,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn "decscc"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "register_operand" "0,?r")
		  (match_operator:SI 4 "comparison_operator"
		     [(match_operand:SI 2 "register_operand" "r,r")
		      (match_operand:SI 3 "arith11_operand" "rI,rI")])))]
  ""
  "@
   com%I3clr,%B4 %3,%2,0\;addi -1,%0,%0
   com%I3clr,%B4 %3,%2,0\;addi,tr -1,%1,%0\;copy %1,%0"
  [(set_attr "type" "binary,binary")
   (set_attr "length" "8,12")])

; Patterns for max and min.  (There is no need for an earlyclobber in the
; last alternative since the middle alternative will match if op0 == op1.)

(define_insn "sminsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(smin:SI (match_operand:SI 1 "register_operand" "%0,0,r")
		 (match_operand:SI 2 "arith11_operand" "r,I,M")))]
  ""
  "@
  comclr,> %2,%0,0\;copy %2,%0
  comiclr,> %2,%0,0\;ldi %2,%0
  comclr,> %1,%2,%0\;copy %1,%0"
[(set_attr "type" "multi,multi,multi")
 (set_attr "length" "8,8,8")])

(define_insn "uminsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(umin:SI (match_operand:SI 1 "register_operand" "%0,0")
		 (match_operand:SI 2 "arith11_operand" "r,I")))]
  ""
  "@
  comclr,>> %2,%0,0\;copy %2,%0
  comiclr,>> %2,%0,0\;ldi %2,%0"
[(set_attr "type" "multi,multi")
 (set_attr "length" "8,8")])

(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(smax:SI (match_operand:SI 1 "register_operand" "%0,0,r")
		 (match_operand:SI 2 "arith11_operand" "r,I,M")))]
  ""
  "@
  comclr,< %2,%0,0\;copy %2,%0
  comiclr,< %2,%0,0\;ldi %2,%0
  comclr,< %1,%2,%0\;copy %1,%0"
[(set_attr "type" "multi,multi,multi")
 (set_attr "length" "8,8,8")])

(define_insn "umaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(umax:SI (match_operand:SI 1 "register_operand" "%0,0")
		 (match_operand:SI 2 "arith11_operand" "r,I")))]
  ""
  "@
  comclr,<< %2,%0,0\;copy %2,%0
  comiclr,<< %2,%0,0\;ldi %2,%0"
[(set_attr "type" "multi,multi")
 (set_attr "length" "8,8")])

(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(abs:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "or,>= %%r0,%1,%0\;subi 0,%0,%0"
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

;;; Experimental conditional move patterns

(define_expand "movsicc"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
	 (match_operator 1 "comparison_operator"
	    [(match_dup 4)
	     (match_dup 5)])
	 (match_operand:SI 2 "reg_or_cint_move_operand" "")
	 (match_operand:SI 3 "reg_or_cint_move_operand" "")))]
  ""
  "
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (hppa_branch_type != CMP_SI)
    FAIL;

  /* operands[1] is currently the result of compare_from_rtx.  We want to
     emit a compare of the original operands.  */
  operands[1] = gen_rtx (code, SImode, hppa_compare_op0, hppa_compare_op1);
  operands[4] = hppa_compare_op0;
  operands[5] = hppa_compare_op1;
}")

; We need the first constraint alternative in order to avoid
; earlyclobbers on all other alternatives.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	    [(match_operand:SI 3 "register_operand" "r,r,r,r,r")
	     (match_operand:SI 4 "arith11_operand" "rI,rI,rI,rI,rI")])
	 (match_operand:SI 1 "reg_or_cint_move_operand" "0,r,J,N,K")
	 (const_int 0)))]
  ""
  "@
   com%I4clr,%S5 %4,%3,0\;ldi 0,%0
   com%I4clr,%B5 %4,%3,%0\;copy %1,%0
   com%I4clr,%B5 %4,%3,%0\;ldi %1,%0
   com%I4clr,%B5 %4,%3,%0\;ldil L'%1,%0
   com%I4clr,%B5 %4,%3,%0\;zdepi %Z1,%0"
  [(set_attr "type" "multi,multi,multi,multi,nullshift")
   (set_attr "length" "8,8,8,8,8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r,r,r,r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
	    [(match_operand:SI 3 "register_operand" "r,r,r,r,r,r,r,r")
	     (match_operand:SI 4 "arith11_operand" "rI,rI,rI,rI,rI,rI,rI,rI")])
	 (match_operand:SI 1 "reg_or_cint_move_operand" "0,0,0,0,r,J,N,K")
	 (match_operand:SI 2 "reg_or_cint_move_operand" "r,J,N,K,0,0,0,0")))]
  ""
  "@
   com%I4clr,%S5 %4,%3,0\;copy %2,%0
   com%I4clr,%S5 %4,%3,0\;ldi %2,%0
   com%I4clr,%S5 %4,%3,0\;ldil L'%2,%0
   com%I4clr,%S5 %4,%3,0\;zdepi %Z2,%0
   com%I4clr,%B5 %4,%3,0\;copy %1,%0
   com%I4clr,%B5 %4,%3,0\;ldi %1,%0
   com%I4clr,%B5 %4,%3,0\;ldil L'%1,%0
   com%I4clr,%B5 %4,%3,0\;zdepi %Z1,%0"
  [(set_attr "type" "multi,multi,multi,nullshift,multi,multi,multi,nullshift")
   (set_attr "length" "8,8,8,8,8,8,8,8")])

;; Conditional Branches

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (EQ, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  /* set up operands from compare.  */
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
  /* fall through and generate default code */
}")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (NE, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (GT, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (LT, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (GE, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "ble"
  [(set (pc)
	(if_then_else (le (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (LE, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

;; Match the branch patterns.


;; Note a long backward conditional branch with an annulled delay slot
;; has a length of 12.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 3 "comparison_operator"
			 [(match_operand:SI 1 "register_operand" "r")
			  (match_operand:SI 2 "arith5_operand" "rL")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  "*
{
  return output_cbranch (operands, INSN_ANNULLED_BRANCH_P (insn),
			 get_attr_length (insn), 0, insn);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 0) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

;; Match the negated branch.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 3 "comparison_operator"
			 [(match_operand:SI 1 "register_operand" "r")
			  (match_operand:SI 2 "arith5_operand" "rL")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  return output_cbranch (operands, INSN_ANNULLED_BRANCH_P (insn),
			 get_attr_length (insn), 1, insn);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 0) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

;; Branch on Bit patterns.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "uint5_operand" ""))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "*
{
  return output_bb (operands, INSN_ANNULLED_BRANCH_P (insn),
			 get_attr_length (insn), 0, insn, 0);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "uint5_operand" ""))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "*
{
  return output_bb (operands, INSN_ANNULLED_BRANCH_P (insn),
			 get_attr_length (insn), 1, insn, 0);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "uint5_operand" ""))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "*
{
  return output_bb (operands, INSN_ANNULLED_BRANCH_P (insn),
			 get_attr_length (insn), 0, insn, 1);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "uint5_operand" ""))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "*
{
  return output_bb (operands, INSN_ANNULLED_BRANCH_P (insn),
			 get_attr_length (insn), 1, insn, 1);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

;; Branch on Variable Bit patterns.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "register_operand" "q"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "*
{
  return output_bvb (operands, INSN_ANNULLED_BRANCH_P (insn),
		     get_attr_length (insn), 0, insn, 0);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "register_operand" "q"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "*
{
  return output_bvb (operands, INSN_ANNULLED_BRANCH_P (insn),
		     get_attr_length (insn), 1, insn, 0);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "register_operand" "q"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "*
{
  return output_bvb (operands, INSN_ANNULLED_BRANCH_P (insn),
		     get_attr_length (insn), 0, insn, 1);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "register_operand" "q"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "*
{
  return output_bvb (operands, INSN_ANNULLED_BRANCH_P (insn),
		     get_attr_length (insn), 1, insn, 1);
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

;; Floating point branches
(define_insn ""
  [(set (pc) (if_then_else (ne (reg:CCFP 0) (const_int 0))
			   (label_ref (match_operand 0 "" ""))
			   (pc)))]
  "! TARGET_SOFT_FLOAT"
  "*
{
  if (INSN_ANNULLED_BRANCH_P (insn))
    return \"ftest\;bl,n %0,0\";
  else
    return \"ftest\;bl%* %0,0\";
}"
  [(set_attr "type" "fbranch")
   (set_attr "length" "8")])

(define_insn ""
  [(set (pc) (if_then_else (ne (reg:CCFP 0) (const_int 0))
			   (pc)
			   (label_ref (match_operand 0 "" ""))))]
  "! TARGET_SOFT_FLOAT"
  "*
{
  if (INSN_ANNULLED_BRANCH_P (insn))
    return \"ftest\;add,tr 0,0,0\;bl,n %0,0\";
  else
    return \"ftest\;add,tr 0,0,0\;bl%* %0,0\";
}"
  [(set_attr "type" "fbranch")
   (set_attr "length" "12")])

;; Move instructions

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, SImode, 0))
    DONE;
}")

;; Reloading an SImode or DImode value requires a scratch register if
;; going in to or out of float point registers.

(define_expand "reload_insi"
  [(set (match_operand:SI 0 "register_operand" "=Z")
	(match_operand:SI 1 "non_hard_reg_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, SImode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

(define_expand "reload_outsi"
  [(set (match_operand:SI 0 "non_hard_reg_operand" "")
	(match_operand:SI 1  "register_operand" "Z"))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, SImode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

;;; pic symbol references

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "symbolic_operand" ""))))]
  "flag_pic && operands[1] == pic_offset_table_rtx"
  "ldw T'%2(%1),%0"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand"
				"=r,r,r,r,r,Q,*q,!f,f,*T")
	(match_operand:SI 1 "move_operand"
				"r,J,N,K,RQ,rM,rM,!fM,*RT,f"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))
   && ! TARGET_SOFT_FLOAT"
  "@
   copy %1,%0
   ldi %1,%0
   ldil L'%1,%0
   zdepi %Z1,%0
   ldw%M1 %1,%0
   stw%M0 %r1,%0
   mtsar %r1
   fcpy,sgl %r1,%0
   fldw%F1 %1,%0
   fstw%F0 %1,%0"
  [(set_attr "type" "move,move,move,shift,load,store,move,fpalu,fpload,fpstore")
   (set_attr "length" "4,4,4,4,4,4,4,4,4,4")])

(define_insn ""
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand"
				"=r,r,r,r,r,Q,*q")
	(match_operand:SI 1 "move_operand"
				"r,J,N,K,RQ,rM,rM"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))
   && TARGET_SOFT_FLOAT"
  "@
   copy %1,%0
   ldi %1,%0
   ldil L'%1,%0
   zdepi %Z1,%0
   ldw%M1 %1,%0
   stw%M0 %r1,%0
   mtsar %r1"
  [(set_attr "type" "move,move,move,move,load,store,move")
   (set_attr "length" "4,4,4,4,4,4,4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "basereg_operand" "r")
			 (match_operand:SI 2 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"ldwx %1(0,%2),%0\";
  else
    return \"ldwx %2(0,%1),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "basereg_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"ldwx %2(0,%1),%0\";
  else
    return \"ldwx %1(0,%2),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

;; Load or store with base-register modification.

(define_insn "pre_ldwm"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "=r")
			 (match_operand:SI 2 "pre_cint_operand" ""))))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
{
  if (INTVAL (operands[2]) < 0)
    return \"ldwm %2(0,%1),%0\";
  return \"ldws,mb %2(0,%1),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn "pre_stwm"
  [(set (mem:SI (plus:SI (match_operand:SI 0 "register_operand" "=r")
			 (match_operand:SI 1 "pre_cint_operand" "")))
	(match_operand:SI 2 "reg_or_0_operand" "rM"))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_dup 1)))]
  ""
  "*
{
  if (INTVAL (operands[1]) < 0)
    return \"stwm %r2,%1(0,%0)\";
  return \"stws,mb %r2,%1(0,%0)\";
}"
  [(set_attr "type" "store")
   (set_attr "length" "4")])

(define_insn "post_ldwm"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (match_operand:SI 1 "register_operand" "=r")))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_operand:SI 2 "post_cint_operand" "")))]
  ""
  "*
{
  if (INTVAL (operands[2]) > 0)
    return \"ldwm %2(0,%1),%0\";
  return \"ldws,ma %2(0,%1),%1\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn "post_stwm"
  [(set (mem:SI (match_operand:SI 0 "register_operand" "=r"))
	(match_operand:SI 1 "reg_or_0_operand" "rM"))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (match_operand:SI 2 "post_cint_operand" "")))]
  ""
  "*
{
  if (INTVAL (operands[2]) > 0)
    return \"stwm %r1,%2(0,%0)\";
  return \"stws,ma %r1,%2(0,%0)\";
}"
  [(set_attr "type" "store")
   (set_attr "length" "4")])

;; For pic
;; Note since this pattern can be created at reload time (via movsi), all
;; the same rules for movsi apply here.  (no new pseudos, no temporaries).
(define_insn "pic_load_label"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(match_operand:SI 1 "pic_label_operand" ""))]
  ""
  "*
{
  rtx label_rtx = gen_label_rtx ();
  rtx xoperands[3];
  extern FILE *asm_out_file;

  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = label_rtx;
  output_asm_insn (\"bl .+8,%0\", xoperands);
  output_asm_insn (\"depi 0,31,2,%0\", xoperands);
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, \"L\",
			     CODE_LABEL_NUMBER (label_rtx));

  /* If we're trying to load the address of a label that happens to be
     close, then we can use a shorter sequence.  */
  if (GET_CODE (operands[1]) == LABEL_REF
      && insn_addresses
      && abs (insn_addresses[INSN_UID (XEXP (operands[1], 0))]
	        - insn_current_address) < 8100)
    {
      /* Prefixing with R% here is wrong, it extracts just 11 bits and is
	 always non-negative.  */
      output_asm_insn (\"ldo %1-%2(%0),%0\", xoperands);
    }
  else
    {
      output_asm_insn (\"addil L%%%1-%2,%0\", xoperands);
      output_asm_insn (\"ldo R%%%1-%2(%0),%0\", xoperands);
    }
  return \"\";
}"
  [(set_attr "type" "multi")
   (set_attr "length" "16")])		; 12 or 16

(define_insn "pic2_highpart"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (match_operand:SI 1 "register_operand" "r")
		 (high:SI (match_operand 2 "" ""))))]
  "symbolic_operand (operands[2], Pmode)
   && ! function_label_operand (operands[2])
   && flag_pic == 2"
  "addil LT'%G2,%1"
  [(set_attr "type" "binary")
   (set_attr "length" "4")])

; We need this to make sure CSE doesn't simplify a memory load with a
; symbolic address, whose content it think it knows.  For PIC, what CSE
; think is the real value will be the address of that value.
(define_insn "pic2_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (lo_sum:SI (match_operand:SI 1 "register_operand" "r")
			   (unspec:SI [(match_operand:SI 2 "symbolic_operand" "")] 0))))]
  ""
  "*
{
  if (flag_pic != 2)
    abort ();
  return \"ldw RT'%G2(%1),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])


;; Always use addil rather than ldil;add sequences.  This allows the
;; HP linker to eliminate the dp relocation if the symbolic operand
;; lives in the TEXT space.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(high:SI (match_operand 1 "" "")))]
  "symbolic_operand (operands[1], Pmode)
   && ! function_label_operand (operands[1])
   && ! read_only_operand (operands[1])
   && ! flag_pic"
  "*
{
  if (TARGET_LONG_LOAD_STORE)
    return \"addil NLR'%H1,%%r27\;ldo N'%H1(%%r1),%%r1\";
  else
    return \"addil LR'%H1,%%r27\";
}"
  [(set_attr "type" "binary")
   (set (attr "length")
      (if_then_else (eq (symbol_ref "TARGET_LONG_LOAD_STORE") (const_int 0))
		    (const_int 4)
		    (const_int 8)))])


;; This is for use in the prologue/epilogue code.  We need it
;; to add large constants to a stack pointer or frame pointer.
;; Because of the additional %r1 pressure, we probably do not
;; want to use this in general code, so make it available
;; only after reload.
(define_insn "add_high_const"
  [(set (match_operand:SI 0 "register_operand" "=!a,*r")
	(plus:SI (match_operand:SI 1 "register_operand" "r,r")
		 (high:SI (match_operand 2 "const_int_operand" ""))))]
  "reload_completed"
  "@
   addil L'%G2,%1
   ldil L'%G2,%0\;addl %0,%1,%0"
  [(set_attr "type" "binary,binary")
   (set_attr "length" "4,8")])

;; For function addresses.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "function_label_operand" "")))]
  "!TARGET_PORTABLE_RUNTIME"
  "ldil LP'%G1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

;; This version is used only for the portable runtime conventions model
;; (it does not use/support plabels)
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "function_label_operand" "")))]
  "TARGET_PORTABLE_RUNTIME"
  "ldil L'%G1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand 1 "" "")))]
  "(!flag_pic || !symbolic_operand (operands[1]), Pmode)
    && !is_function_label_plus_const (operands[1])"
  "*
{
  if (symbolic_operand (operands[1], Pmode))
    return \"ldil LR'%H1,%0\";
  else
    return \"ldil L'%G1,%0\";
}"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

;; lo_sum of a function address.
;;
;; Note since we are not supporting MPE style external calls we can
;; use the short ldil;ldo sequence.  If one wanted to support
;; MPE external calls you would want to generate something like
;; ldil;ldo;extru;ldw;add.  See the HP compiler's output for details.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "function_label_operand" "")))]
  "!TARGET_PORTABLE_RUNTIME"
  "ldo RP'%G2(%1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

;; This version is used only for the portable runtime conventions model
;; (it does not use/support plabels)
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "function_label_operand" "")))]
  "TARGET_PORTABLE_RUNTIME"
  "ldo R'%G2(%1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "immediate_operand" "i")))]
  "!is_function_label_plus_const (operands[2])"
  "*
{
  if (flag_pic && symbolic_operand (operands[2], Pmode))
    abort ();
  else if (symbolic_operand (operands[2], Pmode))
    return \"ldo RR'%G2(%1),%0\";
  else
    return \"ldo R'%G2(%1),%0\";
}"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

;; Now that a symbolic_address plus a constant is broken up early
;; in the compilation phase (for better CSE) we need a special
;; combiner pattern to load the symbolic address plus the constant
;; in only 2 instructions. (For cases where the symbolic address
;; was not a common subexpression.)
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "symbolic_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" ""))]
  "! (flag_pic && pic_label_operand (operands[1], SImode))"
  [(set (match_dup 2) (high:SI (match_dup 1)))
   (set (match_dup 0) (lo_sum:SI (match_dup 2) (match_dup 1)))]
  "")

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, HImode, 0))
    DONE;
}")

(define_insn ""
  [(set (match_operand:HI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,Q,*q,!f")
	(match_operand:HI 1 "move_operand" "r,J,N,K,RQ,rM,rM,!fM"))]
  "register_operand (operands[0], HImode)
   || reg_or_0_operand (operands[1], HImode)"
  "@
   copy %1,%0
   ldi %1,%0
   ldil L'%1,%0
   zdepi %Z1,%0
   ldh%M1 %1,%0
   sth%M0 %r1,%0
   mtsar %r1
   fcpy,sgl %r1,%0"
  [(set_attr "type" "move,move,move,shift,load,store,move,fpalu")
   (set_attr "length" "4,4,4,4,4,4,4,4")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mem:HI (plus:SI (match_operand:SI 1 "basereg_operand" "r")
			 (match_operand:SI 2 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"ldhx %1(0,%2),%0\";
  else
    return \"ldhx %2(0,%1),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mem:HI (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "basereg_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"ldhx %2(0,%1),%0\";
  else
    return \"ldhx %1(0,%2),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

; Now zero extended variants.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (mem:HI
			  (plus:SI
			    (match_operand:SI 1 "basereg_operand" "r")
			    (match_operand:SI 2 "register_operand" "r")))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"ldhx %1(0,%2),%0\";
  else
    return \"ldhx %2(0,%1),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (mem:HI
			  (plus:SI
			     (match_operand:SI 1 "register_operand" "r")
			     (match_operand:SI 2 "basereg_operand" "r")))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"ldhx %2(0,%1),%0\";
  else
    return \"ldhx %1(0,%2),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mem:HI (plus:SI (match_operand:SI 1 "register_operand" "=r")
			 (match_operand:SI 2 "int5_operand" "L"))))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldhs,mb %2(0,%1),%0"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

; And a zero extended variant.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (mem:HI
			  (plus:SI
			    (match_operand:SI 1 "register_operand" "=r")
			    (match_operand:SI 2 "int5_operand" "L")))))
   (set (match_dup 1)
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldhs,mb %2(0,%1),%0"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (mem:HI (plus:SI (match_operand:SI 0 "register_operand" "=r")
			 (match_operand:SI 1 "int5_operand" "L")))
	(match_operand:HI 2 "reg_or_0_operand" "rM"))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_dup 1)))]
  ""
  "sths,mb %r2,%1(0,%0)"
  [(set_attr "type" "store")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(high:HI (match_operand 1 "const_int_operand" "")))]
  ""
  "ldil L'%G1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lo_sum:HI (match_operand:HI 1 "register_operand" "r")
		   (match_operand 2 "const_int_operand" "")))]
  ""
  "ldo R'%G2(%1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, QImode, 0))
    DONE;
}")

(define_insn ""
  [(set (match_operand:QI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,Q,*q,!f")
	(match_operand:QI 1 "move_operand" "r,J,N,K,RQ,rM,rM,!fM"))]
  "register_operand (operands[0], QImode)
   || reg_or_0_operand (operands[1], QImode)"
  "@
   copy %1,%0
   ldi %1,%0
   ldil L'%1,%0
   zdepi %Z1,%0
   ldb%M1 %1,%0
   stb%M0 %r1,%0
   mtsar %r1
   fcpy,sgl %r1,%0"
  [(set_attr "type" "move,move,move,shift,load,store,move,fpalu")
   (set_attr "length" "4,4,4,4,4,4,4,4")])

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(mem:QI (plus:SI (match_operand:SI 1 "basereg_operand" "r")
			 (match_operand:SI 2 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"ldbx %1(0,%2),%0\";
  else
    return \"ldbx %2(0,%1),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(mem:QI (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "basereg_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"ldbx %2(0,%1),%0\";
  else
    return \"ldbx %1(0,%2),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

; Indexed byte load with zero extension to SImode or HImode.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (mem:QI
			  (plus:SI
			    (match_operand:SI 1 "basereg_operand" "r")
			    (match_operand:SI 2 "register_operand" "r")))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"ldbx %1(0,%2),%0\";
  else
    return \"ldbx %2(0,%1),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (mem:QI
			  (plus:SI
			    (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "basereg_operand" "r")))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"ldbx %2(0,%1),%0\";
  else
    return \"ldbx %1(0,%2),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI (mem:QI
			  (plus:SI
			    (match_operand:SI 1 "basereg_operand" "r")
			    (match_operand:SI 2 "register_operand" "r")))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"ldbx %1(0,%2),%0\";
  else
    return \"ldbx %2(0,%1),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI (mem:QI
			  (plus:SI
			    (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "basereg_operand" "r")))))]
  "! TARGET_DISABLE_INDEXING"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"ldbx %2(0,%1),%0\";
  else
    return \"ldbx %1(0,%2),%0\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(mem:QI (plus:SI (match_operand:SI 1 "register_operand" "=r")
			 (match_operand:SI 2 "int5_operand" "L"))))
   (set (match_dup 1) (plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldbs,mb %2(0,%1),%0"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

; Now the same thing with zero extensions.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (mem:QI (plus:SI
				  (match_operand:SI 1 "register_operand" "=r")
				  (match_operand:SI 2 "int5_operand" "L")))))
   (set (match_dup 1) (plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldbs,mb %2(0,%1),%0"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI (mem:QI (plus:SI
				  (match_operand:SI 1 "register_operand" "=r")
				  (match_operand:SI 2 "int5_operand" "L")))))
   (set (match_dup 1) (plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldbs,mb %2(0,%1),%0"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (mem:QI (plus:SI (match_operand:SI 0 "register_operand" "=r")
			 (match_operand:SI 1 "int5_operand" "L")))
	(match_operand:QI 2 "reg_or_0_operand" "rM"))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_dup 1)))]
  ""
  "stbs,mb %r2,%1(0,%0)"
  [(set_attr "type" "store")
   (set_attr "length" "4")])

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it will not successfully combine with anything.
(define_expand "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "" ""))
		   (mem:BLK (match_operand:BLK 1 "" "")))
	      (clobber (match_dup 0))
	      (clobber (match_dup 1))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))
	      (use (match_operand:SI 2 "arith_operand" ""))
	      (use (match_operand:SI 3 "const_int_operand" ""))])]
  ""
  "
{
  int size, align;
  /* HP provides very fast block move library routine for the PA;
     this routine includes:

	4x4 byte at a time block moves,
	1x4 byte at a time with alignment checked at runtime with
	    attempts to align the source and destination as needed
	1x1 byte loop

     With that in mind, here's the heuristics to try and guess when
     the inlined block move will be better than the library block
     move:

	If the size isn't constant, then always use the library routines.

	If the size is large in respect to the known alignment, then use
	the library routines.

	If the size is small in repsect to the known alignment, then open
	code the copy (since that will lead to better scheduling).

        Else use the block move pattern.   */

  /* Undetermined size, use the library routine.  */
  if (GET_CODE (operands[2]) != CONST_INT)
    FAIL;

  size = INTVAL (operands[2]);
  align = INTVAL (operands[3]);
  align = align > 4 ? 4 : align;

  /* If size/alignment > 8 (eg size is large in respect to alignment),
     then use the library routines.  */
  if (size/align > 16)
    FAIL;

  /* This does happen, but not often enough to worry much about.  */
  if (size/align < MOVE_RATIO)
    FAIL;
  
  /* Fall through means we're going to use our block move pattern.  */
  operands[0] = copy_to_mode_reg (SImode, XEXP (operands[0], 0));
  operands[1] = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  operands[4] = gen_reg_rtx (SImode);
  operands[5] = gen_reg_rtx (SImode);
  emit_insn (gen_movstrsi_internal (operands[0], operands[1], operands[4],
				     operands[5], operands[2], operands[3],
				     gen_reg_rtx (SImode)));
  DONE;
}")

;; The operand constraints are written like this to support both compile-time
;; and run-time determined byte count.  If the count is run-time determined,
;; the register with the byte count is clobbered by the copying code, and
;; therefore it is forced to operand 2.  If the count is compile-time
;; determined, we need two scratch registers for the unrolled code.
(define_insn "movstrsi_internal"
  [(set (mem:BLK (match_operand:SI 0 "register_operand" "+r,r"))
	(mem:BLK (match_operand:SI 1 "register_operand" "+r,r")))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (match_operand:SI 2 "register_operand" "=r,r"))	;loop cnt/tmp
   (clobber (match_operand:SI 3 "register_operand" "=&r,&r"))	;item tmp
   (clobber (match_operand:SI 6 "register_operand" "=&r,&r"))	;item tmp2
   (use (match_operand:SI 4 "arith_operand" "J,2"))	 ;byte count
   (use (match_operand:SI 5 "const_int_operand" "n,n"))] ;alignment
  ""
  "* return output_block_move (operands, !which_alternative);"
  [(set_attr "type" "multi,multi")])

;; Floating point move insns

;; This pattern forces (set (reg:DF ...) (const_double ...))
;; to be reloaded by putting the constant into memory when
;; reg is a floating point register.
;;
;; For integer registers we use ldil;ldo to set the appropriate
;; value.
;;
;; This must come before the movdf pattern, and it must be present
;; to handle obscure reloading cases.
(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=?r,f")
	(match_operand:DF 1 "" "?F,m"))]
  "GET_CODE (operands[1]) == CONST_DOUBLE
   && operands[1] != CONST0_RTX (DFmode)
   && ! TARGET_SOFT_FLOAT"
  "* return (which_alternative == 0 ? output_move_double (operands)
				    : \"fldd%F1 %1,%0\");"
  [(set_attr "type" "move,fpload")
   (set_attr "length" "16,4")])

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DFmode, 0))
    DONE;
}")

;; Reloading an SImode or DImode value requires a scratch register if
;; going in to or out of float point registers.

(define_expand "reload_indf"
  [(set (match_operand:DF 0 "register_operand" "=Z")
	(match_operand:DF 1 "non_hard_reg_operand" ""))
   (clobber (match_operand:DF 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, DFmode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

(define_expand "reload_outdf" 
 [(set (match_operand:DF 0 "non_hard_reg_operand" "")
	(match_operand:DF 1  "register_operand" "Z"))
   (clobber (match_operand:DF 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, DFmode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

(define_insn ""
  [(set (match_operand:DF 0 "reg_or_nonsymb_mem_operand"
			  "=f,*r,RQ,?o,?Q,f,*r,*r")
	(match_operand:DF 1 "reg_or_0_or_nonsymb_mem_operand"
			  "fG,*rG,f,*r,*r,RQ,o,Q"))]
  "(register_operand (operands[0], DFmode)
    || reg_or_0_operand (operands[1], DFmode))
   && ! TARGET_SOFT_FLOAT"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1])
      || operands[1] == CONST0_RTX (DFmode))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "fpalu,move,fpstore,store,store,fpload,load,load")
   (set_attr "length" "4,8,4,8,16,4,8,16")])

(define_insn ""
  [(set (match_operand:DF 0 "reg_or_nonsymb_mem_operand"
			  "=r,?o,?Q,r,r")
	(match_operand:DF 1 "reg_or_0_or_nonsymb_mem_operand"
			  "rG,r,r,o,Q"))]
  "(register_operand (operands[0], DFmode)
    || reg_or_0_operand (operands[1], DFmode))
   && TARGET_SOFT_FLOAT"
  "*
{
  return output_move_double (operands);
}"
  [(set_attr "type" "move,store,store,load,load")
   (set_attr "length" "8,8,16,8,16")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(mem:DF (plus:SI (match_operand:SI 1 "basereg_operand" "r")
			 (match_operand:SI 2 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING && ! TARGET_SOFT_FLOAT"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"flddx %1(0,%2),%0\";
  else
    return \"flddx %2(0,%1),%0\";
}"
  [(set_attr "type" "fpload")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(mem:DF (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "basereg_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING && ! TARGET_SOFT_FLOAT"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"flddx %2(0,%1),%0\";
  else
    return \"flddx %1(0,%2),%0\";
}"
  [(set_attr "type" "fpload")
   (set_attr "length" "4")])

(define_insn ""
  [(set (mem:DF (plus:SI (match_operand:SI 1 "basereg_operand" "r")
			 (match_operand:SI 2 "register_operand" "r")))
	(match_operand:DF 0 "register_operand" "fx"))]
  "! TARGET_DISABLE_INDEXING && ! TARGET_SOFT_FLOAT"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"fstdx %0,%1(0,%2)\";
  else
    return \"fstdx %0,%2(0,%1)\";
}"
  [(set_attr "type" "fpstore")
   (set_attr "length" "4")])

(define_insn ""
  [(set (mem:DF (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "basereg_operand" "r")))
	(match_operand:DF 0 "register_operand" "fx"))]
  "! TARGET_DISABLE_INDEXING && ! TARGET_SOFT_FLOAT"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"fstdx %0,%2(0,%1)\";
  else
    return \"fstdx %0,%1(0,%2)\";
}"
  [(set_attr "type" "fpstore")
   (set_attr "length" "4")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode, 0))
    DONE;
}")

(define_expand "reload_indi"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(match_operand:DI 1 "non_hard_reg_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

(define_expand "reload_outdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "register_operand" "f"))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(high:DI (match_operand 1 "" "")))]
  ""
  "*
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];

  if (GET_CODE (op1) == CONST_INT)
    {
      operands[0] = operand_subword (op0, 1, 0, DImode);
      output_asm_insn (\"ldil L'%1,%0\", operands);

      operands[0] = operand_subword (op0, 0, 0, DImode);
      if (INTVAL (op1) < 0)
	output_asm_insn (\"ldi -1,%0\", operands);
      else
	output_asm_insn (\"ldi 0,%0\", operands);
      return \"\";
    }
  else if (GET_CODE (op1) == CONST_DOUBLE)
    {
      operands[0] = operand_subword (op0, 1, 0, DImode);
      operands[1] = GEN_INT (CONST_DOUBLE_LOW (op1));
      output_asm_insn (\"ldil L'%1,%0\", operands);

      operands[0] = operand_subword (op0, 0, 0, DImode);
      operands[1] = GEN_INT (CONST_DOUBLE_HIGH (op1));
      output_asm_insn (singlemove_string (operands), operands);
      return \"\";
    }
  else
    abort ();
}"
  [(set_attr "type" "move")
   (set_attr "length" "8")])

;;; Experimental

(define_insn ""
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand"
			  "=r,o,Q,r,r,r,f,f,*T")
	(match_operand:DI 1 "general_operand"
			  "rM,r,r,o,Q,i,fM,*T,f"))]
  "(register_operand (operands[0], DImode)
    || reg_or_0_operand (operands[1], DImode))
   && ! TARGET_SOFT_FLOAT"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1])
      || (operands[1] == CONST0_RTX (DImode)))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "move,store,store,load,load,multi,fpalu,fpload,fpstore")
   (set_attr "length" "8,8,16,8,16,16,4,4,4")])

(define_insn ""
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand"
			  "=r,o,Q,r,r,r")
	(match_operand:DI 1 "general_operand"
			  "rM,r,r,o,Q,i"))]
  "(register_operand (operands[0], DImode)
    || reg_or_0_operand (operands[1], DImode))
   && TARGET_SOFT_FLOAT"
  "*
{
  return output_move_double (operands);
}"
  [(set_attr "type" "move,store,store,load,load,multi")
   (set_attr "length" "8,8,16,8,16,16")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,&r")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "0,r")
		   (match_operand:DI 2 "immediate_operand" "i,i")))]
  ""
  "*
{
  /* Don't output a 64 bit constant, since we can't trust the assembler to
     handle it correctly.  */
  if (GET_CODE (operands[2]) == CONST_DOUBLE)
    operands[2] = GEN_INT (CONST_DOUBLE_LOW (operands[2]));
  if (which_alternative == 1)
    output_asm_insn (\"copy %1,%0\", operands);
  return \"ldo R'%G2(%R1),%R0\";
}"
  [(set_attr "type" "move,move")
   (set_attr "length" "4,8")])

;; This pattern forces (set (reg:SF ...) (const_double ...))
;; to be reloaded by putting the constant into memory when
;; reg is a floating point register.
;;
;; For integer registers we use ldil;ldo to set the appropriate
;; value.
;;
;; This must come before the movsf pattern, and it must be present
;; to handle obscure reloading cases.
(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=?r,f")
	(match_operand:SF 1 "" "?F,m"))]
  "GET_CODE (operands[1]) == CONST_DOUBLE
   && operands[1] != CONST0_RTX (SFmode)
   && ! TARGET_SOFT_FLOAT"
  "* return (which_alternative == 0 ? singlemove_string (operands)
				    : \" fldw%F1 %1,%0\");"
  [(set_attr "type" "move,fpload")
   (set_attr "length" "8,4")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, SFmode, 0))
    DONE;
}")

;; Reloading an SImode or DImode value requires a scratch register if
;; going in to or out of float point registers.

(define_expand "reload_insf"
  [(set (match_operand:SF 0 "register_operand" "=Z")
	(match_operand:SF 1 "non_hard_reg_operand" ""))
   (clobber (match_operand:SF 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, SFmode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

(define_expand "reload_outsf"
  [(set (match_operand:SF 0 "non_hard_reg_operand" "")
	(match_operand:SF 1  "register_operand" "Z"))
   (clobber (match_operand:SF 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, SFmode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

(define_insn ""
  [(set (match_operand:SF 0 "reg_or_nonsymb_mem_operand"
			  "=f,r,f,r,RQ,Q")
	(match_operand:SF 1 "reg_or_0_or_nonsymb_mem_operand"
			  "fG,rG,RQ,Q,f,rG"))]
  "(register_operand (operands[0], SFmode)
    || reg_or_0_operand (operands[1], SFmode))
   && ! TARGET_SOFT_FLOAT"
  "@
   fcpy,sgl %r1,%0
   copy %r1,%0
   fldw%F1 %1,%0
   ldw%M1 %1,%0
   fstw%F0 %r1,%0
   stw%M0 %r1,%0"
  [(set_attr "type" "fpalu,move,fpload,load,fpstore,store")
   (set_attr "length" "4,4,4,4,4,4")])

(define_insn ""
  [(set (match_operand:SF 0 "reg_or_nonsymb_mem_operand"
			  "=r,r,Q")
	(match_operand:SF 1 "reg_or_0_or_nonsymb_mem_operand"
			  "rG,Q,rG"))]
  "(register_operand (operands[0], SFmode)
    || reg_or_0_operand (operands[1], SFmode))
   && TARGET_SOFT_FLOAT"
  "@
   copy %r1,%0
   ldw%M1 %1,%0
   stw%M0 %r1,%0"
  [(set_attr "type" "move,load,store")
   (set_attr "length" "4,4,4")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(mem:SF (plus:SI (match_operand:SI 1 "basereg_operand" "r")
			 (match_operand:SI 2 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING && ! TARGET_SOFT_FLOAT"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"fldwx %1(0,%2),%0\";
  else
    return \"fldwx %2(0,%1),%0\";
}"
  [(set_attr "type" "fpload")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(mem:SF (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "basereg_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING && ! TARGET_SOFT_FLOAT"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"fldwx %2(0,%1),%0\";
  else
    return \"fldwx %1(0,%2),%0\";
}"
  [(set_attr "type" "fpload")
   (set_attr "length" "4")])

(define_insn ""
  [(set (mem:SF (plus:SI (match_operand:SI 1 "basereg_operand" "r")
			 (match_operand:SI 2 "register_operand" "r")))
      (match_operand:SF 0 "register_operand" "fx"))]
  "! TARGET_DISABLE_INDEXING && ! TARGET_SOFT_FLOAT"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[2] == hard_frame_pointer_rtx
      || operands[2] == stack_pointer_rtx)
    return \"fstwx %0,%1(0,%2)\";
  else
    return \"fstwx %0,%2(0,%1)\";
}"
  [(set_attr "type" "fpstore")
   (set_attr "length" "4")])

(define_insn ""
  [(set (mem:SF (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "basereg_operand" "r")))
      (match_operand:SF 0 "register_operand" "fx"))]
  "! TARGET_DISABLE_INDEXING && ! TARGET_SOFT_FLOAT"
  "*
{
  /* Reload can create backwards (relative to cse) unscaled index
     address modes when eliminating registers and possibly for
     pseudos that don't get hard registers.  Deal with it.  */
  if (operands[1] == hard_frame_pointer_rtx
      || operands[1] == stack_pointer_rtx)
    return \"fstwx %0,%2(0,%1)\";
  else
    return \"fstwx %0,%1(0,%2)\";
}"
  [(set_attr "type" "fpstore")
   (set_attr "length" "4")])


;;- zero extension instructions

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:HI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,16,%0
   ldh%M1 %1,%0"
  [(set_attr "type" "shift,load")
   (set_attr "length" "4,4")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI
	 (match_operand:QI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,8,%0
   ldb%M1 %1,%0"
  [(set_attr "type" "shift,load")
   (set_attr "length" "4,4")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:QI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,8,%0
   ldb%M1 %1,%0"
  [(set_attr "type" "shift,load")
   (set_attr "length" "4,4")])

;;- sign extension instructions

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "extrs %1,31,16,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "extrs %1,31,8,%0"
  [(set_attr "type" "shift") 
  (set_attr "length" "4")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "extrs %1,31,8,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

;; Conversions between float and double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF
	 (match_operand:SF 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fcnvff,sgl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fcnvff,dbl,sgl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

;; Conversion between fixed point and floating point.
;; Note that among the fix-to-float insns
;; the ones that start with SImode come first.
;; That is so that an operand that is a CONST_INT
;; (and therefore lacks a specific machine mode).
;; will be recognized as SImode (which is always valid)
;; rather than as QImode or HImode.

;; This pattern forces (set (reg:SF ...) (float:SF (const_int ...)))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general floatsisf2 pattern.
(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "const_int_operand" "m")))]
  "! TARGET_SOFT_FLOAT"
  "fldw%F1 %1,%0\;fcnvxf,sgl,sgl %0,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "8")])

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fcnvxf,sgl,sgl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

;; This pattern forces (set (reg:DF ...) (float:DF (const_int ...)))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general floatsidf2 pattern.
(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "const_int_operand" "m")))]
  "! TARGET_SOFT_FLOAT"
  "fldw%F1 %1,%0\;fcnvxf,sgl,dbl %0,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "8")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fcnvxf,sgl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_expand "floatunssisf2"
  [(set (subreg:SI (match_dup 2) 1)
	(match_operand:SI 1 "register_operand" ""))
   (set (subreg:SI (match_dup 2) 0)
	(const_int 0))
   (set (match_operand:SF 0 "register_operand" "")
	(float:SF (match_dup 2)))]
  "TARGET_SNAKE && ! TARGET_SOFT_FLOAT"
  "operands[2] = gen_reg_rtx (DImode);")

(define_expand "floatunssidf2"
  [(set (subreg:SI (match_dup 2) 1)
	(match_operand:SI 1 "register_operand" ""))
   (set (subreg:SI (match_dup 2) 0)
	(const_int 0))
   (set (match_operand:DF 0 "register_operand" "")
	(float:DF (match_dup 2)))]
  "TARGET_SNAKE && ! TARGET_SOFT_FLOAT"
  "operands[2] = gen_reg_rtx (DImode);")

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_SNAKE && ! TARGET_SOFT_FLOAT"
  "fcnvxf,dbl,sgl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:DI 1 "register_operand" "f")))]
  "TARGET_SNAKE && ! TARGET_SOFT_FLOAT"
  "fcnvxf,dbl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  "! TARGET_SOFT_FLOAT"
  "fcnvfxt,sgl,sgl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "f"))))]
  "! TARGET_SOFT_FLOAT"
  "fcnvfxt,dbl,sgl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  "TARGET_SNAKE && ! TARGET_SOFT_FLOAT"
  "fcnvfxt,sgl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(fix:DI (fix:DF (match_operand:DF 1 "register_operand" "f"))))]
  "TARGET_SNAKE && ! TARGET_SOFT_FLOAT"
  "fcnvfxt,dbl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

;;- arithmetic instructions

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "register_operand" "%r")
		 (match_operand:DI 2 "arith11_operand" "rI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL (operands[2]) >= 0)
	return \"addi %2,%R1,%R0\;addc %1,0,%0\";
      else
	return \"addi %2,%R1,%R0\;subb %1,0,%0\";
    }
  else
    return \"add %R2,%R1,%R0\;addc %2,%1,%0\";
}"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:SI 2 "register_operand" "r")))]
  ""
  "uaddcm %2,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "4")])

;; define_splits to optimize cases of adding a constant integer
;; to a register when the constant does not fit in 14 bits.  */
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "const_int_operand" "")))
   (clobber (match_operand:SI 4 "register_operand" ""))]
  "! cint_ok_for_move (INTVAL (operands[2]))
   && VAL_14_BITS_P (INTVAL (operands[2]) >> 1)"
  [(set (match_dup 4) (plus:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (plus:SI (match_dup 4) (match_dup 3)))]
  "
{
  int val = INTVAL (operands[2]);
  int low = (val < 0) ? -0x2000 : 0x1fff;
  int rest = val - low;

  operands[2] = GEN_INT (rest);
  operands[3] = GEN_INT (low);
}")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "const_int_operand" "")))
   (clobber (match_operand:SI 4 "register_operand" ""))]
  "! cint_ok_for_move (INTVAL (operands[2]))"
  [(set (match_dup 4) (match_dup 2))
   (set (match_dup 0) (plus:SI (mult:SI (match_dup 4) (match_dup 3))
			       (match_dup 1)))]
  "
{
  HOST_WIDE_INT intval = INTVAL (operands[2]);

  /* Try dividing the constant by 2, then 4, and finally 8 to see
     if we can get a constant which can be loaded into a register
     in a single instruction (cint_ok_for_move). 

     If that fails, try to negate the constant and subtract it
     from our input operand.  */
  if (intval % 2 == 0 && cint_ok_for_move (intval / 2))
    {
      operands[2] = GEN_INT (intval / 2);
      operands[3] = GEN_INT (2);
    }
  else if (intval % 4 == 0 && cint_ok_for_move (intval / 4))
    {
      operands[2] = GEN_INT (intval / 4);
      operands[3] = GEN_INT (4);
    }
  else if (intval % 8 == 0 && cint_ok_for_move (intval / 8))
    {
      operands[2] = GEN_INT (intval / 8);
      operands[3] = GEN_INT (8);
    }
  else if (cint_ok_for_move (-intval))
    {
      emit_insn (gen_rtx (SET, VOIDmode, operands[4], GEN_INT (-intval)));
      emit_insn (gen_subsi3 (operands[0], operands[1], operands[4]));
      DONE;
    }
  else
    FAIL;
}")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%r,r")
		 (match_operand:SI 2 "arith_operand" "r,J")))]
  ""
  "@
   addl %1,%2,%0
   ldo %2(%1),%0"
  [(set_attr "type" "binary,binary")
   (set_attr "length" "4,4")])

;; Disgusting kludge to work around reload bugs with frame pointer
;; elimination.  Similar to other magic reload patterns in the
;; indexed memory operations.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI (plus:SI (match_operand:SI 1 "register_operand" "%r")
			  (match_operand:SI 2 "register_operand" "r"))
		 (match_operand:SI 3 "const_int_operand" "rL")))]
  "reload_in_progress"
  "*
{
  if (GET_CODE (operands[3]) == CONST_INT)
    return \"ldo %3(%2),%0\;addl %1,%0,%0\";
  else
    return \"addl %3,%2,%0\;addl %1,%0,%0\";
}"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "register_operand" "r")))]
  ""
  "sub %R1,%R2,%R0\;subb %1,%2,%0"
  [(set_attr "type" "binary")
  (set_attr "length" "8")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "arith11_operand" "r,I")
		  (match_operand:SI 2 "register_operand" "r,r")))]
  ""
  "@
   sub %1,%2,%0
   subi %1,%2,%0"
  [(set_attr "type" "binary,binary")
   (set_attr "length" "4,4")])

;; Clobbering a "register_operand" instead of a match_scratch
;; in operand3 of millicode calls avoids spilling %r1 and
;; produces better code.

;; The mulsi3 insns set up registers for the millicode call.
(define_expand "mulsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (mult:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_dup 3))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  if (TARGET_SNAKE && ! TARGET_DISABLE_FPREGS && ! TARGET_SOFT_FLOAT)
    {
      rtx scratch = gen_reg_rtx (DImode);
      operands[1] = force_reg (SImode, operands[1]);
      operands[2] = force_reg (SImode, operands[2]);
      emit_insn (gen_umulsidi3 (scratch, operands[1], operands[2]));
      emit_insn (gen_rtx (SET, VOIDmode,
			  operands[0],
			  gen_rtx (SUBREG, SImode, scratch, 1)));
      DONE;
    }
  operands[3] = gen_reg_rtx (SImode);
}")

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=f")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "f"))
		 (zero_extend:DI (match_operand:SI 2 "nonimmediate_operand" "f"))))]
  "TARGET_SNAKE && ! TARGET_DISABLE_FPREGS && ! TARGET_SOFT_FLOAT"
  "xmpyu %1,%2,%0"
  [(set_attr "type" "fpmuldbl")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:DI 0 "nonimmediate_operand" "=f")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "f"))
		 (match_operand:DI 2 "uint32_operand" "f")))]
  "TARGET_SNAKE && ! TARGET_DISABLE_FPREGS && ! TARGET_SOFT_FLOAT"
  "xmpyu %1,%R2,%0"
  [(set_attr "type" "fpmuldbl")
   (set_attr "length" "4")])

(define_insn ""
  [(set (reg:SI 29) (mult:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_operand:SI 0 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "* return output_mul_insn (0, insn);"
  [(set_attr "type" "milli")
   (set (attr "length")
     (cond [
;; Target (or stub) within reach
            (and (lt (plus (symbol_ref "total_code_bytes") (pc))
                     (const_int 240000))
                 (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0)))
            (const_int 4)

;; NO_SPACE_REGS
            (ne (symbol_ref "TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS")
                (const_int 0))
            (const_int 8)

;; Out of reach, but not PIC or PORTABLE_RUNTIME
;; same as NO_SPACE_REGS code
            (and (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0))
                 (eq (symbol_ref "flag_pic")
                     (const_int 0)))
            (const_int 8)]

;; Out of range and either PIC or PORTABLE_RUNTIME
	  (const_int 24)))])

;;; Division and mod.
(define_expand "divsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (div:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_dup 3))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  operands[3] = gen_reg_rtx (SImode);
  if (GET_CODE (operands[2]) == CONST_INT && emit_hpdiv_const (operands, 0))
    DONE;
}")

(define_insn ""
  [(set (reg:SI 29)
	(div:SI (reg:SI 26) (match_operand:SI 0 "div_operand" "")))
   (clobber (match_operand:SI 1 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "*
   return output_div_insn (operands, 0, insn);"
  [(set_attr "type" "milli")
   (set (attr "length")
     (cond [
;; Target (or stub) within reach
            (and (lt (plus (symbol_ref "total_code_bytes") (pc))
                     (const_int 240000))
                 (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0)))
            (const_int 4)

;; NO_SPACE_REGS
            (ne (symbol_ref "TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS")
                (const_int 0))
            (const_int 8)

;; Out of reach, but not PIC or PORTABLE_RUNTIME
;; same as NO_SPACE_REGS code
            (and (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0))
                 (eq (symbol_ref "flag_pic")
                     (const_int 0)))
            (const_int 8)]

;; Out of range and either PIC or PORTABLE_RUNTIME
	  (const_int 24)))])

(define_expand "udivsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (udiv:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_dup 3))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  operands[3] = gen_reg_rtx (SImode);
  if (GET_CODE (operands[2]) == CONST_INT && emit_hpdiv_const (operands, 1))
    DONE;
}")

(define_insn ""
  [(set (reg:SI 29)
	(udiv:SI (reg:SI 26) (match_operand:SI 0 "div_operand" "")))
   (clobber (match_operand:SI 1 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "*
   return output_div_insn (operands, 1, insn);"
  [(set_attr "type" "milli")
   (set (attr "length")
     (cond [
;; Target (or stub) within reach
            (and (lt (plus (symbol_ref "total_code_bytes") (pc))
                     (const_int 240000))
                 (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0)))
            (const_int 4)

;; NO_SPACE_REGS
            (ne (symbol_ref "TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS")
                (const_int 0))
            (const_int 8)

;; Out of reach, but not PIC or PORTABLE_RUNTIME
;; same as NO_SPACE_REGS code
            (and (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0))
                 (eq (symbol_ref "flag_pic")
                     (const_int 0)))
            (const_int 8)]

;; Out of range and either PIC or PORTABLE_RUNTIME
	  (const_int 24)))])

(define_expand "modsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (mod:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_dup 3))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  operands[3] = gen_reg_rtx (SImode);
}")

(define_insn ""
  [(set (reg:SI 29) (mod:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_operand:SI 0 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "*
  return output_mod_insn (0, insn);"
  [(set_attr "type" "milli")
   (set (attr "length")
     (cond [
;; Target (or stub) within reach
            (and (lt (plus (symbol_ref "total_code_bytes") (pc))
                     (const_int 240000))
                 (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0)))
            (const_int 4)

;; NO_SPACE_REGS
            (ne (symbol_ref "TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS")
                (const_int 0))
            (const_int 8)

;; Out of reach, but not PIC or PORTABLE_RUNTIME
;; same as NO_SPACE_REGS code
            (and (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0))
                 (eq (symbol_ref "flag_pic")
                     (const_int 0)))
            (const_int 8)]

;; Out of range and either PIC or PORTABLE_RUNTIME
	  (const_int 24)))])

(define_expand "umodsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (umod:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_dup 3))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  operands[3] = gen_reg_rtx (SImode);
}")

(define_insn ""
  [(set (reg:SI 29) (umod:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_operand:SI 0 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "*
  return output_mod_insn (1, insn);"
  [(set_attr "type" "milli")
   (set (attr "length")
     (cond [
;; Target (or stub) within reach
            (and (lt (plus (symbol_ref "total_code_bytes") (pc))
                     (const_int 240000))
                 (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0)))
            (const_int 4)

;; NO_SPACE_REGS
            (ne (symbol_ref "TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS")
                (const_int 0))
            (const_int 8)

;; Out of reach, but not PIC or PORTABLE_RUNTIME
;; same as NO_SPACE_REGS code
            (and (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0))
                 (eq (symbol_ref "flag_pic")
                     (const_int 0)))
            (const_int 8)]

;; Out of range and either PIC or PORTABLE_RUNTIME
	  (const_int 24)))])

;;- and instructions
;; We define DImode `and` so with DImode `not` we can get
;; DImode `andn`.  Other combinations are possible.

(define_expand "anddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "
{
  if (! register_operand (operands[1], DImode)
      || ! register_operand (operands[2], DImode))
    /* Let GCC break this into word-at-a-time operations.  */
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "register_operand" "%r")
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "and %1,%2,%0\;and %R1,%R2,%R0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

; The ? for op1 makes reload prefer zdepi instead of loading a huge
; constant with ldil;ldo.
(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "register_operand" "%?r,0")
		(match_operand:SI 2 "and_operand" "rO,P")))]
  ""
  "* return output_and (operands); "
  [(set_attr "type" "binary,shift")
   (set_attr "length" "4,4")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "andcm %2,%1,%0\;andcm %R2,%R1,%R0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "andcm %2,%1,%0"
  [(set_attr "type" "binary")
  (set_attr "length" "4")])

(define_expand "iordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(ior:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "
{
  if (! register_operand (operands[1], DImode)
      || ! register_operand (operands[2], DImode))
    /* Let GCC break this into word-at-a-time operations.  */
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (match_operand:DI 1 "register_operand" "%r")
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "or %1,%2,%0\;or %R1,%R2,%R0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

;; Need a define_expand because we've run out of CONST_OK... characters.
(define_expand "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "arith32_operand" "")))]
  ""
  "
{
  if (! (ior_operand (operands[2]) || register_operand (operands[2])))
    operands[2] = force_reg (SImode, operands[2]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "ior_operand" "M,i")))]
  ""
  "* return output_ior (operands); "
  [(set_attr "type" "binary,shift")
   (set_attr "length" "4,4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "%r")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "or %1,%2,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "4")])

(define_expand "xordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(xor:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "
{
  if (! register_operand (operands[1], DImode)
      || ! register_operand (operands[2], DImode))
    /* Let GCC break this into word-at-a-time operations.  */
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(xor:DI (match_operand:DI 1 "register_operand" "%r")
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "xor %1,%2,%0\;xor %R1,%R2,%R0"
  [(set_attr "type" "binary")
   (set_attr "length" "8")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "register_operand" "%r")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "xor %1,%2,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "4")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "sub 0,%R1,%R0\;subb 0,%1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "8")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sub 0,%1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "4")])

(define_expand "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(not:DI (match_operand:DI 1 "arith_double_operand" "")))]
  ""
  "
{
  if (! register_operand (operands[1], DImode))
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "uaddcm 0,%1,%0\;uaddcm 0,%R1,%R0"
  [(set_attr "type" "unary")
   (set_attr "length" "8")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "uaddcm 0,%1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "4")])

;; Floating point arithmetic instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fadd,dbl %1,%2,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fadd,sgl %1,%2,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "register_operand" "f")
		  (match_operand:DF 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fsub,dbl %1,%2,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fsub,sgl %1,%2,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fmpy,dbl %1,%2,%0"
  [(set_attr "type" "fpmuldbl")
   (set_attr "length" "4")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fmpy,sgl %1,%2,%0"
  [(set_attr "type" "fpmulsgl")
   (set_attr "length" "4")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "register_operand" "f")
		(match_operand:DF 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fdiv,dbl %1,%2,%0"
  [(set_attr "type" "fpdivdbl")
   (set_attr "length" "4")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fdiv,sgl %1,%2,%0"
  [(set_attr "type" "fpdivsgl")
   (set_attr "length" "4")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fsub,dbl 0,%1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fsub,sgl 0,%1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fabs,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fabs,sgl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fsqrt,dbl %1,%0"
  [(set_attr "type" "fpsqrtdbl")
   (set_attr "length" "4")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT"
  "fsqrt,sgl %1,%0"
  [(set_attr "type" "fpsqrtsgl")
   (set_attr "length" "4")])

;;- Shift instructions

;; Optimized special case of shifting.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (const_int 24)))]
  ""
  "ldb%M1 %1,%0"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (const_int 16)))]
  ""
  "ldh%M1 %1,%0"
  [(set_attr "type" "load")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r")
			  (match_operand:SI 3 "shadd_operand" ""))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sh%O3addl %2,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "4")])

;; This variant of the above insn can occur if the first operand
;; is the frame pointer.  This is a kludge, but there doesn't
;; seem to be a way around it.  Only recognize it while reloading.
;; Note how operand 3 uses a predicate of "const_int_operand", but 
;; has constraints allowing a register.  I don't know how this works,
;; but it somehow makes sure that out-of-range constants are placed
;; in a register which somehow magically is a "const_int_operand".
;; (this was stolen from alpha.md, I'm not going to try and change it.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=&r,r")
	(plus:SI (plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r,r")
				   (match_operand:SI 4 "shadd_operand" ""))
			  (match_operand:SI 1 "register_operand" "r,r"))
		 (match_operand:SI 3 "const_int_operand" "r,J")))]
  "reload_in_progress"
  "@
   sh%O4addl %2,%1,%0\;addl %3,%0,%0
   sh%O4addl %2,%1,%0\;ldo %3(%0),%0"
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_operand:SI 1 "lhs_lshift_operand" "")
		   (match_operand:SI 2 "arith32_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      rtx temp = gen_reg_rtx (SImode);
      emit_insn (gen_subsi3 (temp, GEN_INT (31), operands[2]));
      if (GET_CODE (operands[1]) == CONST_INT)
	emit_insn (gen_zvdep_imm (operands[0], operands[1], temp));
      else
	emit_insn (gen_zvdep32 (operands[0], operands[1], temp));
      DONE;
    }
  /* Make sure both inputs are not constants,
     there are no patterns for that.  */
  operands[1] = force_reg (SImode, operands[1]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "zdep %1,%P2,%L2,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

; Match cases of op1 a CONST_INT here that zvdep_imm doesn't handle.
; Doing it like this makes slightly better code since reload can
; replace a register with a known value in range -16..15 with a
; constant.  Ideally, we would like to merge zvdep32 and zvdep_imm,
; but since we have no more CONST_OK... characters, that is not
; possible.
(define_insn "zvdep32"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "arith5_operand" "r,L")
		   (minus:SI (const_int 31)
			     (match_operand:SI 2 "register_operand" "q,q"))))]
  ""
  "@
   zvdep %1,32,%0
   zvdepi %1,32,%0"
  [(set_attr "type" "shift,shift")
   (set_attr "length" "4,4")])

(define_insn "zvdep_imm"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "lhs_lshift_cint_operand" "")
		   (minus:SI (const_int 31)
			     (match_operand:SI 2 "register_operand" "q"))))]
  ""
  "*
{
  int x = INTVAL (operands[1]);
  operands[2] = GEN_INT (4 + exact_log2 ((x >> 4) + 1));
  operands[1] = GEN_INT ((x & 0xf) - 0x10);
  return \"zvdepi %1,%2,%0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "vdepi_ior"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (ashift:SI (match_operand:SI 1 "const_int_operand" "")
			   (minus:SI (const_int 31)
				     (match_operand:SI 2 "register_operand" "q")))
		(match_operand:SI 3 "register_operand" "0")))]
  ; accept ...0001...1, can this be generalized?
  "exact_log2 (INTVAL (operands[1]) + 1) >= 0"
  "*
{
  int x = INTVAL (operands[1]);
  operands[2] = GEN_INT (exact_log2 (x + 1));
  return \"vdepi -1,%2,%0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "vdepi_and"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (rotate:SI (match_operand:SI 1 "const_int_operand" "")
			   (minus:SI (const_int 31)
				     (match_operand:SI 2 "register_operand" "q")))
		(match_operand:SI 3 "register_operand" "0")))]
  ; this can be generalized...!
  "INTVAL (operands[1]) == -2"
  "*
{
  int x = INTVAL (operands[1]);
  operands[2] = GEN_INT (exact_log2 ((~x) + 1));
  return \"vdepi 0,%2,%0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "arith32_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      rtx temp = gen_reg_rtx (SImode);
      emit_insn (gen_subsi3 (temp, GEN_INT (31), operands[2]));
      emit_insn (gen_vextrs32 (operands[0], operands[1], temp));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "extrs %1,%P2,%L2,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "vextrs32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (minus:SI (const_int 31)
			       (match_operand:SI 2 "register_operand" "q"))))]
  ""
  "vextrs %1,32,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "arith32_operand" "q,n")))]
  ""
  "@
   vshd 0,%1,%0
   extru %1,%P2,%L2,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(rotatert:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "arith32_operand" "q,n")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      operands[2] = GEN_INT (INTVAL (operands[2]) & 31);
      return \"shd %1,%1,%2,%0\";
    }
  else
    return \"vshd %1,%1,%0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 5 "plus_xor_ior_operator"
	  [(ashift:SI (match_operand:SI 1 "register_operand" "r")
		      (match_operand:SI 3 "const_int_operand" "n"))
	   (lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			(match_operand:SI 4 "const_int_operand" "n"))]))]
  "INTVAL (operands[3]) + INTVAL (operands[4]) == 32"
  "shd %1,%2,%4,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 5 "plus_xor_ior_operator"
	  [(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			(match_operand:SI 4 "const_int_operand" "n"))
	   (ashift:SI (match_operand:SI 1 "register_operand" "r")
		      (match_operand:SI 3 "const_int_operand" "n"))]))]
  "INTVAL (operands[3]) + INTVAL (operands[4]) == 32"
  "shd %1,%2,%4,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			   (match_operand:SI 2 "const_int_operand" ""))
		(match_operand:SI 3 "const_int_operand" "")))]
  "exact_log2 (1 + (INTVAL (operands[3]) >> (INTVAL (operands[2]) & 31))) >= 0"
  "*
{
  int cnt = INTVAL (operands[2]) & 31;
  operands[3] = GEN_INT (exact_log2 (1 + (INTVAL (operands[3]) >> cnt)));
  operands[2] = GEN_INT (31 - cnt);
  return \"zdep %1,%2,%3,%0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

;; Unconditional and other jump instructions.

(define_insn "return"
  [(return)]
  "hppa_can_use_return_insn_p ()"
  "bv%* 0(%%r2)"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

;; Use a different pattern for functions which have non-trivial
;; epilogues so as not to confuse jump and reorg.
(define_insn "return_internal"
  [(use (reg:SI 2))
   (return)]
  ""
  "bv%* 0(%%r2)"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_expand "prologue"
  [(const_int 0)]
  ""
  "hppa_expand_prologue ();DONE;")

(define_expand "epilogue"
  [(return)]
  ""
  "
{
  /* Try to use the trivial return first.  Else use the full
     epilogue.  */
  if (hppa_can_use_return_insn_p ())
   emit_jump_insn (gen_return ());
  else
    {
      hppa_expand_epilogue ();
      emit_jump_insn (gen_return_internal ());
    }
  DONE;
}")

;; Special because we use the value placed in %r2 by the bl instruction
;; from within its delay slot to set the value for the 2nd parameter to
;; the call.
(define_insn "call_profiler"
  [(unspec_volatile [(const_int 0)] 0)
   (use (match_operand:SI 0 "const_int_operand" ""))]
  ""
  "bl _mcount,%%r2\;ldo %0(%%r2),%%r25"
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

(define_insn "blockage"
  [(unspec_volatile [(const_int 2)] 0)]
  ""
  ""
  [(set_attr "length" "0")])

(define_insn "switch_jump"
  [(set:DI (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "bl %l0,0%#"
  [(set_attr "type" "uncond_branch")
   (set_attr "length" "4")])

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "bl%* %l0,0"
  [(set_attr "type" "uncond_branch")
   (set (attr "length")
    (cond [(eq (symbol_ref "jump_in_call_delay (insn)") (const_int 0))
	   (const_int 4)
;; If the jump is in the delay slot of a call, then its length depends
;; on whether or not we can add the proper offset to %r2 with an ldo
;; instruction.
	   (lt (abs (minus (match_dup 0) (plus (pc) (const_int 8))))
		    (const_int 8184))
           (const_int 4)]
	  (const_int 8)))])

;; Subroutines of "casesi".
;; operand 0 is index
;; operand 1 is the minimum bound
;; operand 2 is the maximum bound - minimum bound + 1
;; operand 3 is CODE_LABEL for the table;
;; operand 4 is the CODE_LABEL to go to if index out of range.

(define_expand "casesi"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "const_int_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand 3 "" "")
   (match_operand 4 "" "")]
  ""
  "
{
  if (GET_CODE (operands[0]) != REG)
    operands[0] = force_reg (SImode, operands[0]);

  if (operands[1] != const0_rtx)
    {
      rtx reg = gen_reg_rtx (SImode);

      operands[1] = GEN_INT (-INTVAL (operands[1]));
      if (!INT_14_BITS (operands[1]))
	operands[1] = force_reg (SImode, operands[1]);
      emit_insn (gen_addsi3 (reg, operands[0], operands[1]));

      operands[0] = reg;
    }

  if (!INT_11_BITS (operands[2]))
    operands[2] = force_reg (SImode, operands[2]);

  emit_jump_insn (gen_casesi0 (operands[0], operands[2],
			       operands[3], operands[4]));
  DONE;
}")

(define_insn "casesi0"
  [(set (pc)
	(if_then_else (leu (match_operand:SI 0 "register_operand" "r")
			   (match_operand:SI 1 "arith11_operand" "rI"))
		      (plus:SI (mem:SI (plus:SI (pc) (match_dup 0)))
			       (label_ref (match_operand 2 "" "")))
		      (pc)))
   (use (label_ref (match_operand 3 "" "")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      operands[1] = GEN_INT (~INTVAL (operands[1]));
      return \"addi,uv %1,%0,0\;blr,n %0,0\;b,n %l3\";
    }
  else
    {
      return \"sub,>> %0,%1,0\;blr,n %0,0\;b,n %l3\";
    }
}"
  [(set_attr "type" "multi")
   (set_attr "length" "12")])

;; Need nops for the calls because execution is supposed to continue
;; past; we don't want to nullify an instruction that we need.
;;- jump to subroutine

(define_expand "call"
  [(parallel [(call (match_operand:SI 0 "" "")
		    (match_operand 1 "" ""))
	      (clobber (reg:SI 2))])]
  ""
  "
{
  rtx op;
  rtx call_insn;

  if (TARGET_PORTABLE_RUNTIME)
    op = force_reg (SImode, XEXP (operands[0], 0));
  else
    op = XEXP (operands[0], 0);

  /* Use two different patterns for calls to explicitly named functions
     and calls through function pointers.  This is necessary as these two
     types of calls use different calling conventions, and CSE might try
     to change the named call into an indirect call in some cases (using
     two patterns keeps CSE from performing this optimization).  */
  if (GET_CODE (op) == SYMBOL_REF)
    call_insn = emit_call_insn (gen_call_internal_symref (op, operands[1]));
  else
    {
      rtx tmpreg = gen_rtx (REG, SImode, 22);
      emit_move_insn (tmpreg, force_reg (SImode, op));
      call_insn = emit_call_insn (gen_call_internal_reg (operands[1]));
    }

  if (flag_pic)
    {
      use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn), pic_offset_table_rtx);

      /* After each call we must restore the PIC register, even if it
	 doesn't appear to be used.

         This will set regs_ever_live for the callee saved register we
	 stored the PIC register in.  */
      emit_move_insn (pic_offset_table_rtx,
		      gen_rtx (REG, SImode, PIC_OFFSET_TABLE_REGNUM_SAVED));
      emit_insn (gen_rtx (USE, VOIDmode, pic_offset_table_rtx));

      /* Gross.  We have to keep the scheduler from moving the restore
	 of the PIC register away from the call.  SCHED_GROUP_P is
	 supposed to do this, but for some reason the compiler will
	 go into an infinite loop when we use that.

	 This method (blockage insn) may make worse code (then again
	 it may not since calls are nearly blockages anyway), but at
	 least it should work.  */
      emit_insn (gen_blockage ());
    }
  DONE;
}")

(define_insn "call_internal_symref"
  [(call (mem:SI (match_operand:SI 0 "call_operand_address" ""))
	 (match_operand 1 "" "i"))
   (clobber (reg:SI 2))
   (use (const_int 0))]
  "! TARGET_PORTABLE_RUNTIME"
  "*
{
  output_arg_descriptor (insn);
  return output_call (insn, operands[0], gen_rtx (REG, SImode, 2));
}"
  [(set_attr "type" "call")
   (set (attr "length")
;;       If we're sure that we can either reach the target or that the
;;	 linker can use a long-branch stub, then the length is 4 bytes.
;;
;;	 For long-calls the length will be either 52 bytes (non-pic)
;;	 or 68 bytes (pic).  */
;;	 Else we have to use a long-call;
      (if_then_else (lt (plus (symbol_ref "total_code_bytes") (pc))
			(const_int 240000))
		    (const_int 4)
		    (if_then_else (eq (symbol_ref "flag_pic")
				      (const_int 0))
				  (const_int 52)
				  (const_int 68))))])

(define_insn "call_internal_reg"
  [(call (mem:SI (reg:SI 22))
	 (match_operand 0 "" "i"))
   (clobber (reg:SI 2))
   (use (const_int 1))]
  ""
  "*
{
  rtx xoperands[2];

  /* First the special case for kernels, level 0 systems, etc.  */
  if (TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS)
    return \"ble 0(%%sr4,%%r22)\;copy %%r31,%%r2\";

  /* Now the normal case -- we can reach $$dyncall directly or
     we're sure that we can get there via a long-branch stub. 

     No need to check target flags as the length uniquely identifies
     the remaining cases.  */
  if (get_attr_length (insn) == 8)
    return \".CALL\\tARGW0=GR\;bl $$dyncall,%%r31\;copy %%r31,%%r2\";

  /* Long millicode call, but we are not generating PIC or portable runtime
     code.  */
  if (get_attr_length (insn) == 12)
    return \"CALL\\tARGW0=GR\;ldil L%%$$dyncall,%%r2\;ble R%%$$dyncall(%%sr4,%%r2)\;copy %%r31,%%r2\";

  /* Long millicode call for portable runtime.  */
  if (get_attr_length (insn) == 20)
    return \"ldil L%%$$dyncall,%%r31\;ldo R%%$$dyncall(%%r31),%%r31\;blr 0,%%r2\;bv,n 0(%%r31)\;nop\";

  /* If we're generating PIC code.  */
  xoperands[0] = operands[0];
  xoperands[1] = gen_label_rtx ();
  output_asm_insn (\"bl .+8,%%r1\", xoperands);
  output_asm_insn (\"addil L%%$$dyncall-%1,%%r1\", xoperands);
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, \"L\",
			     CODE_LABEL_NUMBER (xoperands[1]));
  output_asm_insn (\"ldo R%%$$dyncall-%1(%%r1),%%r1\", xoperands);
  output_asm_insn (\"blr 0,%%r2\", xoperands);
  output_asm_insn (\"bv,n 0(%%r1)\\n\\tnop\", xoperands);
  return \"\";
}"
  [(set_attr "type" "dyncall")
   (set (attr "length")
     (cond [
;; First NO_SPACE_REGS
	    (ne (symbol_ref "TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS")
		(const_int 0))
	    (const_int 8)

;; Target (or stub) within reach
	    (and (lt (plus (symbol_ref "total_code_bytes") (pc))
		     (const_int 240000))
		 (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
		     (const_int 0)))
	    (const_int 8)

;; Out of reach, but not PIC or PORTABLE_RUNTIME
	    (and (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
		     (const_int 0))
		 (eq (symbol_ref "flag_pic")
		     (const_int 0)))
	    (const_int 12)

	    (ne (symbol_ref "TARGET_PORTABLE_RUNTIME")
		(const_int 0))
	    (const_int 20)]

;; Out of range PIC case
	  (const_int 24)))])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand:SI 1 "" "")
			 (match_operand 2 "" "")))
	      (clobber (reg:SI 2))])]
  ""
  "
{
  rtx op;
  rtx call_insn;

  if (TARGET_PORTABLE_RUNTIME)
    op = force_reg (SImode, XEXP (operands[1], 0));
  else
    op = XEXP (operands[1], 0);

  /* Use two different patterns for calls to explicitly named functions
     and calls through function pointers.  This is necessary as these two
     types of calls use different calling conventions, and CSE might try
     to change the named call into an indirect call in some cases (using
     two patterns keeps CSE from performing this optimization).  */
  if (GET_CODE (op) == SYMBOL_REF)
    call_insn = emit_call_insn (gen_call_value_internal_symref (operands[0],
								op,
								operands[2]));
  else
    {
      rtx tmpreg = gen_rtx (REG, SImode, 22);
      emit_move_insn (tmpreg, force_reg (SImode, op));
      call_insn = emit_call_insn (gen_call_value_internal_reg (operands[0],
							       operands[2]));
    }
  if (flag_pic)
    {
      use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn), pic_offset_table_rtx);

      /* After each call we must restore the PIC register, even if it
	 doesn't appear to be used.

         This will set regs_ever_live for the callee saved register we
	 stored the PIC register in.  */
      emit_move_insn (pic_offset_table_rtx,
		      gen_rtx (REG, SImode, PIC_OFFSET_TABLE_REGNUM_SAVED));
      emit_insn (gen_rtx (USE, VOIDmode, pic_offset_table_rtx));

      /* Gross.  We have to keep the scheduler from moving the restore
	 of the PIC register away from the call.  SCHED_GROUP_P is
	 supposed to do this, but for some reason the compiler will
	 go into an infinite loop when we use that.

	 This method (blockage insn) may make worse code (then again
	 it may not since calls are nearly blockages anyway), but at
	 least it should work.  */
      emit_insn (gen_blockage ());
    }
  DONE;
}")

(define_insn "call_value_internal_symref"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand:SI 1 "call_operand_address" ""))
	      (match_operand 2 "" "i")))
   (clobber (reg:SI 2))
   (use (const_int 0))]
  ;;- Don't use operand 1 for most machines.
  "! TARGET_PORTABLE_RUNTIME"
  "*
{
  output_arg_descriptor (insn);
  return output_call (insn, operands[1], gen_rtx (REG, SImode, 2));
}"
  [(set_attr "type" "call")
   (set (attr "length")
;;       If we're sure that we can either reach the target or that the
;;	 linker can use a long-branch stub, then the length is 4 bytes.
;;
;;	 For long-calls the length will be either 52 bytes (non-pic)
;;	 or 68 bytes (pic).  */
;;	 Else we have to use a long-call;
      (if_then_else (lt (plus (symbol_ref "total_code_bytes") (pc))
			(const_int 240000))
		    (const_int 4)
		    (if_then_else (eq (symbol_ref "flag_pic")
				      (const_int 0))
				  (const_int 52)
				  (const_int 68))))])

(define_insn "call_value_internal_reg"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (reg:SI 22))
	      (match_operand 1 "" "i")))
   (clobber (reg:SI 2))
   (use (const_int 1))]
  ""
  "*
{
  rtx xoperands[2];

  /* First the special case for kernels, level 0 systems, etc.  */
  if (TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS)
    return \"ble 0(%%sr4,%%r22)\;copy %%r31,%%r2\";

  /* Now the normal case -- we can reach $$dyncall directly or
     we're sure that we can get there via a long-branch stub. 

     No need to check target flags as the length uniquely identifies
     the remaining cases.  */
  if (get_attr_length (insn) == 8)
    return \".CALL\\tARGW0=GR\;bl $$dyncall,%%r31\;copy %%r31,%%r2\";

  /* Long millicode call, but we are not generating PIC or portable runtime
     code.  */
  if (get_attr_length (insn) == 12)
    return \".CALL\\tARGW0=GR\;ldil L%%$$dyncall,%%r2\;ble R%%$$dyncall(%%sr4,%%r2)\;copy %%r31,%%r2\";

  /* Long millicode call for portable runtime.  */
  if (get_attr_length (insn) == 20)
    return \"ldil L%%$$dyncall,%%r31\;ldo R%%$$dyncall(%%r31),%%r31\;blr 0,%%r2\;bv,n 0(%%r31)\;nop\";

  /* If we're generating PIC code.  */
  xoperands[0] = operands[1];
  xoperands[1] = gen_label_rtx ();
  output_asm_insn (\"bl .+8,%%r1\", xoperands);
  output_asm_insn (\"addil L%%$$dyncall-%1,%%r1\", xoperands);
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, \"L\",
			     CODE_LABEL_NUMBER (xoperands[1]));
  output_asm_insn (\"ldo R%%$$dyncall-%1(%%r1),%%r1\", xoperands);
  output_asm_insn (\"blr 0,%%r2\", xoperands);
  output_asm_insn (\"bv,n 0(%%r1)\\n\\tnop\", xoperands);
  return \"\";
}"
  [(set_attr "type" "dyncall")
   (set (attr "length")
     (cond [
;; First NO_SPACE_REGS
	    (ne (symbol_ref "TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS")
		(const_int 0))
	    (const_int 8)

;; Target (or stub) within reach
	    (and (lt (plus (symbol_ref "total_code_bytes") (pc))
		     (const_int 240000))
		 (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
		     (const_int 0)))
	    (const_int 8)

;; Out of reach, but not PIC or PORTABLE_RUNTIME
	    (and (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
		     (const_int 0))
		 (eq (symbol_ref "flag_pic")
		     (const_int 0)))
	    (const_int 12)

	    (ne (symbol_ref "TARGET_PORTABLE_RUNTIME")
		(const_int 0))
	    (const_int 20)]

;; Out of range PIC case
	  (const_int 24)))])

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  emit_call_insn (gen_call (operands[0], const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
}")
(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type" "move")
   (set_attr "length" "4")])

;; These are just placeholders so we know where branch tables
;; begin and end.
(define_insn "begin_brtab"
  [(const_int 1)]
  "TARGET_GAS"
  ".begin_brtab"
  [(set_attr "type" "move")
   (set_attr "length" "0")])

(define_insn "end_brtab"
  [(const_int 2)]
  "TARGET_GAS"
  ".end_brtab"
  [(set_attr "type" "move")
   (set_attr "length" "0")])

;;; Hope this is only within a function...
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "bv%* 0(%0)"
  [(set_attr "type" "branch")
   (set_attr "length" "4")])

(define_insn "extzv"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "uint5_operand" "")
			 (match_operand:SI 3 "uint5_operand" "")))]
  ""
  "extru %1,%3+%2-1,%2,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 1)
			 (match_operand:SI 3 "register_operand" "q")))]
  ""
  "vextru %1,1,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "extv"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "uint5_operand" "")
			 (match_operand:SI 3 "uint5_operand" "")))]
  ""
  "extrs %1,%3+%2-1,%2,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 1)
			 (match_operand:SI 3 "register_operand" "q")))]
  ""
  "vextrs %1,1,%0"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

(define_insn "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r,r")
			 (match_operand:SI 1 "uint5_operand" "")
			 (match_operand:SI 2 "uint5_operand" ""))
	(match_operand:SI 3 "arith5_operand" "r,L"))]
  ""
  "@
   dep %3,%2+%1-1,%1,%0
   depi %3,%2+%1-1,%1,%0"
  [(set_attr "type" "shift,shift")
   (set_attr "length" "4,4")])

;; Optimize insertion of const_int values of type 1...1xxxx.
(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (match_operand:SI 1 "uint5_operand" "")
			 (match_operand:SI 2 "uint5_operand" ""))
	(match_operand:SI 3 "const_int_operand" ""))]
  "(INTVAL (operands[3]) & 0x10) != 0 &&
   (~INTVAL (operands[3]) & (1L << INTVAL (operands[1])) - 1 & ~0xf) == 0"
  "*
{
  operands[3] = GEN_INT ((INTVAL (operands[3]) & 0xf) - 0x10);
  return \"depi %3,%2+%1-1,%1,%0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "4")])

;; This insn is used for some loop tests, typically loops reversed when
;; strength reduction is used.  It is actually created when the instruction
;; combination phase combines the special loop test.  Since this insn
;; is both a jump insn and has an output, it must deal with it's own
;; reloads, hence the `m' constraints.  The `!' constraints direct reload
;; to not choose the register alternatives in the event a reload is needed.
(define_insn "decrement_and_branch_until_zero"
  [(set (pc)
	(if_then_else
	  (match_operator 2 "comparison_operator"
	   [(plus:SI (match_operand:SI 0 "register_operand" "+!r,!*f,!*m")
		     (match_operand:SI 1 "int5_operand" "L,L,L"))
	    (const_int 0)])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_dup 1)))
   (clobber (match_scratch:SI 4 "=X,r,r"))]
  ""
  "* return output_dbra (operands, insn, which_alternative); "
;; Do not expect to understand this the first time through.
[(set_attr "type" "cbranch,multi,multi")
 (set (attr "length")
      (if_then_else (eq_attr "alternative" "0")
;; Loop counter in register case
;; Short branch has length of 4
;; Long branch has length of 8
	(if_then_else (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8))

;; Loop counter in FP reg case.
;; Extra goo to deal with additional reload insns.
	(if_then_else (eq_attr "alternative" "1")
	  (if_then_else (lt (match_dup 3) (pc))
	    (if_then_else
	      (lt (abs (minus (match_dup 3) (plus (pc) (const_int 24))))
		  (const_int 8184))
	      (const_int 24)
	      (const_int 28))
	    (if_then_else
	      (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		  (const_int 8184))
	      (const_int 24)
	      (const_int 28)))
;; Loop counter in memory case.
;; Extra goo to deal with additional reload insns.
	(if_then_else (lt (match_dup 3) (pc))
	  (if_then_else
	    (lt (abs (minus (match_dup 3) (plus (pc) (const_int 12))))
		(const_int 8184))
	    (const_int 12)
	    (const_int 16))
	  (if_then_else
	    (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		(const_int 8184))
	    (const_int 12)
	    (const_int 16))))))])

;; Simply another variant of the dbra pattern.  More restrictive
;; in testing the comparison operator as it must worry about overflow
;; problems.
(define_insn ""
  [(set (pc)
	(if_then_else
	  (match_operator 2 "eq_neq_comparison_operator"
	   [(match_operand:SI 0 "register_operand" "+!r,!*f,!*m")
	    (match_operand:SI 5 "const_int_operand" "")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_operand:SI 1 "int5_operand" "L,L,L")))
   (clobber (match_scratch:SI 4 "=X,r,r"))]
  "INTVAL (operands[5]) == - INTVAL (operands[1])"
"* return output_dbra (operands, insn, which_alternative);"
;; Do not expect to understand this the first time through.
[(set_attr "type" "cbranch,multi,multi")
 (set (attr "length")
      (if_then_else (eq_attr "alternative" "0")
;; Loop counter in register case
;; Short branch has length of 4
;; Long branch has length of 8
	(if_then_else (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8))

;; Loop counter in FP reg case.
;; Extra goo to deal with additional reload insns.
	(if_then_else (eq_attr "alternative" "1")
	  (if_then_else (lt (match_dup 3) (pc))
	    (if_then_else
	      (lt (abs (minus (match_dup 3) (plus (pc) (const_int 24))))
		  (const_int 8184))
	      (const_int 24)
	      (const_int 28))
	    (if_then_else
	      (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		  (const_int 8184))
	      (const_int 24)
	      (const_int 28)))
;; Loop counter in memory case.
;; Extra goo to deal with additional reload insns.
	(if_then_else (lt (match_dup 3) (pc))
	  (if_then_else
	    (lt (abs (minus (match_dup 3) (plus (pc) (const_int 12))))
		(const_int 8184))
	    (const_int 12)
	    (const_int 16))
	  (if_then_else
	    (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		(const_int 8184))
	    (const_int 12)
	    (const_int 16))))))])

(define_insn ""
  [(set (pc)
	(if_then_else
	  (match_operator 2 "movb_comparison_operator"
	   [(match_operand:SI 1 "register_operand" "r,r,r,r") (const_int 0)])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))
   (set (match_operand:SI 0 "register_operand" "=!r,!*f,!*m,!*q")
	(match_dup 1))]
  ""
"* return output_movb (operands, insn, which_alternative, 0); "
;; Do not expect to understand this the first time through.
[(set_attr "type" "cbranch,multi,multi,multi")
 (set (attr "length")
      (if_then_else (eq_attr "alternative" "0")
;; Loop counter in register case
;; Short branch has length of 4
;; Long branch has length of 8
	(if_then_else (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8))

;; Loop counter in FP reg case.
;; Extra goo to deal with additional reload insns.
	(if_then_else (eq_attr "alternative" "1")
	  (if_then_else (lt (match_dup 3) (pc))
	    (if_then_else
	      (lt (abs (minus (match_dup 3) (plus (pc) (const_int 12))))
		  (const_int 8184))
	      (const_int 12)
	      (const_int 16))
	    (if_then_else
	      (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		  (const_int 8184))
	      (const_int 12)
	      (const_int 16)))
;; Loop counter in memory or sar case.
;; Extra goo to deal with additional reload insns.
	(if_then_else
	  (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
	      (const_int 8184))
	  (const_int 8)
	  (const_int 12)))))])

;; Handle negated branch.
(define_insn ""
  [(set (pc)
	(if_then_else
	  (match_operator 2 "movb_comparison_operator"
	   [(match_operand:SI 1 "register_operand" "r,r,r,r") (const_int 0)])
	  (pc)
	  (label_ref (match_operand 3 "" ""))))
   (set (match_operand:SI 0 "register_operand" "=!r,!*f,!*m,!*q")
	(match_dup 1))]
  ""
"* return output_movb (operands, insn, which_alternative, 1); "
;; Do not expect to understand this the first time through.
[(set_attr "type" "cbranch,multi,multi,multi")
 (set (attr "length")
      (if_then_else (eq_attr "alternative" "0")
;; Loop counter in register case
;; Short branch has length of 4
;; Long branch has length of 8
	(if_then_else (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8))

;; Loop counter in FP reg case.
;; Extra goo to deal with additional reload insns.
	(if_then_else (eq_attr "alternative" "1")
	  (if_then_else (lt (match_dup 3) (pc))
	    (if_then_else
	      (lt (abs (minus (match_dup 3) (plus (pc) (const_int 12))))
		  (const_int 8184))
	      (const_int 12)
	      (const_int 16))
	    (if_then_else
	      (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		  (const_int 8184))
	      (const_int 12)
	      (const_int 16)))
;; Loop counter in memory or SAR case.
;; Extra goo to deal with additional reload insns.
	(if_then_else
	  (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
	      (const_int 8184))
	  (const_int 8)
	  (const_int 12)))))])

;; The next several patterns (parallel_addb, parallel_movb, fmpyadd and
;; fmpysub aren't currently used by the FSF sources, but will be soon.
;;
;; They're in the FSF tree for documentation and to make Cygnus<->FSF
;; merging easier.
(define_insn ""
  [(set (pc) (label_ref (match_operand 3 "" "" )))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "ireg_or_int5_operand" "rL")))]
  "reload_completed && operands[0] == operands[1] || operands[0] == operands[2]"
  "*
{
  return output_parallel_addb (operands, get_attr_length (insn));
}"
  [(set_attr "type" "parallel_branch")
   (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 3) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc) (label_ref (match_operand 2 "" "" )))
   (set (match_operand:SF 0 "register_operand" "=r")
	(match_operand:SF 1 "ireg_or_int5_operand" "rL"))]
  "reload_completed"
  "*
{
  return output_parallel_movb (operands, get_attr_length (insn));
}"
  [(set_attr "type" "parallel_branch")
   (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc) (label_ref (match_operand 2 "" "" )))
   (set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "ireg_or_int5_operand" "rL"))]
  "reload_completed"
  "*
{
  return output_parallel_movb (operands, get_attr_length (insn));
}"
  [(set_attr "type" "parallel_branch")
   (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc) (label_ref (match_operand 2 "" "" )))
   (set (match_operand:HI 0 "register_operand" "=r")
	(match_operand:HI 1 "ireg_or_int5_operand" "rL"))]
  "reload_completed"
  "*
{
  return output_parallel_movb (operands, get_attr_length (insn));
}"
  [(set_attr "type" "parallel_branch")
   (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

(define_insn ""
  [(set (pc) (label_ref (match_operand 2 "" "" )))
   (set (match_operand:QI 0 "register_operand" "=r")
	(match_operand:QI 1 "ireg_or_int5_operand" "rL"))]
  "reload_completed"
  "*
{
  return output_parallel_movb (operands, get_attr_length (insn));
}"
  [(set_attr "type" "parallel_branch")
   (set (attr "length")
    (if_then_else (lt (abs (minus (match_dup 2) (plus (pc) (const_int 8))))
		      (const_int 8184))
           (const_int 4)
	   (const_int 8)))])

;; These insns will replace fmpyadd and fmpysub peepholes when the
;; independent insn combination code is installed at the FSF.  Until
;; then these patterns aren't used by the FSF compiler and are only
;; here for documentation purposes.
(define_insn ""
  [(set (match_operand 0 "register_operand" "=f")
	(mult (match_operand 1 "register_operand" "f")
	      (match_operand 2 "register_operand" "f")))
   (set (match_operand 3 "register_operand" "+f")
	(plus (match_operand 4 "register_operand" "f")
	      (match_operand 5 "register_operand" "f")))]
  "TARGET_SNAKE && ! TARGET_SOFT_FLOAT
   && reload_completed && fmpyaddoperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    {
      if (rtx_equal_p (operands[3], operands[5]))
	return \"fmpyadd,dbl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,dbl %1,%2,%0,%5,%3\";
    }
  else
    {
      if (rtx_equal_p (operands[3], operands[5]))
	return \"fmpyadd,sgl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,sgl %1,%2,%0,%5,%3\";
    }
}"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand 0 "register_operand" "=f")
	(mult (match_operand 1 "register_operand" "f")
	      (match_operand 2 "register_operand" "f")))
   (set (match_operand 3 "register_operand" "+f")
	(minus (match_operand 4 "register_operand" "f")
	       (match_operand 5 "register_operand" "f")))]
  "TARGET_SNAKE && ! TARGET_SOFT_FLOAT
   && reload_completed && fmpysuboperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    return \"fmpysub,dbl %1,%2,%0,%5,%3\";
  else
    return \"fmpysub,sgl %1,%2,%0,%5,%3\";
}"
  [(set_attr "type" "fpalu")
   (set_attr "length" "4")])

;; The next four peepholes take advantage of the new 5 operand
;; fmpy{add,sub} instructions available on 1.1 CPUS.  Basically
;; fmpyadd performs a multiply and add/sub of independent operands
;; at the same time.  Because the operands must be independent
;; combine will not try to combine such insns...  Thus we have
;; to use a peephole.
(define_peephole
  [(set (match_operand 0 "register_operand" "=f")
	(mult (match_operand 1 "register_operand" "f")
	      (match_operand 2 "register_operand" "f")))
   (set (match_operand 3 "register_operand" "+f")
	(plus (match_operand 4 "register_operand" "f")
	      (match_operand 5 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT && TARGET_SNAKE && fmpyaddoperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    {
      if (rtx_equal_p (operands[5], operands[3]))
	return \"fmpyadd,dbl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,dbl %1,%2,%0,%5,%3\";
    }
  else
    {
      if (rtx_equal_p (operands[5], operands[3]))
	return \"fmpyadd,sgl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,sgl %1,%2,%0,%5,%3\";
    }
}")

(define_peephole
  [(set (match_operand 3 "register_operand" "+f")
	(plus (match_operand 4 "register_operand" "f")
	      (match_operand 5 "register_operand" "f")))
   (set (match_operand 0 "register_operand" "=f")
	(mult (match_operand 1 "register_operand" "f")
	      (match_operand 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT && TARGET_SNAKE && fmpyaddoperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    {
      if (rtx_equal_p (operands[3], operands[5]))
	return \"fmpyadd,dbl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,dbl %1,%2,%0,%5,%3\";
    }
  else
    {
      if (rtx_equal_p (operands[3], operands[5]))
	return \"fmpyadd,sgl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,sgl %1,%2,%0,%5,%3\";
    }
}")

;; Note fsub subtracts the second operand from the first while fmpysub
;; does the opposite for the subtraction operands!
(define_peephole
  [(set (match_operand 0 "register_operand" "=f")
	(mult (match_operand 1 "register_operand" "f")
	      (match_operand 2 "register_operand" "f")))
   (set (match_operand 3 "register_operand" "+f")
	(minus (match_operand 4 "register_operand" "f")
	       (match_operand 5 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT && TARGET_SNAKE && fmpysuboperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    return \"fmpysub,dbl %1,%2,%0,%5,%3\";
  else
    return \"fmpysub,sgl %1,%2,%0,%5,%3\";
}")

(define_peephole
  [(set (match_operand 3 "register_operand" "+f")
	(minus (match_operand 4 "register_operand" "f")
	       (match_operand 5 "register_operand" "f")))
   (set (match_operand 0 "register_operand" "=f")
	(mult (match_operand 1 "register_operand" "f")
	      (match_operand 2 "register_operand" "f")))]
  "! TARGET_SOFT_FLOAT && TARGET_SNAKE && fmpysuboperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    return \"fmpysub,dbl %1,%2,%0,%5,%3\";
  else
    return \"fmpysub,sgl %1,%2,%0,%5,%3\";
}")

;; Clean up turds left by reload.
(define_peephole
  [(set (match_operand 0 "reg_or_nonsymb_mem_operand" "")
	(match_operand 1 "register_operand" "f"))
   (set (match_operand 2 "register_operand" "f")
	(match_dup 0))]
  "! TARGET_SOFT_FLOAT
   && GET_CODE (operands[0]) == MEM
   && ! MEM_VOLATILE_P (operands[0])
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && GET_MODE (operands[0]) == GET_MODE (operands[2])
   && GET_MODE (operands[0]) == DFmode
   && REGNO_REG_CLASS (REGNO (operands[1]))
      == REGNO_REG_CLASS (REGNO (operands[2]))"
  "*
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx xoperands[2];

  if (FP_REG_P (operands[1]))
    output_asm_insn (output_fp_move_double (operands), operands);
  else
    output_asm_insn (output_move_double (operands), operands);

  if (rtx_equal_p (operands[1], operands[2]))
    return \"\";

  xoperands[0] = operands[2];
  xoperands[1] = operands[1];
      
  if (FP_REG_P (xoperands[1]))
    output_asm_insn (output_fp_move_double (xoperands), xoperands);
  else
    output_asm_insn (output_move_double (xoperands), xoperands);

  return \"\";
}")

(define_peephole
  [(set (match_operand 0 "register_operand" "f")
	(match_operand 1 "reg_or_nonsymb_mem_operand" ""))
   (set (match_operand 2 "register_operand" "f")
	(match_dup 1))]
  "! TARGET_SOFT_FLOAT
   && GET_CODE (operands[1]) == MEM
   && ! MEM_VOLATILE_P (operands[1])
   && GET_MODE (operands[0]) == GET_MODE (operands[1])
   && GET_MODE (operands[0]) == GET_MODE (operands[2])
   && GET_MODE (operands[0]) == DFmode
   && REGNO_REG_CLASS (REGNO (operands[1]))
      == REGNO_REG_CLASS (REGNO (operands[2]))"
  "*
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx xoperands[2];

  if (FP_REG_P (operands[0]))
    output_asm_insn (output_fp_move_double (operands), operands);
  else
    output_asm_insn (output_move_double (operands), operands);

  xoperands[0] = operands[2];
  xoperands[1] = operands[0];
      
  if (FP_REG_P (xoperands[1]))
    output_asm_insn (output_fp_move_double (xoperands), xoperands);
  else
    output_asm_insn (output_move_double (xoperands), xoperands);

  return \"\";
}")

;; Flush the I and D cache line found at the address in operand 0.
;; This is used by the trampoline code for nested functions.
;; So long as the trampoline itself is less than 32 bytes this
;; is sufficient.

(define_insn "dcacheflush"
  [(unspec_volatile [(const_int 1)] 0)
   (use (mem:SI (match_operand:SI 0 "register_operand" "r")))
   (use (mem:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "fdc 0(0,%0)\;fdc 0(0,%1)\;sync"
  [(set_attr "type" "multi")
   (set_attr "length" "12")])

(define_insn "icacheflush"
  [(unspec_volatile [(const_int 2)] 0)
   (use (mem:SI (match_operand:SI 0 "register_operand" "r")))
   (use (mem:SI (match_operand:SI 1 "register_operand" "r")))
   (use (match_operand:SI 2 "register_operand" "r"))
   (clobber (match_operand:SI 3 "register_operand" "=&r"))
   (clobber (match_operand:SI 4 "register_operand" "=&r"))]
  ""
  "mfsp %%sr0,%4\;ldsid (0,%2),%3\;mtsp %3,%%sr0\;fic 0(%%sr0,%0)\;fic 0(%%sr0,%1)\;sync\;mtsp %4,%%sr0\;nop\;nop\;nop\;nop\;nop\;nop"
  [(set_attr "type" "multi")
   (set_attr "length" "52")])

;; An out-of-line prologue.
(define_insn "outline_prologue_call"
  [(unspec_volatile [(const_int 0)] 0)
   (clobber (reg:SI 31))
   (clobber (reg:SI 22))
   (clobber (reg:SI 21))
   (clobber (reg:SI 20))
   (clobber (reg:SI 19))
   (clobber (reg:SI 1))]
  ""
  "*
{
  extern int frame_pointer_needed;

  /* We need two different versions depending on whether or not we
     need a frame pointer.   Also note that we return to the instruction
     immediately after the branch rather than two instructions after the
     break as normally is the case.  */
  if (frame_pointer_needed)
    {
      /* Must import the magic millicode routine(s).  */
      output_asm_insn (\".IMPORT __outline_prologue_fp,MILLICODE\", NULL);

      if (TARGET_PORTABLE_RUNTIME)
	{
	  output_asm_insn (\"ldil L'__outline_prologue_fp,%%r31\", NULL);
	  output_asm_insn (\"ble,n R'__outline_prologue_fp(%%sr0,%%r31)\",
			   NULL);
	}
      else
	output_asm_insn (\"bl,n __outline_prologue_fp,%%r31\", NULL);
    }
  else
    {
      /* Must import the magic millicode routine(s).  */
      output_asm_insn (\".IMPORT __outline_prologue,MILLICODE\", NULL);

      if (TARGET_PORTABLE_RUNTIME)
	{
	  output_asm_insn (\"ldil L'__outline_prologue,%%r31\", NULL);
	  output_asm_insn (\"ble,n R'__outline_prologue(%%sr0,%%r31)\", NULL);
	}
      else
	output_asm_insn (\"bl,n __outline_prologue,%%r31\", NULL);
    }
  return \"\";
}"
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

;; An out-of-line epilogue.
(define_insn "outline_epilogue_call"
  [(unspec_volatile [(const_int 1)] 0)
   (use (reg:SI 29))
   (use (reg:SI 28))
   (clobber (reg:SI 31))
   (clobber (reg:SI 22))
   (clobber (reg:SI 21))
   (clobber (reg:SI 20))
   (clobber (reg:SI 19))
   (clobber (reg:SI 2))
   (clobber (reg:SI 1))]
  ""
  "*
{
  extern int frame_pointer_needed;

  /* We need two different versions depending on whether or not we
     need a frame pointer.   Also note that we return to the instruction
     immediately after the branch rather than two instructions after the
     break as normally is the case.  */
  if (frame_pointer_needed)
    {
      /* Must import the magic millicode routine.  */
      output_asm_insn (\".IMPORT __outline_epilogue_fp,MILLICODE\", NULL);

      /* The out-of-line prologue will make sure we return to the right
	 instruction.  */
      if (TARGET_PORTABLE_RUNTIME)
	{
	  output_asm_insn (\"ldil L'__outline_epilogue_fp,%%r31\", NULL);
	  output_asm_insn (\"ble,n R'__outline_epilogue_fp(%%sr0,%%r31)\",
			   NULL);
	}
      else
	output_asm_insn (\"bl,n __outline_epilogue_fp,%%r31\", NULL);
    }
  else
    {
      /* Must import the magic millicode routine.  */
      output_asm_insn (\".IMPORT __outline_epilogue,MILLICODE\", NULL);

      /* The out-of-line prologue will make sure we return to the right
	 instruction.  */
      if (TARGET_PORTABLE_RUNTIME)
	{
	  output_asm_insn (\"ldil L'__outline_epilogue,%%r31\", NULL);
	  output_asm_insn (\"ble,n R'__outline_epilogue(%%sr0,%%r31)\", NULL);
	}
      else
	output_asm_insn (\"bl,n __outline_epilogue,%%r31\", NULL);
    }
  return \"\";
}"
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

;; Given a function pointer, canonicalize it so it can be 
;; reliably compared to another function pointer.  */
(define_expand "canonicalize_funcptr_for_compare"
  [(set (reg:SI 26) (match_operand:SI 1 "register_operand" ""))
   (parallel [(set (reg:SI 29) (unspec:SI [(reg:SI 26)] 0))
	      (clobber (match_dup 2))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 22))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "register_operand" "")
	(reg:SI 29))]
  "! TARGET_PORTABLE_RUNTIME"
  "
{
  operands[2] = gen_reg_rtx (SImode);
  if (GET_CODE (operands[1]) != REG)
    {
      rtx tmp = gen_reg_rtx (Pmode);
      emit_move_insn (tmp, operands[1]);
      operands[1] = tmp;
    }
}")

(define_insn ""
  [(set (reg:SI 29) (unspec:SI [(reg:SI 26)] 0))
   (clobber (match_operand:SI 0 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 22))
   (clobber (reg:SI 31))]
  ""
  "*
{
  /* Must import the magic millicode routine.  */
  output_asm_insn (\".IMPORT $$sh_func_adrs,MILLICODE\", NULL);

  /* This is absolutely fucking amazing.

     First, copy our input parameter into %r29 just in case we don't
     need to call $$sh_func_adrs.  */
  output_asm_insn (\"copy %%r26,%%r29\", NULL);

  /* Next, examine the low two bits in %r26, if they aren't 0x2, then
     we use %r26 unchanged.  */
  if (get_attr_length (insn) == 32)
    output_asm_insn (\"extru %%r26,31,2,%%r31\;comib,<>,n 2,%%r31,.+24\", NULL);
  else if (get_attr_length (insn) == 40)
    output_asm_insn (\"extru %%r26,31,2,%%r31\;comib,<>,n 2,%%r31,.+32\", NULL);
  else if (get_attr_length (insn) == 44)
    output_asm_insn (\"extru %%r26,31,2,%%r31\;comib,<>,n 2,%%r31,.+36\", NULL);
  else
    output_asm_insn (\"extru %%r26,31,2,%%r31\;comib,<>,n 2,%%r31,.+20\", NULL);

  /* Next, compare %r26 with 4096, if %r26 is less than or equal to
     4096, then we use %r26 unchanged.  */
  if (get_attr_length (insn) == 32)
    output_asm_insn (\"ldi 4096,%%r31\;comb,<<,n %%r26,%%r31,.+16\", NULL);
  else if (get_attr_length (insn) == 40)
    output_asm_insn (\"ldi 4096,%%r31\;comb,<<,n %%r26,%%r31,.+24\", NULL);
  else if (get_attr_length (insn) == 44)
    output_asm_insn (\"ldi 4096,%%r31\;comb,<<,n %%r26,%%r31,.+28\", NULL);
  else
    output_asm_insn (\"ldi 4096,%%r31\;comb,<<,n %%r26,%%r31,.+12\", NULL);

  /* Else call $$sh_func_adrs to extract the function's real add24.  */
  return output_millicode_call (insn,
				gen_rtx (SYMBOL_REF, SImode,
					 \"$$sh_func_adrs\"));
}"
  [(set_attr "type" "multi")
   (set (attr "length")
     (cond [
;; Target (or stub) within reach
            (and (lt (plus (symbol_ref "total_code_bytes") (pc))
                     (const_int 240000))
                 (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0)))
            (const_int 28)

;; NO_SPACE_REGS
            (ne (symbol_ref "TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS")
                (const_int 0))
            (const_int 32)

;; Out of reach, but not PIC or PORTABLE_RUNTIME
;; same as NO_SPACE_REGS code
            (and (eq (symbol_ref "TARGET_PORTABLE_RUNTIME")
                     (const_int 0))
                 (eq (symbol_ref "flag_pic")
                     (const_int 0)))
            (const_int 32)

;; PORTABLE_RUTNIME
	    (ne (symbol_ref "TARGET_PORTABLE_RUNTIME")
		(const_int 0))
	    (const_int 40)]

;; Out of range and PIC 
	  (const_int 44)))])


