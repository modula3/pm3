(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jul 30 13:55:56 EST 1997 by hosking *)

INTERFACE Uucontext;

FROM Ctypes IMPORT int, char_star, unsigned_int, double;
FROM Ctypes IMPORT unsigned_long;
FROM Utypes IMPORT size_t;

(*** <mach/ppc/thread_status.h> ***)

TYPE
  (* ppc_thread_state is the structure that is exported to user threads for 
     use in status/mutate calls.  This structure should never change. *)
  struct_ppc_thread_state = RECORD
    srr0: unsigned_int;		(* Instruction address register (PC) *)
    srr1: unsigned_int;		(* Machine state register (supervisor) *)
    r0:   unsigned_int;
    r1:   unsigned_int;
    r2:   unsigned_int;
    r3:   unsigned_int;
    r4:   unsigned_int;
    r5:   unsigned_int;
    r6:   unsigned_int;
    r7:   unsigned_int;
    r8:   unsigned_int;
    r9:   unsigned_int;
    r10:  unsigned_int;
    r11:  unsigned_int;
    r12:  unsigned_int;
    r13:  unsigned_int;
    r14:  unsigned_int;
    r15:  unsigned_int;
    r16:  unsigned_int;
    r17:  unsigned_int;
    r18:  unsigned_int;
    r19:  unsigned_int;
    r20:  unsigned_int;
    r21:  unsigned_int;
    r22:  unsigned_int;
    r23:  unsigned_int;
    r24:  unsigned_int;
    r25:  unsigned_int;
    r26:  unsigned_int;
    r27:  unsigned_int;
    r28:  unsigned_int;
    r29:  unsigned_int;
    r30:  unsigned_int;
    r31:  unsigned_int;

    cr:   unsigned_int;		(* Condition register *)
    xer:  unsigned_int;		(* User's integer exception register *)
    lr:   unsigned_int;		(* Link register *)
    ctr:  unsigned_int;		(* Count register *)
    mq:   unsigned_int;		(* MQ register (601 only) *)

    vrsave: unsigned_int;	(* Vector Save Register *)
  END;
  ppc_thread_state_t = struct_ppc_thread_state;

  (* This structure should be double-word aligned for performance *)
  struct_ppc_float_state = RECORD
    fpregs:    ARRAY [0..31] OF double;

    fpscr_pad: unsigned_int;	(* fpscr is 64 bits, 32 bits of rubbish *)
    fpscr:     unsigned_int;	(* floating point status register *)
  END;
  ppc_float_state_t = struct_ppc_float_state;

  struct_ppc_vector_state = RECORD
    save_vr:      ARRAY[0..31] OF ARRAY [0..3] OF unsigned_long;
    save_vscr:    ARRAY [0..3] OF unsigned_long;
    save_pad5:    ARRAY [0..3] OF unsigned_int;
    save_vrvalid: unsigned_int;	(* VRs that have been saved *)
    save_pad6:    ARRAY [0..6] OF unsigned_int;
  END;
  ppc_vector_state_t = struct_ppc_vector_state;

  (* ppc_exception_state

     This structure corresponds to some additional state of the user
     registers as saved in the PCB upon kernel entry. They are only
     available if an exception is passed out of the kernel, and even
     then not all are guaranteed to be updated.

     Some padding is included in this structure which allows space for
     servers to store temporary values if need be, to maintain binary
     compatiblity. *)
  struct_ppc_exception_state = RECORD
    dar:       unsigned_long;	(* Fault registers for coredump *)
    dsisr:     unsigned_long;
    exception: unsigned_long;	(* number of powerpc exception taken *)
    pad0:      unsigned_long;	(* align to 16 bytes *)

    pad1:      ARRAY [0..3] OF unsigned_long; (* space in PCB "just in case" *)
  END;
  ppc_exception_state_t = struct_ppc_exception_state;

(*** <ppc/ucontext.h> ***)

TYPE
  struct_mcontext = RECORD
    es: ppc_exception_state_t;
    ss: ppc_thread_state_t;
    fs: ppc_float_state_t;
    vs: ppc_vector_state_t;
  END;
  mcontext_t = UNTRACED REF struct_mcontext;

(*** <sys/signal.h> ***)

TYPE
  (* Structure used in sigaltstack call. *)
  struct_sigaltstack = RECORD
    ss_sp:    char_star;   (* signal stack base *)
    ss_size:  int;	   (* signal stack length *)
    ss_flags: int;	   (* SA_DISABLE and/or SA_ONSTACK *)
  END;
  stack_t = struct_sigaltstack;

(*** <sys/ucontext.h> ***)

TYPE
  sigset_t = unsigned_int;

  struct_ucontext = RECORD
    sc_onstack:  int;
    uc_sigmask:  sigset_t;       	 (* signal mask used by this context *)
    uc_stack:    stack_t;		 (* stack used by this context *)
    uc_link:     struct_ucontext_star;	 (* pointer to resuming context *)
    uc_mcsize:   size_t;		 (* size of the machine context passed in *)
    uc_mcontext: mcontext_t;		 (* machine specific context *)
  END;
  struct_ucontext_star = UNTRACED REF struct_ucontext;
  ucontext_t = struct_ucontext;


END Uucontext.
