(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk    *)
(*      modified on Tue Mar  2 17:18:02 PST 1993 by muller    *)
(* ow 03.10.1994 *)

INTERFACE Usignal;

FROM Ctypes IMPORT int, unsigned_int;

(*** <signal.h> ***)

CONST
  SIGHUP    =  1;      (* hangup *)
  SIGINT    =  2;      (* interrupt *)
  SIGQUIT   =  3;      (* quit *)
  SIGILL    =  4;      (* illegal instruction (not reset when caught) *)
  SIGTRAP   =  5;      (* trace trap (not reset when caught) *)
  SIGIOT    =  6;      (* IOT instruction *)
  SIGEMT    =  7;      (* EMT instruction *)
  SIGFPE    =  8;      (* floating point exception *)
      FPE_INTOVF_TRAP      =  1;  (* integer overflow *)
      FPE_INTDIV_TRAP      =  2;  (* integer divide by zero *)
      FPE_FLTDIV_TRAP      =  3;  (* floating/decimal divide by zero *)
      FPE_FLTOVF_TRAP      =  4;  (* floating overflow *)
      FPE_FLTUND_TRAP      =  5;  (* floating underflow *)
      FPE_FPU_NP_TRAP      =  6;  (* floating point unit not present *)
      FPE_SUBRNG_TRAP      =  7;  (* subrange out of bounds *)
  SIGKILL   =  9;      (* kill (cannot be caught or ignored) *)
  SIGBUS    =  10;     (* bus error *)
      BUS_PAGE_FAULT       = 12;  (* page fault protection base *)
      BUS_SEGNP_FAULT      = 26;  (* segment not present *)
      BUS_STK_FAULT        = 27;  (* stack fault *)
  SIGSEGV   =  11;     (* segmentation violation *)
  SIGSYS    =  12;     (* bad argument to system call *)
  SIGPIPE   =  13;     (* write on a pipe with no one to read it *)
  SIGALRM   =  14;     (* alarm clock *)
  SIGTERM   =  15;     (* software termination signal from kill *)
  SIGURG    =  16;     (* urgent condition on IO channel *)
  SIGSTOP   =  17;     (* sendable stop signal not from tty *)
  SIGTSTP   =  18;     (* stop signal from tty *)
  SIGCONT   =  19;     (* continue a stopped process *)
  SIGCHLD   =  20;     (* to parent on child stop or exit *)
  SIGTTIN   =  21;     (* to readers pgrp upon background tty read *)
  SIGTTOU   =  22;     (* like TTIN for output if (tp->t_local&LTOSTOP) *)
  SIGIO     =  23;     (* input/output possible signal *)
  SIGXCPU   =  24;     (* exceeded CPU time limit *)
  SIGXFSZ   =  25;     (* exceeded file size limit *)
  SIGVTALRM =  26;     (* virtual time alarm *)
  SIGPROF   =  27;     (* profiling time alarm *)
  SIGWINCH  =  28;     (* window size changes *)
  SIGINFO   =  29;     (* information request *)
  SIGUSR1   =  30;     (* user defined signal 1 *)
  SIGUSR2   =  31;     (* user defined signal 2 *)

  (* System V definitions *)
  SIGCLD    = SIGCHLD;
  SIGABRT   = SIGIOT;


(* Signal vector "template" used in sigaction call. *)
TYPE
  SignalHandler = PROCEDURE (sig, code: int;
                             scp: UNTRACED REF struct_sigcontext);

  sigset_t = ARRAY [0..3] OF unsigned_int;
  sigset_t_star = UNTRACED REF sigset_t;

  struct_sigvec  = RECORD
    sv_handler: SignalHandler;     (* signal handler *)
    sv_mask:    int;               (* signal mask to apply *)
    sv_flags:   int;               (* see signal options below *)
  END;
    

CONST
  empty_sigset_t = sigset_t{ 0, .. };
  empty_sv_mask  : int = 0;

CONST
 (* Valid flags defined for sv_flags field of sigvec structure. *)
  SV_ONSTACK     = 16_0001;   (* run on special signal stack *)
  SV_RESTART     = 16_0002;   (* restart system calls on sigs *)
  SV_RESETHAND   = 16_0004;   (* reset to SIG_DFL when taking signal *)
  SV_NOCLDSTOP   = 16_0008;   (* do not generate SIGCHLD on child stop *)
  SV_NODEFER     = 16_0010;   (* don't mask the signal we're delivering *)

  (* Defines for sigprocmask() call. POSIX. *)
  SIG_BLOCK    = 1;    (* Add these signals to block mask *)
  SIG_UNBLOCK  = 2;    (* Remove these signals from block mask *)
  SIG_SETMASK  = 3;    (* Set block mask to this mask *)

TYPE
  struct_sigaction = RECORD
    sa_handler  : SignalHandler;        (* signal handler *)
    sa_flags    : int;                  (* signal action flags *)
    sa_mask     : sigset_t;             (* signals to block while in handler *)
  END;

  struct_sigaction_star = UNTRACED REF struct_sigaction;

CONST
 (* Valid flags defined for sa_flags field of sigaction structure. *)
  SA_ONSTACK     = 16_0001;   (* run on special signal stack *)
  SA_RESTART     = 16_0002;   (* restart system calls on sigs *)
  SA_RESETHAND   = 16_0004;   (* reset to SIG_DFL when taking signal *)
  SA_NOCLDSTOP   = 16_0008;   (* do not generate SIGCHLD on child stop *)
  SA_NODEFER     = 16_0010;   (* don't mask the signal we're delivering *)

TYPE
  struct_sigstack = RECORD 
    ss_sp:      ADDRESS; (* signal stack pointer *)
    ss_onstack: int;     (* current status *)
  END;

(*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to properly restore state if
 * a non-standard exit is performed.
 *)

TYPE
  struct_sigcontext = RECORD
    sc_mask: sigset_t; (* signal mask to restore *)
    sc_onstack: int;   (* sigstack state to restore *)
    sc_gs: int;
    sc_fs: int;
    sc_es: int;
    sc_ds: int;
    sc_edi: int;
    sc_esi: int;
    sc_ebp: int;       (* frame pointer *)
    sc_isp: int;
    sc_ebx: int;
    sc_edx: int;
    sc_ecx: int;
    sc_eax: int;
    sc_trapno: int;
    sc_err: int;
    sc_eip: int;       (* program counter *)
    sc_cs: int;
    sc_efl: int;
    sc_esp: int;       (* stack pinter *)
    sc_ss: int;
  END;

(* Do not modifiy these variables *)
VAR (*CONST*)
  BADSIG, SIG_ERR, SIG_DFL, SIG_IGN, SIG_HOLD: SignalHandler;


(* Convert a signal number to a mask suitable for sigblock(). *)
<*INLINE*> PROCEDURE sigmask (n: int): int;


(*** kill(2) - send signal to a process ***)

<*EXTERNAL*> PROCEDURE kill (pid, sig: int): int;


(*** killpg(2) - send signal to a process or process group ***)

<*EXTERNAL*> PROCEDURE killpg (pgrp, sig: int): int;


(*** sigblock(2) - block signals ***)

<*EXTERNAL*> PROCEDURE sigblock (mask: int): int;


(*** sigpause(2) - atomically release blocked signals and wait for
                   interrupt ***)

<*EXTERNAL*> PROCEDURE sigpause (sigmask: int): int;


(*** sigpending(2) - examine pending signals ***)

<*EXTERNAL*> PROCEDURE sigpending (VAR set: sigset_t): int;


(*** sigsetmask(2) - set current signal mask ***)

<*EXTERNAL*> PROCEDURE sigsetmask (mask: int): unsigned_int;


(*** sigstack(2) - set and/or get signal stack context ***)

(* FIXME - It is OK for ss and/or oss to be NIL, so we shouldn't use VAR *)
<*EXTERNAL*> PROCEDURE sigstack (VAR ss, oss: struct_sigstack): int;

(*** sigsuspend(2) - release blocked signals and wait for interrupt ***)

<*EXTERNAL*>
PROCEDURE sigsuspend (VAR sigmask: sigset_t): int;

(*** sigaction(2) - software signal facilities ***)

<*EXTERNAL*>
PROCEDURE sigaction (sig: int;  act, oact: struct_sigaction_star): int;

(*** sigvec(2) - software signal facilities ***)

(* FIXME - It is OK for vec and/or ovec to be NIL, so we shouldn't use VAR *)
<*EXTERNAL*>
PROCEDURE sigvec (sig: int; VAR vec, ovec: struct_sigvec): int;

(* FIXME - It is OK for vec and/or ovec to be NIL, so we shouldn't use VAR *)
<*EXTERNAL*>
PROCEDURE sigprocmask (how: int; VAR set, oldset: sigset_t) : int;

<*EXTERNAL*>
PROCEDURE sigemptyset (VAR set: sigset_t) : int;

<*EXTERNAL*>
PROCEDURE sigfillset (VAR set: sigset_t) : int;

<*EXTERNAL*>
PROCEDURE sigaddset (VAR set: sigset_t; signo: int) : int;

<*EXTERNAL*>
PROCEDURE sigdelset (VAR set: sigset_t; signo: int) : int;

<*EXTERNAL*>
PROCEDURE sigismember(VAR set: sigset_t; signo: int) : int;

END Usignal.
