(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Mon Jan  5 11:11:07 GMT 1998 by rrw               *)
(*      modified on Fri Feb 24 15:18:21 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:58:12 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Tue Mar  2 17:18:02 PST 1993 by muller            *)


INTERFACE Usignal;

FROM Ctypes IMPORT int, unsigned_int, unsigned_short_int, unsigned_long_int;

(*** <signal.h> ***)

CONST
  SIGHUP    =  1;      (* hangup *)
  SIGINT    =  2;      (* interrupt *)
  SIGQUIT   =  3;      (* quit *)
  SIGILL    =  4;      (* illegal instruction (not reset when caught) *)
  SIGTRAP   =  5;      (* trace trap (not reset when caught) *)
  SIGIOT    =  6;      (* IOT instruction *)
  (* Linux 1.1.73 doesn't have SIGEMT - rrw *)
  SIGEMT    =  7;      (* EMT instruction *)
  SIGBUS    =  7;      (* bus error *)
      BUS_HWERR	  = 1;     (* misc hardware error (e.g. timeout) *)
      BUS_ALIGN	  = 2;     (* hardware alignment error *)
  SIGFPE    =  8;      (* floating point exception *)
      FPE_INTDIV_TRAP      = 20;  (* integer divide by zero *)
      FPE_INTOVF_TRAP      = 21;  (* integer overflow *)
      FPE_FLTOPERR_TRAP    =  1;  (* [floating operand error] *)
      FPE_FLTDEN_TRAP      =  2;  (* [floating denormalized operand] *)
      FPE_FLTDIV_TRAP      =  3;  (* [floating divide by zero] *)
      FPE_FLTOVF_TRAP      =  4;  (* [floating overflow] *)
      FPE_FLTUND_TRAP      =  5;  (* [floating underflow] *)
      FPE_FLTINEX_TRAP     =  6;  (* [floating inexact result] *)
      FPE_UUOP_TRAP        =  7;  (* [floating undefined opcode] *)
      FPE_DATACH_TRAP      =  8;  (* [floating data chain exception] *)
      FPE_FLTSTK_TRAP      = 16;  (* [floating stack fault] *)
      FPE_FPA_ENABLE       = 17;  (* [FPA not enabled] *)
      FPE_FPA_ERROR        = 18;  (* [FPA arithmetic exception] *)
  SIGKILL   =  9;      (* kill (cannot be caught or ignored) *)
  SIGUSR1   =  10;     (* User signal 1 (from SysV) *)
  SIGSEGV   =  11;     (* segmentation violation *)
      SEGV_NOMAP  = 3;     (* no mapping at the fault address *)
      SEGV_PROT   = 4;      (* access exceeded protections *)
      SEGV_OBJERR = 5;    (* object returned errno value *)
  (* No SIGSYS in Linux 1.1.73 - rrw *)
  SIGSYS    =  12;     (* bad argument to system call *)
  SIGUSR2   =  12;     (* User signal 2 (from SysV) *)
  SIGPIPE   =  13;     (* write on a pipe with no one to read it *)
  SIGALRM   =  14;     (* alarm clock *)
  SIGTERM   =  15;     (* software termination signal from kill *)
  SIGSTKFLT =  16;
  SIGCHLD   =  17;     (* to parent on child stop or exit *)
  SIGCONT   =  18;     (* continue a stopped process *)
  SIGSTOP   =  19;     (* sendable stop signal not from tty *)
  SIGTSTP   =  20;     (* stop signal from tty *)
  SIGTTIN   =  21;     (* to readers pgrp upon background tty read *)
  SIGTTOU   =  22;     (* like TTIN for output if (tp->t_local&LTOSTOP) *)
  SIGIO     =  23;     (* input/output possible signal *)
  SIGURG    =  SIGIO;  (* urgent condition on IO channel *)
  SIGPOLL   =  SIGIO;
  SIGXCPU   =  24;     (* exceeded CPU time limit *)
  SIGXFSZ   =  25;     (* exceeded file size limit *)
  SIGVTALRM =  26;     (* virtual time alarm *)
  SIGPROF   =  27;     (* profiling time alarm *)
  SIGWINCH  =  28;     (* window size changes *)
  (* SIGLOST is commented out of /usr/include/linux/signal.h in Linux 1.1.73 - rrw *)
  SIGLOST   =  29;     (* Sys-V rec lock: notify user upon server crash *)
  (* Under Linux 1.1.73, signals 30 and 31 are :  - rrw *)
  SIGPWR     = 30;
  SIGUNUSED  = 31;

  (* System V definitions *)
  SIGCLD    = SIGCHLD;
  SIGABRT   = SIGIOT;

CONST
  SIGSET_NWORDS = 1024 DIV (8*4);

(* Signal vector "template" used in sigaction call. *)
TYPE
  SignalHandler = PROCEDURE (sig: int;
                             scp: struct_sigcontext;
                             code: int);

  sigset_t = RECORD
    val : ARRAY [0..SIGSET_NWORDS] OF INTEGER;
  END;
  sigset_t_star = UNTRACED REF sigset_t;


CONST
  empty_sigset_t : sigset_t = sigset_t{ARRAY [0..SIGSET_NWORDS] OF INTEGER{0, ..}};
  empty_sv_mask  : sigset_t = sigset_t{ARRAY [0..SIGSET_NWORDS] OF INTEGER{0, ..}};

CONST
  SV_ONSTACK   = 16_0001;  (* take signal on signal stack *)
  SV_INTERRUPT = 16_0002;  (* do not restart system on signal return *)
  (* SV_OLDSIG is not provided (explicitly, anyway) by glibc2 *)
  SV_OLDSIG    = 16_1000;  (* Emulate old signal() for POSIX *)
  SV_RESETHAND = 16_0004;  (* Reset handler to SIG_DFL on receipt *)

  (* Defines for sigprocmask() call. POSIX. *)
  SIG_BLOCK    = 0;    (* Add these signals to block mask *)
  SIG_UNBLOCK  = 1;    (* Remove these signals from block mask *)
  SIG_SETMASK  = 2;    (* Set block mask to this mask *)

TYPE
  SignalActionHandler  = PROCEDURE (sig: int);
  SignalRestoreHandler = PROCEDURE ();

  struct_sigaction = RECORD
    sa_handler  : SignalActionHandler;  (* signal handler *)
    sa_mask     : sigset_t;             (* signals to block while in handler *)
    sa_flags    : int;                  (* signal action flags *)
    sa_restorer : SignalRestoreHandler; (* restores interrupted state *)
  END;

  struct_sigaction_star = UNTRACED REF struct_sigaction;

 (* valid flags define for sa_flag field of sigaction structure  *)
CONST
  SA_NOCLDSTOP = 1;           (* Don't generate SIGCLD when children stop *)
  SA_STACK       = 16_08000000;
  SA_RESTART     = 16_10000000;
  SA_INTERRUPT   = 16_20000000;
  SA_NOMASK      = 16_40000000;
  SA_ONESHOT     = 16_80000000;

  SA_ONSTACK     = 16_0001;   (* run on special signal stack *)
  SA_OLDSTYLE    = 16_0002;   (* old "unreliable" UNIX semantics *)
  SA_NODUMP      = 16_0010;   (* termination by this sig does not use a 
                                 core file *)
  SA_PARTDUMP    = 16_0020;   (* create a partial dump for this signal *)
  SA_FULLDUMP    = 16_0040;   (* create a full dump (with data areas) *)
  SA_SIGSETSTYLE = 16_0080;   (* new system V sigset type semantics *)

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
 *
 * WARNING: THE sigcontext MUST BE KEPT CONSISTENT WITH /usr/include/setjmp.h
 * AND THE LIBC ROUTINES setjmp() AND longjmp()
 *
 *)

TYPE
  (* There seems to be no simple corresponding structure under Linux - 
      use the structure in Csetjmp.i3 instead *)
  struct_sigcontext = RECORD
      gs, gsh: unsigned_short_int;
      fs, fsh: unsigned_short_int;
      es, esh: unsigned_short_int;
      ds, dsh: unsigned_short_int;
      edi: unsigned_long_int;
      esi: unsigned_long_int;
      ebp: unsigned_long_int;
      esp: unsigned_long_int;
      ebx: unsigned_long_int;
      edx: unsigned_long_int;
      ecx: unsigned_long_int;
      eax: unsigned_long_int;
      trapno: unsigned_long_int;
      err: unsigned_long_int;
      eip: unsigned_long_int;
      cs, csh: unsigned_short_int;
      eflags: unsigned_long_int;
      esp_at_signal: unsigned_long_int;
      ss, ssh: unsigned_short_int;
      i387: unsigned_long_int; (* Actually a struct _fpstate * *)
      oldmask: unsigned_long_int;
      cr2: unsigned_long_int;
    END;
  
 struct_fpreg = RECORD
   significand : ARRAY [0..3] OF unsigned_short_int;
   exponent : unsigned_short_int;
 END;

 struct_fpstate = RECORD
   cw : unsigned_long_int;
   sw : unsigned_long_int;
   tag : unsigned_long_int;
   ipoff : unsigned_long_int;
   cssel : unsigned_long_int;
   dataoff: unsigned_long_int;
   datasel : unsigned_long_int;
   st : ARRAY [0..7] OF struct_fpreg;
   status : unsigned_long_int;
 END;  


(* Do not modifiy these variables *)
VAR (*CONST*)
  BADSIG, SIG_ERR, SIG_DFL, SIG_IGN, SIG_HOLD: SignalActionHandler;


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

<*EXTERNAL*> PROCEDURE sigstack (VAR ss, oss: struct_sigstack): int;


(*** sigaction(2) - software signal facilities ***)

<*EXTERNAL*>
PROCEDURE sigaction (sig: int;  act, oact: struct_sigaction_star): int;


(*** sigprocmask(2) - set the blocked signals ***)

<*EXTERNAL*>
PROCEDURE sigprocmask(how: int; set, oldset: sigset_t_star): int;

(*
PROCEDURE SigWord(sig : INTEGER) : INTEGER;
PROCEDURE SigMask(sig : INTEGER) : Word.T;
PROCEDURE SigIsMember(set : sigset_t; sig : INTEGER) : BOOLEAN;
PROCEDURE SigAddSet(set : sigset_t; sig : INTEGER) : sigset_t;
PROCEDURE SigDelSet(set : sigset_t; sig : INTEGER) : sigset_t;
*)

END Usignal.
