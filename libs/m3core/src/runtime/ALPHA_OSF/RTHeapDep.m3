(* Copyright (C) 1992, 1996 Digital Equipment Corporation    *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Nov 14 14:25:40 PST 1996 by heydon   *)

UNSAFE MODULE RTHeapDep;

IMPORT RT0u, RTHeapRep, RTCollectorSRC, RTMachine;
IMPORT Cstdlib, Ctypes, Umman, Unix, Uresource, Usignal, Utime, Utypes, Word;

VAR
  (* true iff "Init" has been called *)
  initialized := FALSE;

  (* original handler for "SIGSEGV" signal; set by "Init" *)
  origSIGSEGV: Usignal.SignalHandler := NIL;

TYPE
  AlphaInsn = RECORD
    x:  BITS 26 FOR [0..16_FFFFFF];
    op: BITS  6 FOR [0..16_3F];
  END;

PROCEDURE Fault (sig: Ctypes.int;
                 sip: UNTRACED REF Usignal.siginfo_t;
                 scp: UNTRACED REF Usignal.struct_sigcontext) =
(* Fault is called upon a SIGSEGV signal caused by a VM fault.  If
   RTHeapRep.Fault is not able to handle the fault, it invokes the
   previous action installed in "origSIGSEGV". *)
  VAR
    sip_fault := LOOPHOLE(sip, UNTRACED REF Usignal.siginfo_t_fault);
    mode: RTHeapRep.Mode;
    insn := LOOPHOLE(scp.sc_pc, UNTRACED REF AlphaInsn)^;
  BEGIN
    CASE insn.op OF
    | 16_0D..16_0F, 16_24..16_27, 16_2C..16_2F =>
      mode := RTHeapRep.Mode.ReadWrite;
    ELSE
      mode := RTHeapRep.Mode.ReadOnly;
    END;
    (* try handling memory fault using "RTHeapRep.Fault" *)
    IF sig = Usignal.SIGSEGV AND sip # NIL THEN
      <*ASSERT sig = sip.si_signo*>
      IF RTHeapRep.Fault(sip_fault.si_addr, mode) THEN
        RETURN;
      END
    END;
    (* otherwise, use "origSIGSEGV" to handle the fault *)
    IF origSIGSEGV = Usignal.SIG_IGN THEN
      RETURN
    ELSIF origSIGSEGV = Usignal.SIG_DFL THEN
      Core(sig, sip, scp);
    ELSE
      origSIGSEGV(sig, sip, scp);
    END;
  END Fault;

(* record if core is already being dumped *)
VAR dumped_core := FALSE;

PROCEDURE Core (sig: Ctypes.int;
                <*UNUSED*> sip: UNTRACED REF Usignal.siginfo_t;
                <*UNUSED*> scp: UNTRACED REF Usignal.struct_sigcontext) =
(* Core is a signal handler for signals that dump core; it completes the
   current collection before dumping core.  This makes core files easier to
   debug, and avoids an Ultrix bug that creates incomplete core files if
   heap pages are read-protected. *)
  BEGIN
    INC(RT0u.inCritical);
    IF NOT dumped_core THEN
      (* indicate that this thread will dump core *)
      dumped_core := TRUE;

      (* clean up the heap and install default handler *)
      EVAL RTHeapRep.Crash();

      (* establish default handler *)
      VAR
        new, old: Usignal.struct_sigaction;
      BEGIN
        new.sa_flags := 0;
        new.sa_handler := Usignal.SIG_DFL;
        EVAL Usignal.sigemptyset(new.sa_mask);
        EVAL Usignal.sigaction(sig, new, old);
      END;

      VAR set: Usignal.sigset_t;
      BEGIN
        EVAL Usignal.sigemptyset(set);
        EVAL Usignal.sigprocmask(Usignal.SIG_SETMASK, set);
      END;

      (* now, dump core *)
      Cstdlib.abort ();
      <* ASSERT FALSE *>
    END;
    DEC(RT0u.inCritical);
  END Core;

PROCEDURE Init () =
(* Init establishes a handler for SIGSEGV, caused by VM faults,
   and for all other signals that cause core dumps. System-call
   faults are handled in "RTHeapDepC.c". *)
  BEGIN
    (* check that "BytesPerPage" is an acceptable value *)
    VAR vmPageBytes := Unix.getpagesize(); BEGIN
      <* ASSERT BytesPerPage >= vmPageBytes *>
      <* ASSERT BytesPerPage MOD vmPageBytes = 0 *>
    END;

    (* establish SIGSEGV handler; remember previous handler *)
    VAR
      new, old : Usignal.struct_sigaction;
    BEGIN
      new.sa_flags := Word.Or(Usignal.SA_NODEFER,
                              Word.Or(Usignal.SA_RESTART, Usignal.SA_SIGINFO));
      new.sa_handler := Fault;
      WITH i = Usignal.sigemptyset(new.sa_mask) DO
        <*ASSERT i = 0*>
      END;
      (* block the "SIGVTALRM" signal when signal handlers are called *)
      WITH i = Usignal.sigaddset(new.sa_mask, Usignal.SIGVTALRM) DO
        <*ASSERT i = 0*>
      END;
      WITH i = Usignal.sigaction(Usignal.SIGSEGV, new, old) DO
        <*ASSERT i = 0*>
      END;
      origSIGSEGV := old.sa_handler;
    END;

    PROCEDURE OverrideDefaultWithCore(sig: Ctypes.int) =
    (* If no handler currently exists for signal "sig",
       install a handler for "sig" that dumps core. *)
      VAR
        new, old: Usignal.struct_sigaction;
      BEGIN
        new.sa_flags := Usignal.SA_SIGINFO;
        new.sa_handler := Core;
        WITH i = Usignal.sigemptyset(new.sa_mask) DO
          <*ASSERT i = 0*>
        END;
        WITH i = Usignal.sigaddset(new.sa_mask, Usignal.SIGVTALRM) DO
          <*ASSERT i = 0*>
        END;
        WITH i = Usignal.sigaction(sig, new, old) DO
          <*ASSERT i = 0*>
        END;
        (* If the old handler was not the default, restore it. *)
        IF old.sa_handler # Usignal.SIG_DFL THEN
          WITH i = Usignal.sigaction(sig, old, new) DO
            <*ASSERT i = 0*>
          END;
        END;
      END OverrideDefaultWithCore;

    BEGIN
      (* override signal handling for all signals that normally dump core *)
      OverrideDefaultWithCore(Usignal.SIGQUIT);
      OverrideDefaultWithCore(Usignal.SIGILL);
      OverrideDefaultWithCore(Usignal.SIGTRAP);
      OverrideDefaultWithCore(Usignal.SIGIOT);
      OverrideDefaultWithCore(Usignal.SIGEMT);
      OverrideDefaultWithCore(Usignal.SIGFPE);
      OverrideDefaultWithCore(Usignal.SIGBUS);
      OverrideDefaultWithCore(Usignal.SIGSYS);
    END;
  END Init;

PROCEDURE Protect(p: Page; n: CARDINAL; readable, writable: BOOLEAN) =
  BEGIN
    IF NOT initialized THEN Init(); initialized := TRUE; END;
    VAR prot: Ctypes.int := 0; BEGIN
      IF readable THEN prot := Word.Or(prot, Umman.PROT_READ); END;
      IF writable THEN prot := Word.Or(prot, Umman.PROT_WRITE); END;
      VAR
        addr := LOOPHOLE(p * BytesPerPage, Utypes.caddr_t);
        ret := Umman.mprotect(addr, n * BytesPerPage, prot);
      BEGIN
        <* ASSERT ret = 0 *>
      END;
    END;
  END Protect;

PROCEDURE TimevalSecs(READONLY t: Utime.struct_timeval): REAL =
(* Return the number of seconds represented by "t" as a floating-
   point number. *)
  BEGIN
    RETURN FLOAT(t.tv_sec) + (FLOAT(t.tv_usec) / 1.0e6)
  END TimevalSecs;

PROCEDURE TimeUsed (): REAL =
  VAR
    usage: Uresource.struct_rusage;
    ret := Uresource.getrusage(Uresource.RUSAGE_SELF, usage);
  BEGIN
    <* ASSERT ret = 0 *>
    RETURN TimevalSecs(usage.ru_utime) + TimevalSecs(usage.ru_stime);
  END TimeUsed;

PROCEDURE VMFaultTime (): REAL =
  BEGIN
    RETURN 0.010; (* guess 10ms to handle a page fault *)
  END VMFaultTime;

BEGIN
  IF VM THEN
    RTMachine.RTHeapRep_Fault  := LOOPHOLE (RTHeapRep.Fault, ADDRESS);
    RTMachine.RTCSRC_FinishVM  := LOOPHOLE (RTCollectorSRC.FinishVM, ADDRESS);
  END;
END RTHeapDep.
