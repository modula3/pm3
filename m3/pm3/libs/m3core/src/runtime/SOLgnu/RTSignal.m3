(* Copyright (C) 1992, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov 21 13:15:02 PST 1994 by kalsow    *)
(*      modified on Mon Mar 16 18:10:15 PST 1992 by muller    *)

UNSAFE MODULE RTSignal;

IMPORT RTMisc, RTProcess, Usignal, Uprocess;
FROM Ctypes IMPORT int;
FROM Usignal IMPORT siginfo_t_fault_star;
FROM Uucontext IMPORT ucontext_t_star;

VAR
  DefaultHandler   : Usignal.SignalHandler;
  IgnoreSignal     : Usignal.SignalHandler;
  initial_handlers : ARRAY [0..5] OF Usignal.struct_sigaction;

PROCEDURE InstallHandlers () =
  BEGIN
    DefaultHandler := LOOPHOLE (0, Usignal.SignalHandler);
    IgnoreSignal   := LOOPHOLE (1, Usignal.SignalHandler);

    SetHandler (0, Usignal.SIGHUP,  Shutdown);
    SetHandler (1, Usignal.SIGINT,  Interrupt);
    SetHandler (2, Usignal.SIGQUIT, Quit);
    SetHandler (3, Usignal.SIGSEGV, SegV);
    SetHandler (4, Usignal.SIGPIPE, IgnoreSignal);
    SetHandler (5, Usignal.SIGTERM, Shutdown);
  END InstallHandlers;

PROCEDURE SetHandler (id: INTEGER; sig: int;  handler: Usignal.SignalHandler) =
  (* Note: we use the LOOPHOLE to prevent the runtime check for
     nested procedure.  The runtime check crashes when
     handler = IgnoreSignal = 1. *)
  VAR new: Usignal.struct_sigaction;
  BEGIN
    new.sa_handler := LOOPHOLE (handler, Usignal.SignalHandler);
    new.sa_flags   := Usignal.SA_SIGINFO;
    WITH i = Usignal.sigemptyset(new.sa_mask) DO
      <*ASSERT i = 0*>
    END;
    WITH i = Usignal.sigaction (sig, new, initial_handlers[id]) DO
      <*ASSERT i = 0*>
    END;
    IF (initial_handlers[id].sa_handler # DefaultHandler) THEN
      (* don't override inherited, non-default handlers *)
      WITH i = Usignal.sigaction (sig, initial_handlers[id], new) DO
        <*ASSERT i = 0*>
      END;
    END;
  END SetHandler;

PROCEDURE RestoreHandlers () =
  BEGIN
    RestoreHandler (0, Usignal.SIGHUP);
    RestoreHandler (1, Usignal.SIGINT);
    RestoreHandler (2, Usignal.SIGQUIT);
    RestoreHandler (3, Usignal.SIGSEGV);
    RestoreHandler (4, Usignal.SIGPIPE);
    RestoreHandler (5, Usignal.SIGTERM);
  END RestoreHandlers;

PROCEDURE RestoreHandler (id: INTEGER;  sig: int) =
  VAR old: Usignal.struct_sigaction;
  BEGIN
    EVAL Usignal.sigaction (sig, initial_handlers[id], old);
  END RestoreHandler;

PROCEDURE Shutdown (sig: int;
                    <*UNUSED*> sip: siginfo_t_fault_star;
                    <*UNUSED*> uap: ucontext_t_star) =
  VAR new, old: Usignal.struct_sigaction;
  BEGIN
    new.sa_handler := DefaultHandler;
    new.sa_flags   := 0;
    EVAL Usignal.sigemptyset(new.sa_mask);
    RTProcess.InvokeExitors ();                   (* flush stdio... *)
    EVAL Usignal.sigaction (sig, new, old);       (* restore default handler *)
    EVAL Usignal.kill (Uprocess.getpid (), sig);  (* and resend the signal *)
  END Shutdown;

PROCEDURE Interrupt (sig: int;
                     sip: siginfo_t_fault_star;
                     uap: ucontext_t_star) =
  VAR h := RTProcess.OnInterrupt (NIL);
  BEGIN
    IF (h = NIL) THEN
      Shutdown (sig, sip, uap);
    ELSE
      EVAL RTProcess.OnInterrupt (h); (* reinstall the handler *)
      h ();
    END;
  END Interrupt;

PROCEDURE Quit (<*UNUSED*> sig: int;
                <*UNUSED*> sip: siginfo_t_fault_star;
                uap: ucontext_t_star) =
  VAR pc := 0;
  BEGIN
    IF uap # NIL THEN pc := uap.uc_mcontext.gregs.pc; END;
    RTMisc.FatalErrorPC (pc, "aborted");
  END Quit;

PROCEDURE SegV (<*UNUSED*> sig: int;
                <*UNUSED*> sip: siginfo_t_fault_star;
                uap: ucontext_t_star) =
  VAR pc := 0;
  BEGIN
    IF uap # NIL THEN pc := uap.uc_mcontext.gregs.pc; END;
    RTMisc.FatalErrorPC (pc,
      "Segmentation violation - possible attempt to dereference NIL");
  END SegV;

BEGIN
END RTSignal.
