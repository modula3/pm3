(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Nov 19 09:26:45 PST 1994 by kalsow     *)
(*      modified on Wed Jun  2 15:22:58 PDT 1993 by muller     *)

(* The linker generates an inital direct call to this module's
   main body.  All Modula-3 code reached from here. *)

UNSAFE MODULE RTLinker;

IMPORT RT0, RT0u, RTParams, RTProcess, RTHeapRep, RTMisc;
IMPORT RTTypeSRC, RTSignal, RTHooks, RTThreadInit, RTHeapInfo;

VAR init_done := FALSE;

PROCEDURE ExportProcs () =
  VAR
    n: RT0.ModulePtr;
    m: UNTRACED REF RT0.ModulePtr := info.modules;
    p: RT0.ProcPtr;
    v: UNTRACED REF ADDRESS;
  BEGIN
    (* initialize the interface records with the exported procedures *)
    FOR i := 0 TO info.n_modules - 1 DO
      n := m^;
      IF (n # NIL) AND (n.proc_info # NIL) THEN
        p := n.proc_info;
        WHILE (p.proc # NIL) DO
          v := p.export;
          IF (v # NIL) THEN v^ := p.proc; END;
          INC (p, ADRSIZE (p^));
        END;
      END;
      INC (m, ADRSIZE (m^));
    END;

    (* finally, patch up any "static" procedure constants *)
    m := info.modules;
    FOR i := 0 TO info.n_modules - 1 DO
      n := m^;
      IF (n = NIL) THEN
        RTMisc.FatalErrorI ("empty slot in module table ", i);
      END;
      IF (n.link # NIL) THEN n.link () END;
      INC (m, ADRSIZE (m^));
    END;
  END ExportProcs;

PROCEDURE RunMainBodies () =
  VAR
    n: RT0.ModulePtr;
    m: UNTRACED REF RT0.ModulePtr := info.modules;
  BEGIN
    FOR i := 0 TO info.n_modules - 1 DO
      n := m^;
      IF (n # NIL) AND (n.main # NIL) THEN n.main () END;
      INC (m, ADRSIZE (m^));
    END;
  END RunMainBodies;

BEGIN
  IF NOT init_done THEN
    init_done := TRUE;

    (* spread the linker variables around where they're needed *)
    RT0u.nModules    := info.n_modules;
    RT0u.modules     := info.modules;

    RTHooks.bottom_of_stack := info.bottom_of_stack;
    RTHooks.top_of_stack    := info.top_of_stack;

    (* run each of the init procs to initialize the interface records *)
    ExportProcs ();

    (* fix the runtime type structures *)
    RTTypeSRC.Init ();
   
    (* initialize the runtime *)
    RTSignal.InstallHandlers ();
    RTParams.Init ();
    RTHeapRep.Init ();
    RTThreadInit.Init ();
    RTHeapInfo.Init ();

    (* run the module main bodies *)
    RunMainBodies ();

    (* and be done *)
    RTProcess.Exit (0);
  END;
END RTLinker.


