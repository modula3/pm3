(* From File: MxGen.m3 *)
(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE MxGenC;

IMPORT Wr, Thread, Stdio;
IMPORT Mx, MxRep, MxMap, M3ID, Target;

FROM MxGenRep IMPORT BinderSuffix, UnitInfo, Err, Common;

REVEAL
  T = Common BRANDED "MxGenC.T" OBJECT
    (* C output information *)
    wr            : Wr.T         := NIL;
    eol           : TEXT         := NIL;
  OVERRIDES
    generateMain := GenerateMain;
  END;

TYPE
  State = T;

(*------------------------------------------------------------------------*)

PROCEDURE New(base: Mx.LinkSet;  output: Wr.T;
                        verbose: BOOLEAN;  windowsGUI: BOOLEAN): T=
  VAR
    t := NEW(T, 
             base    := base,
             wr      := output,
             verbose := verbose,
             errors  := Stdio.stdout,
             gui     := windowsGUI,
             eol     := Target.EOL);
  BEGIN
    RETURN t;
  END New;

PROCEDURE GenerateMain (s: State) =
  BEGIN
    GenTypeDecls (s);
    ImportMain (s);
    GenerateEntry (s);
  END GenerateMain;

(*------------------------------------------------------------------------*)

PROCEDURE GenTypeDecls (<*UNUSED*> VAR s: State) =
  BEGIN
  END GenTypeDecls;

(*------------------------------------------------------------------------*)

PROCEDURE ImportMain (VAR s: State) =
  VAR
    main  := M3ID.Add ("Main");
    units := MxMap.GetData (s.base.modules);
    u     : Mx.Unit;
  BEGIN
    s.main_units := NIL;

    (* find the modules exporting "Main" *)
    FOR i := 0 TO LAST (units^) DO
      u := units[i].value;
      IF (u # NIL) THEN
        FOR i := u.exported_units.start
              TO u.exported_units.start + u.exported_units.cnt - 1 DO
          IF (u.info[i] = main) THEN
            s.main_units := NEW (UnitInfo, unit := u, next := s.main_units);
            ImportUnit (s, s.main_units);
            EXIT;
          END;
        END;
      END;
    END;

    IF s.main_units = NIL THEN
      Err (s, "No module implements \"Main\".", s.eol);
    END;
  END ImportMain;

PROCEDURE ImportUnit (READONLY s: State;  ui: UnitInfo) =
  VAR u := ui.unit;
  BEGIN
    ui.binder := M3ID.ToText (u.name) & BinderSuffix [u.interface];
    Out (s, "extern void* ", ui.binder, "();", s.eol);
  END ImportUnit;

(*------------------------------------------------------------------------*)

PROCEDURE GenerateEntry (VAR s: State) =
  VAR ui: UnitInfo;
  BEGIN
    Out (s, "extern void RTLinker__InitRuntime ();", s.eol);
    Out (s, "extern void RTLinker__AddUnit ();", s.eol);
    Out (s, "extern void RTProcess__Exit ();", s.eol, s.eol);

    IF (s.gui) THEN
      Out (s, "#include <windows.h>", s.eol);
      Out (s, "int WINAPI ");
      Out (s, "WinMain (HINSTANCE self, HINSTANCE prev,", s.eol);
      Out (s, "                    LPSTR args, int mode)", s.eol);
      Out (s, "{", s.eol);
      Out (s, "  RTLinker__InitRuntime (-1, args, ");
      Out (s,                        "GetEnvironmentStringsA(), self);", s.eol);
    ELSE
      Out (s, "int main (argc, argv, envp)", s.eol);
      Out (s, "int argc;", s.eol);
      Out (s, "char **argv;", s.eol);
      Out (s, "char **envp;", s.eol);
      Out (s, "{", s.eol);
      Out (s, "  RTLinker__InitRuntime (argc, argv, envp, 0);", s.eol);
    END;

    ui := s.main_units;
    WHILE (ui # NIL) DO
      Out (s, "  RTLinker__AddUnit (", ui.binder, ");", s.eol);
      ui := ui.next;
    END;

    Out (s, "  RTProcess__Exit (0);", s.eol);
    Out (s, "  return 0;", s.eol);
    Out (s, "}", s.eol, s.eol);
  END GenerateEntry;

(*------------------------------------------------------------------------*)

PROCEDURE Out (READONLY s: State;  a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (s.wr, a) END;
    IF (b # NIL) THEN Wr.PutText (s.wr, b) END;
    IF (c # NIL) THEN Wr.PutText (s.wr, c) END;
    IF (d # NIL) THEN Wr.PutText (s.wr, d) END;
  END Out;

BEGIN
END MxGenC.
