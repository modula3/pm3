(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxGenRep.m3                                           *)
(* Last Modified On Thu Jan 18 15:09:58 EST 1996 By ldd        *)
(*                                                             *)
(* From File: MxGen.m3                                         *)
(* Last Modified On Thu Feb 23 08:53:23 PST 1995 By kalsow     *)
(*      Modified On Fri Jul  2 19:33:09 PDT 1993 By muller     *)
MODULE MxGenRep;

IMPORT IntRefTbl, Wr, IntArraySort, Fmt, Thread;
IMPORT Mx, MxRep, M3ID, MxMap;

<* FATAL Thread.Alerted, Wr.Failure *>

REVEAL 
  Common = Public BRANDED "MxGenRep.Common" OBJECT
  END;

TYPE
  State = Common;

PROCEDURE CheckOpaques (s: State) =
  VAR o: OpaqueInfo;
  BEGIN
    s.opaques := NEW (IntRefTbl.Default).init ();
    ForEachUnit (s, NoteOpaques);
    ForEachUnit (s, IdentifyOpaques);
    o := s.all_opaques;
    WHILE (o # NIL) DO
      IF (o.reveal = NIL) THEN
        Err (s, "opaque type never revealed: ",TName(o.type.type), Wr.EOL);
        Err (s, "  defined in ", MxRep.UnitName (o.t_unit), Wr.EOL);
      END;
      o := o.next;
    END;
  END CheckOpaques;

PROCEDURE NoteOpaques (s: State;  u: Mx.Unit) =
  VAR o: Mx.OpaqueType;  z: OpaqueInfo;  ref: REFANY;
  BEGIN
    o := u.opaques;
    WHILE (o # NIL) DO
      IF s.opaques.get (o.type, ref) THEN
        z := ref;
        Err (s, "opaque type defined twice: ", TName (z.type.type), Wr.EOL);
        Err (s, "  defined in  ", MxRep.UnitName (z.t_unit), Wr.EOL);
        Err (s, "  and also    ", MxRep.UnitName (u), Wr.EOL);
      ELSE
        z := NEW (OpaqueInfo, type := o, t_unit := u, next:= s.all_opaques);
        s.all_opaques := z;
        EVAL s.opaques.put (o.type, z);
        INC (s.n_opaques);
      END;
      o := o.next;
    END;
  END NoteOpaques;

PROCEDURE IdentifyOpaques (s: State;  u: Mx.Unit) =
  VAR z: OpaqueInfo;  ref: REFANY;  r := u.revelations;
  BEGIN
    WHILE (r # NIL) DO
      IF (r.partial) OR (NOT r.export) THEN
        (* ignore for now *)
      ELSIF s.opaques.get (r.lhs, ref) THEN
        z := ref;
        IF (z.reveal # NIL) THEN
          Err (s, "multiple revelations for opaque type:  ",
                   TName(z.type.type), Wr.EOL );
          Err (s, "  defined in  ", MxRep.UnitName (z.t_unit), Wr.EOL);
          Err (s, "  revealed in ", MxRep.UnitName (z.r_unit), Wr.EOL);
          Err (s, "  and also in ", MxRep.UnitName (u), Wr.EOL);
        ELSE
          z.reveal := r;
          z.r_unit := u;
        END;
      ELSE
        Err (s, "revelation without matching opaque type declaration:  ",
                 TName (r.lhs), Wr.EOL);
        Err (s, "  revealed in ", MxRep.UnitName (u), Wr.EOL);
      END;
      r := r.next;
    END;
  END IdentifyOpaques;

(*------------------------------------------------------------------------*)

PROCEDURE FindBuiltins (s: State) =
  VAR u: Mx.Unit;
  BEGIN
    s.builtin_unit := NIL;
    s.builtin_name := M3ID.Add (Mx.BuiltinUnitName);
    u := MxMap.Get (s.base.interfaces, s.builtin_name);
    IF (u = NIL) THEN
      Err (s, "builtins are missing: ", Mx.BuiltinUnitName, Wr.EOL);
    END;
    s.builtin_unit := u;
  END FindBuiltins;

(*------------------------------------------------------------------------*)

PROCEDURE SortUnits (all_ui: UnitInfo;  n_units: INTEGER): UnitInfo =
  (* generate a sorted list of the imports *)
  VAR
    ui,vi : UnitInfo;
    cnt   := 0;
    units := NEW (REF ARRAY OF UnitInfo, n_units);
    map   := NEW (REF ARRAY OF INTEGER, n_units);

  PROCEDURE CmpUnit (a, b: INTEGER): [-1..1] =
    VAR ax := units[a].unit.name;  bx := units[b].unit.name;
    BEGIN
      IF    (ax = bx)          THEN RETURN 0;
      ELSIF M3ID.IsLT (ax, bx) THEN RETURN -1;
      ELSE                          RETURN +1;
      END;
    END CmpUnit;

  BEGIN
    ui := all_ui;
    WHILE (ui # NIL) DO
      units [cnt] := ui;
      map   [cnt] := cnt;
      INC (cnt);
      ui := ui.next;
    END;
    <*ASSERT cnt = n_units*>
      
    IntArraySort.Sort (map^, CmpUnit);

    (* rebuild the linked list *)
    ui := NIL;
    FOR i := n_units-1 TO 0 BY -1 DO
      vi := units[map[i]];
      vi.next := ui;
      ui := vi;
    END;

    RETURN ui;
  END SortUnits;

PROCEDURE FindUnit (s: State;  intfs: REF ARRAY OF UnitInfo;
                     READONLY name: Mx.Name): UnitInfo =
  VAR x: INTEGER;
  BEGIN
    x := MxMap.GetIndex (s.base.interfaces, name);
    IF (0 <= x) AND (x <= LAST (intfs^))
      THEN RETURN intfs[x];
      ELSE RETURN NIL;
    END;
  END FindUnit;

(*------------------------------------------------------------------------*)

PROCEDURE ForEachUnit (s: State;  p: UnitProc) =
  VAR x: MxMap.Contents;  u: Mx.Unit;
  BEGIN
    x := MxMap.GetData (s.base.interfaces);
    FOR i := 0 TO LAST (x^) DO
      u := x[i].value;
      IF (u # NIL) THEN p (s, u) END;
    END;
    x := MxMap.GetData (s.base.modules);
    FOR i := 0 TO LAST (x^) DO
      u := x[i].value;
      IF (u # NIL) THEN p (s, u) END;
    END;
  END ForEachUnit;

PROCEDURE TName (t: Mx.TypeName): TEXT =
  BEGIN
    RETURN "_t" & Fmt.Unsigned (t, 16);
  END TName;

PROCEDURE Err (s: State;  a, b, c, d: TEXT := NIL) =
  BEGIN
    s.failed := TRUE;
    IF (s.errors = NIL) THEN RETURN END;
    IF (a # NIL) THEN Wr.PutText (s.errors, a); END;
    IF (b # NIL) THEN Wr.PutText (s.errors, b); END;
    IF (c # NIL) THEN Wr.PutText (s.errors, c); END;
    IF (d # NIL) THEN Wr.PutText (s.errors, d); END;
  END Err;

PROCEDURE Out (s: State;  a, b, c, d: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN Wr.PutText (s.output, a) END;
    IF (b # NIL) THEN Wr.PutText (s.output, b) END;
    IF (c # NIL) THEN Wr.PutText (s.output, c) END;
    IF (d # NIL) THEN Wr.PutText (s.output, d) END;
  END Out;

BEGIN

END MxGenRep. 
