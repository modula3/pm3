MODULE M3LDepends;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_PL_F;

IMPORT SeqM3AST_AS_Used_interface_id;
IMPORT SeqM3AST_AS_Module, SeqM3AST_AS_Module_id;

IMPORT M3CUnit;
IMPORT M3Context;

(*Debug IMPORT M3CId, Wr, Stdio, Fmt; Debug*)

CONST  (* states of computation *)
  Needed = 0;
  InProgress = 1;
  Done = 2;

NoGenerics =
  M3CUnit.TypeSet{M3CUnit.Type.Interface, M3CUnit.Type.Interface_gen_ins,
                  M3CUnit.Type.Module, M3CUnit.Type.Module_gen_ins};
TYPE
  ContextClosure = M3Context.Closure OBJECT
    dependsClosure: Closure;
  END;

REVEAL Closure = Closure_public BRANDED OBJECT END;

(*PUBLIC*)
PROCEDURE Set(c: M3Context.T; cl: Closure) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    Clear(c);
    M3Context.ApplyToSet(c, NEW(M3Context.Closure, callback := SetExportedBy),
      NoGenerics);
    M3Context.ApplyToSet(c, NEW(ContextClosure, dependsClosure := cl,
      callback := SetSimpleDependsOn), NoGenerics);
    M3Context.ApplyToSet(c, NEW(ContextClosure, dependsClosure := cl,
      callback := CloseDependsOn), NoGenerics);
(*Debug     M3Context.Apply(c, NEW(M3Context.Closure, callback := Debug));  Debug*)     
  END Set;

(*PUBLIC*)
PROCEDURE Clear(c: M3Context.T) RAISES {}=
  <*FATAL ANY*>
  BEGIN
    M3Context.ApplyToSet(c, NEW(M3Context.Closure, callback := ClearUnit),
        NoGenerics);   
  END Clear;

PROCEDURE ClearUnit(
    <*UNUSED*> cl: M3Context.Closure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT;
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    TYPECASE cu.as_root OF <*NOWARN*>
    | M3AST_AS.Module(m) =>
        m.pl_tmp_dep_status := Needed;
        m.pl_dependson_s := NIL;
    | M3AST_AS.Interface(i) =>
        VAR
          id: M3AST_AS.Interface_id := i.as_id;
        BEGIN
          id.pl_isexportedby_s := SeqM3AST_AS_Module_id.Null;
        END;
    END; (* if *)
  END ClearUnit;

(*Debug
PROCEDURE Debug(
    cl: M3Context.Closure;
    ut: M3CUnit.Type;
    name: TEXT;
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    iter: SeqM3AST_AS_Module.Iter;
    m, tm: M3AST_AS.Module;
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    TYPECASE cu.as_root OF
    | M3AST_AS.Module(m) =>
        Put("\nDependsOn list for %s\n - ", m, NIL);
        iter := SeqM3AST_AS_Module.NewIter(m.pl_dependson_s);
        WHILE SeqM3AST_AS_Module.Next(iter, tm) DO
          Put("%s  ", tm, NIL);
        END;
    ELSE
    END;
  END Debug;
Debug*)

(*PRIVATE*)
PROCEDURE SetExportedBy(
    <*UNUSED*> cl: M3Context.Closure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT;
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    iter: SeqM3AST_AS_Used_interface_id.Iter;
    used_intf_id: M3AST_AS.Used_interface_id;
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    TYPECASE cu.as_root OF
    | M3AST_AS.Module(m) =>
        m.pl_tmp_dep_status := Needed;
        (* add myself to exportedby list of all interfaces *) 
        iter := SeqM3AST_AS_Used_interface_id.NewIter(m.sm_export_s);
        WHILE SeqM3AST_AS_Used_interface_id.Next(iter, used_intf_id) DO
          (* be graceful about unresolved names *)
          IF used_intf_id.sm_def # NIL THEN      
            SeqM3AST_AS_Module_id.AddFront(
              NARROW(used_intf_id.sm_def,
                M3AST_AS.Interface_id).pl_isexportedby_s, m.as_id);
          END; (* if *)
        END; (* while *)
    ELSE    
    END; (* typecase *)
  END SetExportedBy;

(*PRIVATE*)
PROCEDURE SetSimpleDependsOn(
    cl: ContextClosure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT;
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    i: M3AST_AS.Interface;
    iter: SeqM3AST_AS_Used_interface_id.Iter;
    used_intf_id: M3AST_AS.Used_interface_id;
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    TYPECASE cu.as_root OF
    | M3AST_AS.Module(m) =>
        iter := SeqM3AST_AS_Used_interface_id.NewIter(m.sm_import_s);
        WHILE SeqM3AST_AS_Used_interface_id.Next(iter, used_intf_id) DO
          i := InterfaceFromUsedId(used_intf_id);
          IF (i # NIL) AND cl.dependsClosure.callback(m, i) THEN (* finally! *)
            AddExporters(m, i);
          END; (* if *)
        END; (* while *)
    ELSE
    END;
  END SetSimpleDependsOn;

(*PRIVATE*)
PROCEDURE CloseDependsOn(
    cl: ContextClosure;
    ut: M3CUnit.Type;
    <*UNUSED*> name: TEXT;
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    TYPECASE cu.as_root OF
    | M3AST_AS.Module =>
        DoCloseDependsOn(cu.as_root, cl);
    ELSE
    END;
  END CloseDependsOn;

(*PRIVATE*)
PROCEDURE DoCloseDependsOn(m: M3AST_AS.Module; cl: ContextClosure) RAISES {}=
  VAR
    iter, iter2: SeqM3AST_AS_Module.Iter;
    tm, ttm: M3AST_AS.Module;
    depends, ndepends: SeqM3AST_AS_Module.T;
    secondary := FALSE;
  BEGIN
    (*Debug Put("DoCloseDependsOn m= %s - ", m);  Debug*)
    IF m.pl_tmp_dep_status = Done THEN 
      (*Debug Put("Done\n");  Debug*)
      RETURN
    ELSIF m.pl_tmp_dep_status = InProgress THEN 
      (*Debug Put("InProgress\n"); Cycle(m); Debug*)
      RETURN
    ELSE 
      (*Debug Put("Starting\n"); Debug*)
      m.pl_tmp_dep_status := InProgress;
      depends := m.pl_dependson_s; 
      REPEAT
        ndepends := NIL;
        iter := SeqM3AST_AS_Module.NewIter(depends);
        WHILE SeqM3AST_AS_Module.Next(iter, tm) DO
          IF tm.pl_tmp_dep_status = Needed THEN DoCloseDependsOn(tm, cl); END;
          iter2 := SeqM3AST_AS_Module.NewIter(tm.pl_dependson_s);
          WHILE SeqM3AST_AS_Module.Next(iter2, ttm) DO
            IF AddModule(m, ttm) THEN
              IF secondary THEN Trap() END;
              SeqM3AST_AS_Module.AddFront(ndepends, ttm);
            END;
          END;
        END; (* while *)
        depends := ndepends;
        secondary := TRUE;
      UNTIL SeqM3AST_AS_Module.Empty(depends);
      m.pl_tmp_dep_status := Done;
      (*Debug Put("DoCloseDependsOn m= %s - ", m); 
         Put("Finished (%s)\n", m);  Debug*)
    END;
  END DoCloseDependsOn;

(*PRIVATE*)
PROCEDURE Trap()= BEGIN END Trap;

(*PRIVATE*)
PROCEDURE InterfaceFromUsedId(used_intf_id: M3AST_AS.Used_interface_id
    ): M3AST_AS.Interface RAISES {}=
  BEGIN
    (* be graceful about unresolved names *)
    IF used_intf_id.sm_def = NIL THEN RETURN NIL
    ELSE RETURN NARROW(used_intf_id.sm_def, M3AST_AS.Interface_id).sm_spec;
    END;
  END InterfaceFromUsedId;

(*PRIVATE*)
PROCEDURE AddModule(m, dm: M3AST_AS.Module): BOOLEAN RAISES {}=
  VAR
    xm: M3AST_AS.Module;
    iter: SeqM3AST_AS_Module.Iter;
  BEGIN
    (* No dependency on self *)
    IF m = dm THEN RETURN FALSE
    ELSE
      (* Have we already seen this *)
      iter := SeqM3AST_AS_Module.NewIter(m.pl_dependson_s);
      WHILE SeqM3AST_AS_Module.Next(iter, xm) DO
        IF xm = dm THEN RETURN FALSE END;
      END; (* while *)
      (*Debug Put("Adding %s to dependson list of %s\n", dm, m); Debug*)
      SeqM3AST_AS_Module.AddFront(m.pl_dependson_s, dm);
      RETURN TRUE;
    END;
  END AddModule;

(*PRIVATE*)
PROCEDURE AddExporters(
    m: M3AST_AS.Module;
    i: M3AST_AS.Interface) RAISES {}=
  VAR
    iter := SeqM3AST_AS_Module_id.NewIter(
        NARROW(i.as_id, M3AST_AS.Interface_id).pl_isexportedby_s);
    mod_id: M3AST_AS.Module_id;
  BEGIN
    (* Add all the exporters of 'i' to the depends-on list of 'm' *)
    (*Debug Put("AddExporters of %s to %s\n", i, m);  Debug*)
    WHILE SeqM3AST_AS_Module_id.Next(iter, mod_id) DO
      EVAL AddModule(m, mod_id.sm_spec);
    END; (* while *)
  END AddExporters;

(*Debug
PROCEDURE Put(t: TEXT; u1, u2: M3AST_AS.UNIT := NIL) RAISES {}=
  VAR t1, t2: TEXT := NIL;
  BEGIN
    IF u1 # NIL THEN t1 := M3CId.ToText(u1.as_id.lx_symrep); END;
    IF u2 # NIL THEN t2 := M3CId.ToText(u2.as_id.lx_symrep) END;
    Wr.PutText(Stdio.stdout, Fmt.F(t, t1, t2));
  END Put;
Debug*)

(*PUBLIC*)
PROCEDURE Default(): Closure RAISES {}=
  BEGIN
    RETURN NEW(Closure, callback := DefaultUses);
  END Default;

(*PRIVATE*)
PROCEDURE DefaultUses(
    <*UNUSED*> cl: Closure_public;
    <*UNUSED*> m: M3AST_AS.Module;
    <*UNUSED*> i: M3AST_AS.Interface): BOOLEAN RAISES {}=
  BEGIN
    (* 'm' depends on 'i' if 'i' is on the sm_import_s list of 'm'. *)
    RETURN TRUE;
  END DefaultUses;

(*****************************************************
(*PRIVATE*)
PROCEDURE Cycle(<*UNUSED*> m: M3AST_AS.Module) RAISES {}=
  BEGIN
  END Cycle;
***************************************************)

BEGIN
END M3LDepends.
