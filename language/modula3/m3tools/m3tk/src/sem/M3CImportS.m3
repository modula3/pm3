MODULE M3CImportS;

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

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

IMPORT M3AST, M3AST_AS;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_IMPORTED;

IMPORT M3ASTNext;
IMPORT M3Context, M3CSrcPos; (* for Standard *)


PROCEDURE Set(an: M3AST.NODE) RAISES {}=
  VAR
    unit: M3AST_AS.UNIT_NORMAL;
    m: M3AST_AS.Module;
    used_intf_id: M3AST_AS.Used_interface_id;
    iter: SeqM3AST_AS_Used_interface_id.Iter;
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Module(module) =>
        m := module;
        unit := module;
    | M3AST_AS.Interface(interface) =>
        m := NIL;
        unit := interface;
    ELSE
      RETURN
    END;

    (* Everybody imports Standard - ugh not next time *)
    AddStandard(unit);

    IF m # NIL THEN
      iter := SeqM3AST_AS_Used_interface_id.NewIter(m.sm_export_s);
      WHILE SeqM3AST_AS_Used_interface_id.Next(iter, used_intf_id) DO
        AddInterface(unit, used_intf_id);
      END; (* while *)
    END; (* if *)
    AddImports(unit, unit.as_import_s);
  END Set;


PROCEDURE AddInterface(
    unit: M3AST_AS.UNIT_NORMAL;
    used_intf_id: M3AST_AS.Used_interface_id)
    RAISES {}=
  VAR
    id := used_intf_id.lx_symrep;
    xused_intf_id: M3AST_AS.Used_interface_id;
    iter := SeqM3AST_AS_Used_interface_id.NewIter(unit.sm_import_s);
  BEGIN
    (* Don't add errant ids *)
    IF id = NIL THEN RETURN END;
    (* sm_import_s is really a set, no duplicates, no self *)
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, xused_intf_id) DO
      IF id = xused_intf_id.lx_symrep THEN
        RETURN
      END; (* if *)
    END; (* while *)
    (* new; add it and everything it imports, but be graceful about
    unresolved names. Do not add self *)
    IF used_intf_id.sm_def # unit.as_id THEN
      SeqM3AST_AS_Used_interface_id.AddRear(unit.sm_import_s, used_intf_id);
      IF used_intf_id.sm_def # NIL THEN      
        VAR spec := NARROW(used_intf_id.sm_def, M3AST_AS.UNIT_ID).sm_spec;
        BEGIN
          IF spec # NIL THEN
            TYPECASE spec OF
            | M3AST_AS.UNIT_NORMAL(unit_normal) =>
                AddImports(unit, unit_normal.as_import_s);
            ELSE (* error elsewhere, (import of generic) *)
            END;
          END;
        END;
      END; (* if *)
    END; (* if *)
  END AddInterface;


PROCEDURE AddStandard(unit: M3AST_AS.UNIT_NORMAL) RAISES {}=
  VAR
    cu: M3AST_AS.Compilation_Unit;
    std_id: M3AST_AS.Interface_id;
    used_intf_id: M3AST_AS.Used_interface_id;
  BEGIN
    cu := M3Context.Standard();
    IF unit = cu.as_root THEN RETURN END; (* this is Standard *)
    std_id := cu.as_root.as_id;
    used_intf_id := NEW(M3AST_AS.Used_interface_id).init();
    used_intf_id.lx_symrep := std_id.lx_symrep;
    used_intf_id.sm_def := std_id;
    used_intf_id.lx_srcpos := M3CSrcPos.Null;
    AddInterface(unit, used_intf_id);
  END AddStandard;


PROCEDURE AddImports(
    unit: M3AST_AS.UNIT_NORMAL;
    seqImported: SeqM3AST_AS_IMPORTED.T)
    RAISES {}=
  VAR
    iter : M3ASTNext.IterImportedId;
    used_interface_id: M3AST_AS.Used_interface_id;
  BEGIN
    iter := M3ASTNext.NewIterImportedId(seqImported);
    WHILE M3ASTNext.ImportedId(iter, used_interface_id) DO
      (* The case "IMPORT I AS J; FROM J IMPORT ..." will cause
         "used_interface_id" to bind to an "Interface_AS_id", which
         doesnt provide any new information, so we ignore it here.
      *)
      TYPECASE used_interface_id.sm_def OF
      | NULL =>
      | M3AST_AS.Interface_AS_id =>
      | M3AST_AS.Interface_id =>   
          AddInterface(unit, used_interface_id);
      ELSE
      END; (* typecase *)
    END; (* while *)
  END AddImports;


BEGIN
END M3CImportS.
