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
(**)
(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3AST_TL_F;

IMPORT PropertyV;

IMPORT M3AST_AS;
IMPORT M3AST_FE_priv_F AS Previous_View;

TYPE
  SRC_NODE = Previous_View.SRC_NODE OBJECT
    tl_pset: PropertyV.Set := PropertyV.NullSet;
  END;

REVEAL
  M3AST_AS.SRC_NODE <: SRC_NODE;

(* pass through names *)

TYPE
  NODE = Previous_View.NODE;  
(*SRC_NODE = Previous_View.SRC_NODE;*)
  SRC_NODE_C = Previous_View.SRC_NODE_C;
  ID = Previous_View.ID;
  LITERAL = Previous_View.LITERAL;
  Whitespace = Previous_View.Whitespace;
  Comment = Previous_View.Comment;
  Pragma = Previous_View.Pragma;
  BadChar = Previous_View.BadChar;
  Token = Previous_View.Token;

  DEF_ID = Previous_View.DEF_ID;
  UNIT_ID = Previous_View.UNIT_ID;
  Module_id = Previous_View.Module_id;
  Interface_id = Previous_View.Interface_id;
  Interface_AS_id = Previous_View.Interface_AS_id;
  F_Interface_id = Previous_View.F_Interface_id;
  TYPED_ID = Previous_View.TYPED_ID;
  FORMAL_ID = Previous_View.FORMAL_ID;
  F_Value_id = Previous_View.F_Value_id;
  F_Var_id = Previous_View.F_Var_id;
  F_Readonly_id = Previous_View.F_Readonly_id;
  Type_id = Previous_View.Type_id;
  Const_id = Previous_View.Const_id;
  Var_id = Previous_View.Var_id;
  Proc_id = Previous_View.Proc_id;
  Enum_id = Previous_View.Enum_id;
  METHOD_OVERRIDE_ID = Previous_View.METHOD_OVERRIDE_ID;
  Method_id = Previous_View.Method_id;
  Override_id = Previous_View.Override_id;
  Field_id = Previous_View.Field_id;
  For_id = Previous_View.For_id;
  Handler_id = Previous_View.Handler_id;
  Tcase_id = Previous_View.Tcase_id;
  With_id = Previous_View.With_id;
  Exc_id = Previous_View.Exc_id;
  USED_ID = Previous_View.USED_ID;
  Used_interface_id = Previous_View.Used_interface_id;
  Used_def_id = Previous_View.Used_def_id;
  Qual_used_id = Previous_View.Qual_used_id;
  Compilation_Unit = Previous_View.Compilation_Unit;
  UNIT = Previous_View.UNIT;
  UNIT_WITH_BODY = Previous_View.UNIT_WITH_BODY;
  UNIT_GEN_DEF = Previous_View.UNIT_GEN_DEF;
  Interface_gen_def = Previous_View.Interface_gen_def;
  Module_gen_def = Previous_View.Module_gen_def;
  UNIT_NORMAL = Previous_View.UNIT_NORMAL;
  Interface = Previous_View.Interface;
  Module = Previous_View.Module;
  UNIT_GEN_INS = Previous_View.UNIT_GEN_INS;
  Interface_gen_ins = Previous_View.Interface_gen_ins;
  Module_gen_ins = Previous_View.Module_gen_ins;
  Unsafe = Previous_View.Unsafe;
  IMPORTED = Previous_View.IMPORTED;
  Simple_import = Previous_View.Simple_import;
  Import_item = Previous_View.Import_item;
  From_import = Previous_View.From_import;
  DECL_REVL = Previous_View.DECL_REVL;
  DECL = Previous_View.DECL;
  Const_decl_s = Previous_View.Const_decl_s;
  Type_decl_s = Previous_View.Type_decl_s;
  Var_decl_s = Previous_View.Var_decl_s;
  Exc_decl_s = Previous_View.Exc_decl_s;
  Proc_decl = Previous_View.Proc_decl;
  Const_decl = Previous_View.Const_decl;
  TYPE_DECL = Previous_View.TYPE_DECL;
  Subtype_decl = Previous_View.Subtype_decl;
  Concrete_decl = Previous_View.Concrete_decl;
  Var_decl = Previous_View.Var_decl;
  Exc_decl = Previous_View.Exc_decl;
  Revelation_s = Previous_View.Revelation_s;
  REVELATION = Previous_View.REVELATION;
  Subtype_reveal = Previous_View.Subtype_reveal;
  Concrete_reveal = Previous_View.Concrete_reveal;
  EXP_TYPE = Previous_View.EXP_TYPE;
  M3TYPE = Previous_View.M3TYPE;
  Named_type = Previous_View.Named_type;
  TYPE_SPEC = Previous_View.TYPE_SPEC;
  FLOAT_TYPE = Previous_View.FLOAT_TYPE;
  Real_type = Previous_View.Real_type;
  LongReal_type = Previous_View.LongReal_type;
  Extended_type = Previous_View.Extended_type;
  Integer_type = Previous_View.Integer_type;
  Null_type = Previous_View.Null_type;
  RefAny_type = Previous_View.RefAny_type;
  Address_type = Previous_View.Address_type;
  Root_type = Previous_View.Root_type;
  Untraced = Previous_View.Untraced;
  Packed_type = Previous_View.Packed_type;
  Array_type = Previous_View.Array_type;
  Enumeration_type = Previous_View.Enumeration_type;
  Set_type = Previous_View.Set_type;
  Subrange_type = Previous_View.Subrange_type;
  RANGE_EXP = Previous_View.RANGE_EXP;
  Range_EXP = Previous_View.Range_EXP;
  Range = Previous_View.Range;
  Record_type = Previous_View.Record_type;
  Fields = Previous_View.Fields;
  BRANDED_TYPE = Previous_View.BRANDED_TYPE;
  Brand = Previous_View.Brand;
  Ref_type = Previous_View.Ref_type;
  Object_type = Previous_View.Object_type;
  METHOD_OVERRIDE = Previous_View.METHOD_OVERRIDE;
  Method = Previous_View.Method;
  Override = Previous_View.Override;
  Procedure_type = Previous_View.Procedure_type;
  Formal_param = Previous_View.Formal_param;
  RAISEES = Previous_View.RAISEES;
  Raisees_some = Previous_View.Raisees_some;
  Raisees_any = Previous_View.Raisees_any;
  Opaque_type = Previous_View.Opaque_type;
  EXP = Previous_View.EXP;
  NUMERIC_LITERAL = Previous_View.NUMERIC_LITERAL;
  Integer_literal = Previous_View.Integer_literal;
  Real_literal = Previous_View.Real_literal;
  LongReal_literal = Previous_View.LongReal_literal;
  Extended_literal = Previous_View.Extended_literal;
  Char_literal = Previous_View.Char_literal;
  Text_literal = Previous_View.Text_literal;
  Nil_literal = Previous_View.Nil_literal;
  Exp_used_id = Previous_View.Exp_used_id;
  Call = Previous_View.Call;
  NEWCall = Previous_View.NEWCall;
  Actual = Previous_View.Actual;
  Index = Previous_View.Index;
  Constructor = Previous_View.Constructor;
  Propagate = Previous_View.Propagate;
  CONS_ELEM = Previous_View.CONS_ELEM;
  RANGE_EXP_elem = Previous_View.RANGE_EXP_elem;
  Actual_elem = Previous_View.Actual_elem;
  BINARY = Previous_View.BINARY;
  Plus = Previous_View.Plus;
  Minus = Previous_View.Minus;
  Times = Previous_View.Times;
  Rdiv = Previous_View.Rdiv;
  Textcat = Previous_View.Textcat;
  Div = Previous_View.Div;
  Mod = Previous_View.Mod;
  Eq = Previous_View.Eq;
  Ne = Previous_View.Ne;
  Gt = Previous_View.Gt;
  Lt = Previous_View.Lt;
  Ge = Previous_View.Ge;
  Le = Previous_View.Le;
  And = Previous_View.And;
  Or = Previous_View.Or;
  In = Previous_View.In;
  Select = Previous_View.Select;
  UNARY = Previous_View.UNARY;
  Not = Previous_View.Not;
  Unaryplus = Previous_View.Unaryplus;
  Unaryminus = Previous_View.Unaryminus;
  Deref = Previous_View.Deref;
  STM = Previous_View.STM;
  STM_WSS = Previous_View.STM_WSS;
  SUBSTM_WSS = Previous_View.SUBSTM_WSS;
  Assign_st = Previous_View.Assign_st;
  Call_st = Previous_View.Call_st;
  Case_st = Previous_View.Case_st;
  Case = Previous_View.Case;
  Else_stm = Previous_View.Else_stm;
  Eval_st = Previous_View.Eval_st;
  Exit_st = Previous_View.Exit_st;
  Raise_st = Previous_View.Raise_st;
  Typecase_st = Previous_View.Typecase_st;
  Tcase = Previous_View.Tcase;
  Handler = Previous_View.Handler;
  Return_st = Previous_View.Return_st;
  For_st = Previous_View.For_st;
  By = Previous_View.By;
  If_st = Previous_View.If_st;
  Elsif = Previous_View.Elsif;
  Lock_st = Previous_View.Lock_st;
  Loop_st = Previous_View.Loop_st;
  Repeat_st = Previous_View.Repeat_st;
  Try_st = Previous_View.Try_st;
  TRY_TAIL = Previous_View.TRY_TAIL;
  Try_except = Previous_View.Try_except;
  Try_finally = Previous_View.Try_finally;
  While_st = Previous_View.While_st;
  With_st = Previous_View.With_st;
  Binding = Previous_View.Binding;
  Block = Previous_View.Block;
  Bad_EXP = Previous_View.Bad_EXP;
  Bad_M3TYPE = Previous_View.Bad_M3TYPE;
  Bad_STM = Previous_View.Bad_STM;
  Opaque_type_Revln = Previous_View.Opaque_type_Revln;
  INIT_ID = Previous_View.INIT_ID;
  CCV_ID = Previous_View.CCV_ID;
  RECOBJ_ID = Previous_View.RECOBJ_ID;
  REDEF_ID = Previous_View.REDEF_ID;
  Type_type = Previous_View.Type_type;
  Any_type = Previous_View.Any_type;
  Void_type = Previous_View.Void_type;
  TypeActual = Previous_View.TypeActual;
  SCOPE = Previous_View.SCOPE;

END M3AST_TL_F.
