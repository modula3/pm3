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
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE M3ASTOp_AS;

(* This view adds concrete methods to the M3AST_AS objects to satisfy
the AST_{Init,Name,WalkRep,Iter,CopyRep} abstract methods.  *)

IMPORT M3AST_AS;
IMPORT M3AST_AS_Init, M3AST_PG_Init;
IMPORT M3AST_AS_Display;
IMPORT M3AST_AS_Walk;
IMPORT M3AST_AS_Copy;
IMPORT M3AST_AS_Iter;
IMPORT M3AST_AS_Name;
IMPORT M3AST_AS_F AS Previous_View;

TYPE
  SRC_NODE_C = Previous_View.SRC_NODE_C OBJECT
  OVERRIDES
    display  := M3AST_AS_Display.SRC_NODE_C;
  END;

  ID = Previous_View.ID OBJECT
  OVERRIDES
    display := M3AST_AS_Display.ID;
  END;

  LITERAL = Previous_View.LITERAL OBJECT
  OVERRIDES
    display  := M3AST_AS_Display.LITERAL;
  END;

  Whitespace = Previous_View.Whitespace OBJECT
  OVERRIDES
    display  := M3AST_AS_Display.Whitespace;
    name := M3AST_AS_Name.Whitespace;
  END;

  Comment = Previous_View.Comment OBJECT
  OVERRIDES
    display  := M3AST_AS_Display.Comment;
    name := M3AST_AS_Name.Comment;
  END;

  Pragma = Previous_View.Pragma OBJECT
  OVERRIDES
    display  := M3AST_AS_Display.Pragma;
    name := M3AST_AS_Name.Pragma;
  END;

  BadChar = Previous_View.BadChar OBJECT
  OVERRIDES
    display  := M3AST_AS_Display.BadChar;
    name  := M3AST_AS_Name.BadChar;
  END;

  Token = Previous_View.Token OBJECT
  OVERRIDES
    display  := M3AST_AS_Display.Token;
    name := M3AST_AS_Name.Token;
  END;

  BINARY = Previous_View.BINARY OBJECT
  OVERRIDES
    walk := M3AST_AS_Walk.BINARY;
    newIter := M3AST_AS_Iter.BINARY_newIter;
  END;

  UNARY = Previous_View.UNARY OBJECT
  OVERRIDES
    walk := M3AST_AS_Walk.UNARY;
    newIter := M3AST_AS_Iter.UNARY_newIter;
  END;

  Module_id = Previous_View.Module_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Module_id;
    name := M3AST_AS_Name.Module_id;
  END;            
  Interface_id = Previous_View.Interface_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Interface_id;
    name := M3AST_AS_Name.Interface_id;
  END;         
  F_Interface_id = Previous_View.F_Interface_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.F_Interface_id;
    name := M3AST_AS_Name.F_Interface_id;
  END;         
  Interface_AS_id = Previous_View.Interface_AS_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Interface_AS_id;
    name := M3AST_AS_Name.Interface_AS_id;
  END;         
  F_Value_id = Previous_View.F_Value_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.F_Value_id;
    name := M3AST_AS_Name.F_Value_id;
  END;         
  F_Var_id = Previous_View.F_Var_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.F_Var_id;
    name := M3AST_AS_Name.F_Var_id;
  END;           
  F_Readonly_id = Previous_View.F_Readonly_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.F_Readonly_id;
    name := M3AST_AS_Name.F_Readonly_id;
  END;      
  Type_id = Previous_View.Type_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Type_id;
    name := M3AST_AS_Name.Type_id;
  END;             
  Const_id = Previous_View.Const_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Const_id;
    name := M3AST_AS_Name.Const_id;
  END;            
  Var_id = Previous_View.Var_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Var_id;
    name := M3AST_AS_Name.Var_id;
  END;               
  Proc_id = Previous_View.Proc_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Proc_id;
    name := M3AST_AS_Name.Proc_id;
  END;             
  Enum_id = Previous_View.Enum_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Enum_id;
    name := M3AST_AS_Name.Enum_id;
  END;             
  Method_id = Previous_View.Method_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Method_id;
    name := M3AST_AS_Name.Method_id;
  END;           
  Override_id = Previous_View.Override_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Override_id;
    name := M3AST_AS_Name.Override_id;
  END;           
  Field_id = Previous_View.Field_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Field_id;
    name := M3AST_AS_Name.Field_id;
  END;            
  For_id = Previous_View.For_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.For_id;
    name := M3AST_AS_Name.For_id;
  END;              
  Handler_id = Previous_View.Handler_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Handler_id;
    name := M3AST_AS_Name.Handler_id;
  END;          
  Tcase_id = Previous_View.Tcase_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Tcase_id;
    name := M3AST_AS_Name.Tcase_id;
  END;            
  With_id = Previous_View.With_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.With_id;
    name := M3AST_AS_Name.With_id;
  END;             
  Exc_id = Previous_View.Exc_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Exc_id;
    name := M3AST_AS_Name.Exc_id;
  END;              
  Used_interface_id = Previous_View.Used_interface_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Used_interface_id;
    name := M3AST_AS_Name.Used_interface_id;
  END;    
  Used_def_id = Previous_View.Used_def_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Used_def_id;
    name := M3AST_AS_Name.Used_def_id;
  END;          
  Qual_used_id = Previous_View.Qual_used_id OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Qual_used_id;
    name := M3AST_AS_Name.Qual_used_id;
    walk := M3AST_AS_Walk.Qual_used_id;
    newIter := M3AST_AS_Iter.Qual_used_id_newIter;
  END;         
  Compilation_Unit = Previous_View.Compilation_Unit OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Compilation_Unit;
    name := M3AST_AS_Name.Compilation_Unit;
    walk := M3AST_AS_Walk.Compilation_Unit; 
    newIter := M3AST_AS_Iter.Compilation_Unit_newIter;
  END;
  Interface_gen_def = Previous_View.Interface_gen_def OBJECT
  OVERRIDES
    init := M3AST_PG_Init.Interface_gen_def;
    copy := M3AST_AS_Copy.Interface_gen_def;
    name := M3AST_AS_Name.Interface_gen_def;
    walk := M3AST_AS_Walk.Interface_gen_def; 
    newIter := M3AST_AS_Iter.Interface_gen_def_newIter;
  END;
  Module_gen_def = Previous_View.Module_gen_def OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Module_gen_def;
    name := M3AST_AS_Name.Module_gen_def;
    walk := M3AST_AS_Walk.Module_gen_def; 
    newIter := M3AST_AS_Iter.Module_gen_def_newIter;
  END;
  Interface_gen_ins = Previous_View.Interface_gen_ins OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Interface_gen_ins;
    name := M3AST_AS_Name.Interface_gen_ins;
    walk := M3AST_AS_Walk.Interface_gen_ins; 
    newIter := M3AST_AS_Iter.Interface_gen_ins_newIter;
  END;
  Module_gen_ins = Previous_View.Module_gen_ins OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Module_gen_ins;
    name := M3AST_AS_Name.Module_gen_ins;
    walk := M3AST_AS_Walk.Module_gen_ins; 
    newIter := M3AST_AS_Iter.Module_gen_ins_newIter;
  END;
  Interface = Previous_View.Interface OBJECT
  OVERRIDES
    init := M3AST_PG_Init.Interface;
    copy := M3AST_AS_Copy.Interface;
    name := M3AST_AS_Name.Interface;
    walk := M3AST_AS_Walk.Interface; 
    newIter := M3AST_AS_Iter.Interface_newIter;
  END;
  Module = Previous_View.Module OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Module;
    name := M3AST_AS_Name.Module;
    walk := M3AST_AS_Walk.Module; 
    newIter := M3AST_AS_Iter.Module_newIter;
  END;
  Unsafe = Previous_View.Unsafe OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Unsafe;
    name := M3AST_AS_Name.Unsafe;
  END;
  Import_item = Previous_View.Import_item OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Import_item;
    name := M3AST_AS_Name.Import_item;
    walk := M3AST_AS_Walk.Import_item; 
    newIter := M3AST_AS_Iter.Import_item_newIter;
  END;       
  Simple_import = Previous_View.Simple_import OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Simple_import;
    name := M3AST_AS_Name.Simple_import;
    walk := M3AST_AS_Walk.Simple_import; 
    newIter := M3AST_AS_Iter.Simple_import_newIter;
  END;       
  From_import = Previous_View.From_import OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.From_import;
    name := M3AST_AS_Name.From_import;
    walk := M3AST_AS_Walk.From_import; 
    newIter := M3AST_AS_Iter.From_import_newIter;
  END;         
  Revelation_s = Previous_View.Revelation_s OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Revelation_s;
    name := M3AST_AS_Name.Revelation_s;
    walk := M3AST_AS_Walk.Revelation_s; 
    newIter := M3AST_AS_Iter.Revelation_s_newIter;
  END;       
  Const_decl_s = Previous_View.Const_decl_s OBJECT
  OVERRIDES
    init := M3AST_PG_Init.DECL;
    copy := M3AST_AS_Copy.Const_decl_s;
    name := M3AST_AS_Name.Const_decl_s;
    walk := M3AST_AS_Walk.Const_decl_s; 
    newIter := M3AST_AS_Iter.Const_decl_s_newIter;
  END;            
  Type_decl_s = Previous_View.Type_decl_s OBJECT
  OVERRIDES
    init := M3AST_PG_Init.DECL;
    copy := M3AST_AS_Copy.Type_decl_s;
    name := M3AST_AS_Name.Type_decl_s;
    walk := M3AST_AS_Walk.Type_decl_s; 
    newIter := M3AST_AS_Iter.Type_decl_s_newIter;
  END;             
  Var_decl_s = Previous_View.Var_decl_s OBJECT
  OVERRIDES
    init := M3AST_PG_Init.DECL;
    copy := M3AST_AS_Copy.Var_decl_s;
    name := M3AST_AS_Name.Var_decl_s;
    walk := M3AST_AS_Walk.Var_decl_s; 
    newIter := M3AST_AS_Iter.Var_decl_s_newIter;
  END;              
  Exc_decl_s = Previous_View.Exc_decl_s OBJECT
  OVERRIDES
    init := M3AST_PG_Init.DECL;
    copy := M3AST_AS_Copy.Exc_decl_s;
    name := M3AST_AS_Name.Exc_decl_s;
    walk := M3AST_AS_Walk.Exc_decl_s; 
    newIter := M3AST_AS_Iter.Exc_decl_s_newIter;
  END;              
  Proc_decl = Previous_View.Proc_decl OBJECT
  OVERRIDES
    init := M3AST_PG_Init.DECL;
    copy := M3AST_AS_Copy.Proc_decl;
    name := M3AST_AS_Name.Proc_decl;
    walk := M3AST_AS_Walk.Proc_decl; 
    newIter := M3AST_AS_Iter.Proc_decl_newIter;
  END;               
  Const_decl = Previous_View.Const_decl OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Const_decl;
    name := M3AST_AS_Name.Const_decl;
    walk := M3AST_AS_Walk.Const_decl; 
    newIter := M3AST_AS_Iter.Const_decl_newIter;
  END;          
  Var_decl = Previous_View.Var_decl OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Var_decl;
    name := M3AST_AS_Name.Var_decl;
    walk := M3AST_AS_Walk.Var_decl; 
    newIter := M3AST_AS_Iter.Var_decl_newIter;
  END;            
  Exc_decl = Previous_View.Exc_decl OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Exc_decl;
    name := M3AST_AS_Name.Exc_decl;
    walk := M3AST_AS_Walk.Exc_decl; 
    newIter := M3AST_AS_Iter.Exc_decl_newIter;
  END;            
  Subtype_decl = Previous_View.Subtype_decl OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Subtype_decl;
    name := M3AST_AS_Name.Subtype_decl;
    walk := M3AST_AS_Walk.Subtype_decl; 
    newIter := M3AST_AS_Iter.Subtype_decl_newIter;
  END;       
  Concrete_decl = Previous_View.Concrete_decl OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Concrete_decl;
    name := M3AST_AS_Name.Concrete_decl;
    walk := M3AST_AS_Walk.Concrete_decl; 
    newIter := M3AST_AS_Iter.Concrete_decl_newIter;
  END;      
  Subtype_reveal = Previous_View.Subtype_reveal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Subtype_reveal;
    name := M3AST_AS_Name.Subtype_reveal;
    walk := M3AST_AS_Walk.Subtype_reveal; 
    newIter := M3AST_AS_Iter.Subtype_reveal_newIter;
  END;    
  Concrete_reveal = Previous_View.Concrete_reveal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Concrete_reveal;
    name := M3AST_AS_Name.Concrete_reveal;
    walk := M3AST_AS_Walk.Concrete_reveal; 
    newIter := M3AST_AS_Iter.Concrete_reveal_newIter;
  END;   
  Named_type = Previous_View.Named_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Named_type;
    name := M3AST_AS_Name.Named_type;
    walk := M3AST_AS_Walk.Named_type; 
    newIter := M3AST_AS_Iter.Named_type_newIter;
  END;            
  Integer_type = Previous_View.Integer_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Integer_type;
    name := M3AST_AS_Name.Integer_type;
  END;
  WideChar_type = Previous_View.WideChar_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.WideChar_type;
    name := M3AST_AS_Name.WideChar_type;
  END;
  Real_type = Previous_View.Real_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Real_type;
    name := M3AST_AS_Name.Real_type;
  END;
  LongReal_type = Previous_View.LongReal_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.LongReal_type;
    name := M3AST_AS_Name.LongReal_type;
  END;
  Extended_type = Previous_View.Extended_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Extended_type;
    name := M3AST_AS_Name.Extended_type;
  END;
  Null_type = Previous_View.Null_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Null_type;
    name := M3AST_AS_Name.Null_type;
  END;
  RefAny_type = Previous_View.RefAny_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.RefAny_type;
    name := M3AST_AS_Name.RefAny_type;
  END;
  Address_type = Previous_View.Address_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Address_type;
    name := M3AST_AS_Name.Address_type;
  END;
  Root_type = Previous_View.Root_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Root_type;
    name := M3AST_AS_Name.Root_type;
    walk := M3AST_AS_Walk.Root_type;
    newIter := M3AST_AS_Iter.Root_type_newIter;
  END;
  Enumeration_type = Previous_View.Enumeration_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Enumeration_type;
    name := M3AST_AS_Name.Enumeration_type;
    walk := M3AST_AS_Walk.Enumeration_type; 
    newIter := M3AST_AS_Iter.Enumeration_type_newIter;
  END;
  Subrange_type = Previous_View.Subrange_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Subrange_type;
    name := M3AST_AS_Name.Subrange_type;
    walk := M3AST_AS_Walk.Subrange_type; 
    newIter := M3AST_AS_Iter.Subrange_type_newIter;
  END;
  Array_type = Previous_View.Array_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Array_type;
    name := M3AST_AS_Name.Array_type;
    walk := M3AST_AS_Walk.Array_type; 
    newIter := M3AST_AS_Iter.Array_type_newIter;
  END;
  Record_type = Previous_View.Record_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Record_type;
    name := M3AST_AS_Name.Record_type;
    walk := M3AST_AS_Walk.Record_type; 
    newIter := M3AST_AS_Iter.Record_type_newIter;
  END;
  Object_type = Previous_View.Object_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Object_type;
    name := M3AST_AS_Name.Object_type;
    walk := M3AST_AS_Walk.Object_type; 
    newIter := M3AST_AS_Iter.Object_type_newIter;
  END;
  Set_type = Previous_View.Set_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Set_type;
    name := M3AST_AS_Name.Set_type;
    walk := M3AST_AS_Walk.Set_type; 
    newIter := M3AST_AS_Iter.Set_type_newIter;
  END;
  Procedure_type = Previous_View.Procedure_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Procedure_type;
    name := M3AST_AS_Name.Procedure_type;
    walk := M3AST_AS_Walk.Procedure_type; 
    newIter := M3AST_AS_Iter.Procedure_type_newIter;
  END;
  Ref_type = Previous_View.Ref_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Ref_type;
    name := M3AST_AS_Name.Ref_type;
    walk := M3AST_AS_Walk.Ref_type; 
    newIter := M3AST_AS_Iter.Ref_type_newIter;
  END;
  Packed_type = Previous_View.Packed_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Packed_type;
    name := M3AST_AS_Name.Packed_type;
    walk := M3AST_AS_Walk.Packed_type; 
    newIter := M3AST_AS_Iter.Packed_type_newIter;
  END;
  Opaque_type = Previous_View.Opaque_type OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Opaque_type;
    name := M3AST_AS_Name.Opaque_type;
    walk := M3AST_AS_Walk.Opaque_type; 
    newIter := M3AST_AS_Iter.Opaque_type_newIter;
  END;
  Brand = Previous_View.Brand OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Brand;
    name := M3AST_AS_Name.Brand;
    walk := M3AST_AS_Walk.Brand; 
    newIter := M3AST_AS_Iter.Brand_newIter;
  END;
  Untraced = Previous_View.Untraced OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Untraced;
    name := M3AST_AS_Name.Untraced;
  END;
  Fields = Previous_View.Fields OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Fields;
    name := M3AST_AS_Name.Fields;
    walk := M3AST_AS_Walk.Fields; 
    newIter := M3AST_AS_Iter.Fields_newIter;
  END;
  Method = Previous_View.Method OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Method;
    name := M3AST_AS_Name.Method;
    walk := M3AST_AS_Walk.Method; 
    newIter := M3AST_AS_Iter.Method_newIter;
  END;
  Override = Previous_View.Override OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Override;
    name := M3AST_AS_Name.Override;
    walk := M3AST_AS_Walk.Override; 
    newIter := M3AST_AS_Iter.Override_newIter;
  END;
  Formal_param = Previous_View.Formal_param OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Formal_param;
    name := M3AST_AS_Name.Formal_param;
    walk := M3AST_AS_Walk.Formal_param; 
    newIter := M3AST_AS_Iter.Formal_param_newIter;
  END;
  Raisees_some = Previous_View.Raisees_some OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Raisees_some;
    name := M3AST_AS_Name.Raisees_some;
    walk := M3AST_AS_Walk.Raisees_some; 
    newIter := M3AST_AS_Iter.Raisees_some_newIter;
  END;
  Raisees_any = Previous_View.Raisees_any OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Raisees_any;
    name := M3AST_AS_Name.Raisees_any;
  END;
  Range = Previous_View.Range OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Range;
    name := M3AST_AS_Name.Range;
    walk := M3AST_AS_Walk.Range; 
    newIter := M3AST_AS_Iter.Range_newIter;
  END;
  Range_EXP = Previous_View.Range_EXP OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Range_EXP;
    name := M3AST_AS_Name.Range_EXP;
    walk := M3AST_AS_Walk.Range_EXP; 
    newIter := M3AST_AS_Iter.Range_EXP_newIter;
  END;
  Integer_literal = Previous_View.Integer_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Integer_literal;
    name := M3AST_AS_Name.Integer_literal;
  END;
  Real_literal = Previous_View.Real_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Real_literal;
    name := M3AST_AS_Name.Real_literal;
  END;
  LongReal_literal = Previous_View.LongReal_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.LongReal_literal;
    name := M3AST_AS_Name.LongReal_literal;
  END;
  Extended_literal = Previous_View.Extended_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Extended_literal;
    name := M3AST_AS_Name.Extended_literal;
  END;
  Text_literal = Previous_View.Text_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Text_literal;
    name := M3AST_AS_Name.Text_literal;
  END;
  WideText_literal = Previous_View.WideText_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.WideText_literal;
    name := M3AST_AS_Name.WideText_literal;
  END;
  Char_literal = Previous_View.Char_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Char_literal;
    name := M3AST_AS_Name.Char_literal;
  END;
  WideChar_literal = Previous_View.WideChar_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.WideChar_literal;
    name := M3AST_AS_Name.WideChar_literal;
  END;
  Nil_literal = Previous_View.Nil_literal OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Nil_literal;
    name := M3AST_AS_Name.Nil_literal;
  END;
  Exp_used_id = Previous_View.Exp_used_id OBJECT
  OVERRIDES
    init := M3AST_AS_Init.Exp_used_id;
    copy := M3AST_AS_Copy.Exp_used_id;
    name := M3AST_AS_Name.Exp_used_id;
  END;      
  Constructor = Previous_View.Constructor OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Constructor;
    name := M3AST_AS_Name.Constructor;
    walk := M3AST_AS_Walk.Constructor; 
    newIter := M3AST_AS_Iter.Constructor_newIter;
  END;
  RANGE_EXP_elem = Previous_View.RANGE_EXP_elem OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.RANGE_EXP_elem;
    name := M3AST_AS_Name.RANGE_EXP_elem;
    walk := M3AST_AS_Walk.RANGE_EXP_elem; 
    newIter := M3AST_AS_Iter.RANGE_EXP_elem_newIter;
  END;
  Actual_elem = Previous_View.Actual_elem OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Actual_elem;
    name := M3AST_AS_Name.Actual_elem;
    walk := M3AST_AS_Walk.Actual_elem; 
    newIter := M3AST_AS_Iter.Actual_elem_newIter;
  END;
  Propagate = Previous_View.Propagate OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Propagate;
    name := M3AST_AS_Name.Propagate;
  END;
  Plus = Previous_View.Plus OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Plus;
    name := M3AST_AS_Name.Plus;
  END;
  Minus = Previous_View.Minus OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Minus;
    name := M3AST_AS_Name.Minus;
  END;
  Times = Previous_View.Times OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Times;
    name := M3AST_AS_Name.Times;
  END;
  Rdiv = Previous_View.Rdiv OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Rdiv;
    name := M3AST_AS_Name.Rdiv;
  END;
  Textcat = Previous_View.Textcat OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Textcat;
    name := M3AST_AS_Name.Textcat;
  END;
  Div = Previous_View.Div OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Div;
    name := M3AST_AS_Name.Div;
  END;
  Mod = Previous_View.Mod OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Mod;
    name := M3AST_AS_Name.Mod;
  END;
  Eq = Previous_View.Eq OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Eq;
    name := M3AST_AS_Name.Eq;
  END;
  Ne = Previous_View.Ne OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Ne;
    name := M3AST_AS_Name.Ne;
  END;
  Gt = Previous_View.Gt OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Gt;
    name := M3AST_AS_Name.Gt;
  END;
  Lt = Previous_View.Lt OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Lt;
    name := M3AST_AS_Name.Lt;
  END;
  Ge = Previous_View.Ge OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Ge;
    name := M3AST_AS_Name.Ge;
  END;
  Le = Previous_View.Le OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Le;
    name := M3AST_AS_Name.Le;
  END;
  And = Previous_View.And OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.And;
    name := M3AST_AS_Name.And;
  END;
  Or = Previous_View.Or OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Or;
    name := M3AST_AS_Name.Or;
  END;
  In = Previous_View.In OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.In;
    name := M3AST_AS_Name.In;
  END;
  Not = Previous_View.Not OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Not;
    name := M3AST_AS_Name.Not;
  END;
  Unaryplus = Previous_View.Unaryplus OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Unaryplus;
    name := M3AST_AS_Name.Unaryplus;
  END;
  Unaryminus = Previous_View.Unaryminus OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Unaryminus;
    name := M3AST_AS_Name.Unaryminus;
  END;
  Deref = Previous_View.Deref OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Deref;
    name := M3AST_AS_Name.Deref;
  END;
  Call = Previous_View.Call OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Call;
    name := M3AST_AS_Name.Call;
    walk := M3AST_AS_Walk.Call; 
    newIter := M3AST_AS_Iter.Call_newIter;
  END;
  NEWCall = Previous_View.NEWCall OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.NEWCall;
    name := M3AST_AS_Name.NEWCall;
    walk := M3AST_AS_Walk.Call; 
    newIter := M3AST_AS_Iter.Call_newIter;
  END;
  Index = Previous_View.Index OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Index;
    name := M3AST_AS_Name.Index;
    walk := M3AST_AS_Walk.Index; 
    newIter := M3AST_AS_Iter.Index_newIter;
  END;
  Actual = Previous_View.Actual OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Actual;
    name := M3AST_AS_Name.Actual;
    walk := M3AST_AS_Walk.Actual; 
    newIter := M3AST_AS_Iter.Actual_newIter;
  END;
  Select = Previous_View.Select OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Select;
    name := M3AST_AS_Name.Select;
    walk := M3AST_AS_Walk.Select;
    newIter := M3AST_AS_Iter.Select_newIter;
  END;
  Assign_st = Previous_View.Assign_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Assign_st;
    name := M3AST_AS_Name.Assign_st;
    walk := M3AST_AS_Walk.Assign_st; 
    newIter := M3AST_AS_Iter.Assign_st_newIter;
  END;
  Call_st = Previous_View.Call_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Call_st;
    name := M3AST_AS_Name.Call_st;
    walk := M3AST_AS_Walk.Call_st; 
    newIter := M3AST_AS_Iter.Call_st_newIter;
  END;
  Case_st = Previous_View.Case_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Case_st;
    name := M3AST_AS_Name.Case_st;
    walk := M3AST_AS_Walk.Case_st; 
    newIter := M3AST_AS_Iter.Case_st_newIter;
  END;
  Eval_st = Previous_View.Eval_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Eval_st;
    name := M3AST_AS_Name.Eval_st;
    walk := M3AST_AS_Walk.Eval_st; 
    newIter := M3AST_AS_Iter.Eval_st_newIter;
  END;
  Exit_st = Previous_View.Exit_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Exit_st;
    name := M3AST_AS_Name.Exit_st;
  END;
  For_st = Previous_View.For_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.For_st;
    name := M3AST_AS_Name.For_st;
    walk := M3AST_AS_Walk.For_st; 
    newIter := M3AST_AS_Iter.For_st_newIter;
  END;
  If_st = Previous_View.If_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.If_st;
    name := M3AST_AS_Name.If_st;
    walk := M3AST_AS_Walk.If_st; 
    newIter := M3AST_AS_Iter.If_st_newIter;
  END;
  Lock_st = Previous_View.Lock_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Lock_st;
    name := M3AST_AS_Name.Lock_st;
    walk := M3AST_AS_Walk.Lock_st; 
    newIter := M3AST_AS_Iter.Lock_st_newIter;
  END;
  Loop_st = Previous_View.Loop_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Loop_st;
    name := M3AST_AS_Name.Loop_st;
    walk := M3AST_AS_Walk.Loop_st; 
    newIter := M3AST_AS_Iter.Loop_st_newIter;
  END;
  Raise_st = Previous_View.Raise_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Raise_st;
    name := M3AST_AS_Name.Raise_st;
    walk := M3AST_AS_Walk.Raise_st; 
    newIter := M3AST_AS_Iter.Raise_st_newIter;
  END;
  Repeat_st = Previous_View.Repeat_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Repeat_st;
    name := M3AST_AS_Name.Repeat_st;
    walk := M3AST_AS_Walk.Repeat_st; 
    newIter := M3AST_AS_Iter.Repeat_st_newIter;
  END;
  Return_st = Previous_View.Return_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Return_st;
    name := M3AST_AS_Name.Return_st;
    walk := M3AST_AS_Walk.Return_st; 
    newIter := M3AST_AS_Iter.Return_st_newIter;
  END;
  Try_st = Previous_View.Try_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Try_st;
    name := M3AST_AS_Name.Try_st;
    walk := M3AST_AS_Walk.Try_st; 
    newIter := M3AST_AS_Iter.Try_st_newIter;
  END;
  Typecase_st = Previous_View.Typecase_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Typecase_st;
    name := M3AST_AS_Name.Typecase_st;
    walk := M3AST_AS_Walk.Typecase_st; 
    newIter := M3AST_AS_Iter.Typecase_st_newIter;
  END;
  While_st = Previous_View.While_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.While_st;
    name := M3AST_AS_Name.While_st;
    walk := M3AST_AS_Walk.While_st; 
    newIter := M3AST_AS_Iter.While_st_newIter;
  END;
  With_st = Previous_View.With_st OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.With_st;
    name := M3AST_AS_Name.With_st;
    walk := M3AST_AS_Walk.With_st; 
    newIter := M3AST_AS_Iter.With_st_newIter;
  END;
  Block = Previous_View.Block OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Block;
    name := M3AST_AS_Name.Block;
    walk := M3AST_AS_Walk.Block; 
    newIter := M3AST_AS_Iter.Block_newIter;
  END;
  Case = Previous_View.Case OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Case;
    name := M3AST_AS_Name.Case;
    walk := M3AST_AS_Walk.Case;
    newIter := M3AST_AS_Iter.Case_newIter;
  END;
  Else_stm = Previous_View.Else_stm OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Else_stm;
    name := M3AST_AS_Name.Else_stm;
    walk := M3AST_AS_Walk.Else_stm; 
    newIter := M3AST_AS_Iter.Else_stm_newIter;
  END;
  By = Previous_View.By OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.By;
    name := M3AST_AS_Name.By;
    walk := M3AST_AS_Walk.By; 
    newIter := M3AST_AS_Iter.By_newIter;
  END;
  Elsif = Previous_View.Elsif OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Elsif;
    name := M3AST_AS_Name.Elsif;
    walk := M3AST_AS_Walk.Elsif; 
    newIter := M3AST_AS_Iter.Elsif_newIter;
  END;
  Try_except = Previous_View.Try_except OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Try_except;
    name := M3AST_AS_Name.Try_except;
    walk := M3AST_AS_Walk.Try_except; 
    newIter := M3AST_AS_Iter.Try_except_newIter;
  END;
  Try_finally = Previous_View.Try_finally OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Try_finally;
    name := M3AST_AS_Name.Try_finally;
    walk := M3AST_AS_Walk.Try_finally; 
    newIter := M3AST_AS_Iter.Try_finally_newIter;
  END;
  Tcase = Previous_View.Tcase OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Tcase;
    name := M3AST_AS_Name.Tcase;
    walk := M3AST_AS_Walk.Tcase; 
    newIter := M3AST_AS_Iter.Tcase_newIter;
  END;
  Handler = Previous_View.Handler OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Handler;
    name := M3AST_AS_Name.Handler;
    walk := M3AST_AS_Walk.Handler; 
    newIter := M3AST_AS_Iter.Handler_newIter;
  END;
  Binding = Previous_View.Binding OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Binding;
    name := M3AST_AS_Name.Binding;
    walk := M3AST_AS_Walk.Binding; 
    newIter := M3AST_AS_Iter.Binding_newIter;
  END;
  Bad_EXP = Previous_View.Bad_EXP OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Bad_EXP;
    name := M3AST_AS_Name.Bad_EXP;
  END;
  Bad_M3TYPE = Previous_View.Bad_M3TYPE OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Bad_M3TYPE;
    name := M3AST_AS_Name.Bad_M3TYPE;
  END;
  Bad_STM = Previous_View.Bad_STM OBJECT
  OVERRIDES
    copy := M3AST_AS_Copy.Bad_STM;
    name := M3AST_AS_Name.Bad_STM;
  END;

REVEAL
  M3AST_AS.Module_id <: Module_id;            
  M3AST_AS.Interface_id <: Interface_id;         
  M3AST_AS.F_Value_id <: F_Value_id;         
  M3AST_AS.F_Var_id <: F_Var_id;           
  M3AST_AS.F_Readonly_id <: F_Readonly_id;      
  M3AST_AS.Type_id <: Type_id;             
  M3AST_AS.Const_id <: Const_id;            
  M3AST_AS.Var_id <: Var_id;               
  M3AST_AS.Proc_id <: Proc_id;             
  M3AST_AS.Enum_id <: Enum_id;             
  M3AST_AS.Method_id <: Method_id;           
  M3AST_AS.Field_id <: Field_id;            
  M3AST_AS.For_id <: For_id;              
  M3AST_AS.Handler_id <: Handler_id;          
  M3AST_AS.Tcase_id <: Tcase_id;            
  M3AST_AS.With_id <: With_id;             
  M3AST_AS.Exc_id <: Exc_id;              
  M3AST_AS.Used_interface_id <: Used_interface_id;    
  M3AST_AS.Used_def_id <: Used_def_id;          
  M3AST_AS.Qual_used_id <: Qual_used_id;         
  M3AST_AS.Compilation_Unit <: Compilation_Unit;
  M3AST_AS.Interface_gen_def <: Interface_gen_def;
  M3AST_AS.Module_gen_def <: Module_gen_def;
  M3AST_AS.Interface <: Interface;
  M3AST_AS.Module <: Module;
  M3AST_AS.Interface_gen_ins <: Interface_gen_ins;
  M3AST_AS.Module_gen_ins <: Module_gen_ins;
  M3AST_AS.Unsafe <: Unsafe;
  M3AST_AS.Import_item <: Import_item;       
  M3AST_AS.Simple_import <: Simple_import;       
  M3AST_AS.From_import <: From_import;         
  M3AST_AS.Revelation_s <: Revelation_s;       
  M3AST_AS.Const_decl_s <: Const_decl_s;            
  M3AST_AS.Type_decl_s <: Type_decl_s;             
  M3AST_AS.Var_decl_s <: Var_decl_s;              
  M3AST_AS.Exc_decl_s <: Exc_decl_s;              
  M3AST_AS.Proc_decl <: Proc_decl;               
  M3AST_AS.Const_decl <: Const_decl;          
  M3AST_AS.Var_decl <: Var_decl;            
  M3AST_AS.Exc_decl <: Exc_decl;            
  M3AST_AS.Subtype_decl <: Subtype_decl;       
  M3AST_AS.Concrete_decl <: Concrete_decl;      
  M3AST_AS.Subtype_reveal <: Subtype_reveal;    
  M3AST_AS.Concrete_reveal <: Concrete_reveal;   
  M3AST_AS.Named_type <: Named_type;            
  M3AST_AS.Integer_type <: Integer_type;
  M3AST_AS.WideChar_type <: WideChar_type;
  M3AST_AS.Real_type <: Real_type;
  M3AST_AS.LongReal_type <: LongReal_type;
  M3AST_AS.Extended_type <: Extended_type;
  M3AST_AS.Null_type <: Null_type;
  M3AST_AS.RefAny_type <: RefAny_type;
  M3AST_AS.Address_type <: Address_type;
  M3AST_AS.Root_type <: Root_type;
  M3AST_AS.Enumeration_type <: Enumeration_type;
  M3AST_AS.Subrange_type <: Subrange_type;
  M3AST_AS.Array_type <: Array_type;
  M3AST_AS.Record_type <: Record_type;
  M3AST_AS.Object_type <: Object_type;
  M3AST_AS.Set_type <: Set_type;
  M3AST_AS.Procedure_type <: Procedure_type;
  M3AST_AS.Ref_type <: Ref_type;
  M3AST_AS.Packed_type <: Packed_type;
  M3AST_AS.Opaque_type <: Opaque_type;
  M3AST_AS.Brand <: Brand;
  M3AST_AS.Untraced <: Untraced;
  M3AST_AS.Fields <: Fields;
  M3AST_AS.Method <: Method;
  M3AST_AS.Override <: Override;
  M3AST_AS.Formal_param <: Formal_param;
  M3AST_AS.Raisees_some <: Raisees_some;
  M3AST_AS.Raisees_any <: Raisees_any;
  M3AST_AS.Range <: Range;
  M3AST_AS.Integer_literal <: Integer_literal;
  M3AST_AS.Real_literal <: Real_literal;
  M3AST_AS.LongReal_literal <: LongReal_literal;
  M3AST_AS.Extended_literal <: Extended_literal;
  M3AST_AS.Text_literal <: Text_literal;
  M3AST_AS.WideText_literal <: WideText_literal;
  M3AST_AS.Char_literal <: Char_literal;
  M3AST_AS.WideChar_literal <: WideChar_literal;
  M3AST_AS.Nil_literal <: Nil_literal;
  M3AST_AS.Exp_used_id <: Exp_used_id;      
  M3AST_AS.Constructor <: Constructor;
  M3AST_AS.Propagate <: Propagate;
  M3AST_AS.Plus <: Plus;
  M3AST_AS.Minus <: Minus;
  M3AST_AS.Times <: Times;
  M3AST_AS.Rdiv <: Rdiv;
  M3AST_AS.Textcat <: Textcat;
  M3AST_AS.Div <: Div;
  M3AST_AS.Mod <: Mod;
  M3AST_AS.Eq <: Eq;
  M3AST_AS.Ne <: Ne;
  M3AST_AS.Gt <: Gt;
  M3AST_AS.Lt <: Lt;
  M3AST_AS.Ge <: Ge;
  M3AST_AS.And <: And;
  M3AST_AS.Or <: Or;
  M3AST_AS.In <: In;
  M3AST_AS.Select <: Select;
  M3AST_AS.Not <: Not;
  M3AST_AS.Unaryplus <: Unaryplus;
  M3AST_AS.Unaryminus <: Unaryminus;
  M3AST_AS.Deref <: Deref;
  M3AST_AS.Call <: Call;
  M3AST_AS.NEWCall <: NEWCall;
  M3AST_AS.Index <: Index;
  M3AST_AS.Actual <: Actual;
  M3AST_AS.STM <: STM;
  M3AST_AS.Assign_st <: Assign_st;
  M3AST_AS.Call_st <: Call_st;
  M3AST_AS.Case_st <: Case_st;
  M3AST_AS.Eval_st <: Eval_st;
  M3AST_AS.Exit_st <: Exit_st;
  M3AST_AS.For_st <: For_st;
  M3AST_AS.If_st <: If_st;
  M3AST_AS.Lock_st <: Lock_st;
  M3AST_AS.Loop_st <: Loop_st;
  M3AST_AS.Raise_st <: Raise_st;
  M3AST_AS.Repeat_st <: Repeat_st;
  M3AST_AS.Return_st <: Return_st;
  M3AST_AS.Try_st <: Try_st;
  M3AST_AS.Typecase_st <: Typecase_st;
  M3AST_AS.While_st <: While_st;
  M3AST_AS.With_st <: With_st;
  M3AST_AS.Block <: Block;
  M3AST_AS.Case <: Case;
  M3AST_AS.Else_stm <: Else_stm;
  M3AST_AS.By <: By;
  M3AST_AS.Elsif <: Elsif;
  M3AST_AS.Try_except <: Try_except;
  M3AST_AS.Try_finally <: Try_finally;
  M3AST_AS.Tcase <: Tcase;
  M3AST_AS.Handler <: Handler;
  M3AST_AS.Binding <: Binding;
  M3AST_AS.Bad_EXP <: Bad_EXP;
  M3AST_AS.Bad_M3TYPE <: Bad_M3TYPE;
  M3AST_AS.Bad_STM <: Bad_STM;

TYPE
  (* Pass through names *)

  NODE = Previous_View.NODE;
  SRC_NODE = Previous_View.SRC_NODE;
  DEF_ID = Previous_View.DEF_ID;
  UNIT_ID = Previous_View.UNIT_ID;
  TYPED_ID = Previous_View.TYPED_ID;
  FORMAL_ID = Previous_View.FORMAL_ID;
  METHOD_OVERRIDE_ID = Previous_View.METHOD_OVERRIDE_ID;
  USED_ID = Previous_View.USED_ID;
  UNIT = Previous_View.UNIT;
  UNIT_GEN_DEF = Previous_View.UNIT_GEN_DEF;
  UNIT_GEN_INS = Previous_View.UNIT_GEN_INS;
  UNIT_NORMAL = Previous_View.UNIT_NORMAL;
  UNIT_WITH_BODY = Previous_View.UNIT_WITH_BODY;
  IMPORTED = Previous_View.IMPORTED;
  DECL_REVL = Previous_View.DECL_REVL;
  DECL = Previous_View.DECL;
  TYPE_DECL = Previous_View.TYPE_DECL;
  REVELATION = Previous_View.REVELATION;
  EXP_TYPE = Previous_View.EXP_TYPE;
  M3TYPE = Previous_View.M3TYPE;
  TYPE_SPEC = Previous_View.TYPE_SPEC;
  METHOD_OVERRIDE = Previous_View.METHOD_OVERRIDE;
  BRANDED_TYPE = Previous_View.BRANDED_TYPE;
  FLOAT_TYPE = Previous_View.FLOAT_TYPE;
  RAISEES = Previous_View.RAISEES;
  EXP = Previous_View.EXP; 
  NUMERIC_LITERAL = Previous_View.NUMERIC_LITERAL;
  CONS_ELEM = Previous_View.CONS_ELEM;
  RANGE_EXP = Previous_View.RANGE_EXP;
  STM = Previous_View.STM;
  STM_WSS = Previous_View.STM_WSS;
  SUBSTM_WSS = Previous_View.SUBSTM_WSS;
  TRY_TAIL = Previous_View.TRY_TAIL;

END M3ASTOp_AS.
