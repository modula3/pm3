(************************************************************************
!		                                                        *
!*                                                                      *
!*         Copyright 1994 Sun Microsystems, Inc. All Rights Reserved.   *
!*                                                                      *
!*      Permission to use, copy, modify, and distribute this software   *
!*      and its documentation for any purpose and without fee is hereby *
!*      granted, provided that the above copyright notice appear in all *
!*      copies and that both that copyright notice and this permission  *
!*      notice appear in supporting documentation, and that the name of *
!*      Sun Microsystems, Inc. (SMI) not be used in advertising or      *
!*      publicity pertaining to distribution of the software without    *
!*      specific, written prior permission.                             *
!*                                                                      *
!*                                                                      *
!*      SMI DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,      *
!*      INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY,	        *
!*      FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.           *
!*      IN NO EVENT SHALL SMI BE LIABLE FOR ANY SPECIAL, INCIDENTAL,    *
!*	INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER     *
!*      RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *
!*      ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,        *
!*      ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE     *
!*      OF THIS SOFTWARE.                                               *
!*                                                                      *
!***********************************************************************)

(* Modified by Michel Dagenais, 15 April 1997. *)

(* This Module traverses the abstract syntax tree (ast) for a Modula-3 unit.
   While doing so, it recreates the associated source code, and checks
   for adherence to programming conventions.

   The Set procedure is the entry point into the Module. Then, DoIt is
   called recursively to select the appropriate procedure to process
   each node type. The procedures CheckIndentation, CheckVarName and
   CheckContsName are called whenever appropriate in the ast to check
   the programming conventions.

   The module ends with a number of support procedures which create and 
   attach to the ast the missing text needed to print out the formatted 
   source code. *)

MODULE M3Conv;

IMPORT
    AST, M3AST_LX, M3AST_AS, M3AST_SM, M3AST_PG,
    SeqM3AST_LX_SRC_NODE,
    M3AST_LX_F, M3AST_AS_F, M3AST_SM_F, M3AST_PG_F,
    M3CId, M3Assert,
    M3CToken, M3CWhitespace, M3CLiteral, M3CComment, M3CPragma,
    Text, M3CSrcPos, ASCII, Wr, Stdio, Fmt, Thread,
    SeqM3AST_AS_IMPORTED,
    SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_Used_def_id,
    SeqM3AST_AS_Import_item, SeqM3AST_AS_Override,
    SeqM3AST_AS_REVELATION, SeqM3AST_AS_DECL_REVL,
    SeqM3AST_AS_Const_decl, SeqM3AST_AS_TYPE_DECL,
    SeqM3AST_AS_Var_decl, SeqM3AST_AS_Exc_decl,
    SeqM3AST_AS_Var_id, 
    SeqM3AST_AS_Enum_id, SeqM3AST_AS_Field_id,
    SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Qual_used_id,
    SeqM3AST_AS_Fields, SeqM3AST_AS_Method,
    SeqM3AST_AS_M3TYPE,
    SeqM3AST_AS_Formal_param, SeqM3AST_AS_CONS_ELEM,
    SeqM3AST_AS_EXP, SeqM3AST_AS_Actual,
    SeqM3AST_AS_Case, SeqM3AST_AS_STM,
    SeqM3AST_AS_Elsif, SeqM3AST_AS_Tcase,
    SeqM3AST_AS_Handler, SeqM3AST_AS_Binding,
    SeqM3AST_AS_RANGE_EXP;

TYPE
  (* Information to carry through the tree traversal. *)

  Handle = OBJECT
    isModule, suppressPROC := FALSE;
    indent := 0;
    checkIndent := 0;
    comments: M3CComment.Iter;
    comment: M3CComment.T;
    pragmas: M3CPragma.Iter;
    pragma: M3CPragma.T;
    this_unit_id: M3AST_AS.UNIT_ID;
    checkFormat := FALSE;
    continuation := FALSE;
    previousLine := 0;
  END;

  WS = {Space, CommaSpace, Newline, SemiNewline, SemiSpace};

CONST
  WSV = ARRAY WS OF TEXT {" ", ", ", "\n", ";\n", "; "};
  Indent_Size = 2;

VAR
  ws_g: ARRAY WS OF M3AST_LX.Whitespace_rep;
  indent_ws_g: ARRAY [0..15] OF M3AST_LX.Whitespace_rep;
  id_char_set := ASCII.AlphaNumerics;

<*FATAL Thread.Alerted*>  
<*FATAL Wr.Failure*>

(* This procedure initializes the needed data structures and recursively
   call DoIt to process the unit ast. *)

PROCEDURE Set(n: M3AST_AS.SRC_NODE_C; indent := 0; checkFormat := FALSE;
    underscore := FALSE)=
  VAR cu := NARROW(n, M3AST_AS.Compilation_Unit);
  BEGIN
    (* Create the handle to carry the information needed during the
       tree traversal. *)

    WITH h = NEW(Handle, comments := M3CComment.NewIter(cu.lx_comments),
        this_unit_id := cu.as_root.as_id, indent := indent, 
        checkIndent := indent, checkFormat := checkFormat, 
        continuation := FALSE, pragmas := M3CPragma.NewIter(cu.lx_pragmas)) DO

      (* When underscore is true, such characters will be in the set of
         acceptable characters for indentifiers. *)

      id_char_set := ASCII.AlphaNumerics;
      IF underscore THEN id_char_set := id_char_set + ASCII.Set{'_'}; END;

      (* The comments and pragmas are stored separately from the rest of
         the tokens in the ast. In the handle, "comments" is the list of
         comments and "comment" is the next one to process. The fields
         "pragmas" and "pragma" serve the same purpose for pragmas. *)

      EVAL M3CComment.Next(h.comments, h.comment);
      EVAL M3CPragma.Next(h.pragmas, h.pragma);

      (* Process the unit calling DoIt recursively. *)

      DoIt(h, NIL, n);

      (* For each ast node, the preceeding comments are processed during
         the traversal. After the traversal, some comments after the last
         ast node may remain. *)

      WHILE h.comment # NIL DO
        D(h, n, M3CComment.Body(h.comment));
        IF M3CComment.Next(h.comments, h.comment) THEN
        ELSE h.comment := NIL;
        END;
      END;
    END;
  END Set;

(* This procedure performs some formatting/checking operations required
   for all ast nodes and then calls the appropriate procedure depending
   on the ast node type. *)

PROCEDURE DoIt(h: Handle; parent, n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    (* Check the identation for this ast node *)

    CheckIndentation(h,n);

    (* Process any comments in front of this ast node *)

    IF parent # NIL THEN Append(h, parent, n); END;

    (* Start with an empty list of lexical nodes. This list will be
       filled with keywords, whitespace, separators... by the procedures
       called recursively. *)

    n.lx_node_s := SeqM3AST_LX_SRC_NODE.Null;

    (* For each type an appropriate procedure is provided. For literal there
       is nothing more to do. For a unit, the associated ast root needs to
       be processed. *)

    TYPECASE n OF
    | M3AST_AS.Compilation_Unit(q) => DoIt(h, n, q.as_root);
    | M3AST_AS.Interface(q) => Interface(h, q);
    | M3AST_AS.Qual_used_id(q) => Qual_used_id(h, q);
    | M3AST_AS.Module(q) => Module(h, q);
    | M3AST_AS.Unsafe(q) => Unsafe(h, q);
    | M3AST_AS.Import_item(q) => Import_item(h, q);
    | M3AST_AS.Simple_import(q) => Simple_import(h, q);
    | M3AST_AS.From_import(q) => From_import(h, q);
    | M3AST_AS.Revelation_s(q) => Revelation_s(h, q);
    | M3AST_AS.Const_decl_s(q) => Const_decl_s(h, q);
    | M3AST_AS.Type_decl_s(q) => Type_decl_s(h, q);
    | M3AST_AS.Var_decl_s(q) => Var_decl_s(h, q);
    | M3AST_AS.Exc_decl_s(q) => Exc_decl_s(h, q);
    | M3AST_AS.Proc_decl(q) => Proc_decl(h, q);
    | M3AST_PG.Inline(q) => Inline(h, q);
    | M3AST_PG.External(q) => External(h, q);
    | M3AST_AS.Const_decl(q) => Const_decl(h, q);
    | M3AST_AS.Var_decl(q) => Var_decl(h, q);
    | M3AST_AS.Exc_decl(q) => Exc_decl(h, q);
    | M3AST_AS.Subtype_decl(q) => Subtype_decl(h, q);
    | M3AST_AS.TYPE_DECL(q) => TYPE_DECL(h, q);
    | M3AST_AS.Concrete_decl(q) => Concrete_decl(h, q);
    | M3AST_AS.REVELATION(q) => REVELATION(h, q);
    | M3AST_AS.Concrete_reveal(q) => Concrete_reveal(h, q);
    | M3AST_AS.Subtype_reveal(q) => Subtype_reveal(h, q);
    | M3AST_AS.Named_type(q) => Named_type(h, q)
    | M3AST_AS.Integer_type(q) => Integer_type(h, q)
    | M3AST_AS.Real_type(q) => Real_type(h, q)
    | M3AST_AS.LongReal_type(q) => LongReal_type(h, q)
    | M3AST_AS.Extended_type(q) => Extended_type(h, q)
    | M3AST_AS.Null_type(q) => Null_type(h, q)
    | M3AST_AS.RefAny_type(q) => RefAny_type(h, q)
    | M3AST_AS.Address_type(q) => Address_type(h, q)
    | M3AST_AS.Root_type(q) => Root_type(h, q)
    | M3AST_AS.Array_type(q) => Array_type(h, q);
    | M3AST_AS.Enumeration_type(q) => Enumeration_type(h, q)
    | M3AST_AS.Subrange_type(q) => Subrange_type(h, q)
    | M3AST_AS.Record_type(q) => Record_type(h, q)
    | M3AST_AS.Object_type(q) => Object_type(h, q)
    | M3AST_AS.Set_type(q) => Set_type(h, q)
    | M3AST_AS.Procedure_type(q) => Procedure_type(h, q)
    | M3AST_AS.Ref_type(q) => Ref_type(h, q)
    | M3AST_AS.Packed_type(q) => Packed_type(h, q)
    | M3AST_AS.Opaque_type(q) => Opaque_type(h, q)
    | M3AST_SM.Type_type(q) => Type_type(h, q);
    | M3AST_SM.Any_type(q) => Any_type(h, q);
    | M3AST_AS.Brand(q) => Brand(h, q)
    | M3AST_AS.Untraced(q) => Untraced(h, q)
    | M3AST_AS.Fields(q) => Fields(h, q)
    | M3AST_AS.Method(q) => Method(h, q)
    | M3AST_AS.Override(q) => Override(h, q)
    | M3AST_AS.Formal_param(q) => Formal_param(h, q)
    | M3AST_AS.Raisees_any(q) => Raisees_any(h, q)
    | M3AST_AS.Raisees_some(q) => Raisees_some(h, q)
    | M3AST_AS.Range(q) => Range(h, q)
    | M3AST_AS.Range_EXP(q) => Range_EXP(h, q)
    | M3AST_AS.Constructor(q) => Constructor(h, q);
    | M3AST_AS.Propagate(q) => Propagate(h, q);
    | M3AST_AS.RANGE_EXP_elem(q) => RANGE_EXP_elem(h, q);
    | M3AST_AS.Actual_elem(q) => Actual_elem(h, q);
    | M3AST_AS.BINARY(q) => BINARY(h, q);
    | M3AST_AS.UNARY(q) => UNARY(h, q);
    | M3AST_AS.Select(q) => Select(h, q);
    | M3AST_AS.Call(q) => Call(h, q);
    | M3AST_AS.Index(q) => Index(h, q);
    | M3AST_AS.Actual(q) => Actual(h, q)
    | M3AST_AS.Exp_used_id(q) => Exp_used_id(h, q)
    | M3AST_AS.LITERAL => (* nothing more to do *)
    | M3AST_AS.Block(q) => Block(h, q)
    | M3AST_AS.Assign_st(q) => Assign_st(h, q);
    | M3AST_AS.Call_st(q) => Call_st(h, q);
    | M3AST_AS.Case_st(q) => Case_st(h, q);
    | M3AST_AS.Eval_st(q) => Eval_st(h, q);
    | M3AST_AS.Exit_st(q) => Exit_st(h, q);
    | M3AST_AS.For_st(q) => For_st(h, q);
    | M3AST_AS.If_st(q) => If_st(h, q);
    | M3AST_AS.Lock_st(q) => Lock_st(h, q);
    | M3AST_AS.Loop_st(q) => Loop_st(h, q);
    | M3AST_AS.Raise_st(q) => Raise_st(h, q);
    | M3AST_AS.Repeat_st(q) => Repeat_st(h, q);
    | M3AST_AS.Return_st(q) => Return_st(h, q);
    | M3AST_AS.Try_st(q) => Try_st(h, q);
    | M3AST_AS.Typecase_st(q) => Typecase_st(h, q);
    | M3AST_AS.While_st(q) => While_st(h, q);
    | M3AST_AS.With_st(q) => With_st(h, q);
    | M3AST_AS.Case(q) => Case(h, q);
    | M3AST_AS.Else_stm(q) => Else_stm(h, q);
    | M3AST_AS.By(q) => By(h, q);
    | M3AST_AS.Elsif(q) => Elsif(h, q);
    | M3AST_AS.Try_except(q) => Try_except(h, q);
    | M3AST_AS.Try_finally(q) => Try_finally(h, q);
    | M3AST_AS.Tcase(q) => Tcase(h, q);
    | M3AST_AS.Handler(q) => Handler(h, q);
    | M3AST_AS.Binding(q) => Binding(h, q);
    ELSE
      M3Assert.Check(FALSE);
    END
  END DoIt;

(* All the following procedures are similar. They insert the associated
   keywords, indent anything which starts on a new line, process the
   child nodes, insert the needed separators between the childs or at the
   end, and check the characters used in identifiers againts the
   programming conventions. The current indentation level is increased
   before processing the childs whenever appropriate and restored after. *)

PROCEDURE DoUNIT_WITH_BODY(h: Handle; n: M3AST_AS.UNIT_WITH_BODY)=
  VAR
    m2: M3AST_AS.IMPORTED;
    iter2 := SeqM3AST_AS_IMPORTED.NewIter(n.as_import_s);
  BEGIN
    (* Check that the unit name is a proper "constant" identifier *)

    CheckConstName(h,n.as_id);

    (* Recursively process the list of imported interfaces *)

    IF NOT SeqM3AST_AS_IMPORTED.Empty(n.as_import_s) THEN NL(h, n); END;
    WHILE SeqM3AST_AS_IMPORTED.Next(iter2, m2) DO DoIt(h, n, m2); END;
    NL(h, n); NL(h, n);

    (* Process the unit block *)

    DoIt(h, n, n.as_block);
  END DoUNIT_WITH_BODY;

PROCEDURE DECL_Prelude(h: Handle; n: M3AST_AS.SRC_NODE; s: M3CToken.T)=
  VAR
    e_decl: M3AST_PG.EXTERNAL_DECL;
  BEGIN
    IF h.indent = 0 THEN NL(h, n); END;
    Indent(h, n);
    IF M3AST_PG.IsA_EXTERNAL_DECL(n, e_decl) THEN
      WITH x = e_decl.pg_external DO
        IF x # NIL THEN DoIt(h, n, x); END;
      END;
    END; (* if *)
    Append(h, n, NewToken(s));
    NLIncIndent(h, n);
  END DECL_Prelude;

(*PRIVATE*)
PROCEDURE UnitPostlude(h: Handle; n: M3AST_AS.UNIT) RAISES {}=
  BEGIN
    D(h, n, " " & M3CId.ToText(n.as_id.lx_symrep) & "." & "\n");
  END UnitPostlude;

PROCEDURE Interface(h: Handle; n: M3AST_AS.Interface)=
  BEGIN
    h.isModule := FALSE;

    (* Process all the EXTERNAL declarations *)

    WITH x = n.vEXTERNAL_DECL.pg_external DO
      IF x # NIL THEN DoIt(h, n, x) END;
    END;
    IF n.as_unsafe # NIL THEN DoIt(h, n, n.as_unsafe); END;

    (* attach the keyword INTERFACE, a space, and the unit identifier
       to this ast node. Then process the interface body. *)
 
    Append(h, n, NewToken(M3CToken.INTERFACE_)); Space(h, n);
    Append(h, n, n.as_id);
    SCNL(h, n);
    DoUNIT_WITH_BODY(h, n);
    UnitPostlude(h, n);
  END Interface;

PROCEDURE Qual_used_id(h: Handle; n: M3AST_AS.Qual_used_id)=
  BEGIN
    (* For a qualified identifier attach the interface name, the dot
       and the identifier. *)

    IF n.as_intf_id # NIL THEN 
      Append(h, n, n.as_intf_id); 
      Append(h, n, NewToken(M3CToken.Dot)); 
    END;
    Append(h, n, n.as_id);
  END Qual_used_id;

PROCEDURE Module(h: Handle; n: M3AST_AS.Module)=
  VAR
    m: M3AST_AS.Used_interface_id;
    iter := SeqM3AST_AS_Used_interface_id.NewIter(n.as_export_s);
    isFirst := TRUE;
  BEGIN
    h.isModule := TRUE;
    IF n.as_unsafe # NIL THEN DoIt(h, n, n.as_unsafe); END;
    Append(h, n, NewToken(M3CToken.MODULE_)); Space(h, n);
    Append(h, n, n.as_id);
    IF NOT SeqM3AST_AS_Used_interface_id.Empty(n.as_export_s) THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.EXPORTS_)); Space(h, n);
    END;
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, m) DO
      Between(h, n, isFirst, CS); Append(h, n, m);
    END;
    SCNL(h, n);
    DoUNIT_WITH_BODY(h, n);
    UnitPostlude(h, n);
  END Module;

PROCEDURE Unsafe(h: Handle; n: M3AST_AS.Unsafe)=
  BEGIN
    Append(h, n, NewToken(M3CToken.UNSAFE_)); Space(h, n);
  END Unsafe;

PROCEDURE Import_item(h: Handle; n: M3AST_AS.Import_item)=
  BEGIN
    Append(h, n, n.as_intf_id);
    IF n.as_id # NIL THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.AS_)); Space(h, n); 
      Append(h, n, n.as_id);
    END;
  END Import_item;

PROCEDURE Simple_import(h: Handle; n: M3AST_AS.Simple_import)=
  VAR
    m: M3AST_AS.Import_item;
    iter := SeqM3AST_AS_Import_item.NewIter(n.as_import_item_s);
    isFirst := TRUE;
  BEGIN
    Append(h, n, NewToken(M3CToken.IMPORT_)); Space(h, n);
    WHILE SeqM3AST_AS_Import_item.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    SCNL(h, n);
  END Simple_import;


PROCEDURE From_import(h: Handle; n: M3AST_AS.From_import)=
  VAR
    m: M3AST_AS.Used_def_id;
    iter := SeqM3AST_AS_Used_def_id.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    Append(h, n, NewToken(M3CToken.FROM_));
    Space(h, n); Append(h, n, n.as_intf_id);
    Space(h, n); Append(h, n, NewToken(M3CToken.IMPORT_));
    Space(h, n); 
    WHILE SeqM3AST_AS_Used_def_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS); Append(h, n, m); 
    END;
    SCNL(h, n);
  END From_import;


PROCEDURE Revelation_s(h: Handle; n: M3AST_AS.Revelation_s)=
  VAR
    m: M3AST_AS.REVELATION;
    iter := SeqM3AST_AS_REVELATION.NewIter(n.as_reveal_s);
  BEGIN
    (* The declaration prelude performs the indentation and increases
       the indentation level. *)

    DECL_Prelude(h, n, M3CToken.REVEAL_);

    (* Process all the revelations. *)

    WHILE SeqM3AST_AS_REVELATION.Next(iter, m) DO DoIt(h, n, m); END;

    (* Restore the initial indentation level. *)

    DecIndent(h);
  END Revelation_s;


PROCEDURE Const_decl_s(h: Handle; n: M3AST_AS.Const_decl_s)=
  VAR
    m: M3AST_AS.Const_decl;
    iter := SeqM3AST_AS_Const_decl.NewIter(n.as_const_decl_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.CONST_);
    WHILE SeqM3AST_AS_Const_decl.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Const_decl_s;


PROCEDURE Type_decl_s(h: Handle; n: M3AST_AS.Type_decl_s)=
  VAR
    m: M3AST_AS.TYPE_DECL;
    iter := SeqM3AST_AS_TYPE_DECL.NewIter(n.as_type_decl_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.TYPE_);
    WHILE SeqM3AST_AS_TYPE_DECL.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Type_decl_s;


PROCEDURE Var_decl_s(h: Handle; n: M3AST_AS.Var_decl_s)=
  VAR
    m: M3AST_AS.Var_decl;
    iter := SeqM3AST_AS_Var_decl.NewIter(n.as_var_decl_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.VAR_);
    WHILE SeqM3AST_AS_Var_decl.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Var_decl_s;


PROCEDURE Exc_decl_s(h: Handle; n: M3AST_AS.Exc_decl_s)=
  VAR
    m: M3AST_AS.Exc_decl;
    iter := SeqM3AST_AS_Exc_decl.NewIter(n.as_exc_decl_s);
  BEGIN
    DECL_Prelude(h, n, M3CToken.EXCEPTION_);
    WHILE SeqM3AST_AS_Exc_decl.Next(iter, m) DO DoIt(h, n, m); END;
    DecIndent(h);
  END Exc_decl_s;


PROCEDURE Proc_decl(h: Handle; n: M3AST_AS.Proc_decl)=
  BEGIN
    CheckConstName(h,n.as_id);
    NL(h, n); Indent(h, n);
    WITH x = n.vEXTERNAL_DECL.pg_external DO
      IF x # NIL THEN DoIt(h, n, x); END;
    END;
    IF n.pg_inline # NIL THEN DoIt(h, n, n.pg_inline); END;
    Append(h, n, NewToken(M3CToken.PROCEDURE_)); Space(h, n);
    Append(h, n, n.as_id);
    h.suppressPROC := TRUE; DoIt(h, n, n.as_type);
    IF n.as_body # NIL THEN 

      (* The procedure body is there. Put the equal sign. The indentation
         level is increased before the procedure body is processed and the
         indentation level is restored after. *)

      Append(h, n, NewToken(M3CToken.Equal)); 
      NLIncIndent(h, n);
      DoIt(h, n, n.as_body); 
      D(h, n, " " & M3CId.ToText(n.as_id.lx_symrep));
      DecIndent(h);
    END;
    SCNL(h, n);
  END Proc_decl;

PROCEDURE Inline(h: Handle; n: M3AST_PG.Inline)=
  BEGIN
    (* This ought to be a real pragma node *)
    D(h, n, "<*INLINE*> ");
  END Inline;

PROCEDURE External(h: Handle; n: M3AST_PG.External)=
  BEGIN
    (* This ought to be a real pragma node *)
    D(h, n, "<*EXTERNAL");
    IF n.lx_lang_spec # NIL THEN
      Space(h, n);
      D(h, n, M3CLiteral.ToText(n.lx_lang_spec));
      Space(h, n);
    END;
    D(h, n, "*> ");
  END External;

PROCEDURE Const_decl(h: Handle; n: M3AST_AS.Const_decl)=
  BEGIN
    CheckConstName(h,n.as_id);
    Indent(h, n);
    Append(h, n, n.as_id);
    IF n.as_type # NIL THEN
      Append(h, n, NewToken(M3CToken.Colon));
      DoIt(h, n, n.as_type);
    END;
    Space(h, n); Append(h, n, NewToken(M3CToken.Equal));
    Space(h, n); DoIt(h, n, n.as_exp);
    SCNL(h, n);
  END Const_decl;


PROCEDURE Var_decl(h: Handle; n: M3AST_AS.Var_decl)=
  VAR
    m: M3AST_AS.Var_id;
    iter := SeqM3AST_AS_Var_id.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    WHILE SeqM3AST_AS_Var_id.Next(iter, m) DO 
      CheckVarName(h,m);
      Between(h, n, isFirst, CS); Append(h, n , m);
    END;
    IF n.as_type # NIL THEN
      Append(h, n, NewToken(M3CToken.Colon)); Space(h, n);
      DoIt(h, n, n.as_type);
    END;
    IF n.as_default # NIL THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
      DoIt(h, n, n.as_default);
    END;
    SCNL(h, n);
  END Var_decl;


PROCEDURE Exc_decl(h: Handle; n: M3AST_AS.Exc_decl)=
  BEGIN
    CheckConstName(h,n.as_id);
    Indent(h, n);
    Append(h, n, n.as_id);
    IF n.as_type # NIL THEN 
      Append(h, n, NewToken(M3CToken.Bra));
      DoIt(h, n, n.as_type);
      Append(h, n, NewToken(M3CToken.Ket));
    END;
    SCNL(h, n);
  END Exc_decl;


PROCEDURE Subtype_decl(h: Handle; n: M3AST_AS.Subtype_decl)=
  BEGIN
    TYPE_DECL(h, n);
  END Subtype_decl;

PROCEDURE TYPE_DECL(h: Handle; n: M3AST_AS.TYPE_DECL)=
  BEGIN
    CheckConstName(h,n.as_id);
    Indent(h, n);
    Append(h, n, n.as_id);
    Space(h, n); 
    IF ISTYPE(n, M3AST_AS.Subtype_decl) THEN
      Append(h, n, NewToken(M3CToken.Subtype))
    ELSE Append(h, n, NewToken(M3CToken.Equal));
    END;
    Space(h, n);
    DoIt(h, n, n.as_type);
    SCNL(h, n);
  END TYPE_DECL;


PROCEDURE Concrete_decl(h: Handle; n: M3AST_AS.Concrete_decl)=
  BEGIN
    TYPE_DECL(h, n);
  END Concrete_decl;


PROCEDURE Subtype_reveal(h: Handle; n: M3AST_AS.Subtype_reveal)=
  BEGIN
    REVELATION(h, n);
  END Subtype_reveal;


(* PRIVATE *)
PROCEDURE REVELATION(h: Handle; n: M3AST_AS.REVELATION)=
  BEGIN
    Indent(h, n);
    DoIt(h, n, n.as_qual_id);
    Space(h, n); 
    IF ISTYPE(n, M3AST_AS.Subtype_reveal) THEN
      Append(h, n, NewToken(M3CToken.Subtype))
    ELSE Append(h, n, NewToken(M3CToken.Equal));
    END;
    Space(h, n); 
    DoIt(h, n, n.as_type);
    SCNL(h, n);
  END REVELATION;

PROCEDURE Concrete_reveal(h: Handle; n: M3AST_AS.Concrete_reveal)=
  BEGIN
    REVELATION(h, n);
  END Concrete_reveal;


PROCEDURE Named_type(h: Handle; n: M3AST_AS.Named_type)=
  BEGIN
    DoIt(h, n, n.as_qual_id);
  END Named_type;


PROCEDURE Integer_type(
    h: Handle; n: M3AST_AS.Integer_type)=
  BEGIN
    Append(h, n, NewToken(M3CToken.INTEGER_));
  END Integer_type;

PROCEDURE Real_type(
    h: Handle; n: M3AST_AS.Real_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.REAL_));
  END Real_type;

PROCEDURE LongReal_type(
    h: Handle; n: M3AST_AS.LongReal_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.LONGREAL_));
  END LongReal_type;

PROCEDURE Extended_type(
    h: Handle; n: M3AST_AS.Extended_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.EXTENDED_));
  END Extended_type;

PROCEDURE Null_type(
    h: Handle; n: M3AST_AS.Null_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.NULL_));
  END Null_type;

PROCEDURE RefAny_type(
    h: Handle; n: M3AST_AS.RefAny_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.REFANY_));
  END RefAny_type;

PROCEDURE Address_type(
    h: Handle; n: M3AST_AS.Address_type;)=
  BEGIN
    Append(h, n, NewToken(M3CToken.ADDRESS_));
  END Address_type;

PROCEDURE Root_type(
    h: Handle; n: M3AST_AS.Root_type)=
  BEGIN
    IF n.as_trace_mode # NIL THEN
      Append(h, n, NewToken(M3CToken.UNTRACED_));
      Append(h, n, NewToken(M3CToken.ROOT_));
    ELSE Append(h, n, NewToken(M3CToken.ROOT_));
    END;
  END Root_type;

PROCEDURE Array_type(h: Handle; n: M3AST_AS.Array_type)=
  VAR
    m: M3AST_AS.M3TYPE;
    iter := SeqM3AST_AS_M3TYPE.NewIter(n.as_indextype_s);
    isFirst := TRUE;
  BEGIN
    Append(h, n, NewToken(M3CToken.ARRAY_));
    Space(h, n);
    IF NOT SeqM3AST_AS_M3TYPE.Exhausted(iter) THEN
      WHILE SeqM3AST_AS_M3TYPE.Next(iter, m) DO 
        Between(h, n, isFirst, CommaSpace); DoIt(h, n, m); 
      END;
      Space(h, n);
    END;  
    Append(h, n, NewToken(M3CToken.OF_));
    Space(h, n);
    DoIt(h, n, n.as_elementtype);
  END Array_type;

PROCEDURE Enumeration_type(h: Handle; n: M3AST_AS.Enumeration_type)=
  VAR
    m: M3AST_AS.Enum_id;
    iter := SeqM3AST_AS_Enum_id.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    Append(h, n, NewToken(M3CToken.CurlyBra));
    WHILE SeqM3AST_AS_Enum_id.Next(iter, m) DO 
      CheckConstName(h,m);
      Between(h, n, isFirst, CS);
      Append(h, n, m);
    END;
    Append(h, n, NewToken(M3CToken.CurlyKet));
  END Enumeration_type;


PROCEDURE Subrange_type(h: Handle; n: M3AST_AS.Subrange_type)=
  BEGIN
    Append(h, n, NewToken(M3CToken.SquareBra));
    DoIt(h, n, n.as_range);
    Append(h, n, NewToken(M3CToken.SquareKet));
  END Subrange_type;

PROCEDURE Record_type(h: Handle; n: M3AST_AS.Record_type)=
  VAR
    m: M3AST_AS.Fields;
    iter := SeqM3AST_AS_Fields.NewIter(n.as_fields_s);
  BEGIN
    Append(h, n, NewToken(M3CToken.RECORD_));
    NLIncIndent(h, n);
    WHILE SeqM3AST_AS_Fields.Next(iter, m) DO 
      DoIt(h, n, m); 
    END;
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_));
  END Record_type;


PROCEDURE Object_type(h: Handle; n: M3AST_AS.Object_type)
     =
  VAR
    m: M3AST_AS.Fields;
    iter := SeqM3AST_AS_Fields.NewIter(n.as_fields_s);
    m2: M3AST_AS.Method;
    iter2 := SeqM3AST_AS_Method.NewIter(n.as_method_s);
    m3: M3AST_AS.Override;
    iter3 := SeqM3AST_AS_Override.NewIter(n.as_override_s);
  BEGIN
    (* Process the object ancestor type and brand. *)

    IF n.as_ancestor # NIL THEN DoIt(h, n, n.as_ancestor); Space(h, n); END;
    IF n.as_brand # NIL THEN DoIt(h, n, n.as_brand); END;
    Append(h, n, NewToken(M3CToken.OBJECT_));
    NLIncIndent(h, n);

    (* Process the list of fields, with the indentation level increased. *)

    WHILE SeqM3AST_AS_Fields.Next(iter, m) DO DoIt(h, n, m); END;

    (* Process the methods list *)

    IF NOT SeqM3AST_AS_Method.Empty(n.as_method_s) THEN
      DecIndent(h); Indent(h, n);
      Append(h, n, NewToken(M3CToken.METHODS_));
      NLIncIndent(h, n);
      WHILE SeqM3AST_AS_Method.Next(iter2, m2) DO DoIt(h, n, m2); END;
    END; (* if *)

    (* Process the list of overrides *)

    IF NOT SeqM3AST_AS_Override.Empty(n.as_override_s) THEN
      DecIndent(h); Indent(h, n);
      Append(h, n, NewToken(M3CToken.OVERRIDES_));
      NLIncIndent(h, n);
      WHILE SeqM3AST_AS_Override.Next(iter3, m3) DO DoIt(h, n, m3); END;
    END; (* if *)

    (* Restore the indentation level and attach the END keyword. *)

    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_));
  END Object_type;


PROCEDURE Set_type(h: Handle; n: M3AST_AS.Set_type)=
  BEGIN
    Append(h, n, NewToken(M3CToken.SET_)); Space(h, n);
    Append(h, n, NewToken(M3CToken.OF_)); Space(h, n);
    DoIt(h, n, n.as_type);
  END Set_type;


PROCEDURE Procedure_type(h: Handle; n: M3AST_AS.Procedure_type)=
  VAR
    m: M3AST_AS.Formal_param;
    iter := SeqM3AST_AS_Formal_param.NewIter(n.as_formal_param_s);
    isFirst := TRUE;
  BEGIN
    IF NOT h.suppressPROC THEN 
      Append(h, n, NewToken(M3CToken.PROCEDURE_)); Space(h, n);
      Append(h, n, NewToken(M3CToken.Bra));
    ELSE Append(h, n, NewToken(M3CToken.Bra));
    END;
    h.suppressPROC := FALSE;
    WHILE SeqM3AST_AS_Formal_param.Next(iter, m) DO
      Between(h, n, isFirst, ScS); DoIt(h, n, m); 
    END;
    Append(h, n, NewToken(M3CToken.Ket));
    IF n.as_result_type # NIL THEN 
      Append(h, n, NewToken(M3CToken.Colon));
      Space(h, n);
      DoIt(h, n, n.as_result_type); 
    END;
    IF n.as_raises # NIL THEN DoIt(h, n, n.as_raises); END;
  END Procedure_type;


PROCEDURE Ref_type(h: Handle; n: M3AST_AS.Ref_type)=
  BEGIN
    IF n.as_trace_mode # NIL THEN DoIt(h, n, n.as_trace_mode); END;
    IF n.as_brand # NIL THEN DoIt(h, n, n.as_brand); END;
    Append(h, n, NewToken(M3CToken.REF_)); Space(h, n);
    DoIt(h, n, n.as_type);
  END Ref_type;

PROCEDURE Packed_type(h: Handle; n: M3AST_AS.Packed_type)=
  BEGIN
    Append(h, n, NewToken(M3CToken.BITS_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.FOR_)); Space(h, n);
    DoIt(h, n, n.as_type);
  END Packed_type;


PROCEDURE Opaque_type(h: Handle; n: M3AST_AS.Opaque_type)=
  BEGIN
    DoIt(h, n, n.as_type);
  END Opaque_type;


PROCEDURE Type_type(h: Handle; n: M3AST_SM.Type_type)=
  BEGIN
    D(h, n, "M3TYPE");
  END Type_type;


PROCEDURE Any_type(h: Handle; n: M3AST_SM.Any_type)=
  BEGIN
    D(h, n, "ANY");
  END Any_type;


PROCEDURE Brand(h: Handle; n: M3AST_AS.Brand)=
  BEGIN
    Append(h, n, NewToken(M3CToken.BRANDED_)); Space(h, n);
    IF n.as_exp # NIL THEN DoIt(h, n, n.as_exp) END;
  END Brand;


PROCEDURE Untraced(h: Handle; n: M3AST_AS.Untraced)=
  BEGIN
    Append(h, n, NewToken(M3CToken.UNTRACED_)); Space(h, n);
  END Untraced;


PROCEDURE Fields(h: Handle; n: M3AST_AS.Fields)=
  VAR
    m: M3AST_AS.Field_id;
    iter := SeqM3AST_AS_Field_id.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    WHILE SeqM3AST_AS_Field_id.Next(iter, m) DO 
      CheckVarName(h,m);
      Between(h, n, isFirst, CS); Append(h, n, m); 
    END;
    IF n.as_type # NIL THEN
      Append(h, n, NewToken(M3CToken.Colon)); Space(h, n);
      DoIt(h, n, n.as_type); 
    END;
    IF n.as_default # NIL THEN 
      Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
      DoIt(h, n, n.as_default);
    END;
    SCNL(h, n);
  END Fields;


PROCEDURE Method(h: Handle; n: M3AST_AS.Method)=
  BEGIN
    CheckVarName(h,n.as_id);
    Indent(h, n);
    Append(h, n, n.as_id);
    h.suppressPROC := TRUE; DoIt(h, n, n.as_type);
    IF n.as_default # NIL THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n); 
      DoIt(h, n, n.as_default);
    END;
    SCNL(h, n);
  END Method;


PROCEDURE Override(h: Handle; n: M3AST_AS.Override)=
  BEGIN
    Indent(h, n);
    Append(h, n, n.as_id);
    Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n); 
    DoIt(h, n, n.as_default);
    SCNL(h, n);
  END Override;


PROCEDURE Formal_param(h: Handle; n: M3AST_AS.Formal_param)=
  VAR
    m: M3AST_AS.FORMAL_ID;
    iter := SeqM3AST_AS_FORMAL_ID.NewIter(n.as_id_s);
    isFirst := TRUE;
  BEGIN
    WHILE SeqM3AST_AS_FORMAL_ID.Next(iter, m) DO 
      CheckVarName(h,m);
      IF isFirst THEN
        TYPECASE m OF
        | M3AST_AS.F_Var_id => 
            Append(h, n, NewToken(M3CToken.VAR_)); Space(h, n);
        | M3AST_AS.F_Readonly_id =>
            Append(h, n, NewToken(M3CToken.READONLY_)); Space(h, n);
        ELSE (* VALUE implied *)
        END;
      END; (* if *)
      Between(h, n, isFirst, CS); Append(h, n, m); 
    END;
    IF n.as_formal_type # NIL THEN
      Append(h, n, NewToken(M3CToken.Colon)); Space(h, n);
      DoIt(h, n, n.as_formal_type);
    END;
    IF n.as_default # NIL THEN
      Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
      DoIt(h, n, n.as_default);
    END;
  END Formal_param;


PROCEDURE Raisees_any(h: Handle; n: M3AST_AS.Raisees_any)=
  BEGIN
    Space(h, n);
    Append(h, n, NewToken(M3CToken.RAISES_));
    Space(h, n);
    Append(h, n, NewToken(M3CToken.ANY_));
  END Raisees_any;


PROCEDURE Raisees_some(h: Handle; n: M3AST_AS.Raisees_some)=
  VAR
    m: M3AST_AS.Qual_used_id;
    iter := SeqM3AST_AS_Qual_used_id.NewIter(n.as_raisees_s);
    isFirst := TRUE;
  BEGIN
    Space(h, n);
    Append(h, n, NewToken(M3CToken.RAISES_));
    Space(h, n);
    Append(h, n, NewToken(M3CToken.CurlyBra));
    WHILE SeqM3AST_AS_Qual_used_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Append(h, n, NewToken(M3CToken.CurlyKet));
  END Raisees_some;


PROCEDURE Range(h: Handle; n: M3AST_AS.Range)=
  BEGIN
    DoIt(h, n, n.as_exp1);
    Space(h, n);
    Append(h, n, NewToken(M3CToken.Range));
    Space(h, n);
    DoIt(h, n, n.as_exp2);
  END Range;


PROCEDURE Range_EXP(h: Handle; n: M3AST_AS.Range_EXP)=
  BEGIN
    DoIt(h, n, n.as_exp);
  END Range_EXP;

PROCEDURE Constructor(h: Handle; n: M3AST_AS.Constructor)=
  VAR
    m: M3AST_AS.CONS_ELEM;
    iter := SeqM3AST_AS_CONS_ELEM.NewIter(n.as_element_s);
    isFirst := TRUE;
  BEGIN
    DoIt(h, n, n.as_type);
    Space(h, n);
    Append(h, n, NewToken(M3CToken.CurlyBra));
    WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    IF n.as_propagate # NIL THEN DoIt(h, n, n.as_propagate); END;
    Append(h, n, NewToken(M3CToken.CurlyKet));
  END Constructor;


PROCEDURE Propagate(h: Handle; n: M3AST_AS.Propagate)=
  BEGIN
    Append(h, n, NewToken(M3CToken.Comma));
    Space(h, n);
    Append(h, n, NewToken(M3CToken.Range));
  END Propagate;

PROCEDURE RANGE_EXP_elem(h: Handle; n: M3AST_AS.RANGE_EXP_elem)=
  BEGIN
    DoIt(h, n, n.as_range_exp);
  END RANGE_EXP_elem;


PROCEDURE Actual_elem(h: Handle; n: M3AST_AS.Actual_elem)=
  BEGIN
    DoIt(h, n, n.as_actual);
  END Actual_elem;

PROCEDURE LowerPrec(child, parent: M3AST_AS.EXP; right: BOOLEAN): 
    BOOLEAN RAISES {}=
  VAR c_prec, p_prec: INTEGER;
  BEGIN
    (* Returns TRUE if child is lower precedence than parent 
      (and hence needs bracketing). 'right' denotes whether this
      is the right hand side, for which a bracket is needed
      if precedence is equal.
    *)

    (* parent is either a Binary or Unary; child may be any expression *)
    TYPECASE child OF
    | M3AST_AS.BINARY(b) =>
        c_prec := BPrec(b);
    | M3AST_AS.UNARY(u) =>
        c_prec := UPrec(u);
    ELSE
      RETURN FALSE
    END; (* if *)

    TYPECASE parent OF <*NOWARN*>
    | M3AST_AS.BINARY(b) =>
        p_prec := BPrec(b);
    | M3AST_AS.UNARY(u) =>
        p_prec := UPrec(u);
    END; (* if *)

    RETURN c_prec < p_prec OR (right AND c_prec = p_prec);
  END LowerPrec;

(*PRIVATE*)
PROCEDURE BPrec(op: M3AST_AS.BINARY): INTEGER RAISES {}=
  BEGIN
    TYPECASE op OF <*NOWARN*>
    | M3AST_AS.Times, M3AST_AS.Rdiv, 
          M3AST_AS.Div, M3AST_AS.Mod => RETURN 7;

    | M3AST_AS.Plus, M3AST_AS.Minus, M3AST_AS.Textcat => RETURN 6;

    | M3AST_AS.Eq, M3AST_AS.Ne, 
          M3AST_AS.Gt, M3AST_AS.Lt,
          M3AST_AS.Ge, M3AST_AS.Le, M3AST_AS.In => RETURN 5;
    
    | M3AST_AS.And => RETURN 3;

    | M3AST_AS.Or => RETURN 2;
    END; (* case *)
  END BPrec;

(*PRIVATE*)
PROCEDURE UPrec(op: M3AST_AS.UNARY): INTEGER RAISES {}=
  BEGIN
    TYPECASE op OF <*NOWARN*>
    | M3AST_AS.Deref => RETURN 9;

    | M3AST_AS.Unaryplus, M3AST_AS.Unaryminus => RETURN 8;

    | M3AST_AS.Not => RETURN 4;
    END; (* case *)
  END UPrec;


PROCEDURE BINARY(h: Handle; n: M3AST_AS.BINARY)=
  VAR bracket := FALSE;
  BEGIN
    (* Output nested parenthesis as needed *)

    IF LowerPrec(n.as_exp1, n, right := FALSE) THEN
      Append(h, n, NewToken(M3CToken.Bra)); bracket := TRUE;
    END; (* if *)

    (* Process the first part of the binary operator expression and
       close the parenthesis if needed. *)

    DoIt(h, n, n.as_exp1);
    IF bracket THEN Append(h, n, NewToken(M3CToken.Ket)) END;

    (* Attach the text for the operator token *)

    Space(h, n); Append(h, n, NewToken(BTokenFor(n))); Space(h, n); 
    bracket := FALSE;

    (* Process the second part of the binary operator, inside parenthesis
       if needed. *)

    IF LowerPrec(n.as_exp2, n, right := TRUE) THEN
      Append(h, n, NewToken(M3CToken.Bra)); bracket := TRUE;
    END; (* if *)
    DoIt(h, n, n.as_exp2);
    IF bracket THEN Append(h, n, NewToken(M3CToken.Ket)) END;
  END BINARY;

PROCEDURE BTokenFor(n: M3AST_AS.BINARY): M3CToken.T=
  BEGIN
    TYPECASE n OF <*NOWARN*>
    | M3AST_AS.Plus => RETURN M3CToken.Plus;
    | M3AST_AS.Minus => RETURN M3CToken.Minus;
    | M3AST_AS.Times => RETURN M3CToken.Times;
    | M3AST_AS.Rdiv => RETURN M3CToken.Divide;
    | M3AST_AS.Textcat => RETURN M3CToken.Ampersand;
    | M3AST_AS.Div => RETURN M3CToken.DIV_;
    | M3AST_AS.Mod => RETURN M3CToken.MOD_;
    | M3AST_AS.Eq => RETURN M3CToken.Equal;
    | M3AST_AS.Ne => RETURN M3CToken.NotEqual;
    | M3AST_AS.Gt => RETURN M3CToken.GreaterThan;
    | M3AST_AS.Lt => RETURN M3CToken.LessThan;
    | M3AST_AS.Ge => RETURN M3CToken.GreaterThanOrEqual;
    | M3AST_AS.Le => RETURN M3CToken.LessThanOrEqual;
    | M3AST_AS.And => RETURN M3CToken.AND_;
    | M3AST_AS.Or => RETURN M3CToken.OR_;
    | M3AST_AS.In => RETURN M3CToken.IN_;
    END;
  END BTokenFor;

PROCEDURE UNARY(h: Handle; n: M3AST_AS.UNARY)=
  VAR bracket := FALSE;
  BEGIN
    IF NOT ISTYPE(n, M3AST_AS.Deref) THEN 
      Append(h, n, NewToken(UTokenFor(n)));
      IF ISTYPE(n, M3AST_AS.Not) THEN Space(h, n); END;
    END;
    IF LowerPrec(n.as_exp, n, right := TRUE) THEN
      Append(h, n, NewToken(M3CToken.Bra)); bracket := TRUE;
    END; (* if *)
    DoIt(h, n, n.as_exp);
    IF ISTYPE(n, M3AST_AS.Deref) THEN 
      Append(h, n, NewToken(UTokenFor(n)));
    END;
    IF bracket THEN Append(h, n, NewToken(M3CToken.Ket)) END;
  END UNARY;

PROCEDURE UTokenFor(n: M3AST_AS.UNARY): M3CToken.T=
  BEGIN
    TYPECASE n OF <*NOWARN*>
    | M3AST_AS.Not => RETURN M3CToken.NOT_;
    | M3AST_AS.Unaryplus => RETURN M3CToken.Plus;
    | M3AST_AS.Unaryminus => RETURN M3CToken.Minus;
    | M3AST_AS.Deref => RETURN M3CToken.Dereference;
    END
  END UTokenFor;

PROCEDURE Select(h: Handle; n: M3AST_AS.Select)=
  BEGIN
    DoIt(h, n, n.as_exp);
    Append(h, n, NewToken(M3CToken.Dot));
    DoIt(h, n, n.as_id);
  END Select;

PROCEDURE Call(h: Handle; n: M3AST_AS.Call)=
  VAR
    m: M3AST_AS.Actual;
    iter := SeqM3AST_AS_Actual.NewIter(n.as_param_s);
    isFirst := TRUE;
  BEGIN
    DoIt(h, n, n.as_callexp);
    Append(h, n, NewToken(M3CToken.Bra));
    WHILE SeqM3AST_AS_Actual.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Append(h, n, NewToken(M3CToken.Ket));
  END Call;


PROCEDURE Index(h: Handle; n: M3AST_AS.Index)=
  VAR
    m: M3AST_AS.EXP;
    iter := SeqM3AST_AS_EXP.NewIter(n.as_exp_s);
    isFirst := TRUE;
  BEGIN
    DoIt(h, n, n.as_array);
    Append(h, n, NewToken(M3CToken.SquareBra));
    WHILE SeqM3AST_AS_EXP.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Append(h, n, NewToken(M3CToken.SquareKet));
  END Index;

PROCEDURE Actual(h: Handle; n: M3AST_AS_F.Actual)=
  BEGIN
    IF n.as_id # NIL THEN
      DoIt(h, n, n.as_id);
      Space(h, n);
      Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
    END;
    DoIt(h, n, n.as_exp_type);
  END Actual;


PROCEDURE Exp_used_id(h: Handle; n: M3AST_AS.Exp_used_id)= 
  BEGIN
    Append(h, n, n.vUSED_ID);
    IF n.vUSED_ID # NIL AND n.vUSED_ID.sm_def # NIL THEN 
      Append(h, n, n.vUSED_ID.sm_def); 
    END;
  END Exp_used_id;

PROCEDURE Block(h: Handle; n: M3AST_AS.Block)=

  PROCEDURE LastNodeOf(n: M3AST_AS.SRC_NODE): M3AST_AS.SRC_NODE=
    VAR result: M3AST_AS.SRC_NODE := NIL;
    PROCEDURE Visit(n: M3AST_AS.SRC_NODE)=
      VAR iter := n.newIter(); child: AST.NODE;
      BEGIN
        result := n;
        WHILE iter.next(child) DO
          IF child # NIL THEN
            Visit(child) 
          END;
        END;
      END Visit;
    BEGIN
      Visit(n);
      RETURN result
    END LastNodeOf;

  VAR
    m: M3AST_AS.DECL_REVL;
    iter := SeqM3AST_AS_DECL_REVL.NewIter(n.as_decl_s);
  BEGIN
    WHILE SeqM3AST_AS_DECL_REVL.Next(iter, m) DO
      DoIt(h, n, m); 
      IF ISTYPE(h.this_unit_id, M3AST_AS.Interface_id) THEN
        FlushComments(h, n, FALSE, LastNodeOf(m));
      END;
    END;
    IF h.isModule THEN
      Indent(h, n);
      Append(h, n, NewToken(M3CToken.BEGIN_));
      NLIncIndent(h, n);
    END; (* if *) 
    VisitSeqStm(h, n, n.as_stm_s);
    IF h.isModule THEN DecIndent(h); END;
    Indent(h, n); 
    Append(h, n, NewToken(M3CToken.END_));
  END Block;

PROCEDURE Assign_st(h: Handle; n: M3AST_AS.Assign_st)=
  BEGIN
    Indent(h, n);
    DoIt(h, n, n.as_lhs_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
    DoIt(h, n, n.as_rhs_exp);
  END Assign_st;


PROCEDURE Call_st(h: Handle; n: M3AST_AS.Call_st)=
  BEGIN
    Indent(h, n);
    DoIt(h, n, n.as_call);
  END Call_st;


PROCEDURE Case_st(h: Handle; n: M3AST_AS.Case_st)=
  VAR
    m: M3AST_AS.Case;
    iter := SeqM3AST_AS_Case.NewIter(n.as_case_s);
  BEGIN
    (* Process the case expression adding CASE before and OF after. *)
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.CASE_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.OF_)); NL(h, n);

    (* The cases are indented as far as checking is concerned. For
       formatting purposes no indentation is needed because the vertical
       bar separator and space generate the needed indentation. For
       this reason a separate indentation level count is stored for
       checking and for formatting purposes. *)

    IncCheckIndent(h);

    (* Process all the cases. *)

    WHILE SeqM3AST_AS_Case.Next(iter, m) DO DoIt(h, n, m); END;

    (* Restore the indentation level. *)

    DecCheckIndent(h);

    (* Process the ELSE clause. *)

    IF n.as_else # NIL THEN DoIt(h, n, n.as_else); END;
    Indent(h, n); 
    Append(h, n, NewToken(M3CToken.END_));
  END Case_st;


PROCEDURE Eval_st(h: Handle; n: M3AST_AS.Eval_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.EVAL_)); Space(h, n);
    DoIt(h, n, n.as_exp);
  END Eval_st;


PROCEDURE Exit_st(h: Handle; n: M3AST_AS.Exit_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.EXIT_));
  END Exit_st;


PROCEDURE For_st(h: Handle; n: M3AST_AS.For_st)=
  BEGIN
    CheckVarName(h,n.as_id);
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.FOR_)); Space(h, n);
    Append(h, n, n.as_id); Space(h, n); Append(h, n, NewToken(M3CToken.Becomes)); Space(h, n);
    DoIt(h, n, n.as_from); Space(h, n); Append(h, n, NewToken(M3CToken.TO_)); Space(h, n);
    DoIt(h, n, n.as_to);
    IF n.as_by # NIL THEN DoIt(h, n, n.as_by); END;    
    Space(h, n); Append(h, n, NewToken(M3CToken.DO_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n); 
    Append(h, n, NewToken(M3CToken.END_)); 
  END For_st;


PROCEDURE If_st(h: Handle; n: M3AST_AS.If_st)=
  VAR
    m: M3AST_AS.Elsif;
    iter := SeqM3AST_AS_Elsif.NewIter(n.as_elsif_s);
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.IF_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.THEN_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
    WHILE SeqM3AST_AS_Elsif.Next(iter, m) DO DoIt(h, n, m); END;
    IF n.as_else # NIL THEN DoIt(h, n, n.as_else); END;
    Indent(h, n); 
    Append(h, n, NewToken(M3CToken.END_)); 
  END If_st;


PROCEDURE Lock_st(h: Handle; n: M3AST_AS.Lock_st)=
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.LOCK_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.DO_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END Lock_st;


PROCEDURE Loop_st(h: Handle; n: M3AST_AS.Loop_st)=
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.LOOP_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END Loop_st;


PROCEDURE Raise_st(h: Handle; n: M3AST_AS.Raise_st)=
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.RAISE_)); Space(h, n);
    DoIt(h, n, n.as_qual_id);
    IF n.as_exp_void # NIL THEN 
      Space(h, n); 
      Append(h, n, NewToken(M3CToken.Bra)); 
      DoIt(h, n, n.as_exp_void); 
      Append(h, n, NewToken(M3CToken.Ket)); 
    END;
  END Raise_st;


PROCEDURE Repeat_st(h: Handle; n: M3AST_AS.Repeat_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.REPEAT_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.UNTIL_)); Space(h, n);
    DoIt(h, n, n.as_exp);
  END Repeat_st;


PROCEDURE Return_st(h: Handle; n: M3AST_AS.Return_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.RETURN_)); Space(h, n);
    IF n.as_exp # NIL THEN DoIt(h, n, n.as_exp); END;
  END Return_st;


PROCEDURE Try_st(h: Handle; n: M3AST_AS.Try_st)=
  BEGIN
    Indent(h, n); Append(h, n, NewToken(M3CToken.TRY_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
    DoIt(h, n, n.as_try_tail);
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END Try_st;


PROCEDURE Typecase_st(h: Handle; n: M3AST_AS.Typecase_st)=
  VAR
    m: M3AST_AS.Tcase;
    iter := SeqM3AST_AS_Tcase.NewIter(n.as_tcase_s);
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.TYPECASE_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.OF_)); NL(h, n);
    IncCheckIndent(h);
    WHILE SeqM3AST_AS_Tcase.Next(iter, m) DO DoIt(h, n, m); END;
    DecCheckIndent(h);
    IF n.as_else # NIL THEN DoIt(h, n, n.as_else); END;
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END Typecase_st;


PROCEDURE While_st(h: Handle; n: M3AST_AS.While_st)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.WHILE_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.DO_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END While_st;


PROCEDURE With_st(h: Handle; n: M3AST_AS.With_st)=
  VAR
    m: M3AST_AS.Binding;
    iter := SeqM3AST_AS_Binding.NewIter(n.as_binding_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.WITH_)); Space(h, n);
    WHILE SeqM3AST_AS_Binding.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Space(h, n); Append(h, n, NewToken(M3CToken.DO_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h); Indent(h, n);
    Append(h, n, NewToken(M3CToken.END_)); 
  END With_st;


PROCEDURE Case(h: Handle; n: M3AST_AS.Case)=
  VAR
    m: M3AST_AS.RANGE_EXP;
    iter := SeqM3AST_AS_RANGE_EXP.NewIter(n.as_case_label_s);
    isFirst := TRUE;
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.Bar)); Space(h, n);
    WHILE SeqM3AST_AS_RANGE_EXP.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    Space(h, n); 
    Append(h, n, NewToken(M3CToken.Implies)); Space(h, n);
    IncFmtIndent(h); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecFmtIndent(h); DecIndent(h); NL(h, n);
  END Case;

PROCEDURE Else_stm(h: Handle; n: M3AST_AS.Else_stm)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.ELSE_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
  END Else_stm;


PROCEDURE By(h: Handle; n: M3AST_AS.By)=
  BEGIN
    Space(h, n); Append(h, n, NewToken(M3CToken.BY_)); Space(h, n);
    DoIt(h, n, n.as_exp);
  END By;


PROCEDURE Elsif(h: Handle; n: M3AST_AS.Elsif)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.ELSIF_)); Space(h, n);
    DoIt(h, n, n.as_exp);
    Space(h, n); Append(h, n, NewToken(M3CToken.THEN_)); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
  END Elsif;


PROCEDURE Try_except(h: Handle; n: M3AST_AS.Try_except)=
  VAR
    m: M3AST_AS.Handler;
    iter := SeqM3AST_AS_Handler.NewIter(n.as_handler_s);
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.EXCEPT_)); 
    NL(h, n);
    IncCheckIndent(h);
    WHILE SeqM3AST_AS_Handler.Next(iter, m) DO DoIt(h, n, m); END;
    DecCheckIndent(h);
    IF n.as_else # NIL THEN DoIt(h, n, n.as_else); END;
  END Try_except;


PROCEDURE Try_finally(h: Handle; n: M3AST_AS.Try_finally)=
  BEGIN
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.FINALLY_));
    NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecIndent(h);
  END Try_finally;


PROCEDURE Tcase(h: Handle; n: M3AST_AS.Tcase)=
  VAR
    m: M3AST_AS.M3TYPE;
    iter := SeqM3AST_AS_M3TYPE.NewIter(n.as_type_s);
    isFirst := TRUE;
  BEGIN
    CheckVarName(h,n.as_id);
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.Bar)); Space(h, n);
    WHILE SeqM3AST_AS_M3TYPE.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    IF n.as_id # NIL THEN 
      Append(h, n, NewToken(M3CToken.Bra));
      Append(h, n, n.as_id); 
      Append(h, n, NewToken(M3CToken.Ket));
    END;
    Space(h, n); 
    Append(h, n, NewToken(M3CToken.Implies)); Space(h, n);
    IncFmtIndent(h); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecFmtIndent(h); DecIndent(h); NL(h, n);
  END Tcase;


PROCEDURE Handler(h: Handle; n: M3AST_AS.Handler)=
  VAR
    m: M3AST_AS.Qual_used_id;
    iter := SeqM3AST_AS_Qual_used_id.NewIter(n.as_qual_id_s);
    isFirst := TRUE;
  BEGIN
    CheckVarName(h,n.as_id);
    Indent(h, n);
    Append(h, n, NewToken(M3CToken.Bar)); Space(h, n);
    WHILE SeqM3AST_AS_Qual_used_id.Next(iter, m) DO 
      Between(h, n, isFirst, CS); DoIt(h, n, m); 
    END;
    IF n.as_id # NIL THEN 
      Append(h, n, NewToken(M3CToken.Bra));
      Append(h, n, n.as_id); 
      Append(h, n, NewToken(M3CToken.Ket));
    END;
    Space(h, n); 
    Append(h, n, NewToken(M3CToken.Implies)); Space(h, n); 
    IncFmtIndent(h); NLIncIndent(h, n);
    VisitSeqStm(h, n, n.as_stm_s);
    DecFmtIndent(h); DecIndent(h); NL(h, n);
  END Handler;


PROCEDURE Binding(h: Handle; n: M3AST_AS.Binding)=
  BEGIN
    CheckVarName(h,n.as_id);
    Append(h, n, n.as_id);
    Space(h, n); Append(h, n, NewToken(M3CToken.Equal)); Space(h, n);
    DoIt(h, n, n.as_exp);
  END Binding;

PROCEDURE VisitSeqStm(h: Handle; n: M3AST_AS.SRC_NODE_C; ss: SeqM3AST_AS_STM.T)=
  VAR
    m: M3AST_AS.STM;
    iter := SeqM3AST_AS_STM.NewIter(ss);
  BEGIN
    WHILE SeqM3AST_AS_STM.Next(iter, m) DO 
      DoIt(h, n, m);
      SCNL(h, n);
    END;
  END VisitSeqStm;

(* Add the comments to associated ast node. *)

PROCEDURE FlushComments(h: Handle; n: M3AST_AS.SRC_NODE_C; follow: BOOLEAN;
    m: M3AST_AS.SRC_NODE)=
  VAR p: PROCEDURE(t: M3CComment.T): M3AST_AS.SRC_NODE;
  BEGIN
    IF follow THEN p := M3CComment.FollowingNode;
    ELSE p := M3CComment.PrecedingNode;
    END;

    (* Take from the list all the commants applicable to the current node. *)

    WHILE h.comment # NIL AND p(h.comment) = m DO

      (* Attach to the node the text associated with the comment. *)

      D(h, n, M3CComment.Body(h.comment));
      NL(h, n);

      (* Fetch the next comment from the list. *)

      IF M3CComment.Next(h.comments, h.comment) THEN
      ELSE h.comment := NIL;
      END;
    END;
  END FlushComments;

(* All the deviations from the programming conventions are reported through
   this procedure. *)

PROCEDURE ReportError(t: TEXT; line, char: INTEGER) =
  BEGIN
    Wr.PutText(Stdio.stdout,t & ", line " & Fmt.Int(line) & ", char " &
        Fmt.Int(char) & "\n");
  END ReportError;

(* The identifier for a variable (or field and method) should start with
   a lower case character and contain only alphanumeric characters. *)

PROCEDURE CheckVarName(h: Handle; name: M3AST_AS.ID) =
  VAR
    t: TEXT;
    line, char: CARDINAL;
  BEGIN
    IF NOT h.checkFormat OR name = NIL THEN RETURN; END; (* name is optional *)
    t := M3CId.ToText(name.lx_symrep);
    IF Text.Length(t) = 0 THEN RETURN END; (* must be ok *)
    line := M3CSrcPos.Unpack(name.lx_srcpos,char);

    IF NOT Text.GetChar(t,0) IN ASCII.Lowers THEN
      ReportError("Identifier should start with a lower case",line,char);
    END;

    FOR i := 1 TO Text.Length(t) - 1 DO
      IF NOT Text.GetChar(t,i) IN id_char_set THEN
        ReportError("Non recommended character '" & Text.Sub(t,i,1) &
            "' variable in identifier",line,char + i);
      END;
    END;
  END CheckVarName;

(* The identifier for a constant (or unit and procedure) should start with
   an upper case character and only contain alphanumeric characters. *)

PROCEDURE CheckConstName(h: Handle; name: M3AST_AS.ID) =
  VAR
    t: TEXT;
    line, char: CARDINAL;
  BEGIN
    IF NOT h.checkFormat OR name = NIL THEN RETURN; END; (* name is optional *)
    t := M3CId.ToText(name.lx_symrep);
    IF Text.Length(t) = 0 THEN RETURN END; (* must be ok *)
    line := M3CSrcPos.Unpack(name.lx_srcpos,char);

    IF NOT Text.GetChar(t,0) IN ASCII.Uppers THEN
      ReportError("Identifier should start with an upper case",line,char);
    END;

    FOR i := 1 TO Text.Length(t) - 1 DO
      IF NOT Text.GetChar(t,i) IN id_char_set THEN
        ReportError("Non recommended character '" & Text.Sub(t,i,1) &
            "' in constant identifier",line,char + i);
      END;
    END;
  END CheckConstName;

(* If the node is the first on a new line, check its indentation. *)

PROCEDURE CheckIndentation(h: Handle; n: M3AST_AS.SRC_NODE_C) =
  VAR
    line, nline, pline, char, nchar, pchar: CARDINAL;
    inPragma: BOOLEAN := TRUE;
  BEGIN
    IF NOT h.checkFormat THEN RETURN; END;

    (* These types of nodes are not associated with source code text and
       their position may not be correct for checking the indentation.
       For instance, the position of the left hand side expression should
       be used for checking the indentation instead of the assignment
       statement which may have a strange position value since it is not
       associated with an actual source code token. *)

    TYPECASE n OF
    | M3AST_AS.Compilation_Unit, M3AST_AS.Call_st, M3AST_AS.Call,
          M3AST_AS.Actual, M3AST_AS.Select, M3AST_AS.BINARY,
          M3AST_AS.Assign_st => RETURN;

    (* Perhaps some of the types below will need to be added to the list
       of tokens to skip.

    M3AST_AS.Compilation_Unit, M3AST_AS.Interface, M3AST_AS.Qual_used_id,
    M3AST_AS.Module, M3AST_AS.Unsafe, M3AST_AS.Import_item,
    M3AST_AS.Simple_import, M3AST_AS.From_import, M3AST_AS.Revelation_s,
    M3AST_AS.Const_decl_s, M3AST_AS.Type_decl_s, M3AST_AS.Var_decl_s,
    M3AST_AS.Exc_decl_s, M3AST_AS.Proc_decl, M3AST_PG.Inline,
    M3AST_PG.External, M3AST_AS.Const_decl, M3AST_AS.Var_decl,
    M3AST_AS.Exc_decl, M3AST_AS.Subtype_decl, M3AST_AS.TYPE_DECL,
    M3AST_AS.Concrete_decl, M3AST_AS.REVELATION, M3AST_AS.Concrete_reveal,
    M3AST_AS.Subtype_reveal, M3AST_AS.Named_type, M3AST_AS.Integer_type,
    M3AST_AS.Real_type, M3AST_AS.LongReal_type, M3AST_AS.Extended_type,
    M3AST_AS.Null_type, M3AST_AS.RefAny_type, M3AST_AS.Address_type,
    M3AST_AS.Root_type, M3AST_AS.Array_type, M3AST_AS.Enumeration_type,
    M3AST_AS.Subrange_type, M3AST_AS.Record_type, M3AST_AS.Object_type,
    M3AST_AS.Set_type, M3AST_AS.Procedure_type, M3AST_AS.Ref_type,
    M3AST_AS.Packed_type, M3AST_AS.Opaque_type, M3AST_SM.Type_type,
    M3AST_SM.Any_type, M3AST_AS.Brand, M3AST_AS.Untraced,
    M3AST_AS.Fields, M3AST_AS.Method, M3AST_AS.Override,
    M3AST_AS.Formal_param, M3AST_AS.Raisees_any, M3AST_AS.Raisees_some,
    M3AST_AS.Range, M3AST_AS.Range_EXP, M3AST_AS.Constructor,
    M3AST_AS.Propagate, M3AST_AS.RANGE_EXP_elem, M3AST_AS.Actual_elem,
    M3AST_AS.BINARY, M3AST_AS.UNARY, M3AST_AS.Select,
    M3AST_AS.Call, M3AST_AS.Index, M3AST_AS.Actual,
    M3AST_AS.Exp_used_id, M3AST_AS.LITERAL, M3AST_AS.Block,
    M3AST_AS.Assign_st, M3AST_AS.Call_st, M3AST_AS.Case_st,
    M3AST_AS.Eval_st, M3AST_AS.Exit_st, M3AST_AS.For_st,
    M3AST_AS.If_st, M3AST_AS.Lock_st, M3AST_AS.Loop_st,
    M3AST_AS.Raise_st, M3AST_AS.Repeat_st, M3AST_AS.Return_st,
    M3AST_AS.Try_st, M3AST_AS.Typecase_st, M3AST_AS.While_st,
    M3AST_AS.With_st, M3AST_AS.Case, M3AST_AS.Else_stm,
    M3AST_AS.By, M3AST_AS.Elsif, M3AST_AS.Try_except,
    M3AST_AS.Try_finally, M3AST_AS.Tcase, M3AST_AS.Handler,
    M3AST_AS.Binding,
    *)

    ELSE
    END;

    (* Process all the pragmas preceeding the current node and then
       the current node. *)

    nline := M3CSrcPos.Unpack(n.lx_srcpos,nchar);
    LOOP
      IF h.pragma # NIL THEN
        pline := M3CSrcPos.Unpack(M3CPragma.Position(h.pragma),pchar);

        (* The next pragma is passed this node, process the node. *)

        IF (pline > nline) OR ((pline = nline) AND (pchar >= nchar)) THEN
          inPragma := FALSE;
          line := nline;
          char := nchar;

        (* The pragma is before the node, remove it from the list and
           process it. *)

        ELSE
          line := pline;
          char := pchar;
          IF M3CPragma.Next(h.pragmas, h.pragma) THEN
          ELSE h.pragma := NIL;
          END;
        END;

      (* There are no pragmas left, process the node. *)

      ELSE
        inPragma := FALSE;
        line := nline;
        char := nchar;
      END;

      (* This node is the first on its line, check the indentation. *)
        
      IF line > h.previousLine THEN

        (* We are on a continuation line, it did not fit the line length.
           a larger indentation is used. *)

        IF h.continuation THEN
          (* The indentation is not what it should. *)
          IF char # (h.checkIndent + 2) * Indent_Size THEN
            ReportError("Continued line indentation should be " &
                Fmt.Int((h.checkIndent + 2) * Indent_Size), line, char);
          END;

        (* It is a new line, check the indentation accordingly. *)

        ELSIF char # h.checkIndent * Indent_Size THEN
          (* The indentation is not what it should. *)
          ReportError("New line indentation should be " &
              Fmt.Int(h.checkIndent * Indent_Size), line, char);
        END;
      END;

      (* Remember the current line to detect line changes. *)

      h.previousLine := line;

      (* Pragmas may or may not start a new line. Typically FATAL do
         while UNUSED are within a PROCEDURE declaration. *)

      IF NOT inPragma THEN 
        h.continuation := TRUE;
        RETURN;
      END;
    END;
  END CheckIndentation;

(* The following are support procedures for adding the whitespace and
   other tokens to ast nodes, and for computing the indentation level. *)

(* Create precomputed whitespace strings for indentation. *)

PROCEDURE MkWS()=
  VAR indent := "";
  BEGIN
    FOR i := FIRST(WS) TO LAST(WS) DO 
      ws_g[i] := M3CWhitespace.Enter(WSV[i]);
    END;
    FOR i := 1 TO Indent_Size DO
      indent := indent & " ";
    END;
    FOR i := FIRST(indent_ws_g) TO LAST(indent_ws_g) DO
      VAR this_indent := "";
      BEGIN
        FOR j := 1 TO i DO
          this_indent := this_indent & indent;
        END;
        indent_ws_g[i] := M3CWhitespace.Enter(this_indent);
      END;
    END;
  END MkWS;

PROCEDURE NewToken(t: M3CToken.T): M3AST_LX.Token=
  BEGIN
    WITH n = NEW(M3AST_LX.Token) DO
      n.lx_token_rep := M3CToken.Token_rep(t);
      RETURN n;
    END
  END NewToken;

PROCEDURE NewWhitespace(ws: WS): M3AST_LX.Whitespace=
  BEGIN
    WITH n = NEW(M3AST_LX.Whitespace) DO
      n.lx_whitespace_rep := ws_g[ws];
      RETURN n;
    END
  END NewWhitespace;

(* Add a token with the specified text to the current node. *)

PROCEDURE D(<*UNUSED*> h: Handle; n: M3AST_AS.SRC_NODE_C; t: TEXT)=
  BEGIN
    WITH ws = NEW(M3AST_LX.Whitespace) DO
      ws.lx_whitespace_rep := M3CWhitespace.Enter(t);
      SeqM3AST_LX_SRC_NODE.AddRear(n.lx_node_s, ws);
    END
  END D;

(* Add the indentation white space before the current node. *)

PROCEDURE Indent(h: Handle; n: M3AST_AS.SRC_NODE)=
  BEGIN
    IF h.indent # 0 THEN
      WITH ws = NEW(M3AST_LX.Whitespace) DO
        ws.lx_whitespace_rep := indent_ws_g[h.indent];
        Append(h, n, ws);
      END;
    END;
  END Indent;

PROCEDURE NLIncIndent(h: Handle; n: M3AST_AS.SRC_NODE)=
  BEGIN
    NL(h, n); IncIndent(h);
  END NLIncIndent;

(* Update the current level of indentation. The indentation level for
   checking and formatting purposes is usually the same. However, for
   CASE, TYPECASE and TRY EXCEPT, the vertical bar separator is flush with 
   the enclosing block. Thus, from the point of view of formatting, there
   is no additional indentation. The checking, however, does not see the
   vertical bar which is not stored in the ast and verifies the indentation
   of the "case" following the bar. Thus, for checking purposes, the
   "case" appears indented. For this reason each indentation level is
   stored separately even if in most cases they are incremented and
   decremented together. *)

PROCEDURE IncIndent(h: Handle)=
  BEGIN
    INC(h.indent);
    INC(h.checkIndent);
  END IncIndent;

EXCEPTION IndentUnderflow;

PROCEDURE DecIndent(h: Handle)=
  BEGIN
    IF h.indent = 0 THEN 
      <*FATAL IndentUnderflow*>
      BEGIN RAISE IndentUnderflow; END;
    END;
    DEC(h.indent);
    DEC(h.checkIndent);
  END DecIndent;

PROCEDURE IncCheckIndent(h: Handle)=
  BEGIN
    INC(h.checkIndent);
  END IncCheckIndent;

PROCEDURE DecCheckIndent(h: Handle)=
  <*FATAL IndentUnderflow*>
  BEGIN
    IF h.checkIndent = 0 THEN RAISE IndentUnderflow; END;
    DEC(h.checkIndent);
  END DecCheckIndent;

PROCEDURE IncFmtIndent(h: Handle)=
  BEGIN
    INC(h.indent);
  END IncFmtIndent;

PROCEDURE DecFmtIndent(h: Handle)=
  <*FATAL IndentUnderflow*>
  BEGIN
    IF h.indent = 0 THEN RAISE IndentUnderflow; END;
    DEC(h.indent);
  END DecFmtIndent;

PROCEDURE Space(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.Space));
  END Space;

(* When a newline is produced, the current logical line is finished and
   the next token, when checked for indentation, should not be considered
   as a continuation. *)

PROCEDURE SCNL(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.SemiNewline));
    h.continuation := FALSE;
  END SCNL;

PROCEDURE CS(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.CommaSpace));
  END CS;

PROCEDURE ScS(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.SemiSpace));
  END ScS;

PROCEDURE NL(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.Newline));
    h.continuation := FALSE;
  END NL;

PROCEDURE CommaSpace(h: Handle; n: M3AST_AS.SRC_NODE_C)=
  BEGIN
    Append(h, n, NewWhitespace(WS.CommaSpace));
  END CommaSpace;

(* Process all the comments associated with this node. *)

PROCEDURE Append(h: Handle; n: M3AST_LX.SRC_NODE_C; s: M3AST_LX.SRC_NODE)=
  BEGIN
    FlushComments(h, n, TRUE, s);
    SeqM3AST_LX_SRC_NODE.AddRear(n.lx_node_s, s);
  END Append;

(* As long as we are "between" elements, call "p". This is typically used
   before each element in a list. For the first element nothing is done.
   Before each subsequent element "p" is called, for example to insert a
   coma and a space. *)

PROCEDURE Between(
    h: Handle;
    n: M3AST_AS.SRC_NODE_C;
    VAR isFirst: BOOLEAN;
    p: PROCEDURE(h: Handle; n: M3AST_AS.SRC_NODE_C)) =
  BEGIN
    IF isFirst THEN isFirst := FALSE; RETURN; END;
    p(h, n);
  END Between;

(* Initialize the precomputed white space strings used for indentation. *)

BEGIN
  MkWS();
END M3Conv.
