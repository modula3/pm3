(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE M3AST;

IMPORT M3ID, M3Lexer, Target;

TYPE
  T = OBJECT
    nodes       : REF ARRAY OF Node;
    floats      : REF ARRAY OF Target.Float;
    ints        : REF ARRAY OF Target.Int;
    texts       : REF ARRAY OF TEXT;
    safe        : BOOLEAN;
    interface   : BOOLEAN;
    external    : BOOLEAN;
    module_cc   : M3ID.T;  (* Target.CallingConvention name for module *)
    scope_info  : REFANY;  (* for use by M3Scope module *)
  END;

  Node = RECORD
    op     : BITS  8 FOR [0..255];  (* see OP_* definitions below *)
    width  : BITS 24 FOR NodeIndex;
    info   : INTEGER;
    client : INTEGER;
  END;

  NodeIndex = [0..16_ffffff];

TYPE
  ErrorHandler = PROCEDURE (msg: TEXT;  scan: M3Lexer.T): BOOLEAN;

PROCEDURE Parse (scan: M3Lexer.T;  err: ErrorHandler): T;
(* Parses the compilation unit in "scan" and returns its AST.  If an
   error is encountered during the parse, "err(msg,scan)" is called
   with "msg" describing the error.  If "err" returns "TRUE", the parse
   is terminated, otherwise it resumes.  "err" must be a top-level
   procedure. *)

PROCEDURE NumChildren (t: T;  n: NodeIndex): CARDINAL;
(* Returns the number of immediate children of node "n". *)

PROCEDURE GetChildren (t: T;  n: NodeIndex;  VAR ch: ARRAY OF NodeIndex): CARDINAL;
(* Returns the number of immediate children of node "n" and sets "ch"
   to their indicies.  If "ch" is too small, the extra children are
   left out of "ch", but the full count is returned. *)

PROCEDURE NthChild (t: T;  n: NodeIndex;  ch: CARDINAL): NodeIndex;
(* Returns the index of the "ch"-th (zero-based) immediate child of "n".
   Returns 0 if there is no such child. *)

CONST
  OP_Empty         =  00;

  OP_Unit          =  01;   OP_Generic       =  02;   OP_GenInstance   =  03;
  OP_Block         =  04;   OP_GenericArg    =  05;

  OP_Import        =  06;   OP_ImportAs      =  07;   OP_FromImport    =  08;
  OP_Export        =  09;
 
  OP_ConstDecl     =  10;   OP_TypeDecl      =  11;   OP_OpaqueDecl    =  12;
  OP_ExceptDecl    =  13;   OP_VarDecl       =  14;   OP_ProcDecl      =  15;
  OP_Reveal        =  16;   OP_RevealPartial =  17;   OP_VarDefn       =  18;

  OP_StmtList      =  19;   OP_Assign        =  20;   OP_Assert        =  21;
  OP_CallStmt      =  22;   OP_Case          =  23;   OP_CaseBranch    =  24;
  OP_CaseLabel     =  25;   OP_CaseRange     =  26;   OP_CaseElse      =  27;
  OP_Exit          =  28;   OP_Eval          =  29;   OP_For1          =  30;
  OP_ForN          =  31;   OP_If            =  32;   OP_IfClause      =  33;
  OP_IfElse        =  34;   OP_Lock          =  35;   OP_Loop          =  36;
  OP_Raise         =  37;   OP_RaiseValue    =  38;   OP_Repeat        =  39;
  OP_Return        =  40;   OP_ReturnValue   =  41;   OP_TryFinally    =  42;
  OP_TryExcept     =  43;   OP_TryHandler    =  44;   OP_TryHandlerVar =  45;
  OP_TryElse       =  46;   OP_TypeCase      =  47;   OP_TypeCaseArm   =  48;
  OP_TypeCaseVar   =  49;   OP_TypeCaseElse  =  50;   OP_While         =  51;
  OP_With          =  52;

  (* <type> *)
  OP_Array         =  53;   OP_OpenArray     =  54;   OP_Enum          =  55;
  OP_EnumDefn      =  56;   OP_NamedType     =  57;   OP_Method        =  58;
  OP_Override      =  59;   OP_Packed        =  60;   OP_ProcType      =  61;
  OP_Formal        =  62;   OP_FormalDefn    =  63;   OP_Field         =  64;
  OP_FieldDefn     =  65;   OP_Raises        =  66;   OP_RaisesAny     =  67;
  OP_Object        =  68;   OP_NoBrand       =  69;   OP_DefaultBrand  =  70;
  OP_Record        =  71;   OP_Ref           =  72;   OP_Root          =  73;
  OP_Set           =  74;   OP_Subrange      =  75;   OP_UntracedRef   =  76;
  OP_UntracedRoot  =  77;

  (* <expr> *)
  OP_Or            =  78;   OP_And           =  79;   OP_Not           =  80;
  OP_EQ            =  81;   OP_NE            =  82;   OP_LT            =  83;
  OP_LE            =  84;   OP_GT            =  85;   OP_GE            =  86;
  OP_Member        =  87;   OP_Add           =  88;   OP_Subtract      =  89;
  OP_Concat        =  90;   OP_Multiply      =  91;   OP_Divide        =  92;
  OP_Div           =  93;   OP_Mod           =  94;   OP_UnaryPlus     =  95;
  OP_UnaryMinus    =  96;   OP_Deref         =  97;   OP_Subscript     =  98;
  OP_CallExpr      =  99;   OP_ConsExpr      = 100;   OP_Etc           = 101;
  OP_RangeExpr     = 102;   OP_NameBind      = 103;   OP_Qualify       = 104;

  (* literals *)
  OP_Id            = 105;   OP_Int           = 106;   OP_BigInt        = 107;
  OP_Real          = 108;   OP_LReal         = 109;   OP_EReal         = 110;
  OP_Char          = 111;   OP_Text          = 112;

  (* pragmas *)
  OP_Attributes    = 113;   OP_Inline        = 114;   OP_Unused        = 115;
  OP_Obsolete      = 116;   OP_External      = 117;   OP_Alias         = 118;
  OP_CallConv      = 119;   OP_Fatal         = 120;   OP_FatalAny      = 121;

TYPE
  OP = [0..121];

TYPE
  XX = RECORD
    op         : OP;
    info_class : BITS 4 FOR [0..15];
    op_class   : BITS 4 FOR [0..15];
    min_ch     : [0..255];  (* mininum number of children *)
    max_ch     : [0..255];  (* maximum number of children, 255 = unbounded *)
  END;
(*
  Op Classes:
     0 -- empty
     1 -- Unit / Module
     2 -- Declaration
     3 -- Statement
     4 -- Expr Operator
     5 -- Literal
     6 -- Pragma
     7 -- Type

  Info Classes:
     0 -- no info
     1 -- info = M3ID.T
     2 -- info = parameter mode: 0=VALUE, 1=VAR, 2=READONLY
     3 -- info = integer value
     4 -- info = index into ast.ints^
     5 -- info = index into ast.floats^
     6 -- info = index into ast.texts^
     7 -- info = M3ID.T of Target.CallingConvention
     8 -- info = ORD (char)
*)

CONST
  OpMap = ARRAY OP OF XX{
  XX{OP_Empty,         0, 0, 0, 0   }, (*                                      *)

  XX{OP_Unit,          1, 1, 1, 255 }, (* {Export}, {<import>}, Block          *)
  XX{OP_Generic,       1, 1, 1, 255 }, (* {GenericArg}, {<import>}, Block      *)
  XX{OP_GenInstance,   1, 1, 1, 255 }, (* generic:Id,  {GenericArg}            *)
  XX{OP_Block,         1, 0, 0, 255 }, (* {<decl>}, StmtList                   *)
  XX{OP_GenericArg,    1, 1, 0, 0   }, (*                                      *)

  (* <import> / <export> *)
  XX{OP_Import,        1, 1, 0, 0   }, (*                                      *)
  XX{OP_ImportAs,      1, 1, 1, 1   }, (* import_interface: Id                 *)
  XX{OP_FromImport,    1, 1, 1, 1   }, (* import_interface: Id                 *)
  XX{OP_Export,        1, 1, 0, 0   }, (*                                      *)
 
  (* <decl> *)
  XX{OP_ConstDecl,     2, 1, 2, 3   }, (* Type|Empty, Expr, [Attr]             *)
  XX{OP_TypeDecl,      2, 1, 1, 2   }, (* Type, [Attr]                         *)
  XX{OP_OpaqueDecl,    2, 1, 1, 2   }, (* Type, [Attr]                         *)
  XX{OP_ExceptDecl,    2, 1, 1, 2   }, (* Type, [Attr]                         *)
  XX{OP_VarDecl,       2, 0, 2, 255 }, (* {VarDefn},Type|Empty,Expr|Empty,[Attr] *)
  XX{OP_ProcDecl,      2, 1, 2, 3   }, (* Type, [Attr], [Block]                *)
  XX{OP_Reveal,        2, 0, 2, 3   }, (* QID, Type, [Attr]                    *)
  XX{OP_RevealPartial, 2, 0, 2, 3   }, (* QID, Type, [Attr]                    *)
  XX{OP_VarDefn,       2, 1, 0, 0   }, (*                                      *)

  (* <stmt> *)
  XX{OP_StmtList,      3, 0, 0, 255 }, (* {Stmt}                               *)
  XX{OP_Assign,        3, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Assert,        3, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_CallStmt,      3, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_Case,          3, 0, 1, 255 }, (* Expr, {CaseBranch}, [CaseElse]       *)
  XX{OP_CaseBranch,    3, 0, 2, 255 }, (* {CaseLabel|CaseRange}, StmtList      *)
  XX{OP_CaseLabel,     3, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_CaseRange,     3, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_CaseElse,      3, 0, 1, 1   }, (* StmtList                             *)
  XX{OP_Exit,          3, 0, 0, 0   }, (*                                      *)
  XX{OP_Eval,          3, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_For1,          3, 1, 3, 3   }, (* from:Expr, to:Expr, StmtList         *)
  XX{OP_ForN,          3, 1, 4, 4   }, (* from:Expr, to:Expr, by:Expr, StmtList *)
  XX{OP_If,            3, 0, 1, 255 }, (* {IfClause}, [IfElse]                 *)
  XX{OP_IfClause,      3, 0, 2, 2   }, (* Expr, StmtList                       *)
  XX{OP_IfElse,        3, 0, 1, 1   }, (* StmtList                             *)
  XX{OP_Lock,          3, 0, 2, 2   }, (* Expr, StmtList                       *)
  XX{OP_Loop,          3, 0, 1, 1   }, (* StmtList                             *)
  XX{OP_Raise,         3, 0, 1, 1   }, (* QID                                  *)
  XX{OP_RaiseValue,    3, 0, 2, 2   }, (* QID, Expr                            *)
  XX{OP_Repeat,        3, 0, 2, 2   }, (* StmtList, Expr                       *)
  XX{OP_Return,        3, 0, 0, 0   }, (*                                      *)
  XX{OP_ReturnValue,   3, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_TryFinally,    3, 0, 2, 2   }, (* StmtList, StmtList                   *)
  XX{OP_TryExcept,     3, 0, 2, 255 }, (* StmtList, {TryHandler|TryHandlerVar}, [TypeElse] *)
  XX{OP_TryHandler,    3, 0, 2, 255 }, (* {QID}, StmtList                      *)
  XX{OP_TryHandlerVar, 3, 1, 2, 255 }, (* {QID}, StmtList                      *)
  XX{OP_TryElse,       3, 0, 1, 1   }, (* StmtList                             *)
  XX{OP_TypeCase,      3, 0, 2, 255 }, (* Expr, {TCArm|TCVar}, [TCElse]        *)
  XX{OP_TypeCaseArm,   3, 0, 2, 255 }, (* {Type}, StmtList                     *)
  XX{OP_TypeCaseVar,   3, 1, 2, 255 }, (* {Type}, StmtList                     *)
  XX{OP_TypeCaseElse,  3, 0, 1, 1   }, (* StmtList                             *)
  XX{OP_While,         3, 0, 2, 2   }, (* Expr, StmtList                       *)
  XX{OP_With,          3, 1, 2, 2   }, (* Expr, StmtList                       *)

  (* <type> *)
  XX{OP_Array,         7, 0, 2, 2   }, (* index:Type,  elt:Type                *)
  XX{OP_OpenArray,     7, 0, 1, 1   }, (* elt:Type                             *)
  XX{OP_Enum,          7, 0, 0, 255 }, (* {EnumDefn}                           *)
  XX{OP_EnumDefn,      7, 1, 0, 0   }, (*                                      *)
  XX{OP_NamedType,     7, 0, 1, 1   }, (* QID                                  *)
  XX{OP_Method,        7, 1, 2, 2   }, (* Type|Empty, Expr|Empty               *)
  XX{OP_Override,      7, 1, 1, 1   }, (* Expr                                 *)
  XX{OP_Packed,        7, 0, 2, 2   }, (* Expr, Type                           *)
  XX{OP_ProcType,      7, 7, 2, 255 }, (* {Formal}, Type|Empty, Raises|RaisesAny *)
  XX{OP_Formal,        7, 2, 3, 255 }, (* {FormalDefn} Type|Empty Expr|Empty   *)
  XX{OP_FormalDefn,    7, 1, 0, 0   }, (*                                      *)
  XX{OP_Field,         7, 0, 3, 255 }, (* {FieldDefn} Type|Empty Expr|Empty    *)
  XX{OP_FieldDefn,     7, 1, 0, 0   }, (*                                      *)
  XX{OP_Raises,        7, 0, 0, 255 }, (* {QID}                                *)
  XX{OP_RaisesAny,     7, 0, 0, 0   }, (*                                      *)
  XX{OP_Object,        7, 0, 2, 255 }, (* super:Type, <brand>, {Field}, {Method}, {Override}          *)
  XX{OP_NoBrand,       7, 0, 0, 0   }, (*                                      *)
  XX{OP_DefaultBrand,  7, 0, 0, 0   }, (*                                      *)
  XX{OP_Record,        7, 0, 0, 255 }, (* {Field}                              *)
  XX{OP_Ref,           7, 0, 2, 2   }, (* <brand>, Type                        *)
  XX{OP_Root,          7, 0, 0, 0   }, (*                                      *)
  XX{OP_Set,           7, 0, 1, 1   }, (* Type                                 *)
  XX{OP_Subrange,      7, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_UntracedRef,   7, 0, 2, 2   }, (* <brand>, Type                        *)
  XX{OP_UntracedRoot,  7, 0, 0, 0   }, (*                                      *)

  (* <expr> *)
  XX{OP_Or,            4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_And,           4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Not,           4, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_EQ,            4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_NE,            4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_LT,            4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_LE,            4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_GT,            4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_GE,            4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Member,        4, 0, 2, 2   }, (* elt:Expr, set:Expr                   *)
  XX{OP_Add,           4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Subtract,      4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Concat,        4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Multiply,      4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Divide,        4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Div,           4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_Mod,           4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_UnaryPlus,     4, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_UnaryMinus,    4, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_Deref,         4, 0, 1, 1   }, (* Expr                                 *)
  XX{OP_Subscript,     4, 0, 2, 2   }, (* array:Expr,  index:Expr              *)
  XX{OP_CallExpr,      4, 0, 1, 255 }, (* proc:Expr,  {Expr}                   *)
  XX{OP_ConsExpr,      4, 0, 1, 255 }, (* type:Expr,  {Expr}                   *)
  XX{OP_Etc,           4, 0, 0, 0   }, (* ".."                                 *)
  XX{OP_RangeExpr,     4, 0, 2, 2   }, (* Expr, Expr                           *)
  XX{OP_NameBind,      4, 0, 2, 2   }, (* id:Expr  value:Expr                  *)
  XX{OP_Qualify,       4, 1, 1, 1   }, (* Expr                                 *)

  (* literals *)
  XX{OP_Id,            5, 1, 0, 0   }, (*                                      *)
  XX{OP_Int,           5, 3, 0, 0   }, (*                                      *)
  XX{OP_BigInt,        5, 4, 0, 0   }, (*                                      *)
  XX{OP_Real,          5, 5, 0, 0   }, (*                                      *)
  XX{OP_LReal,         5, 5, 0, 0   }, (*                                      *)
  XX{OP_EReal,         5, 5, 0, 0   }, (*                                      *)
  XX{OP_Char,          5, 8, 0, 0   }, (*                                      *)
  XX{OP_Text,          5, 6, 0, 0   }, (*                                      *)


  XX{OP_Attributes,    6, 0, 0, 255 }, (* {attr}                               *)
  XX{OP_Inline,        6, 0, 0, 0   }, (*                                      *)
  XX{OP_Unused,        6, 0, 0, 0   }, (*                                      *)
  XX{OP_Obsolete,      6, 0, 0, 0   }, (*                                      *)
  XX{OP_External,      6, 0, 0, 0   }, (*                                      *)
  XX{OP_Alias,         6, 1, 0, 0   }, (*                                      *)
  XX{OP_CallConv,      6, 7, 0, 0   }, (*                                      *)
  XX{OP_Fatal,         6, 0, 0, 0   }, (* { QID }                              *)
  XX{OP_FatalAny,      6, 0, 0, 0   }  (*                                      *)
  };

  (* QID("x")   => (Id "x") *)
  (* QID("x.y") => (Qualify "y" (Id "x")) *)
  (* <brand> ::= Expr | NoBrand | DefaultBrand  *)
END M3AST.

(* Given a node index "i" in an AST "t", let "n" be "t.nodes[i]".  Then,
   "n.op" is the top-level operator of the subtree rooted at "i".

   The nodes "t.nodes[i]", ..., "t.nodes[i+n.width-1]" form the subtree rooted
   at node "i".

   The nodes of the tree are arranged in a top-down order with the root
   node first.  The node "t.node[i+1]" is the first child of "n", call it "ch".
   "t.node[i+1+ch.width]" is its second child, and so on.

   "n.client" is client data, uninterpreted by the parser.  It is initialized
   to zero.

   If an "M3AST" client fails to initialize the "Target" module,
   it will be initialized to correspond to the system named
   by "TARGET" in the cm3 configuration file (cm3.cfg).
*)

