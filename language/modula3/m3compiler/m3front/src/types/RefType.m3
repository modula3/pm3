(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RefType.m3                                            *)
(* Last modified on Tue May 23 15:26:35 PDT 1995 by kalsow     *)
(*      modified on Thu Dec  5 17:20:18 PST 1991 by muller     *)

MODULE RefType;

IMPORT M3, M3ID, CG, Token, Type, TypeRep, Scanner, ObjectType, Target;
IMPORT Null, Reff, Addr, Error, Expr, Module, M3Buf, TextExpr;
IMPORT M3String, Revelation, OpenArrayType, TipeMap, TipeDesc, TypeFP;
IMPORT ProcType, ObjectAdr, Word, M3RT;

TYPE
  P = Type.T BRANDED "RefType.T"OBJECT
	brandE     : Expr.T;
	brand      : M3String.T;
	target     : Type.T;
        isTraced   : BOOLEAN;
        user_name  : TEXT;
      OVERRIDES
        check      := Check;
        check_align:= TypeRep.ScalarAlign;
        isEqual    := EqualChk;
        isSubtype  := Subtyper;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := TypeRep.InitToZeros;
        mapper     := TypeRep.GenRefMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
      END;

TYPE
  BrandNode = BRANDED "RefType.BrandNode" REF RECORD
    next  : BrandNode;
    brand : M3String.T;
    type  : Type.T;
    error : BOOLEAN;
  END;

VAR root       : M3ID.T  := M3ID.NoID;
VAR brand_buf  : M3Buf.T := NIL;
VAR all_brands : ARRAY [0..97] OF BrandNode;

PROCEDURE Reset () =
  BEGIN
    FOR i := FIRST (all_brands) TO LAST (all_brands) DO
      all_brands[i] := NIL;
    END;
  END Reset;

PROCEDURE Parse (): Type.T =
  VAR brand: Expr.T;  traced: BOOLEAN;  super: Type.T;
  BEGIN
    traced := TRUE;
    brand := NIL;
    super := NIL;
    IF (Scanner.cur.token = Token.T.tUNTRACED) THEN
      Scanner.GetToken (); (* UNTRACED *)
      IF (Scanner.cur.token = Token.T.tIDENT) THEN
        IF root = M3ID.NoID THEN root := M3ID.Add ("ROOT"); END;
        IF (Scanner.cur.id # root) THEN
	  Error.ID (Scanner.cur.id, "expected UNTRACED ROOT");
	END;
	Scanner.GetToken (); (* IDENT *)
	super := ObjectAdr.T;
	IF (Scanner.cur.token # Token.T.tOBJECT)
          AND (Scanner.cur.token # Token.T.tBRANDED) THEN RETURN super END;
      END;
      traced := FALSE;
    END;
    brand := ParseBrand ();
    IF (Scanner.cur.token = Token.T.tREF) THEN
      IF (super # NIL) THEN Error.Msg ("expected OBJECT declaration") END;
      Scanner.GetToken (); (* REF *)
      RETURN New (Type.Parse (), traced, brand);
    ELSE (* must be an object type *)
      IF (super = NIL) AND (NOT traced) THEN
        Error.Msg ("expected UNTRACED ROOT OBJECT");
      END;
      RETURN ObjectType.Parse (super, traced, brand);
    END;
  END Parse;

PROCEDURE New (target: Type.T;  traced: BOOLEAN;  brand: Expr.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Ref);
    p.isTraced  := traced;
    p.brandE    := brand;
    p.brand     := NIL;
    p.target    := target;
    p.user_name := NIL;
    RETURN p;
  END New;

PROCEDURE ParseBrand (): Expr.T =
  VAR brand: Expr.T;
  BEGIN
    brand := NIL;
    IF (Scanner.cur.token = Token.T.tBRANDED) THEN
      Scanner.GetToken (); (* BRANDED *)
      IF (Scanner.cur.token IN Token.ExprStart)
        THEN brand := Expr.Parse ();
        ELSE brand := GenerateBrand ();
      END;
    END;
    RETURN brand;
  END ParseBrand;

PROCEDURE GenerateBrand (): Expr.T =
  CONST Suffix = ARRAY BOOLEAN OF CHAR { 'M', 'I' };
  VAR j: INTEGER;  counter := Module.CurrentCounter ();
  BEGIN
    (* bump the counter *)
    j := LAST (counter);
    WHILE (counter[j] = '9') DO  counter[j] := '0'; DEC (j);  END;
    counter[j] := VAL (ORD (counter[j]) + 1, CHAR);
    Module.SetCurrentCounter (counter);

    (* build the string *)
    IF (brand_buf = NIL) THEN brand_buf := M3Buf.New (); END;
    M3ID.Put      (brand_buf, Module.Name (NIL));
    M3Buf.PutText (brand_buf, "_^%#%^_");
    M3Buf.PutSub  (brand_buf, counter);
    M3Buf.PutChar (brand_buf, Suffix [Module.IsInterface ()]);

    RETURN TextExpr.New (M3String.Add (M3Buf.ToText (brand_buf)));
  END GenerateBrand;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    LOOP
      IF (t = NIL) THEN RETURN NIL END;
      IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
      IF (t.info.class = Type.Class.Ref) THEN RETURN t END;
      IF (t.info.class # Type.Class.Opaque) THEN RETURN NIL END;
      t := Revelation.LookUp (t);
    END;
  END Reduce;

PROCEDURE Is (t: Type.T): BOOLEAN =
  BEGIN
    RETURN (Reduce (t) # NIL);
  END Is;

PROCEDURE IsBranded (t: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    RETURN (p # NIL) AND (p.brand # NIL);
  END IsBranded;

PROCEDURE Split (t: Type.T;  VAR target: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    target := p.target;
    RETURN TRUE;
  END Split;

PROCEDURE NoteBrand (t: Type.T;  b: M3String.T) =
  VAR cell : INTEGER   := M3String.Hash (b) MOD NUMBER (all_brands);
  VAR node : BrandNode := all_brands[cell];
  BEGIN
    IF (b = NIL) OR (t = NIL) THEN RETURN END;
    LOOP
      IF (node = NIL) THEN
        (* add an entry to the table *)
        node := NEW (BrandNode, type := t, brand := b, error := FALSE);
        node.next := all_brands[cell];
        all_brands[cell] := node;
        RETURN;
      END;
      IF (node.brand = b) AND (node.type # t) THEN
        IF (t.origin # node.type.origin) THEN
          (* error, duplicate brand *)
          BrandError (t, b);
          IF NOT node.error THEN BrandError (node.type, b) END;
          node.error := TRUE;
        END;
        RETURN;
      END;
      node := node.next;
    END;
  END NoteBrand;

PROCEDURE BrandError (t: Type.T;  b: M3String.T) =
  VAR save := Scanner.offset;
  BEGIN
    Scanner.offset := t.origin;
    Error.Txt (M3String.ToText (b), "duplicate brand");
    Scanner.offset := save;
  END BrandError;

PROCEDURE Check (p: P) =
  VAR
    t: Type.T;
    hash: INTEGER;
    info: Type.Info;
    brand: Expr.T;
    cs := M3.OuterCheckState;
  BEGIN
    hash := 839;

    IF (p.brandE # NIL) THEN
      Expr.TypeCheck (p.brandE, cs);
      brand := Expr.ConstValue (p.brandE);
      IF (brand = NIL) THEN
        Error.Msg ("brand is not a constant");
      ELSIF TextExpr.Split (brand, p.brand) THEN
        hash := Word.Plus (Word.Times (hash, 37), M3String.Hash (p.brand));
        NoteBrand (p, p.brand);
      ELSE
        Error.Msg ("brand is not a TEXT constant");
      END;
    END;

    t := Type.Strip (p.target);
    IF (t # NIL) THEN
      hash := Word.Plus (Word.Times (hash, 43), ORD (t.info.class));
    END;

    p.info.size      := Target.Address.size;
    p.info.min_size  := Target.Address.size;
    p.info.alignment := Target.Address.align;
    p.info.mem_type  := CG.Type.Addr;
    p.info.stk_type  := CG.Type.Addr;
    p.info.class     := Type.Class.Ref;
    p.info.isTraced  := p.isTraced;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := hash;

    INC (Type.recursionDepth); (*------------------------------------*)
      p.checked := TRUE;
      IF (p.target # NIL) THEN
        p.target := Type.CheckInfo (p.target, info);
      END;
    DEC (Type.recursionDepth); (*------------------------------------*)

    IF (NOT p.isTraced) AND (info.isTraced) AND Module.IsSafe() THEN
      Error.Msg ("unsafe: untraced ref type to a traced type");
    END;
    EVAL Type.IsAlignedOk (p.target, 0);
  END Check;

PROCEDURE Compiler (p: P) =
  VAR brand: TEXT := NIL;
  BEGIN
    Type.Compile (p.target);
    IF (p.brand # NIL) THEN brand := M3String.ToText (p.brand) END;
    CG.Declare_pointer (Type.GlobalUID (p),
                        Type.GlobalUID (p.target),
                        brand, p.isTraced);
  END Compiler;

PROCEDURE NoteRefName (t: Type.T;  name: TEXT) =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN p.user_name := name; END;
  END NoteRefName;

PROCEDURE InitTypecell (t: Type.T;  offset, prev: INTEGER) =
  VAR
    p         : P := t;
    type_map  := GenTypeMap (p, refs_only := FALSE);
    gc_map    := GenTypeMap (p, refs_only := TRUE);
    type_desc := GenTypeDesc (p);
    initProc  := GenInitProc (p);
    dims      : INTEGER;
    size      : INTEGER;
    alignment : INTEGER;
    elemSize  : INTEGER;
    ta        : Type.T;
    info      : Type.Info;
    brand     : INTEGER := 0;
    isz       : INTEGER := Target.Integer.size;
    name_offs : INTEGER := 0;
    fp        := TypeFP.FromType (p);
    globals   := Module.GlobalData (NIL);
  BEGIN
    EVAL Type.CheckInfo (p.target, info);
    ta := Type.Base (p.target);
    dims := OpenArrayType.OpenDepth (ta);
    alignment := info.alignment;
    IF (dims = 0) THEN
      (* not an open array *)
      size := info.size;
      elemSize := 0;
    ELSE (* target is an open array *)
      WITH ai = Target.Integer.align, ae = info.alignment DO
        size := Target.Address.size;           (* address of the elements *)
        size := ((size + ai - 1) DIV ai) * ai; (* align. for the sizes *)
        INC (size, Target.Integer.size * dims);  (* the sizes *)
        size := ((size + ae - 1) DIV ae) * ae; (* align. for the elements *)
      END;
      elemSize := OpenArrayType.EltPack (ta);
    END;
    size := MAX (size DIV Target.Byte, 1);
    alignment := MAX (alignment DIV Target.Byte, 1);
    elemSize := elemSize DIV Target.Byte;

    IF (p.brand # NIL) THEN
      brand := Module.Allocate (Target.Char.size*(M3String.Length(p.brand)+1),
                                Target.Char.align, "brand");
      M3String.Init_chars (brand, p.brand);
    END;

    IF (p.user_name # NIL) THEN
      name_offs := CG.EmitText (p.user_name);
    END;

    (* generate my Type cell info *)
    CG.Init_intt   (offset + M3RT.TC_selfID, isz, Type.GlobalUID (p));
    FOR i := FIRST (fp.byte) TO LAST (fp.byte) DO
      CG.Init_intt (offset + M3RT.TC_fp + i * 8, 8, fp.byte[i]);
    END;
    IF (p.isTraced) THEN
      CG.Init_intt (offset + M3RT.TC_traced, isz, 1);
    END;
    CG.Init_intt   (offset + M3RT.TC_dataSize, isz, size);
    CG.Init_intt   (offset + M3RT.TC_dataAlignment, isz, alignment);
    IF (dims > 0) THEN
      CG.Init_intt (offset + M3RT.TC_nDimensions, isz, dims);
      CG.Init_intt (offset + M3RT.TC_elementSize, isz, elemSize);
    END;
    IF (type_map > 0) THEN
      CG.Init_var (offset + M3RT.TC_type_map, globals, type_map);
    END;
    IF (gc_map > 0) THEN
      CG.Init_var (offset + M3RT.TC_gc_map, globals, gc_map);
    END;
    IF (type_desc > 0) THEN
      CG.Init_var (offset + M3RT.TC_type_desc, globals, type_desc);
    END;
    IF (initProc # NIL) THEN
      CG.Init_proc (offset + M3RT.TC_initProc, initProc);
    END;
    IF (brand # 0) THEN
      CG.Init_var (offset + M3RT.TC_brand, globals, brand);
    END;
    IF (p.user_name # NIL) THEN
      CG.Init_var (offset + M3RT.TC_name,  globals, name_offs);
    END;
    IF (prev # 0) THEN
      CG.Init_var (offset + M3RT.TC_next,  globals, prev);
    END;
  END InitTypecell;

PROCEDURE GenTypeMap (p: P;  refs_only: BOOLEAN): INTEGER =
  (* generate my "TypeMap" (called by the garbage collector) *)
  BEGIN
    TipeMap.Start ();
    Type.GenMap (p.target, 0, -1, refs_only);
    RETURN TipeMap.Finish ("type map for ", Type.Name (p));
  END GenTypeMap;

PROCEDURE GenTypeDesc (p: P): INTEGER =
  (* generate my "TypeDesc" (called by the pickle machinery) *)
  BEGIN
    IF NOT p.isTraced THEN RETURN -1 END;
    TipeDesc.Start ();
    Type.GenDesc (p.target);
    RETURN TipeDesc.Finish ("type description for ", Type.Name (p));
  END GenTypeDesc;

PROCEDURE GenInitProc (p: P): CG.Proc =
  VAR name: TEXT;  proc: CG.Proc;  ref: CG.Var;  info: Type.Info;
  BEGIN
    IF Type.InitCost (p.target, TRUE) <= 0 THEN RETURN NIL END;

    (* generate the procedure body *)

    CG.Gen_location (p.origin);
    name := Module.Prefix (NIL) & Type.Name (p) & "_INIT";
    CG.Comment (-1, name);
    Scanner.offset := p.origin;
    CG.Gen_location (p.origin);
    proc := CG.Declare_procedure (M3ID.Add (name), 1, CG.Type.Void,
                                  lev := 0, cc := Target.DefaultCall,
                                  exported := FALSE, parent := NIL);
    ref := CG.Declare_param (M3ID.NoID, Target.Address.size,
                             Target.Address.align, CG.Type.Addr,
                             Type.GlobalUID (p),
                             in_memory := FALSE, up_level := FALSE,
                             f := CG.Always);
    CG.Begin_procedure (proc);
    

    (* initialize the referent *)
    EVAL Type.CheckInfo (p.target, info);
    CG.Load_addr (ref);
    CG.Boost_alignment (info.alignment);
    Type.InitValue (p.target, TRUE);

    CG.Exit_proc (CG.Type.Void);
    CG.End_procedure (proc);
    RETURN proc;
  END GenInitProc;

PROCEDURE EqualChk (a: P;  t: Type.T;  x: Type.Assumption): BOOLEAN =
  VAR b: P := t;
  BEGIN
    RETURN (a.isTraced = b.isTraced)
       AND (a.brand = b.brand)
       AND ((a.target = NIL AND b.target = NIL)
             OR Type.IsEqual (a.target, b.target, x));
  END EqualChk;

PROCEDURE Subtyper (a: P;  b: Type.T): BOOLEAN =
  BEGIN
    IF Type.IsEqual (a, b, NIL) THEN RETURN TRUE END;

    IF Type.IsEqual (a, Null.T, NIL) THEN
      RETURN Type.IsSubtype (b, Reff.T)
          OR Type.IsSubtype (b, Addr.T)
          OR ProcType.Is (b);
    END;

    RETURN ((a.isTraced) AND Type.IsEqual (b, Reff.T, NIL))
        OR ((NOT a.isTraced) AND Type.IsEqual (b, Addr.T, NIL));
  END Subtyper;

PROCEDURE InitCoster (<*UNUSED*>p: P;  zeroed: BOOLEAN): INTEGER =
  BEGIN
    IF NOT zeroed THEN RETURN 1 ELSE RETURN 0 END;
  END InitCoster;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF Type.IsEqual (p, Reff.T, NIL) THEN
      EVAL TipeDesc.AddO (TipeDesc.Op.Refany, p);
    ELSIF Type.IsEqual (p, Addr.T, NIL) THEN
      EVAL TipeDesc.AddO (TipeDesc.Op.Address, p);
    ELSIF Type.IsEqual (p, Null.T, NIL) THEN
      EVAL TipeDesc.AddO (TipeDesc.Op.Null, p);
    ELSE
      TypeRep.GenRefDesc (p);
    END;
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  BEGIN
    IF Type.IsEqual (p, Reff.T, NIL) THEN
      x.tag := "$refany";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, Addr.T, NIL) THEN
      x.tag := "$address";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, Null.T, NIL) THEN
      x.tag := "$null";
      x.n_nodes := 0;
    ELSE
      M3Buf.PutText (x.buf, "REF");
      IF (NOT p.isTraced) THEN M3Buf.PutText (x.buf, "-UNTRACED") END;
      IF (p.brand # NIL) THEN
        M3Buf.PutText (x.buf, "-BRAND ");
        M3Buf.PutInt  (x.buf, M3String.Length (p.brand));
        M3Buf.PutChar (x.buf, ' ');
        M3String.Put  (x.buf, p.brand);
      END;
      x.n_nodes  := 1;
      x.nodes[0] := p.target;
    END;
  END FPrinter;

BEGIN
END RefType.
