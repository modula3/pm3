(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RefType.m3                                            *)
(* Last modified on Tue May 23 15:26:35 PDT 1995 by kalsow     *)
(*      modified on Thu Dec  5 17:20:18 PST 1991 by muller     *)

MODULE RefType;

IMPORT M3, M3ID, CG, Token, Type, TypeRep, Scanner, ObjectType, Target;
IMPORT Null, Reff, Addr, Error, Module, M3Buf, Brand;
IMPORT Revelation, OpenArrayType, TipeMap, TipeDesc, TypeFP;
IMPORT ProcType, ObjectAdr, Word, M3RT;
IMPORT RecordType, ArrayType, ReffTransient, ObjectTransient;

TYPE
  P = Type.T BRANDED "RefType.T"OBJECT
        brand      : Brand.T;
	target     : Type.T;
        isTraced   : BOOLEAN;
        user_name  : TEXT;
        isTransient: BOOLEAN;
        warn       : BOOLEAN;
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

VAR root, refany: M3ID.T := M3ID.NoID;

PROCEDURE Parse (): Type.T =
  VAR brand: Brand.T := NIL;  traced: BOOLEAN := TRUE;  super: Type.T := NIL;
      transient: BOOLEAN := FALSE;
  BEGIN
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
      transient := TRUE;
    ELSIF (Scanner.cur.token = Token.T.tTRANSIENT) THEN
      transient := TRUE;
      Scanner.GetToken(); Scanner.Match(Token.T.tENDPRAGMA);(* <*TRANSIENT*> *)
      IF Scanner.cur.token = Token.T.tIDENT THEN
        IF refany = M3ID.NoID THEN refany := M3ID.Add ("REFANY"); END;
        IF (Scanner.cur.id = refany) THEN
          Scanner.GetToken ();		 (* REFANY *)
          RETURN ReffTransient.T;
        END;
        IF root = M3ID.NoID THEN root := M3ID.Add ("ROOT"); END;
        IF (Scanner.cur.id # root) THEN
          Error.ID (Scanner.cur.id, "expected <*TRANSIENT*> ROOT/REFANY");
        END;
        Scanner.GetToken ();		 (* ROOT *)
        super := ObjectTransient.T;
        IF (Scanner.cur.token # Token.T.tOBJECT)
          AND (Scanner.cur.token # Token.T.tBRANDED) THEN RETURN super END;
      ELSIF Scanner.cur.token = Token.T.tRECORD THEN
        RETURN RecordType.Parse (transient);
      ELSIF Scanner.cur.token = Token.T.tARRAY THEN
        RETURN ArrayType.Parse (transient);
      END;
    END;
    brand := Brand.Parse ();
    IF (Scanner.cur.token = Token.T.tREF) THEN
      IF (super # NIL) THEN Error.Msg ("expected OBJECT declaration") END;
      Scanner.GetToken (); (* REF *)
      VAR origin := Scanner.offset;
          t := Type.Parse();
          save := Scanner.offset;
      BEGIN
        Scanner.offset := origin;
        t := New (t, traced, transient, brand);
        Scanner.offset := save;
        RETURN t;
      END;
    ELSE (* must be an object type *)
      IF (super = NIL) AND (NOT traced) THEN
        Error.Msg ("expected UNTRACED ROOT OBJECT");
      END;
      IF (super = NIL) AND transient THEN
        Error.Msg ("expected <*TRANSIENT*> ROOT OBJECT");
      END;
      RETURN ObjectType.Parse (super, traced, transient, brand);
    END;
  END Parse;

PROCEDURE New (target: Type.T;  traced, transient: BOOLEAN;  brand: Brand.T;
               warn := TRUE):
  Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Ref);
    p.isTraced   := traced;
    p.brand      := brand;
    p.target     := target;
    p.user_name  := NIL;
    p.warn       := warn;
    p.isTransient:= transient;
    RETURN p;
  END New;

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

PROCEDURE IsTransient (t: Type.T): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    RETURN (p # NIL) AND (p.isTransient);
  END IsTransient;

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

PROCEDURE Check (p: P) =
  VAR
    t: Type.T;
    hash: INTEGER := 839;
    info: Type.Info;
    cs := M3.OuterCheckState;
  BEGIN
    Brand.Check (p.brand, p, hash, cs);

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
    p.info.isTransient := p.isTransient;

    INC (Type.recursionDepth); (*------------------------------------*)
      p.checked := TRUE;
      IF (p.target # NIL) THEN
        p.target := Type.CheckInfo (p.target, info);
      END;
    DEC (Type.recursionDepth); (*------------------------------------*)

    IF p.isTraced THEN
      IF p.warn
        AND p.isTransient AND info.isTraced AND NOT info.isTransient THEN
        Error.Warn (1, "transient ref type to a non-transient type");
      END;
    ELSIF info.isTraced AND Module.IsSafe() THEN
      Error.Msg ("unsafe: untraced ref type to a traced type");
    END;
    EVAL Type.IsAlignedOk (p.target, 0);
  END Check;

PROCEDURE Compiler (p: P) =
  BEGIN
    Type.Compile (p.target);
    CG.Declare_pointer (Type.GlobalUID (p), Type.GlobalUID (p.target),
                        Brand.ToText (p.brand), p.isTraced, p.isTransient);
  END Compiler;

PROCEDURE NoteRefName (t: Type.T;  name: TEXT) =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL) THEN p.user_name := name; END;
  END NoteRefName;

PROCEDURE InitTypecell (t: Type.T;  offset, prev: INTEGER) =
  TYPE TKind = M3RT.TypeKind;
  CONST Kind = ARRAY BOOLEAN OF TKind { TKind.Ref, TKind.Array};
  TYPE RK = ARRAY BOOLEAN OF M3RT.TraceKind;
  CONST Traced = ARRAY BOOLEAN OF RK
    { RK { M3RT.TraceKind.Untraced, M3RT.TraceKind.Untraced },
      RK { M3RT.TraceKind.Traced,   M3RT.TraceKind.Transient } };
  VAR
    p         : P := t;
    brand     := Brand.Compile (p.brand);
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
    isz       : INTEGER := Target.Integer.size;
    name_offs : INTEGER := 0;
    fp        := TypeFP.FromType (p);
    globals   := Module.GlobalData (is_const := FALSE);
    consts    := Module.GlobalData (is_const := TRUE);
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

    IF (p.user_name # NIL) THEN
      name_offs := CG.EmitText (p.user_name, is_const := TRUE);
    END;

    (* generate my Type cell info *)
    CG.Init_intt   (offset + M3RT.TC_selfID, isz, Type.GlobalUID (p), FALSE);
    FOR i := FIRST (fp.byte) TO LAST (fp.byte) DO
      CG.Init_intt (offset + M3RT.TC_fp + i * 8, 8, fp.byte[i], FALSE);
    END;
    CG.Init_intt (offset + M3RT.TC_traced, 8, ORD (Traced[p.isTraced, p.isTransient]), FALSE);
    CG.Init_intt (offset + M3RT.TC_kind, 8, ORD (Kind[dims > 0]), FALSE);
    CG.Init_intt (offset + M3RT.TC_dataAlignment, 8, alignment, FALSE);
    CG.Init_intt (offset + M3RT.TC_dataSize, isz, size, FALSE);
    IF (type_map >= 0) THEN
      CG.Init_var (offset + M3RT.TC_type_map, consts, type_map, FALSE);
    END;
    IF (gc_map >= 0) THEN
      CG.Init_var (offset + M3RT.TC_gc_map, consts, gc_map, FALSE);
    END;
    IF (type_desc >= 0) THEN
      CG.Init_var (offset + M3RT.TC_type_desc, consts, type_desc, FALSE);
    END;
    IF (initProc # NIL) THEN
      CG.Init_proc (offset + M3RT.TC_initProc, initProc, FALSE);
    END;
    IF (brand >= 0) THEN
      CG.Init_var (offset + M3RT.TC_brand, consts, brand, FALSE);
    END;
    IF (p.user_name # NIL) THEN
      CG.Init_var (offset + M3RT.TC_name,  consts, name_offs, FALSE);
    END;
    IF (prev # 0) THEN
      CG.Init_var (offset + M3RT.TC_next,  globals, prev, FALSE);
    END;

    IF (dims > 0) THEN
      (* REF ARRAY specific extensions to the typecell *)
      CG.Init_intt (offset + M3RT.ATC_nDimensions, isz, dims, FALSE);
      CG.Init_intt (offset + M3RT.ATC_elementSize, isz, elemSize, FALSE);
    END;

  END InitTypecell;

PROCEDURE GenTypeMap (p: P;  refs_only: BOOLEAN): INTEGER =
  (* generate my "TypeMap" (called by the garbage collector) *)
  BEGIN
    TipeMap.Start ();
    Type.GenMap (p.target, 0, -1, refs_only, transient := FALSE);
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
    name := Type.LinkName (p, "_INIT");
    CG.Comment (-1, FALSE, name);
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
       AND (a.isTransient = b.isTransient)
       AND Brand.Equal (a.brand, b.brand)
       AND ((a.target = NIL AND b.target = NIL)
             OR Type.IsEqual (a.target, b.target, x));
  END EqualChk;

PROCEDURE Subtyper (a: P;  b: Type.T): BOOLEAN =
  BEGIN
    IF Type.IsEqual (a, b, NIL) THEN RETURN TRUE END;

    IF Type.IsEqual (a, Null.T, NIL) THEN
      RETURN Type.IsSubtype (b, Reff.T)
          OR Type.IsSubtype (b, ReffTransient.T)
          OR Type.IsSubtype (b, Addr.T)
          OR ProcType.Is (b);
    END;

    RETURN ((a.isTraced) AND Type.IsEqual (b, Reff.T, NIL))
        OR ((a.isTraced) AND (a.isTransient)
            AND Type.IsEqual (b, ReffTransient.T, NIL))
        OR ((NOT a.isTraced) AND Type.IsEqual (b, Addr.T, NIL));
  END Subtyper;

PROCEDURE InitCoster (<*UNUSED*>p: P;  zeroed: BOOLEAN): INTEGER =
  BEGIN
    IF NOT zeroed THEN RETURN 1 ELSE RETURN 0 END;
  END InitCoster;

PROCEDURE GenDesc (p: P) =
  BEGIN
    IF Type.IsEqual (p, Reff.T, NIL) OR
       Type.IsEqual (p, ReffTransient.T, NIL) THEN
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
    ELSIF Type.IsEqual (p, ReffTransient.T, NIL) THEN
      x.tag := "$refanytransient";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, Addr.T, NIL) THEN
      x.tag := "$address";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, Null.T, NIL) THEN
      x.tag := "$null";
      x.n_nodes := 0;
    ELSE
      M3Buf.PutText (x.buf, "REF");
      IF p.isTraced THEN
        IF p.isTransient THEN M3Buf.PutText (x.buf, "-TRANSIENT") END;
      ELSE M3Buf.PutText (x.buf, "-UNTRACED") END;
      Brand.GenFPrint (p.brand, x);
      x.n_nodes  := 1;
      x.nodes[0] := p.target;
    END;
  END FPrinter;

BEGIN
END RefType.
