MODULE BldFace;

IMPORT M3ID, Quake, QValue, QVal, QVSeq, BldHooks, BldQuake, BldQRep, M3Driver;
IMPORT Text, QMachRep, Msg, TextSeq, LibSeq, Lib;
FROM M3Driver IMPORT InterErr;

REVEAL
  T = Public BRANDED OBJECT
    machine: BldQuake.T;
  OVERRIDES
    init     := Init;
    compileC := CompileC;
    backend  := Backend;
    assemble := Assemble;
    link     := Link;
    makelib  := MakeLib;
  END;

VAR (* CONST *)
  BOTH     : M3ID.T;
  STATIC   : M3ID.T;
  SHARED   : M3ID.T;
  COMPILER : M3ID.T;
  BACKEND  : M3ID.T;
  outarr   : ARRAY M3Driver.BackendOutput OF M3ID.T;

PROCEDURE Init(t: T; m: BldQuake.T):T RAISES {Error} =
  PROCEDURE GetText(x: TEXT): TEXT RAISES {Quake.Error}=
    VAR val: QValue.T;
    BEGIN
      IF NOT m.get(M3ID.Add(x), val) THEN
        RETURN NIL;
      END;
      RETURN QVal.ToText(m, val);
    END GetText;
  PROCEDURE GetBool(x: TEXT): BOOLEAN RAISES {Quake.Error}=
    VAR val: QValue.T;
    BEGIN
      IF NOT m.get(M3ID.Add(x), val) THEN
        RETURN FALSE;
      END;
      RETURN QVal.ToBool(m, val);
    END GetBool;
  PROCEDURE GetInt(x: TEXT): INTEGER RAISES {Quake.Error}=
    VAR val: QValue.T;
    BEGIN
      IF NOT m.get(M3ID.Add(x), val) THEN
        RETURN 0;
      END;
      RETURN QVal.ToInt(m, val);
    END GetInt;
  PROCEDURE GetID(x: TEXT): INTEGER RAISES {Quake.Error}=
    VAR val: QValue.T;
    BEGIN
      IF NOT m.get(M3ID.Add(x), val) THEN
        RETURN M3ID.NoID;
      END;
      RETURN QVal.ToID(m, val);
    END GetID;
  VAR
    val    : QValue.T;
    id     : M3ID.T;
  BEGIN
    TRY
      t.machine := m;
      t.link_coverage := GetText("M3_COVERAGE_LIB");    
      t.conv := Text.GetChar(GetText("NAMING_CONVENTIONS"), 0);
      IF m.get(M3ID.Add("TARGET_NAMING_CONVENTIONS"), val) THEN
        t.target_conv := Text.GetChar(QVal.ToText(m, val), 0);
      END;
      t.shared_libs := NOT GetBool("M3_STANDALONE");
      t.ext_pass_6 := GetBool("M3_BACKEND_EXTERNAL");
    
      id := GetID("M3_BACKEND_OUTPUT");
      FOR i := FIRST(outarr) TO LAST(outarr) DO
        IF id = outarr[i] THEN t.backend_output := i; EXIT END;
      END;
      
      t.ext_pass_7    := m.get(M3ID.Add("m3_assemble"), val);
      t.target := GetText("TARGET");
      t.has_loader := GetBool("M3_HAS_LOADER");
      id := GetID("M3_GENERATE_LIB");
      IF id = BOTH THEN
        t.gen_shared := TRUE; t.gen_static := TRUE;
      ELSIF id = STATIC THEN
        t.gen_static := TRUE
      ELSIF id = SHARED THEN
        t.gen_shared := TRUE;
      ELSE
        Msg.FatalError(NIL, "invalid value for M3_GENERATE_LIB", M3ID.ToText(id));
      END;
      
      id := GetID("M3_M3MAIN");
      IF id = COMPILER THEN
        t.gen_method := M3Driver.GenMethod.Compiler;
      ELSIF id = BACKEND THEN
        t.gen_method := M3Driver.GenMethod.Backend;
      ELSE
        Msg.FatalError(NIL, "invalid value for M3_M3MAIN", M3ID.ToText(id));
      END;
    EXCEPT
      Quake.Error => RAISE Error;
    END;
    t.m3front_options := NEW(M3Driver.OptArr, m.m3front_options.size());
    FOR I := FIRST(t.m3front_options^) TO LAST(t.m3front_options^) DO
      t.m3front_options[I] := m.m3front_options.get(I);
    END;
    RETURN t;
  END Init;

PROCEDURE CompileC(t: T; source, object: TEXT; includes: TextSeq.T; 
                   optimize, debug, shared: BOOLEAN) RAISES {InterErr} =
  VAR seq  := NEW(QVSeq.T).init(includes.size());
      val  : QValue.T;
  BEGIN
    TRY
      val.kind := QValue.Kind.String;
      FOR I := 0 TO includes.size() - 1 DO
        val.int := M3ID.Add(includes.get(I));
        seq.addhi(val);
      END;
      IF BldHooks.CompileC(t.machine, source, object, seq, optimize, debug, shared) # 0 THEN
        RAISE InterErr;
      END;
    EXCEPT
      Quake.Error => RAISE InterErr;
    END;
  END CompileC;

PROCEDURE Backend(t: T; source, object: TEXT; optimize, debug, 
                  shared: BOOLEAN) RAISES {InterErr} =
  BEGIN
    TRY
      IF BldHooks.M3Backend(t.machine, source, object, optimize, debug, shared) # 0 THEN
        RAISE InterErr;
      END;
    EXCEPT
      Quake.Error => RAISE InterErr;
    END;
  END Backend;

PROCEDURE Assemble(t: T; source, object: TEXT; optimize, debug, 
                   shared: BOOLEAN) RAISES {InterErr} =
  BEGIN
    TRY
      IF BldHooks.M3Assemble(t.machine, source, object, optimize, debug, 
                             shared) # 0 THEN
        RAISE InterErr;
      END;
    EXCEPT
      Quake.Error => RAISE InterErr;
    END;
  END Assemble;

PROCEDURE Link(t: T; prog: TEXT; objs: TextSeq.T; libs: LibSeq.T; debug, 
               shared: BOOLEAN) RAISES {InterErr} =
  VAR
    objseq := NEW(QVSeq.T).init(objs.size());
    libseq := NEW(QVSeq.T).init(libs.size());
    lib    : Lib.T;
    seq    : QVSeq.T;
    val    : QValue.T;
  BEGIN
    TRY
      val.kind := QValue.Kind.String;
      FOR I := 0 TO objs.size() - 1 DO
        val.int := M3ID.Add(objs.get(I));
        objseq.addhi(val);
      END;

      FOR I := 0 TO libs.size() - 1 DO
        lib := libs.get(I);
        seq := NEW(QVSeq.T).init(2);
        val.kind := QValue.Kind.String;
        IF lib.dir # NIL THEN
          val.int := M3ID.Add(lib.dir);
        ELSE
          val.int := M3ID.Add("");
        END;
        seq.addhi(val);
        val.int := M3ID.Add(lib.base);
        seq.addhi(val);
        val.ref := seq;
        val.kind := QValue.Kind.Array;
        libseq.addhi(val);
      END;

      IF BldHooks.M3Link(t.machine, prog, objseq, libseq, debug, shared) # 0 THEN
        RAISE InterErr;
      END;
    EXCEPT
      Quake.Error => RAISE InterErr;
    END;
  END Link;

PROCEDURE MakeLib(t: T; name, libs, imp: TEXT; static,
                  shared: BOOLEAN) RAISES {InterErr} =
  BEGIN
    TRY
      IF BldHooks.M3MakeLib(t.machine, name, libs, imp, static, 
                            shared) # 0 THEN
        RAISE InterErr;
      END;
    EXCEPT
      Quake.Error => RAISE InterErr;
    END;
  END MakeLib;

BEGIN
  BOTH     := M3ID.Add("BOTH");
  STATIC   := M3ID.Add("STATIC");
  SHARED   := M3ID.Add("SHARED");
  COMPILER := M3ID.Add("COMPILER");
  BACKEND  := M3ID.Add("BACKEND");
  outarr[M3Driver.BackendOutput.Asm] := M3ID.Add("ASM");
  outarr[M3Driver.BackendOutput.Obj] := M3ID.Add("OBJ");
END BldFace.
