MODULE BldFace;

IMPORT M3ID, Quake, QValue, QVal, QVSeq, QVTbl, BldHooks, BldQuake, BldQRep, 
    M3Driver, Msg, QMachRep, TextSeq, LibSeq, Lib;

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
  COMPILER : M3ID.T;
  BACKEND  : M3ID.T;
  outarr   : ARRAY M3Driver.BackendOutput OF M3ID.T;

PROCEDURE Init(t: T; m: BldQuake.T):T RAISES {Error} =

  (* Get a value from its name in the quake machine. *)
  PROCEDURE GetIt(x: TEXT; VAR val: QValue.T) RAISES {Error}=
    BEGIN
      IF NOT m.get(M3ID.Add(x), val) THEN
        Msg.FatalError(NIL,"Quake template variable " & x & " not defined");
        RAISE Error;
      END;
    END GetIt;

  (* Get the option value, first element of the array mapped to the option. *)
  PROCEDURE GetOptionValue(option: TEXT; VAR val: QValue.T) 
      RAISES {Quake.Error, Error} =
    VAR
      tmpval: QValue.T;
      seq: QVSeq.T;
    BEGIN
      IF (NOT options.get(M3ID.Add(option),tmpval)) OR
         (tmpval.kind # QValue.Kind.Array) THEN 
        Msg.FatalError(NIL,"No array value for option " & option);
        RAISE Error;
      END;
      seq := QVal.ToArray(m,tmpval);
      IF seq.size() < 1 THEN 
        Msg.FatalError(NIL,"No value for option " & option);
        RAISE Error;
      END;
      val := seq.get(0);
    END GetOptionValue;

  VAR
    val    : QValue.T;
    id     : M3ID.T;
    options: QVTbl.T;
  BEGIN
    TRY
      t.machine := m;
      t.lib_name := m.lib_name;
      t.pgm_name := m.pgm_name;
      t.no_m3main := m.no_m3main;

      GetIt("M3_COVERAGE_LIB",val);    
      t.link_coverage := QVal.ToText(m,val);

      GetIt("INTERNAL_BACKEND",val);
      t.ext_pass_6 := NOT QVal.ToBool(m,val);

      GetIt("BOOTSTRAP_TO_IL",val);
      t.bootstrap_il := QVal.ToBool(m,val);

      GetIt("M3_BACKEND_OUTPUT",val);    
      id := QVal.ToID(m,val);
      FOR i := FIRST(outarr) TO LAST(outarr) DO
        IF id = outarr[i] THEN t.backend_output := i; EXIT END;
      END;

      t.ext_pass_7 := t.backend_output = M3Driver.BackendOutput.Asm;

      GetIt("TARGET",val);
      t.target := QVal.ToText(m,val);

      t.has_loader := FALSE;

      GetIt("OPTION_GUI",val);
      t.gui := QVal.ToBool(m,val);

      GetIt("Options",val);
      options := QVal.ToTable(m,val);

      GetOptionValue("standalone",val);
      t.shared_libs := NOT QVal.ToBool(m,val);

      GetOptionValue("shared_lib",val);
      t.gen_shared := QVal.ToBool(m,val);

      GetOptionValue("static_lib",val);
      t.gen_static := QVal.ToBool(m,val);

      GetOptionValue("m3main_in_C",val);      
      IF QVal.ToBool(m,val) THEN
        t.gen_method := M3Driver.GenMethod.Compiler;
      ELSE
        t.gen_method := M3Driver.GenMethod.Backend;
      END;

      GetOptionValue("dump_config",val);
      t.dump_config := QVal.ToBool(m,val);

      GetOptionValue("bootstrap",val);
      t.bootstrap_mode := QVal.ToBool(m,val);

      GetOptionValue("debuginfo",val);
      t.do_debug := QVal.ToBool(m,val);

      GetOptionValue("heap_stats",val);
      t.heap_stats := QVal.ToBool(m,val);

      GetOptionValue("keep_cache",val);
      t.keep_cache := QVal.ToBool(m,val);

      GetOptionValue("keep_files",val);
      t.keep_files := QVal.ToBool(m,val);

      GetOptionValue("no_make",val);
      t.make_mode := NOT QVal.ToBool(m,val);

      GetOptionValue("compile_once",val);
      t.compile_once := QVal.ToBool(m,val);

      GetOptionValue("optimization",val);
      t.do_optimize := QVal.ToBool(m,val);

      GetOptionValue("skip_link",val);
      t.skip_link := QVal.ToBool(m,val);

      GetOptionValue("times",val);
      t.times := QVal.ToBool(m,val);

      GetOptionValue("coverage",val);
      t.do_coverage := QVal.ToBool(m,val);

      GetOptionValue("msg_level",val);
      t.msg_level := QVal.ToInt(m,val);

      GetOptionValue("warning_level",val);
      t.warning_level := QVal.ToInt(m,val);
      m.m3front_options.addhi("-w" & QVal.ToText(m,val));
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

PROCEDURE MakeLib(t: T; name: TEXT; libs, imp: TextSeq.T; static,
                  shared: BOOLEAN) RAISES {InterErr} =
  VAR 
    libseq := NEW(QVSeq.T).init(libs.size());
    impseq := NEW(QVSeq.T).init(imp.size());
    val: QValue.T;
  BEGIN
    TRY
      val.kind := QValue.Kind.String;
      FOR I := 0 TO libs.size() - 1 DO
        val.int := M3ID.Add(libs.get(I));
        libseq.addhi(val);
      END;
      FOR I := 0 TO imp.size() - 1 DO
        val.int := M3ID.Add(imp.get(I));
        impseq.addhi(val);
      END;
      IF BldHooks.M3MakeLib(t.machine, name, libseq, impseq, static, 
                            shared) # 0 THEN
        RAISE InterErr;
      END;
    EXCEPT
      Quake.Error => RAISE InterErr;
    END;
  END MakeLib;

BEGIN
  COMPILER := M3ID.Add("COMPILER");
  BACKEND  := M3ID.Add("BACKEND");
  outarr[M3Driver.BackendOutput.Asm] := M3ID.Add("ASM");
  outarr[M3Driver.BackendOutput.Obj] := M3ID.Add("OBJ");
END BldFace.
