MODULE BldHooks;

IMPORT IntM3LibsTbl, QValue, BldQRep, M3ID, QVSeq, QVTbl, M3Libs, QMachRep;
IMPORT QVal;
FROM BldQuake IMPORT T;
FROM Quake IMPORT Error;


PROCEDURE DeleteFile(t: T; x: TEXT) RAISES {Error} =
  VAR args := NEW(REF ARRAY OF QValue.T, 1);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int  := M3ID.Add(x);
    args[0].ref  := NIL;    
    EVAL ExecHook(t, "delete_file", args);
  END DeleteFile;

PROCEDURE LinkFile(t: T; from, to: TEXT) RAISES {Error}=
  VAR args := NEW(REF ARRAY OF QValue.T, 2);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int  := M3ID.Add(from);
    args[0].ref  := NIL;
    args[1].kind := QValue.Kind.String;
    args[1].int  := M3ID.Add(to);
    args[1].ref  := NIL;
    EVAL ExecHook(t, "link_file", args);
  END LinkFile;

PROCEDURE EmacsCompile(t: T; x: TEXT) RAISES {Error}=
  VAR args := NEW(REF ARRAY OF QValue.T, 1);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int  := M3ID.Add(x);
    args[0].ref  := NIL;    
    EVAL ExecHook(t, "emacs_compile", args);
  END EmacsCompile;

PROCEDURE GenMapHooks(t: T) RAISES {Error}=
  BEGIN
    EVAL ExecHook(t, "gen_map_hooks", NIL);
  END GenMapHooks;

PROCEDURE BeforeDoM3Hooks(t: T) RAISES {Error}=
  BEGIN
    EVAL ExecHook(t, "before_do_m3_hooks", NIL);
  END BeforeDoM3Hooks;

PROCEDURE InstallUnitsHooks(t: T; install_impls: BOOLEAN): 
  REF ARRAY OF IntM3LibsTbl.T RAISES {Error}=
  VAR
    arg: QValue.T;
    arr: QVSeq.T;
    ret: REF ARRAY OF IntM3LibsTbl.T;
    tbl: QVTbl.T;
    id: INTEGER;
    m3libs_arr: QVSeq.T;
    m3libs: M3Libs.T;
    size: INTEGER;
    iterator: QVTbl.Iterator;
    args := NEW(REF ARRAY OF QValue.T, 1);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int := QValue.BoolID[install_impls];
    args[0].ref := NIL;
    IF NOT ExecHook(t, "install_units_hooks", args, TRUE) THEN
      RETURN NIL 
    END;
    t.pop(arg);
    arr := QVal.ToArray(t, arg);
    size := arr.size();
    IF size > 0 THEN
      ret := NEW(REF ARRAY OF IntM3LibsTbl.T, arr.size());
      FOR i := 0 TO LAST(ret^) DO
        tbl:= QVal.ToTable(t, arr.get(i));
        ret[i] := NEW(IntM3LibsTbl.Default).init();
        iterator := tbl.iterate();
        WHILE iterator.next(id, arg) DO
          m3libs_arr := QVal.ToArray(t, arg);
          m3libs := NEW(M3Libs.T, loc := QVal.ToText(t, m3libs_arr.get(0)),
                        hidden := QVal.ToBool(t, m3libs_arr.get(1)),
                        local := QVal.ToBool(t, m3libs_arr.get(2)));
          EVAL ret[i].put(id, m3libs);
        END;
      END;
      RETURN ret;
    ELSE
      RETURN NIL;
    END;
  END InstallUnitsHooks;

PROCEDURE WhereUnitsHooks(t: T): REF ARRAY OF IntM3LibsTbl.T RAISES {Error} =
  VAR
    arg: QValue.T;
    arr: QVSeq.T;
    ret: REF ARRAY OF IntM3LibsTbl.T;
    tbl: QVTbl.T;
    id: INTEGER;
    m3libs_arr: QVSeq.T;
    m3libs: M3Libs.T;
    size: INTEGER;
    iterator: QVTbl.Iterator;
  BEGIN
    IF NOT ExecHook(t, "where_units_hooks", NIL, TRUE) THEN
      RETURN NIL END;
    t.pop(arg);
    arr := QVal.ToArray(t, arg);
    size := arr.size();
    IF size > 0 THEN
      ret := NEW(REF ARRAY OF IntM3LibsTbl.T, arr.size());
      FOR i := 0 TO LAST(ret^) DO
        tbl:= QVal.ToTable(t, arr.get(i));
        ret[i] := NEW(IntM3LibsTbl.Default).init();
        iterator := tbl.iterate();
        WHILE iterator.next(id, arg) DO
          m3libs_arr := QVal.ToArray(t, arg);
          m3libs := NEW(M3Libs.T, loc := QVal.ToText(t, m3libs_arr.get(0)),
                        hidden := QVal.ToBool(t, m3libs_arr.get(1)),
                        local := QVal.ToBool(t, m3libs_arr.get(2)));
          EVAL ret[i].put(id, m3libs);
        END;
      END;
      RETURN ret;
    ELSE
      RETURN NIL;
    END;
  END WhereUnitsHooks;

PROCEDURE CompileC(t: T; src, obj: TEXT; incl: QVSeq.T; optimize, debug,
                   shared: BOOLEAN): INTEGER RAISES {Error} =
  VAR args := NEW(REF ARRAY OF QValue.T, 3);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int  := M3ID.Add(src);
    args[1].kind := QValue.Kind.String;
    args[1].int  := M3ID.Add(obj);
    args[2].kind := QValue.Kind.Array;
    args[2].ref  := incl;
    IF ExecHook(t, "m3_compile_c", args, TRUE) THEN
      t.pop(args[0]);
      RETURN QVal.ToInt(t, args[0]);
    ELSE
      RETURN -1;
    END;
  END CompileC;

PROCEDURE M3Link(t: T; prog: TEXT; objs, libs: QVSeq.T; debug, 
                 shared: BOOLEAN): INTEGER RAISES {Error} =
  VAR args := NEW(REF ARRAY OF QValue.T, 3);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int  := M3ID.Add(prog);
    args[1].kind := QValue.Kind.Array;
    args[1].ref  := objs;
    args[2].kind := QValue.Kind.Array;
    args[2].ref  := libs;
    IF ExecHook(t, "m3_link", args, TRUE) THEN
      t.pop(args[0]);
      RETURN QVal.ToInt(t, args[0]);
    ELSE
      RETURN -1;
    END;
  END M3Link;

PROCEDURE M3MakeLib(t: T; name: TEXT; libs, imp: QVSeq.T; static, 
                    shared: BOOLEAN): INTEGER RAISES {Error}=
  VAR args := NEW(REF ARRAY OF QValue.T, 3);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int  := M3ID.Add(name);
    args[1].kind := QValue.Kind.Array;
    args[1].ref  := libs;
    args[2].kind := QValue.Kind.Array;
    args[2].ref  := imp;
    IF ExecHook(t, "m3_make_lib", args, TRUE) THEN
      t.pop(args[0]);
      RETURN QVal.ToInt(t, args[0]);
    ELSE
      RETURN -1;
    END;   
  END M3MakeLib;

PROCEDURE NoteShlib(t: T; name: TEXT): INTEGER RAISES {Error} = 
  VAR args := NEW(REF ARRAY OF QValue.T, 1);
  BEGIN
    args[0] := QValue.T{QValue.Kind.String, M3ID.Add(name), NIL};
    IF NOT ExecHook(t, "m3_note_shlib", args, FALSE) THEN
      RETURN -1;
    END;
    RETURN 0;
  END NoteShlib;

PROCEDURE M3Assemble(t: T; src, obj: TEXT; optimize, debug, shared: BOOLEAN):
  INTEGER RAISES {Error}=
  VAR args := NEW(REF ARRAY OF QValue.T, 2);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int  := M3ID.Add(src);
    args[1].kind := QValue.Kind.String;
    args[1].int  := M3ID.Add(obj);
    IF ExecHook(t, "m3_assemble", args, TRUE) THEN
      t.pop(args[0]);
      RETURN QVal.ToInt(t, args[0]);
    ELSE
      RETURN -1;
    END;   
  END M3Assemble;

PROCEDURE M3Backend(t: T; src, obj: TEXT; optimize, debug, 
                    shared: BOOLEAN): INTEGER RAISES {Error}=
  VAR args := NEW(REF ARRAY OF QValue.T, 2);
  BEGIN
    args[0].kind := QValue.Kind.String;
    args[0].int  := M3ID.Add(src);
    args[1].kind := QValue.Kind.String;
    args[1].int  := M3ID.Add(obj);
    IF ExecHook(t, "m3_backend", args, TRUE) THEN
      t.pop(args[0]);
      RETURN QVal.ToInt(t, args[0]);
    ELSE
      RETURN -1;
    END;   
  END M3Backend;



PROCEDURE ExecHook(t: T; name: TEXT; READONLY args: REF ARRAY OF QValue.T;
                   isFunc: BOOLEAN := FALSE): BOOLEAN RAISES {Error} = 
  VAR val: QValue.T;
  BEGIN
    IF NOT t.get(M3ID.Add(name), val) THEN
      RETURN FALSE;
    END;
    t.call(QVal.ToProc(t, val), args, isFunc);
    RETURN TRUE;
  END ExecHook;


BEGIN

END BldHooks.
