MODULE BldQuake EXPORTS BldQuake, BldQRep;

IMPORT M3Driver, M3Buf, QMachine, QCode, QValue, QVal, M3ID, Wr;
IMPORT QMachRep, IntTextTbl, Pathname, Text, M3File;
IMPORT TextLocTbl, IntMapTbl, TextTextTbl, QVSeq, IntM3LibsTbl;
IMPORT M3Path, M3Libs, TextSeq, TextRefTbl, Utils, Unit, OSError;
IMPORT FileWr, File, FS, Arg, BldPosix, BldWin32, BldHooks, Process;
IMPORT Pipe, Thread, TextList, BldFace, FileRd, Rd;
IMPORT Location AS Loc;
FROM Quake IMPORT Error;

CONST
  M3EXPORTS = ".M3EXPORTS";
  HIDDEN = TRUE;
  VISIBLE = FALSE;
  LOCAL = TRUE;
  IMPORTED = FALSE;
  NOT_A_PACKAGE_TEXT = "-not-a-package!";
  M3TFILE = ".M3IMPTAB";
  M3SHIP_FILE = ".M3SHIP";
  M3OVERRIDES = ".M3OVERRIDES";
  M3WEB = ".M3WEB";

VAR
  NOT_A_PACKAGE: M3ID.T;
  all: M3ID.T;

REVEAL
  T = Private BRANDED "BldQuake.T" OBJECT
  OVERRIDES
    init := Init;
    setup := Setup;
    getsl := GetSL;
  END;

TYPE
  NK = M3Path.Kind;

TYPE
  ProcRec = RECORD
    proc     : QValue.Proc;
    readonly := FALSE;
  END;

PROCEDURE Init (t: T; wr: Wr.T; package: TEXT; package_dir: Pathname.T; 
                build_dir: Pathname.T): T RAISES {Error} = 
  VAR
    value := QValue.T{QValue.Kind.Proc, 0, NIL};
    procs := InitProcs();
  BEGIN
    EVAL QMachine.T.init(t, wr);

    (* reset the compiler *)
    M3Driver.ResetCompiler(t.cur_wr());
    
    FOR i := FIRST (procs^) TO LAST (procs^) DO
      value.ref := procs[i].proc;
      t.put (procs[i].proc.info.name, value, procs[i].readonly);
    END;
    t.package_dir := package_dir;
    t.build_dir := build_dir;
    t.package := package;
    t.build_package := package;
    t.all_ship_dirs := NEW(TextRefTbl.Default).init();
    t.imports := NEW(IntTextTbl.Default).init();
    t.pkg_cache := NEW(IntTextTbl.Default).init(10);
    EVAL t.pkg_cache.put(M3ID.Add(t.package), t.package_dir);
    t.pkg_overrides := NEW(IntTextTbl.Default).init(10);
    t.locations := NEW(TextLocTbl.Default).init(10);
    t.pkg_dirs := NEW(IntMapTbl.Default).init(10);
    t.m3libs := NEW(IntM3LibsTbl.Default).init();
    t.m3libs_x := NEW(TextSeq.T).init();
    t.other_libs := NEW(IntM3LibsTbl.Default).init();
    t.other_libs_x := NEW(TextSeq.T).init();
    t.interface_sources := NEW(IntM3LibsTbl.Default).init();
    t.module_sources := NEW(IntM3LibsTbl.Default).init();
    t.generic_interface_sources := NEW(IntM3LibsTbl.Default).init();
    t.generic_module_sources := NEW(IntM3LibsTbl.Default).init();
    t.c_sources := NEW(IntM3LibsTbl.Default).init();
    t.c_inputs := NEW(IntTextTbl.Default).init();
    t.h_sources := NEW(IntM3LibsTbl.Default).init();
    t.h_inputs := NEW(TextSeq.T).init();
    t.h_dirs := NEW(TextTextTbl.Default).init();
    t.s_sources := NEW(IntM3LibsTbl.Default).init();
    t.tfile_args := M3Buf.New();
    t.templates := NEW(IntM3LibsTbl.Default).init();
    t.derived_sources := NEW(TextTextTbl.Default).init();
    t.resources := NEW(TextTextTbl.Default).init();
    t.cleanup_procs := NEW(TextSeq.T).init();
    t.m3_options := NEW(TextSeq.T).init();
    t.m3front_options := NEW(TextSeq.T).init();
    t.conv := NIL;
    t.target_conv := NIL;
    t.lib_name := NIL;
    t.pgm_name := NIL;
    t.no_m3main := FALSE;
    RETURN t;
  END Init;

(*----------------------------------------------------- package locations ---*)

PROCEDURE DoPkg(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    QMachRep.PushString(t, Pkg(t, QVal.ToText(t, arg)));
  END DoPkg;

PROCEDURE Pkg(t: T; x: TEXT): Pathname.T =
  VAR path: Pathname.T;
      id := M3ID.Add(x);
  BEGIN
    IF t.pkg_cache.get(id, path) THEN
      RETURN path;
    END;
    path := t.PKG_USE;
    EVAL t.pkg_overrides.get(id, path);
    path := path & t.SL & x;
    EVAL t.pkg_cache.put(id, path);
    RETURN path;
  END Pkg;

PROCEDURE DoM3include(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR fn, fname, dir, pkg: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(pkg);
    t.pop(dir);
    t.pop(fname);
    t.pop(fn);
    M3include(t, QVal.ToText(t, fn), QVal.ToText(t, fname), QVal.ToText(t, dir),
              QVal.ToText(t, pkg));
  END DoM3include;

PROCEDURE M3include(t: QMachine.T; fn: TEXT; fname: TEXT; dir: Pathname.T;
                    pkg: TEXT) RAISES {Error}=
  BEGIN
    WITH t = NARROW(t, T) DO
      TRY
        QMachRep.Include(t, fn);
      EXCEPT
      | Error(msg) =>
          RAISE Error("error while reading \"" & fname & "\"" & t.CR &
              "    from directory \"" & dir & "\" of package \"" & pkg & 
              "\" (" & Pkg(t, pkg) & ")" & t.CR & t.CR & msg);
      END;
    END;
  END M3include;

(*----------------------------------------------------- general locations ---*)

PROCEDURE DoLocation(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR pkg, subdir: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(subdir);
    t.pop(pkg);
    QMachRep.PushString(t, Location(t, QVal.ToText(t, pkg), 
                                 QVal.ToText(t, subdir)));
  END DoLocation;

PROCEDURE Location(t: T; pkg: TEXT; subdir: Pathname.T): Pathname.T =
  VAR 
    id := M3ID.Add(pkg); 
    dir_map: TextTextTbl.T;
    new_path: Pathname.T;
  BEGIN
    IF id = NOT_A_PACKAGE THEN
      EVAL t.locations.put(subdir, Loc.T{NOT_A_PACKAGE, subdir});
      RETURN subdir;
    END;

    IF NOT t.pkg_dirs.get(id, dir_map) THEN
      (* we've never heard of this package before *)
      IF Pathname.Absolute(subdir) THEN
        new_path := subdir;
      ELSE
        new_path := Pkg(t, pkg) & t.SL & subdir;
      END;
      dir_map := NEW(TextTextTbl.Default).init(5);
      EVAL dir_map.put(subdir, new_path);
      EVAL t.pkg_dirs.put(id, dir_map);
      EVAL t.locations.put(new_path, Loc.T{id, subdir});
      RETURN new_path;
    END;

    (* see if we've heard of this subdirectory *)
    IF NOT dir_map.get(subdir, new_path) THEN
      (* nope, it's a new subdirectory *)
      IF Pathname.Absolute(subdir) THEN
        new_path := subdir;
      ELSE
        new_path := Pkg(t, pkg) & t.SL & subdir;
      END;
      EVAL dir_map.put(subdir, new_path);
      EVAL t.locations.put(new_path, Loc.T{id, subdir});
    END;

    RETURN new_path;
  END Location;

PROCEDURE DoLocPkg(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    QMachRep.PushString(t, LocPkg(t, QVal.ToText(t, arg)));
  END DoLocPkg;

PROCEDURE LocPkg(t: T; loc: TEXT): TEXT =
  VAR rec: Loc.T;
  BEGIN
    IF t.locations.get(loc, rec) THEN
      RETURN M3ID.ToText(rec.id);
    END;
    RETURN NIL;
  END LocPkg;

PROCEDURE DoLocSubdir(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    QMachRep.PushString(t, LocSubdir(t, QVal.ToText(t, arg)));
  END DoLocSubdir;

PROCEDURE LocSubdir(t: T; loc: TEXT): TEXT =
  VAR rec: Loc.T;
  BEGIN
    EVAL t.locations.get(loc, rec);
    RETURN rec.subdir;
  END LocSubdir;

(*-------------------------------------------------------- relative paths ---*)

PROCEDURE DoPathOf(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    QMachRep.PushString(t, PathOf(t, QVal.ToText(t, arg)));
  END DoPathOf;

PROCEDURE PathOf(t: T; x: Pathname.T): Pathname.T RAISES {Error} =
  VAR p := Pathname.Prefix(M3ID.ToText(
                               t.includes[t.reg.ip-1].file.source_file));
  BEGIN
    (* XXX DERIVED SOURCES !!! *)
    IF NOT Text.Equal(p, t.path_of_path) THEN
      t.path_of_path := p;
      WITH subdir = PkgSubdir(t) DO
       IF Pathname.Absolute(subdir) THEN
          t.path_of_base := subdir & t.SL;
       ELSE
          t.path_of_base := Pkg(t, t.package) & t.SL & subdir & t.SL;
        END;
      END;
    END;
    RETURN t.path_of_base & x;
  END PathOf;

PROCEDURE DoNormalize(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR a, b: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(b);
    t.pop(a);
    QMachRep.PushString(t, Normalize(t, QVal.ToText(t, a), 
                                     QVal.ToText(t, b)));
  END DoNormalize;

PROCEDURE Normalize(t: T; prefix: Pathname.T; unfixed: Pathname.T): Pathname.T 
  RAISES {Error} =
  BEGIN
    TRY
      RETURN Pathname.Compose(
                 QMachRep.StripPrefix(t, Pathname.Decompose(prefix),
                                      QMachRep.CanonicalizePath(Pathname.Decompose(unfixed))));
    EXCEPT
    | Pathname.Invalid =>
      RAISE Error("Normalize: invalid pathname!");
    END;
  END Normalize;


PROCEDURE DoPkgSubdir(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  BEGIN
    <* ASSERT n_args = 0 *>
    QMachRep.PushString(t, PkgSubdir(t));
  END DoPkgSubdir;

PROCEDURE PkgSubdir(t: T): Pathname.T RAISES {Error} =
  VAR p := 
      Pathname.Prefix(M3ID.ToText(t.includes[t.reg.ip-1].file.source_file));
  BEGIN
    IF NOT Text.Equal(p, t.pkg_subdir_path) THEN
      t.pkg_subdir_path := p;
      t.pkg_subdir_base := Normalize(t, t.package_dir, t.pkg_subdir_path);
    END;
    RETURN t.pkg_subdir_base;
  END PkgSubdir;

(*----------------------------------------------------------------- names ---*)

PROCEDURE DoProgramName(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    QMachRep.PushString(t, ProgramName(t, QVal.ToText(t, arg)));
  END DoProgramName;

PROCEDURE ProgramName(t: T; x: TEXT): TEXT=
  BEGIN
    RETURN x & t.PGM_ext;
  END ProgramName;

PROCEDURE DoLibraryName(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    QMachRep.PushString(t, LibraryName(t, QVal.ToText(t, arg)));
  END DoLibraryName;

PROCEDURE LibraryName(t: T; x: TEXT): TEXT=
  BEGIN
    RETURN t.LIB_pre & x & t.LIB_ext;
  END LibraryName;

(*------------------------------------ calls used in generated files only ---*)

(* U_DoDefineLib, U_DoDefinePmg are empty functions right now. *)

PROCEDURE Do_ImportTemplate(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x, pkg, subdir: QValue.T;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(subdir);
    t.pop(pkg);
    t.pop(x);
    U_ImportTemplate(t, QVal.ToText(t, x), QVal.ToText(t, pkg), QVal.ToText(t, subdir));
  END Do_ImportTemplate;

PROCEDURE U_ImportTemplate(t: T; x, pkg, subdir: TEXT) RAISES {Error}=
  BEGIN
    M3include(t, Pkg(t, pkg) & t.SL & subdir & t.SL & x, x, subdir, pkg);
  END U_ImportTemplate;

PROCEDURE Do_ImportM3Lib(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x, pkg, subdir: QValue.T;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(subdir);
    t.pop(pkg);
    t.pop(x);
    U_ImportM3Lib(t, QVal.ToText(t, x), QVal.ToText(t, pkg), QVal.ToText(t, subdir));  
  END Do_ImportM3Lib;

PROCEDURE U_ImportM3Lib(t: T; x, pkg, subdir: TEXT) =
  VAR id := M3ID.Add(x);
  BEGIN
    EVAL t.m3libs.put(id, NEW(M3Libs.T, loc := Location(t, pkg, subdir), 
                              hidden := HIDDEN, 
                              local := IMPORTED));
    t.m3libs_x.addlo(x);
  END U_ImportM3Lib;

PROCEDURE Do_ImportOtherLib(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x, pn, l: QValue.T;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(l);
    t.pop(pn);
    t.pop(x);
    U_ImportOtherLib(t, QVal.ToText(t, x), QVal.ToText(t, pn), QVal.ToBool(t, l));
  END Do_ImportOtherLib;



PROCEDURE U_ImportOtherLib(t: T; x: TEXT; pn: TEXT; l: BOOLEAN)=
  VAR id := M3ID.Add(x); lib: M3Libs.T;
  BEGIN
    IF NOT t.other_libs.get(id, lib) THEN
      EVAL t.other_libs.put(id, NEW(M3Libs.T, 
                                    loc := Location(t, NOT_A_PACKAGE_TEXT, 
                                                    pn), 
                                    hidden := HIDDEN,
                                    local := l));
      t.other_libs_x.addhi(x);
    END;
  END U_ImportOtherLib;

PROCEDURE Do_MapAddInterface(t: QMachine.T; n_args := 4) RAISES {Error} =
  VAR unit, pkg, subdir, visibility: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(visibility);
    t.pop(subdir);
    t.pop(pkg);
    t.pop(unit);
    U_MapAddInterface(t, QVal.ToID(t, unit), QVal.ToText(t, pkg), QVal.ToText(t, subdir), QVal.ToBool(t, visibility));
  END Do_MapAddInterface;

PROCEDURE U_MapAddInterface(t: T; unit: M3ID.T; pkg, subdir: TEXT; 
                            visibility: BOOLEAN)=
  BEGIN
    EVAL t.interface_sources.put(unit, 
                                 NEW(M3Libs.T, loc := Location(t, pkg, subdir),
                                     hidden := visibility, 
                                     local := IMPORTED));
  END U_MapAddInterface;

PROCEDURE Do_MapAddGenericInterface(t: QMachine.T; n_args: INTEGER) 
  RAISES {Error}=
  VAR unit, pkg, subdir, visibility: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(visibility);
    t.pop(subdir);
    t.pop(pkg);
    t.pop(unit);
    TRY
      U_MapAddGenericInterface(t, QVal.ToText(t, pkg), QVal.ToText(t, subdir), 
                               QVal.ToBool(t, visibility), QVal.ToID(t, unit));
    EXCEPT
      M3Driver.Error => CErr("U_MapAddGenericInterface");
    END;
  END Do_MapAddGenericInterface;

PROCEDURE U_MapAddGenericInterface(t: T; pkg, subdir: TEXT; 
                                   visibility: BOOLEAN; 
                                   unit: M3ID.T := M3ID.NoID; 
                                   unit_name: TEXT := NIL) RAISES {M3Driver.Error} =
  VAR loc := Location(t, pkg, subdir);
  BEGIN
    IF unit_name = NIL THEN 
      unit_name := M3ID.ToText(unit)
    ELSIF unit = M3ID.NoID THEN 
      unit := M3ID.Add(unit_name) END;

    M3Driver.AddSourceFile(NIL, loc & t.SL & unit_name, TRUE);
    EVAL t.generic_interface_sources.put(unit,
                                         NEW(M3Libs.T, loc := Location(t, pkg,
                                                                       subdir),
                                             hidden := visibility, 
                                             local := IMPORTED));
  END U_MapAddGenericInterface;

PROCEDURE Do_MapAddModule(t: QMachine.T; n_args := 4) RAISES {Error} =
  VAR unit, pkg, subdir, visibility: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(visibility);
    t.pop(subdir);
    t.pop(pkg);
    t.pop(unit);
    U_MapAddModule(t, QVal.ToID(t, unit), QVal.ToText(t, pkg), 
                   QVal.ToText(t, subdir), QVal.ToBool(t, visibility));
  END Do_MapAddModule;

PROCEDURE U_MapAddModule(t: T; unit: M3ID.T; pkg, subdir: TEXT; 
                            visibility: BOOLEAN)=
  BEGIN
    EVAL t.module_sources.put(unit, 
                                 NEW(M3Libs.T, loc := Location(t, pkg, subdir),
                                     hidden := visibility, local := IMPORTED));
  END U_MapAddModule;

PROCEDURE Do_MapAddGenericModule(t: QMachine.T; n_args: INTEGER) 
  RAISES {Error}=
  VAR unit, pkg, subdir, visibility: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(visibility);
    t.pop(subdir);
    t.pop(pkg);
    t.pop(unit);
    TRY 
      U_MapAddGenericModule(t, QVal.ToText(t, pkg), QVal.ToText(t, subdir), 
                            QVal.ToBool(t, visibility), QVal.ToID(t, unit));
    EXCEPT
      M3Driver.Error => CErr("U_MapAddGenericModule");
    END;
  END Do_MapAddGenericModule;

PROCEDURE U_MapAddGenericModule(t: T; pkg, subdir: TEXT; 
                                   visibility: BOOLEAN; 
                                   unit: M3ID.T := M3ID.NoID;
                                   unit_name: TEXT := NIL) RAISES {M3Driver.Error}=
  VAR loc := Location(t, pkg, subdir);
  BEGIN
    IF unit_name = NIL THEN 
      unit_name := M3ID.ToText(unit)
    ELSIF unit = M3ID.NoID THEN 
      unit := M3ID.Add(unit_name) END;

    M3Driver.AddSourceFile(NIL, loc & t.SL & unit_name, TRUE);
    EVAL t.generic_module_sources.put(unit,
                                         NEW(M3Libs.T, loc := Location(t, pkg,
                                                                       subdir),
                                             hidden := visibility, 
                                             local := IMPORTED));
  END U_MapAddGenericModule;

PROCEDURE Do_MapAddC(t: QMachine.T; n_args := 4) RAISES {Error} =
  VAR unit, pkg, subdir, visibility: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(visibility);
    t.pop(subdir);
    t.pop(pkg);
    t.pop(unit);
    U_MapAddC(t, QVal.ToID(t, unit), QVal.ToText(t, pkg), 
              QVal.ToText(t, subdir), QVal.ToBool(t, visibility));
  END Do_MapAddC;

PROCEDURE U_MapAddC(t: T; unit: M3ID.T; pkg, subdir: TEXT;
                    visibility: BOOLEAN) =
  BEGIN
    EVAL t.c_sources.put(unit, 
                         NEW(M3Libs.T, loc := Location(t, pkg, subdir),
                             hidden := visibility, local := IMPORTED));
  END U_MapAddC;


PROCEDURE Do_MapAddH(t: QMachine.T; n_args: INTEGER) 
  RAISES {Error}=
  VAR unit, pkg, subdir, visibility: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(visibility);
    t.pop(subdir);
    t.pop(pkg);
    t.pop(unit);
    U_MapAddH(t, QVal.ToID(t, unit), QVal.ToText(t, pkg), 
              QVal.ToText(t, subdir), QVal.ToBool(t, visibility));
  END Do_MapAddH;

PROCEDURE U_MapAddH(t: T; unit: M3ID.T; pkg, subdir: TEXT; 
                    visibility: BOOLEAN; unit_name: TEXT := NIL)=
  VAR loc := Location(t, pkg, subdir);
  BEGIN
    IF unit_name = NIL THEN unit_name := M3ID.ToText(unit) END;
    EVAL t.h_sources.put(unit, NEW(M3Libs.T, loc := Location(t, pkg, subdir),
                                   hidden := visibility, local := IMPORTED));
    t.h_inputs.addhi(loc & t.SL & unit_name);
    EVAL t.h_dirs.put(loc, loc);
    IF visibility = VISIBLE THEN
      TRY
        M3Driver.AddSourceFile(loc, unit_name, TRUE);
      EXCEPT
        M3Driver.Error => CErr("M3Driver.AddSourceFile");
      END;
    END;
  END U_MapAddH;

PROCEDURE Do_MapAddS(t: QMachine.T; n_args := 4) RAISES {Error} =
  VAR unit, pkg, subdir, visibility: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(visibility);
    t.pop(subdir);
    t.pop(pkg);
    t.pop(unit);
    U_MapAddS(t, QVal.ToID(t, unit), QVal.ToText(t, pkg), 
              QVal.ToText(t, subdir), QVal.ToBool(t, visibility));
  END Do_MapAddS;

PROCEDURE U_MapAddS(t: T; unit: M3ID.T; pkg, subdir: TEXT;
                    visibility: BOOLEAN)=
  BEGIN
    EVAL t.s_sources.put(unit, 
                         NEW(M3Libs.T, loc := Location(t, pkg, subdir),
                             hidden := visibility, local := IMPORTED));
  END U_MapAddS;

(*--------------------------------------------------------------- imports ---*)

PROCEDURE DoIncludeDir(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    IncludeDir(t, QVal.ToText(t, arg));
  END DoIncludeDir;

PROCEDURE IncludeDir(t: T; dir: TEXT) RAISES {Error}=
  BEGIN
    dir := Pathname.Prefix(M3ID.ToText(
                               t.includes[t.reg.ip-1].file.source_file)) & 
                               t.SL & dir & t.SL & "m3makefile";
    IF NOT M3File.IsReadable(dir) THEN
      RAISE Error("unable to read " & dir);
    END;
    QMachRep.Include(t, dir);
  END IncludeDir;

PROCEDURE DoIncludePkg(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    IncludePkg(t, QVal.ToText(t, arg));
  END DoIncludePkg;

PROCEDURE IncludePkg(t: T; pkg: TEXT) RAISES {Error}=
  VAR
    save_pkg := t.package;
    save_dir := t.package_dir;
  BEGIN
    t.package := pkg;
    t.package_dir := Normalize(t, "", Pkg(t, pkg));
    M3include (t, t.package_dir & t.SL & "src" & t.SL & "m3makefile", 
               "m3makefile", "src", t.package);
    t.package := save_pkg;
    t.package_dir := save_dir;
  END IncludePkg;

PROCEDURE DoImportObj(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    ImportObj(t, QVal.ToText(t, arg));
  END DoImportObj;

PROCEDURE ImportObj(t: T; x: TEXT) RAISES {Error} =
  VAR file := PathOf(t, x);
  BEGIN
    TRY
      M3Driver.AddSourceFile(NIL, file, TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;
  END ImportObj;

PROCEDURE DoImport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    Import(t, QVal.ToText(t, arg));
  END DoImport;

PROCEDURE Import(t: T; p: TEXT) RAISES {Error}=
  BEGIN
    ImportVersion(t, p, t.build_dir);
  END Import;

PROCEDURE DoImportVersion(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR version, p: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(version);
    t.pop(p);
    ImportVersion(t, QVal.ToText(t, p), QVal.ToText(t, version), 
                  QVal.ToID(t, p));
  END DoImportVersion;

PROCEDURE ImportVersion(t: T; p, version: TEXT; id := M3ID.NoID) RAISES {Error}=
  BEGIN
    IF Text.Equal(p, t.build_package) THEN
      RAISE Error("cannot import package into itself: \"" & p & "\"" & t.CR & t.CR);
    END;
    IF id = M3ID.NoID THEN id := M3ID.Add(p) END;
    IF NOT t.imports.put(id, version) THEN
      M3include(t, Pkg(t, p) & t.SL & version & t.SL & M3EXPORTS,
                M3EXPORTS, version, p);
    END;
  END ImportVersion;

PROCEDURE DoImportLib(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR pn, x: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(pn);
    t.pop(x);
    ImportLib(t, QVal.ToText(t, x), QVal.ToText(t, pn));
  END DoImportLib;

PROCEDURE ImportLib(t: T; x: TEXT; pn: TEXT)=
  BEGIN
    U_ImportOtherLib(t, x, pn, LOCAL);
  END ImportLib;

(*--------------------------------------------------------------- objects ---*)

PROCEDURE DoPgmObject(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR ext, x: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(ext);
    t.pop(x);
    PgmObject(t, QVal.ToText(t, x), QVal.ToText(t, ext));
  END DoPgmObject;

PROCEDURE PgmObject(t: T; x: TEXT; ext: TEXT) RAISES {Error} =
  BEGIN
  (* Do nothing. May be used to collect object files later *)
  END PgmObject;

(*-------------------------------------------------------------- deleting ---*)
(* If the variable `_clean' is defined, delete all files `d.e', where e is *)
(* one of the extensions in `ext'. *)

PROCEDURE DoDeriveds(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    d, ext: QValue.T;
    seq: QVSeq.T;
    arr: REF ARRAY OF TEXT;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(ext);
    t.pop(d);
    seq := QVal.ToArray(t, ext);
    arr := NEW(REF ARRAY OF TEXT, seq.size());
    FOR i := 0 TO seq.size() - 1 DO
      arr[i] := QVal.ToText(t, seq.get(i));
    END;
    Deriveds(t, QVal.ToText(t, d), arr^);
  END DoDeriveds;

PROCEDURE Deriveds(t: T; d: TEXT; READONLY ext: ARRAY OF TEXT) RAISES {Error}=
  VAR 
    wr := t.cur_wr();
    tmp: TEXT;
    val: QValue.T;
  BEGIN
    IF t.get(M3ID.Add("_clean"), val) THEN
      IF t.get(M3ID.Add("_quiet"), val) THEN
        FOR i := FIRST(ext) TO LAST(ext) DO
          BldHooks.DeleteFile(t, d & ext[i]);
        END;
      ELSE
        Wr.PutText(wr, "delete"); <* NOWARN *>
        FOR i:= FIRST(ext) TO LAST(ext) DO
          tmp := d & ext[i];
          Wr.PutText(wr, " " & tmp); <* NOWARN *>
          BldHooks.DeleteFile(t, tmp);
        END;
        Wr.PutText(wr, t.CR); <* NOWARN *>
      END;
    END;
  END Deriveds;

(*--------------------------------------------------------------- sources ---*)

PROCEDURE DoPgmSource(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    TRY
      M3Driver.AddSourceFile(NIL, PathOf(t, QVal.ToText(t, arg)) ,
                             cmd_line := TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;
  END DoPgmSource;

PROCEDURE DoInterface(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    Interface(t, QVal.ToText(t, arg), HIDDEN);
  END DoInterface;


PROCEDURE DoINTERFACE(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    Interface(t, QVal.ToText(t, arg), VISIBLE);
  END DoINTERFACE;

PROCEDURE Interface(t: T; x: TEXT; visibility: BOOLEAN) RAISES {Error} =
  VAR 
    id : M3ID.T;
  BEGIN
    id := M3ID.Add(x & ".i3");
    EVAL t.interface_sources.put(id, NEW(M3Libs.T, 
                                         loc := Location(t, t.package, 
                                                         PkgSubdir(t)),
                                         hidden := visibility,
                                         local := LOCAL));
    PgmObject(t, x, t.IO_ext);
    TRY
      M3Driver.AddSourceFile(NIL, PathOf(t, x & ".i3") , cmd_line := TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;
    Deriveds(t, x, t.intf_extensions);
  END Interface;

PROCEDURE Implementation(t: T; x: TEXT) RAISES {Error} =
  VAR 
    id : M3ID.T;
  BEGIN
    id := M3ID.Add(x & ".m3");
    EVAL t.module_sources.put(id, NEW(M3Libs.T, 
                                         loc := Location(t, t.package, 
                                                         PkgSubdir(t)),
                                         hidden := HIDDEN,
                                         local := LOCAL));
    PgmObject(t, x, t.MO_ext);
    TRY
      M3Driver.AddSourceFile(NIL, PathOf(t, x & ".m3") , cmd_line := TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;
    Deriveds(t, x, t.impl_extensions);
  END Implementation;

PROCEDURE DoImplementation(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    Implementation(t, QVal.ToText(t, arg));
  END DoImplementation;


PROCEDURE DoModule(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR 
    arg: QValue.T; x: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    Interface(t, x, HIDDEN);
    Implementation(t, x);
  END DoModule;

PROCEDURE DoMODULE(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR 
    arg: QValue.T; x: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    Interface(t, x, VISIBLE);
    Implementation(t, x);
  END DoMODULE;

PROCEDURE HSource(t: T; x: TEXT) RAISES {Error}=
  VAR here := PathOf(t, ""); fn := x & ".h";
  BEGIN
    EVAL t.h_sources.put(M3ID.Add(fn), NEW(M3Libs.T, 
                                          loc := Location(t, t.package, 
                                                          PkgSubdir(t)),
                                          hidden := VISIBLE,
                                          local := LOCAL));
    t.h_inputs.addhi(here & t.SL & fn);
    EVAL t.h_dirs.put(here, here);
    TRY
      M3Driver.AddSourceFile(here, fn, TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;
  END HSource;

PROCEDURE DoHSource(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    HSource(t, QVal.ToText(t, arg));
  END DoHSource;

PROCEDURE CSource(t: T; x: TEXT) RAISES {Error}=
  VAR fn := x & ".c"; here := PathOf(t, "");
  BEGIN
    EVAL t.c_sources.put(M3ID.Add(fn), NEW(M3Libs.T, 
                                           loc := Location(t, t.package,
                                                           PkgSubdir(t)),
                                           hidden := HIDDEN,
                                           local := LOCAL));
    EVAL t.c_inputs.put(M3ID.Add(x), PathOf(t, fn));
    EVAL t.h_dirs.put(here, here);
    TRY
      M3Driver.AddSourceFile(NIL, PathOf(t, fn), TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;
    PgmObject(t, x, t.OBJ_ext);
    Deriveds(t, x, t.c_extensions);
  END CSource;

PROCEDURE DoCSource(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    CSource(t, QVal.ToText(t, arg));
  END DoCSource;

PROCEDURE SSource(t: T; x: TEXT) RAISES {Error}=
  VAR fn := x & ".s";
  BEGIN
    EVAL t.s_sources.put(M3ID.Add(fn), NEW(M3Libs.T, 
                                          loc := Location(t, t.package, 
                                                          PkgSubdir(t)),
                                          hidden := HIDDEN,
                                          local := LOCAL));
    TRY
      M3Driver.AddSourceFile(NIL, PathOf(t, fn), TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;
    PgmObject(t, x, t.OBJ_ext);
    Deriveds(t, x,t. s_extensions);
  END SSource;


PROCEDURE DoSSource(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    SSource(t, QVal.ToText(t, arg));
  END DoSSource;

(*------------------------------------------------------------- generics ----*)

PROCEDURE GenericIntf(t: T; x: TEXT; vis: BOOLEAN) RAISES {Error}=
  BEGIN
    EVAL t.generic_interface_sources.put(M3ID.Add(x & ".ig"),
                                         NEW(M3Libs.T, 
                                             loc := Location(t, t.package, 
                                                             PkgSubdir(t)),
                                             hidden := vis,
                                             local := LOCAL));
    TRY
      M3Driver.AddSourceFile(NIL, PathOf(t, x & ".ig") , cmd_line := TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;
  END GenericIntf;

PROCEDURE GenericImpl(t: T; x: TEXT; vis: BOOLEAN) RAISES {Error}=
  BEGIN
    EVAL t.generic_module_sources.put(M3ID.Add(x & ".mg"),
                                         NEW(M3Libs.T, 
                                             loc := Location(t, t.package, 
                                                             PkgSubdir(t)),
                                             hidden := vis,
                                             local := LOCAL));
    TRY
      M3Driver.AddSourceFile(NIL, PathOf(t, x & ".mg"), cmd_line := TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;      
  END GenericImpl;

PROCEDURE DoGenericInterface(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    GenericIntf(t, QVal.ToText(t, arg), HIDDEN);
  END DoGenericInterface;

PROCEDURE DoGENERIC_INTERFACE(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    GenericIntf(t, QVal.ToText(t, arg), VISIBLE);
  END DoGENERIC_INTERFACE;

PROCEDURE DoGenericImplementation(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    GenericImpl(t, QVal.ToText(t, arg), HIDDEN);
  END DoGenericImplementation;

PROCEDURE DoGENERIC_IMPLEMENTATION(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    GenericImpl(t, QVal.ToText(t, arg), VISIBLE);
  END DoGENERIC_IMPLEMENTATION;


PROCEDURE DoGenericModule(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    arg: QValue.T; x: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    GenericIntf(t, x, HIDDEN);
    GenericImpl(t, x, HIDDEN);
  END DoGenericModule;

PROCEDURE DoGENERIC_MODULE(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    arg: QValue.T; x: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    GenericIntf(t, x, VISIBLE);
    GenericImpl(t, x, VISIBLE);
  END DoGENERIC_MODULE;

PROCEDURE BuildGenericIntf(t: T; nm: TEXT; generic: TEXT; args: QVSeq.T; 
                           vis: BOOLEAN; 
                           unsafe: BOOLEAN := FALSE) RAISES {Error}=
  VAR
    file := nm & ".i3";
    tmp  := ".generic.tmp";
    sep  := "";
    wr   :  Wr.T;
  BEGIN
    IF t.all THEN
      TRY
        wr := Utils.OpenWriter(tmp, TRUE);
        Wr.PutText(wr, "(*generated by m3build*)" & t.CR & t.CR);
        IF unsafe THEN
          Wr.PutText(wr, "UNSAFE ");
        END;
        Wr.PutText(wr, "INTERFACE " & nm & " = " & generic & " (");
        FOR i := 0 TO args.size() - 1 DO
          Wr.PutText(wr, sep & QVal.ToText(t, args.get(i)));
          sep := ", ";
        END;
        Wr.PutText(wr, ") END " & nm & "." & t.CR);
        Utils.CloseWriter(wr, tmp);
      EXCEPT
        M3Driver.Error, Thread.Alerted, Wr.Failure => FErr(tmp);
      END;
      CopyIfNew(t, tmp, file);
      BldHooks.DeleteFile(t, tmp);
    END;
    DerivedInterface(t, nm, vis);
  END BuildGenericIntf;

PROCEDURE DoBuildGenericIntf(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR vis, args, generic, nm: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(vis);
    t.pop(args);
    t.pop(generic);
    t.pop(nm);
    BuildGenericIntf(t, QVal.ToText(t, nm), QVal.ToText(t, generic), QVal.ToArray(t, args), QVal.ToBool(t, vis));
  END DoBuildGenericIntf;

PROCEDURE DoBuildGenericUnsafeIntf(t: QMachine.T; 
                                   n_args: INTEGER) RAISES {Error}=
  VAR vis, args, generic, nm: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(vis);
    t.pop(args);
    t.pop(generic);
    t.pop(nm);
    BuildGenericIntf(t, QVal.ToText(t, nm), QVal.ToText(t, generic), QVal.ToArray(t, args), QVal.ToBool(t, vis), TRUE);
  END DoBuildGenericUnsafeIntf;

PROCEDURE BuildGenericImpl(t: T; nm: TEXT; generic: TEXT; 
                           args: QVSeq.T;
                           unsafe : BOOLEAN := FALSE) RAISES {Error}=
  VAR
    file := nm & ".m3";
    tmp  := ".generic.tmp";
    sep  := "";
    wr   :  Wr.T;
  BEGIN
    IF t.all THEN
      TRY
        wr := Utils.OpenWriter(tmp, TRUE);
        Wr.PutText(wr, "(*generated by m3build*)" & t.CR & t.CR);
        IF unsafe THEN
          Wr.PutText(wr, "UNSAFE ");
        END;
        Wr.PutText(wr, "MODULE " & nm & " = " & generic & " (");
        FOR i := 0 TO args.size() - 1 DO
          Wr.PutText(wr, sep & QVal.ToText(t, args.get(i)));
          sep := ", ";
        END;
        Wr.PutText(wr, ") END " & nm & "." & t.CR);
        Utils.CloseWriter(wr, tmp);
      EXCEPT
        M3Driver.Error, Thread.Alerted, Wr.Failure => FErr(tmp);
      END;
      CopyIfNew(t, tmp, file);
      BldHooks.DeleteFile(t, tmp);
    END;
    DerivedImplementation(t, nm);
  END BuildGenericImpl;

PROCEDURE DoBuildGenericImpl(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR args, generic, nm: QValue.T;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(args);
    t.pop(generic);
    t.pop(nm);
    BuildGenericImpl(t, QVal.ToText(t, nm), QVal.ToText(t, generic), 
                     QVal.ToArray(t, args));
  END DoBuildGenericImpl;

PROCEDURE DoBuildGenericUnsafeImpl(t: QMachine.T;
                                   n_args: INTEGER) RAISES {Error}=
  VAR args, generic, nm: QValue.T;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(args);
    t.pop(generic);
    t.pop(nm);
    BuildGenericImpl(t, QVal.ToText(t, nm), QVal.ToText(t, generic), 
                     QVal.ToArray(t, args), TRUE);
  END DoBuildGenericUnsafeImpl;

(*------------------------------------------------------- derived sources ---*)

PROCEDURE DerivedInterface(t: T; x: TEXT; hidden: BOOLEAN) RAISES {Error} =
  VAR im := x & ".i3";
  BEGIN
    EVAL t.interface_sources.put(M3ID.Add(im), 
                                 NEW(M3Libs.T, 
                                     loc := Location(t, t.build_package,
                                                     t.build_dir),
                                     hidden := hidden,
                                     local := TRUE));
    TRY
      M3Driver.AddSourceFile(NIL, im, TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;      
    EVAL t.derived_sources.put(im, im);
    PgmObject(t, x, t.IO_ext);
    Deriveds(t, x, t.intf_extensions);
    Deriveds(t, im, t.no_extension);
  END DerivedInterface;

PROCEDURE DoDerivedInterface(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR hidden, x: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(hidden);
    t.pop(x);
    DerivedInterface(t, QVal.ToText(t, x), QVal.ToBool(t, hidden));
  END DoDerivedInterface;

PROCEDURE DerivedImplementation(t: T; x: TEXT) RAISES {Error}=
  VAR mm := x & ".m3";
  BEGIN
    EVAL t.module_sources.put(M3ID.Add(mm),
                              NEW(M3Libs.T, 
                                  loc := Location(t, t.build_package, 
                                                  t.build_dir),
                                  hidden := TRUE,
                                  local := TRUE));
    TRY
      M3Driver.AddSourceFile(NIL, mm, TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;      
    EVAL t.derived_sources.put(mm, mm);
    PgmObject(t, x, t.MO_ext);
    Deriveds(t, x, t.impl_extensions);
    Deriveds(t, mm, t.no_extension);
  END DerivedImplementation;

PROCEDURE DoDerivedImplementation(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    DerivedImplementation(t, QVal.ToText(t, x));
  END DoDerivedImplementation;

PROCEDURE DerivedC(t: T; x: TEXT) RAISES {Error}=
  VAR fn := x & ".c";
  BEGIN
    EVAL t.c_sources.put(M3ID.Add(fn),
                         NEW(M3Libs.T, loc := Location(t, t.build_package, 
                                                       t.build_dir),
                             hidden := TRUE,
                             local := TRUE));
    EVAL t.c_inputs.put(M3ID.Add(x), fn);
    EVAL t.h_dirs.put(".", ".");
    TRY
      M3Driver.AddSourceFile(NIL, fn, TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;      
    EVAL t.derived_sources.put(fn, fn);
    PgmObject(t, x, t.OBJ_ext);
    Deriveds(t, x, t.c_extensions);
    Deriveds(t, fn, t.no_extension);
  END DerivedC;

PROCEDURE DoDerivedC(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    DerivedC(t, QVal.ToText(t, x));
  END DoDerivedC;


PROCEDURE DerivedH(t: T; x: TEXT) RAISES {Error} =
  VAR fn := x & ".h";
  BEGIN
    EVAL t.h_sources.put(M3ID.Add(fn),
                         NEW(M3Libs.T, loc := Location(t, t.build_package, 
                                                       t.build_dir),
                             hidden := TRUE,
                             local := TRUE));
    t.h_inputs.addhi(fn);
    EVAL t.h_dirs.put(".", ".");
    TRY
      M3Driver.AddSourceFile(NIL, fn, TRUE);
    EXCEPT
      M3Driver.Error => CErr("M3Driver.AddSourceFile");
    END;      
    EVAL t.derived_sources.put(fn, fn);
    Deriveds(t, fn, t.no_extension);
  END DerivedH;

PROCEDURE DoDerivedH(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    DerivedH(t, QVal.ToText(t, x));
  END DoDerivedH;



(*------------------------------------------------------------ "-T" files ---*)
(* compiler -Tfile generation. *)

PROCEDURE GenerateTFile(t: T) RAISES {Error} =
  PROCEDURE WalkUnits(s: IntM3LibsTbl.T)=
    VAR 
      iterator := s.iterate();
      val: REFANY;
      u: M3Libs.T;
      m: INTEGER;
      new: List;
    BEGIN
      WHILE iterator.next(m, u) DO
        IF NOT u.hidden OR u.local THEN
          val := NIL;
          EVAL t_dirs.get(u.loc, val);
          new := NEW(List, name := M3ID.ToText(m), next := val);
          EVAL t_dirs.put(u.loc, new);
        END;
      END;
    END WalkUnits;

  TYPE
    List = REF RECORD
      name: TEXT;
      next: List;
    END;
  VAR
    t_dirs := NEW(TextRefTbl.Default).init();
    wr: Wr.T;
    iterator: TextRefTbl.Iterator;
    units: List;
    ref: REFANY;
    d: TEXT;
    val: QValue.T;
  BEGIN
    WalkUnits(t.interface_sources);
    WalkUnits(t.generic_interface_sources);
    WalkUnits(t.generic_module_sources);

    TRY
      wr := Utils.OpenWriter(M3TFILE, TRUE);
      iterator := t_dirs.iterate();
      WHILE iterator.next(d, ref) DO
        units := NARROW(ref, List);
        Wr.PutText(wr, "@" & d & t.CR);
        WHILE units # NIL DO 
          Wr.PutText(wr, units.name & t.CR);
          units := units.next;
        END;
      END;
      Utils.CloseWriter(wr, M3TFILE);
    EXCEPT
      M3Driver.Error, Thread.Alerted, Wr.Failure => FErr(M3TFILE);
    END;

    IF t.all THEN
      InstallDerived(t, M3TFILE);
    END;
    Deriveds(t, M3TFILE, t.no_extension);
    TRY
      Unit.PushTable(M3TFILE);
    EXCEPT
      M3Driver.Error => CErr("Unit.PushTable");
    END;
    (* define M3SEARCH_TABLES *)
    val.kind := QValue.Kind.String;
    val.int  := M3ID.Add("-T" & M3TFILE);
    t.put(M3ID.Add("M3SEARCH_TABLES"), val);
  END GenerateTFile;

PROCEDURE DoGenerateTFile(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  BEGIN
    <* ASSERT n_args = 0 *>
    GenerateTFile(t);
  END DoGenerateTFile;

(*------------------------------------------------------------ .M3EXPORTS ---*)

TYPE
  GenType = {Pgm, Lib, Other};

PROCEDURE Visibility(v: BOOLEAN): TEXT = 
  BEGIN
    IF v THEN RETURN "hidden" ELSE RETURN "" END;
  END Visibility;

PROCEDURE GenM3Exports(t: T; x: TEXT; type: GenType) RAISES {Error}=

  PROCEDURE GenUnitMap(s: IntM3LibsTbl.T; txt: TEXT; wr: Wr.T) RAISES {Thread.Alerted, Wr.Failure} =
    VAR 
      iterator := s.iterate();
      e: INTEGER;
      u: M3Libs.T;
    BEGIN
      WHILE iterator.next(e, u) DO
        IF u.local THEN
          Wr.PutText(wr, "_map_add_" & txt & "(\"" & Escape(M3ID.ToText(e)) &
                         "\", \"" & Escape(LocPkg(t, u.loc)) &
                         "\", \"" & Escape(LocSubdir(t, u.loc)) &
                         "\", \"" & Visibility(u.hidden) & "\")" & t.CR);
        END;
      END;
    END GenUnitMap;
    
  VAR
    wr: Wr.T; 
    oviter := t.pkg_overrides.iterate();
    ov: INTEGER;
    ov_val: TEXT;
    impiter := t.imports.iterate();
    im: INTEGER;
    im_val: TEXT;
    libiter := t.m3libs.iterate();
    l: INTEGER;
    lib: M3Libs.T;
    u: M3Libs.T;
  BEGIN
    TRY
      wr := Utils.OpenWriter(M3EXPORTS, TRUE);
      Wr.PutText(wr, "% exports of " & t.build_package & t.CR);
      IF type = GenType.Lib THEN
        Wr.PutText(wr, "_define_lib(\"" & x & "\")" & t.CR);
      ELSIF type = GenType.Pgm THEN
        Wr.PutText(wr, "_define_pgm(\"" & x & "\")" & t.CR);
      END;

      (* output the foreign imports (in order!) *)
      FOR i := 0 TO t.other_libs_x.size() - 1 DO
        WITH l = t.other_libs_x.get(i) DO
          EVAL t.other_libs.get(M3ID.Add(l), u);
          IF u.local THEN
            Wr.PutText(wr, "_import_otherlib(\"" & Escape(l) & "\", \"" & 
              Escape(u.loc) & "\", \"IMPORTED\")" & t.CR);
          END;
        END;
      END;

      (*  copy forward overrides *)
      WHILE oviter.next(ov, ov_val) DO
        Wr.PutText(wr, "override(\"" & Escape(M3ID.ToText(ov)) & "\", \"" & 
          Escape(ov_val) & "\")" & t.CR);
      END;
      
      (* copy forward package imports *)
      WHILE impiter.next(im, im_val) DO
        Wr.PutText(wr, "import_version(\"" & Escape(M3ID.ToText(im)) & "\", \"" &
          Escape(im_val) & "\")" & t.CR);
      END;

      (* output the library imports *)
      WHILE libiter.next(l, lib) DO
        IF lib.local THEN
          Wr.PutText(wr, "_import_m3lib(\"" & Escape(M3ID.ToText(l)) & 
            "\", \"" & Escape(LocPkg(t, lib.loc)) & "\", \"" &
            Escape(LocSubdir(t, lib.loc)) & "\")" & t.CR);
        END;
      END;

      (* output the unit map *)
      GenUnitMap(t.interface_sources, "interface", wr);
      GenUnitMap(t.generic_interface_sources, "generic_interface", wr);
      GenUnitMap(t.generic_module_sources, "generic_module", wr);
      GenUnitMap(t.module_sources, "module", wr);
      GenUnitMap(t.c_sources, "c", wr);
      GenUnitMap(t.h_sources, "h", wr);
      GenUnitMap(t.s_sources, "s", wr);

      Wr.PutText(wr, Escape(M3Buf.ToText(t.tfile_args)));

      (* dump any 'custom' calls; these must come after any _import_template() *)
      (* calls, which may introduce the definitions of the custom calls *)
      BldHooks.GenMapHooks(t);
    
      IF t.all THEN
        InstallDerived(t, M3EXPORTS)
      END;
      Deriveds(t, M3EXPORTS, t.no_extension);
      Utils.CloseWriter(wr, M3EXPORTS);
    EXCEPT
      M3Driver.Error, Thread.Alerted, Wr.Failure => FErr(M3EXPORTS);
    END;
  END GenM3Exports;

PROCEDURE DoGenM3Exports(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    GenM3Exports(t, QVal.ToText(t, x), GenType.Other);
  END DoGenM3Exports;

(*------------------------------------------------------ program building ---*)

PROCEDURE U_Program(t: T; x: TEXT) RAISES {Error}=
  TYPE
    Exts2 = ARRAY [1..2] OF TEXT;
    Exts5 = ARRAY [1..5] OF TEXT;
  BEGIN
    GenM3Exports(t, x, GenType.Pgm);
    IF t.all THEN
      t.no_m3main := FALSE;
      IF NOT Text.Equal(x, "") THEN
        t.pgm_name := x;
      ELSE
        t.pgm_name := NIL;
      END;
      M3(t);
      InstallSources(t);
    END;
    Deriveds(t, x, Exts2{ t.PGM_ext, ".m3x" });
    Deriveds(t, "", 
             Exts5{ "_m3main.c", "_m3main.o", "_m3main.obj", M3WEB, M3TFILE });
  END U_Program;

PROCEDURE DoProgram(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T; x: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg); x := QVal.ToText(t, arg);
    U_Program(t, x);
    IF NARROW(t,T).all THEN
      InstallDerived(t, ProgramName(t, x));
    END;
  END DoProgram;

PROCEDURE DoPROGRAM(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T; x: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg); x := QVal.ToText(t, arg);
    U_Program(t, x);
    BinExport(t, ProgramName(t, x), TRUE);
  END DoPROGRAM;

PROCEDURE CProgram(t: T; x: TEXT) RAISES {Error}=
  TYPE
    Exts2 = ARRAY [1..2] OF TEXT;
    Exts1 = ARRAY [1..1] OF TEXT;
  BEGIN
    GenM3Exports(t, x, GenType.Pgm);
    IF t.all THEN
      t.no_m3main := TRUE;
      IF NOT Text.Equal(x, "") THEN
        t.pgm_name := x;
      ELSE
        t.pgm_name := NIL;
      END;
      M3(t);
      InstallSources(t);
    END;
    Deriveds(t, x, Exts2{ t.PGM_ext, ".m3x" });
    Deriveds(t, "",Exts1{ M3TFILE });
  END CProgram;

PROCEDURE DoCProgram(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    CProgram(t, QVal.ToText(t, arg));
  END DoCProgram;

PROCEDURE DoCPROGRAM(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T; x: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg); x := QVal.ToText(t, arg);
    CProgram(t, x);
    BinExport(t, ProgramName(t, x), TRUE);
  END DoCPROGRAM;

(*------------------------------------------------------ library building ---*)

PROCEDURE Library(t: T; x: TEXT) RAISES {Error}=
  TYPE
    Exts = ARRAY [0..3] OF TEXT;
  VAR 
    lib := t.LIB_pre & x & t.LIB_ext;
    libmx := t.LIB_pre & x & ".m3x";
  BEGIN
    EVAL t.m3libs.put(M3ID.Add(x), NEW(M3Libs.T, 
                              loc := Location(t, t.package, t.build_dir), 
                              hidden := HIDDEN, local := LOCAL));
    GenM3Exports(t, x, GenType.Lib);
    IF t.all THEN
      t.lib_name := lib;
      M3(t);
      InstallDerived(t, lib);
      InstallDerived(t, libmx);
      InstallSources(t);
    END;
    Deriveds(t, "", Exts{lib, libmx, M3WEB, M3TFILE});
    EVAL BldHooks.NoteShlib(t, x);
  END Library;

PROCEDURE DoLibrary(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    Library(t, QVal.ToText(t, arg));
  END DoLibrary;

PROCEDURE DoLIBRARY(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    Library(t, QVal.ToText(t, arg));
  END DoLIBRARY;

PROCEDURE InstallDerived(t: T; x: TEXT) RAISES {Error} =
  VAR 
    wr: Wr.T;
    dest := t.PKG_INSTALL & t.SL & t.build_package & t.SL & t.build_dir;
    val: QValue.T;
  BEGIN
    IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) THEN
      IF QVal.ToBool(t, val) THEN RETURN END;
    END;

    TRY
      wr := FileWr.OpenAppend(M3SHIP_FILE);

      U_InstallDir(t, dest, wr);

      Wr.PutText(wr, "install_file(\"" & Escape(x) & "\", \"" & Escape(dest) &
        "\", \"0644\")" & t.CR);
      Utils.CloseWriter(wr, M3SHIP_FILE);
    EXCEPT
      M3Driver.Error, OSError.E, Thread.Alerted, 
      Wr.Failure => FErr(M3SHIP_FILE);
    END;
  END InstallDerived;

PROCEDURE DoInstallDerived(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    InstallDerived(t, QVal.ToText(t, arg));
  END DoInstallDerived;

PROCEDURE InstallLink(t: T; src, target, dest: TEXT) RAISES {Error}=
  VAR
    val    : QValue.T;
    link   : TEXT;
    wr     : Wr.T;
  BEGIN
    IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) AND QVal.ToBool(t, val) THEN
      InstallFile(t, src, dest, "0755", TRUE);
    ELSE
      target := target & t.SL & src;
      link := dest & t.SL & src;

      TRY
        wr := FileWr.OpenAppend(M3SHIP_FILE);
    
        U_InstallDir(t, dest, wr);
        Wr.PutText(wr, "link_file(\""& Escape(target) & "\", \"" & Escape(link) & "\")" & t.CR);
        Utils.CloseWriter(wr, M3SHIP_FILE);
      EXCEPT 
        M3Driver.Error, OSError.E, Thread.Alerted, 
        Wr.Failure => FErr(M3SHIP_FILE);
      END;
    END;
  END InstallLink;

PROCEDURE DoInstallLink(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR src, target, dest: QValue.T;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(dest);
    t.pop(target);
    t.pop(src);
    InstallLink(t, QVal.ToText(t, src), QVal.ToText(t, target),
        QVal.ToText(t, dest));
  END DoInstallLink;

PROCEDURE InstallAliasLink(t: T; src, target, alias, dest: TEXT) RAISES {Error}=
  VAR
    val    : QValue.T;
    link   : TEXT;
    wr     : Wr.T;
  BEGIN
    IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) AND QVal.ToBool(t, val) THEN
      RAISE Error("install_alias_link not implemented for pkgtools");
    ELSE
      target := target & t.SL & src;
      link := dest & t.SL & alias;

      TRY
        wr := FileWr.OpenAppend(M3SHIP_FILE);
    
        U_InstallDir(t, dest, wr);
        Wr.PutText(wr, "link_file(\""& Escape(target) & "\", \"" & Escape(link) & "\")" & t.CR);
        Utils.CloseWriter(wr, M3SHIP_FILE);
      EXCEPT 
        M3Driver.Error, OSError.E, Thread.Alerted, 
        Wr.Failure => FErr(M3SHIP_FILE);
      END;
    END;
  END InstallAliasLink;

PROCEDURE DoInstallAliasLink(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR src, target, alias, dest: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(dest);
    t.pop(alias);
    t.pop(target);
    t.pop(src);
    InstallAliasLink(t, QVal.ToText(t, src), QVal.ToText(t, target),
        QVal.ToText(t, alias), QVal.ToText(t, dest));
  END DoInstallAliasLink;


(*--------------------------------------------------- exported interfaces ---*)
(* installation of exported interfaces & implementations *)

PROCEDURE InstallSources(t: T) RAISES {Error}=
  PROCEDURE WalkUnits(s: IntM3LibsTbl.T)=
    VAR 
      iterator := s.iterate();
      m: INTEGER;
      u: M3Libs.T;
      val: REFANY;
      new: List;
    BEGIN
      WHILE iterator.next(m, u) DO
        IF u.local THEN
          val := NIL;
          EVAL install_dirs.get(u.loc, val);
          new := NEW(List, name := M3ID.ToText(m), next := val);
          EVAL install_dirs.put(u.loc, new);
        END;
      END;
    END WalkUnits;
  TYPE
    List = REF RECORD
      name: TEXT;
      next: List;
    END;
  VAR
    val: QValue.T;
    install_dirs := NEW(TextRefTbl.Default).init();
    tab: REF ARRAY OF IntM3LibsTbl.T;
    wr: Wr.T;
    iterator : TextRefTbl.Iterator;
    list: List;
    ref: REFANY;
    d: TEXT;
    dest: TEXT;
  BEGIN
    IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) THEN
      IF QVal.ToBool(t, val) THEN RETURN END;
    END;

    WalkUnits(t.interface_sources);
    WalkUnits(t.generic_interface_sources);
    WalkUnits(t.generic_module_sources);
    WalkUnits(t.h_sources);
    WalkUnits(t.templates);
    IF t.get(M3ID.Add("INSTALL_IMPLS"), val) AND QVal.ToBool(t, val) THEN
      WalkUnits(t.module_sources);
      WalkUnits(t.c_sources);
      WalkUnits(t.s_sources);
    END;

    tab := BldHooks.InstallUnitsHooks(t, QVal.ToBool(t, val));
    IF tab # NIL THEN
      FOR i := FIRST(tab^) TO LAST(tab^) DO
        WalkUnits(tab[i]);
      END;
    END;

    TRY
      wr := FileWr.OpenAppend(M3SHIP_FILE);
    
      iterator := install_dirs.iterate();
      WHILE iterator.next(d, ref) DO
        list := NARROW(ref, List);
        dest := t.PKG_INSTALL & t.SL & LocPkg(t, d) & t.SL & LocSubdir(t, d);
        U_InstallDir(t, dest, wr);
        WHILE list # NIL DO
          Wr.PutText(wr, "install_file(\"" & Escape(d & t.SL & list.name) &
            "\", \"" & Escape(dest) & "\", \"0644\")" & t.CR);
          list := list.next;
        END;
      END;
      Utils.CloseWriter(wr, M3SHIP_FILE);
    EXCEPT 
      M3Driver.Error, OSError.E, Thread.Alerted, 
        Wr.Failure => FErr(M3SHIP_FILE);
    END;
  END InstallSources;

PROCEDURE DoInstallSources(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  BEGIN
    <* ASSERT n_args = 0 *>
    InstallSources(t);
  END DoInstallSources;


(*--------------------------------------------- internal export utilities ---*)

PROCEDURE U_InstallDir(t: T; dest: TEXT; wr: Wr.T) RAISES {Error}=
  VAR val: QValue.T; dest_val: REFANY;
  BEGIN
    IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) AND QVal.ToBool(t, val) THEN 
      IF NOT Text.Equal(dest, t.last_ship_dir) THEN
        t.last_ship_dir := dest;
        Wr.PutText(wr, "-l " & dest & t.CRship); <* NOWARN *>
      END;
    ELSE
      IF NOT t.all_ship_dirs.get(dest, dest_val) THEN
        EVAL t.all_ship_dirs.put(dest, NIL);
        Wr.PutText(wr, "make_dir(\""& Escape(dest) & "\")" & t.CR); <* NOWARN *>
      END
    END;
  END U_InstallDir;

(*-------------------------------------------------------------------- M3 ---*)

PROCEDURE DoM3Option(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    NARROW(t, T).m3_options.addhi(QVal.ToText(t, arg));
  END DoM3Option;

PROCEDURE DoRemoveM3Option(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    arg  : QValue.T;
    x    : TEXT;
    opt  : TEXT;
    opts : TextSeq.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg); x := QVal.ToText(t, arg);
    WITH t = NARROW(t, T) DO
      opts := NEW(TextSeq.T).init(MAX(1,t.m3_options.size() - 1));
      FOR i := 0 TO t.m3_options.size() - 1 DO
        opt := t.m3_options.get(i);
        IF NOT Text.Equal(x, opt) THEN
          opts.addhi(opt) END;
      END;
      t.m3_options := opts;
    END;
  END DoRemoveM3Option;

PROCEDURE DoOverride(t: QMachine.T; n_args: INTEGER) RAISES {Error} = 
  VAR
    p_val: QValue.T;
    dir_val: QValue.T;
    p: TEXT;
    dir: Pathname.T;
    id: M3ID.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    WITH t = NARROW(t, T) DO
      t.pop(dir_val);
      t.pop(p_val);
      dir := QVal.ToText(t, dir_val);
      p   := QVal.ToText(t, p_val);
      IF Text.Equal(p, t.package) THEN
        IF NOT t.warned THEN
          <* NOWARN *> Wr.PutText(t.cur_wr(), "m3build: ignoring override(\"" & p & 
          "\", \"" & dir & "\")" & t.CR);
          t.warned := TRUE;
        END;
      ELSE
        id := M3ID.Add(p);
        EVAL t.pkg_overrides.put(id, dir);
        EVAL t.pkg_cache.put(id, dir & t.SL & p);
      END;
    END;
  END DoOverride;

PROCEDURE DoResetCache (<* UNUSED *> t: QMachine.T; n_args: INTEGER) =
  BEGIN
    <*ASSERT n_args = 0 *>
    M3Driver.ResetASTCache();
  END DoResetCache;

PROCEDURE M3 (t: T) RAISES {Error} =
  VAR
    arg          : QValue.T;
    wr           : Wr.T;
    pkg          : INTEGER;
    ov           : TEXT;
    iterator     : IntTextTbl.Iterator;
    interface    : BldFace.T;
    name         : M3Path.T;
    lib          : M3Libs.T;
  BEGIN
    BldHooks.BeforeDoM3Hooks(t);
    GenerateTFile(t);
    
    IF t.get(M3ID.Add("RESET_CACHE"), arg) THEN
      M3Driver.ResetASTCache();
    END;

    (* join the m3 libraries and the other libraries *)
    name.dir := NIL;
    name.kind := NK.A;
    FOR i := 0 TO t.m3libs_x.size() - 1 DO
      name.base := t.m3libs_x.get(i);
      IF t.m3libs.get(M3ID.Add(name.base), lib) THEN
        M3Driver.PushPath(lib.loc);
        M3Driver.AddLibrary(NIL, name);
      ELSE
        RAISE Error("m3libs_x and m3libs are incoherent!");
      END;
    END;
    FOR i := 0 TO t.other_libs_x.size() - 1 DO
      name.base := t.other_libs_x.get(i);
      IF t.other_libs.get(M3ID.Add(name.base), lib) THEN
        M3Driver.PushPath(lib.loc);
        M3Driver.AddLibrary(NIL, name);
      ELSE
        RAISE Error("other_libs_x and other_libs are incoherent!");
      END;
    END;

    (* call the m3driver now integrated *)
    TRY
      interface := NEW(BldFace.T).init(t);
      M3Driver.Compile(interface);
    EXCEPT
    | BldFace.Error => RAISE Error("couldn't initialize interface with the compiler!");
    | M3Driver.M3Error => RAISE Error(NIL);
    END;

    IF NOT t.no_m3main THEN InstallDerived(t, M3WEB); END;

    iterator := t.pkg_overrides.iterate();
    IF iterator.next(pkg, ov) THEN
      BldHooks.DeleteFile(t, M3SHIP_FILE);
      TRY
        wr := Utils.OpenWriter(M3OVERRIDES, TRUE);
        Wr.PutText(wr, t.CR);
      EXCEPT
        M3Driver.Error, Thread.Alerted, Wr.Failure => FErr(M3OVERRIDES);
      END;
    END;
  END M3;

PROCEDURE DoM3 (t: QMachine.T;  n_args: INTEGER) RAISES {Error} =
  BEGIN
    <* ASSERT n_args = 0 *>
    M3(t);
  END DoM3;

(*--------------------------------------------------------------- m3front ---*)
 
PROCEDURE DoM3FrontOption(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    NARROW(t, T).m3front_options.addhi(QVal.ToText(t, arg));
  END DoM3FrontOption;

PROCEDURE DoRemoveM3FrontOption(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR 
    arg  : QValue.T;
    x    : TEXT;
    opt  : TEXT;
    opts : TextSeq.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg); x := QVal.ToText(t, arg);
    WITH t = NARROW(t, T) DO
      opts := NEW(TextSeq.T).init(MAX(1,t.m3front_options.size() - 1));
      FOR i := 0 TO t.m3front_options.size() - 1 DO
        opt := t.m3front_options.get(i);
        IF NOT Text.Equal(x, opt) THEN
          opts.addhi(opt) END;
      END;
      t.m3front_options := opts;
    END;
  END DoRemoveM3FrontOption;

(*------------------------------------------------------ hiding/exporting ---*)
(* These are forwarded in the exports file *)

PROCEDURE SetVisibility(m: T; t: IntM3LibsTbl.T; n: TEXT; hidden: BOOLEAN) RAISES {Error} =
  VAR lib: M3Libs.T;
  BEGIN
    IF t.get(M3ID.Add(n), lib) THEN
      lib.hidden := hidden;
    ELSE
      Err("set_visibility(" & n & ") of unknown unit" & m.CR);
    END;
  END SetVisibility;

PROCEDURE DoHideInterface(m: QMachine.T;  n_args: INTEGER) RAISES {Error} =
  VAR 
    arg : QValue.T;
    t   := NARROW(m, T); 
    x   : TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    SetVisibility(t, t.interface_sources, x & ".i3", HIDDEN);
    M3Buf.PutText(t.tfile_args, "hide_interface(\"" & x & t.QRPCR);    
  END DoHideInterface;

PROCEDURE DoHideGenericInterface(m: QMachine.T; 
                                 n_args: INTEGER) RAISES {Error}=
  VAR 
    arg : QValue.T; 
    t   := NARROW(m, T);
    x   : TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    SetVisibility(t, t.generic_interface_sources, x & ".ig", HIDDEN);
    M3Buf.PutText(t.tfile_args, "hide_generic_interface(\"" & x & t.QRPCR);    
  END DoHideGenericInterface;

PROCEDURE DoHideGenericImplementation(m: QMachine.T; 
                                      n_args: INTEGER) RAISES {Error}=
  VAR
    arg : QValue.T;
    t   := NARROW(m, T);
    x   : TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    SetVisibility(t, t.generic_module_sources, x & ".mg", HIDDEN);
    M3Buf.PutText(t.tfile_args, "hide_generic_implementation(\"" & x & t.QRPCR);    
  END DoHideGenericImplementation;

PROCEDURE DoExportInterface(m: QMachine.T;  n_args: INTEGER) RAISES {Error} =
  VAR 
    arg : QValue.T; 
    t   := NARROW(m, T);
    x   : TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    SetVisibility(t, t.interface_sources, x & ".i3", VISIBLE);
    M3Buf.PutText(t.tfile_args, "export_interface(\"" & x & t.QRPCR);    
  END DoExportInterface;

PROCEDURE DoExportGenericInterface(m: QMachine.T; 
                                   n_args: INTEGER) RAISES {Error}=
  VAR
    arg : QValue.T;
    t   := NARROW(m, T);
    x   : TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    SetVisibility(t, t.generic_interface_sources, x & ".ig", VISIBLE);
    M3Buf.PutText(t.tfile_args, "export_generic_interface(\"" & x & t.QRPCR);    
  END DoExportGenericInterface;

PROCEDURE DoExportGenericImplementation(m: QMachine.T; 
                                        n_args: INTEGER) RAISES {Error}=
  VAR
    arg : QValue.T;
    t   := NARROW(m, T);
    x   : TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    SetVisibility(t, t.generic_module_sources, x & ".mg", VISIBLE);
    M3Buf.PutText(t.tfile_args, 
                  "export_generic_implementation(\"" & x & t.QRPCR);    
  END DoExportGenericImplementation;

(*------------------------------------------------------------- resources ---*)

PROCEDURE ResourceNamed(t: T; rd, x: TEXT) RAISES {Error}=
  BEGIN
    EVAL t.resources.put(rd, PathOf(t, x));
  END ResourceNamed;

PROCEDURE DoResourceNamed(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR rd, x: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(x);
    t.pop(rd);
    ResourceNamed(t, QVal.ToText(t, rd), QVal.ToText(t, x));
  END DoResourceNamed;

PROCEDURE DoResource(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T; x: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    x := QVal.ToText(t, arg);
    ResourceNamed(t, x, x);
  END DoResource;

PROCEDURE DerivedResource(t: T; x: TEXT) RAISES {Error} =
  TYPE Arr = ARRAY [0..0] OF TEXT;
  BEGIN
    EVAL t.resources.put(x, x);
    Deriveds(t, "", Arr{x});
  END DerivedResource;

PROCEDURE DoDerivedResource(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    DerivedResource(t, QVal.ToText(t, arg));
  END DoDerivedResource;

PROCEDURE Bundle(t: T; m: TEXT) RAISES {Error}=
  VAR
    intf     := m & ".i3";
    arglist  := Arg.NewList();
    anystale := FALSE;
    iterator := t.resources.iterate();
    r, p     : TEXT;
    intf_status : File.Status;
    p_status : File.Status;
  BEGIN
    (* The generated sources are in the build directory *)
    DerivedInterface(t, m, VISIBLE);
    DerivedImplementation(t, m);

    TRY
      intf_status := FS.Status(intf);
    EXCEPT OSError.E =>
      anystale := TRUE;
    END;

    IF t.all THEN
      TRY
        WHILE iterator.next(r, p) DO
          Arg.Append(arglist, "-element");
          Arg.Append(arglist, r);
          Arg.Append(arglist, p);
          IF NOT anystale THEN
            p_status := FS.Status(p);
            IF intf_status.modificationTime < p_status.modificationTime THEN 
              anystale := TRUE END;
          END;
        END;
        IF anystale THEN
          Arg.Append(arglist, "-name");
          Arg.Append(arglist, m);
          EVAL Utils.Execute(t.BIN_USE & t.SL & "m3bundle", arglist, NIL, TRUE);
        END;
        EVAL TextTextTbl.Default.init(t.resources);
      EXCEPT
      | OSError.E => FErr(p);
      | M3Driver.Error => CErr("Utils.Execute");
      END;
    END;
    Deriveds(t, m, t.rsrc_extensions);    
  END Bundle;

PROCEDURE DoBundle(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR m: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(m);
    Bundle(t, QVal.ToText(t, m));
  END DoBundle;

(*------------------------------------------------------------- templates ---*)

PROCEDURE Template(t: T; x: TEXT) RAISES {Error}=
  VAR
    fn      := x & ".tmpl";
    full_fn := Pathname.Prefix(M3ID.ToText(t.includes[t.reg.ip-1].file.source_file)) & t.SL & fn;
  BEGIN
    EVAL t.templates.put(M3ID.Add(fn), NEW(M3Libs.T, hidden := VISIBLE,
                                           loc := Location(t, t.package, 
                                                           PkgSubdir(t)),
                                           local := TRUE));

    M3Buf.PutText(t.tfile_args, "_import_template(\"" & fn & "\", \"" & 
      t.package & "\", \"" & PkgSubdir(t) & t.QRPCR);

    M3include(t, full_fn, fn, PkgSubdir(t), t.package);
  END Template;

PROCEDURE DoTemplate(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    Template(t, QVal.ToText(t,x));
  END DoTemplate;

(*------------------------------------------------------------- man pages ---*)

PROCEDURE manPage(t: T; x, sec: TEXT) RAISES {Error} =
  VAR
    fn       := x & "." & sec;
    cat_file := fn;
    man_file := PathOf(t, fn);
    val      : QValue.T;
  BEGIN
    IF t.get(M3ID.Add("MAN_SECTION"), val) THEN
      (* all Modula-3 man pages go in a single section *)
      cat_file := x & "." & QVal.ToText(t, val);
    END;
    
    IF t.all THEN
      IF Stale(cat_file, man_file) THEN
        CopyIfNew(t, man_file, cat_file) END;
    END;
      
    Deriveds(t, cat_file, t.no_extension);
  END manPage;

PROCEDURE DomanPage(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x, sec: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(sec);
    t.pop(x);
    manPage(t, QVal.ToText(t, x), QVal.ToText(t, sec));
  END DomanPage;

PROCEDURE ManPage(t: T; x, sec: TEXT) RAISES {Error}=
  BEGIN
    manPage(t, x, sec);
    ManExport(t, x, sec, TRUE);
  END ManPage;

PROCEDURE DoManPage(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x, sec: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(sec);
    t.pop(x);
    ManPage(t, QVal.ToText(t, x), QVal.ToText(t, sec));
  END DoManPage;

(*---------------------------------------------------------------- emacs ---*)

PROCEDURE DoGnuemacs(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    EmacsExport(t, QVal.ToText(t, x) & ".el", FALSE);
  END DoGnuemacs;
 
PROCEDURE DoCompiledGnuemacs(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR
    val      : QValue.T;
    x        : TEXT;
    src_file : TEXT;
    el_file  : TEXT;
    elc_file : TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(val);
    x := QVal.ToText(t, val);
    
    IF t.get(M3ID.Add("emacs_compile"), val) THEN
      src_file := PathOf(t, x & ".el");
      el_file := x & ".el";
      elc_file := x & ".elc";

      WITH t = NARROW(t, T) DO
        IF t.all THEN
          CopyIfNew(t, src_file, el_file);
          IF Stale(elc_file, el_file) THEN
          BldHooks.EmacsCompile(t, el_file);
          END;
        END;
        
        Deriveds(t, el_file, t.no_extension);
        Deriveds(t, elc_file, t.no_extension);
      END;
      EmacsExport(t, el_file, TRUE);
      EmacsExport(t, elc_file, TRUE);
    ELSE
      EmacsExport(t, x & ".el", FALSE);
    END;
  END DoCompiledGnuemacs;

(*----------------------------------------------------------------- noweb ---*)

PROCEDURE Noweb(t: T; src_file, root, dest_file: TEXT) RAISES {Error}=
  VAR
    tmp_dest := "." & dest_file;
    args     : Arg.List;
  BEGIN
    src_file := PathOf(t, src_file & ".nw");
    IF t.all THEN
      IF Stale(tmp_dest, src_file) THEN
        args := Arg.NewList();
        Arg.Append(args, "-L<*LINE %L \"%F\" *>%N");
        Arg.Append(args, "-R" & root);
        Arg.Append(args, src_file);
        TRY
          EVAL Utils.Execute("notangle", args, tmp_dest, TRUE);
        EXCEPT
          M3Driver.Error => CErr("Utils.Execute(notangle)");
        END;
      END;
      CopyIfNew(t, tmp_dest, dest_file);
    END;
    Deriveds(t, tmp_dest, t.no_extension);
    Deriveds(t, dest_file, t.no_extension);
  END Noweb;

PROCEDURE DoNoweb(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR src_file, root, dest_file: QValue.T;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(dest_file);
    t.pop(root);
    t.pop(src_file);
    Noweb(t, QVal.ToText(t, src_file), QVal.ToText(t, root), QVal.ToText(t, dest_file));
  END DoNoweb;

PROCEDURE DonowebInterface(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR src_file, root, dest_file: QValue.T;
      d: TEXT;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(dest_file);
    t.pop(root);
    t.pop(src_file);
    d := QVal.ToText(t, dest_file);
    Noweb(t, QVal.ToText(t, src_file), QVal.ToText(t, root), d & ".i3");
    DerivedInterface(t, d, HIDDEN);
  END DonowebInterface;

PROCEDURE DoNowebInterface(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR src_file, root, dest_file: QValue.T;
      d: TEXT;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(dest_file);
    t.pop(root);
    t.pop(src_file);
    d := QVal.ToText(t, dest_file);
    Noweb(t, QVal.ToText(t, src_file), QVal.ToText(t, root), d & ".i3");
    DerivedInterface(t, d, VISIBLE);
  END DoNowebInterface;

PROCEDURE DoNowebImplementation(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR src_file, root, dest_file: QValue.T;
      d: TEXT;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(dest_file);
    t.pop(root);
    t.pop(src_file);
    d := QVal.ToText(t, dest_file);
    Noweb(t, QVal.ToText(t, src_file), QVal.ToText(t, root), d & ".m3");
    DerivedImplementation(t, d);
  END DoNowebImplementation;

(*------------------------------------------------------------------ Zume ---*)

PROCEDURE DoZume(t: QMachine.T; n_args: INTEGER) RAISES {Error} =
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    <* NOWARN *> Wr.PutText(t.cur_wr(), "zume is not implemented" & 
      NARROW(t, T).CR);
  END DoZume;
 
(*--------------------------------------------------------------- cleanup ---*)

PROCEDURE DoRegisterCleanUp(t: QMachine.T; n_args: INTEGER) RAISES {Error} <* NOWARN *>=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    NARROW(t, T).cleanup_procs.addhi(QVal.ToText(t, arg));
  END DoRegisterCleanUp;

PROCEDURE DoRunCleanUps(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  BEGIN
    <* ASSERT n_args = 0 *>
    WITH t = NARROW(t, T) DO
      FOR i := 0 TO t.cleanup_procs.size() - 1 DO
        EVAL BldHooks.ExecHook(t, t.cleanup_procs.get(i), NIL);
      END;
    END;
  END DoRunCleanUps;

(*------------------------------------------------------- m3where support ---*)

PROCEDURE DoFindUnit(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  PROCEDURE GenUnit(s: IntM3LibsTbl.T): BOOLEAN=
    VAR u: M3Libs.T;
    BEGIN
      IF s.get(unit_id, u) THEN
        IF h OR NOT u.hidden THEN
          Wr.PutText(t.cur_wr(), u.loc & NARROW(t, T).SL & unit & NARROW(t, T).CR); <* NOWARN *>
          RETURN TRUE;
        END;
      END;
      RETURN FALSE;
    END GenUnit;
  VAR
    arg: QValue.T;
    unit: TEXT;
    unit_id: INTEGER;
    h: BOOLEAN;
    arr: REF ARRAY OF IntM3LibsTbl.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(arg); h := QVal.ToBool(t, arg);
    t.pop(arg); unit := QVal.ToText(t, arg);
    unit_id := QVal.ToID(t, arg);
    WITH t = NARROW(t, T) DO
      IF GenUnit(t.interface_sources)         THEN RETURN END;
      IF GenUnit(t.generic_interface_sources) THEN RETURN END;
      IF GenUnit(t.generic_module_sources)    THEN RETURN END;
      IF GenUnit(t.module_sources)            THEN RETURN END;
      IF GenUnit(t.c_sources)                 THEN RETURN END;
      IF GenUnit(t.h_sources)                 THEN RETURN END;
      IF GenUnit(t.s_sources)                 THEN RETURN END;
    
      arr := BldHooks.WhereUnitsHooks(t);
      IF (arr # NIL) THEN
        FOR i := FIRST(arr^) TO LAST(arr^) DO
          IF GenUnit(arr[i]) THEN RETURN END;
        END;
      END;

      Wr.PutText(t.cur_wr(), "\"" & unit & "\" not found" & t.CR); <* NOWARN *>
    END;
  END DoFindUnit;

PROCEDURE DoEnumUnits(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  PROCEDURE GenUnits(s: IntM3LibsTbl.T) =
    VAR iterator := s.iterate(); m: INTEGER; u: M3Libs.T;
    BEGIN
      WHILE iterator.next(m, u) DO
        IF h OR NOT u.hidden THEN
          Wr.PutText (t.cur_wr(), u.loc & NARROW(t, T).SL & M3ID.ToText(m) & NARROW(t, T).CR); <* NOWARN *>
        END;
      END;
    END GenUnits;
  VAR
    arg: QValue.T;
    h: BOOLEAN;
    arr: REF ARRAY OF IntM3LibsTbl.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg); h := QVal.ToBool(t, arg);
    WITH t = NARROW(t, T) DO
      GenUnits(t.interface_sources);
      GenUnits(t.generic_interface_sources);
      GenUnits(t.generic_module_sources);
      GenUnits(t.module_sources);
      GenUnits(t.h_sources);
      GenUnits(t.c_sources);
      GenUnits(t.s_sources);

      arr := BldHooks.WhereUnitsHooks(t);
      IF arr # NIL THEN
        FOR i := FIRST(arr^) TO LAST(arr^) DO
          GenUnits(arr[i]);
        END;
      END;
    END;
  END DoEnumUnits;

(*--------------------------------------------- internal export utilities ---*)

PROCEDURE NoteOverrides(t: T)RAISES {Error}=
  VAR wr: Wr.T;
  BEGIN
    BldHooks.DeleteFile(t, M3SHIP_FILE);
    TRY
      wr := Utils.OpenWriter(M3OVERRIDES, TRUE);
      Wr.PutText(wr, t.CR);
      Utils.CloseWriter(wr, M3OVERRIDES);
    EXCEPT
      M3Driver.Error, Thread.Alerted, Wr.Failure => FErr(M3OVERRIDES);
    END;
  END NoteOverrides;

PROCEDURE DoInstallFile(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR src, dest, mode, derived: QValue.T;
  BEGIN
    <* ASSERT n_args = 4 *>
    t.pop(derived);
    t.pop(mode);
    t.pop(dest);
    t.pop(src);
    InstallFile(t, QVal.ToText(t, src), QVal.ToText(t,dest),
        QVal.ToText(t,mode),QVal.ToBool(t,derived));
  END DoInstallFile;

PROCEDURE InstallFile(t: T; src, dest,  mode: TEXT; derived: BOOLEAN) RAISES {Error} =
  VAR 
    new_src  := PathOf(t, src);
    val      : QValue.T;
    iterator := t.pkg_overrides.iterate();
    k        : INTEGER;
    v        : TEXT;
    wr       : Wr.T;
  BEGIN
    IF NOT derived THEN
      IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) AND QVal.ToBool(t, val) THEN
        (* Since the package tools refuse to export things that are outside
           the current package and in general we don't know where a source
           file resides, we make links to any exported source files. *)
        IF t.all AND Stale(src, new_src) THEN
          BldHooks.LinkFile(t, new_src, src) END;
        Deriveds(t, src, t.no_extension);
        new_src := src;
      END;
      src := new_src;
    END;
    
    IF NOT t.all THEN RETURN END;

    IF iterator.next(k, v) THEN
      (* there were overrides => don't create any shipping information *)
      NoteOverrides(t);
      RETURN;
    END;

    TRY
      wr := FileWr.OpenAppend(M3SHIP_FILE);
      U_InstallDir(t, dest, wr);
      IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) AND QVal.ToBool(t, val) THEN
        Wr.PutText(wr, t.build_dir & t.SLship & src & t.CRship);
      ELSE
        Wr.PutText(wr, "install_file(\"" & Escape(src) & "\", \"" & Escape(dest)
        & "\", \"" & mode & "\")" & t.CR);
      END;
      Utils.CloseWriter(wr, M3SHIP_FILE);
    EXCEPT 
      M3Driver.Error, OSError.E, Thread.Alerted, 
        Wr.Failure => FErr (M3SHIP_FILE);
    END;

  END InstallFile;

PROCEDURE InstallMan(t: T; x, sec: TEXT; derived: BOOLEAN) RAISES { Error }=
  VAR
    src         := x & "." & sec;
    new_src     : TEXT;
    dest        : TEXT;
    val         : QValue.T;
    man_section : TEXT;
  BEGIN
    IF t.get(M3ID.Add("MAN_SECTION"), val) THEN
      man_section := QVal.ToText(t, val);
      dest := t.MAN_INSTALL & t.SL & "man" & man_section;
      IF NOT derived THEN src := PathOf(t, src) END;
      new_src := x & "." & man_section;
      CopyIfNew(t, src, new_src);
      src := new_src;
      derived := TRUE;
    ELSE
      IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) AND QVal.ToBool(t, val) THEN
        dest := t.MAN_INSTALL & t.SLship & "man" & sec;
      ELSE
        dest := t.MAN_INSTALL & t.SL & "man" & sec;
      END;
    END;
    InstallFile(t, src, dest, "0644", derived);
  END InstallMan;

PROCEDURE InstallSrc(t: T; src, dest, mode: TEXT) RAISES {Error}=
  VAR
    val      : QValue.T;
    k        : INTEGER;
    v        : TEXT;
    iterator := t.pkg_overrides.iterate();
    wr       : Wr.T;
  BEGIN
    IF NOT t.all THEN RETURN END;

    IF iterator.next(k, v) THEN
    (* there were overrides => don't create any shipping information *)
      NoteOverrides(t);
      RETURN;
    END;

    TRY
      wr := FileWr.OpenAppend(M3SHIP_FILE);

      U_InstallDir(t, dest, wr);
      
      IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) AND QVal.ToBool(t, val) THEN
        Wr.PutText(wr, src & t.CRship);
      ELSE
        Wr.PutText(wr, "install_file(\"" & Escape(src) & "\", \"" & Escape(dest) 
        & "\", \"" & mode & "\")" & t.CR);
      END;
      Utils.CloseWriter(wr, M3SHIP_FILE);
    EXCEPT 
      M3Driver.Error, OSError.E, Thread.Alerted, 
        Wr.Failure => FErr (M3SHIP_FILE);
    END;
  END InstallSrc;

VAR last_install_dir := "";

PROCEDURE DoNoteInstall(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR src, dest: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(dest);
    t.pop(src);
    NoteInstall(t, QVal.ToText(t, src), QVal.ToText(t, dest), t.cur_wr(), 
                "<STDOUT>");
  END DoNoteInstall;


PROCEDURE NoteInstall(t: T; src, dest: TEXT; wr: Wr.T; file: TEXT) RAISES {Error}=
  VAR val: QValue.T;
  BEGIN
    IF NOT t.get(M3ID.Add("_quiet"), val) THEN
      TRY
        IF NOT Text.Equal(last_install_dir, dest) THEN
          Wr.PutText(wr, dest & t.CR);
          last_install_dir := dest;
        END;
        Wr.PutText(wr, "   " & src & t.CR);
      EXCEPT
        Thread.Alerted, Wr.Failure => FErr(file);
      END;
    END;
  END NoteInstall;

(*----------------------------------------- user callable export routines ---*)

PROCEDURE BinExport(t: T; x: TEXT; derived: BOOLEAN) RAISES {Error}=
  BEGIN
    InstallFile(t, x, t.BIN_INSTALL, "0755", derived);
  END BinExport;

PROCEDURE DoBinExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    BinExport(t, QVal.ToText(t, x), FALSE);
  END DoBinExport;

PROCEDURE DoBindExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    BinExport(t, QVal.ToText(t, x), TRUE);
  END DoBindExport;

PROCEDURE LibExport(t: T; x: TEXT; derived: BOOLEAN) RAISES {Error}=
  BEGIN
    InstallFile(t, x, t.LIB_INSTALL, "0755", derived);
  END LibExport;

PROCEDURE DoLibExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    LibExport(t, QVal.ToText(t, x), FALSE);
  END DoLibExport;

PROCEDURE DoLibdExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    LibExport(t, QVal.ToText(t, x), TRUE);
  END DoLibdExport;

PROCEDURE EmacsExport(t: T; x: TEXT; derived: BOOLEAN) RAISES {Error}=
  BEGIN
    InstallFile(t, x, t.EMACS_INSTALL, "0644", derived);
  END EmacsExport;

PROCEDURE DoEmacsExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    EmacsExport(t, QVal.ToText(t, x), FALSE);
  END DoEmacsExport;

PROCEDURE DoEmacsdExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    EmacsExport(t, QVal.ToText(t, x), TRUE);
  END DoEmacsdExport;

PROCEDURE DocExport(t: T; x: TEXT; derived: BOOLEAN) RAISES {Error}=
  BEGIN
    InstallFile(t, x, t.DOC_INSTALL, "0644", derived);
  END DocExport;

PROCEDURE DoDocExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    DocExport(t, QVal.ToText(t, x), FALSE);
  END DoDocExport;

PROCEDURE DoDocdExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    DocExport(t, QVal.ToText(t, x), TRUE);
  END DoDocdExport;

PROCEDURE ManExport(t: T; x, s: TEXT; derived: BOOLEAN) RAISES {Error}=
  BEGIN
    InstallMan(t, x, s, derived);
  END ManExport;

PROCEDURE DoManExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x, s: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(s);
    t.pop(x);
    ManExport(t, QVal.ToText(t, x), QVal.ToText(t, s), FALSE);
  END DoManExport;

PROCEDURE DoMandExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x, s: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(s);
    t.pop(x);
    ManExport(t, QVal.ToText(t, x), QVal.ToText(t, s), TRUE);
  END DoMandExport;

PROCEDURE HtmlExport(t: T; x: TEXT) RAISES {Error}=
  BEGIN
    InstallSrc(t, x, t.HTML_INSTALL, "0644");
  END HtmlExport;

PROCEDURE DoHtmlExport(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(x);
    HtmlExport(t, QVal.ToText(t, x));
  END DoHtmlExport;

(*-------------------------------------------------------------- try_exec ---*)

PROCEDURE BuildOptionList(textOptions: TEXT): REF ARRAY OF TEXT =
  VAR options : TextList.T := NIL;
      length  := Text.Length(textOptions);
      args    : REF ARRAY OF TEXT;
      i, j, n : INTEGER:= 0;
  BEGIN
    textOptions := textOptions & " ";
    WHILE i <= length DO
      IF Text.GetChar(textOptions, i) = ' ' THEN
        IF i # j  THEN
          options := TextList.Cons (head := Text.Sub (textOptions, j, i-j),
			            tail := options);
          INC(n);
        END;
        j:= i + 1;
      END;
      INC (i);
    END;
    args := NEW (REF ARRAY OF TEXT, n);
    FOR i:= n-1 TO 0 BY -1 DO
      args^[i]:= options.head;
      options := options.tail;
    END;
    RETURN args;
  END BuildOptionList;

PROCEDURE DoTryExec (t: QMachine.T;  n_args: INTEGER) RAISES {Error} =
  VAR
    echo         := TRUE;
    first        := TRUE;
    command      : TEXT;
    cmd          : TEXT;
    n            : INTEGER;
    handle       : Process.T;
    buffer       : ARRAY [0 .. 4095] OF CHAR;
    nb           : INTEGER;
    args         : REF ARRAY OF TEXT;
    buf          : M3Buf.T;
    hwChildOut, hrSelf : Pipe.T;
    rd           : FileRd.T;
    arg          : QValue.T;
  BEGIN
    IF (n_args <= 0) THEN RETURN; END;

    (* pack the arguments into a single string & pop the stack *)
    buf   := M3Buf.New();
    FOR i := t.reg.sp - n_args TO t.reg.sp - 1 DO
      IF (first) THEN first := FALSE;  ELSE  M3Buf.PutChar (buf, ' ');  END;
      QVal.ToBuf (t, t.stack[i], buf);
      t.stack[i].ref := NIL;
    END;
    t.reg.sp := t.reg.sp - n_args;
    command := M3Buf.ToText (buf);

    (* strip the leading magic characters *)
    n := 0;
    WHILE n < Text.Length (command) DO
      IF Text.GetChar (command, n) = '@' THEN
        echo := FALSE;
      ELSE 
        EXIT;
      END;
      INC (n);
    END;
    command := Text.Sub (command, n);

    (* echo the command & flush any pending output *)
    TRY
      IF echo THEN
        Wr.PutText (t.writer, command);
        Wr.PutText (t.writer, Wr.EOL);
      END;
    EXCEPT
    | Wr.Failure, Thread.Alerted => (* ignore *)
    END;

    (* parse the command and the options *)
    n:= 0;
    command := command & " ";
    WHILE Text.GetChar(command, n) # ' ' DO INC(n);  END;
    cmd := Text.Sub(command, 0, n);
    args:=  BuildOptionList (Text.Sub(command, n));

    (* finally, execute the command *)
    TRY
      Pipe.Open (hr := hrSelf,  hw := hwChildOut);

      handle := Process.Create(cmd, args^, stdin := NIL, stdout := hwChildOut,
                               stderr := hwChildOut);
      hwChildOut.close ();

      rd := NEW(FileRd.T).init(hrSelf);
      LOOP
        nb := Rd.GetSub(rd, buffer);
        IF nb = NUMBER(buffer) THEN
          Wr.PutString(t.writer, buffer);
        ELSE
          Wr.PutString(t.writer, SUBARRAY(buffer, 0, nb));
          EXIT;
        END;
      END;
      Rd.Close(rd);
      Wr.Flush(t.writer);
      n := Process.Wait(handle);

    EXCEPT 
      | OSError.E =>
        RAISE Error ("exec failed" & command);
      | Wr.Failure, Thread.Alerted, Rd.Failure => (* ignore *)
    END;
    arg.kind := QValue.Kind.Integer;
    arg.int := n;
    t.push(arg);
  END DoTryExec;

PROCEDURE DoReplaceChar (t: QMachine.T;  n_args: INTEGER) RAISES {Error} =
  VAR
    string, old, new: QValue.T;
    oldtext, newtext, ret: TEXT;
  BEGIN
    <* ASSERT n_args = 3 *>
    t.pop(new);
    t.pop(old);
    t.pop(string);
    oldtext := QVal.ToText(t, old);
    newtext := QVal.ToText(t, new);
    IF (Text.Length(oldtext) # 1) OR (Text.Length(newtext) # 1) THEN
      RAISE Error ("replacechar: 'old' and 'new' must be a single character each");
    END;
    ret := ReplaceChar(QVal.ToText(t, string), Text.GetChar(oldtext, 0), 
                Text.GetChar(newtext, 0));
    string.int := M3ID.Add(ret);
    t.push(string);
  END DoReplaceChar;

PROCEDURE ReplaceChar(string: TEXT; old: CHAR; new: CHAR): TEXT=
  VAR chars: REF ARRAY OF CHAR;
  BEGIN
    chars := NEW(REF ARRAY OF CHAR, Text.Length(string));
    Text.SetChars(chars^, string);
    FOR I := FIRST(chars^) TO LAST(chars^) DO
      IF chars[I] = old THEN chars[I] := new; END;
    END;
    RETURN Text.FromChars(chars^);
  END ReplaceChar;

PROCEDURE DoW2P (t: QMachine.T;  n_args: INTEGER) RAISES {Error} =
  VAR
    string: QValue.T;
    ret: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(string);
    ret := W2P(QVal.ToText(t, string));
    string.int := M3ID.Add(ret);
    t.push(string);
  END DoW2P;

PROCEDURE W2P(string: TEXT): TEXT=
  VAR 
    chars: REF ARRAY OF CHAR;
    i := 0;
    j := 0;
    len := Text.Length(string);
    car: CHAR;
  BEGIN
    IF len >= 2 AND Text.GetChar(string,1) = ':' THEN
      chars := NEW(REF ARRAY OF CHAR, len + 1);
      chars[0] := '/'; chars[1] := '/';
      chars[2] := Text.GetChar(string,0);
      INC(i,2);
      INC(j,3);
    ELSE
      chars := NEW(REF ARRAY OF CHAR, len);
    END;
    WHILE i < len DO
      car := Text.GetChar(string,i);
      IF car = '\\' THEN car := '/'; END;
      chars[j] := car;
      INC(i); INC(j);
    END;
    RETURN Text.FromChars(chars^);
  END W2P;

PROCEDURE DoP2W (t: QMachine.T;  n_args: INTEGER) RAISES {Error} =
  VAR
    string: QValue.T;
    ret: TEXT;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(string);
    ret := P2W(QVal.ToText(t, string));
    string.int := M3ID.Add(ret);
    t.push(string);
  END DoP2W;

PROCEDURE P2W(string: TEXT): TEXT=
  VAR 
    chars: REF ARRAY OF CHAR;
    i := 0;
    j := 0;
    len := Text.Length(string);
    car: CHAR;
  BEGIN
    IF len >= 3 AND Text.GetChar(string,0) = '/' AND
      Text.GetChar(string,1) = '/' THEN
      chars := NEW(REF ARRAY OF CHAR, len - 1);
      chars[0] := Text.GetChar(string,2);
      chars[1] := ':';
      INC(i,3);
      INC(j,2);
    ELSE
      chars := NEW(REF ARRAY OF CHAR, len);
    END;
    WHILE i < len DO
      car := Text.GetChar(string,i);
      IF car = '/' THEN car := '\\'; END;
      chars[j] := car;
      INC(i); INC(j);
    END;
    RETURN Text.FromChars(chars^);
  END P2W;

PROCEDURE DoInc (t: QMachine.T;  n_args: INTEGER) RAISES {Error} =
  VAR
    string: QValue.T;
    arg1, arg2: INTEGER;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(string);
    arg2 := QVal.ToInt(t, string);
    t.pop(string);
    arg1 := QVal.ToInt(t, string);
    INC(arg1,arg2);
    string.kind := QValue.Kind.Integer;
    string.int := arg1;
    t.push(string);
  END DoInc;

(*-------------------------------------------------------------- dummy ------*)

PROCEDURE DoDummy(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR x: QValue.T;
  BEGIN
    (* clear the stack *)
    FOR I := 1 TO n_args DO
      t.pop(x);
    END;
    (* empty function... *)
  END DoDummy;

(*---------------------------------------------------------- os dependent ---*)

PROCEDURE DoDeleteFile(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    WITH t = NARROW(t, T) DO
      t.delete_file(t, QVal.ToText(t, arg));
    END;
  END DoDeleteFile;

PROCEDURE DoLinkFile(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR from, to: QValue.T;
  BEGIN
    <* ASSERT n_args = 2 *>
    t.pop(to);
    t.pop(from);
    WITH t = NARROW(t, T) DO
      t.link_file(t, QVal.ToText(t, from), QVal.ToText(t, to));
    END;
  END DoLinkFile;

PROCEDURE DoMakeExecutable(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    WITH t = NARROW(t, T) DO
      t.make_executable(t, QVal.ToText(t, arg));
    END;
  END DoMakeExecutable;

PROCEDURE DoMakeDir(t: QMachine.T; n_args: INTEGER) RAISES {Error}=
  VAR arg: QValue.T;
  BEGIN
    <* ASSERT n_args = 1 *>
    t.pop(arg);
    WITH t = NARROW(t, T) DO
      MakeDir(t, QVal.ToText(t, arg));
    END;
  END DoMakeDir;

PROCEDURE MakeDir(t: T; dir: TEXT) RAISES {Error} =
  VAR
    val    : QValue.T;
    dirs   := NEW(TextSeq.T).init();
  BEGIN
    IF t.get(M3ID.Add("_quiet"), val) THEN
      Wr.PutText(t.cur_wr(), "m3mkdir" & dir & t.CR) END; <* NOWARN *>
    IF NOT Pathname.Absolute(dir) THEN
      dir := Pathname.Join(Process.GetWorkingDirectory(), dir);
    END;
    LOOP
      TRY
        EVAL FS.Status(dir);
        EXIT;
      EXCEPT
      | OSError.E =>
        dirs.addlo(dir);
        dir := Pathname.Prefix(dir);
      END;
    END;
    TRY
      FOR i := 0 TO dirs.size() - 1 DO FS.CreateDirectory(dirs.get(i)); END;
    EXCEPT OSError.E =>
        FErr("unable to create directory \"" & dir & "\"");
    END;
  END MakeDir;

PROCEDURE CopyIfNew (<* UNUSED *> t: T; src, dest: TEXT) RAISES {Error} =
  VAR equal:= FALSE;
  BEGIN

    IF M3File.IsDirectory (dest) THEN
      dest := Pathname.Join (dest, Pathname.Last (src), NIL);
    END;

    TRY equal := M3File.IsEqual (src, dest) EXCEPT
      OSError.E => (* SKIP *)
    END;
    TRY
      IF NOT equal THEN M3File.Copy (src, dest) END;
    EXCEPT OSError.E =>
      FErr("unable to copy \""& src &"\" to \""& dest &"\"");
    END;
  END CopyIfNew;

PROCEDURE Stale(target, source: TEXT): BOOLEAN =
  VAR     
    t : File.Status;
    s : File.Status;
  BEGIN
    TRY
      t := FS.Status(target);
      s := FS.Status(source);
      RETURN t.modificationTime < s.modificationTime;
    EXCEPT  OSError.E =>
      RETURN TRUE;
    END;
  END Stale;

PROCEDURE Escape (txt: TEXT): TEXT  =
  VAR
    buf     : M3Buf.T;
    ch      : CHAR;
    len     := Text.Length(txt);
    out_len : INTEGER;
    out_buf : ARRAY [0..199] OF CHAR;
    in_buf  : ARRAY [0..199] OF CHAR;
    new_ch  : BOOLEAN := FALSE;
  BEGIN
    IF (len+len <= NUMBER (out_buf)) THEN
      out_len := 0;
      Text.SetChars (in_buf, txt);
      FOR i := 0 TO len-1 DO
        ch := in_buf[i];
        IF (ch = '\134') THEN
          out_buf[out_len] := ch; INC (out_len);
          new_ch := TRUE;
        END;
        out_buf [out_len] := ch;  INC (out_len);
      END;
      IF (new_ch)
        THEN RETURN Text.FromChars (SUBARRAY (out_buf, 0, out_len));
        ELSE RETURN txt;
      END;
    ELSE
      buf := M3Buf.New();
      FOR i := 0 TO len - 1 DO
        ch := Text.GetChar (txt, i);
        IF (ch = '\134') THEN M3Buf.PutChar (buf, ch); new_ch := TRUE; END;
        M3Buf.PutChar (buf, ch);
      END;
      IF (new_ch)
        THEN RETURN M3Buf.ToText (buf);
        ELSE RETURN txt;
      END;
    END;
  END Escape;

PROCEDURE NewProc (nm      : TEXT;
                   handler : QCode.BuiltinProc;
                   n_args  : INTEGER;
                   isFunc  : BOOLEAN): QValue.Proc =
  VAR
    id   := M3ID.Add (nm);
    info := NEW (QCode.ProcInfo, name := id, isFunc := isFunc,
                   n_args := n_args, builtin := TRUE, handler := handler);
    proc := NEW (QValue.Proc, info := info, env := NIL);
  BEGIN
    RETURN proc;
  END NewProc;

PROCEDURE InitProcs(): REF ARRAY OF ProcRec =
  VAR
    Procs := NEW(REF ARRAY OF ProcRec, 120);
  BEGIN
    Procs[0].proc := NewProc ("reset_cache", DoResetCache, 0, FALSE);
    Procs[1].proc := NewProc ("m3", DoM3, 0, FALSE);
    Procs[2].proc := NewProc("override", DoOverride, 2, FALSE);
    Procs[3].proc := NewProc("Pkg", DoPkg, 1, TRUE);
    Procs[4].proc := NewProc("M3include", DoM3include, 4, FALSE);
    Procs[5].proc := NewProc("Location", DoLocation, 2, TRUE);
    Procs[6].proc := NewProc("loc_pkg", DoLocPkg, 1, TRUE);
    Procs[7].proc := NewProc("loc_subdir", DoLocSubdir, 1, TRUE);
    Procs[8].proc := NewProc("Normalize", DoNormalize, 2, TRUE);
    Procs[9].proc := NewProc("pkg_subdir", DoPkgSubdir, 0, TRUE);
    Procs[10].proc := NewProc("path_of", DoPathOf, 1, TRUE);
    Procs[11].proc := NewProc("library_name", DoLibraryName, 1, TRUE);
    Procs[12].proc := NewProc("program_name", DoProgramName, 1, TRUE);
    Procs[13].proc := NewProc("deriveds", DoDeriveds, 2, FALSE);
    Procs[14].proc := NewProc("include_dir", DoIncludeDir, 1, FALSE);
    Procs[15].proc := NewProc("include_pkg", DoIncludePkg, 1, FALSE);
    Procs[16].proc := NewProc("pgm_object", DoPgmObject, 2, FALSE);
    Procs[17].proc := NewProc("import_obj", DoImportObj, 1, FALSE);
    Procs[18].proc := NewProc("import", DoImport, 1, FALSE);
    Procs[19].proc := NewProc("import_version", DoImportVersion, 2, FALSE);
    Procs[20].proc := NewProc("_define_lib", DoDummy, 1, FALSE);
    Procs[21].proc := NewProc("_define_pmg", DoDummy, 1, FALSE);
    Procs[22].proc := NewProc("_import_template", Do_ImportTemplate, 3, FALSE);
    Procs[23].proc := NewProc("_import_m3lib", Do_ImportM3Lib, 3, FALSE);
    Procs[24].proc := NewProc("_import_otherlib", Do_ImportOtherLib, 3, FALSE);
    Procs[25].proc := NewProc("_map_add_interface", Do_MapAddInterface, 4, FALSE);
    Procs[26].proc := NewProc("_map_add_generic_interface", 
                              Do_MapAddGenericInterface, 4, FALSE);
    Procs[27].proc := NewProc("_map_add_module", Do_MapAddModule, 4, FALSE);
    Procs[28].proc := NewProc("_map_add_generic_module", 
                              Do_MapAddGenericModule, 4, FALSE);
    Procs[29].proc := NewProc("_map_add_c", Do_MapAddC, 4, FALSE);
    Procs[30].proc := NewProc("_map_add_h", Do_MapAddH, 4, FALSE);
    Procs[31].proc := NewProc("_map_add_s", Do_MapAddS, 4, FALSE);
    Procs[32].proc := NewProc("interface", DoInterface, 1, FALSE);
    Procs[33].proc := NewProc("Interface", DoINTERFACE, 1, FALSE);
    Procs[34].proc := NewProc("implementation", DoImplementation, 1, FALSE);
    Procs[35].proc := NewProc("module", DoModule, 1, FALSE);
    Procs[36].proc := NewProc("Module", DoMODULE, 1, FALSE);
    Procs[37].proc := NewProc("generic_interface", DoGenericInterface, 1, 
                              FALSE);
    Procs[38].proc := NewProc("Generic_interface", DoGENERIC_INTERFACE, 1,
                              FALSE);
    Procs[39].proc := NewProc("generic_implementation", 
                              DoGenericImplementation, 1, FALSE);
    Procs[40].proc := NewProc("Generic_implementation", 
                              DoGENERIC_IMPLEMENTATION, 1, FALSE);
    Procs[41].proc := NewProc("generic_module", DoGenericModule, 1, FALSE);
    Procs[42].proc := NewProc("Generic_module", DoGENERIC_MODULE, 1, FALSE);
    Procs[43].proc := NewProc("library", DoLibrary, 1, FALSE);
    Procs[44].proc := NewProc("Library", DoLIBRARY, 1, FALSE);
    Procs[45].proc := NewProc("h_source", DoHSource, 1, FALSE);
    Procs[46].proc := NewProc("c_source", DoCSource, 1, FALSE);
    Procs[47].proc := NewProc("s_source", DoSSource, 1, FALSE);
    Procs[48].proc := NewProc("derived_interface", DoDerivedInterface, 2,
                              FALSE);
    Procs[49].proc := NewProc("derived_implementation",
                              DoDerivedImplementation, 1, FALSE);
    Procs[50].proc := NewProc("derived_c", DoDerivedC, 1, FALSE);
    Procs[51].proc := NewProc("derived_h", DoDerivedH, 1, FALSE);
    Procs[52].proc := NewProc("import_lib", DoImportLib, 2, FALSE);
    Procs[53].proc := NewProc("build_generic_intf", DoBuildGenericIntf, 4, 
                              FALSE);
    Procs[54].proc := NewProc("build_generic_impl", DoBuildGenericImpl, 3, 
                              FALSE);
    Procs[55].proc := NewProc("hide_interface", DoHideInterface, 1, FALSE);
    Procs[56].proc := NewProc("hide_generic_interface", 
                              DoHideGenericInterface, 1, FALSE);
    Procs[57].proc := NewProc("hide_generic_implementation", 
                              DoHideGenericImplementation, 1, FALSE);

    Procs[58].proc := NewProc("export_interface", DoExportInterface, 1, FALSE);
    Procs[59].proc := NewProc("export_generic_interface", 
                              DoExportGenericInterface, 1, FALSE);
    Procs[60].proc := NewProc("export_generic_implementation", 
                              DoExportGenericImplementation, 1, FALSE);
    Procs[61].proc := NewProc("resource_named", DoResourceNamed, 2, FALSE);
    Procs[62].proc := NewProc("resource", DoResource, 1, FALSE);
    Procs[63].proc := NewProc("bundle", DoBundle, 1, FALSE);
    Procs[64].proc := NewProc("template", DoTemplate, 1, FALSE);
    Procs[65].proc := NewProc("BinExport", DoBinExport, 1, FALSE);
    Procs[66].proc := NewProc("BindExport", DoBindExport, 1, FALSE);
    Procs[67].proc := NewProc("LibExport", DoLibExport, 1, FALSE);
    Procs[68].proc := NewProc("LibdExport", DoLibdExport, 1, FALSE);
    Procs[69].proc := NewProc("EmacsExport", DoEmacsExport, 1, FALSE);
    Procs[70].proc := NewProc("EmacsdExport", DoEmacsdExport, 1, FALSE);
    Procs[71].proc := NewProc("DocExport", DoDocExport, 1, FALSE);
    Procs[72].proc := NewProc("DocdExport", DoDocdExport, 1, FALSE);
    Procs[73].proc := NewProc("ManExport", DoManExport, 2, FALSE);
    Procs[74].proc := NewProc("MandExport", DoMandExport, 2, FALSE);
    Procs[75].proc := NewProc("HtmlExport", DoHtmlExport, 1, FALSE);
    Procs[76].proc := NewProc("manPage", DomanPage, 2, FALSE);
    Procs[77].proc := NewProc("ManPage", DoManPage, 2, FALSE);
    Procs[78].proc := NewProc("Gnuemacs", DoGnuemacs, 1, FALSE);
    Procs[79].proc := NewProc("CompiledGnuemacs", DoCompiledGnuemacs, 1, FALSE);
    Procs[80].proc := NewProc("Noweb", DoNoweb, 3, FALSE);
    Procs[81].proc := NewProc("noweb_interface", DonowebInterface, 3, FALSE);
    Procs[82].proc := NewProc("Noweb_interface", DoNowebInterface, 3, FALSE);
    Procs[83].proc := NewProc("noweb_implementation", DoNowebImplementation,
                              3, FALSE);
    Procs[84].proc := NewProc("install_derived", DoInstallDerived, 1, FALSE);
    Procs[85].proc := NewProc("install_link", DoInstallLink, 3, FALSE);
    Procs[86].proc := NewProc("zume", DoZume, 1, FALSE);
    Procs[87].proc := NewProc("RegisterCleanUp", DoRegisterCleanUp, 1, FALSE);
    Procs[88].proc := NewProc("RunCleanUps", DoRunCleanUps, 0, FALSE);
    Procs[89].proc := NewProc("find_unit", DoFindUnit, 2, FALSE);
    Procs[90].proc := NewProc("enum_units", DoEnumUnits, 1, FALSE);
    Procs[91].proc := NewProc("program", DoProgram, 1, FALSE);
    Procs[92].proc := NewProc("Program", DoPROGRAM, 1, FALSE);
    Procs[93].proc := NewProc("c_program", DoCProgram, 1, FALSE);
    Procs[94].proc := NewProc("C_program", DoCPROGRAM, 1, FALSE);
    Procs[95].proc := NewProc("m3_option", DoM3Option, 1, FALSE);
    Procs[96].proc := NewProc("remove_m3_option", DoRemoveM3Option, 1, FALSE);
    Procs[97].proc := NewProc("delete_file", DoDeleteFile, 1, FALSE);
    Procs[98].proc := NewProc("link_file", DoLinkFile, 2, FALSE);
    Procs[99].proc := NewProc("make_executable", DoMakeExecutable, 1, FALSE);
    Procs[100].proc := NewProc("make_dir", DoMakeDir, 1, FALSE);
    Procs[101].proc := NewProc("try_exec", DoTryExec, -1, TRUE);
    Procs[102].proc := NewProc("pgm_source", DoPgmSource, 1, FALSE);
    Procs[103].proc := NewProc("Note_install", DoNoteInstall, 2, FALSE);
    Procs[104].proc := NewProc("_define_pgm", DoDummy, 1, FALSE);
    Procs[105].proc := NewProc("_define_lib", DoDummy, 1, FALSE);
    Procs[106].proc := NewProc("m3front_option", DoM3FrontOption, 1, FALSE);
    Procs[107].proc := NewProc("remove_m3front_option", 
        DoRemoveM3FrontOption, 1, FALSE);
    Procs[108].proc := NewProc("replacechar", DoReplaceChar, 3, TRUE);
    Procs[109].proc := NewProc("w2p", DoW2P, 1, TRUE);
    Procs[110].proc := NewProc("p2w", DoP2W, 1, TRUE);
    Procs[111].proc := NewProc("derived_resource", DoDerivedResource,1, FALSE);
    Procs[112].proc := NewProc("generate_tfile", DoGenerateTFile, 0, FALSE);
    Procs[113].proc := NewProc("gen_m3exports", DoGenM3Exports, 1, FALSE);
    Procs[114].proc := NewProc("install_sources", DoInstallSources, 0, FALSE);
    Procs[115].proc := NewProc("_install_file", DoInstallFile, 4, FALSE);
    Procs[116].proc := NewProc("inc", DoInc, 2, TRUE);
    Procs[117].proc := NewProc("install_alias_link", DoInstallAliasLink, 4,
			       FALSE);
    Procs[118].proc := NewProc("build_generic_unsafe_intf", 
                               DoBuildGenericUnsafeIntf, 4, FALSE);
    Procs[119].proc := NewProc("build_generic_unsafe_impl", 
                               DoBuildGenericUnsafeImpl, 3, FALSE);
    RETURN Procs;
  END InitProcs;

PROCEDURE Setup(t: T) RAISES {Error}=
  PROCEDURE GetText(x: TEXT; msg: TEXT): TEXT RAISES {Error}=
    VAR val: QValue.T;
    BEGIN
      IF NOT t.get(M3ID.Add(x), val) THEN
        RAISE Error(msg & " " & x & " not defined" & t.CR);
      END;
      RETURN QVal.ToText(t, val);
    END GetText;
  PROCEDURE GetVal(x: TEXT; msg: TEXT): QValue.T RAISES {Error}=
    VAR val: QValue.T;
    BEGIN
      IF NOT t.get(M3ID.Add(x), val) THEN
        RAISE Error(msg & " " & x & " not defined" & t.CR);
      END;
      RETURN val;
    END GetVal;
  VAR 
    nm_conv: QVSeq.T;
    tnm_conv: QVSeq.T;
    val: QValue.T;
    wr: Wr.T;
    host_os_type: TEXT;
  BEGIN
    (* Get the host and target naming conventions *)
    t.conv := NEW(M3Driver.NamingConvention);
    nm_conv := QVal.ToArray(t,
        GetVal("NAMING_CONVENTIONS","naming conventions"));
    FillNamingConvention(t,t.conv,nm_conv);

    t.target_conv := NEW(M3Driver.NamingConvention);
    tnm_conv := QVal.ToArray(t,
        GetVal("TARGET_NAMING_CONVENTIONS","target naming conventions"));
    FillNamingConvention(t,t.target_conv,tnm_conv);

    M3Driver.Setup(t.conv, t.target_conv);

    t.OBJ_ext := t.conv.suffix[M3Driver.Suffixes.O];
    t.IO_ext  := t.conv.suffix[M3Driver.Suffixes.IO];
    t.MO_ext  := t.conv.suffix[M3Driver.Suffixes.MO];
    t.LIB_pre := t.conv.lib_prefix;
    t.LIB_ext := t.conv.suffix[M3Driver.Suffixes.A];
    t.PGM_ext := t.conv.suffix[M3Driver.Suffixes.EXE];
    t.CR := t.conv.EOL;
    t.SL := Text.FromChar(t.conv.dirSep);
    t.CRship := GetText("CRship","CR ship");
    t.SLship := GetText("SLship","SL ship");
    t.QRPCR := "\")" & t.CR;

    (* get all install and use directories *)
    t.PKG_USE       := GetText("PKG_USE", "use directory");
    t.PKG_INSTALL   := GetText("PKG_INSTALL", "installation directory");
    t.BIN_USE       := GetText("BIN_USE", "use directory");
    t.BIN_INSTALL   := GetText("BIN_INSTALL", "installation directory");
    t.LIB_USE       := GetText("LIB_USE", "use directory");
    t.LIB_INSTALL   := GetText("LIB_INSTALL", "installation directory");
    t.MAN_INSTALL   := GetText("MAN_INSTALL", "installation directory");
    t.EMACS_INSTALL := GetText("EMACS_INSTALL", "installation directory");
    t.DOC_INSTALL   := GetText("DOC_INSTALL", "installation directory");
    t.HTML_INSTALL  := GetText("HTML_INSTALL", "installation directory");

    t.intf_extensions[0] := t.conv.suffix[M3Driver.Suffixes.IX];
    t.intf_extensions[1] := t.conv.suffix[M3Driver.Suffixes.IC];
    t.intf_extensions[2] := t.conv.suffix[M3Driver.Suffixes.IS];
    t.intf_extensions[3] := t.conv.suffix[M3Driver.Suffixes.IO];
    t.impl_extensions[0] := t.conv.suffix[M3Driver.Suffixes.MX];
    t.impl_extensions[1] := t.conv.suffix[M3Driver.Suffixes.MC];
    t.impl_extensions[2] := t.conv.suffix[M3Driver.Suffixes.MS];
    t.impl_extensions[3] := t.conv.suffix[M3Driver.Suffixes.MO];
    t.c_extensions[0]    := t.conv.suffix[M3Driver.Suffixes.S];
    t.c_extensions[1]    := t.conv.suffix[M3Driver.Suffixes.O];
    t.s_extensions[0]    := t.conv.suffix[M3Driver.Suffixes.O];
    t.no_extension[0]    := t.conv.suffix[M3Driver.Suffixes.Unknown];
    t.rsrc_extensions[0] := t.conv.suffix[M3Driver.Suffixes.I3];
    t.rsrc_extensions[1] := t.conv.suffix[M3Driver.Suffixes.M3];
    FOR i := 0 TO LAST(t.intf_extensions) DO
      t.rsrc_extensions[i+2] := t.intf_extensions[i];
    END;
    FOR i := 0 TO LAST(t.impl_extensions) DO
      t.rsrc_extensions[i+2+LAST(t.intf_extensions)+1] := t.impl_extensions[i];
    END;

    host_os_type := GetText("HOST_OS_TYPE","host os type");

    IF Text.Equal(host_os_type,"POSIX") THEN
      t.delete_file := BldPosix.DelFile;
      t.link_file := BldPosix.LinkFile;
      t.make_executable := BldPosix.MakeExec;
    ELSIF Text.Equal(host_os_type,"WIN32") THEN
      t.delete_file := BldWin32.DelFile;
      t.link_file   := BldWin32.LinkFile;
      t.make_executable := BldWin32.MakeExec;
    ELSE
      Err("OS_TYPE unknown: " & host_os_type);
    END;
   
    (* add the OS dependent variable to the global scope *)

    (* define VISIBLE and HIDDEN *)
    val.kind := QValue.Kind.String;
    val.int := M3ID.Add("");
    t.put(M3ID.Add("VISIBLE"), val);
    val.int := M3ID.Add("HIDDEN");
    t.put(M3ID.Add("HIDDEN"), val);

    (* define IMPORTED and LOCAL *)
    val.int := QValue.BoolID[FALSE];
    t.put(M3ID.Add("IMPORTED"), val);
    val.int := QValue.BoolID[TRUE];
    t.put(M3ID.Add("LOCAL"), val);

    IF t.get(all, val) THEN
      t.all := TRUE;
    END;

    IF t.all THEN
      BldHooks.DeleteFile(t, M3SHIP_FILE);
      BldHooks.DeleteFile(t, M3OVERRIDES);
      TRY
        wr := Utils.OpenWriter(M3SHIP_FILE, TRUE);
        IF t.get(M3ID.Add("HAVE_PKGTOOLS"), val) AND QVal.ToBool(t, val) THEN
          Wr.PutText(wr, t.CRship);
        ELSE
          Wr.PutText(wr, t.CR);
        END;
      EXCEPT
        M3Driver.Error, Thread.Alerted, Wr.Failure => FErr(M3SHIP_FILE);
      END;
    END;
    Deriveds(t, M3SHIP_FILE, t.no_extension);
    Deriveds(t, M3OVERRIDES, t.no_extension);

  END Setup;

PROCEDURE FillNamingConvention(t: T; conv: M3Driver.NamingConvention;
    READONLY nm_conv: QVSeq.T) RAISES {Error} =
  BEGIN
    IF nm_conv.size() < 30 THEN
      RAISE Error("Not enough elements for naming convention" & t.CR);
    END;
    FOR i := FIRST(M3Driver.Suffixes) TO LAST(M3Driver.Suffixes) DO
      conv.suffix[i] := QVal.ToText(t,nm_conv.get(ORD(i)));
    END;
    conv.default_pgm := QVal.ToText(t,nm_conv.get(22));
    conv.lib_prefix := QVal.ToText(t,nm_conv.get(23));
    conv.dirSep := Text.GetChar(QVal.ToText(t,nm_conv.get(24)),0);
    conv.EOL := QVal.ToText(t,nm_conv.get(25));
    conv.pathSep :=Text.GetChar(QVal.ToText(t,nm_conv.get(26)),0);
    conv.volSep :=Text.GetChar(QVal.ToText(t,nm_conv.get(27)),0);
    conv.short_names :=QVal.ToBool(t,nm_conv.get(28));
    conv.case_insensitive_ext :=QVal.ToBool(t,nm_conv.get(29));
  END FillNamingConvention;

PROCEDURE GetSL(t: T): TEXT=
  BEGIN
    RETURN t.SL;
  END GetSL;

PROCEDURE Err(t: TEXT) RAISES {Error}=
  BEGIN
    RAISE Error(t);
  END Err;

PROCEDURE CErr(t: TEXT) RAISES {Error}=
  BEGIN
    RAISE Error("compiler error caught: " & t & "\n");
  END CErr;

PROCEDURE FErr(t: TEXT) RAISES {Error}=
  BEGIN
    RAISE Error("file error: " & t & "\n");
  END FErr;

BEGIN
  (* init the IDs *)
  NOT_A_PACKAGE := M3ID.Add(NOT_A_PACKAGE_TEXT);
  all           := M3ID.Add("_all");
END BldQuake.
