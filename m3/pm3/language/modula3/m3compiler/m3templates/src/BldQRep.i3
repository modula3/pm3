INTERFACE BldQRep;

IMPORT BldQuake, TextRefTbl, Pathname, IntTextTbl, TextLocTbl;
IMPORT IntMapTbl, IntM3LibsTbl, TextSeq, QVSeq, M3Buf, TextTextTbl;
IMPORT M3Driver;

TYPE
  DelFileProc = PROCEDURE (t: BldQuake.T; x: TEXT);
  LinkFileProc = PROCEDURE (t: BldQuake.T; from, to: TEXT);
  MakeExecProc = PROCEDURE (t: BldQuake.T; script: TEXT);

REVEAL
  BldQuake.T <: Private;

TYPE
  Private = BldQuake.Public OBJECT
    PKG_USE         :  TEXT;
    PKG_INSTALL     :  TEXT;
    BIN_USE         :  TEXT;
    BIN_INSTALL     :  TEXT;
    LIB_USE         :  TEXT;
    LIB_INSTALL     :  TEXT;
    MAN_INSTALL     :  TEXT;
    EMACS_INSTALL   :  TEXT;
    DOC_INSTALL     :  TEXT;
    HTML_INSTALL    :  TEXT;
    conv            :  M3Driver.NamingConvention;
    target_conv     :  M3Driver.NamingConvention;
    lib_name        : TEXT := NIL;
    pgm_name        : TEXT := NIL;
    no_m3main       : BOOLEAN := FALSE;
    CR              := "\n";
    SL              := "/";
    CRship          := "\n";
    SLship          := "/";
    QRPCR           := "\")\n"; (* "\")" & CR *)
    OBJ_ext         :  TEXT;
    IO_ext          :  TEXT;  
    MO_ext          :  TEXT;
    LIB_pre         :  TEXT; 
    LIB_ext         :  TEXT;
    PGM_ext         :  TEXT; 
    (* must be 0 based *)
    intf_extensions :  ARRAY [0..3] OF TEXT;
    impl_extensions :  ARRAY [0..3] OF TEXT;
    c_extensions    :  ARRAY [0..1] OF TEXT;
    s_extensions    :  ARRAY [0..0] OF TEXT;
    no_extension    :  ARRAY [0..0] OF TEXT;
    rsrc_extensions  :  ARRAY [0..9] 
                             (* [0..1+NUMBER(intf_extensions)+
                                 NUMBER(impl_extensions) *) OF TEXT; 
    package         :  TEXT;
    build_package   :  TEXT;
    last_ship_dir   := "";
    all_ship_dirs   :  TextRefTbl.T;
    package_dir     :  Pathname.T;
    build_dir       :  Pathname.T;
    imports         :  IntTextTbl.T;
    pkg_cache       :  IntTextTbl.T;
    pkg_overrides   :  IntTextTbl.T;
    locations       :  TextLocTbl.T;
    pkg_dirs        :  IntMapTbl.T;
    m3libs          :  IntM3LibsTbl.T;
    m3libs_x        :  TextSeq.T;
    other_libs      :  IntM3LibsTbl.T;
    other_libs_x    :  TextSeq.T;
    compile_objects :  QVSeq.T;
    interface_sources : IntM3LibsTbl.T;
    module_sources  : IntM3LibsTbl.T;
    generic_interface_sources : IntM3LibsTbl.T;
    generic_module_sources: IntM3LibsTbl.T;
    c_sources       :  IntM3LibsTbl.T;
    c_inputs        :  IntTextTbl.T;
    h_sources       :  IntM3LibsTbl.T;
    h_inputs        :  TextSeq.T;
    h_dirs          :  TextTextTbl.T;
    s_sources       :  IntM3LibsTbl.T;
    tfile_args      :  M3Buf.T;
    templates       :  IntM3LibsTbl.T;
    derived_sources :  TextTextTbl.T;
    resources       :  TextTextTbl.T;
    cleanup_procs   :  TextSeq.T;
    m3_options      :  TextSeq.T;
    delete_file     :  DelFileProc;
    link_file       :  LinkFileProc;
    make_executable :  MakeExecProc;
    path_of_path    := "";
    path_of_base    := "";
    pkg_subdir_path := "";
    pkg_subdir_base := "";
    warned          := FALSE;
    all             := FALSE;
    m3front_options : TextSeq.T;
  END;

END BldQRep.

