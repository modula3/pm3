(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Jul  6 11:28:36 PDT 1994 by jcollin    *)
(* 							       *)
(* The SRC-Modula-3 Compiler driver interface		       *)

INTERFACE M3Driver;

IMPORT Wr, M3Path, LibSeq, TextSeq;

EXCEPTION M3Error;
EXCEPTION Error;

PROCEDURE Init(wr: Wr.T);
PROCEDURE Setup(host,target: NamingConvention);
PROCEDURE ResetCompiler (wr: Wr.T);
PROCEDURE Compile (i: Interface) RAISES {M3Error};
PROCEDURE ResetASTCache ();
PROCEDURE AddSourceFile (dir, name: TEXT;  cmd_line := FALSE) RAISES {Error};
PROCEDURE AddLibrary (file: TEXT;  READONLY name: M3Path.T);
PROCEDURE PushPath (new: TEXT);

TYPE
  BackendOutput = {Asm, Obj};

TYPE
  GenMethod = {Compiler, Backend};

TYPE
  OptArr = REF ARRAY OF TEXT;

  Suffixes = {I3,IC,IS,IO,M3,MC,MS,MO,IG,MG,C,H,S,O,A,AX,PX,Unknown,IX,MX,EXE,
      SO};

  NamingConvention = REF RECORD
      suffix: ARRAY Suffixes OF TEXT;
      default_pgm, lib_prefix, EOL: TEXT;
      dirSep, volSep, pathSep: CHAR;
      short_names, case_insensitive_ext: BOOLEAN;
    END;

EXCEPTION InterErr;

TYPE
  Interface = OBJECT
    dump_config     : BOOLEAN := FALSE;
    bootstrap_mode  : BOOLEAN := FALSE;
    bootstrap_il    : BOOLEAN := FALSE;
    gui             : BOOLEAN := FALSE;
    do_debug        : BOOLEAN := FALSE;
    heap_stats      : BOOLEAN := FALSE;
    keep_cache      : BOOLEAN := FALSE;
    keep_files      : BOOLEAN := FALSE;
    make_mode       : BOOLEAN := TRUE;
    compile_once    : BOOLEAN := FALSE;
    do_optimize     : BOOLEAN := FALSE;
    skip_link       : BOOLEAN := FALSE;
    times           : BOOLEAN := FALSE;
    do_coverage     : BOOLEAN := FALSE;
    lib_name        : TEXT := NIL;
    pgm_name        : TEXT := NIL;
    no_m3main       : BOOLEAN := FALSE;
    link_coverage   : TEXT := NIL;
    msg_level       : INTEGER := 0;
    warning_level   : INTEGER := 3;
    target          : TEXT := NIL;
    shared_libs     : BOOLEAN := FALSE;
    ext_pass_6      : BOOLEAN := FALSE;
    ext_pass_7      : BOOLEAN := FALSE;
    backend_output  := BackendOutput.Asm;
    has_loader      : BOOLEAN := FALSE;
    gen_shared      : BOOLEAN := FALSE;
    gen_static      : BOOLEAN := FALSE;
    gen_method      := GenMethod.Backend;
    m3front_options : OptArr := NIL;
  METHODS
    compileC(source, object: TEXT; includes: TextSeq.T; 
             optimize, debug, shared: BOOLEAN) RAISES {InterErr};
    backend(source, object: TEXT; optimize, debug, shared: BOOLEAN) RAISES {InterErr};
    assemble(source, object: TEXT; optimize, debug, shared: BOOLEAN) RAISES {InterErr};
    link(prog: TEXT; objs: TextSeq.T; libs: LibSeq.T; 
         debug, shared: BOOLEAN) RAISES {InterErr};
    makelib(name: TEXT; libs, imp: TextSeq.T; 
            static, shared: BOOLEAN) RAISES {InterErr};
  END;

END M3Driver.

