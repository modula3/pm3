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
PROCEDURE ResetCompiler (wr: Wr.T);
PROCEDURE Compile (i: Interface; options: TEXT) RAISES {M3Error};
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

EXCEPTION InterErr;

TYPE
  Interface = OBJECT
    link_coverage   : TEXT := NIL;
    conv            : CHAR := '\000';
    target_conv     : CHAR := '\000';
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
    makelib(name, libs, imp: TEXT; static, shared: BOOLEAN) RAISES {InterErr};
  END;

END M3Driver.

