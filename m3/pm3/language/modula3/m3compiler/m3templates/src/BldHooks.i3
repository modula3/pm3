INTERFACE BldHooks;

IMPORT IntM3LibsTbl, QValue, QVSeq;
FROM BldQuake IMPORT T;
FROM Quake IMPORT Error;

PROCEDURE DeleteFile(t: T; x: TEXT) RAISES {Error};
PROCEDURE LinkFile(t: T; from, to: TEXT) RAISES {Error};
PROCEDURE EmacsCompile(t: T; x: TEXT) RAISES {Error};
PROCEDURE GenMapHooks(t: T) RAISES {Error};
PROCEDURE BeforeDoM3Hooks(t: T) RAISES {Error};
PROCEDURE InstallUnitsHooks(t: T; install_impls: BOOLEAN): 
  REF ARRAY OF IntM3LibsTbl.T RAISES {Error};
PROCEDURE WhereUnitsHooks(t: T): REF ARRAY OF IntM3LibsTbl.T RAISES {Error};
PROCEDURE CompileC(t: T; src, obj: TEXT; incl: QVSeq.T; optimize, debug, 
                   shared: BOOLEAN): INTEGER RAISES {Error};
PROCEDURE M3Link(t: T; prog: TEXT; objs, libs: QVSeq.T; debug, 
                 shared: BOOLEAN): INTEGER RAISES {Error};
PROCEDURE M3MakeLib(t: T; name: TEXT; libs, imp: QVSeq.T;
                    static, shared: BOOLEAN): INTEGER RAISES {Error};
PROCEDURE NoteShlib(t: T; name: TEXT): INTEGER RAISES {Error};
PROCEDURE M3Assemble(t: T; src, obj: TEXT; optimize, debug, shared: BOOLEAN):
  INTEGER RAISES {Error};
PROCEDURE M3Backend(t: T; src, obj: TEXT; optimize,
                    debug, shared: BOOLEAN): INTEGER RAISES {Error};
PROCEDURE ExecHook(t: T; name: TEXT; READONLY args: REF ARRAY OF QValue.T;
                   isFunc: BOOLEAN := FALSE): BOOLEAN RAISES {Error};

END BldHooks.
