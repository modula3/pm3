INTERFACE M3DriverRep;

IMPORT Arg, Mx, M3Path;

VAR
  lib_path      : Arg.List;
  libraries     : FileInfo := NIL;

TYPE
  FileInfo = REF RECORD
    next         : FileInfo := NIL;
    library      : TEXT     := NIL;
    source       : TEXT     := NIL;
    object       : TEXT     := NIL;
    link_info    : Mx.Unit  := NIL;
    link_info_ts : INTEGER  := LAST(INTEGER);
    exporters    : FileList := NIL;
    name         : M3Path.T;
    compiling    : BOOLEAN  := FALSE;
    stale_src    : BOOLEAN  := FALSE;
    missing_info : BOOLEAN  := FALSE;
    shared       : BOOLEAN  := FALSE;
  END;

TYPE
  FileList = REF RECORD
    file : FileInfo;
    next : FileList;
  END;

END M3DriverRep.
