(* File under construction... *)

INTERFACE IdeHTTP;

(* The http daemon part of the ide reads a rule file which may contain
   the following:

   ipMask nbbits
   threads nbthreads
   port portno
   refresh refreshdelay

   scanPackage rootdir
   excludePackage pkgname
   buildDir builddir

   map indir outdir
   pass dir
   fail dir
   exec indir command outdir
   execBuiltin indir command outdir

*)

PROCEDURE ReadRules(filename: TEXT);

PROCEDURE StartDaemon(passwd: TEXT);

PROCEDURE RegisterBuiltin(name: TEXT; init: InitProc; request: RequestProc);

TYPE
  InitProc = PROCEDURE(TextRefTbl.T; error: TEXT): BOOLEAN;
  RequestProc = PROCEDURE(command, path, query: TEXT);

END IdeHTTP;
