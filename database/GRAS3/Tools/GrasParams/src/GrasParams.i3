INTERFACE GrasParams;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.3  1997/12/15 16:37:32  roland
    Parameter nameserver changed to agent to avoid confusion about netobjd
    being nameserver.

    Revision 1.2  1997/03/25 17:07:33  roland
    GrasServerId-Parameter added.

    Revision 1.1  1997/02/28 13:28:25  roland
    A utility module for reading GRAS3 configuration options from
    command-line or environment variables.

*)
(***************************************************************************)

(**
   Every GRAS application has to login to the GRAS DBMS. At login time,
   four parameters - one obligat and three optional - can be given to
   the system

   RootPath   : The directory which contains local files and shadow files.
                This is obligate for every GRAS application.

   CacheSize  : The size of the client main memory cache for database pages
                given in number of pages (1 page = 8 kByte).

   ServerId   : The ID of the GRAS server process the program wants to attach to.

   NameServer : The machine running the netobjd that knows about the
                server this application wants to attach to.
*)

TYPE Param = {RootPath, CacheSize, NameServer, ServerId};

CONST
  DefaultKeys = ARRAY Param OF TEXT{"-root", "-chachesize", "-agent", "-id"};
  DefaultEnv = ARRAY Param OF
                 TEXT{"GRAS3", "GRAS3ALLOC", "GRAS3NAMESERVER", "GRAS3SERVERID"};


PROCEDURE SetKeyword (param: Param; key: TEXT);
  (* Replace the default keyword for param by key. *)

PROCEDURE SetEnvName (param: Param; name: TEXT);
  (* Replace the default environment variable name for param by name. *)

PROCEDURE ParseComandLine (VAR root      : TEXT;
                           VAR rootValid : BOOLEAN;
                           VAR cacheSize : CARDINAL;
                           VAR serverid  : TEXT;
                           VAR nameserver: TEXT;
                               quiet     : BOOLEAN    := FALSE);
  (* Lookup environment and parses the command line for the three values.
     Command line switches override environment.  If neither environment
     nor command line specifies CacheSize or NameServer, the default from
     Config.i3 will be returned. If not quiet, occuring errors will be printed
     to stderr. *)

END GrasParams.
