INTERFACE Config;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.8  1998/07/29 15:14:03  roland
    Increasing stack sizes.

    Revision 1.7  1997/12/15 16:41:33  roland
    Use localhost as default nameserver (agent).

    Revision 1.6  1997/06/13 11:26:52  rbnix
    	Functions GetRootPath, GetTempPrefix and GetPersPath replaced
    	by GetRootPrefix.

    Revision 1.5  1997/06/10 12:52:14  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.4  1997/03/21 16:51:01  roland
    New procedure DefaultServerId added that computes the standard
    GRAS-3.<uid> name for the gras3 page server. This will be used, when
    no other id for the server is given.
    GetGrasServer changed to GetGrasServerId.

    Revision 1.3  1997/03/20 16:53:35  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.2  1996/11/21 15:21:18  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.1  1996/02/28 10:56:38  rbnix
        First version of configuration storage.

*)
(***************************************************************************)

(*
 | --- Config -------------------------------------------------------------
 This abstract data object module gives access to commonly used
 configuration data.
 | ------------------------------------------------------------------------
 *)
IMPORT Pathname;

CONST
  DefaultNameServer = "localhost";
  DefaultCacheSize  = 100;
  DefaultStackSize  = 40000;

PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := DefaultCacheSize;
                 nameserver: TEXT         := DefaultNameServer;
                 grasserver: TEXT                               );

PROCEDURE DefaultServerId(): TEXT;

PROCEDURE GetNameServer (): TEXT;
PROCEDURE GetGrasServerId (): TEXT;

PROCEDURE GetRootPrefix (temporary :BOOLEAN): Pathname.T RAISES {Unspecified};
  (*
    Returns the temporary/persistent root path wich should prefix all
    accessed resources and file names.

    Currently the persistent root prefix is determined by the root
    parameter of login. The temporary root prefix can be set by the
    environment variable 'TMPGRAS' otherwise it will be calculated
    (/var/tmp/$USER).
  *)

PROCEDURE GetCacheSize (): CARDINAL;

EXCEPTION Unspecified;

END Config.
