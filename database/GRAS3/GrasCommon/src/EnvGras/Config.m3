UNSAFE MODULE Config;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.11  1997/06/20 14:03:53  roland
    Bugfix: UserId must be initialized before Login.

    Revision 1.10  1997/06/13 16:08:06  roland
    User name now read from passwd.

    Revision 1.9  1997/06/13 11:26:54  rbnix
        Functions GetRootPath, GetTempPrefix and GetPersPath replaced
        by GetRootPrefix.

    Revision 1.8  1997/06/10 12:52:16  roland
    Temporary data of resources is now stored in a directory determined by
    Config.GetTempPath(), the root path, and teh resource
    name. Config.GetTempPath in turn is either a default value (currently
    /var/tmp) or the value of envoronment variable TMPGRAS, if this is a
    valid path. Temporary directories will be deleted on closing a resource.

    Revision 1.7  1997/03/21 16:51:03  roland
    New procedure DefaultServerId added that computes the standard
    GRAS-3.<uid> name for the gras3 page server. This will be used, when
    no other id for the server is given.
    GetGrasServer changed to GetGrasServerId.

    Revision 1.6  1997/03/20 16:53:37  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.5  1996/11/21 15:21:19  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.4  1996/09/26 18:48:29  roland
    Command line is now parsed with ParseParams.

    Attention: All option keywords are prefixed with 'g3'.

    Revision 1.3  1996/08/06 16:25:07  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/06/12 10:36:03  roland
    Command line parsing: parameters are not any longer positional and
    command line can now have switches for other parts of a program.

    Revision 1.2.2.1  1996/04/29 13:36:17  roland
    Changed ASSERT FALSE to Process.Exit in initialization.

    Revision 1.2  1996/02/28 14:08:42  rbnix
        Bug fixed: arg bindings allocated seperately.

    Revision 1.1  1996/02/28 10:56:40  rbnix
        First version of configuration storage.

*)
(***************************************************************************)
(*
 | --- Config -------------------------------------------------------------
 On simplification this module is currently implemented via command line
 arguments and internal default values.
 | ------------------------------------------------------------------------
 *)
IMPORT Pathname, Uugid, Upwd, Fmt, Env, M3toC;


CONST
  DefaultTempPrefix = "/var/tmp";

  TmpGrasVar = "TMPGRAS";

VAR
  rootPrefix: TEXT     := NIL;
  nameServer: TEXT     := DefaultNameServer;
  cacheSize : CARDINAL := DefaultCacheSize;
  grasServer: TEXT     := NIL;
  tempPrefix: TEXT;
  UserId    : INTEGER  := Uugid.getuid();
  Username  : TEXT;

PROCEDURE GetNameServer (): TEXT =
  BEGIN
    RETURN nameServer;
  END GetNameServer;

PROCEDURE GetGrasServerId (): TEXT =
  BEGIN
    RETURN grasServer;
  END GetGrasServerId;

PROCEDURE GetRootPrefix (temporary: BOOLEAN): Pathname.T
  RAISES {Unspecified} =
  BEGIN
    IF temporary THEN
      RETURN tempPrefix;

    ELSE
      IF rootPrefix = NIL THEN
        RAISE Unspecified;
      ELSE
        RETURN rootPrefix;
      END;
    END;
  END GetRootPrefix;

PROCEDURE DefaultServerId (): TEXT =
  CONST baseName = "GRAS-3";
  BEGIN
    RETURN (baseName & "." & Fmt.Int(UserId));
  END DefaultServerId;


PROCEDURE GetCacheSize (): CARDINAL =
  BEGIN
    RETURN cacheSize;
  END GetCacheSize;


PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := DefaultCacheSize;
                 nameserver: TEXT         := DefaultNameServer;
                 grasserver: TEXT                               ) =
  <* FATAL Pathname.Invalid *>
  (* never raised for default *)
  VAR
    tempPath: Pathname.Arcs;
    PwdEntry: Upwd.struct_passwd_star;
  BEGIN
    cacheSize := cachesize;
    nameServer := nameserver;
    grasServer := grasserver;

    rootPrefix := root;
    (* try to find environment variable for directory for temporary
       files *)
    tempPrefix := Env.Get(TmpGrasVar);
    IF (tempPrefix = NIL) OR (NOT Pathname.Valid(tempPrefix))
         OR (NOT Pathname.Absolute(tempPrefix)) THEN
      (* try reading passwd-entry for user *)
      PwdEntry := Upwd.getpwuid(UserId);
      IF PwdEntry # NIL AND PwdEntry.pw_name # NIL THEN
        Username := M3toC.CopyStoT(PwdEntry.pw_name);
      ELSE
        Username := "unknown_uid_" & Fmt.Int(UserId);
      END;
      tempPath := Pathname.Decompose(DefaultTempPrefix);
      tempPath.addhi(Username);
      tempPrefix := Pathname.Compose(tempPath);
    END;
  END Login;


BEGIN
END Config.
