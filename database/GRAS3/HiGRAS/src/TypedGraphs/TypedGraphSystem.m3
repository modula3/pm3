MODULE TypedGraphSystem;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:23:25  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.4  1997/03/24 11:20:43  rbnix
    	Bug fixed in Login: mistook parameters nameServer / grasServer
    	adjusted.

    Revision 1.3  1997/03/21 17:12:08  roland
    Adapted to changed Config. Login parameters are all optional except
    for root directory. Default server name is computed by Config.

    Revision 1.2  1997/03/20 16:54:29  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.1  1997/01/31 10:33:29  roland
    First version of new scheme layer for GRAS_3. Schemes are stored in
    separate graphs. Caches are used for accessing scheme.

*)
(***************************************************************************)

IMPORT ErrorSupport;
IMPORT Pathname;


IMPORT ChgMgmtGraphSystem;
IMPORT TextCursorSet, PageFile;
IMPORT ClientInfoSeq;

(* Implementation of TypedGraphSystem-Interface *)

PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := 0;
                 grasserver: TEXT         := NIL;
                 nameserver: TEXT         := NIL  )
  RAISES {} =
  BEGIN
    ChgMgmtGraphSystem.Login(root, cachesize, grasserver, nameserver);
  END Login;

PROCEDURE DeletePool (baseName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      ChgMgmtGraphSystem.DeletePool(baseName);
    EXCEPT
    | ChgMgmtGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedPoolSystem.DeletePool",
                              "ChgMgmtGraphSystem.InternalError", info));
    END;
  END DeletePool;

PROCEDURE CopyPool (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      ChgMgmtGraphSystem.CopyPool(sourceName, destName);
    EXCEPT
    | ChgMgmtGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedPoolSystem.CopyPool",
                              "ChgMgmtGraphSystem.InternalError", info));
    END;
  END CopyPool;

PROCEDURE RenamePool (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      ChgMgmtGraphSystem.RenamePool(oldName, newName);
    EXCEPT
    | ChgMgmtGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedPoolSystem.RenamePool",
                              "ChgMgmtGraphSystem.InternalError", info));
    END;
  END RenamePool;

PROCEDURE ExistsPool (baseName: Pathname.T): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN ChgMgmtGraphSystem.ExistsPool(baseName);
    EXCEPT
      ChgMgmtGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedPoolSystem.ExistsPool",
                              "ChgMgmtGraphSystem.InternalError", info));
    END;
  END ExistsPool;

PROCEDURE InUse (baseName: Pathname.T): BOOLEAN RAISES {InternalError} =
  BEGIN
    TRY
      RETURN ChgMgmtGraphSystem.PoolInUse(baseName);
    EXCEPT
      ChgMgmtGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedPoolSystem.PoolInUse",
                              "ChgMgmtGraphSystem.InternalError", info));
    END;
  END InUse;

PROCEDURE GetUser (baseName: Pathname.T): ClientInfoSeq.T
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN ChgMgmtGraphSystem.GetPoolUser(baseName);
    EXCEPT
    | ChgMgmtGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedPoolSystem.GetPoolUser",
                              "ChgMgmtGraphSystem.InternalError", info));
    END;
  END GetUser;

PROCEDURE GetPools (): TextCursorSet.T RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      RETURN ChgMgmtGraphSystem.GetPools();
    EXCEPT
      ChgMgmtGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "TypedPoolSystem.GetPools",
                              "ChgMgmtGraphSystem.InternalError", info));
    END;
  END GetPools;

BEGIN
END TypedGraphSystem.
