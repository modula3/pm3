MODULE ChgMgmtGraphSystem;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/04/23 14:09:33  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

    Revision 1.5  1997/03/24 11:19:50  rbnix
    	Bug fixed in Login: mistook parameters nameServer / grasServer
    	adjusted.

    Revision 1.4  1997/03/21 17:10:22  roland
    Adapted to changed Config. Login parameters are all optional except
    for root directory. Default server name is computed by Config.

    Revision 1.3  1997/03/20 16:53:55  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.2  1997/01/31 10:19:44  roland
    Minor corrections in exception handling.

    Revision 1.1  1996/12/23 10:34:58  roland
    Implementation of ChgMgmtGraphSystem separated from ChgMgmtGraph.

*)
(***************************************************************************)

IMPORT Pathname, TextCursorSet, PageFile,
       PersistentGraphSystem, ErrorSupport, ClientInfoSeq;


PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := 0;
                 grasserver: TEXT         := NIL;
                 nameserver: Pathname.T   := NIL  ) =
  BEGIN
    PersistentGraphSystem.Login(root, cachesize, grasserver, nameserver);
  END Login;

PROCEDURE DeletePool (baseName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      PersistentGraphSystem.DeletePool(baseName);
    EXCEPT
    | PersistentGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.DeletePool",
                              "PersistentGraphSystem.InternalError", info));
    END;
  END DeletePool;


PROCEDURE CopyPool (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      PersistentGraphSystem.CopyPool(sourceName, destName);
    EXCEPT
    | PersistentGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.CopyPool",
                              "PersistentGraphSystem.InternalError", info));
    END;
  END CopyPool;


PROCEDURE RenamePool (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      PersistentGraphSystem.RenamePool(oldName, newName);
    EXCEPT
    | PersistentGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.RenamePool",
                              "PersistentGraphSystem.InternalError", info));
    END;
  END RenamePool;


PROCEDURE ExistsPool (baseName: Pathname.T): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN PersistentGraphSystem.ExistsPool(baseName);
    EXCEPT
    | PersistentGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.ExistsPool",
                              "PersistentGraphSystem.InternalError", info));
    END;
  END ExistsPool;


PROCEDURE PoolInUse (baseName: Pathname.T): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN PersistentGraphSystem.PoolInUse(baseName);
    EXCEPT
    | PersistentGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.PoolInUse",
                              "PersistentGraphSystem.InternalError", info));
    END;
  END PoolInUse;


PROCEDURE GetPoolUser (baseName: Pathname.T): ClientInfoSeq.T
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN PersistentGraphSystem.GetPoolUser(baseName);
    EXCEPT
    | PersistentGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.GetPoolUser",
                              "PersistentGraphSystem.InternalError", info));
    END;
  END GetPoolUser;


PROCEDURE GetPools (): TextCursorSet.T
  RAISES {PageFile.NoAccess, InternalError} =
  BEGIN
    TRY
      RETURN PersistentGraphSystem.GetPools();
    EXCEPT
    | PersistentGraphSystem.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.GetPools",
                              "PersistentGraphSystem.InternalError", info));
    END;
  END GetPools;


BEGIN
END ChgMgmtGraphSystem.
