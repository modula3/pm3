INTERFACE VirtualResourceSystem;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.7  1997/03/21 17:04:47  roland
    Adapted to changed Config. Login parameters are all optional except
    for root directory. Default server name is computed by Config.

    Revision 1.6  1997/03/20 16:55:06  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.5  1996/11/21 15:22:53  roland
    System parameters will not be read from command-line by the core
    system. Instead they must be supplied to Config.Login. This can be
    done with VirtualResourceSystem.Login and
    PersistentGraphSystem.Login.

    Revision 1.4  1996/11/21 07:54:49  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.3  1996/11/18 17:52:27  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.2  1996/08/06 16:34:23  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/08/01 18:14:02  rbnix
        New module to administrate resources in whole added.

*)
(***************************************************************************)
(*
 | --- VirtualResourceSystem ----------------------------------------------
 This abstract data object module manages resources. Resources must be closed
 to be modified!

 Currently only the persistent = remote part of resources is served.
 | ------------------------------------------------------------------------
 *)
IMPORT Pathname, TextSeq, AtomList, PageFile, ClientInfoSeq;


PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := 0;
                 grasserver: TEXT         := NIL;
                 nameserver: TEXT         := NIL  );
  (* Supply basic system parameters.  A call to Login is mandatory before
     any operations can be performed.  Trying to open a resource without
     Login will result in a PageFile.NoAccess complaining about this. *)

PROCEDURE DeleteResource (baseName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError};

PROCEDURE CopyResource (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError};

PROCEDURE RenameResource (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess, FatalError};

PROCEDURE ExistsResource (baseName: Pathname.T): BOOLEAN
  RAISES {FatalError};

PROCEDURE ResourceInUse (baseName: Pathname.T): BOOLEAN
  RAISES {FatalError};

PROCEDURE GetResourceUser (baseName: Pathname.T): ClientInfoSeq.T
  RAISES {FatalError};

PROCEDURE GetResources (): TextSeq.T
  RAISES {PageFile.NoAccess, FatalError};

EXCEPTION FatalError(AtomList.T);

END VirtualResourceSystem.
