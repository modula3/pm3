INTERFACE TypedGraphSystem;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:23:24  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.3  1997/03/21 17:12:05  roland
    Adapted to changed Config. Login parameters are all optional except
    for root directory. Default server name is computed by Config.

    Revision 1.2  1997/03/20 16:54:28  renehuel
    These files were changed to use the new gras nameserver.
    They have to explicitly choose the grasserver from which they
    want to be served.
    This is done via the login method which has now one more parameter,
    the id of the desired gras-server

    Revision 1.1  1997/01/31 10:33:27  roland
    First version of new scheme layer for GRAS_3. Schemes are stored in
    separate graphs. Caches are used for accessing scheme.

*)
(***************************************************************************)


(* This abstract data object enhances ChgMgmtGraphSystem. *)

IMPORT Pathname, TextCursorSet, PageFile, ClientInfoSeq;
IMPORT AtomList;


PROCEDURE Login (root      : Pathname.T;
                 cachesize : CARDINAL     := 0;
                 grasserver: TEXT         := NIL;
                 nameserver: TEXT         := NIL  );
  (* Supply basic system parameters.  A call to Login is mandatory before
     any operations can be performed.  Trying to open a graph without Login
     will result in a PageFile.NoAccess complaining about this.  If not
     specified, cachesize and nameserver will be set to system defaults
     (see Config.i3). *)


PROCEDURE DeletePool (baseName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError};
  (* Deletion of the a graph in the administration.  It is possible, that
     this will not result in physical deletion of the graph representing
     files, because these will be necessary to reconstruct other (indirect)
     graphs. *)

PROCEDURE CopyPool (sourceName: Pathname.T; destName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError};
  (* Create a copy of the source graph.  This operation will fail if the
     destination name points to an already existing graph. *)

PROCEDURE RenamePool (oldName: Pathname.T; newName: Pathname.T)
  RAISES {PageFile.NoAccess, InternalError};
  (* Changes the graph to be refered by oldName to be found now as newName.
     This operation will fail if newName is an already existing name. *)

PROCEDURE ExistsPool (baseName: Pathname.T): BOOLEAN
  RAISES {InternalError};
  (* Tests if the specified graph exists. *)

PROCEDURE InUse (baseName: Pathname.T): BOOLEAN RAISES {InternalError};
  (* Tests if the specified graph/scheme is already in use by any
     client. *)

PROCEDURE GetUser (baseName: Pathname.T): ClientInfoSeq.T
  RAISES {InternalError};

PROCEDURE GetPools (): TextCursorSet.T RAISES {PageFile.NoAccess, InternalError};
  (* Returns a name list of all managed graphs. *)


EXCEPTION
  InternalError(AtomList.T);
  NoScheme;

END TypedGraphSystem.
