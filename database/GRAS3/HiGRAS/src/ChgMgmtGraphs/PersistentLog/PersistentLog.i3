INTERFACE PersistentLog;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.4  1998/05/19 10:17:53  roland
    Support for log-groups implemented.

    Revision 1.3  1998/03/17 14:14:01  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.2  1997/04/24 14:30:26  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:19  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.5  1997/02/04 11:15:36  roland
    It is now possible to disable logging in ChgMgmtGraph completely.

    Revision 1.4  1996/11/20 12:21:14  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/11/14 14:17:07  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Access in mode ReadOnlyShared is now considered when opening graphs.

    Revision 1.2  1996/09/20 13:59:09  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:58:04  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT Log AS Super;
IMPORT PageFile, Access, VirtualResource;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = Super.T OBJECT
           METHODS
             open (resource: VirtualResource.T;
                   path    : Pathname.T;
                   access  : Access.Mode;
                   new     : BOOLEAN;
                   local   : BOOLEAN            ) : T
                   RAISES {Access.Locked, PageFile.NoAccess,
                           Super.InternalError, Access.Denied};
                   (* Open a persistent log. *)

             close (resource: VirtualResource.T;
                    path    : Pathname.T;
                    delete  : BOOLEAN;
                    local   : BOOLEAN            )
                    RAISES {Super.InternalError};
                    (* Close the log. *)
           END;

PROCEDURE DeleteOldLogFiles ( res   : VirtualResource.T; 
                              path  : Pathname.T;
                              local : BOOLEAN            )
  RAISES {Super.InternalError, PageFile.NoAccess};

PROCEDURE LogFilesExist ( res   : VirtualResource.T; 
                          path  : Pathname.T;
                          local : BOOLEAN            ) : BOOLEAN 
                         RAISES {Super.InternalError};

PROCEDURE LogFilesInUse ( res   : VirtualResource.T; 
                          path  : Pathname.T;
                          local : BOOLEAN            ) : BOOLEAN 
                         RAISES {Super.InternalError};

END PersistentLog.
