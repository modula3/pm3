INTERFACE ScheduledClientRessource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.6  1997/10/31 14:13:27  roland
    Adapted to new RuleEngine.

    Revision 1.5  1997/04/24 12:12:35  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.4  1996/11/18 17:51:50  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.3  1996/09/09 11:39:35  rbnix
    	Generation of resource events on begin/end of transaction
    	added.

    Revision 1.2  1996/03/11 17:21:31  rbnix
    	Method closeRemoteFile added to close file and delete entry
    	out of collection.

    Revision 1.1  1996/02/09 16:47:00  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- ScheduledClientRessource -------------------------------------------
 In this data type resource events are raised within the visible methods
 startTransaction, commitTransaction and abortTransaction. Such events are
 related to own transaction handling. Within an invisible method 
 resource events are raised when an other transaction is commited. These
 events are raised asynchonous in a seperate thread! 
 | ------------------------------------------------------------------------
 *)
IMPORT BaseScheduledClientRessource AS Super;
IMPORT
  AtomList,
  Pathname,
  PageFile,
  Access,
  ScheduledClientFile;

TYPE
  Notifier = OBJECT
             METHODS
               notify() := NIL;
             END;

TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      (* resource administration *)
      init		(         baseName	:Pathname.T;
                                  access	:Access.Mode;
                                  new		:BOOLEAN;
                                  notifier      :Notifier) :Super.T
			RAISES {Access.Denied, PageFile.NoAccess, FatalError};

      close		()
			RAISES {FatalError};

      openRemoteFile	(         baseName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:ScheduledClientFile.T
			RAISES {Access.Denied, PageFile.NoAccess, FatalError};

      closeRemoteFile	(         file		:ScheduledClientFile.T)
      RAISES {FatalError};

      (* transaction support *)
      startTransaction	() RAISES {FatalError};
      commitTransaction	() RAISES {NotInTransaction, FatalError};
      abortTransaction	() RAISES {NotInTransaction, FatalError};

    END;

EXCEPTION
  FatalError(AtomList.T);
  NotInTransaction;
  
END ScheduledClientRessource.
