INTERFACE ScheduledClientFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.3  1997/04/24 12:12:33  roland
    Added parameter (access) mode for opening a remote file. If a resource
    is opened in ReadWriteExclusive or ReadOnlyShared, the access modes of
    its files have to be identical to that. If a resource is opened as
    ReadWriteShared, files might have any of the three access modes.

    Revision 1.2  1996/11/18 17:51:45  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.1  1996/02/09 16:46:53  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)


(*
 | --- ScheduledClientFile ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)

IMPORT BaseScheduledClientFile AS Super;
IMPORT
  BaseScheduledClientRessource,
  ScheduledClientPage,
  Access,
  PageFile;
IMPORT
  Pathname,
  AtomList;

CONST
  Brand			= "ScheduledClientFile";

TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      open		(         ressource	:BaseScheduledClientRessource.T;
                                  baseName	:Pathname.T;
                                  mode          :Access.Mode;
                                  kind		:Access.Kind;
                                  new		:BOOLEAN)
			:Super.T
			RAISES {Access.Denied, PageFile.NoAccess, FatalError};

      close		() RAISES {FatalError};

      getPage           (         pageNo        :CARDINAL) :ScheduledClientPage.T;
    END;


EXCEPTION
  FatalError(AtomList.T);
  
END ScheduledClientFile.
