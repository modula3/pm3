INTERFACE ScheduledClientPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

    Revision 1.2  1996/11/18 17:51:47  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.1  1996/02/09 16:46:57  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- ScheduledClientPage ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageHandle,
  Access;
IMPORT
  AtomList;

CONST
  Brand			= "ScheduledClientPage";

TYPE
  T			<: Public;

  Public		= <*TRANSIENT*> ROOT OBJECT
    METHODS
      peekAccess	() :PageHandle.T
			RAISES {FatalError};
      
      readAccess	(VAR pageAge: CARDINAL) :PageHandle.T
			RAISES {Access.Locked, FatalError};
      
      writeAccess	(VAR pageAge: CARDINAL) :PageHandle.T
			RAISES {Access.Locked, FatalError};
    END;

EXCEPTION
  FatalError(AtomList.T);

END ScheduledClientPage.
