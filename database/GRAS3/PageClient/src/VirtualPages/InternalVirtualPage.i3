INTERFACE InternalVirtualPage;

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

    Revision 1.2  1996/11/18 17:52:15  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.1  1996/02/29 17:44:13  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- InternalVirtualPage ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageHandle,
  Access,
  VirtualPage;


REVEAL
  VirtualPage.T		<: Internal;

TYPE
  Internal		= VirtualPage.Public OBJECT
    METHODS
      peekAccess	()
			:PageHandle.T
			RAISES {VirtualPage.FatalError}
			:= NIL;
      readAccess	(VAR pageAge: CARDINAL)
			:PageHandle.T
			RAISES {Access.Locked, VirtualPage.FatalError}
			:= NIL;
      
      writeAccess	(VAR pageAge: CARDINAL)
			:PageHandle.T
			RAISES {Access.Locked, VirtualPage.FatalError}
			:= NIL;
    END;
  

END InternalVirtualPage.
