INTERFACE VirtualRemotePage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.1  1996/02/29 17:44:29  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualRemotePage --------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT VirtualPage AS Super;
IMPORT
  ScheduledClientPage;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      init		(         scheduledPage	:ScheduledClientPage.T)
			:T;
    END;
  

END VirtualRemotePage.
