INTERFACE VirtualLocalPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.1  1996/02/29 17:44:21  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualLocalPage ---------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT VirtualPage AS Super;
IMPORT
  PageMedia;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      init		(         pageNo	:CARDINAL;
                                  media		:PageMedia.T)
			:T;
    END;

  
END VirtualLocalPage.
