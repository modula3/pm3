INTERFACE NumberedPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1996/10/07 13:31:42  rbnix
    	New module for numbered pages.

*)
(***************************************************************************)
(*
 | --- NumberedPage -------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT Page AS Super;
IMPORT
  PageData;


CONST
  Brand			= "NumberedPage";

TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      init		(         number	:CARDINAL;
                         READONLY data		:PageData.T)
			:T;

      getNumber		() :CARDINAL;
    END;


END NumberedPage.
