INTERFACE InternalPageHandle;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/03/08 10:29:48  rbnix
    	PageHandles are now tagged by an internal id. Therefore the
    	method init is introduced and the method fmt is enhanced.

*)
(***************************************************************************)
(*
 | --- InternalPageHandle -------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageHandle;


REVEAL
  PageHandle.T		<: Internal;

TYPE
  Internal		= PageHandle.Public OBJECT
    METHODS
      init		() :PageHandle.T;
    END;


END InternalPageHandle.
