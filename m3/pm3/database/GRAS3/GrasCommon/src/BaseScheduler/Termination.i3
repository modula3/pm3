INTERFACE Termination;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.2  1996/03/01 12:56:44  rbnix
    	Format function(s) added.

    Revision 1.1  1996/02/21 13:42:30  rbnix
    	New type collection module Termination added to distuingish
    	different shutdown modes.

*)
(***************************************************************************)

(*
 | --- Termination --------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)

EXCEPTION
  StillInUse;


TYPE
  Mode			= {Try,			(* check for other clients	*)
                           (* Request, *)	(* request other clients	*)
                           Strikt};		(* take no care of other clients*)

PROCEDURE FmtMode	(         mode		:Mode) :TEXT;

  
END Termination.
