MODULE Termination;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/03/01 12:56:45  rbnix
    	Format function(s) added.

*)
(***************************************************************************)
(*
 | --- Termination --------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)

PROCEDURE FmtMode	(         mode		:Mode) :TEXT =
  BEGIN
    CASE mode OF
    | Mode.Try =>
      RETURN "Termination.Mode.Try";
        
    | Mode.Strikt =>
      RETURN "Termination.Mode.Strikt";
    END
  END FmtMode;


BEGIN
END Termination.
