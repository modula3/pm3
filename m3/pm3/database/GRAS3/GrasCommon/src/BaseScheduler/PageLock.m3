MODULE PageLock;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/03/01 12:56:43  rbnix
    	Format function(s) added.

*)
(***************************************************************************)
(*
 | --- PageLock -----------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)

PROCEDURE FmtMode	(         mode		:Mode) :TEXT =
  BEGIN
    CASE mode OF
    | Mode.X =>
      RETURN "PageLock.Mode.X";
      
    | Mode.C =>
      RETURN "PageLock.Mode.C";
      
    | Mode.P =>
      RETURN "PageLock.Mode.P";
      
    | Mode.O =>
      RETURN "PageLock.Mode.O";
      
    | Mode.S =>
      RETURN "PageLock.Mode.S";
    END
  END FmtMode;


BEGIN
END PageLock.
