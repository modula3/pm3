MODULE Access;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/03/01 12:56:40  rbnix
    	Format function(s) added.

*)
(***************************************************************************)
(*
 | --- Access -------------------------------------------------------------
 
 | ------------------------------------------------------------------------
 *)

PROCEDURE FmtMode	(         mode		:Mode) :TEXT =
  BEGIN
    CASE mode OF
    | Mode.ReadOnlyShared =>
      RETURN "Access.Mode.ReadOnlyShared";
      
    | Mode.ReadWriteShared =>
      RETURN "Access.Mode.ReadWriteShared";
      
    | Mode.ReadWriteExclusive =>
      RETURN "Access.Mode.ReadWriteExclusive";
    END
  END FmtMode;
  

PROCEDURE FmtKind	(         kind		:Kind) :TEXT =
  BEGIN
    CASE kind OF
    | Kind.Data =>
      RETURN "Access.Kind.Data";
      
    | Kind.Log =>
      RETURN "Access.Kind.Log";
    END
  END FmtKind;

BEGIN
END Access.
