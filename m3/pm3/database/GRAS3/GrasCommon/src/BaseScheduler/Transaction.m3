MODULE Transaction;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/03/01 12:56:47  rbnix
    	Format function(s) added.

*)
(***************************************************************************)
(*
 | --- Transaction --------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)


PROCEDURE FmtEnd	(         end		:End) :TEXT =
  BEGIN
    CASE end OF
    | End.No =>
      RETURN "Transaction.End.No";
      
    | End.Commit =>
      RETURN "Transaction.End.Commit";
      
    | End.Abort =>
      RETURN "Transaction.End.Abort";
    END
  END FmtEnd;
  

BEGIN
END Transaction.
