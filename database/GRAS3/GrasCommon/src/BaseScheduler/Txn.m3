MODULE Txn;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/04/08 21:56:43  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

    Revision 1.1  1996/03/01 12:56:47  rbnix
    	Format function(s) added.

*)
(***************************************************************************)
(*
 | --- Txn ----------------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)


PROCEDURE FmtEnd	(         end		:End) :TEXT =
  BEGIN
    CASE end OF
    | End.No =>
      RETURN "Txn.End.No";
      
    | End.Commit =>
      RETURN "Txn.End.Commit";
      
    | End.Abort =>
      RETURN "Txn.End.Abort";

    | End.Chain =>
      RETURN "Txn.End.Chain";

    END
  END FmtEnd;
  

BEGIN
END Txn.
