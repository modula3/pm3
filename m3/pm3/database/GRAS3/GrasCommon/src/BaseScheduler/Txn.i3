INTERFACE Txn;

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

    Revision 1.2  1996/03/01 12:56:46  rbnix
    	Format function(s) added.

    Revision 1.1  1996/02/09 16:03:05  rbnix
    	First version of common used scheduler parts.

*)
(***************************************************************************)


(*
 | --- Txn --------------------------------------------------------
  This type collection module reveals some common information about
  transactions.

  User transactions starts with TopLevel. Every begin transaction increases
  the transaction level by 1. If no user transactions are pending the
  transaction level is set to EnvelopeLevel.
 | ------------------------------------------------------------------------
*)


CONST
  EnvelopeLevel		= 0;
  TopLevel		= 1;

  
TYPE
  Level			= [EnvelopeLevel .. LAST (INTEGER)];

  End			= {No,			(* continue		*)
                           Commit,		(* propagate changes	*)
                           Chain,		(* propagate changes and continue *)
                           Abort};		(* forget changes	*)


PROCEDURE FmtEnd	(         end		:End) :TEXT;
  

END Txn.
