INTERFACE Journal;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/02/28 14:12:07  rbnix
    	First version of message journal (protocol) storage.

*)
(***************************************************************************)

(*
 | --- Journal -----------------------------------------------------------
 This abstract data object represents a storage for journal messages.
 | ------------------------------------------------------------------------
 *)

PROCEDURE Add			(         message	:TEXT);
  (*
    Adds a message to the journal.
  *)


END Journal.
