INTERFACE BufferedPageFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1996/10/07 13:32:57  rbnix
    	New module for a asynchronous buffered file.

*)
(***************************************************************************)
(*
 | --- BufferedPageFile ---------------------------------------------------
 This abstract data type represents a specialized PageFile where the access
 operations work asynchronous. It is assured that put data will be stored
 in a way sequentialized by flush operations (not neccessarily between
 them). Regard that flush operations are now used to order the data
 transfer not more.
 | ------------------------------------------------------------------------
 *)

IMPORT PageFile AS Super;

TYPE
  T			<: Public;

  Public		= Super.T OBJECT
      (* nothing more *)
    END;

END BufferedPageFile.
