INTERFACE PageData;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/01/31 10:02:12  rbnix
    	Initial version for subsystem Page.

*)
(***************************************************************************)

(*
 * --- PageData -----------------------------------------------------------
 * This concrete data type module contains common information about
 * data residing on pages.
 * ------------------------------------------------------------------------
 *)

IMPORT Type;


CONST
  Size			= 8192;			(* no of items in a page *)


TYPE
  Index			= [1 .. Size];		(* index range of items	*)
  Item			= Type.Byte;		(* atomic data item	*)
  Part                  = ARRAY OF Item;	(* some data items	*)

  T			= ARRAY Index OF Item;	(* page data		*)


END PageData.
