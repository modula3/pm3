INTERFACE PageData;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

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

IMPORT Type, RTHeapDep;


CONST
  Size			= RTHeapDep.BytesPerPage; (* no of items in a page *)


TYPE
  Index			= [1 .. Size];		(* index range of items	*)
  Item			= Type.Byte;		(* atomic data item	*)
  Part                  = ARRAY OF Item;	(* some data items	*)
  Swizzler		= PROCEDURE(READONLY data: T);
  Unswizzler		= PROCEDURE(VAR data: T);

  T			= ARRAY Index OF Item;	(* page data		*)


END PageData.
