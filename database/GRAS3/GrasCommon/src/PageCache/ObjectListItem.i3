INTERFACE ObjectListItem;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/01/31 10:04:45  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- ObjectListItem -----------------------------------------------------
 * Abstract data type of an entry storable in an ObjectList list. 
 * ------------------------------------------------------------------------
 *)

IMPORT
  BaseObjectList;


TYPE
  T			<: Public;

  Public                = OBJECT
    METHODS
      getList		() :BaseObjectList.T;
    END;
  

END ObjectListItem.
