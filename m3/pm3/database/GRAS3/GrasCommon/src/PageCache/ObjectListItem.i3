INTERFACE ObjectListItem;

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

  Public                = <*TRANSIENT*> ROOT OBJECT
    METHODS
      getList		() :BaseObjectList.T;
    END;
  

END ObjectListItem.
