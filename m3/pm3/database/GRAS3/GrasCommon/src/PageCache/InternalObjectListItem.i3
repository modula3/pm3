INTERFACE InternalObjectListItem;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/01/31 10:04:36  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- InternalObjectListItem ---------------------------------------------
 * Subsystem internal interface to an list item.
 *
 * List items are related to a list by chaining variables stored at the
 * list items itself.
 * ------------------------------------------------------------------------
 *)

IMPORT
  BaseObjectList, ObjectListItem;

REVEAL
  ObjectListItem.T	<: Internal;

TYPE
  Internal		= ObjectListItem.Public OBJECT
    METHODS
      setNext		(        next		:ObjectListItem.T);
      getNext		() :ObjectListItem.T;

      setPrev		(        prev		:ObjectListItem.T);
      getPrev		() :ObjectListItem.T;

      setList		(        list		:BaseObjectList.T);
    END;

END InternalObjectListItem.
