INTERFACE ObjectList;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.2  1996/03/08 10:19:42  rbnix
    	New method getTail added.

    Revision 1.1  1996/01/31 10:04:42  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- ObjectList ---------------------------------------------------------
 * The abstract data type represents a collection of ObjectListItems
 * organized in a list. ObjectItems may reside mostly in one list.
 * ------------------------------------------------------------------------
 *)
IMPORT BaseObjectList AS Super;

IMPORT
  ObjectListItem;

TYPE
  T                     <: Public;

  Public		= Super.T OBJECT
    METHODS
      init		() :T;

      isEmpty           () :BOOLEAN;
      add		(        prev,
                                 item           :ObjectListItem.T);
      remove		(        item           :ObjectListItem.T);

      movetoHead	(        item		:ObjectListItem.T);
      addHead		(        item		:ObjectListItem.T);
      addTail		(        item		:ObjectListItem.T);
      getTail		() :ObjectListItem.T;
      removeTail	() :ObjectListItem.T;
    END;


(*
 * --- Iterator -----------------------------------------------------------
 * The iterator may used to retrieve all items in list. Beware that the
 * list MUSTEND be when using the iterator. The items are retrieved
 * from first to last using the method next.
 * ------------------------------------------------------------------------
 *)
  Iterator              <: PublicIterator;

  PublicIterator	= OBJECT
    METHODS
      init		(        list		:T) :Iterator;
      next		() :ObjectListItem.T;
    END;

END ObjectList.
