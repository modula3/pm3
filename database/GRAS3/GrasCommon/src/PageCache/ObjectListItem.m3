MODULE ObjectListItem EXPORTS ObjectListItem, InternalObjectListItem;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/01/31 10:04:47  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

IMPORT
  BaseObjectList;


REVEAL
  T                     = Internal BRANDED OBJECT
      next, prev	:T;
      list		:BaseObjectList.T
			:= NIL;

    OVERRIDES
      setNext		:= SetNext;
      getNext		:= GetNext;

      setPrev		:= SetPrev;
      getPrev		:= GetPrev;

      setList		:= SetList;
      getList		:= GetList;
    END;


PROCEDURE SetNext	(        self		:T;
                                 next		:T) =
  BEGIN
    self.next := next;
  END SetNext;


PROCEDURE GetNext	(        self		:T) :T =
  BEGIN
    RETURN self.next;
  END GetNext;
  

PROCEDURE SetPrev	(        self		:T;
                                 prev		:T) =
  BEGIN
    self.prev := prev;
  END SetPrev;


PROCEDURE GetPrev	(        self		:T) :T =
  BEGIN
    RETURN self.prev;
  END GetPrev;


PROCEDURE SetList	(        self		:T;
                                 list		:BaseObjectList.T) =
  BEGIN
    self.list := list;
  END SetList;


PROCEDURE GetList	(        self           :T) :BaseObjectList.T =
  BEGIN
    RETURN self.list;
  END GetList;


BEGIN
END ObjectListItem.
