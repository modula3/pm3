MODULE ObjectList;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.2  1996/03/08 10:19:43  rbnix
    	New method getTail added.

    Revision 1.1  1996/01/31 10:04:44  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- ObjectList ---------------------------------------------------------
 * An ObjectList is realized as a cyclic list of double chained items
 * around an invisible anchor item. This is done to unify different item
 * positions in the list.
 * ------------------------------------------------------------------------
 *)


IMPORT
  ObjectListItem, InternalObjectListItem;

REVEAL
  T                     = Public BRANDED OBJECT
      anchor		:ObjectListItem.T := NIL;

    OVERRIDES
      init		:= Init;

      isEmpty		:= IsEmpty;
      add		:= Add;
      remove		:= Remove;

      movetoHead	:= MovetoHead;
      addHead		:= AddHead;
      addTail		:= AddTail;
      getTail		:= GetTail;
      removeTail	:= RemoveTail;
    END;



PROCEDURE Init		(        self		:T) :T =
  BEGIN
    self.anchor := NEW (ObjectListItem.T);
    self.anchor.setPrev (self.anchor);
    self.anchor.setNext (self.anchor);
    self.anchor.setList (self);

    RETURN self;
  END Init;


PROCEDURE IsEmpty       (        self           :T) :BOOLEAN =
  BEGIN
    RETURN (self.anchor.getNext () = self.anchor);
  END IsEmpty;
  

PROCEDURE Add		(        self		:T;
                                 prev,
                                 item           :ObjectListItem.T) =
  BEGIN
    <* ASSERT (self = prev.getList ()) *>
    <* ASSERT (item.getList () = NIL) *>
    WITH next = prev.getNext () DO
      item.setList (self);
      item.setPrev (prev);
      item.setNext (next);
      prev.setNext (item);
      next.setPrev (item);
    END
  END Add;


PROCEDURE Remove	(        self		:T;
                                 item           :ObjectListItem.T) =
  BEGIN
    <* ASSERT (self = item.getList ()) *>
    WITH next = item.getNext (),
         prev = item.getPrev () DO
      item.setList (NIL);
      prev.setNext (next);
      next.setPrev (prev)
    END
  END Remove;
  

PROCEDURE MovetoHead    (        self		:T;
                                 item           :ObjectListItem.T) =
  BEGIN
    IF self.anchor.getNext () = item THEN
      (* nothing to do *)
      
    ELSE
      self.remove (item);
      self.add (self.anchor, item);
    END;
  END MovetoHead;

  
PROCEDURE AddHead	(        self	:T;
                                 item           :ObjectListItem.T) =
  BEGIN
    self.add (self.anchor, item);
  END AddHead;

  
PROCEDURE AddTail	(        self	:T;
                                 item           :ObjectListItem.T) =
  BEGIN
    self.add (self.anchor.getPrev (), item);
  END AddTail;
  

PROCEDURE GetTail	(        self	:T) :ObjectListItem.T =
  BEGIN
    <* ASSERT (NOT (self.isEmpty ())) *>
    WITH last = self.anchor.getPrev () DO
      RETURN last;
    END
  END GetTail;


PROCEDURE RemoveTail    (        self	:T) :ObjectListItem.T =
  BEGIN
    <* ASSERT (NOT (self.isEmpty ())) *>
    WITH last = self.anchor.getPrev () DO
      self.remove (last);
      RETURN last;
    END
  END RemoveTail;
  

(*
 * --- Iterator -----------------------------------------------------------
 *)
REVEAL
  Iterator		= PublicIterator BRANDED OBJECT
      list		:T := NIL;
      item		:ObjectListItem.T := NIL;

    OVERRIDES
      init		:= InitIterator;
      next		:= NextItem;
    END;


PROCEDURE InitIterator	(        self		:Iterator;
                                 list		:T) :Iterator =
  BEGIN
    self.list := list;
    self.item := list.anchor;

    RETURN self;
  END InitIterator;


PROCEDURE NextItem	(        self		:Iterator) :ObjectListItem.T =
  BEGIN
    <* ASSERT (self.item # NIL) *>

    self.item := self.item.getNext ();
    IF self.item = self.list.anchor THEN
      RETURN NIL
    ELSE
      RETURN self.item
    END
  END NextItem;
  

BEGIN
END ObjectList.
