GENERIC INTERFACE CursorList(Element);

(***************************************************************************)
(* This module implements a list of elements of type Element.T.  It
   requires Element to export a comparision functions Compare and Equal. *)
(***************************************************************************)
(** Created by:  Peter Klein                                               *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:43  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:26  hosking
    Import of GRAS3 1.1

    Revision 1.4  1996/08/06 16:22:33  roland
    Merge of PAGESERVER and main branch.

# Revision 1.3  1994/03/30  17:18:01  pk
# New shape for generic parameters according to 3.1.
#
# Revision 1.2  1993/10/31  18:08:51  pk
# InsertBefore/BehindCursor won't crash if list is empty and no cursor
# is defined.
#
# Revision 1.1  1993/10/23  19:24:48  pk
# Initial revision
#
*)
(***************************************************************************)


EXCEPTION
  NotInitialized;                (* The list was not initialized with
                                    Init. *)
  CursorError;                   (* The cursor position is corrupt. *)


TYPE
  T <: Public;

  Public =
    <*TRANSIENT*> ROOT OBJECT
    METHODS
      (**********************************************************************)
      (**                                                                   *)
      (**                      Organization                                 *)
      (**                                                                   *)
      (**********************************************************************)

      init (): T;
            (* Initialize a list created by a NEW(T) command.  After
               initialization, the list is empty and the cursor position is
               undefined. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Queries                                      *)
      (**                                                                   *)
      (**********************************************************************)

      length (): CARDINAL RAISES {NotInitialized};
              (* Determine the number of elements in the list. *)


      atTop (): BOOLEAN RAISES {NotInitialized};
             (* Yields TRUE if the cursor is positioned at the first
                element of the list.  atTop is TRUE on an empty list. *)


      atBottom (): BOOLEAN RAISES {NotInitialized};
                (* Yields TRUE if the cursor is positioned at the last
                   element of the list.  atBottom is TRUE on an empty
                   list. *)


      cursorPosition (): CARDINAL RAISES {NotInitialized};
                      (* Determine the current cursor position.  If the
                         cursor is undefined, 0 is returned.  With a
                         defined cursor, the values will range from 1 to
                         length. *)


      isEmpty (): BOOLEAN RAISES {NotInitialized};
               (* Yields TRUE if the list is empty. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Manipulate Cursor                            *)
      (**                                                                   *)
      (**********************************************************************)

      gotoTop () RAISES {NotInitialized};
               (* Set the cursor to the first element in the list.  If the
                  list is empty, the cursor will remain undefined. *)


      gotoBottom () RAISES {NotInitialized};
                  (* Set the cursor to the last element in the list.  If
                     the list is empty, the cursor will remain
                     undefined. *)


      gotoNext () RAISES {NotInitialized};
                (* Advance the cursor one position.  If the list is empty
                   or the cursor undefined, the cursor will remain
                   undefined.  If the cursor was on the last element, the
                   position will be set to undefined. *)


      gotoPrevious () RAISES {NotInitialized};
                    (* Set back the cursor one position.  If the list is
                       empty or the cursor undefined, the cursor will
                       remain undefined.  If the cursor was on the first
                       element, the position will be set to undefined. *)


      gotoElement (READONLY data: Element.T; VAR found: BOOLEAN)
                   RAISES {NotInitialized};
                   (* Advance the cursor to the next element which is equal
                      to data (where equality is defined by Element.Eq).
                      The search will start at the current element (not
                      after) and proceed in forward direction.  It will
                      wrap at the list end.  If no appropriate element is
                      found, the cursor remains unchanged and found will be
                      FALSE. *)


      gotoPosition (position: CARDINAL)
                    RAISES {NotInitialized, CursorError};
                    (* Set the cursor to the specified position.  If this
                       position exceeds the range 1..length, CursorError
                       will be raised. *)


      step (steps: INTEGER) RAISES {NotInitialized, CursorError};
            (* Move the cursor relative to the current position.  Positive
               values of steps will advance, negative values will set back
               the cursor.  Stepping outside the list as well as calling
               this method with an undefined cursor will raise
               CursorError. *)


      cyclicStep (steps: INTEGER) RAISES {NotInitialized, CursorError};
                  (* Move the cursor relative to the current position.
                     Positive values of steps will advance, negative values
                     will set back the cursor.  The cursor movement will
                     wrap at the list ends.  Calling this method with an
                     undefined cursor will raise CursorError. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Delete                                       *)
      (**                                                                   *)
      (**********************************************************************)

      clear () RAISES {NotInitialized};
             (* Delete all elements in the list.  This will not delete the
                list itself, but the cursor position will be undefined
                hereafter. *)


      deleteCurrent () RAISES {NotInitialized, CursorError};
                     (* Delete the element at the cursor position.  For the
                        cursor handling after deletion see deleteElement.
                        CursorError is raised if there is no current
                        cursor. *)


      deleteElement (READONLY data: Element.T; VAR found: BOOLEAN)
                     RAISES {NotInitialized};
                     (* Search the first element in the list which is equal
                        to data (see gotoElement for details) and delete
                        it.  If no such element can be found, found will
                        return FALSE and the cursor position will remain
                        unchanged.  If the element is deleted, the cursor
                        will be set to the successor of the element.  If
                        there is no successor, it will be set to the
                        predecessor.  If the list is empty, the cursor
                        becomes undefined.  If the cursor was undefined, it
                        will be set to the first list position.  . *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Element Queries                              *)
      (**                                                                   *)
      (**********************************************************************)

      extractCurrent (VAR found: BOOLEAN): Element.T
                      RAISES {NotInitialized};
                      (* Get the current list element and delete it.  If
                         the cursor is undefined, found will return FALSE
                         and an arbitrary value of Element.T will be
                         returned.  For the cursor handling after deletion
                         see deleteElement. *)


      getCurrent (VAR found: BOOLEAN): Element.T RAISES {NotInitialized};
                  (* Get the current list element.  If the cursor is
                     undefined, found will return FALSE and an arbitrary
                     value of Element.T will be returned.  Otherwise, the
                     cursor position will remain unchanged. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Replace                                      *)
      (**                                                                   *)
      (**********************************************************************)

      replaceCurrent (READONLY data: Element.T)
                      RAISES {NotInitialized, CursorError};
                      (* Replace the element at the cursor position.  The
                         cursor will remain unchanged.  If the cursor is
                         undefined, CursorError will be raised. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Insert                                       *)
      (**                                                                   *)
      (**********************************************************************)

      insertAtTop (READONLY data: Element.T) RAISES {NotInitialized};
                   (* Insert data at the top of the list.  After insertion,
                      the cursor will be on the new element. *)


      insertAtBottom (READONLY data: Element.T) RAISES {NotInitialized};
                      (* Insert data at the bottom of the list.  After
                         insertion, the cursor will be on the new
                         element. *)


      insertBeforeCursor (READONLY data: Element.T)
                          RAISES {NotInitialized, CursorError};
                          (* Insert data before the cursor.  After
                             insertion, the cursor will be on the new
                             element.  CursorError is raised if the list is
                             not empty and no cursor is defined. *)


      insertBehindCursor (READONLY data: Element.T)
                          RAISES {NotInitialized, CursorError};
                          (* Insert data behind the cursor.  After
                             insertion, the cursor will be on the new
                             element.  CursorError is raised if the list is
                             not empty and no cursor is defined. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Sorted Lists                                 *)
      (**                                                                   *)
      (**********************************************************************)

      sort (ascending: BOOLEAN) RAISES {NotInitialized};
            (* Sort the list ascending or descending.  Element.Eq and
               Element.Lt are used for establishing the order.  After
               sorting, the cursor will be on the first element or
               undefined, if the list is empty. *)


      insertSorted (READONLY data: Element.T; ascending: BOOLEAN)
                    RAISES {NotInitialized};
                    (* Insert data into the list.  If the list is sorted
                       with the same value of ascending, the element will
                       be inserted respecting the order.  After insertion,
                       the cursor will be on the new element. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Sublists                                     *)
      (**                                                                   *)
      (**********************************************************************)

      removeRange (position1, position2: CARDINAL)
                   RAISES {NotInitialized, CursorError};
                   (* Delete all elements from position1 to position2
                      including.  position1 may be less than, equal to or
                      greater than position2.  If one or both are outside
                      the list, CursorError is raised.  For the cursor
                      handling after deletion see deleteElement. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Swap                                         *)
      (**                                                                   *)
      (**********************************************************************)

      exchangeCurrentWithNext () RAISES {NotInitialized, CursorError};
                               (* Swap the current element and the next
                                  element.  Hereafter, the cursor will be
                                  at the same absolute position (i.e.
                                  pointing to the former successor).  If
                                  the cursor is undefined or no successor
                                  exists, CursorError will be raised. *)


      exchangeCurrentWithPrevious () RAISES {NotInitialized, CursorError};
                                   (* Swap the current element and the
                                      previous element.  Hereafter, the
                                      cursor will be at the same absolute
                                      position (i.e.  pointing to the
                                      former predecessor).  If the cursor
                                      is undefined or no predecessor
                                      exists, CursorError will be
                                      raised. *)


      exchangePosition (position1, position2: CARDINAL)
                        RAISES {NotInitialized, CursorError};
                        (* Swap arbitrary list elements.  The cursor will
                           be set to position1. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Move                                         *)
      (**                                                                   *)
      (**********************************************************************)

      moveCurrentToTop () RAISES {NotInitialized, CursorError};
                        (* Move the current cursor element to the top of
                           the list.  If the cursor is undefined,
                           CursorError will be raised.  Otherwise, the
                           cursor will point to the new top. *)


      moveCurrentToBottom () RAISES {NotInitialized, CursorError};
                           (* Move the current cursor element to the bottom
                              of the list.  If the cursor is undefined,
                              CursorError will be raised.  Otherwise, the
                              cursor will point to the new bottom. *)


      moveElementBeforePosition (from, to: CARDINAL)
                                 RAISES {NotInitialized, CursorError};
                                 (* Move an element from from to to.  The
                                    cursor will be set to to.  If from
                                    and/or to are outside of the list
                                    boundaries, CursorError will be
                                    raised. *)


      moveElementBehindPosition (from, to: CARDINAL)
                                 RAISES {NotInitialized, CursorError};
                                 (* Move an element from from behind to.
                                    The cursor will be set to to.  If from
                                    and/or to are outside of the list
                                    boundaries, CursorError will be
                                    raised. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Copy                                         *)
      (**                                                                   *)
      (**********************************************************************)

      copyCurrentToTop () RAISES {NotInitialized, CursorError};
                        (* Copy the current cursor element to the top of
                           the list.  If the cursor is undefined,
                           CursorError will be raised.  Otherwise, the
                           cursor will point to the new top. *)


      copyCurrentToBottom () RAISES {NotInitialized, CursorError};
                           (* Copy the current cursor element to the bottom
                              of the list.  If the cursor is undefined,
                              CursorError will be raised.  Otherwise, the
                              cursor will point to the new bottom. *)


      copyElementBeforePosition (from, to: CARDINAL)
                                 RAISES {NotInitialized, CursorError};
                                 (* Copy an element from from to to.  The
                                    cursor will be set to to.  If from
                                    and/or to are outside of the list
                                    boundaries, CursorError will be
                                    raised. *)


      copyElementBehindPosition (from, to: CARDINAL)
                                 RAISES {NotInitialized, CursorError};
                                 (* Copy an element from from behind to.
                                    The cursor will be set to to.  If from
                                    and/or to are outside of the list
                                    boundaries, CursorError will be
                                    raised. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Operations on two Lists                      *)
      (**                                                                   *)
      (**********************************************************************)

      copy (): T RAISES {NotInitialized};
            (* Returns a copy of the list. *)


      insertListBeforeCursor (VAR secondList: T)
                              RAISES {NotInitialized, CursorError};
                              (* Insert secondList into the list before the
                                 cursor.  This operation will kill
                                 secondList.  The relative cursor position
                                 in the list will remain unchanged (i.e.
                                 the cursor will be on the same
                                 element). *)


      insertListBehindCursor (VAR secondList: T)
                              RAISES {NotInitialized, CursorError};
                              (* Insert secondList into the list after the
                                 cursor.  This operation will kill
                                 secondList.  The relative cursor position
                                 in the list will remain unchanged (i.e.
                                 the cursor will be on the same
                                 element). *)


      splitListBeforeCursor (tail: T) RAISES {NotInitialized, CursorError};
                             (* The list will be truncated at the cursor
                                position.  The deleted sublist (including
                                the former current element) will be
                                appended to the list tail.  tail has to be
                                an initialized list.  The cursor will be
                                set to the predecessor of the former
                                current element in the list.  In the tail
                                list, the cursor will be set to the first
                                appended element (this was the current
                                element in the other list). *)


      splitListBehindCursor (tail: T) RAISES {NotInitialized, CursorError};
                             (* The list will be truncated behind the
                                cursor position.  The deleted sublist
                                (excluding the current element) will be
                                appended to the list tail.  tail has to be
                                an initialized list.  The cursor will
                                remain unchanged.  In the tail list, the
                                cursor will be set to the first appended
                                element (this was the successor of the
                                current element in the other list). *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Iteration                                    *)
      (**                                                                   *)
      (**********************************************************************)

      loop () RAISES {NotInitialized};
            (* Initialize a loop over a list.  This is actually the same as
               gotoTop. *)


      get (VAR found: BOOLEAN): Element.T RAISES {NotInitialized};
           (* Get the current element and advance the cursor.  If no
              current element exists, found will return FALSE and an
              arbitrary value of type Element.T will be returned. *)


      (**********************************************************************)
      (**                                                                   *)
      (**                      Miscellaneous                                *)
      (**                                                                   *)
      (**********************************************************************)

      invert () RAISES {NotInitialized};
              (* Invert the list, i.e.  the top will become the bottom and
                 vice versa. *)


      pack () RAISES {NotInitialized};
            (* Remove adjacent multiple entries from the list, e.g.
               BBAACDEEAA will be reduced to BACDEA.  Equality is defined
               through Element.Eq.  The cursor will be set to the top of
               the list. *)


      checkConsistency () RAISES {NotInitialized, CursorError};
                        (* Check whether the list is in a consistent
                           state. *)
    END;

END CursorList.
