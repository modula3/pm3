INTERFACE SetList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:44  hosking
    Initial revision

    Revision 1.1  1997/12/23 12:39:41  renehuel
    New modules that implement the lists for the sets and relations, which
    are now organized in lists, and referenced by a cardinal number.

*)
(***************************************************************************)

IMPORT NodeSet;
IMPORT RGGlobal;
IMPORT CardSet;

(* This module implements a list of simple sets. *)

TYPE
  SetInfo = RECORD
               number  : RGGlobal.SimpleSet;
               handle: NodeSet.T;
             END;
    (* The information record stored in the list *)
  T <: Public;
  Public =
    OBJECT
    METHODS
      size (): CARDINAL;
            (* Returns the size of the current list. *)
      addEntry (set: NodeSet.T) : RGGlobal.SimpleSet;
                (* Adds the set to the list. *)
      removeEntry (number : RGGlobal.SimpleSet) RAISES {EntryNotInList};
                   (* Removes the set from the list, and raises an
                      exception when the set to be removed does not exist
                      in the list. *)
      isEntry (number: RGGlobal.SimpleSet): BOOLEAN;
               (* Checks if a set is in the list. *)
      getEntry (number: RGGlobal.SimpleSet): SetInfo RAISES {EntryNotInList};
                (* Returns the set with the number "number". *)
      getAllEntries (): CardSet.T;
                     (* Returns a set with all entries from the list. *)
      init (): T;
            (* Initializes the empty list. *)
    END;

CONST Brand = "NodeSetList";

EXCEPTION
  EntryNotInList;

END SetList.
