INTERFACE RelSetList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:44  hosking
    Initial revision

    Revision 1.1  1997/12/23 12:39:40  renehuel
    New modules that implement the lists for the sets and relations, which
    are now organized in lists, and referenced by a cardinal number.

*)
(***************************************************************************)

IMPORT NodeTypeRelation;
IMPORT RGGlobal;
IMPORT CardSet;

(* This module implements a list of relation sets. *)

TYPE
  RelSetInfo = RECORD
               number  : RGGlobal.RelSet;
               handle: NodeTypeRelation.T;
             END;
    (* The information record stored in the list *)
  T <: Public;
  Public =
    OBJECT
    METHODS
      size (): CARDINAL;
            (* Returns the size of the current list. *)
      addEntry (set: NodeTypeRelation.T) : RGGlobal.RelSet;
                (* Adds the set to the list. *)
      removeEntry (number : RGGlobal.RelSet) RAISES {EntryNotInList};
                   (* Removes the set from the list, and raises an
                      exception when the set to be removed does not exist
                      in the list. *)
      isEntry (number: RGGlobal.RelSet): BOOLEAN;
               (* Checks if a set is in the list. *)
      getEntry (number: RGGlobal.RelSet): RelSetInfo RAISES {EntryNotInList};
                (* Returns the set with the number "number". *)
      getAllEntries (): CardSet.T;
                     (* Returns a set with all entries from the list. *)
      init (): T;
            (* Initializes the empty list. *)
    END;

CONST Brand = "RelationSetList";

EXCEPTION
  EntryNotInList;

END RelSetList.
