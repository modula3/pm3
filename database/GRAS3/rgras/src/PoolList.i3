INTERFACE PoolList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.1  1997/10/24 14:39:00  renehuel
    These files implement the new RGRASGraph interface.

*)
(***************************************************************************)
IMPORT TypedGraphPool, TextCursorSet;

(* This module implements a list of graph pools. *)

TYPE
  PoolInfo = RECORD
               name  : TEXT;
               handle: TypedGraphPool.T;
             END;
    (* The information record stored in the list *)
  T <: Public;
  Public =
    OBJECT
    METHODS
      size (): CARDINAL;
            (* Returns the size of the current list. *)
      addEntry (pool: TypedGraphPool.T; name: TEXT)
                RAISES {EntryAlreadyInList};
                (* Adds the pool to the list, and raises an exception when
                   the entry already existed. *)
      removeEntry (name: TEXT) RAISES {EntryNotInList};
                   (* Removes the pool from the list, and raises an
                      exception when the pool to be removed does not exist
                      in the list. *)
      isEntry (name: TEXT): BOOLEAN;
               (* Checks if a pool is in the list. *)
      getEntry (name: TEXT): PoolInfo RAISES {EntryNotInList};
                (* Returns the pool with the name "name". *)
      getAllEntries (): TextCursorSet.T;
                     (* Returns a set with all entries from the list. *)
      init (): T;
            (* Initializes the empty list. *)
    END;

CONST Brand = "TypedGraphPoolList";

EXCEPTION
  EntryNotInList;
  EntryAlreadyInList;

END PoolList.
