INTERFACE SchemeList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:44  hosking
    Initial revision

    Revision 1.1  1997/10/24 14:39:16  renehuel
    These files implement the new RGRASGraph interface.

*)
(***************************************************************************)


IMPORT Scheme, CardSet;

TYPE
  SchemeInfo = RECORD
                 handle  : Scheme.T;
                 number  : CARDINAL;
                 name    : TEXT;
                 poolName: TEXT;
               END;
    (* The information record stored in the list *)
  T <: Public;
  Public =
    OBJECT
    METHODS
      size (): CARDINAL;
            (* Returns the size of the current list. *)
      addEntry (scheme: Scheme.T; name, poolName: TEXT): CARDINAL
                RAISES {EntryAlreadyInList};
                (* Adds the scheme to the list, and raises an exception
                   when the entry already existed. *)
      removeEntry (number: CARDINAL) RAISES {EntryNotInList};
                   (* Removes the scheme from the list, and raises an
                      exception when the scheme to be removed does not
                      exist in the list. *)
      isEntry (number: CARDINAL): BOOLEAN;
               (* Checks if a scheme is in the list. *)
      isEntryByName (schemeName, poolName: TEXT): BOOLEAN;
                     (* Checks if a scheme with the name 'name' is in the
                        list *)
      getEntry (number: CARDINAL): SchemeInfo RAISES {EntryNotInList};
                (* Returns the scheme with the number "number". *)
      getAllEntries (): CardSet.T;
                     (* Returns a set with all numbers of the schemes in
                        the list *)
      init (): T;
            (* Initializes the empty list. *)
    END;

EXCEPTION
  EntryNotInList;
  EntryAlreadyInList;

END SchemeList.
