INTERFACE GraphList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.1  1997/10/24 14:38:57  renehuel
    These files implement the new RGRASGraph interface.

*)
(***************************************************************************)

IMPORT TypedGraph, CardSet, RGGlobal;

TYPE
  GraphInfo = RECORD
                handle        : TypedGraph.T;
                name          : TEXT;
                poolName      : TEXT;
                schemeName    : TEXT;
                number        : CARDINAL;
                externalNumber: RGGlobal.ExternNumber;
              END;
  (* The information record stored in the list. *)
  T <: Public;
  Public =
    OBJECT
    METHODS
      size (): CARDINAL;
            (* Returns the size of the current list. *)
      addEntry (graph         : TypedGraph.T;
                name          : TEXT;
                poolName      : TEXT;
                schemeName    : TEXT;
                externalNumber: RGGlobal.ExternNumber;
                internalNumber: CARDINAL                := 0): CARDINAL
                RAISES {EntryAlreadyInList};
                (* Adds the graph to the list, and raises an exception when
                   the entry already existed. *)
      removeEntry (number: CARDINAL) RAISES {EntryNotInList};
                   (* Removes the graph from the list, and raises an
                      exception when the graph to be removed does not exist
                      in the list. *)
      isEntry (number: CARDINAL): BOOLEAN;
               (* Checks if a graph with the number 'number' is in the
                  list. *)
      isEntryByName (graphName, poolName: TEXT): BOOLEAN;
                     (* Checks if a graph with the name 'name' is in the
                        list. *)
      isEntryByExternalNumber (externalNumber: RGGlobal.ExternNumber):
                               BOOLEAN;
                               (* Checks if a graph with the external
                                  number 'externalNumber' is in the list*)
      isEntryByHandle (handle: TypedGraph.T): BOOLEAN;
                       (* Returns the graph with the number "number". *)
      getEntry (number: CARDINAL): GraphInfo RAISES {EntryNotInList};
                (* Checks if a graph with the handle 'handle' is in the
                   list. *)
      getEntryByExternalNumber (externalNumber: RGGlobal.ExternNumber):
                                GraphInfo RAISES {EntryNotInList};
                                (* Returns the graph with the external
                                   number "externalNumber" *)
      getEntryByName (graphName, poolName: TEXT): GraphInfo
                      RAISES {EntryNotInList};
                      (* Returns the graph with the name "graphName" from
                         the pool "poolName" *)
      getEntryByHandle (handle: TypedGraph.T): GraphInfo
                        RAISES {EntryNotInList};
                        (* Returns the graph with the handle 'handle' *)
      getAllEntries (): CardSet.T;
                     (* Returns a set with all numbers of the graphs in the
                        list *)
      init (): T;
            (* Initializes the empty list. *)
    END;

EXCEPTION
  EntryNotInList;
  EntryAlreadyInList;

END GraphList.
