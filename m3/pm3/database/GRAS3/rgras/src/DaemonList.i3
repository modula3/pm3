INTERFACE DaemonList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:44  hosking
    Initial revision

    Revision 1.1  1998/05/15 12:21:55  renehuel
    Adapted the RGRASGraph interface to handle events.

*)
(***************************************************************************)

FROM RGGlobal IMPORT GraphEvent, ActionProc, GraphNumber;
IMPORT IntSeq;

TYPE
  DLContent = RECORD
                graphNumber: GraphNumber;
                graphEvent : GraphEvent;
                actionProc : ActionProc;
                triggerIDs  : IntSeq.T;
              END;
  T <: Public;
  Public =
    OBJECT
    METHODS
      addEntry (graphNumber: GraphNumber;
                graphEvent : GraphEvent;
                actionProc : ActionProc;
                triggerIDs  : IntSeq.T     ) RAISES {EntryAlreadyInList};
                (* Adds the information
                   graphNumber/graphEvent/actionProc/triggerID to the list.
                   An exception is raised, when the event was already
                   covered. *)
      removeEntry (graphNumber: GraphNumber;
                   graphEvent : GraphEvent;
                   actionProc : ActionProc   ) RAISES {EntryNotInList};
                   (* Removes the tuple graphNumber/graphEvent/actionProc
                      from the list.  Raises an exception if this tuple is
                      not in the list. *)
      isEntry (graphNumber: GraphNumber;
               graphEvent : GraphEvent;
               actionProc : ActionProc   ): BOOLEAN;
               (* Checks, if the tuple graphNumber/graphEvent/actionProc is
                  already in the list. *)
      getTriggerIDs (graphNumber: GraphNumber;
                    graphEvent : GraphEvent;
                    actionProc : ActionProc   ): IntSeq.T
                    RAISES {EntryNotInList};
                    (* Returns the stored triggerID for the tuple
                       graphNumber/graphEvent/actionProc.  Raises an
                       exception if this tuple is not in the list. *)
      getFirstEntryWithGraphNumber(graphNumber : GraphNumber): DLContent RAISES {EntryNotInList};
      (* Returns the first entry in the list for the chosen graphNumber. *)
      isEntryWithGraphNumber(graphNumber : GraphNumber) : BOOLEAN;
      (* Checks whether an entry with the chosen graphNumber exists in the list. *)
      init (): T;
            (* Initializes the empty list. *)
    END;

CONST Brand = "DaemonList";

EXCEPTION EntryNotInList;
EXCEPTION EntryAlreadyInList;

END DaemonList.
