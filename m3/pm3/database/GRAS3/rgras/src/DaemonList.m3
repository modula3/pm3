MODULE DaemonList;

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

FROM RGGlobal IMPORT EventKind, GraphEvent, ActionProc, GraphNumber;

IMPORT Node, IntSeq;

TYPE
  DLElement = RECORD
                content   : DLContent;
                next, prev: DLPointer;
              END;
  DLPointer = REF DLElement;
  DL = RECORD first, actual: DLPointer;  END;


REVEAL
  T = Public BRANDED OBJECT
        dl: DL
      OVERRIDES
        addEntry                     := AddEntry;
        removeEntry                  := RemoveEntry;
        getTriggerIDs                 := GetTriggerIDs;
        isEntry                      := IsEntry;
        getFirstEntryWithGraphNumber := GetFirstEntryWithGraphNumber;
        isEntryWithGraphNumber       := IsEntryWithGraphNumber;
        init                         := Init;
      END;

PROCEDURE NormalizeDaemonEvent (VAR Event: GraphEvent) =
  (* GraphEvents must not mismatch in nonrelevant fields. *)
  VAR nullNode: Node.T := Node.T{0, 0};
  BEGIN
    CASE Event.Kind OF
    | EventKind.EVNewNode, EventKind.EVDeleteNode =>
        Event.TNode := nullNode;
    | EventKind.EVNewEdge, EventKind.EVDeleteEdge =>
    | EventKind.EVModifyAttribute, EventKind.EVIndexKeyModified =>
        Event.TNode := nullNode;
    | EventKind.EVTransactionStart =>
        Event.SNode := nullNode;
        Event.TNode := nullNode;
        Event.TNo := nullNode;
    | EventKind.EVTransactionEnd =>
        Event.SNode := nullNode;
        Event.TNode := nullNode;
    | EventKind.EVSetCheckpoint =>
        Event.SNode := nullNode;
        Event.TNode := nullNode;
        Event.TNo := nullNode;
    | EventKind.EVUndo, EventKind.EVRedo =>
        Event.SNode := nullNode;
        Event.TNode := nullNode;
    | EventKind.EVOpenGraph, EventKind.EVCloseGraph =>
        Event.SNode := nullNode;
        Event.TNode := nullNode;
        Event.TNo := nullNode;
    | EventKind.EVUserDefined =>
    END;
    Event.Self := FALSE;
    Event.SNodeExists := FALSE;
    Event.TNodeExists := FALSE;
  END NormalizeDaemonEvent;

PROCEDURE Init (self: T): T =
  BEGIN
    self.dl.actual := NIL;
    self.dl.first := NIL;
    RETURN self;
  END Init;

PROCEDURE InternalFindEntry (self       : T;
                             graphNumber: GraphNumber;
                             graphEvent : GraphEvent <*NOWARN*>;
                             actionProc : ActionProc             ):
  DLPointer =
  BEGIN
    (* Running through the list in search of the tuple
       graphNumber/graphEvent/actionProc. *)
    self.dl.actual := self.dl.first;
    WHILE NOT ((self.dl.actual = NIL)
                 OR ((self.dl.actual.content.graphEvent = graphEvent)
                       AND ((self.dl.actual.content.actionProc = actionProc)
                              AND (self.dl.actual.content.graphNumber
                                     = graphNumber)))) DO
      self.dl.actual := self.dl.actual.next;
    END;
    (* Returning a reference to the found entry or NIL if the end of the
       list was reached. *)
    RETURN self.dl.actual;
  END InternalFindEntry;

PROCEDURE IsEntry (self       : T;
                   graphNumber: GraphNumber;
                   graphEvent : GraphEvent <*NOWARN*>;
                   actionProc : ActionProc             ): BOOLEAN =
  BEGIN
    (* Normalizing the graph event to avoid a mismatch caused by
       nonrelevant fields. *)
    NormalizeDaemonEvent(graphEvent);
    (* Now checking for the entry in the list. *)
    RETURN NOT (InternalFindEntry(
                  self, graphNumber, graphEvent, actionProc) = NIL);
  END IsEntry;

PROCEDURE AddEntry (self       : T;
                    graphNumber: GraphNumber;
                    graphEvent : GraphEvent <*NOWARN*>;
                    actionProc : ActionProc;
                    triggerIDs  : IntSeq.T)
  RAISES {EntryAlreadyInList} =
  VAR newEntry: DLPointer;
  BEGIN
    (* First normalizing the graphEvent to avoid mismatch caused by
       nonrelevant fields. *)
    NormalizeDaemonEvent(graphEvent);
    (* Checking if the tuple graphNumber/graphEvent/actionProc is already
       in the list. *)
    IF self.isEntry(graphNumber, graphEvent, actionProc) THEN
      RAISE EntryAlreadyInList
    ELSE
      (* Creating a new entry in the list. *)
      newEntry := NEW(DLPointer);
      newEntry.next := self.dl.first;
      newEntry.prev := NIL;
      (* Setting the information of the new entry. *)
      newEntry.content.graphEvent := graphEvent;
      newEntry.content.actionProc := actionProc;
      newEntry.content.triggerIDs := triggerIDs;
      newEntry.content.graphNumber := graphNumber;
      (* And placing it at the head of the list. *)
      IF self.dl.first # NIL THEN self.dl.first.prev := newEntry; END;
      self.dl.first := newEntry;
    END;
  END AddEntry;

PROCEDURE RemoveEntry (self       : T;
                       graphNumber: GraphNumber;
                       graphEvent : GraphEvent <*NOWARN*>;
                       actionProc : ActionProc             )
  RAISES {EntryNotInList} =
  VAR deletePointer: DLPointer;
  BEGIN
    (* Normalizing graphEvent to avoid mismatch caused by nonrelevant
       fields. *)
    NormalizeDaemonEvent(graphEvent);
    (* Now getting a reference to the entry which is to be deleted. *)
    deletePointer :=
      InternalFindEntry(self, graphNumber, graphEvent, actionProc);
    (* If the entry was found, it can be removed from the list. *)
    IF deletePointer # NIL THEN
      IF deletePointer.prev # NIL THEN
        deletePointer.prev.next := deletePointer.next
      ELSE
        self.dl.first := deletePointer.next;
      END;
      IF deletePointer.next # NIL THEN
        deletePointer.next.prev := deletePointer.prev
      END;
    ELSE
      RAISE EntryNotInList;
    END;
  END RemoveEntry;

PROCEDURE GetTriggerIDs (self       : T;
                        graphNumber: GraphNumber;
                        graphEvent : GraphEvent <*NOWARN*>;
                        actionProc : ActionProc             ): IntSeq.T
  RAISES {EntryNotInList} =
  VAR
    entry : DLPointer;
    result: IntSeq.T  := NIL;
  BEGIN
    (* Normalizing the graphEvent to avoid a mismatch caused by nonrelevant
       fields. *)
    NormalizeDaemonEvent(graphEvent);
    (* Getting a reference to the desired entry. *)
    entry := InternalFindEntry(self, graphNumber, graphEvent, actionProc);
    IF entry = NIL THEN
      RAISE EntryNotInList
    ELSE
      (* If the entry was found, the triggerID can be returned. *)
      result := entry.content.triggerIDs;
    END;
    RETURN result
  END GetTriggerIDs;

PROCEDURE GetFirstEntryWithGraphNumber (self: T; graphNumber: GraphNumber):
  DLContent RAISES {EntryNotInList} =
  BEGIN
    self.dl.actual := self.dl.first;
    WHILE (self.dl.actual # NIL)
            AND (self.dl.actual.content.graphNumber # graphNumber) DO
      self.dl.actual := self.dl.actual.next;
    END;
    IF self.dl.actual = NIL THEN RAISE EntryNotInList; END;
    RETURN self.dl.actual.content
  END GetFirstEntryWithGraphNumber;

PROCEDURE IsEntryWithGraphNumber (self: T; graphNumber: GraphNumber):
  BOOLEAN =
  BEGIN
    self.dl.actual := self.dl.first;
    WHILE (self.dl.actual # NIL)
            AND (self.dl.actual.content.graphNumber # graphNumber) DO
      self.dl.actual := self.dl.actual.next;
    END;
    RETURN self.dl.actual # NIL;
  END IsEntryWithGraphNumber;

BEGIN
END DaemonList.
