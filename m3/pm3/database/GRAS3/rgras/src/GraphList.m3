MODULE GraphList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.1  1997/10/24 14:38:58  renehuel
    These files implement the new RGRASGraph interface.

*)
(***************************************************************************)

IMPORT TypedGraph, CardSet, RGGlobal, Text;

TYPE GraphArray = REF ARRAY OF GraphInfo;

REVEAL
  T = Public BRANDED OBJECT
        graphArray: GraphArray;
        counter   : CARDINAL;
      OVERRIDES
        size                     := Size;
        addEntry                 := AddEntry;
        removeEntry              := RemoveEntry;
        getEntry                 := GetEntry;
        getEntryByExternalNumber := GetEntryByExternalNumber;
        getEntryByName           := GetEntryByName;
        getEntryByHandle         := GetEntryByHandle;
        isEntry                  := IsEntry;
        isEntryByName            := IsEntryByName;
        isEntryByExternalNumber  := IsEntryByExternalNumber;
        isEntryByHandle          := IsEntryByHandle;
        getAllEntries            := GetAllEntries;
        init                     := Init;
      END;

PROCEDURE Size (self: T): CARDINAL =
  BEGIN
    (* Returns the lenght of the array which holds the list. *)
    RETURN NUMBER(self.graphArray^)
  END Size;

PROCEDURE AddEntry (self          : T;
                    graph         : TypedGraph.T;
                    name          : TEXT;
                    poolName      : TEXT;
                    schemeName    : TEXT;
                    externalNumber: RGGlobal.ExternNumber;
                    internalNumber: CARDINAL               ): CARDINAL =
  VAR
    newArray                 : GraphArray;
    length, newInternalNumber: CARDINAL;
  BEGIN
    (* Determining the new internal number for the new entry. *)
    IF (internalNumber # 0) AND (NOT self.isEntry(internalNumber)) THEN
      (* When the parameter internalNumber is # 0, and this number is not
         already used, the new entry will get this number. *)
      newInternalNumber := internalNumber
    ELSE
      (* Otherwise the internal counter is used, and increased for the next
         entry. *)
      newInternalNumber := self.counter;
      self.counter := self.counter + 1;
    END;
    (* Determining the length of the array containing the list. *)
    length := NUMBER(self.graphArray^);
    (* Creating a new array which is 1 field longer than the old one. *)
    newArray := NEW(GraphArray, length + 1);
    (* Copying all entries in the old array into the new one. *)
    SUBARRAY(newArray^, 0, length) := self.graphArray^;
    (* Storing all information in the last field of the new array. *)
    newArray[length].handle := graph;
    newArray[length].name := name;
    newArray[length].poolName := poolName;
    newArray[length].schemeName := schemeName;
    newArray[length].number := newInternalNumber;
    newArray[length].externalNumber := externalNumber;
    (* Overwriting the old array with the new one. *)
    self.graphArray := newArray;
    RETURN newArray[length].number;
  END AddEntry;

PROCEDURE GetEntry (self: T; number: CARDINAL): GraphInfo
  RAISES {EntryNotInList} =
  VAR
    graphInfo: GraphInfo;
    index    : INTEGER;
  BEGIN
    (* Checking if the entry is in the list. *)
    IF self.isEntry(number) THEN
      (* Getting the index of the entry *)
      index := GetIndex(self.graphArray, number);
      (* Returning the information record. *)
      graphInfo := self.graphArray^[index]
    ELSE
      (* Raising an exception if the entry was not in the list. *)
      RAISE EntryNotInList
    END;
    RETURN graphInfo
  END GetEntry;

PROCEDURE GetEntryByExternalNumber (self          : T;
                                    externalNumber: RGGlobal.ExternNumber):
  GraphInfo RAISES {EntryNotInList} =
  VAR graphInfo: GraphInfo;
  BEGIN
    (* Checking if the entry is in the list. *)
    IF self.isEntryByExternalNumber(externalNumber) THEN
      (* Running through the list, searching for the desired entry. *)
      FOR i := 1 TO self.size() DO
        (* If the entry with the chosen externalNumber is found, the
           information record is stored in 'graphInfo'. *)
        IF self.graphArray^[i - 1].externalNumber = externalNumber THEN
          graphInfo := self.graphArray^[i - 1]
        END;
      END;
    ELSE
      RAISE EntryNotInList
    END;
    RETURN graphInfo
  END GetEntryByExternalNumber;

PROCEDURE GetEntryByName (self: T; graphName, poolName: TEXT): GraphInfo
  RAISES {EntryNotInList} =
  VAR graphInfo: GraphInfo;
  BEGIN
    (* Checking if the entry is in the list. *)
    IF self.isEntryByName(graphName, poolName) THEN
      (* Runing through the list, searching for the desired entry. *)
      FOR i := 1 TO self.size() DO
        (* When the entry with the chosen graph- and poolname was found,
           the information record is stored in graphInfo. *)
        IF (Text.Equal(self.graphArray^[i - 1].name, graphName)
              AND Text.Equal(self.graphArray^[i - 1].poolName, poolName)) THEN
          graphInfo := self.graphArray^[i - 1]
        END;
      END;
    ELSE
      (* Raising an exception, when the entry is not in the list. *)
      RAISE EntryNotInList
    END;
    RETURN graphInfo
  END GetEntryByName;

PROCEDURE GetEntryByHandle (self: T; handle: TypedGraph.T): GraphInfo
  RAISES {EntryNotInList} =
  VAR graphInfo: GraphInfo;
  BEGIN
    (* Checking if the entry is in the list. *)
    IF self.isEntryByHandle(handle) THEN
      (* Runing through the list, searching for the desired entry. *)
      FOR i := 1 TO self.size() DO
        (* When the entry with the chosen handle was found, the information
           record is stored in graphInfo. *)
        IF self.graphArray^[i - 1].handle = handle THEN
          graphInfo := self.graphArray^[i - 1]
        END;
      END;
    ELSE
      (* Raising an exception, when the entry is not in the list. *)
      RAISE EntryNotInList
    END;
    RETURN graphInfo
  END GetEntryByHandle;

PROCEDURE IsEntry (self: T; number: CARDINAL): BOOLEAN =
  VAR index: INTEGER;
  BEGIN
    (* Checking existence of an entry, through getting its index, and
       comparing it with -1. *)
    index := GetIndex(self.graphArray, number);
    (* An index = -1 indicates that the entry is not in the list. *)
    RETURN (index # -1)
  END IsEntry;

PROCEDURE IsEntryByName (self: T; graphName, poolName: TEXT): BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN
    found := FALSE;
    (* Running through the list, and searching for the entry with the
       chosen graph- and poolname. *)
    FOR i := 1 TO self.size() DO
      (* When the entry is found, the variable 'found' is set to TRUE. *)
      found :=
        found
          OR (Text.Equal(self.graphArray[i - 1].name, graphName)
                AND Text.Equal(self.graphArray[i - 1].poolName, poolName))
    END;
    RETURN found
  END IsEntryByName;

PROCEDURE IsEntryByExternalNumber (self          : T;
                                   externalNumber: RGGlobal.ExternNumber):
  BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN
    found := FALSE;
    (* Running through the list, and searching for the entry with the
       chosen external number. *)
    FOR i := 1 TO self.size() DO
      (* When the entry with the chosen external number is found, the
         variable 'found' is set to TRUE. *)
      found :=
        found OR (self.graphArray[i - 1].externalNumber = externalNumber)
    END;
    RETURN found
  END IsEntryByExternalNumber;

PROCEDURE IsEntryByHandle (self: T; handle: TypedGraph.T): BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN
    found := FALSE;
    (* Running through the list, and searching for the entry with the
       chosen handle. *)
    FOR i := 1 TO self.size() DO
      (* When the entry with the chosen handle is found, the variable
         'found' is set to TRUE. *)
      found := found OR (self.graphArray[i - 1].handle = handle)
    END;
    RETURN found
  END IsEntryByHandle;

PROCEDURE Init (self: T): T =
  BEGIN
    (* Creating the array. *)
    self.graphArray := NEW(GraphArray, 0);
    (* Setting the internal counter to ist starting value. *)
    self.counter := 1;
    RETURN self;
  END Init;

PROCEDURE GetAllEntries (self: T): CardSet.T =
  VAR graphSet: CardSet.T := CardSet.New();
  BEGIN
    (* All entries in the list are entered into a CardSet. *)
    FOR i := 1 TO self.size() DO
      graphSet.insert(self.graphArray^[i - 1].number)
    END;
    RETURN graphSet;
  END GetAllEntries;

PROCEDURE RemoveEntry (self: T; number: CARDINAL) =
  VAR
    newArray     : GraphArray;
    length, index: CARDINAL;
  BEGIN
    index := 0;
    (* Determining the length of the array which holds the list. *)
    length := NUMBER(self.graphArray^);
    (* Creating a new array, which is 1 field shorter than the old one. *)
    newArray := NEW(GraphArray, length - 1);
    (* Running through the list, and copying all entries from the old array
       into the new one, except the one which is to be deleted. *)
    FOR i := 1 TO length DO
      IF self.graphArray[i - 1].number # number THEN
        newArray[index] := self.graphArray[i - 1];
        index := index + 1;
      END;
    END;
    self.graphArray := newArray;
  END RemoveEntry;

PROCEDURE GetIndex (graphArray: GraphArray; number: CARDINAL): INTEGER =
  VAR length, index: INTEGER := -1;
  BEGIN
    (* Determining the length of the array containing the list. *)
    length := NUMBER(graphArray^);
    (* Running through the array, and storing the index at which the entry
       with the chosen number was found in 'index'. *)
    FOR i := 1 TO length DO
      IF graphArray[i - 1].number = number THEN index := i - 1 END;
    END;
    RETURN index;
  END GetIndex;

BEGIN
END GraphList.
