MODULE SchemeList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:44  hosking
    Initial revision

    Revision 1.1  1997/10/24 14:39:17  renehuel
    These files implement the new RGRASGraph interface.

*)
(***************************************************************************)

IMPORT Scheme, CardSet, Text;

TYPE SchemeArray = REF ARRAY OF SchemeInfo;

REVEAL
  T = Public BRANDED OBJECT
        schemeArray: SchemeArray;
        counter    : CARDINAL
      OVERRIDES
        size             := Size;
        addEntry         := AddEntry;
        removeEntry      := RemoveEntry;
        getEntry         := GetEntry;       
        getAllEntries    := GetAllEntries;
        isEntry          := IsEntry;
        isEntryByName    := IsEntryByName;       
        init             := Init;
      END;

PROCEDURE Size (self: T): CARDINAL =
  BEGIN
    (* Returning the length of the array which holds the list. *)
    RETURN NUMBER(self.schemeArray^)
  END Size;

PROCEDURE AddEntry (self: T; scheme: Scheme.T; name, poolName: TEXT): CARDINAL =
  VAR
    newArray: SchemeArray;
    length  : CARDINAL;
  BEGIN
    (* Determining the length of the array which holds the list. *)
    length := NUMBER(self.schemeArray^);
    (* Creating an array, which is 1 field longer than the old one. *)
    newArray := NEW(SchemeArray, length + 1);
    (* Copying the entries of the old array into the new one. *)
    SUBARRAY(newArray^, 0, length) := self.schemeArray^;
    (* Now storing the new information in the last field of the new array. *)
    newArray[length].handle := scheme;
    newArray[length].name := name;
    newArray[length].poolName := poolName;
    newArray[length].number := self.counter;
    (* Increasing the internal counter *)
    self.counter := self.counter + 1;
    (* Replacing the old array by the new one. *)
    self.schemeArray := newArray;
    (* The internal number of the new list entry is returned. *)
    RETURN newArray[length].number;
  END AddEntry;

PROCEDURE RemoveEntry (self: T; number: CARDINAL) RAISES {EntryNotInList} =
  VAR
    newArray     : SchemeArray;
    length, index: CARDINAL;
  BEGIN
    (* Checking if the entry is in the list. *)
    IF self.isEntry(number) THEN
      (* Determining the length of the old array. *)
      length := NUMBER(self.schemeArray^);
      (* Creating an array which is 1 field shorter than the old one. *)
      newArray := NEW(SchemeArray, length - 1);
      (* Now copying all entries from the old array into the new one, except
         the one which is to be deleted. *)
      index := 0;
      FOR i := 1 TO length DO
        IF self.schemeArray[i - 1].number # number THEN
          newArray[index] := self.schemeArray[i - 1];
          index := index + 1
        END
      END;
      (* Overwriting the old array with the new one. *)
      self.schemeArray := newArray;
    ELSE
      (* Raising an exception if the entry is not in the list. *)
      RAISE EntryNotInList
    END
  END RemoveEntry;

PROCEDURE GetEntry (self: T; number: CARDINAL): SchemeInfo
  RAISES {EntryNotInList} =
  VAR schemeInfo: SchemeInfo;
  BEGIN
    (* Checking if the entry is in the list. *)
    IF self.isEntry(number) THEN
      (* Now looking for the index of the chosen entry. *)
      FOR i := 1 TO self.size() DO      
        IF self.schemeArray^[i - 1].number = number THEN
          (* When the entry was found, the information record is stored in 'schemeInfo'. *)
          schemeInfo := self.schemeArray^[i - 1]
        END;
      END;
    ELSE
      (* Raising an exception, when the entry was not in the list. *)
      RAISE EntryNotInList
    END;
    RETURN schemeInfo
  END GetEntry;

PROCEDURE IsEntry (self: T; number: CARDINAL): BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN
    found := FALSE;
    (* Running through the list, and looking for an entry with the chosen number. *)
    FOR i := 1 TO self.size() DO
      (* When the entry was found, the variable 'found' is set to TRUE. *)
      found := found OR self.schemeArray[i - 1].number = number
    END;
    RETURN found
  END IsEntry;

PROCEDURE IsEntryByName (self: T; schemeName, poolName: TEXT): BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN
    found := FALSE; 
    (* Running through the list, and looking for an entry with the chosen name. *)  
    FOR i := 1 TO self.size() DO
      (* When the entry was found, the variable 'found' is set to TRUE. *)
      found :=
        found
          OR (Text.Equal(self.schemeArray[i - 1].name, schemeName)
                AND Text.Equal(self.schemeArray[i - 1].poolName, poolName))
    END;
    RETURN found
  END IsEntryByName;

PROCEDURE Init (self: T): T =
  BEGIN
    (* Creating the array. *)
    self.schemeArray := NEW(SchemeArray, 0);
    (* Setting the internal counter to its starting value. *)
    self.counter := 1;
    RETURN self;
  END Init;

PROCEDURE GetAllEntries (self: T): CardSet.T =
  VAR schemeSet: CardSet.T := CardSet.New();
  BEGIN
    (* Inserting all entries in the array into a new generated TextCursorSet. *)
      FOR i := 1 TO self.size() DO
        schemeSet.insert(self.schemeArray^[i - 1].number)
      END;   
    RETURN schemeSet;
  END GetAllEntries;

BEGIN
END SchemeList.
