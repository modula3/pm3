MODULE RelSetList;

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

IMPORT RGGlobal, NodeTypeRelation, CardSet;

TYPE RelSetArray = REF ARRAY OF RelSetInfo;

REVEAL
  T = Public BRANDED OBJECT
        relSetArray: RelSetArray;
        counter : RGGlobal.RelSet;
      OVERRIDES
        size          := Size;
        addEntry      := AddEntry;
        removeEntry   := RemoveEntry;
        getEntry      := GetEntry;
        getAllEntries := GetAllEntries;
        isEntry       := IsEntry;
        init          := Init;
      END;

PROCEDURE Size (self: T): CARDINAL =
  BEGIN
    (* Returns the size of the 'list' represented by the array 'setArray' *)
    RETURN NUMBER(self.relSetArray^)
  END Size;

PROCEDURE AddEntry (self: T; set: NodeTypeRelation.T) : RGGlobal.RelSet =
  VAR
    newArray: RelSetArray;
    length  : CARDINAL;
  BEGIN
    (* Determining the length of the array, which holds the list. *)
    length := NUMBER(self.relSetArray^);
    (* Creating an array which is 1 field longer as the old. *)
    newArray := NEW(RelSetArray, length + 1);
    (* Now copying the old array into the new one. *)
    SUBARRAY(newArray^, 0, length) := self.relSetArray^;
    (* Storing the new information in the last field of the new array. *)
    newArray[length].number := self.counter;
    newArray[length].handle := set;
    self.counter := self.counter + 1;
    (* Overwriting the old array with the new one. *)
    self.relSetArray := newArray;    
    RETURN self.counter - 1;
  END AddEntry;

PROCEDURE RemoveEntry (self: T; number: RGGlobal.RelSet) RAISES {EntryNotInList} =
  VAR
    newArray     : RelSetArray;
    length, index: CARDINAL;
  BEGIN
    (* Checking if the entry exists. *)
    IF self.isEntry(number) THEN
      (* Determining the length of the old array, which holds the list. *)
      length := NUMBER(self.relSetArray^);
      (* Creating a new array, which is 1 field shorter than the old one. *)
      newArray := NEW(RelSetArray, length - 1);
      (* Now copying all entries from the old array into the new array,
         except the one which is to be deleted. *)
      index := 0;
      FOR i := 1 TO length DO
        IF NOT self.relSetArray[i - 1].number = number THEN
          newArray[index] := self.relSetArray[i - 1];
          index := index + 1
        END
      END;
      (* Overwriting the old array with the new one. *)
      self.relSetArray := newArray;
    ELSE
      (* Raising an exception, when the entry which should be deleted is
      not in the list. *)
      RAISE EntryNotInList
    END
  END RemoveEntry;

PROCEDURE GetEntry (self: T; number: RGGlobal.RelSet): RelSetInfo
  RAISES {EntryNotInList} =
  VAR relSetInfo: RelSetInfo;
  BEGIN
    (* Checking if the entry is in the list. *)
    IF self.isEntry(number) THEN
      (* Now determining the index of the chosen entry. *)
      FOR i := 1 TO self.size() DO
        IF self.relSetArray^[i - 1].number = number THEN
          (* Reading the information record from the array at the found position. *)
          relSetInfo := self.relSetArray^[i - 1]
        END
      END;
    ELSE
      (* Raising an exception, when the entry is not in the list. *)
      RAISE EntryNotInList
    END;
    RETURN relSetInfo
  END GetEntry;

PROCEDURE IsEntry (self: T; number: RGGlobal.RelSet): BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN    
    found := FALSE;
    (* Running through the whole array, and searching for the entry with the
       number 'number'.*)
    FOR i := 1 TO self.size() DO
      (* When the chosen number could be found, the variable 'found' is set to TRUE. *)
      found := found OR self.relSetArray[i - 1].number = number;
    END;
    RETURN found
  END IsEntry;

PROCEDURE Init (self: T): T =
  BEGIN
    (* Creating the array which contains the list. *)
    self.relSetArray := NEW(RelSetArray, 0);
    self.counter := 1;
    RETURN self;
  END Init;

PROCEDURE GetAllEntries (self: T): CardSet.T =
  VAR entrieSet: CardSet.T := CardSet.New();
  BEGIN
      (* All entries in the list have to be inserted into a CardSet *)
      FOR i := 1 TO self.size() DO       
        entrieSet.insert(self.relSetArray^[i - 1].number)
      END;  
    RETURN entrieSet;
  END GetAllEntries;

BEGIN
END RelSetList.
