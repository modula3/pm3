MODULE PoolList;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:42  hosking
    Initial revision

    Revision 1.2  1997/12/23 12:22:24  renehuel
    Removed bug. Now raising an exception when pool to be inserted into
    the list ist already in the list.

    Revision 1.1  1997/10/24 14:39:01  renehuel
    These files implement the new RGRASGraph interface.

*)
(***************************************************************************)

IMPORT TypedGraphPool, TextCursorSet, Text;

TYPE PoolArray = REF ARRAY OF PoolInfo;

REVEAL
  T = Public BRANDED OBJECT
        poolArray: PoolArray;
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
    (* Returns the size of the 'list' represented by the array 'poolArray' *)
    RETURN NUMBER(self.poolArray^)
  END Size;

PROCEDURE AddEntry (self: T; pool: TypedGraphPool.T; name: TEXT)
  RAISES {EntryAlreadyInList}=
  VAR
    newArray: PoolArray;
    length  : CARDINAL;
  BEGIN
    IF NOT self.isEntry(name) THEN  
    (* Determining the length of the array, which holds the list. *)
    length := NUMBER(self.poolArray^);
    (* Creating an array which is 1 field longer as the old. *)
    newArray := NEW(PoolArray, length + 1);
    (* Now copying the old array into the new one. *)
    SUBARRAY(newArray^, 0, length) := self.poolArray^;
    (* Storing the new information in the last field of the new array. *)
    newArray[length].name := name;
    newArray[length].handle := pool;
    (* Overwriting the old array with the new one. *)
    self.poolArray := newArray;    
    ELSE
      RAISE EntryAlreadyInList;
    END;
  END AddEntry;

PROCEDURE RemoveEntry (self: T; name: TEXT) RAISES {EntryNotInList} =
  VAR
    newArray     : PoolArray;
    length, index: CARDINAL;
  BEGIN
    (* Checking if the entry exists. *)
    IF self.isEntry(name) THEN
      (* Determining the length of the old array, which holds the list. *)
      length := NUMBER(self.poolArray^);
      (* Creating a new array, which is 1 field shorter than the old one. *)
      newArray := NEW(PoolArray, length - 1);
      (* Now copying all entries from the old array into the new array,
         except the one which is to be deleted. *)
      index := 0;
      FOR i := 1 TO length DO
        IF NOT Text.Equal(self.poolArray[i - 1].name,name) THEN
          newArray[index] := self.poolArray[i - 1];
          index := index + 1
        END
      END;
      (* Overwriting the old array with the new one. *)
      self.poolArray := newArray;
    ELSE
      (* Raising an exception, when the entry which should be deleted is
      not in the list. *)
      RAISE EntryNotInList
    END
  END RemoveEntry;

PROCEDURE GetEntry (self: T; name: TEXT): PoolInfo
  RAISES {EntryNotInList} =
  VAR poolInfo: PoolInfo;
  BEGIN
    (* Checking if the entry is in the list. *)
    IF self.isEntry(name) THEN
      (* Now determining the index of the chosen entry. *)
      FOR i := 1 TO self.size() DO
        IF Text.Equal(self.poolArray^[i - 1].name, name) THEN
          (* Reading the information record from the array at the found position. *)
          poolInfo := self.poolArray^[i - 1]
        END
      END;
    ELSE
      (* Raising an exception, when the entry is not in the list. *)
      RAISE EntryNotInList
    END;
    RETURN poolInfo
  END GetEntry;

PROCEDURE IsEntry (self: T; name: TEXT): BOOLEAN =
  VAR found: BOOLEAN;
  BEGIN    
    found := FALSE;
    (* Running through the whole array, and searching for the entry with the
       name 'name'.*)
    FOR i := 1 TO self.size() DO
      (* When the chosen name could be found, the variable 'found' is set to TRUE. *)
      found := found OR Text.Equal(self.poolArray[i - 1].name,name);
    END;
    RETURN found
  END IsEntry;

PROCEDURE Init (self: T): T =
  BEGIN
    (* Creating the array which contains the list. *)
    self.poolArray := NEW(PoolArray, 0);
    RETURN self;
  END Init;

PROCEDURE GetAllEntries (self: T): TextCursorSet.T =
  VAR entrieSet: TextCursorSet.T := TextCursorSet.New();
  BEGIN
      (* All entries in the list have to be inserted into a TextCursorSet *)
      FOR i := 1 TO self.size() DO       
        entrieSet.insert(self.poolArray^[i - 1].name)
      END;  
    RETURN entrieSet;
  END GetAllEntries;

BEGIN
END PoolList.
