INTERFACE FilePos;
IMPORT PageData, Type;

TYPE
  FileIndex = [FIRST(PageData.Index) - 1 .. LAST(PageData.Index)];
    (* The current position pointer of a file points at the last position
       directly before the next position.  So the first position to point
       at on a new page is not 1 (=FIRST(PageData.Index)) but 0. *)

  (* A variable of type FilePos represents a pointer to an arbitrary
     position in the file. *)
  T = RECORD
        page: CARDINAL;
        pPos: FileIndex;
      END;

CONST ZeroPos = T{page := 0, pPos := 0};
  
PROCEDURE Eq (READONLY a, b: T): BOOLEAN;
PROCEDURE Compare(READONLY a, b: T): [-1..1];
  (* a < b => -1, a=b => 0, a>b => 1 *)
  
CONST ByteSize = BYTESIZE(FileIndex) + BYTESIZE(CARDINAL);

PROCEDURE Bytes(READONLY pos: T): CARDINAL;
  (* Returns the number of bytes a pos points into a file *)
  
PROCEDURE ToByteArray (READONLY fp: T; VAR ba: Type.ByteArray);
PROCEDURE FromByteArray (READONLY ba: Type.ByteArray; VAR fp: T);
  (* Conversions to and from array of byte.  It is a checked runtime error
     if NUMBER(ba)<FilePosLength *)


PROCEDURE Increment(VAR pos: T; amount: CARDINAL);
PROCEDURE Decrement(VAR pos: T; amount: CARDINAL; VAR underflow: BOOLEAN);
  (* Increment/Decrement the file-position by amount bytes. *)
  
END FilePos.
