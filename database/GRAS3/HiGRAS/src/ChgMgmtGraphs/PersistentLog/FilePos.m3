MODULE FilePos;

IMPORT Type, Cardinal;


TYPE FileIndexArray = ARRAY [0..BYTESIZE(FileIndex)-1] OF Type.Byte;
     

PROCEDURE Eq (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN (a.page = b.page AND a.pPos = b.pPos);
  END Eq;

PROCEDURE Compare(READONLY a, b: T): [-1..1] =
  BEGIN
    IF (a.page < b.page) OR (a.page = b.page AND a.pPos < b.pPos) THEN
      RETURN -1;
    ELSIF (b.page < a.page) OR (b.page = a.page AND b.pPos < a.pPos) THEN
      RETURN 1;
    ELSE
      RETURN 0;
    END;
  END Compare;
  
PROCEDURE FileIndexToByteArray (    x : FileIndex;
                                VAR ba: FileIndexArray     ) =
  BEGIN
    ba[0] := x MOD 256;
    ba[1] := x DIV 256;
  END FileIndexToByteArray;

PROCEDURE ByteArrayToFileIndex (READONLY ba: FileIndexArray;
                                VAR      x : FileIndex) =
  BEGIN
    x := ba[1];
    x := 256 * x + ba[0];
  END ByteArrayToFileIndex;


CONST
  IntLength       = BYTESIZE(INTEGER);
  FileIndexLength = BYTESIZE(FileIndex);

  FPPageOff = 0;                 (* Offset of page number within file
                                    position *)

  FPpPosOff = BYTESIZE(CARDINAL); (* Offset of page position within file
                                     position *)

PROCEDURE ToByteArray (READONLY fp: T; VAR ba: Type.ByteArray) =
  BEGIN
    <* ASSERT NUMBER(ba) >= ByteSize *>
    Cardinal.ToByteArray(fp.page, SUBARRAY(ba, FPPageOff, IntLength));
    FileIndexToByteArray(fp.pPos, SUBARRAY(ba, FPpPosOff, FileIndexLength));
  END ToByteArray;

PROCEDURE FromByteArray (READONLY ba: Type.ByteArray; VAR fp: T) =
  BEGIN
    <* ASSERT NUMBER(ba) >= ByteSize *>
    Cardinal.FromByteArray(SUBARRAY(ba, FPPageOff, IntLength), fp.page);
    ByteArrayToFileIndex(SUBARRAY(ba, FPpPosOff, FileIndexLength), fp.pPos);
  END FromByteArray;

PROCEDURE Bytes(READONLY pos: T): CARDINAL =
  BEGIN
    RETURN pos.page * LAST(FileIndex) + pos.pPos;
  END Bytes;

PROCEDURE CardToFpos(bytes: CARDINAL; VAR pos: T) =
  BEGIN
    pos.page := bytes DIV LAST(FileIndex);
    pos.pPos := bytes MOD LAST(FileIndex);
  END CardToFpos;
  
PROCEDURE Increment(VAR pos: T; amount: CARDINAL) =
  BEGIN
    INC(amount, Bytes(pos));
    CardToFpos(amount, pos);
  END Increment;
  
PROCEDURE Decrement(VAR pos: T; amount: CARDINAL; VAR underflow: BOOLEAN) =
  VAR bytes: CARDINAL;
  BEGIN
    bytes := Bytes(pos);
    IF amount > bytes THEN
      underflow := TRUE;
      pos := ZeroPos;
    ELSE
      DEC(bytes, amount);
      CardToFpos(bytes, pos);
    END;
  END Decrement;
  
BEGIN
END FilePos.
