UNSAFE MODULE PartIdSet;

IMPORT Word;

TYPE
  Member = UNTRACED REF RECORD
    value: INTEGER;
    next: Member;
  END;

CONST HashTableSize = 511;

REVEAL T = Public BRANDED "PartIdSet.T" OBJECT
  hashTable := ARRAY [0..HashTableSize - 1] OF Member { NIL, .. };
  emptySet := TRUE;
OVERRIDES
  dispose := Dispose;
  insert := Insert;
  contains := Contains;
  empty := Empty;
END;

PROCEDURE hash (val: INTEGER): INTEGER =
  BEGIN
    RETURN Word.Mod(Word.Plus(Word.Times(val, 12345), 6789), HashTableSize);
  END hash;

PROCEDURE Dispose (self: T) =
  VAR
    mem, nextMem: Member;
  BEGIN
    FOR h := 0 TO HashTableSize - 1 DO
      mem := self.hashTable[h];
      WHILE mem # NIL DO
        nextMem := mem.next;
        DISPOSE(mem);
        mem := nextMem;
      END;
    END;
    DISPOSE(self);
  END Dispose;

PROCEDURE Insert (self: T; val: INTEGER) =
  VAR
    h := hash(val);
    mem := NEW(Member, value := val, next := self.hashTable[h]);
  BEGIN
    self.hashTable[h] := mem;
    self.emptySet := FALSE;
  END Insert;

PROCEDURE Contains (self: T; val: INTEGER): BOOLEAN =
  VAR
    h := hash(val);
    mem := self.hashTable[h];
  BEGIN
    WHILE mem # NIL AND mem.value # val DO
      mem := mem.next;
    END;
    RETURN mem # NIL;
  END Contains;

PROCEDURE Empty (self: T): BOOLEAN =
  BEGIN
    RETURN self.emptySet;
  END Empty;

BEGIN
END PartIdSet.
