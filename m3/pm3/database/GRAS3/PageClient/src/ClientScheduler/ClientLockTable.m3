MODULE ClientLockTable;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.5  1997/03/18 15:24:49  roland
    Bugfix in Expand: newtable and newlast have to be initialized in for loop.

    Revision 1.4  1997/01/20 08:54:22  roland
    ClientLockTable uses an array now directly to gain more performance
    (getLastEntry is called much more often than any other procedure of
    the module, so this must be fast).
    Minor, non-critical changes in ScheduledClientPage.

    Revision 1.3  1996/03/06 16:11:31  rbnix
        Journal output improved.

    Revision 1.2  1996/03/06 14:04:10  rbnix
        New method fmt added to get a formatted representation of the
        lock table's value.

    Revision 1.1  1996/02/09 16:46:41  rbnix
        First version of client scheduler added.

*)
(***************************************************************************)


IMPORT Fmt;
IMPORT Transaction, PageLock, ClientLockEntry;


CONST NullEntry = ClientLockEntry.T{PageLock.Mode.O, NIL};

TYPE
  LockTable = REF ARRAY OF ClientLockEntry.T;
  History = REF ARRAY OF Transaction.Level;

REVEAL
  T = Public BRANDED OBJECT
        size: Transaction.Level;  (* table size *)
        top: Transaction.Level;  (* highest index with lock #
                                    PageLock.Mode.O in table or 0 *)
        last: History;           (* last[i] is the highest index j with
                                    lock # PageLock.Mode.O and j<=i or 0 *)
        table: LockTable;        (* the lock-table *)
      OVERRIDES
        init         := Init;
        putEntry     := PutEntry;
        getEntry     := GetEntry;
        getLastEntry := GetLastEntry;
        exists       := Exists;

        fmt := FmtTable;
      END;


(*
 | --- support functions --------------------------------------------------
 *)

PROCEDURE Expand (self: T; level: Transaction.Level) =
  VAR
    newtable: LockTable;
    newlast : History;
    newsize : Transaction.Level;
  BEGIN
    IF self.size < level THEN
      newsize := MAX(level, 2 * self.size);
      newtable := NEW(LockTable, newsize);
      SUBARRAY(newtable^, 0, self.size) := self.table^;
      newlast := NEW(History, newsize);
      SUBARRAY(newlast^, 0, self.size) := self.last^;
      FOR i := self.size TO newsize -1 DO
        newtable[i] := NullEntry;
        newlast[i] := Transaction.EnvelopeLevel;
      END;
      self.table := newtable;
      self.last := newlast;
      self.size := newsize;
    END
  END Expand;


(*
 | --- public functions ---------------------------------------------------
 *)
PROCEDURE Init (self: T): T =
  BEGIN
    self.size := 10;
    self.table := NEW(LockTable, self.size);
    self.last := NEW(History, self.size);
    FOR i := Transaction.EnvelopeLevel TO self.size-1 DO
      self.table[i] := NullEntry;
      self.last[i] := Transaction.EnvelopeLevel;
    END;
    self.top := Transaction.EnvelopeLevel;
    RETURN self;
  END Init;


PROCEDURE PutEntry (self : T;
                    level: Transaction.Level;
                    entry: ClientLockEntry.T  ) =
  VAR lev: Transaction.Level;
  BEGIN
    IF self.size - 1 < level THEN Expand(self, level + 1); END;

    self.table[level] := entry;

    (* update history *)
    IF self.top < level AND entry.lock # PageLock.Mode.O THEN
      (* new top, propagate last non O-Lock *)
      FOR lev := self.top + 1 TO level - 1 DO
        self.last[lev] := self.last[self.top]
      END;
      self.top := level;
      (* This must be a none O-Lock *)
      self.last[level] := level;
    ELSIF level = self.top AND entry.lock = PageLock.Mode.O THEN
      IF level # Transaction.EnvelopeLevel THEN
        (* this is a pop-operation; set top to most recent none O-Lock *)
        self.top := self.last[self.top - 1];
      END;
    ELSIF level < self.top AND entry.lock = PageLock.Mode.O THEN
      IF self.last[level] = level AND level # Transaction.EnvelopeLevel THEN
        (* last-pointers must be adapted to most recent none O-Lock *)
        lev := level;
        WHILE self.last[lev] = level DO
          (* this will stop, because self.last[self.top] # level *)
          self.last[lev] := self.last[level - 1];
          INC(lev)
        END;
      END;
    ELSIF level < self.top AND entry.lock # PageLock.Mode.O THEN
      IF self.last[level] # level THEN
        (* last-pointers must be adapted to new none O-Lock *)
        lev := level;
        WHILE self.last[lev] # lev DO
          (* this will stop, because self.last[self.top] = self.top *)
          self.last[lev] := level;
          INC(lev)
        END;
      END;
    ELSE
      (* Remaining cases are: 1) self.top < level AND entry.lock =
         PageLock.Mode.O and 2) level = self.top AND entry.lock #
         PageLock.Mode.O.  Both have no consequences on top or last. *)
    END;
  END PutEntry;


PROCEDURE GetEntry (self: T; level: Transaction.Level): ClientLockEntry.T =
  BEGIN
    IF self.top < level THEN
      RETURN NullEntry
    ELSE
      RETURN self.table[level];
    END;
  END GetEntry;


PROCEDURE GetLastEntry (    self       : T;
                            searchLevel: Transaction.Level;
                        VAR lastLevel  : Transaction.Level  ):
  ClientLockEntry.T =
  BEGIN
    IF searchLevel >= self.top THEN
      lastLevel := self.top;
      RETURN self.table[self.top]
    ELSE
      lastLevel := self.last[searchLevel];
      RETURN self.table[lastLevel];
    END;
  END GetLastEntry;


PROCEDURE Exists (self       : T;
                  searchLevel: Transaction.Level;
                  lock       : PageLock.ClientMode): BOOLEAN =
  BEGIN
    IF searchLevel > self.top THEN searchLevel := self.top; END;
    WHILE searchLevel > Transaction.EnvelopeLevel
            AND self.table[searchLevel].lock # lock DO
      DEC(searchLevel)
    END;
    RETURN self.table[searchLevel].lock = lock;
  END Exists;


PROCEDURE FmtTable (self: T): TEXT =
  VAR text: TEXT;
  BEGIN
    text := "[\n index = 0, entry = (" & ClientLockEntry.Fmt(self.table[0])
              & ")";
    FOR lev := Transaction.EnvelopeLevel + 1 TO self.top DO
      text := text & " index = " & Fmt.Int(lev) & ", entry = ("
                & ClientLockEntry.Fmt(self.table[lev]) & ")";
    END;
    RETURN text;
  END FmtTable;

BEGIN
END ClientLockTable.
