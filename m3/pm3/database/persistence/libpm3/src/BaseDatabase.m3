UNSAFE MODULE BaseDatabase;

IMPORT Database, Fingerprint, RTDB, RTHeapDB, DBPage, BaseDBPage,
       Transaction, BaseTransaction, ThreadF;
IMPORT SortedTransientFPRefTbl AS FPRefTbl;
IMPORT SortedTransientFPAdrTbl AS FPAdrTbl;
IMPORT IntRefTransientTbl AS PageTbl;

REVEAL
  T = Database.Public BRANDED "BaseDatabase.T" OBJECT
    pageMap: PageTbl.T;
    fpRef: FPRefTbl.T;
    fpAdr: FPAdrTbl.T;
  OVERRIDES
    init       := Init;

    createRoot := CreateRoot;
    mapFP      := MapFP;
    newPage    := NewPage;
    newId      := NewId;
    mapPage    := MapPage;
    unmapPage  := UnmapPage;

    setRoot    := SetRoot;
    getRoot    := GetRoot;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.pageMap := NEW(PageTbl.Default).init();
    self.fpRef := NEW(FPRefTbl.Default).init();
    self.fpAdr := NEW(FPAdrTbl.Default).init();
    RETURN self;
  END Init;

TYPE Root = BRANDED "Database.Root" REF RECORD
  pages: RTDB.Id := 0;
  ref: REFANY := NIL;
END;

PROCEDURE CreateRoot(self: T) =
  BEGIN
    self.root := NEW(Root);
  END CreateRoot;

PROCEDURE MapFP(self: T;
                READONLY fp: Fingerprint.T;
                VAR fpRef: REF Fingerprint.T;
                VAR fpAdr: ADDRESS) =
  VAR
    ref: REFANY;
    adr: ADDRESS;
  BEGIN
    IF self.fpAdr.get(fp, adr) THEN
      fpAdr := adr;
      fpRef := NIL;
      RETURN;
    END;
    IF fpAdr # NIL THEN
      EVAL self.fpRef.delete(fp, ref);
      fpRef := LOOPHOLE(ref, REF Fingerprint.T);
      EVAL self.fpAdr.put(fp, fpAdr);
      RETURN;
    END;
    IF self.fpRef.get(fp, ref) THEN
      fpRef := LOOPHOLE(ref, REF Fingerprint.T);
      fpAdr := NIL;
      RETURN;
    END;
    IF fpRef = NIL THEN
      fpRef := NEW(REF Fingerprint.T);
      fpRef^ := fp;
    END;
    ref := fpRef;
    EVAL self.fpRef.put(fp, ref);
  END MapFP;

PROCEDURE NewPage(self: T): RTDB.Page =
  BEGIN
    RETURN NEW(DBPage.T, db := self).init();
  END NewPage;

PROCEDURE NewId(self: T; page: RTDB.Page) =
  VAR root: Root;
  BEGIN
    LOCK self DO
      root := self.root;
      IF root = NIL THEN
        root := RTHeapDB.SwizzleRoot(self);
        self.root := root;
      END;
      INC(root.pages);
      VAR id := root.pages;
      BEGIN
        page.id := id;
        IF self.pageMap.put(id, page) THEN
          <* ASSERT FALSE *>
        END
      END
    END
  END NewId;

PROCEDURE MapPage(self: T; p: RTDB.Id): RTDB.Page =
  VAR ref: <*TRANSIENT*> REFANY;
  BEGIN
    IF NOT self.pageMap.get(p, ref) THEN
      ref := NEW(DBPage.T, id := p, db := self).init();
      IF self.pageMap.put(p, ref) THEN
        <* ASSERT FALSE *>
      END
    END;
    RETURN ref;
  END MapPage;

PROCEDURE UnmapPage(self: T; p: RTDB.Id) =
  VAR
    ref: <*TRANSIENT*> REFANY;
  BEGIN
    IF NOT self.pageMap.delete(p, ref) THEN
      <* ASSERT FALSE *>
    END;
  END UnmapPage;

PROCEDURE SetRoot(self: T; object: REFANY) RAISES {Transaction.NotInProgress} =
  VAR
    root: Root;
    tr: Transaction.T := ThreadF.myTxn;
  BEGIN
    LOCK self DO
      IF tr = NIL OR NOT tr.isOpen() THEN RAISE Transaction.NotInProgress END;
      root := self.root;
      IF root = NIL THEN
        root := RTHeapDB.SwizzleRoot(self);
        self.root := root;
      END;
      root.ref := object;
    END;
  END SetRoot;

PROCEDURE GetRoot(self: T): REFANY RAISES {Transaction.NotInProgress} =
  VAR
    root: Root;
    tr: Transaction.T := ThreadF.myTxn;
  BEGIN
    LOCK self DO
      IF tr = NIL OR NOT tr.isOpen() THEN RAISE Transaction.NotInProgress END;
      root := self.root;
      IF root = NIL THEN
        root := RTHeapDB.SwizzleRoot(self);
        self.root := root;
      END;
    END;
    RETURN root.ref;
  END GetRoot;

BEGIN
END BaseDatabase.
