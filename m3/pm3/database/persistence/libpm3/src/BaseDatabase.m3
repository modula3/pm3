MODULE BaseDatabase;

IMPORT Database, Fingerprint, RTHeapDB, DBPage, BaseDBPage,
       Transaction, BaseTransaction, RTType;
IMPORT TransientFPRefTbl AS FPRefTbl;
IMPORT TransientFPAdrTbl AS FPAdrTbl;
IMPORT IntRefTransientTbl AS PageTbl;
FROM RTHeapDep IMPORT Page;

REVEAL
  T = Database.Public BRANDED "BaseDatabase.T" OBJECT
    pageMap: PageTbl.T;
    fpRef: FPRefTbl.T;
    fpAdr: FPAdrTbl.T;
  OVERRIDES
    init       := Init;

    mapFP      := MapFP;
    newPage    := NewPage;
    rootTC     := RootTC;
    newId      := NewId;
    pageMapGet := PageMapGet;
    pageMapPut := PageMapPut;
    pageMapDel := PageMapDel;

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

PROCEDURE MapFP(self: T;
                READONLY fp: Fingerprint.T;
                VAR fpRef: REF Fingerprint.T;
                VAR fpAdr: ADDRESS) =
  VAR ref: REFANY;
  BEGIN
    IF fpAdr # NIL THEN
      EVAL self.fpRef.delete(fp, ref);
      EVAL self.fpAdr.put(fp, fpAdr);
      RETURN;
    END;
    IF fpRef # NIL THEN
      EVAL self.fpRef.put(fp, fpRef);
      RETURN;
    END;
    EVAL self.fpAdr.get(fp, fpAdr);
    IF self.fpRef.get(fp, ref) THEN
      fpRef := NARROW(ref, REF Fingerprint.T);
    END;
    <* ASSERT fpRef = NIL OR fpAdr = NIL *>
  END MapFP;

PROCEDURE NewPage(self: T; p: Page; id: Page):
  RTHeapDB.DBPage =
  BEGIN
    RETURN NEW(DBPage.T, p := p, db := self, id := id).init();
  END NewPage;

TYPE
  Root = RTHeapDB.DBRoot OBJECT
    pages := 0;
  END;

PROCEDURE RootTC(<*UNUSED*> self: T): RTType.Typecode =
  BEGIN
    RETURN TYPECODE(Root);
  END RootTC;

PROCEDURE NewId(self: T; n: CARDINAL): Page =
  VAR
    id: Page;
    root: Root;
  BEGIN
    LOCK self DO
      IF self.root = NIL THEN
        self.root := RTHeapDB.SwizzleRoot(self);
      END;
      root := self.root;
      id := root.pages + 1;
      INC(root.pages, n);
      RETURN id;
    END
  END NewId;

PROCEDURE PageMapGet(self: T; id: Page): RTHeapDB.DBPage =
  VAR ref: <*TRANSIENT*> REFANY;
  BEGIN
    IF self.pageMap.get(id, ref) THEN
      RETURN ref;
    END;
    RETURN NIL;
  END PageMapGet;

PROCEDURE PageMapPut(self: T; page: RTHeapDB.DBPage) =
  VAR ref: <*TRANSIENT*> REFANY := page;
  BEGIN
    IF NOT self.pageMap.put(page.id, ref) THEN
      RETURN;
    END;
    <* ASSERT FALSE *>
  END PageMapPut;

PROCEDURE PageMapDel(self: T; page: RTHeapDB.DBPage) =
  VAR ref: <*TRANSIENT*> REFANY;
  BEGIN
    IF self.pageMap.delete(page.id, ref) THEN
      <* ASSERT page = ref *>
      RETURN;
    END;
    <* ASSERT FALSE *>
  END PageMapDel;

PROCEDURE SetRoot(self: T; object: REFANY) RAISES {Transaction.NotInProgress} =
  VAR tr: Transaction.T := RTHeapDB.myTxn;
  BEGIN
    LOCK self DO
      IF tr = NIL OR NOT tr.isOpen() THEN RAISE Transaction.NotInProgress END;
      IF self.root = NIL THEN
        self.root := RTHeapDB.SwizzleRoot(self);
      END;
      self.root.ref := object;
    END;
  END SetRoot;

PROCEDURE GetRoot(self: T): REFANY RAISES {Transaction.NotInProgress} =
  VAR tr: Transaction.T := RTHeapDB.myTxn;
  BEGIN
    LOCK self DO
      IF tr = NIL OR NOT tr.isOpen() THEN RAISE Transaction.NotInProgress END;
      IF self.root = NIL THEN
        self.root := RTHeapDB.SwizzleRoot(self);
      END;
      RETURN self.root.ref;
    END;
  END GetRoot;

BEGIN
END BaseDatabase.
