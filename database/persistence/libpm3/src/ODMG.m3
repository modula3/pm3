MODULE ODMG;

IMPORT Database, Transaction, TextRefBPlusTree;

REVEAL T = Public BRANDED "ODMG.T" OBJECT
  db : Database.T;
OVERRIDES
  close := Close;
  bind := Bind;
  unbind := Unbind;
  lookup := Lookup;
END;

PROCEDURE Open(name: TEXT): T
  RAISES { Database.NotFound, Database.Opened, Transaction.InProgress } =
  BEGIN
    RETURN NEW(T, db := Database.Open(name));
  END Open;

PROCEDURE Close(<*UNUSED*>self: T) =
  BEGIN
  END Close;

PROCEDURE Bind(self: T; object: REFANY; name: TEXT)
  RAISES { ObjectNameNotUnique, Transaction.NotInProgress } =
  VAR root: TextRefBPlusTree.T := self.db.getRoot();
  BEGIN
    IF root = NIL THEN
      root := NEW(TextRefBPlusTree.T).init();
      self.db.setRoot(root);
    END;
    IF root.put(name, object) THEN
      RAISE ObjectNameNotUnique;
    END
  END Bind;

PROCEDURE Unbind(self: T; name: TEXT): REFANY
  RAISES { ObjectNameNotFound, Transaction.NotInProgress } =
  VAR
    root: TextRefBPlusTree.T := self.db.getRoot();
    object: REFANY;
  BEGIN
    root := self.db.getRoot();
    IF root = NIL THEN
      root := NEW(TextRefBPlusTree.T).init();
      self.db.setRoot(root);
    END;
    IF NOT root.delete(name, object) THEN
      RAISE ObjectNameNotFound;
    END;
    RETURN object;
  END Unbind;

PROCEDURE Lookup(self: T; name: TEXT): REFANY
  RAISES { ObjectNameNotFound, Transaction.NotInProgress } =
  VAR
    root: TextRefBPlusTree.T := self.db.getRoot();
    object: REFANY;
  BEGIN
    root := self.db.getRoot();
    IF root = NIL THEN
      root := NEW(TextRefBPlusTree.T).init();
      self.db.setRoot(root);
    END;
    IF NOT root.get(name, object) THEN
      RAISE ObjectNameNotFound;
    END;
    RETURN object;
  END Lookup;

BEGIN
END ODMG.
