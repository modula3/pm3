MODULE BaseTransaction;

IMPORT Transaction;

REVEAL
  T = Transaction.Public BRANDED "BaseTransaction.T" OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    RETURN self;
  END Init;

BEGIN
END BaseTransaction.
