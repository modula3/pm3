MODULE BaseDBPage;

IMPORT DBPage;

REVEAL
  T = DBPage.Public BRANDED "BaseDBPage.T" OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    RETURN self;
  END Init;

BEGIN
END BaseDBPage.
