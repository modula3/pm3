INTERFACE BaseDBPage;

IMPORT RTHeapDB, DBPage;

REVEAL
  DBPage.Private = RTHeapDB.DBPage BRANDED "DBPage.Private" OBJECT
  METHODS
    init(): T;
  END;

TYPE
  T <: DBPage.Public;

END BaseDBPage.
