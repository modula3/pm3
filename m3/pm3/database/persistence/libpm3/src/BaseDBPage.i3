INTERFACE BaseDBPage;

IMPORT RTDB, DBPage;

REVEAL
  DBPage.Private = RTDB.Page BRANDED "DBPage.Private" OBJECT
  METHODS
    init(): T;
  END;

TYPE
  T <: DBPage.Public;

END BaseDBPage.
