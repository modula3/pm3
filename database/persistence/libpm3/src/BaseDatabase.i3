INTERFACE BaseDatabase;

IMPORT Database, RTDB;

REVEAL
  Database.Private = RTDB.T BRANDED "Database.Private" OBJECT
  METHODS
    init(): T;
  END;

TYPE
  T <: Database.Public;

END BaseDatabase.
