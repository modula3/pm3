INTERFACE BaseDatabase;

IMPORT Database, RTHeapDB;

REVEAL
  Database.Private = RTHeapDB.DB BRANDED "Database.Private" OBJECT
  METHODS
    init(): T;
  END;

TYPE
  T <: Database.Public;

END BaseDatabase.
