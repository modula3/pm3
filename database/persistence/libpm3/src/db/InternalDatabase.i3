INTERFACE InternalDatabase;

IMPORT Database, BaseDatabase, BerkeleyDB;

REVEAL
  Database.T <: Internal;

TYPE
  Internal = BaseDatabase.T OBJECT
    db: BerkeleyDB.DB;
  END;

END InternalDatabase.
