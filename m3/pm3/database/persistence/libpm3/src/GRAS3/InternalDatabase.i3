INTERFACE InternalDatabase;

IMPORT Database, BaseDatabase, VirtualRemoteFile;

REVEAL
  Database.T <: Internal;

TYPE
  Internal = BaseDatabase.T OBJECT
    file: VirtualRemoteFile.T;
  END;

END InternalDatabase.
