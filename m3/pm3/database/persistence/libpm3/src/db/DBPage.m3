UNSAFE MODULE DBPage EXPORTS InternalDBPage, DBPage;

IMPORT BerkeleyDB, RTDB, BaseDBPage, Database, BaseDatabase,
       InternalDatabase, InternalTransaction, OSConfig, Thread, ThreadF,
       Transaction, BaseTransaction, RTIO, RTHeapDep, Word;

REVEAL
  T = Internal BRANDED "DBPage.T" OBJECT
  OVERRIDES
    init := Init;

    readAccess := ReadAccess;
    writeAccess := WriteAccess;
    peek := Peek;
    read := Read;
    write := Write;
  END;

PROCEDURE Init(self: T): BaseDBPage.T =
  BEGIN
    EVAL BaseDBPage.T.init(self);
    RETURN self;
  END Init;

EXCEPTION FatalError; <*FATAL FatalError*>
PROCEDURE fatalError (t: TEXT; i: INTEGER) RAISES {FatalError} =
  BEGIN
    RTIO.PutText(t);
    RTIO.PutString(BerkeleyDB.db_strerror(i));
    RTIO.PutText(OSConfig.LineSep);
    RTIO.Flush();
    RAISE FatalError;
  END fatalError;

PROCEDURE ReadAccess(self: T; <*UNUSED*>p: RTDB.Releaser)
  RAISES {Thread.Aborted} =
  VAR
    pageData: RTDB.PageData;
    recno: BerkeleyDB.db_recno_t := self.id;
    db: Database.T := self.db;
    txn: Transaction.T := ThreadF.myTxn;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno),
                             ulen := 0, dlen := 0, doff := 0, flags := 0 };
    data := BerkeleyDB.DBT { data := ADR(pageData[0]),
                             size := 0,
                             ulen := RTHeapDep.BytesPerPage,
                             dlen := 0, doff := 0,
                             flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                              BerkeleyDB.DB_DBT_PARTIAL)
    };
    
  BEGIN
    WITH result = BerkeleyDB.db_get(db.db, txn.dbtxn, key, data, 0) DO
      IF result = BerkeleyDB.DB_LOCK_DEADLOCK THEN Abort()
      ELSIF result = BerkeleyDB.DB_NOTFOUND THEN
        WITH result = BerkeleyDB.db_put(db.db, txn.dbtxn, key, data, 0) DO
          IF result = BerkeleyDB.DB_LOCK_DEADLOCK THEN Abort()
          ELSIF result # 0 THEN fatalError("db_put: ", result) END;
        END;
      ELSIF result # 0 THEN fatalError("db_get: ", result) END;
    END;
  END ReadAccess;

PROCEDURE WriteAccess(self: T; <*UNUSED*>p: RTDB.Releaser)
  RAISES {Thread.Aborted} =
  VAR
    pageData: RTDB.PageData;
    recno: BerkeleyDB.db_recno_t := self.id;
    db: Database.T := self.db;
    txn: Transaction.T := ThreadF.myTxn;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno),
                             ulen := 0, dlen := 0, doff := 0, flags := 0 };
    data := BerkeleyDB.DBT { data := ADR(pageData[0]),
                             size := 0,
                             ulen := RTHeapDep.BytesPerPage,
                             dlen := 0, doff := 0,
                             flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                              BerkeleyDB.DB_DBT_PARTIAL)
    };
  BEGIN
    WITH result = BerkeleyDB.db_put(db.db, txn.dbtxn, key, data, 0) DO
      IF result = BerkeleyDB.DB_LOCK_DEADLOCK THEN Abort()
      ELSIF result # 0 THEN fatalError("db_put: ", result) END;
    END;
  END WriteAccess;

PROCEDURE Peek(self: T; p: RTDB.Swizzler) RAISES {Thread.Aborted} =
  VAR
    pageData: RTDB.PageData;
    recno: BerkeleyDB.db_recno_t := self.id;
    db: Database.T := self.db;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno),
                             ulen := 0, dlen := 0, doff := 0, flags := 0 };
    data := BerkeleyDB.DBT { data := ADR(pageData[0]),
                             size := RTHeapDep.BytesPerPage,
                             ulen := RTHeapDep.BytesPerPage,
                             dlen := 0, doff := 0,
                             flags := BerkeleyDB.DB_DBT_USERMEM
    };
  BEGIN
    WITH result = BerkeleyDB.db_get(db.db, NIL, key, data, 0) DO
      IF result = BerkeleyDB.DB_LOCK_DEADLOCK THEN Abort()
      ELSIF result # 0 THEN fatalError("db_get: ", result) END;
    END;
    p(self, pageData);
  END Peek;

PROCEDURE Read(self: T; p: RTDB.Swizzler) RAISES {Thread.Aborted} =
  VAR
    pageData: RTDB.PageData;
    recno: BerkeleyDB.db_recno_t := self.id;
    db: Database.T := self.db;
    txn: Transaction.T := ThreadF.myTxn;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno),
                             ulen := 0, dlen := 0, doff := 0, flags := 0 };
    data := BerkeleyDB.DBT { data := ADR(pageData[0]),
                             size := RTHeapDep.BytesPerPage,
                             ulen := RTHeapDep.BytesPerPage,
                             dlen := 0, doff := 0,
                             flags := BerkeleyDB.DB_DBT_USERMEM
    };
  BEGIN
    WITH result = BerkeleyDB.db_get(db.db, txn.dbtxn, key, data, 0) DO
      IF result = BerkeleyDB.DB_LOCK_DEADLOCK THEN Abort()
      ELSIF result # 0 THEN fatalError("db_get: ", result) END;
    END;
    p(self, pageData);
  END Read;

PROCEDURE Write(self: T; p: RTDB.Unswizzler) RAISES {Thread.Aborted} =
  VAR
    pageData: RTDB.PageData;
    recno: BerkeleyDB.db_recno_t := self.id;
    db: Database.T := self.db;
    txn: Transaction.T := ThreadF.myTxn;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno),
                             ulen := 0, dlen := 0, doff := 0, flags := 0 };
    data := BerkeleyDB.DBT { data := ADR(pageData[0]),
                             size := RTHeapDep.BytesPerPage,
                             ulen := RTHeapDep.BytesPerPage,
                             dlen := 0, doff := 0,
                             flags := BerkeleyDB.DB_DBT_USERMEM
    };
  BEGIN
    p(self, pageData);
    WITH result = BerkeleyDB.db_put(db.db, txn.dbtxn, key, data, 0) DO
      IF result = BerkeleyDB.DB_LOCK_DEADLOCK THEN Abort()
      ELSIF result # 0 THEN fatalError("db_put: ", result) END;
    END;
  END Write;

PROCEDURE Abort() RAISES {Thread.Aborted} =
  <*FATAL Transaction.NotInProgress*>
  BEGIN
    NARROW(ThreadF.myTxn, Transaction.T).abort();
    RAISE Thread.Aborted;
  END Abort;

BEGIN
END DBPage.
