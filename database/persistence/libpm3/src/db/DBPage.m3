UNSAFE MODULE DBPage EXPORTS InternalDBPage, DBPage;

IMPORT BerkeleyDB, BaseDBPage, Database, BaseDatabase,
       InternalDatabase, InternalTransaction, OSConfig, Thread,
       Transaction, BaseTransaction, RTIO, RTHeapDB, Word;
FROM RTHeapDep IMPORT BytesPerPage;

REVEAL
  T = Internal BRANDED "DBPage.T" OBJECT
    lsn := BerkeleyDB.DB_LSN { 0, 0 };
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
    EVAL Internal.init(self);
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

PROCEDURE error (t: TEXT; err: INTEGER) =
  BEGIN
    CASE err OF
    | BerkeleyDB.DB_LOCK_DEADLOCK =>
      <*FATAL Transaction.NotInProgress*>
      VAR txn: Transaction.T := RTHeapDB.myTxn;
      BEGIN
        txn.abort();
        RAISE Thread.Aborted;
      END;
    ELSE fatalError(t, err) END;
  END error;

PROCEDURE ReadAccess(self: T; p: RTHeapDB.Releaser) =
  VAR
    db: Database.T := self.db;
    txn: Transaction.T := RTHeapDB.myTxn;
    recno: BerkeleyDB.db_recno_t := self.id;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno) };
    new: BerkeleyDB.DB_LSN;
    data := BerkeleyDB.DBT { data := ADR(new), size := BYTESIZE(new),
                             ulen := BYTESIZE(new), dlen := BYTESIZE(new),
                             flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                              BerkeleyDB.DB_DBT_PARTIAL)
    };
    
  BEGIN
    WITH result = BerkeleyDB.db_get(db.db, txn.dbtxn, key, data, 0) DO
      IF result = 0 THEN
        IF BerkeleyDB.log_compare(self.lsn, new) # 0 AND p # NIL THEN
          p(self);
        END
      ELSIF result = BerkeleyDB.DB_NOTFOUND THEN
        <* ASSERT new = BerkeleyDB.DB_LSN {0, 0} *>
        WITH result = BerkeleyDB.db_put(db.db, txn.dbtxn, key, data, 0) DO
          IF result = 0 THEN RETURN END;
          error("ReadAccess: ", result);
        END
      ELSE
        error("ReadAccess: ", result);
      END;
    END
  END ReadAccess;

PROCEDURE WriteAccess(self: T; p: RTHeapDB.Releaser) =
  VAR
    db: Database.T := self.db;
    txn: Transaction.T := RTHeapDB.myTxn;
    recno: BerkeleyDB.db_recno_t := self.id;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno) };
    new: BerkeleyDB.DB_LSN;
    data := BerkeleyDB.DBT { data := ADR(new), size := BYTESIZE(new),
                             ulen := BYTESIZE(new), dlen := BYTESIZE(new),
                             flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                              BerkeleyDB.DB_DBT_PARTIAL)
    };
  BEGIN
    WITH result = BerkeleyDB.db_get(db.db, txn.dbtxn, key, data,
                                    BerkeleyDB.DB_RMW) DO
      IF result = 0 THEN
        IF BerkeleyDB.log_compare(self.lsn, new) # 0 AND p # NIL THEN
          p(self);
        END
      ELSIF result = BerkeleyDB.DB_NOTFOUND THEN
        <* ASSERT new = BerkeleyDB.DB_LSN {0, 0} *>
        WITH result = BerkeleyDB.db_put(db.db, txn.dbtxn, key, data, 0) DO
          IF result = 0 THEN RETURN END;
          error("WriteAccess: ", result);
        END
      ELSE        
        error("WriteAccess: ", result);
      END
    END
  END WriteAccess;

PROCEDURE Peek(self: T; p: RTHeapDB.PageIn) =
  VAR
    pageData := NEW(UNTRACED REF RTHeapDB.PageData);
    db: Database.T := self.db;
    recno: BerkeleyDB.db_recno_t := self.id;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno) };
    data := BerkeleyDB.DBT { data := ADR(pageData[0]),
                             size := BytesPerPage,
                             ulen := BytesPerPage,
                             dlen := BytesPerPage,
                             doff := BYTESIZE(BerkeleyDB.DB_LSN),
                             flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                              BerkeleyDB.DB_DBT_PARTIAL)
    };
  BEGIN
    TRY
      WITH flags = BerkeleyDB.DB_DIRTY_READ,
           result = BerkeleyDB.db_get(db.db, NIL, key, data, flags) DO
        IF result = 0 THEN
          p(self, pageData^);
          RETURN;
        END;
        error("Peek: ", result);
      END;
    FINALLY
      DISPOSE(pageData);
    END
  END Peek;

PROCEDURE Read(self: T; p: RTHeapDB.PageIn) =
  VAR
    pageData := NEW(UNTRACED REF RTHeapDB.PageData);
    db: Database.T := self.db;
    txn: Transaction.T := RTHeapDB.myTxn;
    recno: BerkeleyDB.db_recno_t := self.id;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno) };
    new: BerkeleyDB.DB_LSN;
    lsn := BerkeleyDB.DBT { data := ADR(new), size := BYTESIZE(new),
                            ulen := BYTESIZE(new), dlen := BYTESIZE(new),
                            flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                             BerkeleyDB.DB_DBT_PARTIAL)
    };
    data := BerkeleyDB.DBT { data := ADR(pageData[0]),
                             size := BytesPerPage,
                             ulen := BytesPerPage,
                             dlen := BytesPerPage,
                             doff := BYTESIZE(BerkeleyDB.DB_LSN),
                             flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                              BerkeleyDB.DB_DBT_PARTIAL)
    };
  BEGIN
    TRY
      WITH result = BerkeleyDB.db_get(db.db, txn.dbtxn, key, lsn, 0) DO
        IF result = 0 THEN
          self.lsn := new;
        ELSE
          error("Read: ", result);
        END;
      END;
      WITH result = BerkeleyDB.db_get(db.db, txn.dbtxn, key, data, 0) DO
        IF result = 0 THEN
          p(self, pageData^);
        ELSE
          error("Read: ", result);
        END;
      END;
    FINALLY
      DISPOSE(pageData);
    END
  END Read;

PROCEDURE Write(self: T; p: RTHeapDB.PageOut) =
  VAR
    oldData := NEW(UNTRACED REF RTHeapDB.PageData);
    newData := NEW(UNTRACED REF RTHeapDB.PageData);
    db: Database.T := self.db;
    txn: Transaction.T := RTHeapDB.myTxn;
    recno: BerkeleyDB.db_recno_t := self.id;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno) };
    newLSN: BerkeleyDB.DB_LSN;
    lsn := BerkeleyDB.DBT { data := ADR(newLSN), size := BYTESIZE(newLSN),
                            ulen := BYTESIZE(newLSN), dlen := BYTESIZE(newLSN),
                            flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                             BerkeleyDB.DB_DBT_PARTIAL)
    };
    new := BerkeleyDB.DBT { data := ADR(newData[0]),
                            size := BytesPerPage,
                            ulen := BytesPerPage,
                            dlen := BytesPerPage,
                            doff := BYTESIZE(BerkeleyDB.DB_LSN),
                            flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                             BerkeleyDB.DB_DBT_PARTIAL)
    };
    old := BerkeleyDB.DBT { data := ADR(oldData[0]),
                            size := BytesPerPage,
                            ulen := BytesPerPage,
                            dlen := BytesPerPage,
                            doff := BYTESIZE(BerkeleyDB.DB_LSN),
                            flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                             BerkeleyDB.DB_DBT_PARTIAL)
    };
  BEGIN
    TRY
      p(self, newData^);
      WITH result = BerkeleyDB.db_get(db.db, txn.dbtxn, key, old, 0) DO
        IF result = 0 THEN
          VAR
            i := 0;
            bsw := BYTESIZE(Word.T);
          BEGIN
            WHILE i < BytesPerPage DO
              IF SUBARRAY(newData^, i, bsw) = SUBARRAY(oldData^, i, bsw) THEN
                INC(i, bsw);
              ELSE
                new.data := ADR(newData[i]);
                new.doff := i;
                REPEAT
                  INC(i, bsw);
                  IF i >= BytesPerPage THEN EXIT END;
                UNTIL SUBARRAY(newData^, i, bsw) = SUBARRAY(oldData^, i, bsw);
                new.size := MIN(i, BytesPerPage) - new.doff;
                INC(new.doff, BYTESIZE(BerkeleyDB.DB_LSN));
                new.ulen := new.size;
                new.dlen := new.size;
                WITH result = BerkeleyDB.db_put(db.db, txn.dbtxn, key, new, 0) DO
                  IF result # 0 THEN
                    error("Write: ", result);
                    RETURN;
                  END
                END
              END
            END
          END
        ELSIF result = BerkeleyDB.DB_NOTFOUND THEN
          WITH result = BerkeleyDB.db_put(db.db, txn.dbtxn, key, new, 0) DO
            IF result # 0 THEN
              error("Write: ", result);
              RETURN;
            END
          END
        ELSE
          error("Write: ", result);
          RETURN;
        END
      END;
      BerkeleyDB.db_txn_last_lsn(txn.dbtxn, newLSN);
      WITH result = BerkeleyDB.db_put(db.db, txn.dbtxn, key, lsn, 0) DO
        IF result # 0 THEN
          error("Write: ", result);
          RETURN;
        END
      END;
      self.lsn := newLSN;
    FINALLY
      DISPOSE(newData);
      DISPOSE(oldData);
    END
  END Write;

BEGIN
END DBPage.
