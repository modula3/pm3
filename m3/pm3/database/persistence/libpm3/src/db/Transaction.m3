UNSAFE MODULE Transaction EXPORTS Transaction, InternalTransaction;

IMPORT BerkeleyDB, RTIO, RTHeapDB, DB, BaseTransaction, Thread, ThreadF,
       Transaction, OSConfig, DBPage, BaseDBPage, InternalDBPage, RTDB,
       BaseDatabase, Database, InternalDatabase, RTHeapDep, Word;

REVEAL
  T = Internal BRANDED "Transaction.T" OBJECT
    open := FALSE;
  OVERRIDES
    init := Init;

    begin := Begin;
    commit := Commit;
    chain := Chain;
    abort := Abort;
    checkpoint := Checkpoint;
    isOpen := IsOpen;
    lock := Lock;
  END;

PROCEDURE Init(self: T): BaseTransaction.T =
  BEGIN
    EVAL BaseTransaction.T.init(self);
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
 
PROCEDURE Begin(self: T)
  RAISES { Thread.Aborted, InProgress } =
  VAR parent: BerkeleyDB.DB_TXN := NIL;
  BEGIN
    LOCK self DO
      IF self.dbtxn # NIL THEN RAISE InProgress END;
      IF ThreadF.myTxn # NIL THEN
        parent := NARROW(ThreadF.myTxn, Transaction.T).dbtxn;
        RTHeapDB.Flush(release := FALSE);
      END;
      EVAL self.init();
      WITH result = BerkeleyDB.db_env_txn_begin(DB.dbenv,
                                                parent, self.dbtxn, 0) DO
        IF result # 0 THEN fatalError("db_env_txn_begin: ", result) END
      END;
      ThreadF.TxnBegin(self);
    END
  END Begin;

PROCEDURE Commit(self: T)
  RAISES { NotInProgress, Thread.Aborted } =
  BEGIN
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
      RTHeapDB.Flush();
      ThreadF.TxnCommit();
      WITH result = BerkeleyDB.db_txn_commit(self.dbtxn, 0) DO
        self.dbtxn := NIL;
        IF result # 0 THEN fatalError("db_txn_commit: ", result) END
      END;
    END;
  END Commit;

PROCEDURE Chain(self: T)
  RAISES { NotInProgress, Thread.Aborted } =
  VAR parent: BerkeleyDB.DB_TXN := NIL;
  BEGIN
    LOCK self DO
      RTHeapDB.Flush(release := FALSE);
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
      WITH result = BerkeleyDB.db_txn_commit(self.dbtxn, 0) DO
        self.dbtxn := NIL;
        IF result # 0 THEN fatalError("db_txn_commit: ", result) END
      END;
      IF self.parent # NIL THEN
        parent := NARROW(self.parent, Transaction.T).dbtxn;
      END;
      WITH result = BerkeleyDB.db_env_txn_begin(DB.dbenv,
                                                parent, self.dbtxn, 0) DO
        IF result # 0 THEN fatalError("db_env_txn_begin: ", result) END
      END;
    END
  END Chain; 

PROCEDURE Abort(self: T) RAISES { NotInProgress } =
  BEGIN
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
      RTHeapDB.Abort();
      WITH result = BerkeleyDB.db_txn_abort(self.dbtxn) DO
        self.dbtxn := NIL;
        IF result # 0 THEN fatalError("db_txn_abort: ", result) END
      END;
      ThreadF.TxnAbort();
    END;
  END Abort;

PROCEDURE Checkpoint(self: T)
  RAISES { NotInProgress, Thread.Aborted } =
  BEGIN
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
      RTHeapDB.Flush(release := FALSE);
(*
      WITH result = BerkeleyDB.db_env_txn_checkpoint(DB.dbenv, 0, 0 , 0) DO
        IF result # 0 THEN fatalError("db_env_txn_checkpoint: ", result) END
      END;
*)
    END;
  END Checkpoint;

PROCEDURE IsOpen(self: T): BOOLEAN =
  BEGIN
    LOCK self DO
      RETURN self.dbtxn # NIL;
    END;
  END IsOpen;

PROCEDURE Lock(self: T; object: REFANY; mode: LockMode)
  RAISES { Thread.Aborted } =
  <* FATAL Transaction.NotInProgress *>
  VAR
    p: DBPage.T;
    pageData: RTDB.PageData;
    recno: BerkeleyDB.db_recno_t;
    db: Database.T;
    key  := BerkeleyDB.DBT { data := ADR(recno), size := BYTESIZE(recno),
                             ulen := 0, dlen := 0, doff := 0, flags := 0 };
    data := BerkeleyDB.DBT { data := ADR(pageData[0]),
                             size := RTHeapDep.BytesPerPage,
                             ulen := RTHeapDep.BytesPerPage,
                             dlen := 0, doff := 0,
                             flags := Word.Or(BerkeleyDB.DB_DBT_USERMEM,
                                              BerkeleyDB.DB_DBT_PARTIAL)
    };
  BEGIN
    IF object = NIL THEN RETURN END;
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
      p := NARROW(RTHeapDB.RefPageMap(object), DBPage.T);
      IF p # NIL THEN
        recno := p.id;
        db := p.db;
        CASE mode OF
        | LockMode.READ  =>
          WITH result = BerkeleyDB.db_get(db.db, NIL, key, data, 0) DO
            IF result = BerkeleyDB.DB_LOCK_DEADLOCK THEN
              self.abort();
              RAISE Thread.Aborted;
            ELSIF result # 0 THEN fatalError("db_get: ", result) END;
          END;
        | LockMode.WRITE =>
          WITH result = BerkeleyDB.db_put(db.db, NIL, key, data, 0) DO
            IF result = BerkeleyDB.DB_LOCK_DEADLOCK THEN
              self.abort();
              RAISE Thread.Aborted
            ELSIF result # 0 THEN fatalError("db_put: ", result) END;
          END;
        END
      END
    END
  END Lock;

BEGIN
END Transaction.
