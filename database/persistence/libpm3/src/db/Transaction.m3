MODULE Transaction EXPORTS Transaction, InternalTransaction;

IMPORT BerkeleyDB, RTIO, RTHeapDB, DB, BaseTransaction, Thread,
       Transaction, OSConfig, DBPage, BaseDBPage, InternalDBPage;

REVEAL
  T = Internal BRANDED "Transaction.T" OBJECT
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
 
PROCEDURE Begin(self: T)
  RAISES { Thread.Aborted, InProgress } =
  BEGIN
    RTHeapDB.Flush();
    LOCK self DO
      IF self.dbtxn # NIL THEN RAISE InProgress END;
      VAR parent: BerkeleyDB.DB_TXN;
      BEGIN
        TYPECASE RTHeapDB.myTxn OF
        | Transaction.T(p) =>
          parent := p.dbtxn;
        ELSE parent := NIL END;
        EVAL self.init();
        WITH result = BerkeleyDB.db_env_txn_begin(DB.dbenv,
                                                  parent, self.dbtxn, 0) DO
          IF result # 0 THEN fatalError("db_env_txn_begin: ", result) END
        END
      END
    END;
    RTHeapDB.Begin(self);
  END Begin;

PROCEDURE Commit(self: T)
  RAISES { NotInProgress, Thread.Aborted } =
  BEGIN
    RTHeapDB.Flush();
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
      WITH result = BerkeleyDB.db_txn_commit(self.dbtxn, 0) DO
        self.dbtxn := NIL;
        IF result # 0 THEN fatalError("db_txn_commit: ", result) END
      END;
    END;
    RTHeapDB.Commit();
  END Commit;

PROCEDURE Chain(self: T)
  RAISES { NotInProgress, Thread.Aborted } =
  BEGIN
    RTHeapDB.Flush();
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
      WITH result = BerkeleyDB.db_txn_commit(self.dbtxn, 0) DO
        self.dbtxn := NIL;
        IF result # 0 THEN fatalError("db_txn_commit: ", result) END
      END;
      VAR parent: BerkeleyDB.DB_TXN;
      BEGIN
        TYPECASE self.parent OF
        | NULL => parent := NIL;
        | Transaction.T(p) => parent := p.dbtxn;
        ELSE parent := NIL END;
        WITH result = BerkeleyDB.db_env_txn_begin(DB.dbenv,
                                                  parent, self.dbtxn, 0) DO
          IF result # 0 THEN fatalError("db_env_txn_begin: ", result) END
        END
      END
    END
  END Chain; 

PROCEDURE Abort(self: T) RAISES { NotInProgress } =
  BEGIN
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
      WITH result = BerkeleyDB.db_txn_abort(self.dbtxn) DO
        self.dbtxn := NIL;
        IF result # 0 THEN fatalError("db_txn_abort: ", result) END
      END;
    END;
    RTHeapDB.Abort();
  END Abort;

PROCEDURE Checkpoint(self: T)
  RAISES { NotInProgress, Thread.Aborted } =
  BEGIN
    RTHeapDB.Flush();
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
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
  BEGIN
    IF object = NIL THEN RETURN END;
    LOCK self DO
      IF self.dbtxn = NIL THEN RAISE NotInProgress END;
    END;
    WITH p = NARROW(RTHeapDB.RefPageMap(object), DBPage.T) DO
      IF p # NIL THEN
        CASE mode OF
        | LockMode.READ  => p.readAccess(NIL);
        | LockMode.WRITE => p.writeAccess(NIL);
        END
      END
    END
  END Lock;

BEGIN
END Transaction.
