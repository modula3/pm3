UNSAFE MODULE Database EXPORTS Database, InternalDatabase;

IMPORT RTIO, Pathname, BaseDatabase,
       InternalTransaction, BaseTransaction, Transaction,
       TextRefTransientTbl AS TextRefTbl, RTProcess, RTParams,
       Lex, Scan, OSConfig, FloatMode, DB, RTCollector,
       RTHeapDB, RTHeapDep, Thread, ThreadF, BerkeleyDB, Word, M3toC, Uerror;

REVEAL
  T = Internal BRANDED "Database.T" OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(self: T): BaseDatabase.T =
  BEGIN
    EVAL BaseDatabase.T.init(self);
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
 
VAR
  dbs := NEW(TextRefTbl.Default).init();
  mutex := NEW(MUTEX);

PROCEDURE Create(name: Pathname.T)
  RAISES { Thread.Aborted, Exists } = 
  VAR
    db: T := NEW(T).init();
    tr: Transaction.T := NEW(Transaction.T).init();
  BEGIN
    LOCK mutex DO
      WITH result = BerkeleyDB.db_create(db.db, DB.dbenv, 0) DO
        IF result # 0 THEN fatalError("db_create: ", result) END;
      END;
      WITH result = BerkeleyDB.db_set_pagesize(db.db,
                                               RTHeapDep.BytesPerPage * 8) DO
        IF result # 0 THEN fatalError("db_set_pagesize: ", result) END;
      END;        
      WITH result = BerkeleyDB.db_set_re_len(db.db, RTHeapDep.BytesPerPage) DO
        IF result # 0 THEN fatalError("db_set_re_len: ", result) END;
      END;
      WITH result = BerkeleyDB.db_env_txn_begin(DB.dbenv,
                                                NIL, tr.dbtxn, 0) DO
        IF result # 0 THEN fatalError("db_env_txn_begin: ", result) END;
      END;
      WITH flags  = BerkeleyDB.DB_THREAD,
           flags  = Word.Or(flags, BerkeleyDB.DB_CREATE),
           flags  = Word.Or(flags, BerkeleyDB.DB_EXCL),
           path   = M3toC.SharedTtoS(name),
           result = BerkeleyDB.db_open(db.db, tr.dbtxn, path, NIL,
                                       BerkeleyDB.DBTYPE.DB_QUEUE,
                                       flags, 8_664) DO
        M3toC.FreeSharedS(name, path);
        IF result = Uerror.EEXIST THEN RAISE Exists;
        ELSIF result # 0 THEN fatalError("db_open: ", result) END;
      END;
      ThreadF.TxnBegin(tr);
      RTHeapDB.Flush(db);
      ThreadF.TxnCommit();
      WITH result = BerkeleyDB.db_txn_commit(tr.dbtxn, 0) DO
        tr.dbtxn := NIL;
        IF result # 0 THEN fatalError("db_txn_commit: ", result) END;
      END;
      WITH result = BerkeleyDB.db_close(db.db, 0) DO
        db.db := NIL;
        IF result # 0 THEN fatalError("db_close: ", result) END;
      END;
    END
  END Create;

PROCEDURE Open(name: Pathname.T): T
  RAISES { Opened, NotFound } =
  VAR
    ref: <*TRANSIENT*> REFANY;
    db: T;
  BEGIN
    LOCK mutex DO
      IF dbs.get(name, ref) THEN
        db := ref;
        IF db.db # NIL THEN RAISE Opened END;
      ELSE
        db := NEW(T).init();
        WITH result = BerkeleyDB.db_create(db.db, DB.dbenv, 0) DO
          IF result # 0 THEN fatalError("db_create: ", result) END;
        END;
        WITH flags  = Word.Or(BerkeleyDB.DB_THREAD, BerkeleyDB.DB_AUTO_COMMIT),
             path   = M3toC.SharedTtoS(name),
             result = BerkeleyDB.db_open(db.db, NIL, path, NIL,
                                         BerkeleyDB.DBTYPE.DB_QUEUE,
                                         flags, 8_664) DO
          M3toC.FreeSharedS(name, path);
          IF result = Uerror.ENOENT THEN
            EVAL BerkeleyDB.db_close(db.db, 0);
            db.db := NIL;
            RAISE NotFound;
          ELSIF result # 0 THEN fatalError("db_open: ", result) END;
        END;
        EVAL dbs.put(name, db);
      END;
    END;
    RETURN db;
  END Open;

PROCEDURE Close () =
  BEGIN
    RTCollector.Disable();
    LOCK mutex DO
      VAR it := dbs.iterate();
          k: TEXT;
          v: <*TRANSIENT*> REFANY;
      BEGIN
        WHILE it.next(k, v) DO
          VAR db: T := v;
          BEGIN
            IF db.db # NIL THEN
              WITH result = BerkeleyDB.db_close(db.db, 0) DO
                db.db := NIL;
                IF result # 0 THEN fatalError("db_close: ", result) END;
              END
            END
          END
        END
      END;
      WITH result = BerkeleyDB.db_env_close(DB.dbenv, 0) DO
        DB.dbenv := NIL;
        IF result # 0 THEN fatalError("db_env_close: ", result) END;
      END
    END
  END Close;

BEGIN
  WITH result = BerkeleyDB.db_env_create(DB.dbenv, 0) DO
    IF result # 0 THEN fatalError("db_env_create: ", result) END;
  END;
  WITH flags = BerkeleyDB.DB_CREATE,
       flags = Word.Or(flags, BerkeleyDB.DB_INIT_MPOOL),
       flags = Word.Or(flags, BerkeleyDB.DB_INIT_LOCK),
       flags = Word.Or(flags, BerkeleyDB.DB_INIT_LOG),
       flags = Word.Or(flags, BerkeleyDB.DB_INIT_TXN),
       flags = Word.Or(flags, BerkeleyDB.DB_RECOVER),
       flags = Word.Or(flags, BerkeleyDB.DB_THREAD),
       result = BerkeleyDB.db_env_open(DB.dbenv, NIL, flags, 8_664) DO
    IF result # 0 THEN fatalError("db_env_open: ", result) END;
  END;
  BerkeleyDB.db_env_set_errfile(DB.dbenv, DB.stderr);
  IF RTParams.IsPresent("cachesize") THEN
    TRY
      WITH cachesize = Scan.Int(RTParams.Value("cachesize")),
           result = BerkeleyDB.db_env_set_cachesize(DB.dbenv, 0, cachesize, 0)
       DO
        IF result # 0 THEN fatalError("db_env_set_cachesize: ", result) END;
      END;
    EXCEPT
    | FloatMode.Trap, Lex.Error =>
      RTIO.PutText("@M3cachesize requires an integer argument");
      RTIO.PutChar('\n');
      RTIO.Flush();
    END;
  END;
  RTProcess.RegisterExitor (Close);
END Database.
