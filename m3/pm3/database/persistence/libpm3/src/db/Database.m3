UNSAFE MODULE Database EXPORTS Database, InternalDatabase;

IMPORT RTIO, Pathname, BaseDatabase, Text,
       InternalTransaction, BaseTransaction, Transaction,
       TextRefTransientTbl AS TextRefTbl, RTProcess, RTParams,
       Lex, Scan, OSConfig, FloatMode, DB, RTCollector, Scheduler,
       RTHeapDB, Thread, BerkeleyDB, Word, M3toC, Uerror, Cstdlib, ThreadF;
FROM RTHeapDep IMPORT BytesPerPage;

REVEAL
  T = Internal BRANDED "Database.T" OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(self: T): BaseDatabase.T =
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
      WITH pagesize = BytesPerPage * 2,
           result = BerkeleyDB.db_set_pagesize(db.db, pagesize) DO
        IF result # 0 THEN fatalError("db_set_pagesize: ", result) END;
      END;        
      WITH len = BYTESIZE(BerkeleyDB.DB_LSN) + BytesPerPage,
           result = BerkeleyDB.db_set_re_len(db.db, len) DO
        IF result # 0 THEN fatalError("db_set_re_len: ", result) END;
      END;
      WITH result = BerkeleyDB.db_env_txn_begin(DB.dbenv,
                                                NIL, tr.dbtxn, 0) DO
        IF result # 0 THEN fatalError("db_env_txn_begin: ", result) END;
      END;
      WITH flags  = BerkeleyDB.DB_THREAD,
           flags  = Word.Or(flags, BerkeleyDB.DB_CREATE),
           flags  = Word.Or(flags, BerkeleyDB.DB_EXCL),
           flags  = Word.Or(flags, BerkeleyDB.DB_DIRTY_READ),
           path   = M3toC.SharedTtoS(name),
           result = BerkeleyDB.db_open(db.db, tr.dbtxn, path, NIL,
                                       BerkeleyDB.DBTYPE.DB_QUEUE,
                                       flags, 8_664) DO
        M3toC.FreeSharedS(name, path);
        IF result = Uerror.EEXIST THEN RAISE Exists;
        ELSIF result # 0 THEN fatalError("db_open: ", result) END;
      END;
      RTHeapDB.Begin(tr);
      RTHeapDB.Flush(db);
      RTHeapDB.Commit();
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

PROCEDURE yield(): INTEGER =
  BEGIN
    Scheduler.Yield();
    RETURN 0;
  END yield;

TYPE DeadlockDetector = Thread.Closure OBJECT
  interval := 0.5D0;
  atype: BerkeleyDB.u_int32_t;
  aborts := 0;
OVERRIDES
  apply := DeadlockDetect;
END;

PROCEDURE DeadlockDetect(self: DeadlockDetector): REFANY =
  VAR aborted: BerkeleyDB.int;
  BEGIN
    LOOP
      Thread.Pause(self.interval);
      WITH result
        = BerkeleyDB.db_env_lock_detect(DB.dbenv, 0, self.atype, aborted) DO
        IF result # 0 THEN fatalError("db_env_lock_detect: ", result) END;
      END;        
      INC(self.aborts, aborted);
    END;
  END DeadlockDetect;

PROCEDURE Checkpoint() =
  BEGIN
    LOCK mutex DO
      WITH result = BerkeleyDB.db_env_txn_checkpoint(DB.dbenv, 0, 0, 0) DO
        IF result # 0 THEN fatalError("db_env_txn_checkpoint: ", result) END
      END
    END
  END Checkpoint;

PROCEDURE Malloc (size: INTEGER): ADDRESS =
  VAR res: ADDRESS;
  BEGIN
    ThreadF.SuspendOthers();
    res := Cstdlib.malloc(size);
    ThreadF.ResumeOthers();
    RETURN res;
  END Malloc;

PROCEDURE Realloc (ptr: ADDRESS; size: INTEGER): ADDRESS =
  VAR res: ADDRESS;
  BEGIN
    ThreadF.SuspendOthers();
    res := Cstdlib.realloc(ptr, size);
    ThreadF.ResumeOthers();
    RETURN res;
  END Realloc;

PROCEDURE Free (ptr: ADDRESS) =
  BEGIN
    ThreadF.SuspendOthers();
    Cstdlib.free(ptr);
    ThreadF.ResumeOthers();
  END Free;

VAR flags: BerkeleyDB.u_int32_t;
BEGIN
  (* db_env_create *)
  WITH result = BerkeleyDB.db_env_create(ADR(DB.dbenv), 0) DO
    IF result # 0 THEN fatalError("db_env_create: ", result) END;
  END;

  (* db_env_set_alloc *)
  WITH result = BerkeleyDB.db_env_set_alloc(DB.dbenv, Malloc, Realloc, Free) DO
    IF result # 0 THEN fatalError("db_env_set_malloc: ", result) END;
  END;

  (* db_env_set_func_yield *)
  WITH result = BerkeleyDB.db_env_set_func_yield(yield) DO
    IF result # 0 THEN fatalError("db_env_set_func_yield", result) END;
  END;

  (* db_set_lk_maxlocks *)
  WITH db_maxlocks = RTParams.Value("DB_MAXLOCKS") DO
    IF db_maxlocks # NIL THEN
      TRY
        WITH i = Scan.Int(db_maxlocks),
             result = BerkeleyDB.db_env_set_lk_max_locks(DB.dbenv, i) DO
          IF result # 0 THEN fatalError("db_env_set_lk_max_locks", result) END;
        END
      EXCEPT
      | Lex.Error, FloatMode.Trap =>
        RTIO.PutText("@M3DB_MAXLOCKS ");
        RTIO.PutText(db_maxlocks);
        RTIO.PutText(" is invalid");
        RTIO.PutText(OSConfig.LineSep);
        RTIO.Flush();
        RAISE FatalError;
      END
    END
  END;

  (* db_env_set_lk_detect *)
  WITH db_lock = RTParams.Value("DB_LOCK") DO
    IF db_lock = NIL OR Text.Equal(db_lock, "DEFAULT") THEN
      flags := BerkeleyDB.DB_LOCK_DEFAULT;
    ELSIF Text.Equal(db_lock, "EXPIRE") THEN
      flags := BerkeleyDB.DB_LOCK_EXPIRE;
    ELSIF Text.Equal(db_lock, "MAXLOCKS") THEN
      flags := BerkeleyDB.DB_LOCK_MAXLOCKS;
    ELSIF Text.Equal(db_lock, "MINLOCKS") THEN
      flags := BerkeleyDB.DB_LOCK_MINLOCKS;
    ELSIF Text.Equal(db_lock, "MINWRITE") THEN
      flags := BerkeleyDB.DB_LOCK_MINWRITE;
    ELSIF Text.Equal(db_lock, "OLDEST") THEN
      flags := BerkeleyDB.DB_LOCK_OLDEST;
    ELSIF Text.Equal(db_lock, "RANDOM") THEN
      flags := BerkeleyDB.DB_LOCK_RANDOM;
    ELSIF Text.Equal(db_lock, "YOUNGEST") THEN
      flags := BerkeleyDB.DB_LOCK_YOUNGEST;
    ELSE
      RTIO.PutText("@M3DB_LOCK mode ");
      RTIO.PutText(db_lock);
      RTIO.PutText(" is invalid");
      RTIO.PutText(OSConfig.LineSep);
      RTIO.Flush();
      RAISE FatalError;
    END;
    WITH db_detect = RTParams.Value("DB_DETECT") DO
      IF db_detect = NIL THEN
        WITH result = BerkeleyDB.db_env_set_lk_detect(DB.dbenv, flags) DO
          IF result # 0 THEN fatalError("db_env_set_lk_detect: ", result) END;
        END;
      ELSIF Text.Equal(db_detect, "none") THEN
        (* skip *)
      ELSE
        TRY
          WITH i = Scan.Int(db_detect) DO
            EVAL Thread.Fork(NEW(DeadlockDetector, atype := flags,
                                 interval := FLOAT(i, LONGREAL) / 1000.0D0));
          END
        EXCEPT
        | Lex.Error, FloatMode.Trap =>
          RTIO.PutText("@M3DB_DETECT ");
          RTIO.PutText(db_detect);
          RTIO.PutText(" is invalid");
          RTIO.PutText(OSConfig.LineSep);
          RTIO.Flush();
          RAISE FatalError;
        END
      END
    END
  END;

  (* db_env_open *)
  flags := BerkeleyDB.DB_CREATE;
  flags := Word.Or(flags, BerkeleyDB.DB_INIT_MPOOL);
  flags := Word.Or(flags, BerkeleyDB.DB_INIT_LOCK);
  flags := Word.Or(flags, BerkeleyDB.DB_INIT_LOG);
  flags := Word.Or(flags, BerkeleyDB.DB_INIT_TXN);
  flags := Word.Or(flags, BerkeleyDB.DB_THREAD);
  IF RTParams.IsPresent("DB_USE_ENVIRON") THEN
    flags := Word.Or(flags, BerkeleyDB.DB_USE_ENVIRON);
  END;
  IF RTParams.IsPresent("DB_USE_ENVIRON_ROOT") THEN
    flags := Word.Or(flags, BerkeleyDB.DB_USE_ENVIRON_ROOT);
  END;
  IF RTParams.IsPresent("DB_RECOVER") THEN
    flags := Word.Or(flags, BerkeleyDB.DB_RECOVER);
  END;
  IF RTParams.IsPresent("DB_RECOVER_FATAL") THEN
    flags := Word.Or(flags, BerkeleyDB.DB_RECOVER_FATAL);
  END;
  WITH db_home = RTParams.Value("DB_HOME"),
       path = M3toC.SharedTtoS(db_home),
       result = BerkeleyDB.db_env_open(DB.dbenv, path, flags, 8_664) DO
    M3toC.FreeSharedS(db_home, path);
    IF result # 0 THEN fatalError("db_env_open: ", result) END;
  END;
  
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
