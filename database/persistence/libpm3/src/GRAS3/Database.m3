MODULE Database EXPORTS Database, InternalDatabase;

IMPORT RTIO, Pathname, Access, PageFile,
       BaseDatabase, InternalTransaction, BaseTransaction,
       VirtualResourceSystem, VirtualResource,
       VirtualRemoteFile, Txn,
       TextRefTransientTbl AS TextRefTbl,
       Atom, AtomList, RTProcess, RTParams, Lex, Scan, Env, Config, FloatMode,
       DB, Transaction, RTCollector,
       RTHeapDB, Thread;

FROM Text IMPORT Length;
FROM DB IMPORT resource;

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
PROCEDURE fatalAtoms (t: AtomList.T) RAISES {FatalError} =
  BEGIN
    WHILE t # NIL DO
      RTIO.PutText(Atom.ToText(t.head)); RTIO.PutChar('\n');
      t := t.tail;
    END;
    RTIO.Flush();
    RAISE FatalError;
  END fatalAtoms;
 
PROCEDURE errorText (t: TEXT) =
  BEGIN
    RTIO.PutText(t); RTIO.PutChar('\n'); RTIO.Flush();
  END errorText;

VAR
  dbs := NEW(TextRefTbl.Default).init();
  mutex := NEW(MUTEX);

PROCEDURE Create(name: Pathname.T)
  RAISES { Exists, Thread.Aborted, Transaction.InProgress } = 
  VAR
    db: T := NEW(T).init();
    tr := NEW(Transaction.T).init();
  BEGIN
    LOCK mutex DO
      TRY
        IF resource.getTransactionLevel() # Txn.EnvelopeLevel THEN
          RAISE Transaction.InProgress;
        END;
        IF resource.existsFile (name, local := FALSE) THEN
          RAISE Exists;
        END;
        db.file :=
            NEW(VirtualRemoteFile.T).open (resource, name,
                                           Access.Mode.ReadWriteExclusive,
                                           Access.Kind.Data,
                                           new := TRUE);
        resource.beginTransaction();
        RTHeapDB.Begin(tr);
        TRY
          RTHeapDB.Flush(db)
        EXCEPT
        | Thread.Aborted =>
          resource.deleteFile(name, local := FALSE);
          RAISE Thread.Aborted;
        END;
        db.file.close();
        db.file := NIL;
        resource.commitTransaction(); <*NOWARN*> (* NotInTransaction *)
        RTHeapDB.Commit();
        IF dbs.put(name, db) THEN
          <* ASSERT FALSE *>
        END;
      EXCEPT
      | Access.Denied(t) => errorText(t); RAISE FatalError;
      | PageFile.NoAccess(t) => errorText(t); RAISE FatalError;
      | VirtualResource.FatalError(t) => fatalAtoms(t);
      END
    END
  END Create;

PROCEDURE Open(name: Pathname.T): T
  RAISES { NotFound, Opened, Transaction.InProgress } =
  VAR
    self: T;
    ref: <*TRANSIENT*> REFANY;
  BEGIN
    LOCK mutex DO
      TRY
        IF resource.getTransactionLevel() # Txn.EnvelopeLevel THEN
          RAISE Transaction.InProgress;
        END;
        IF NOT resource.existsFile (name, local := FALSE) THEN
          RAISE NotFound;
        END;
        IF dbs.get(name, ref) THEN
          self := ref;
          IF self.file # NIL THEN RAISE Opened END;
        ELSE
          self := NEW(T).init();
          EVAL dbs.put(name, self);
        END;
        self.file :=
            NEW(VirtualRemoteFile.T).open(resource, name,
                                          Access.Mode.ReadWriteShared,
                                          Access.Kind.Data, new := FALSE);
      EXCEPT
      | Access.Denied(t) => errorText(t); RAISE FatalError;
      | PageFile.NoAccess(t) => errorText(t); RAISE FatalError;
      | VirtualResource.FatalError(t) => fatalAtoms(t);
      END
    END;
    RETURN self;
  END Open;

PROCEDURE Close () =
  BEGIN
    RTCollector.Disable();
    WHILE resource.getTransactionLevel() # Txn.EnvelopeLevel DO
      TRY
        resource.abortTransaction();
      EXCEPT
      | VirtualResource.FatalError(t) => errorText(Atom.ToText(t.head))
      | VirtualResource.NotInTransaction =>
      END
    END;
    LOCK mutex DO
      VAR it := dbs.iterate();
          k: TEXT;
          v: <*TRANSIENT*> REFANY;
      BEGIN
        WHILE it.next(k, v) DO
          TRY
            VAR db: T := v;
            BEGIN
              IF db.file # NIL THEN
                db.file.close();
                db.file := NIL;
              END
            END
          EXCEPT
          | VirtualResource.FatalError(t) => errorText(Atom.ToText(t.head));
          END
        END
      END
    END;
    TRY
      resource.close();
    EXCEPT
    | VirtualResource.FatalError(t) => errorText(Atom.ToText(t.head));
    END
  END Close;

BEGIN
  VAR
    root := Env.Get("PM3ROOT");
    cacheSize := Config.DefaultCacheSize;
    agent := Config.DefaultNameServer;
    newres: BOOLEAN;
  BEGIN
    IF RTParams.IsPresent("root") THEN
      root := RTParams.Value("root");
    END;
    IF root = NIL OR Length(root) = 0 THEN
      root := Pathname.Current;
    END;
    IF RTParams.IsPresent("cachesize") THEN
      TRY
        cacheSize := Scan.Int(RTParams.Value("cachesize"));
      EXCEPT
      | FloatMode.Trap, Lex.Error =>
        RTIO.PutText("@M3cachesize requires an integer argument");
        RTIO.PutChar('\n');
        RTIO.Flush();
      END;
    END;
    IF RTParams.IsPresent("agent") THEN
      agent := RTParams.Value("agent");
    END;
    VirtualResourceSystem.Login(root, cacheSize, nameserver := agent);
    TRY
      newres := NOT VirtualResourceSystem.ExistsResource("PM3");
    EXCEPT
    | VirtualResourceSystem.FatalError(t) => fatalAtoms(t);
    END;
    TRY
      resource := NEW(VirtualResource.T).open("PM3",
                                              Access.Mode.ReadWriteShared,
                                              new := newres);
    EXCEPT
    | Access.Denied(t) => errorText(t); RAISE FatalError;
    | PageFile.NoAccess(t) => errorText(t); RAISE FatalError;
    END;
  END;
  RTProcess.RegisterExitor (Close);
END Database.
