MODULE DBPage EXPORTS InternalDBPage, DBPage;

IMPORT RTDB, InternalVirtualPage, BaseDBPage, Database, BaseDatabase,
       InternalDatabase, InternalTransaction;
IMPORT PageCache, PageData;
IMPORT Thread, ThreadF, Transaction, BaseTransaction;
IMPORT Access, VirtualPage, Atom, AtomList, RTIO;

REVEAL
  T = Internal BRANDED "DBPage.T" OBJECT
    pageAge: CARDINAL := 0;
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
PROCEDURE fatalAtoms (t: AtomList.T) RAISES {FatalError} =
  BEGIN
    WHILE t # NIL DO
      RTIO.PutText(Atom.ToText(t.head)); RTIO.PutChar('\n');
      t := t.tail;
    END;
    RTIO.Flush();
    RAISE FatalError;
  END fatalAtoms;
 
PROCEDURE ReadAccess(self: T; p: RTDB.Releaser) RAISES {Thread.Aborted} =
  VAR
    page := NARROW(self.db, Database.T).file.getPage(self.id - 1);
    newPageAge: CARDINAL;
  BEGIN
    PageCache.BeginAccess();
    TRY
      EVAL page.readAccess(newPageAge);
    EXCEPT
    | Access.Locked =>
      PageCache.EndAccess();
      Abort();
    | VirtualPage.FatalError(t) => fatalAtoms(t);
    END;
    IF self.pageAge # newPageAge THEN
      self.pageAge := newPageAge;
      p(self);
    END;
    PageCache.EndAccess();
  END ReadAccess;

PROCEDURE WriteAccess(self: T; <*UNUSED*>p: RTDB.Releaser)
  RAISES {Thread.Aborted} =
  VAR
    page := NARROW(self.db, Database.T).file.getPage(self.id - 1);
    newPageAge: CARDINAL;
  BEGIN
    PageCache.BeginAccess();
    TRY
      EVAL page.writeAccess(newPageAge);
    EXCEPT
    | Access.Locked =>
      PageCache.EndAccess();
      Abort();
    | VirtualPage.FatalError(t) => fatalAtoms(t);
    END;
    PageCache.EndAccess();
  END WriteAccess;

PROCEDURE Peek(self: T; p: RTDB.Swizzler) RAISES {Thread.Aborted} =
  VAR page := NARROW(self.db, Database.T).file.getPage(self.id - 1);
  PROCEDURE P(READONLY data: PageData.T) = BEGIN p(self, data) END P;
  BEGIN
    (* use getAll until peekAll is implemented *)
    (* page.peekAll(p); *)
    TRY
      page.getAll(P);
    EXCEPT
    | Access.Locked => Abort();
    | VirtualPage.FatalError(t) => fatalAtoms(t);
    END
  END Peek;

PROCEDURE Read(self: T; p: RTDB.Swizzler) RAISES {Thread.Aborted} =
  VAR page := NARROW(self.db, Database.T).file.getPage(self.id - 1);
  PROCEDURE P(READONLY data: PageData.T) = BEGIN p(self, data) END P;
  BEGIN
    TRY
      page.getAll(P);
    EXCEPT
    | Access.Locked => Abort();
    | VirtualPage.FatalError(t) => fatalAtoms(t);
    END
  END Read;

PROCEDURE Write(self: T; p: RTDB.Unswizzler) RAISES {Thread.Aborted} =
  VAR page := NARROW(self.db, Database.T).file.getPage(self.id - 1);
  PROCEDURE P(VAR data: PageData.T) = BEGIN p(self, data) END P;
  BEGIN
    TRY
      page.putAll(P);
    EXCEPT
    | Access.Locked => Abort();
    | VirtualPage.FatalError(t) => fatalAtoms(t);
    END
  END Write;

PROCEDURE Abort() RAISES {Thread.Aborted} =
  <*FATAL Transaction.NotInProgress*>
  BEGIN
    NARROW(ThreadF.myTxn, Transaction.T).abort();
    RAISE Thread.Aborted;
  END Abort;

BEGIN
END DBPage.
