(* Copyright (C) 2005, Purdue Research Foundation           *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)

INTERFACE RTHeapDB;

IMPORT RTHeapDep, Fingerprint;
FROM RTHeapDep IMPORT Page, BytesPerPage;
FROM RT0 IMPORT Typecode;

TYPE
  Byte = BITS 8 FOR [0..16_FF];
  PageData = ARRAY [0..BytesPerPage-1] OF Byte;

  Releaser = PROCEDURE(page: DBPage);
  PageIn = PROCEDURE(page: DBPage; READONLY data: PageData);
  PageOut = PROCEDURE(page: DBPage; VAR data: PageData);

  DBPage = MUTEX OBJECT
    p: Page;				 (* heap page *)
    db: DB;				 (* database *)
    id: Page;				 (* DB page *)
  METHODS
    readAccess(p: Releaser);
    writeAccess(p: Releaser);
    peek(p: PageIn);
    read(p: PageIn);
    write(p: PageOut);
  END;

  DBRoot = OBJECT
    ref: REFANY := NIL;
  END;

  DB = MUTEX OBJECT
    <*TRANSIENT*> root: DBRoot;
  METHODS
    mapFP(READONLY fp: Fingerprint.T; VAR fpRef: REF Fingerprint.T;
          VAR fpAdr: ADDRESS);
    newPage(p: Page; id: Page := 0): DBPage;
    pageMapGet(id: Page): DBPage;
    pageMapPut(page: DBPage);
    pageMapDel(page: DBPage);
    newId(n: CARDINAL): Page;
    rootTC(): Typecode;
  END;

TYPE
  Txn = MUTEX OBJECT
    parent: Txn;
    page: Page := 0;		       (* current allocation page of txn *)
    next: ADDRESS := NIL;	       (* address of next available byte *)
    limit: ADDRESS := NIL;	       (* address of first unavailable byte *)
  END;

VAR myTxn: Txn; (* The txn of the currently running thread *)

PROCEDURE RefPageMap(object: REFANY): DBPage;

PROCEDURE Transfer(from, to: Txn);
PROCEDURE Begin(child: Txn);
PROCEDURE Abort();
PROCEDURE Commit();
PROCEDURE Flush(db: DB := NIL);
PROCEDURE SwizzleRoot(db: DB): REFANY;

END RTHeapDB.
