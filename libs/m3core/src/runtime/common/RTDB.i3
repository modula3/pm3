(* Copyright (C) 1998, Purdue Research Foundation            *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE RTDB;

IMPORT RTHeapDep;
IMPORT Fingerprint;
IMPORT RTTxn;
IMPORT Thread;

TYPE
  Id = BITS 32 FOR [0 .. 16_7fffffff];
  PageData = RTHeapDep.PageData;
  Releaser = PROCEDURE(page: Page);
  Swizzler = PROCEDURE(page: Page; READONLY data: PageData);
  Unswizzler = PROCEDURE(page: Page; VAR data: PageData);
  Page = <*TRANSIENT*> ROOT OBJECT
    p: RTHeapDep.Page;			 (* heap page *)
    db: T;				 (* database *)
    id: Id := 0;			 (* DB page *)
    lastReader: RTTxn.T;
    writer: RTTxn.T;
  METHODS
    readAccess(p: Releaser) RAISES { Thread.Aborted };
    writeAccess(p: Releaser) RAISES { Thread.Aborted };
    peek(p: Swizzler) RAISES { Thread.Aborted };
    read(p: Swizzler) RAISES { Thread.Aborted };
    write(p: Unswizzler) RAISES { Thread.Aborted };
  END;

  T = MUTEX OBJECT
    <*TRANSIENT*> root: REFANY
  METHODS
    createRoot();
    mapFP(READONLY fp: Fingerprint.T; VAR fpRef: REF Fingerprint.T;
          VAR fpAdr: ADDRESS);
    newPage(): Page;
    newId(p: Page);
    mapPage(p: Id): Page;
    unmapPage(p: Id);
  END;

END RTDB.
