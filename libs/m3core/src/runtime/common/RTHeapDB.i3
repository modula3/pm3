(* Copyright (C) 1998, Purdue Research Foundation            *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

INTERFACE RTHeapDB;

IMPORT RTDB, RTTxn, Thread;

EXCEPTION Disabled;

PROCEDURE RefPageMap(object: REFANY): RTDB.Page;

PROCEDURE Abort();
PROCEDURE Flush(db: RTDB.T := NIL; release := TRUE) RAISES { Thread.Aborted };
PROCEDURE SwizzleRoot(db: RTDB.T): REFANY;

PROCEDURE Transfer(from: RTTxn.T; to: RTTxn.T);

END RTHeapDB.
