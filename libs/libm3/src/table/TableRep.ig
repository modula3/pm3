(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)

GENERIC INTERFACE TableRep(Key, Value);

TYPE
  Buckets = REF ARRAY OF EntryList;
  EntryList = REF RECORD
    key: Key.T;
    value: Value.T;
    tail: EntryList;
  END;

END TableRep.
