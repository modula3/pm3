(* Copyright 1992 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)

GENERIC INTERFACE TableTransientRep(Key, Value);

TYPE
  Buckets = <*TRANSIENT*> REF ARRAY OF EntryList;
  EntryList = <*TRANSIENT*> REF RECORD
    key: Key.T;
    value: Value.T;
    tail: EntryList;
  END;

END TableTransientRep. 
