GENERIC INTERFACE SortedTableRep(Key, Value);

TYPE
  Node = REF RECORD
    key     : Key.T;
    value   : Value.T;
    lo      : Node := NIL;
    hi      : Node := NIL;
    priority: INTEGER    (* random num; tree is a heap on these *)
  END;

END SortedTableRep.
