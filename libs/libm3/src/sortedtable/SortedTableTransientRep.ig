GENERIC INTERFACE SortedTableTransientRep(Key, Value);

TYPE
  Node = <*TRANSIENT*> REF RECORD
    key     : Key.T;
    value   : Value.T;
    lo      : Node := NIL;
    hi      : Node := NIL;
    priority: INTEGER    (* random num; tree is a heap on these *)
  END;

END SortedTableTransientRep.
