(* Created on Tue Nov 8 1998 by Jiawan Chen       *)
(* A "BPlusTree.T" is a subtype of a "SortedTable.T", but it is implemented
   using B+ trees. *)

GENERIC INTERFACE BPlusTree(Key, Value, SortedTbl);
(* Where the same requirments exist on the "Key" and "Value" interfaces
   as those described in the generic "SortedTable" interface and where
   "SortedTbl" is the generic instance "SortedTable(Key, Value)". *)

CONST Brand = "(BPlusTree " & Key.Brand & " " & Value.Brand & ")";
(* The type "T" is revealed to have brand "Brand". *)

TYPE
  T <: Public;
  Public = SortedTbl.T OBJECT
  METHODS
    init(n := 128): T;
    (* The expression "NEW(T).init(n)" evaluates to a new table with no
       elements whose minimal item number is n. The "init" method may also 
       be invoked on an existing table to delete all of its entries. *)
    keyCompare(READONLY k1, k2: Key.T): [-1..1];
  END;

  Iterator <: IteratorPublic;
  IteratorPublic = SortedTbl.Iterator OBJECT
  METHODS
    reset();
  END;
END BPlusTree.
