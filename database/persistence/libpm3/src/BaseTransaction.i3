INTERFACE BaseTransaction;

IMPORT Transaction, RTHeapDB;

REVEAL
  Transaction.Private = RTHeapDB.Txn BRANDED "Transaction.Private" OBJECT
  METHODS
    init(): T;
  END;

TYPE
  T <: Transaction.Public;

END BaseTransaction.
