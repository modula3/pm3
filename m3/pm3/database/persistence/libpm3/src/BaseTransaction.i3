INTERFACE BaseTransaction;

IMPORT Transaction, RTTxn;

REVEAL
  Transaction.Private = RTTxn.T BRANDED "Transaction.Private" OBJECT
  METHODS
    init(): T;
  END;

TYPE
  T <: Transaction.Public;

END BaseTransaction.
