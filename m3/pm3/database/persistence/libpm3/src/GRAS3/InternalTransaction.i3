INTERFACE InternalTransaction;

IMPORT Transaction, BaseTransaction;

REVEAL
  Transaction.T <: Internal;

TYPE
  Internal = BaseTransaction.T OBJECT END;

END InternalTransaction.
