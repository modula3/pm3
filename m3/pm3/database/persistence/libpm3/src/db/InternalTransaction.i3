INTERFACE InternalTransaction;

IMPORT Transaction, BaseTransaction, BerkeleyDB;

REVEAL
  Transaction.T <: Internal;

TYPE
  Internal = BaseTransaction.T OBJECT
    dbtxn: BerkeleyDB.DB_TXN;
  END;

END InternalTransaction.
