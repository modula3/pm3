INTERFACE Database;

IMPORT Pathname;
IMPORT Transaction;
IMPORT Thread;

EXCEPTION
  Exists;
  NotFound;
  Closed;
  Opened;

PROCEDURE Create(name: Pathname.T)
  RAISES { Exists, Transaction.InProgress, Thread.Aborted };

PROCEDURE Open(name: Pathname.T): T
  RAISES { NotFound, Opened, Transaction.InProgress };
  (* NotFound is raised if named database does not exist;
     Opened is raised if database is already open;
     Transaction.InProgress is raised if a transaction is already open *)

PROCEDURE Checkpoint();

TYPE
  T <: Public;
  Private <: <*TRANSIENT*> ROOT;
  Public = Private OBJECT METHODS
    getRoot(): REFANY RAISES { Transaction.NotInProgress };
    (* Transaction.NotInProgress is raised if no transaction is open *)

    setRoot(object: REFANY) RAISES { Transaction.NotInProgress };
    (* Transaction.NotInProgress is raised if no transaction is open *)
  END;
END Database.
