INTERFACE Transaction;

IMPORT Thread;

EXCEPTION
  InProgress;
  NotInProgress;

TYPE
  LockMode = { READ, WRITE };
  (* Read locks allow shared access to an object;
     Write locks indicate exclusive access to an object;
     Upgrade locks allow shared access to an object but indicate intention
     to upgrade to a write lock. *)

  T <: Public;
  Private <: <*TRANSIENT*> ROOT;
  Public = Private OBJECT METHODS

    begin() RAISES { InProgress, Thread.Aborted };
    (* Starts (opens) transaction. *)

    commit() RAISES { NotInProgress, Thread.Aborted };
    (* Commits and closes transaction *)

    chain() RAISES { NotInProgress, Thread.Aborted };
    (* Commits and reopens transaction; retains locks if possible *)

    abort() RAISES { NotInProgress };
    (* Aborts and closes transaction *)

    checkpoint() RAISES { NotInProgress, Thread.Aborted };
    (* Checkpoints updates, retains locks and leaves transaction open *)

    isOpen(): BOOLEAN;
    (* Returns true if this transaction is open, otherwise false *)

    lock(object: REFANY; mode: LockMode)
    RAISES { NotInProgress, Thread.Aborted };
    (* Explicitly obtain a lock on an object *)
  END;
END Transaction.
