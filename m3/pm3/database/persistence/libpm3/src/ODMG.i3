INTERFACE ODMG;

IMPORT Database, Transaction;

EXCEPTION
  ObjectNameNotFound;
  ObjectNameNotUnique;

CONST Create = Database.Create;

PROCEDURE Open(database_name: TEXT): T
  RAISES { Database.NotFound, Database.Opened, Transaction.InProgress };
  (* Database.NotFound is raised if named database does not exist;
     Database.Opened is raised if database is already open;
     Transaction.InProgress is raised if a transaction is already open *)

TYPE
  T <: Public;
  Public = <*TRANSIENT*> ROOT OBJECT METHODS
    close () RAISES { Database.Closed, Transaction.InProgress };
    (* Database.Closed is raised if database is already closed;
       Transaction.InProgress is raised if a transaction is still open *)

    bind(object: REFANY; name: TEXT)
    RAISES { ObjectNameNotUnique, Transaction.NotInProgress };
    (* ObjectNameNotUnique is raised if name is already bound;
       Transaction.NotInProgress is raised if no transaction is open *)

    unbind(name: TEXT): REFANY
    RAISES { ObjectNameNotFound, Transaction.NotInProgress };
    (* ObjectNameNotFound is raised if name is not bound;
       Transaction.NotInProgress is raised if no transaction is open *)

    lookup(lookup: TEXT): REFANY
    RAISES { ObjectNameNotFound, Transaction.NotInProgress };
    (* ObjectNameNotFound is raised if name is not bound;
       Transaction.NotInProgress is raised if no transaction is open *)
  END;
END ODMG.
