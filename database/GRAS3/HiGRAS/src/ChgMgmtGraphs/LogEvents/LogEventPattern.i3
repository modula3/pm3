INTERFACE LogEventPattern;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:45  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:29  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/12/02 17:56:39  roland
    New event types and event contexts for user recovery operations
    introduced.

*)
(***************************************************************************)

IMPORT EventPattern, LogEvents, Txn;
FROM EventType IMPORT Unknown, Mismatch;

TYPE
  T = EventPattern.T;
    (* LogEventPatterns have the same attributes as
       LogEvents. *)

PROCEDURE Create (op: LogEvents.Operation): T;
  (* Create an event pattern.  All its attributes are set to wildcards *)

(* Updates to pattern attributes *)
PROCEDURE SetPoolName (p: T; name: TEXT) RAISES {Unknown, Mismatch};
PROCEDURE SetPool (p: T; pool: REFANY) RAISES {Unknown, Mismatch};
PROCEDURE SetGraphNumber (p: T; number: CARDINAL) RAISES {Unknown, Mismatch};
PROCEDURE SetGraph (p: T; graph: REFANY) RAISES {Unknown, Mismatch};
PROCEDURE SetPreEvent (p: T; ispre: BOOLEAN) RAISES {Unknown, Mismatch};
PROCEDURE SetLevel (p: T; level: Txn.Level)
  RAISES {Unknown, Mismatch};

(* redoIth patterns *)
PROCEDURE SetSonNo (p: T; son: CARDINAL) RAISES {Mismatch, Unknown};

(* Attribute queries *)

PROCEDURE GetOperation (p: T): LogEvents.Operation RAISES {Unknown};
PROCEDURE GetPoolName (p: T): TEXT RAISES {Mismatch, Unknown};
PROCEDURE GetPool (p: T): REFANY RAISES {Mismatch, Unknown};
PROCEDURE GetGraphNo (p: T): CARDINAL RAISES {Mismatch, Unknown};
PROCEDURE GetGraph (p: T): REFANY RAISES {Mismatch, Unknown};
PROCEDURE GetIsPreEvent (p: T): BOOLEAN RAISES {Mismatch, Unknown};
PROCEDURE GetLevel (p: T): Txn.Level RAISES {Mismatch, Unknown};

(* redoIth patterns *)
PROCEDURE GetSonNo (p: T): CARDINAL RAISES {Mismatch, Unknown};
  
END LogEventPattern.
