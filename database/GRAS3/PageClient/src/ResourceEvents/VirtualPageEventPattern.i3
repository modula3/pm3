INTERFACE VirtualPageEventPattern;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/10/31 14:14:19  roland
    Subsystem introduces event and pattern types for virtual resources.

*)
(***************************************************************************)

IMPORT EventPattern, VirtualPageEvent, Txn;
FROM EventType IMPORT Unknown, Mismatch;

TYPE
  T = EventPattern.T;
    (* VirtualPageEventPatterns have the same attributes as
       VirtualPageEvents. *)

PROCEDURE Create (op: VirtualPageEvent.Operation): T;
  (* Create an event pattern.  All its attributes are set to wildcards *)

(* Updates to pattern attributes *)
PROCEDURE SetResourceName (p: T; name: TEXT) RAISES {Unknown, Mismatch};
PROCEDURE SetPreEvent (p: T; ispre: BOOLEAN) RAISES {Unknown, Mismatch};
PROCEDURE SetLevel (p: T; level: Txn.Level) RAISES {Unknown, Mismatch};
PROCEDURE SetResource (p: T; resource: REFANY) RAISES {Unknown, Mismatch};


(* Queries for pattern attributes. *)
PROCEDURE GetResourceName (p: T): TEXT RAISES {Unknown, Mismatch};
PROCEDURE GetOperation (p: T): VirtualPageEvent.Operation
  RAISES {Unknown};
PROCEDURE GetIsPreEvent (p: T): BOOLEAN RAISES {Unknown, Mismatch};
PROCEDURE GetLevel (p: T): Txn.Level RAISES {Unknown, Mismatch};
PROCEDURE GetResource (p: T): REFANY RAISES {Unknown, Mismatch};

END VirtualPageEventPattern.
