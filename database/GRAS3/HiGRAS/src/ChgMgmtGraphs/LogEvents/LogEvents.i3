INTERFACE LogEvents;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/12/02 17:56:41  roland
    New event types and event contexts for user recovery operations
    introduced.

*)
(***************************************************************************)

IMPORT Event, Transaction;
FROM EventType IMPORT Mismatch, Unknown;


TYPE
  T = Event.T;
    (** All LogEvents have the following attributes:
          poolName,
          pool      : the pool in which the event occurred its name.
          isPreEvent: Events can be signaled before (pre) or after (post)
                        the operation has been performed.
          level     : the transaction-level of the transaction
                        that signaled the event.
          graphNo,
          graph     : the graph in which the event occurred and its number.

        Additionally to this basic set of event attributes, different event
        types have different attributes.

        Checkpoint events have no additional attributes

        Undo/Redo events have no additional attributes

        RedoIth events
          SonNo     : the number of the son checkpoint to go to

        Backstep/Forstep events have no additional attributes

    *)


  Operation = {Checkpoint, Undo, Redo, RedoNext, RedoPrev, RedoIth,
               Backstep, Forstep};

CONST
  EventTypeName = ARRAY Operation OF
                    TEXT{"Checkpoint", "Undo", "Redo", "RedoNext",
                         "RedoPrev", "RedoIth", "Backstep", "Forstep"};
  (* LogEvents are declared in EventTypes.i3 with these names. *)

CONST
  (* Attribute names of the above event types *)
  LevelAttribute       = "Level";
  PoolNameAttribute    = "PoolName";
  PoolAttribute        = "Pool";
  IsPreEventAttribute  = "IsPreEvent";
  GraphNumberAttribute = "GraphNumber";
  GraphAttribute       = "Graph";

  SonNoAttribute = "SonNo";

PROCEDURE SignalCheckpoint (transUnit : CARDINAL;
                            poolName  : TEXT;
                            pool      : REFANY;
                            graphNo   : CARDINAL;
                            graph     : REFANY;
                            isPreEvent: BOOLEAN;
                            level     : Transaction.Level);

PROCEDURE SignalUndo (transUnit : CARDINAL;
                      poolName  : TEXT;
                      pool      : REFANY;
                      graphNo   : CARDINAL;
                      graph     : REFANY;
                      isPreEvent: BOOLEAN;
                      level     : Transaction.Level);


PROCEDURE SignalRedo (transUnit : CARDINAL;
                      poolName  : TEXT;
                      pool      : REFANY;
                      graphNo   : CARDINAL;
                      graph     : REFANY;
                      isPreEvent: BOOLEAN;
                      level     : Transaction.Level);


PROCEDURE SignalRedoNext (transUnit : CARDINAL;
                          poolName  : TEXT;
                          pool      : REFANY;
                          graphNo   : CARDINAL;
                          graph     : REFANY;
                          isPreEvent: BOOLEAN;
                          level     : Transaction.Level);


PROCEDURE SignalRedoPrev (transUnit : CARDINAL;
                          poolName  : TEXT;
                          pool      : REFANY;
                          graphNo   : CARDINAL;
                          graph     : REFANY;
                          isPreEvent: BOOLEAN;
                          level     : Transaction.Level);


PROCEDURE SignalRedoIth (transUnit : CARDINAL;
                         poolName  : TEXT;
                         pool      : REFANY;
                         graphNo   : CARDINAL;
                         graph     : REFANY;
                         isPreEvent: BOOLEAN;
                         level     : Transaction.Level;
                         son       : CARDINAL           );


PROCEDURE SignalBackstep (transUnit : CARDINAL;
                          poolName  : TEXT;
                          pool      : REFANY;
                          graphNo   : CARDINAL;
                          graph     : REFANY;
                          isPreEvent: BOOLEAN;
                          level     : Transaction.Level);


PROCEDURE SignalForstep (transUnit : CARDINAL;
                         poolName  : TEXT;
                         pool      : REFANY;
                         graphNo   : CARDINAL;
                         graph     : REFANY;
                         isPreEvent: BOOLEAN;
                         level     : Transaction.Level);


(* Queries for event attributes.*)
PROCEDURE GetOperation (ev: T): Operation RAISES {Unknown};
PROCEDURE GetPoolName (ev: T): TEXT RAISES {Mismatch, Unknown};
PROCEDURE GetPool (ev: T): REFANY RAISES {Mismatch, Unknown};
PROCEDURE GetGraphNo (ev: T): CARDINAL RAISES {Mismatch, Unknown};
PROCEDURE GetGraph (ev: T): REFANY RAISES {Mismatch, Unknown};
PROCEDURE GetIsPreEvent (ev: T): BOOLEAN RAISES {Mismatch, Unknown};
PROCEDURE GetLevel (ev: T): Transaction.Level RAISES {Mismatch, Unknown};

(* RedoIth *)
PROCEDURE GetSonNo (ev: T): CARDINAL RAISES {Mismatch, Unknown};

END LogEvents.
