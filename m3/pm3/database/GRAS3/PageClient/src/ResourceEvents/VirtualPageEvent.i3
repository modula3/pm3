INTERFACE VirtualPageEvent;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:14:18  roland
    Subsystem introduces event and pattern types for virtual resources.

*)
(***************************************************************************)

IMPORT Event, Transaction;
FROM EventType IMPORT Mismatch, Unknown;

TYPE Operation = {Begin, Commit, Abort, RemoteCommit};

CONST
  EventTypeName = ARRAY Operation OF
                    TEXT{
                    "VPBegin", "VPCommit", "VPAbort", "VPRemoteCommit"};
  (* VirtualPageEvents are declared in EventTypes.i3 with these names. *)

CONST
  (* Attribute names of the above event types *)
  LevelAttribute        = "VPLevel";
  ResourceNameAttribute = "VPResourceName";
  ResourceAttribute     = "VPResource";
  OperationAttribtue    = "VPOperation";
  IsPreEventAttribute   = "VPIsPreEvent";

TYPE
  T = Event.T;
    (** VirtualPageEvents have the following attributes:
        1) VPBegin, VPCommit, VPAbort
          resourceName,
          resource    : the resource to which the operation was applied
                        and its name.
          isPreEvent  : Events can be signaled before (pre) or after (post)
                        the operation has been performed.
          level       : the transaction-level of the started or ended
                        transaction.

        2) VPRemoteCommit
          resourceName,
          resource    : the resource to which the operation was applied
                        and its name.
    *)

PROCEDURE SignalBegin (transUnit   : CARDINAL;
                       resourceName: TEXT;
                       resource    : REFANY;
                       isPreEvent  : BOOLEAN;
                       level       : Transaction.Level);
PROCEDURE SignalCommit (transUnit   : CARDINAL;
                        resourceName: TEXT;
                        resource    : REFANY;
                        isPreEvent  : BOOLEAN;
                        level       : Transaction.Level);
PROCEDURE SignalAbort (transUnit   : CARDINAL;
                       resourceName: TEXT;
                       resource    : REFANY;
                       isPreEvent  : BOOLEAN;
                       level       : Transaction.Level);
PROCEDURE SignalRemoteCommit (transUnit   : CARDINAL;
                              resourceName: TEXT;
                              resource    : REFANY    );
  (* Send an event to the RuleEngine *)


(* Queries for event attributes.  Note that RemoteCommit events are always
   post events and their level is always Transaction.TopLevel.  GRAS will
   always supply the resource and the resource name, only if events of
   other clients are monitored, the reference to the resource will be lost
   during transfer.  A RemoteCommit event is treated as a local event.  The
   mechanism used for this is different from the RuleEngine. *)
PROCEDURE GetOperation (ev: T): Operation RAISES {Unknown};
PROCEDURE GetResourceName (ev: T): TEXT RAISES {Mismatch, Unknown};
PROCEDURE GetIsPreEvent (ev: T): BOOLEAN RAISES {Mismatch, Unknown};
PROCEDURE GetLevel (ev: T): Transaction.Level RAISES {Mismatch, Unknown};
PROCEDURE GetResource (ev: T): REFANY RAISES {Mismatch, Unknown};

END VirtualPageEvent.
