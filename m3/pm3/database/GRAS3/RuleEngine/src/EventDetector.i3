INTERFACE EventDetector;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:50  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:40  hosking
    Import of GRAS3 1.1

    Revision 1.1  1998/08/12 11:04:39  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

*)
(***************************************************************************)

(* An EventDetector.T is a unit which is notified when triggers of certain
   types are registered with the RuleEngine. *)

IMPORT CardSeq;

CONST Brand = "EventDetector";

TYPE
  T <: Public;

  Public =
    <*TRANSIENT*> ROOT
    OBJECT
    METHODS
      init (types: CardSeq.T): T;
            (* Initialize this detector for the event types in types. *)

      notifyRegistration (type: CARDINAL);
      notifyUnregistration (type: CARDINAL);
                            (* These methods are called from the RuleEngine
                               whenever triggers for events of type type
                               are registered and unregistered. *)

      triggersActive (type: CARDINAL): BOOLEAN;
                      (* For every event type with which the detector was
                         initialized, it controls a counter for registered
                         triggers.  This method returns TRUE iff the
                         counter for type is not 0. *)
    END;


END EventDetector.
