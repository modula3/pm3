INTERFACE EventHandlerClass;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/11/12 15:33:39  roland
    Bugfixes in support for specialized EventHandlers.

*)
(***************************************************************************)

IMPORT TriggerStorage, EventHandler, Trigger, ActivatedActions;

TYPE
  ActionStorage = ARRAY Trigger.CouplingMode OF ActivatedActions.T;

  Private = EventHandler.Public OBJECT
              ts: TriggerStorage.T;
              aa: REF ARRAY OF ActionStorage;
            END;

REVEAL
  EventHandler.T <: Private;

END EventHandlerClass.
