INTERFACE EventHandlerClass;

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

    Revision 1.1  1997/11/12 15:33:39  roland
    Bugfixes in support for specialized EventHandlers.

*)
(***************************************************************************)

IMPORT TriggerStorage, EventHandler, Trigger, ActivatedActions;

TYPE
  ActionStorage = ARRAY Trigger.CouplingMode OF ActivatedActions.T;

  Private = EventHandler.Public OBJECT
              ts: TriggerStorage.T;
              aa: <*TRANSIENT*> REF ARRAY OF ActionStorage;
            END;

REVEAL
  EventHandler.T <: Private;

END EventHandlerClass.
