(*!  DATA TYPE MODULE *)
INTERFACE ActivatedActions;

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

    Revision 1.1  1997/10/31 14:03:55  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The event handler
    subsystem implements the mapping between events and action via triggers.

*)
(***************************************************************************)

(* ActivatedActions stores the actions computed by trigger storage.
   Retrieval with the get-method removes the activated actions from the
   storage. *)

IMPORT Action, Event, ContextSet;

TYPE
  T = <*TRANSIENT*> ROOT OBJECT
      METHODS
        store (event   : Event.T;
               context : ContextSet.T;
               level   : CARDINAL;
               priority: CARDINAL;
               act     : Action.T;
               userdata: <*TRANSIENT*> REFANY        );

        killClient      (c: CARDINAL);
        killTransaction (level: CARDINAL);

        get     (VAR event   : Event.T;
                 VAR context : ContextSet.T;
                 VAR level   : CARDINAL;
                 VAR action  : Action.T;
                 VAR userdata: <*TRANSIENT*> REFANY        ): BOOLEAN;

        highest (): CARDINAL;

        notEmpty (): BOOLEAN;
      END;

  Default <: T OBJECT METHODS init (): Default; END;

END ActivatedActions.
