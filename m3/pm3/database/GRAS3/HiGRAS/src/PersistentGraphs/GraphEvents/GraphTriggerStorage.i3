(*!  DATA TYPE MODULE *)
INTERFACE GraphTriggerStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.1  1997/11/12 15:24:01  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

(* A specialization of TriggerStorage.T to store graph events *)

IMPORT Trigger, Action;
IMPORT TriggerStorage AS Super;

TYPE

  T <: Public;
  
  Public =
    Super.T OBJECT
    METHODS
      init (): T;
      getNextAction (VAR act      : Action.T;
                     VAR depsFirst: BOOLEAN;
                     VAR depsTarg : BOOLEAN;
                     VAR coupl    : Trigger.CouplingMode;
                     VAR priority : CARDINAL;
                     VAR userdata : REFANY                ): BOOLEAN;
                     (* notifyEvent computes all activated actions for
                        event e.  These are held internal and can be
                        queried with getNextAction.  depsFirst und depsTarg
                        specify whether the activated action depends on the
                        existance of the first node of the event (source
                        node for edge events, node for attribute events)
                        and target node (edge events only) *)

    END;


END GraphTriggerStorage. 
