(*!  DATA TYPE MODULE *)
INTERFACE GraphActivatedActions;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:46  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:32  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/11/12 15:23:51  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

(* GraphActivatedActions stores the actions computed by a graph trigger
   storage.  Retrieval with the get-method removes the activated actions
   from the storage.  Respects dependencies of actions on deleted nodes. *)

IMPORT Action, Event, ContextSet;
IMPORT Node, Txn;
IMPORT ActivatedActions AS Super;

TYPE
  T <: Public;

  Public = Super.T OBJECT
           METHODS
             init (): T;

             store (event    : Event.T;
                    depsFirst: BOOLEAN;
                    depsTarg : BOOLEAN;
                    context  : ContextSet.T;
                    level    : CARDINAL;
                    priority : CARDINAL;
                    act      : Action.T;
                    userdata : REFANY        );

             notifyNodeDeletion (level: Txn.Level; node: Node.T);

           END;

END GraphActivatedActions.
