(*!  DATA TYPE MODULE *)
INTERFACE GraphEventHandler;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.1  1997/11/12 15:23:56  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

(* A GraphEventHandler.T is a specialization of EventHandler.  It is aware
   of the types of events on Graphs and is able to handle Edge and
   attribute events as dependents of node events. *)

IMPORT EventHandler AS Super;
IMPORT Node, TriggerStorage, Transaction;

TYPE
  T <: Public;

  Public = Super.T OBJECT
           METHODS
             init (ts: TriggerStorage.T): T;
             notifyNodeDeletion (unit : CARDINAL;
                                 level: Transaction.Level;
                                 node : Node.T             );
           END;

CONST Brand = "GraphEventHandler";

END GraphEventHandler.
