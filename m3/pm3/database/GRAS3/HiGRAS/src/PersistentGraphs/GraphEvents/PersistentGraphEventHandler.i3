(*! DATA OBJECT MODULE *)
INTERFACE PersistentGraphEventHandler;

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

    Revision 1.1  1997/11/12 15:24:04  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

(* Create a specialized event handler for managin graph events.  The
   difference to normal event handlers is that this event handler is aware
   of nodes, edges, and attributes, so that events on edges and attributes
   can be handled as depending on the nodes they concern. *)

IMPORT Node, Txn;

PROCEDURE Install ();
  (* Register specialized event handler with the RuleEngine. *)

PROCEDURE NotifyNodeDeletion (unit: CARDINAL;
                              level: Txn.Level;
                              node: Node.T);
  (* Inform event handler about deleted node *)

END PersistentGraphEventHandler.
