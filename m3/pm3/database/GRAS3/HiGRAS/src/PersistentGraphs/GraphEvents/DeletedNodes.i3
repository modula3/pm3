INTERFACE DeletedNodes;

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

    Revision 1.1  1997/11/12 15:23:48  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

IMPORT Node, Txn;

(* DeletedNodes store information about deleted Nodes for a
   GraphActivatedActions.T.  A deleted node is characterized by a node, a
   transaction level and a timestamp, all provided by
   GraphActivatedActions. *)

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (): T;

      insert (node: Node.T; level: Txn.Level; timestamp: CARDINAL);
              (* insert (node, level, timestamp) in set *)

      clear ();
             (* clear set *)

      killTransaction (level: Txn.Level);
                       (* remove all deleted nodes with a higher or equal
                          transaction level *)

      invalid (node: Node.T; timestamp: CARDINAL): BOOLEAN;
             (* TRUE iff set contains a corresponding deleted node with a
                later time stamp. *)
    END;

END DeletedNodes.
