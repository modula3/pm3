INTERFACE TargetCacheStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/07/07 15:43:44  roland
    Added caches for relations in a Scheme.

*)
(***************************************************************************)

IMPORT NodeSet, NodeCard, ChgMgmtGraph, AtomList;

CONST Brand = "TargetCacheStorage";

(* Keys are (node number, label) pairs, values are sets of target nodes. *)

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (graph: ChgMgmtGraph.T): T;
            (* All read write operations will concern the given graph *)

      read (READONLY key  : NodeCard.T;
            VAR      val  : NodeSet.T;
            VAR      found: BOOLEAN     ) RAISES {Error};
            (* Tries to find all incoming edges labelled key.num of node
               key.node *)

      write (READONLY key: NodeCard.T; val: NodeSet.T) RAISES {Error};
             (* For each n \in val, draws an edge from key.node to n
                labelled key.num.  Note: write will never delete edges
                which are not in val. *)
    END;

EXCEPTION Error(AtomList.T);

END TargetCacheStorage.
