INTERFACE TypeCache;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:46  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:35  hosking
    Import of GRAS3 1.1

    Revision 1.2  1997/10/31 14:23:28  roland
    Adapted to new RuleEngine.

    Revision 1.1  1997/07/07 15:56:49  roland
    New caches for node types of a typed graph.

*)
(***************************************************************************)

(* A TypeCache holds the value of the Type-IndexAttribute of nodes in a
   TypedGraph.  It is coupled with the RuleEngine to synchronize
   with transactions of other clients. *)

IMPORT Node, TypedGraphPool;

TYPE
  T <: Public;

  Public = <*TRANSIENT*> ROOT OBJECT
           METHODS
             init (pool: TypedGraphPool.T): T;

             get (node: Node.T; VAR val: TEXT): BOOLEAN;
                  (* Return the value stored for node.  If it is not in the
                     cache the return-value will be FALSE. *)
             put (node: Node.T; val: TEXT);
                  (* Stores val as value for key *)
             delete (node: Node.T);
                     (* If cache has an entry for key, it is removed. *)
             flush ();
                    (* Clears the cache. *)
           END;

END TypeCache.
