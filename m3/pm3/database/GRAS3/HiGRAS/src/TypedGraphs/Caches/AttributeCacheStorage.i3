INTERFACE AttributeCacheStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:28  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:36:52  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT Text, NodeCard, ChgMgmtGraph, AtomList;

CONST Brand = "AttributeCacheStorage";

 (* Keys are (node number, attribute number) pairs, values are attributes. *)

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (graph: ChgMgmtGraph.T): T;
            (* All read write operations will concern the given graph *)

      read (READONLY key: NodeCard.T; VAR val: Text.T; VAR found: BOOLEAN)
            RAISES {Error};
            (* Tries to find a node with index key.text at attribute
               key.num *)

      write (READONLY key: NodeCard.T; val: Text.T) RAISES {Error};
             (* Writes index attribute key.num to node val wit value
                key.text. *)
    END;

EXCEPTION
  Error(AtomList.T);

END AttributeCacheStorage.
