INTERFACE LabelCacheStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:38  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.2  1997/02/04 11:17:43  roland
    Obsolete import of Text removed.

    Revision 1.1  1997/01/31 10:37:05  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT Cardinal, Node, ChgMgmtGraph, AtomList;

CONST Brand = "LabelCacheStorage";

 (* Keys are node numbers, values are node labels. *)

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (graph: ChgMgmtGraph.T): T;
            (* All read write operations will concern the given graph *)

      read (READONLY key: Node.T; VAR val: Cardinal.T; VAR found: BOOLEAN)
            RAISES {Error};
            (* Tries to find a node with index key.text at attribute
               key.num *)

      write (READONLY key: Node.T; val: Cardinal.T) RAISES {Error};
             (* Writes index attribute key.num to node val wit value
                key.text. *)
    END;

EXCEPTION
  Error(AtomList.T);



END LabelCacheStorage.
