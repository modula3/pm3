MODULE NameCacheStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.2  1997/07/21 10:49:51  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1  1997/05/01 13:24:41  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:37:09  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT ChgMgmtGraph, PersistentGraph, CardText,
       Access, ErrorSupport, NodeSet, Node;

REVEAL
  T = Public BRANDED Brand OBJECT
        graph: ChgMgmtGraph.T;
      OVERRIDES
        init  := Init;
        read  := Read;
        write := Write;
      END;

PROCEDURE Init (st: T; graph: ChgMgmtGraph.T): T =
  BEGIN
    st.graph := graph;
    RETURN st;
  END Init;


PROCEDURE Read (         st   : T;
                READONLY key  : CardText.T;
                VAR      val  : Node.T;
                VAR      found: BOOLEAN     ) RAISES {Error} =
  VAR
    set: NodeSet.T;
    ok : BOOLEAN;
  BEGIN
    TRY
      set := st.graph.getNodesWithIndex(key.num, key.text);
      val := set.extractAnyElement(ok);
      IF NOT ok OR set.card() > 0 THEN
        found := FALSE
      ELSE
        found := TRUE;
      END;
    EXCEPT
    | Access.Locked =>
        RAISE Error(ErrorSupport.Create(
                      "NameCacheStorage.Read", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE Error(ErrorSupport.Propagate(
                      "NameCacheStorage.Read",
                      "PersistentGraph.InternalError", info));
    END;
  END Read;

PROCEDURE Write (st: T; READONLY key: CardText.T; READONLY val: Node.T)
  RAISES {Error} =
  BEGIN
    TRY
      st.graph.putIndex(val, key.num, key.text);
    EXCEPT
      PersistentGraph.IndexUsed =>
        RAISE Error(ErrorSupport.Create("NameCacheStorage.Write",
                                        "PersistentGraph.IndexUsed"));
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          Error(ErrorSupport.Propagate("NameCacheStorage.Write",
                                       "ChgMgmtGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE Error(ErrorSupport.Create("NameCacheStorage.Write",
                                        "PersistentGraph.NodeNotFound"));
    | ChgMgmtGraph.LogError (info) =>
        RAISE Error(ErrorSupport.Propagate("NameCacheStorage.Write",
                                           "ChgMgmtGraph.LogError", info));
    | PersistentGraph.NotOwner =>
        RAISE
          Error(ErrorSupport.Create(
                  "NameCacheStorage.Write", "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE Error(ErrorSupport.Create(
                      "NameCacheStorage.Write", "Access.Locked"));
    END;
  END Write;

BEGIN
END NameCacheStorage.
