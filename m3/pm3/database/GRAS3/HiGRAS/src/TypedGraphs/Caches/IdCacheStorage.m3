MODULE IdCacheStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:37  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:37:04  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT ChgMgmtGraph, PersistentGraph, Text, NodeCard, Access, ErrorSupport;

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
                READONLY key  : NodeCard.T;
                VAR      val  : Text.T;
                VAR      found: BOOLEAN     ) RAISES {Error} =
  BEGIN
    TRY
      found := FALSE;
      val := st.graph.getIndex(key.node, key.num, found);
    EXCEPT
    | Access.Locked =>
        RAISE Error(ErrorSupport.Create(
                      "IdCacheStorage.Read", "Access.Locked"));
    | PersistentGraph.NotOwner =>
        RAISE Error(ErrorSupport.Create(
                      "IdCacheStorage.Read", "PeristentGraph.NotOwner"));
    | PersistentGraph.InternalError (info) =>
        RAISE Error(ErrorSupport.Propagate(
                      "IdCacheStorage.Read",
                      "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound => found := FALSE;
    END;
  END Read;

PROCEDURE Write (st: T; READONLY key: NodeCard.T; val: Text.T)
  RAISES {Error} =
  BEGIN
    TRY
      st.graph.putIndex(key.node, key.num, val);
    EXCEPT
      PersistentGraph.IndexUsed =>
        RAISE
          Error(ErrorSupport.Create(
                  "IdCacheStorage.Write", "PersistentGraph.IndexUsed"));
    | PersistentGraph.NotOwner =>
        RAISE Error(ErrorSupport.Create(
                      "IdCacheStorage.Write", "PersistentGraph.NotOwner"));
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          Error(ErrorSupport.Propagate("IdCacheStorage.Write",
                                       "ChgMgmtGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE Error(ErrorSupport.Create("IdCacheStorage.Write",
                                        "PersistentGraph.NodeNotFound"));
    | ChgMgmtGraph.LogError (info) =>
        RAISE Error(ErrorSupport.Propagate("IdCacheStorage.Write",
                                           "ChgMgmtGraph.LogError", info));
    | Access.Locked =>
        RAISE Error(ErrorSupport.Create(
                      "IdCacheStorage.Write", "Access.Locked"));
    END;
  END Write;


BEGIN
END IdCacheStorage.
