MODULE AttributeCacheStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:29  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:36:53  roland
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
      val := st.graph.getAttribute(key.node, key.num, 0, LAST(CARDINAL));
      found := TRUE;
    EXCEPT
    | Access.Locked =>
        RAISE Error(ErrorSupport.Create(
                      "IdCacheStorage.Read", "Access.Locked"));
    | PersistentGraph.InternalError (info) =>
        RAISE Error(ErrorSupport.Propagate(
                      "IdCacheStorage.Read",
                      "PersistentGraph.InternalError", info));
    | PersistentGraph.NotOwner =>
        RAISE Error(ErrorSupport.Create("AttributeCacheStorage.Read",
                                        "PersistentGraph.NotOwner"));
    | PersistentGraph.NodeNotFound => found := FALSE;
    END;
  END Read;

PROCEDURE Write (st: T; READONLY key: NodeCard.T; val: Text.T)
  RAISES {Error} =
  BEGIN
    TRY
      st.graph.putAttribute(key.node, key.num, 0, val);
    EXCEPT
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          Error(ErrorSupport.Propagate("IdCacheStorage.Write",
                                       "ChgMgmtGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE Error(ErrorSupport.Create("IdCacheStorage.Write",
                                        "PersistentGraph.NodeNotFound"));
    | ChgMgmtGraph.LogError (info) =>
        RAISE Error(ErrorSupport.Propagate("AttributeCacheStorage.Write",
                                           "ChgMgmtGraph.LogError", info));
    | PersistentGraph.NotOwner =>
        RAISE Error(ErrorSupport.Create("AttributeCacheStorage.Write",
                                        "PersistentGraph.NotOwner"));
    | Access.Locked =>
        RAISE Error(ErrorSupport.Create(
                      "AttributeCacheStorage.Write", "Access.Locked"));
    END;
  END Write;

BEGIN
END AttributeCacheStorage.
