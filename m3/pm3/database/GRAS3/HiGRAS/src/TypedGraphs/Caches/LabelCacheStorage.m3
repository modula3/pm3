MODULE LabelCacheStorage;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:24:39  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:37:07  roland
    Simple caches for attributes, labels, names and identifeiers of a
    scheme. Used only for read-only or exclusiv access to a scheme.

*)
(***************************************************************************)

IMPORT ChgMgmtGraph, PersistentGraph, Cardinal, Access, ErrorSupport, Node;

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
                READONLY key  : Node.T;
                VAR      val  : Cardinal.T;
                VAR      found: BOOLEAN     ) RAISES {Error} =
  BEGIN
    TRY
      found := FALSE;
      val := st.graph.getNodeLabel(key);
      found := TRUE;
    EXCEPT
    | Access.Locked =>
        RAISE Error(ErrorSupport.Create(
                      "IdCacheStorage.Read", "Access.Locked"));
    | PersistentGraph.NotOwner =>
        RAISE
          Error(ErrorSupport.Create(
                  "LabelCacheStorage.Read", "PersistentGraph.NotOwner"));
    | PersistentGraph.InternalError (info) =>
        RAISE Error(ErrorSupport.Propagate(
                      "IdCacheStorage.Read",
                      "PersistentGraph.InternalError", info));
    | PersistentGraph.NodeNotFound => found := FALSE;
    END;
  END Read;

PROCEDURE Write (st: T; READONLY key: Node.T; val: Cardinal.T)
  RAISES {Error} =
  BEGIN
    TRY
      st.graph.putNodeLabel(key, val);
    EXCEPT
    | ChgMgmtGraph.InternalError (info) =>
        RAISE
          Error(ErrorSupport.Propagate("IdCacheStorage.Write",
                                       "ChgMgmtGraph.InternalError", info));
    | PersistentGraph.NodeNotFound =>
        RAISE Error(ErrorSupport.Create("IdCacheStorage.Write",
                                        "PersistentGraph.NodeNotFound"));
    | PersistentGraph.NotOwner =>
        RAISE Error(ErrorSupport.Create("LabelCacheStorage.Write",
                                        "PersistentGraph.NotOwner"));
    | ChgMgmtGraph.LogError (info) =>
        RAISE Error(ErrorSupport.Propagate("LabelCacheStorage.Write",
                                           "ChgMgmtGraph.LogError", info));
    | Access.Locked =>
        RAISE Error(ErrorSupport.Create(
                      "LabelCacheStorage.Write", "Access.Locked"));
    END;
  END Write;

BEGIN
END LabelCacheStorage.
