INTERFACE LongAttributeCache;

(***************************************************************************)
(* This module provides a type for caching the attributes of
   LongAttributeStorage. *)
(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:39:11  roland
    Subsystem PersistentGraph adapted to handle graph boundary crossing
    edges. This has consequences on the architecture of the subsystem as
    well as on the graph model and interface.

    Graphs are organized in pools. Every graph has a number in the
    pool. Pools are the units of transaction management. Two graphs might
    be related by one external relation storage storing the edges between
    nodes of them. Nodes are identified by pairs (graph, entity), where
    graph is the number of the graph in the pool and entity the node
    number within the graph. Graphs and external relation storages are
    administered by the pool in a separate graph.

    Revision 1.2  1997/03/25 17:04:00  roland
    To avoid deadlocks with propagate callbacks, the cache now does'nt
    access LongAttributeStorage directly. This is the responsibility of
    PersistentGraph again.

    Revision 1.1  1997/02/06 13:44:10  roland
    Attributes of LongAttributeStorage are now cached in LongAttributeCache.

*)
(***************************************************************************)


IMPORT VirtualResource;
IMPORT AtomList;

(* The cache does not read or write to the actual storage.  Nevertheless,
   it defines a trigger to be informed about transactions of other clients.
   If another client commits a transaction, the cache will be cleared. *)

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (resource: VirtualResource.T): T;
            (* Initialize the cache.  Attributes will be read from and
               stored in the LongAttributeStorage of storage. *)

      close ();
             (* needed to remove trigger from trigger storage *)

      store (entity, attributeNo: CARDINAL; val: TEXT);
             (* replace the cache entry for entity, attributeNo to val *)

      put (entity     : CARDINAL;
           attributeNo: CARDINAL;
           start      : CARDINAL;
           attribute  : TEXT      ) RAISES {};
           (* change the entry for entity, attributeNo if it is present in
              the cache. *)

      has (entity, attributeNo: CARDINAL): BOOLEAN;
           (* TRUE, iff the cache stores *)

      delete (entity: CARDINAL; attributeNo: CARDINAL) RAISES {};
              (* Deletes the entry of attributeNo of the given entity *)


      get (    entity     : CARDINAL;
               attributeNo: CARDINAL;
               start      : CARDINAL;
               length     : CARDINAL;
           VAR found      : BOOLEAN   ): TEXT RAISES {};
           (* Read length bytes from the attribute at attributeNo of the
              given entity starting at start.  The returned value might be
              shorter than length if less than length bytes were stored. *)


      truncate (entity: CARDINAL; attributeNo: CARDINAL; size: CARDINAL)
                RAISES {};
                (* The given attribute is truncated to size bytes.  The
                   attribute must contain at least size bytes before. *)

      clear () RAISES {};
             (* Clear cache.  Contents will not be written to storage. *)

      removeEntity (ent: CARDINAL);
                    (* Remove all cached attributes for ent *)
    END;


EXCEPTION InternalError(AtomList.T);

END LongAttributeCache.
