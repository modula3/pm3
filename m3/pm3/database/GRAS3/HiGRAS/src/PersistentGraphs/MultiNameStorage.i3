INTERFACE MultiNameStorage;

(***************************************************************************)
(* This module offers tagged names for entities like NameStorage, but here
   more every name may be used more than once. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:39:17  roland
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

    Revision 1.4  1996/11/20 12:23:00  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/17 13:09:05  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.2  1996/08/06 16:26:31  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/07/24 09:20:05  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.1.2.1  1996/04/29 13:43:43  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.1  1994/01/20  18:41:29  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Access, ITPFile, CardSet, TaggedNameSet;
IMPORT AtomList;

PROCEDURE PutName (file  : ITPFile.T;
                   entity: CARDINAL;
                   tag   : CARDINAL;
                   name  : TEXT       )
  RAISES {Access.Locked, TagUsed, InternalError};
  (* Store a name for an entity under a tag. *)


PROCEDURE DeleteName (file  : ITPFile.T;
                      entity: CARDINAL;
                      tag   : CARDINAL;
                      name  : TEXT       )
  RAISES {Access.Locked, InternalError, NameNotFound};
  (* Delete the given name. *)


PROCEDURE DeleteAllNamesForEntity (    file   : ITPFile.T;
                                       entity : CARDINAL;
                                   VAR deleted: TaggedNameSet.T)
  RAISES {Access.Locked, InternalError};
  (* Deletes all names for the given entity.  deleted returns a set of all
     (tag, name) pairs which where deleted. *)


PROCEDURE GetName (    file  : ITPFile.T;
                       entity: CARDINAL;
                       tag   : CARDINAL;
                   VAR found : BOOLEAN    ): TEXT RAISES {Access.Locked, InternalError};
  (* Returns the name for the entity stored under tag. *)


PROCEDURE FindEntitiesForName (file: ITPFile.T; tag: CARDINAL; name: TEXT):
  CardSet.T RAISES {Access.Locked, InternalError};
  (* Returns a set of all entities with the given name. *)


PROCEDURE FindTagsForEntity (file: ITPFile.T; entity: CARDINAL): CardSet.T
  RAISES {Access.Locked, InternalError};
  (* Returns a set of all tags which contain a name for entity. *)

EXCEPTION
  InternalError(AtomList.T);
  TagUsed;
  NameNotFound;

END MultiNameStorage.
