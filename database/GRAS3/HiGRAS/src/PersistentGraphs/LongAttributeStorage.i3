INTERFACE LongAttributeStorage;

(***************************************************************************)
(* This module stores "arbitrary" sized attributes for an entity.  Note
   that the entity must exist for this to work.  There is a considerably
   faster access to the first 255 bytes of attribute 0 of each entity
   compared to other attribute positions. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:39:14  roland
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

    Revision 1.5  1996/11/20 12:22:56  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.4  1996/09/17 13:09:02  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.3  1996/08/06 16:26:29  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/07/24 09:20:01  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.2.2.1  1996/04/29 13:43:40  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.2  1995/07/05  16:03:11  alex
# Add the method deleteAttribute to PersistentGraph
#
# Revision 1.1  1994/01/20  18:41:25  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Access, ITPFile, CardSet, AttributeDescriptionSet;
IMPORT AtomList;

PROCEDURE PutAttribute (file       : ITPFile.T;
                        entity     : CARDINAL;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        attribute  : TEXT       )
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Store the attribute at attributeNo of the given entity starting at
     start. *)

PROCEDURE DeleteAttribute (file       : ITPFile.T;
                           entity     : CARDINAL;
                           attributeNo: CARDINAL   )
  RAISES {Access.Locked, InternalError};
  (* Deletes the attribute of attributeNo of the given entity *)


PROCEDURE GetAttribute (file       : ITPFile.T;
                        entity     : CARDINAL;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        length     : CARDINAL   ): TEXT
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Read length bytes from the attribute at attributeNo of the given
     entity starting at start.  The returned value might be shorter than
     length if less than length bytes were stored. *)


PROCEDURE TruncateAttribute (file       : ITPFile.T;
                             entity     : CARDINAL;
                             attributeNo: CARDINAL;
                             size       : CARDINAL   )
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* The given attribute is truncated to size bytes.  The attribute must
     contain at least size bytes before. *)


PROCEDURE DeleteAllAttributes (file: ITPFile.T; entity: CARDINAL)
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Deletes all attributes of the given entity. *)


PROCEDURE GetAllAttributeNumbers (file: ITPFile.T; entity: CARDINAL):
  CardSet.T RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Returns a set with all attribute numbers in use for the given
     entity. *)


PROCEDURE GetAllAttributes (file: ITPFile.T; entity: CARDINAL):
  AttributeDescriptionSet.T
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Returns a set of all attribute numbers and values stored for the given
     entity. *)

EXCEPTION
  EntityNotFound;
  InternalError(AtomList.T);

END LongAttributeStorage.
