INTERFACE RelationStorage;

(***************************************************************************)
(* This part of the file serves to store relations of the form (order,
   entity1, entity2, entity3). *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:39:44  roland
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

    Revision 1.4  1996/11/20 12:23:16  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/17 13:09:15  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.2  1996/08/06 16:26:43  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/07/24 09:20:14  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.1.2.1  1996/04/29 13:43:53  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.1  1994/01/20  18:41:50  pk
# Initial revision
#
*)
(***************************************************************************)
IMPORT CardSet, Access, ITPFile, RelationRecordStorage, CardRelation;
IMPORT AtomList;

TYPE
  RelationRange = RECORD
                    recordRange: RelationRecordStorage.Range;
                    location   : RelationRecordStorage.RecordLocation;
                    valid      : BOOLEAN;
                    elementNo  : CARDINAL;
                  END;
    (* Describes a search range in the relation storage.  This is
       opaque. *)

  Order = [1 .. 6];


PROCEDURE Init (file: ITPFile.T; tree: CARDINAL)
  RAISES {Access.Locked, InternalError};
  (* Initialize the RelationStorage for the file. *)


PROCEDURE PutRelation (file                     : ITPFile.T;
                       tree                     : CARDINAL;
                       order                    : Order;
                       entity1, entity2, entity3: CARDINAL   )
  RAISES {Access.Locked, InternalError};
  (* Stores the relation (order, entity1, entity2, entity3). *)


PROCEDURE DeleteRelation (file                     : ITPFile.T;
                          tree                     : CARDINAL;
                          order                    : Order;
                          entity1, entity2, entity3: CARDINAL   )
  RAISES {Access.Locked, RelationNotFound, InternalError};
  (* Deletes the relation (order, entity1, entity2, entity3). *)


PROCEDURE DeleteRelationsByEntity (    file   : ITPFile.T;
                                       tree   : CARDINAL;
                                       order  : Order;
                                       entity : CARDINAL;
                                   VAR deleted: CardRelation.T )
  RAISES {Access.Locked, InternalError};
  (* Deletes all relations (order, entity, ?, ?).  The second and third
     components of the deleted relations are returned in the implicitly
     created set deleted. *)


PROCEDURE IsARelation (file                     : ITPFile.T;
                       tree                     : CARDINAL;
                       order                    : Order;
                       entity1, entity2, entity3: CARDINAL   ): BOOLEAN
  RAISES {Access.Locked, InternalError};
  (* Returns TRUE if (order, entity1, entity2, entity3) is a stored
     relation. *)


PROCEDURE InitRelationRange (file  : ITPFile.T;
                             tree  : CARDINAL;
                             order : Order;
                             entity: CARDINAL   ): RelationRange
  RAISES {Access.Locked, InternalError};
  (* Returns a range to loop over all (order, entity, ?, ?) relations. *)


PROCEDURE GetFromRelationRange (    file            : ITPFile.T;
                                    tree            : CARDINAL;
                                VAR range           : RelationRange;
                                VAR found           : BOOLEAN;
                                VAR entity2, entity3: CARDINAL       )
  RAISES {Access.Locked, InternalError};
  (* Computes the current (entity2, entity3) part of a relation in range
     and advances the range pointer to the next relation.  If the last
     relation in the range was already retrieved, found returns FALSE. *)


PROCEDURE GetRelationRange (         file   : ITPFile.T;
                                     tree   : CARDINAL;
                            READONLY range  : RelationRange;
                                     permute: BOOLEAN        ): CardRelation.T
  RAISES {Access.Locked, InternalError};
  (* If permute is FALSE, returns all (entity2, entity3) pairs in the range
     in a set.  If permute is TRUE, the set is filled with (entity3,
     entity2) pairs.  The current element of the set is the current element
     of the range. *)


PROCEDURE NoOfThirdComponents (file            : ITPFile.T;
                               tree            : CARDINAL;
                               order           : Order;
                               entity1, entity2: CARDINAL   ): CARDINAL
  RAISES {Access.Locked, InternalError};
  (* Computes the number of all (order, entity1, entity2, ?) relations. *)


PROCEDURE GetThirdComponents (file            : ITPFile.T;
                              tree            : CARDINAL;
                              order           : Order;
                              entity1, entity2: CARDINAL   ): CardSet.T
  RAISES {Access.Locked, InternalError};
  (* Returns a set with all tuples (entity3, 0) where (order, entity1,
     entity2, entity3) is a relation. *)


PROCEDURE GetFirstThirdComponent (    file            : ITPFile.T;
                                      tree            : CARDINAL;
                                      order           : Order;
                                      entity1, entity2: CARDINAL;
                                  VAR found           : BOOLEAN    ):
  CARDINAL RAISES {Access.Locked, InternalError};
  (* Returns the first entity3 where (order, entity1, entity2, entity3) is
     a relation. *)

EXCEPTION
  RelationNotFound;
  InternalError(AtomList.T);

END RelationStorage.
