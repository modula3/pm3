INTERFACE EntityStorage;

(***************************************************************************)
(* This module contains the part of the file which is used for storage of
   basic entities.  Each entity has a label and an associated attribute. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:39:05  roland
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

    Revision 1.4  1996/11/20 12:22:52  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/08/06 16:26:26  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/07/24 09:19:56  rbnix
    	Error handling adjusted: internal errors are now guarded by
    	assertions rather than exceptions. This should simplify
    	locating errors.

    Revision 1.2.2.1  1996/04/29 13:43:34  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.2  1994/04/01  14:50:39  pk
# CreateEntity/Node returns the node number.
#
# Revision 1.1  1994/01/20  18:41:21  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT
  AtomList;
IMPORT
  Access,
  ITPFile;


PROCEDURE Init (file: ITPFile.T) RAISES {Access.Locked, InternalError};
  (* Initialize the EntityStorage for the file. *)


PROCEDURE CreateEntityNumber (file: ITPFile.T; neighbour: CARDINAL):
  CARDINAL RAISES {Access.Locked, InternalError};
  (* Create a number for an entity.  This is to be used only immediately
     before a call to CreateEntity.  neighbour may denote an entity in
     whose `neighbourship' the new entity should be created. *)


PROCEDURE CreateEntity (label: CARDINAL): CARDINAL RAISES {Access.Locked, InternalError};
  (* Creates an entity with the given label and the number created by a
     previous call to CreateEntityNumber.  The entity number is returned
     again. *)


PROCEDURE DeleteEntity (file: ITPFile.T; entity: CARDINAL)
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Removes the entity with the given number. *)


PROCEDURE ExistsEntity (file: ITPFile.T; entity: CARDINAL): BOOLEAN
  RAISES {Access.Locked, InternalError};
  (* Returns TRUE if entity exists. *)


PROCEDURE PutLabel (file: ITPFile.T; entity: CARDINAL; label: CARDINAL)
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Stores a label for the given entity. *)


PROCEDURE GetLabel (file: ITPFile.T; entity: CARDINAL): CARDINAL
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Retrieves the label for the given entity. *)


PROCEDURE PutAttribute (file: ITPFile.T; entity: CARDINAL; attribute: TEXT)
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Stores an attribute for the given entity. *)


PROCEDURE GetAttribute (file: ITPFile.T; entity: CARDINAL): TEXT
  RAISES {Access.Locked, EntityNotFound, InternalError};
  (* Retrieves the attribute for the given entity. *)

EXCEPTION
  EntityNotFound;
  InternalError(AtomList.T);
  
END EntityStorage.
