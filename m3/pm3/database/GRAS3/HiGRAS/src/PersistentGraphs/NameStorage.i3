INTERFACE NameStorage;

(***************************************************************************)
(* With this part of the file, names may be attached to an entity.
   Different names for one entity are distinguished by tags. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:39:20  roland
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

    Revision 1.3  1996/11/20 12:23:03  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/08/06 16:26:33  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.2  1996/07/24 09:20:08  rbnix
    	Error handling adjusted: internal errors are now guarded by
    	assertions rather than exceptions. This should simplify
    	locating errors.

    Revision 1.1.2.1  1996/04/29 13:43:45  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.1  1994/01/20  18:41:33  pk
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
  (* Initialize the NameStorage for the file. *)


PROCEDURE PutName (    file       : ITPFile.T;
                       tag        : CARDINAL;
                       name       : TEXT;
                       entity     : CARDINAL;
                   VAR otherEntity: CARDINAL;
                   VAR key1, key2 : CARDINAL;
                   VAR done       : BOOLEAN    )
  RAISES {Access.Locked, InternalError};
  (* Store name on tag for entity.  The keys return a unique internal
     representation of the (name, tag) tuple.  If the name is empty,
     nothing is stored and key1 returns 0.  If there is already an entity
     with the given name, done returns FALSE and otherEntity returns the
     number of the entity which also has the name.  An otherEntity value of
     0 denotes that there is already more than one entity with that
     name. *)


PROCEDURE PutEntity (file      : ITPFile.T;
                     key1, key2: CARDINAL;
                     entity    : CARDINAL   )
  RAISES {Access.Locked, NameNotFound, InternalError};
  (* Stores a new entity number for the name denoted by the keys. *)


PROCEDURE DeleteName (    file      : ITPFile.T;
                          tag       : CARDINAL;
                          name      : TEXT;
                      VAR entity    : CARDINAL;
                      VAR key1, key2: CARDINAL;
                      VAR multiple  : BOOLEAN    )
  RAISES {Access.Locked, NameNotFound, InternalError};
  (* Deletes the name on tag.  The procedure returns the entity and keys
     associated with the name.  If multiple returns TRUE, nothing happened
     because another entity has that name, too. *)


PROCEDURE DeleteNameByKeys (    file      : ITPFile.T;
                                key1, key2: CARDINAL;
                            VAR entity    : CARDINAL;
                            VAR multiple  : BOOLEAN    )
  RAISES {Access.Locked, NameNotFound, InternalError};
  (* Deletes the name described by the given keys.  The procedure returns
     the entity associated with the name.  If multiple returns TRUE,
     nothing happened because another entity has that name, too. *)


PROCEDURE FindName (    file      : ITPFile.T;
                        tag       : CARDINAL;
                        name      : TEXT;
                    VAR entity    : CARDINAL;
                    VAR key1, key2: CARDINAL;
                    VAR found     : BOOLEAN;
                    VAR multiple  : BOOLEAN    )
		   RAISES {Access.Locked, InternalError};
  (* Computes the entity and keys for name on tag.  If found returns FALSE,
     no such name exists.  If multiple returns TRUE, the entity could not
     be determined because more than one entity has that name. *)


PROCEDURE GetNameFromKeys (file: ITPFile.T; key1, key2: CARDINAL): TEXT
  RAISES {Access.Locked, NameNotFound, InternalError};
  (* Returns the name represented by the given keys. *)


PROCEDURE GetTagFromKeys (file: ITPFile.T; key1, key2: CARDINAL): CARDINAL
  RAISES {Access.Locked, NameNotFound, InternalError};
  (* Returns the tag represented by the given keys. *)


PROCEDURE GetNameAndTagFromKeys (    file      : ITPFile.T;
                                     key1, key2: CARDINAL;
                                 VAR name      : TEXT;
                                 VAR tag       : CARDINAL   )
  RAISES {Access.Locked, NameNotFound, InternalError};
  (* Returns the name and the tag represented by the given keys. *)

EXCEPTION
  InternalError(AtomList.T);
  NameNotFound;
  
END NameStorage.
