INTERFACE Names;


(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.4  1998/05/19 10:20:36  roland
    Bugfixes for local graphs and pretty-printing.

    Revision 1.3  1998/03/17 14:19:42  kluck
    Administration of local graphs implemented. (MK)

    Revision 1.2  1997/07/07 15:38:44  roland
    Corrected comments.

    Revision 1.1  1997/03/26 11:39:22  roland
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

*)
(***************************************************************************)

(* This abstract data type implements a collection of names.  Besides the
   global collection, subcollections may be declared and names inserted and
   removed from these collections.  Names can also be related by different
   - user-declared - binary relationships. *)

IMPORT TextIdSet, TextCursorSet;
IMPORT AtomList, Pathname, Access;
IMPORT VirtualResource;

CONST All = 0;

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      login (resource: VirtualResource.T; collectionname: Pathname.T)
             RAISES {Access.Locked, InternalError};
             (* Open the names-collection named 'collectionname'.  If it
                does not exist, it will be created. *)
      logout ();
              (* close the collection *)

      declareCollection (cname: TEXT): CARDINAL
                         RAISES {Access.Locked, InternalError};
                         (* This creates a new (empty) sub-collection of
                            names as subset of the global collection.  The
                            number returned is an identifier for this
                            collection that must be used for queries on
                            this collection.  If a collection with this
                            name already exists, DeclareCollection will
                            simply return its identifier. *)

      declareCollectionAttribute (attrname: TEXT): CARDINAL
                                  RAISES {Access.Locked, InternalError};
                                  (* A collection may have atributes (e.g.
                                     a counter for unique names.  The
                                     number returned serves as
                                     identification for future references
                                     to this attribute. *)

      setCollectionAttribute (attr : CARDINAL;
                              value: TEXT;
                              coll : CARDINAL   := All)
                              RAISES {Access.Locked, InternalError,
                                      Undeclared};
                              (* Set the attribute of the collection. *)

      getCollectionAttribute (attr: CARDINAL; coll: CARDINAL := All): TEXT
                              RAISES {Access.Locked, InternalError,
                                      Undeclared, Unknown};
                              (* Read the attribute of a collection *)

      getAllCollections (): TextIdSet.T
                         RAISES {Access.Locked, InternalError};
                         (* Return all declared collections. *)

      getAllDeclCollectionAttributes (): TextIdSet.T
                                      RAISES {Access.Locked, InternalError};
                                      (* Return all declared collection
                                         attributes *)

      insert (name: TEXT; local: BOOLEAN; collection: CARDINAL := All)
              RAISES {Undeclared, Access.Locked, InternalError};
              (* Insert a name in the collection collection.  All is the id
                 of the collection containing all names. *)

      remove (name: TEXT; local: BOOLEAN; collection: CARDINAL := All)
              RAISES {Undeclared, Access.Locked, InternalError};
              (* Remove name from collection.  If collection = All then
                 name is removed from the global collection as well as all
                 other collections. *)

      change (local: BOOLEAN; name: TEXT; new: TEXT)
              RAISES {Unknown, Access.Locked, InternalError};
              (* Rename name to new.  It is not tested if new exists in the
                 collection.  If this is the case, results of later queries
                 may lead to wrong results, because two entries with the
                 same name exist. *)

      id (name: TEXT; local: BOOLEAN; collection: CARDINAL := All):
          CARDINAL
          RAISES {Unknown, Undeclared, Access.Locked, InternalError};
          (* Returns a unique numeric identifier for name.  Name must be in
             the specified collection. *)

      isRemoteId (id: CARDINAL):BOOLEAN
          RAISES {Access.Locked, InternalError};
          (* Returns true if id is a declared name in the remote part of names. *)

      name (id: CARDINAL; local: BOOLEAN; collection: CARDINAL := All):
            TEXT
            RAISES {Unknown, Undeclared, Access.Locked, InternalError};
            (* Return the name belonging to id. *)

      contained (name: TEXT; local: BOOLEAN; collection: CARDINAL := All):
                 BOOLEAN RAISES {Undeclared, Access.Locked, InternalError};
                 (* Test whether name is in collection *)

      getAllEntries (local: BOOLEAN; collection: CARDINAL := All):
                     TextCursorSet.T
                     RAISES {Undeclared, Access.Locked, InternalError};
                     (* Returns the set of all names in collection. *)

      declareAttribute (aname: TEXT): CARDINAL
                        RAISES {Access.Locked, InternalError};
                        (* Declare a attribute for entries *)

      setAttribute (name: TEXT; local: BOOLEAN; attr: CARDINAL; value: TEXT)
                    RAISES {Access.Locked, InternalError, Undeclared,
                            Unknown};
                    (* Set the value of attribute attr of entry name to
                       value *)

      getAttribute (name: TEXT; local: BOOLEAN; attr: CARDINAL): TEXT
                    RAISES {Access.Locked, InternalError, Undeclared,
                            Unknown};
                    (* Read the value of attribute attr of entry name *)

      getAllDeclAttributes (): TextIdSet.T
                            RAISES {Access.Locked, InternalError};
                            (* Return all declared attributes *)

      declareRelation (rname: TEXT): CARDINAL
                       RAISES {Access.Locked, InternalError};
                       (* Declares a (new) relation in which names may
                          participate.  The returned number serves as
                          identification for queries. *)

      getAllRelations (): TextIdSet.T
                       RAISES {Access.Locked, InternalError};

      insertInRelation (n1: TEXT; n2: TEXT; local: BOOLEAN; rel: CARDINAL)
                        RAISES {Undeclared, Unknown, Access.Locked,
                                InternalError};
                        (* Puts (n1, n2) in relation rel.  n1 and n2 must
                           be members of the collection. *)

      removeFromRelation (n1: TEXT; n2: TEXT; local: BOOLEAN; rel: CARDINAL)
                          RAISES {Undeclared, Access.Locked, InternalError};
                          (* removes (n1, n2) from rel. *)

      targets (n1: TEXT; local: BOOLEAN; rel: CARDINAL): TextCursorSet.T
               RAISES {Undeclared, Unknown, Access.Locked, InternalError};
               (* Return all x with (n1, x) in rel. *)

      sources (n2: TEXT; local: BOOLEAN; rel: CARDINAL): TextCursorSet.T
               RAISES {Undeclared, Unknown, Access.Locked, InternalError};
               (* Return all y with (y, n2) in rel. *)

      related (local: BOOLEAN; n1, n2: TEXT; rel: CARDINAL): BOOLEAN
               RAISES {Undeclared, Unknown, Access.Locked, InternalError};
               (* TRUE iff (n1, n2) in rel. *)
    END;

EXCEPTION
  Undeclared;                    (* An operation was called with an
                                    undeclared collection or relation. *)
  Unknown;                       (* A name was referenced that is not in
                                    the collection *)
  InternalError(AtomList.T);

END Names.




