INTERFACE InternPersistentGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.4  1998/03/17 14:14:14  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.3  1997/04/24 14:32:25  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.2  1997/04/23 14:33:46  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:10  roland
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

IMPORT PersistentGraphPool, PersistentNames, ExtConnectorStorage, Database,
       PageFile, Access;
IMPORT Pathname;

REVEAL PersistentGraphPool.T <: Internal;

TYPE
  Internal =
    PersistentGraphPool.Public OBJECT
    METHODS
      openIntern (name: Pathname.T; access: Access.Mode; new: BOOLEAN):
                  PersistentGraphPool.T
                  RAISES {Access.Denied, PageFile.NoAccess};
                  (* Used for opening a pool through the inheritance
                     hierarchie *)

      loginToNames (names: PersistentNames.T)
                    RAISES {Access.Locked,
                            PersistentGraphPool.InternalError};
                    (* Used for opening a pool through the inheritance
                       hierarchie *)

      externalRelations ( g1    : CARDINAL;
                          g2    : CARDINAL;
                          local : BOOLEAN   ) : ExtConnectorStorage.T
                         RAISES {PersistentGraphPool.InternalError,
                                 PersistentGraphPool.NotExistent,
                                 Access.Locked};
                         (* Return (and create if necessary) the storage
                            for relations between g1 and g2. *)

      iterateNeighbours (graph: CARDINAL): NeighbourIterator
                         RAISES {PersistentGraphPool.NotExistent};
                         (* Return an iterator with which all external
                            relation of graph can be visited. *)
      openDB (    graph  : Pathname.T;
                  local  : BOOLEAN := FALSE; 
                  access : Access.Mode;
                  errChk : BOOLEAN;
              VAR number : CARDINAL;
              VAR db     : Database.T  )
              RAISES {PersistentGraphPool.InternalError, Access.Locked,
                      PageFile.NoAccess, Access.Denied};
              (* A graph has been opened, insert it in the collection (if
                 new) and return its id and database *)
      closeDB (id: CARDINAL) RAISES {PersistentGraphPool.InternalError};
               (* The graph with number 'id' was closed. *)

    END;

  NeighbourIterator =
    OBJECT
    METHODS
      get (VAR neighb: CARDINAL): BOOLEAN RAISES {};
           (* If not all neighbours were visited, go to the next and return
              TRUE, otherwise return FALSE. *)
    END;

END InternPersistentGraphPool.
