MODULE PersistentGraphPool
         EXPORTS PersistentGraphPool, InternPersistentGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:46  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:32  hosking
    Import of GRAS3 1.1

    Revision 1.9  1998/05/19 10:20:39  roland
    Bugfixes for local graphs and pretty-printing.

    Revision 1.8  1998/03/18 13:39:33  roland
    More slight modifications to local parameters (default values and
    parameter ordering)

    Revision 1.7  1998/03/17 14:14:18  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.6  1997/10/31 14:20:33  roland
    Adapted to new RuleEngine.

    Revision 1.5  1997/10/24 14:17:26  renehuel
    Bugfix in CopyGraph.
    Starting to copy pages with pagenr 0, not 1.

    Revision 1.4  1997/07/21 10:43:04  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.3  1997/04/24 14:32:49  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.2  1997/04/23 14:33:55  roland
    Minor bugfixes and adaptions.

    Revision 1.1  1997/03/26 11:39:32  roland
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

IMPORT PersistentNames, Names, Access, PageFile, PageData;
IMPORT ExtConnectorStorage, TextCursorSet;
IMPORT ClientInfoSeq, Database, ErrorSupport;
IMPORT VirtualResource AS Super;
IMPORT Pathname, Fmt, TextSeq, Text;
IMPORT Txn, VirtualPage, VirtualFile, VirtualRemoteFile, VirtualLocalFile;

REVEAL
  T = Internal BRANDED OBJECT
        graphs    : PersistentNames.T;
        extRels   : ERTable;
        openGraphs: OGTable;
      OVERRIDES
        (* public *)
        open              := Open;
        close             := Close;
        beginTransaction  := BeginTransaction;
        commitTransaction := CommitTransaction;
        abortTransaction  := AbortTransaction;
        getNames          := GetNames;

        deleteGraph   := DeleteGraph;
        copyGraph     := IntraCopyGraph;
        copyGraphs    := IntraCopyGraphs;
        renameGraph   := RenameGraph;
        existsGraph   := ExistsGraph;
        graphNumber   := GraphNumber;
        graphInUse    := GraphInUse;
        getGraphUser  := GetGraphUser;
        getGraphs     := GetGraphs;
        getNeighbours := GetNeighbours;

        (* internal *)
        openIntern        := OpenIntern;
        loginToNames      := LoginToNames;
        externalRelations := ExternalRelations;
        iterateNeighbours := IterateNeighbours;
        closeDB           := CloseDB;
        openDB            := OpenDB;
      END;

PROCEDURE ErrAbort (pool: T) =
  BEGIN
    TRY
      Super.T.abortTransaction(pool);
    EXCEPT
      Super.NotInTransaction =>  (* ignore *)
    | Super.FatalError =>        (* ignore *)
    END;
  END ErrAbort;

PROCEDURE OpenIntern (pool  : T;
                      name  : Pathname.T;
                      access: Access.Mode;
                      new   : BOOLEAN      ): T
  RAISES {Access.Denied, PageFile.NoAccess} =
  BEGIN
    pool := Super.T.open(pool, name, access, new);
    ERTableInit(pool.extRels);
    OGTableInit(pool.openGraphs);
    RETURN pool;
  END OpenIntern;

PROCEDURE LoginToNames (pool: T; names: PersistentNames.T)
  RAISES {InternalError, Access.Locked} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      pool.graphs := names;
      PersistentNames.T.login(names, pool, ".GRAS");
      Super.T.commitTransaction(pool);
    EXCEPT
      Names.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraphPool.LoginToNames",
                                       "Names.InternalError", info));
    | Super.FatalError (info) =>
        ErrAbort(pool);
        RAISE
          InternalError(ErrorSupport.Propagate("PersistentGraphPool.Open",
                                               "Super.FatalError", info));
    | Access.Locked => ErrAbort(pool); RAISE Access.Locked;
    END;
  END LoginToNames;

PROCEDURE Open (pool  : T;
                name  : Pathname.T;
                access: Access.Mode;
                new   : BOOLEAN      ): T
  RAISES {InternalError, Access.Denied, PageFile.NoAccess, Access.Locked} =
  BEGIN
    pool := OpenIntern(pool, name, access, new);
    LoginToNames(pool, NEW(PersistentNames.T));
    RETURN pool;
  END Open;

PROCEDURE Close (pool: T) RAISES {InternalError} =
  BEGIN
    TRY
      IF pool.graphs # NIL THEN pool.graphs.logout(); END;
      Super.T.close(pool);
    EXCEPT
      Super.FatalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate("PersistentGraphPool.Close",
                                               "Super.FatalError", info));
    END;
  END Close;

PROCEDURE BeginTransaction (pool: T) RAISES {InternalError} =
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
    EXCEPT
      Super.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.BeginTransaction",
                              "Super.FatalError", info));
    END;
  END BeginTransaction;

PROCEDURE CommitTransaction (pool: T)
  RAISES {InternalError, NotInTransaction} =
  BEGIN
    TRY
      Super.T.commitTransaction(pool);
    EXCEPT
      Super.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.CommitTransaction",
                              "Super.FatalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    END;
  END CommitTransaction;

PROCEDURE AbortTransaction (pool: T)
  RAISES {InternalError, NotInTransaction} =
  BEGIN
    TRY
      Super.T.abortTransaction(pool);
    EXCEPT
      Super.FatalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.AbortTransaction",
                              "Super.FatalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    END;
  END AbortTransaction;

PROCEDURE GetNames (pool: T): Names.T =
  BEGIN
    RETURN pool.graphs;
  END GetNames;

PROCEDURE DeleteGraph (pool    : T;
                       baseName: Pathname.T;
                       local   : BOOLEAN      := FALSE)
  RAISES {InUse, NotExistent, InternalError, Access.Locked} =
  VAR
    extrels: TextCursorSet.T;
    extrel : Pathname.T;
    erfound: BOOLEAN;
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      IF Super.T.fileInUse(pool, baseName, local) THEN RAISE InUse END;
      (* handle local graphs *)
      IF local THEN Super.T.deleteFile(pool, baseName, local); RETURN; END;
      (* look for any neighbour relations *)
      Super.T.beginTransaction(pool);
      IF NOT pool.graphs.existsGraph(baseName, local) THEN
        Super.T.abortTransaction(pool);
        RAISE NotExistent;
      END;
      pool.graphs.removeGraph(baseName, local, extrels);

      (* Look for any conflicts with neighbours *)
      extrels.loop();
      extrel := extrels.get(erfound);
      WHILE erfound DO
        IF Super.T.fileInUse(pool, extrel, local) THEN
          Super.T.abortTransaction(pool);
          RAISE InUse;
        END;
        extrel := extrels.get(erfound);
      END;

      (* When no conflicts were found, the relations can be deleted *)
      extrels.loop();
      extrel := extrels.get(erfound);
      WHILE erfound DO
        Super.T.deleteFile(pool, extrel, local);
        extrel := extrels.get(erfound);
      END;
      extrels.dispose();

      Super.T.deleteFile(pool, baseName, local);
      Super.T.commitTransaction(pool);
    EXCEPT
      Super.FatalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraphPool.DeleteGraph",
                                       "Super.FatalError", info));
    | PageFile.NoAccess (msg) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.DeleteGraph",
                                    "PageFile.NoAccess: " & msg));
    | PersistentNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.DeleteGraph",
                              "PersistentNames.InternalError", info));
    END;
  END DeleteGraph;

PROCEDURE IntraCopyGraph (pool      : T;
                          sourceName: Pathname.T;
                          destName  : Pathname.T;
                          embedded  : BOOLEAN;
                          local     : BOOLEAN      := FALSE)
  RAISES {InUse, Existent, NotExistent, InternalError, Access.Locked} =
  VAR
    neighs         : TextCursorSet.T;
    nb, exr, newexr: Pathname.T;
    found          : BOOLEAN;
    nbId, destId   : CARDINAL;
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      IF NOT pool.graphs.existsGraph(sourceName, local) THEN
        Super.T.abortTransaction(pool);
        RAISE NotExistent;
      END;
      IF Super.T.fileInUse(pool, sourceName, local) THEN
        Super.T.abortTransaction(pool);
        RAISE InUse;
      END;
      IF pool.graphs.existsGraph(destName, local) THEN
        Super.T.abortTransaction(pool);
        RAISE Existent;
      END;
      Super.T.copyFile(pool, sourceName, destName, local);
      pool.graphs.copy(sourceName, destName);
      IF embedded THEN
        destId := pool.graphs.getGraphNumber(destName, local);
        (* get all neighbours and copy the external relation files *)
        neighs := pool.graphs.getAllNeighbours(sourceName, local);
        neighs.loop();
        nb := neighs.get(found);
        WHILE found DO
          exr := pool.graphs.getExternalRelation(sourceName, nb, local);
          nbId := pool.graphs.getGraphNumber(nb, local);
          newexr := ExtRelName(destId, nbId);
          Super.T.copyFile(pool, exr, newexr, local);
          pool.graphs.insertExtRelation(destName, nb, local, newexr);
          nb := neighs.get(found);
        END;
        neighs.dispose();
      END;
      Super.T.commitTransaction(pool);
    EXCEPT
      PersistentNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.IntraCopyGraph",
                              "PersistentNames.InternalError", info));
    | PageFile.NoAccess (msg) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.IntraCopyGraph",
                                    "PageFile.NoAccess: " & msg));
    | Super.FatalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.IntraCopyGraph",
                              "Super.FatalError", info));
    | PersistentNames.Unknown =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.IntraCopyGraph",
                                    "PersistentNames.Unknown"));
    | PersistentNames.Existent =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.IntraCopyGraph",
                                    "PersistentNames.Existent"));
    END;
  END IntraCopyGraph;

PROCEDURE IntraCopyGraphs (pool        : T;
                           sources     : TextSeq.T;
                           destinations: TextSeq.T;
                           embedded    : BOOLEAN;
                           local       : BOOLEAN     := FALSE)
  RAISES {InUse, Existent, NotExistent, InternalError, Access.Locked} =
  VAR
    neighs, allNewExR: TextCursorSet.T;
    nb, exr, newexr  : Pathname.T;
    found, intern    : BOOLEAN;
    nbId, destId     : CARDINAL;
    nbindex          : CARDINAL;

  PROCEDURE InSources (    sources: TextSeq.T;
                           nb     : Pathname.T;
                       VAR nbindex: CARDINAL    ): BOOLEAN =
    BEGIN
      FOR i := 0 TO sources.size() - 1 DO
        IF Text.Equal(sources.get(i), nb) THEN
          nbindex := i;
          RETURN TRUE;
        END;
      END;
      RETURN FALSE;
    END InSources;

  <* FATAL Super.NotInTransaction *>
  BEGIN
    IF sources.size() > destinations.size() THEN RAISE NotExistent END;
    TRY
      Super.T.beginTransaction(pool);
      FOR i := 0 TO sources.size() - 1 DO
        IF NOT pool.graphs.existsGraph(sources.get(i), local) THEN
          Super.T.abortTransaction(pool);
          RAISE NotExistent
        END;
        IF Super.T.fileInUse(pool, sources.get(i), local) THEN
          Super.T.abortTransaction(pool);
          RAISE InUse;
        END;
        IF pool.graphs.existsGraph(destinations.get(i), local) THEN
          Super.T.abortTransaction(pool);
          RAISE Existent
        END;
      END;

      (* copy plain graphs *)
      FOR i := 0 TO sources.size() - 1 DO
        Super.T.copyFile(pool, sources.get(i), destinations.get(i), local);
        pool.graphs.copy(sources.get(i), destinations.get(i));
      END;

      (* Now copy all external relation storages connected to sources.  If
         not embedded, copy only thoes connected to a graph within
         sources. *)
      allNewExR := TextCursorSet.New();
      FOR i := 0 TO sources.size() - 1 DO
        destId := pool.graphs.getGraphNumber(destinations.get(i), local);
        (* get all neighbours and copy the external relation files *)
        neighs := pool.graphs.getAllNeighbours(sources.get(i), local);
        neighs.loop();
        nb := neighs.get(found);
        WHILE found DO
          intern := InSources(sources, nb, nbindex);
          IF intern THEN
            exr := pool.graphs.getExternalRelation(
                     sources.get(i), sources.get(nbindex), local);
            nbId :=
              pool.graphs.getGraphNumber(destinations.get(nbindex), local);
            newexr := ExtRelName(destId, nbId);
            IF NOT allNewExR.in(newexr) THEN
              (* copy only, if not already done *)
              Super.T.copyFile(pool, exr, newexr, local);
              pool.graphs.insertExtRelation(
                destinations.get(i), destinations.get(nbindex), local,
                newexr);
              allNewExR.insert(newexr);
            END;
          ELSIF embedded THEN
            exr :=
              pool.graphs.getExternalRelation(sources.get(i), nb, local);
            nbId := pool.graphs.getGraphNumber(nb, local);
            newexr := ExtRelName(destId, nbId);
            Super.T.copyFile(pool, exr, newexr, local);
            pool.graphs.insertExtRelation(
              destinations.get(i), nb, local, newexr);
          END;
          nb := neighs.get(found);
        END;
        neighs.dispose();
      END;
      Super.T.commitTransaction(pool);
      allNewExR.dispose();
    EXCEPT
      PersistentNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.IntraCopyGraph",
                              "PersistentNames.InternalError", info));
    | PageFile.NoAccess (msg) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.IntraCopyGraph",
                                    "PageFile.NoAccess: " & msg));
    | Super.FatalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.IntraCopyGraph",
                              "Super.FatalError", info));
    | PersistentNames.Unknown =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.IntraCopyGraph",
                                    "PersistentNames.Unknown"));
    | PersistentNames.Existent =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.IntraCopyGraph",
                                    "PersistentNames.Existent"));
    END;
  END IntraCopyGraphs;

PROCEDURE RenameGraph (pool   : T;
                       oldName: Pathname.T;
                       newName: Pathname.T;
                       local  : BOOLEAN      := FALSE)
  RAISES {InUse, Existent, NotExistent, InternalError, Access.Locked} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      IF NOT pool.graphs.existsGraph(oldName, local) THEN
        Super.T.abortTransaction(pool);
        RAISE NotExistent;
      END;
      IF Super.T.fileInUse(pool, oldName, local) THEN
        Super.T.abortTransaction(pool);
        RAISE InUse;
      END;
      IF pool.graphs.existsGraph(newName, local) THEN
        Super.T.abortTransaction(pool);
        RAISE Existent;
      END;
      pool.graphs.renameGraph(local, oldName, newName);
      Super.T.renameFile(pool, oldName, newName, local);
      Super.T.commitTransaction(pool);
    EXCEPT
      PersistentNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.ExistsGraph",
                              "PersistentNames.InternalError", info));
    | PersistentNames.Unknown =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.RenameGraph",
                                    "PersistentNames.Unknown"));
    | Super.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraphPool.RenameGraph",
                                       "Super.FatalError", info));
    | PageFile.NoAccess (msg) =>
        RAISE InternalError(
                ErrorSupport.Create("PersistentGraphPool.RenameGraph",
                                    "PageFile.NoAccess: " & msg));
    END;
  END RenameGraph;

PROCEDURE ExistsGraph (pool    : T;
                       baseName: Pathname.T;
                       local   : BOOLEAN      := FALSE): BOOLEAN
  RAISES {InternalError, Access.Locked} =
  BEGIN
    TRY
      RETURN pool.graphs.existsGraph(baseName, local);
    EXCEPT
      PersistentNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.ExistsGraph",
                              "PersistentNames.InternalError", info));
    END;
  END ExistsGraph;

PROCEDURE GraphNumber (pool: T; name: Pathname.T; local: BOOLEAN := FALSE):
  CARDINAL RAISES {InternalError, NotExistent, Access.Locked} =
  BEGIN
    TRY
      RETURN pool.graphs.getGraphNumber(name, local);
    EXCEPT
      PersistentNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.GetGraphNumber",
                              "PersistentNames.InternalError", info));
    | PersistentNames.Unknown => RAISE NotExistent;
    END;
  END GraphNumber;

PROCEDURE GraphInUse (pool    : T;
                      baseName: Pathname.T;
                      local   : BOOLEAN      := FALSE): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN Super.T.fileInUse(pool, baseName, local);
    EXCEPT
      Super.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraphPool.GraphInUse",
                                       "Super.FatalError", info));
    END;
  END GraphInUse;

PROCEDURE GetGraphUser (pool: T; baseName: Pathname.T): ClientInfoSeq.T
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN Super.T.getFileUser(pool, baseName);
    EXCEPT
      Super.FatalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraphPool.GetGraphUser",
                                       "Super.FatalError", info));
    END;
  END GetGraphUser;

PROCEDURE GetGraphs (pool: T; local: BOOLEAN := FALSE): TextCursorSet.T
  RAISES {InternalError, Access.Locked} =
  BEGIN
    TRY
      RETURN pool.graphs.getGraphs(local);
    EXCEPT
      PersistentNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.GetGraphs",
                              "PersistentNames.InternalError", info));
    END;
  END GetGraphs;

PROCEDURE GetNeighbours (pool : T;
                         graph: Pathname.T;
                         local: BOOLEAN      := FALSE): TextCursorSet.T
  RAISES {NotExistent, InternalError, Access.Locked} =
  BEGIN
    TRY
      RETURN pool.graphs.getAllNeighbours(graph, local);
    EXCEPT
      PersistentNames.Unknown => RAISE NotExistent;
    | PersistentNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.GetNeighbours",
                              "PersistentNames.InternalError", info));
    END;
  END GetNeighbours;


PROCEDURE ExternalRelations (pool : T;
                             g1   : CARDINAL;
                             g2   : CARDINAL;
                             local: BOOLEAN   ): ExtConnectorStorage.T
  RAISES {InternalError, NotExistent, Access.Locked} =
  <* FATAL ERTableNotFound, OGTableNotFound *>
  VAR
    storage               : ExtConnectorStorage.T;
    stname, g1name, g2name: Pathname.T;
    db                    : Database.T;
    erlist                : ExtRelList;
    openCount             : CARDINAL;
  BEGIN
    TRY
      g1name := pool.graphs.getGraphName(g1, local);
      g2name := pool.graphs.getGraphName(g2, local);
    EXCEPT
      PersistentNames.Unknown => RAISE NotExistent;
    | PersistentNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.ExternalRelations",
                              "PersistentNames.InternalError", info));
    END;
    TRY
      IF ERTableExists(pool.extRels, g1, g2) THEN
        ERTableGet(pool.extRels, g1, g2, stname, storage);
        RETURN storage;
      ELSE
        stname := ExtRelName(g1, g2);
        (* check if storage physically exists *)
        IF pool.graphs.existsExtRelation(g1name, g2name, local) THEN
          (* it does, so open it *)
          storage := NEW(ExtConnectorStorage.T).open(
                       pool, stname, pool.getAccessMode(), new := FALSE,
                       local := FALSE, db1 := g1, db2 := g2);
        ELSE
          (* storage does not exist, create one *)
          storage := NEW(ExtConnectorStorage.T).open(
                       pool, stname, pool.getAccessMode(), new := TRUE,
                       local := FALSE, db1 := g1, db2 := g2);
          pool.graphs.insertExtRelation(g1name, g2name, local, stname);
        END;
        (* insert storage in ERTable *)
        ERTablePut(pool.extRels, g1, g2, stname, storage);
        (* update erlist of both incident graphs *)
        IF IsOpen(pool, g1) THEN
          OGTableGet(pool.openGraphs, g1, g1name, db, openCount, erlist);
          ERListInsert(erlist, g2);
          OGTablePut(pool.openGraphs, g1, g1name, db, openCount, erlist);
        END;
        IF IsOpen(pool, g2) THEN
          OGTableGet(pool.openGraphs, g2, g2name, db, openCount, erlist);
          ERListInsert(erlist, g1);
          OGTablePut(pool.openGraphs, g2, g2name, db, openCount, erlist);
        END;
        RETURN storage;
      END;
    EXCEPT
      PersistentNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.ExternalRelations",
                              "PersistentNames.InternalError", info));
    | PersistentNames.Unknown =>
        RAISE InternalError(ErrorSupport.Create(
                              "PersistentGraphPool.ExternalRelations",
                              "PersistentNames.Unknown"));
    | PersistentNames.Existent =>
        RAISE InternalError(ErrorSupport.Create(
                              "PersistentGraphPool.ExternalRelations",
                              "PersistentNames.Existent"));
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.ExternalRelations",
                              "ExtConnectorStorage.InternalError", info));
    | PageFile.NoAccess (msg) =>
        RAISE InternalError(ErrorSupport.Create(
                              "PersistentGraphPool.ExternalRelations",
                              "PageFile.NoAccess: " & msg));
    | Access.Denied (msg) =>
        RAISE InternalError(ErrorSupport.Create(
                              "PersistentGraphPool.ExternalRelations",
                              "Access.Denied: " & msg));
    END;
  END ExternalRelations;

TYPE
  NeighbourIteratorPriv = NeighbourIterator BRANDED OBJECT
                            pool     : T;
                            graph    : CARDINAL;
                            list, act: ExtRelList := NIL;
                          OVERRIDES
                            get := IteratorGet;
                          END;

PROCEDURE IteratorGet (it: NeighbourIteratorPriv; VAR neighb: CARDINAL):
  BOOLEAN RAISES {} =
  BEGIN
    IF it.act # NIL THEN
      neighb := it.act^.neighbour;
      it.act := ERListGetNext(it.act);
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END IteratorGet;

PROCEDURE IterateNeighbours (pool: T; graph: CARDINAL): NeighbourIterator
  RAISES {NotExistent} =
  VAR
    erlist   : ExtRelList;
    openCount: CARDINAL;
    dummy    : Pathname.T;
    db       : Database.T;
  <* FATAL OGTableNotFound *>
  BEGIN
    (* This is only possible, if graph is open *)
    IF NOT IsOpen(pool, graph) THEN RAISE NotExistent END;
    OGTableGet(pool.openGraphs, graph, dummy, db, openCount, erlist);
    RETURN NEW(NeighbourIteratorPriv, pool := pool, graph := graph,
               list := erlist, act := erlist);
  END IterateNeighbours;

PROCEDURE OpenDB (    pool    : T;
                      graph   : Pathname.T;
                      local   : BOOLEAN;
                      access  : Access.Mode;
                      errCheck: BOOLEAN;
                  VAR number  : CARDINAL;
                  VAR db      : Database.T   )
  RAISES {Access.Denied, InternalError, Access.Locked, PageFile.NoAccess} =
  VAR
    erlist   : ExtRelList;
    openCount: CARDINAL;
    dummy    : Pathname.T;
    neighs   : TextCursorSet.T;
    found    : BOOLEAN;
    nb       : Pathname.T;
  <* FATAL OGTableNotFound *>
  BEGIN
    TRY
      IF pool.graphs.existsGraph(graph, local) THEN
        (* this is an already existing graph, get the number from names *)
        number := pool.graphs.getGraphNumber(graph, local);
        (*-- the number is unique, therefore you don't need the local parm
           --*)
        (*-- for IsOpen --*)
        IF IsOpen(pool, number) THEN
          (* this client already holds a copy of db *)
          OGTableGet(pool.openGraphs, number, dummy, db, openCount, erlist);
          INC(openCount);
          OGTablePut(pool.openGraphs, number, dummy, db, openCount, erlist);
        ELSE
          (* the graph exists, but was not used by this client, yet *)
          (* open the database file *)
          db := NEW(Database.T).open(pool, graph, access, FALSE (*new*),
                                     local, errorChecks := errCheck);
          (* find all its neighbours and insert them into the external
             relationship list *)
          ERListInit(erlist);
          neighs := pool.graphs.getAllNeighbours(graph, local);
          neighs.loop();
          nb := neighs.get(found);
          WHILE found DO
            ERListInsert(erlist, pool.graphs.getGraphNumber(nb, local));
            nb := neighs.get(found);
          END;
          neighs.dispose();
          (* create new entry in open-graph table *)
          OGTablePut(pool.openGraphs, number, graph, db, 1, erlist);
        END;
      ELSE
        (* graph db does not exist, create it *)
        db := NEW(Database.T).open(pool, graph, access, TRUE (*new*),
                                   local, errorChecks := errCheck);
        pool.graphs.insertGraph(graph, local);
        number := pool.graphs.getGraphNumber(graph, local);
        (* create new entry in open-graph table; a new db does'nt have any
           neighbours *)
        OGTablePut(pool.openGraphs, number, graph, db, 1, NIL);
      END;
    EXCEPT
      PersistentNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.OpenDB",
                              "PersistentNames.InternalError", info));
    | PersistentNames.Unknown =>
        RAISE
          InternalError(ErrorSupport.Create("PersistentGraphPool.OpenDB",
                                            "PersistentNames.Unknown"));
    END;
  END OpenDB;

PROCEDURE CloseDB (pool: T; id: CARDINAL) RAISES {InternalError} =
  VAR
    erlist   : ExtRelList;
    openCount: CARDINAL;
    name     : Pathname.T;
    db       : Database.T;
    ername   : Pathname.T;
    erstorage: ExtConnectorStorage.T;
  <* FATAL OGTableNotFound, ERTableNotFound *>
  BEGIN
    TRY
      IF IsOpen(pool, id) THEN
        (* retrieve information about open graph *)
        OGTableGet(pool.openGraphs, id, name, db, openCount, erlist);
        DEC(openCount);
        IF openCount = 0 THEN
          (* this client does not use this database any longer *)
          OGTableRemove(pool.openGraphs, id);
          (* remove all external relations from the ertable and close the
             corresponding files if the neighbour is also not used. *)
          WHILE erlist # NIL DO
            IF NOT IsOpen(pool, erlist^.neighbour) THEN
              (* neither the neighbour graph nor the db we are closing are
                 used by this client.  Therfore, the external relations
                 between the two graphs won't be used, too. *)
              (* An entry in the ERTable only exists, if the ER between
                 graph id and erlist^.neighbour was used. *)
              IF ERTableExists(pool.extRels, id, erlist^.neighbour) THEN
                ERTableGet(
                  pool.extRels, id, erlist^.neighbour, ername, erstorage);
                ERTableRemove(pool.extRels, id, erlist^.neighbour);
                erstorage.close();
              END;
            END;
            erlist := ERListGetNext(erlist);
          END;
          db.close();
        ELSE
          (* the database is still in use *)
          OGTablePut(pool.openGraphs, id, name, db, openCount, erlist);
        END;

      ELSE
        (* the graph is not open to our knowledge; this might be an error,
           but ... *)
      END;
    EXCEPT
    | ExtConnectorStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.CloseDB",
                              "ExtConnectorStorage.InternalError", info));
    | Database.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraphPool.CloseDB",
                                       "Database.InternalError", info));
    END;
  END CloseDB;

PROCEDURE CopyGraph (sourcePool : T;
                     sourceGraph: Pathname.T;
                     sourcelocal: BOOLEAN;
                     targetPool : T;
                     targetGraph: Pathname.T;
                     targetlocal: BOOLEAN     )
  RAISES {InUse, NotExistent, Existent, InTransaction, InternalError,
          Access.Locked} =
  <* FATAL Super.NotInTransaction *>
  PROCEDURE BeginTransaction () RAISES {Super.FatalError} =
    BEGIN
      Super.T.beginTransaction(sourcePool);
      Super.T.beginTransaction(targetPool);
    END BeginTransaction;

  PROCEDURE CommitTransaction () RAISES {Super.FatalError} =
    BEGIN
      TRY
        Super.T.commitTransaction(sourcePool);
        Super.T.commitTransaction(targetPool);
      EXCEPT
        Super.FatalError (info) =>
          AbortTransaction();
          RAISE Super.FatalError(info);
      END;
    END CommitTransaction;

  PROCEDURE AbortTransaction () RAISES {} =
    BEGIN
      TRY
        Super.T.commitTransaction(sourcePool);
      EXCEPT
        Super.FatalError =>      (* ignore *)
      END;
      TRY
        Super.T.commitTransaction(targetPool);
      EXCEPT
        Super.FatalError =>      (* ignore *)
      END;
    END AbortTransaction;

  PROCEDURE ErrClose (file: VirtualRemoteFile.T) =
    BEGIN
      TRY
        file.close();
      EXCEPT
        Super.FatalError =>      (* ignore *)
      END;
    END ErrClose;

  PROCEDURE CopyPoolFile (source     : T;
                          sourceName : Pathname.T;
                          sourcelocal: BOOLEAN;
                          target     : T;
                          targetName : Pathname.T;
                          targetlocal: BOOLEAN     )
    RAISES {InternalError} =

    VAR
      sourceFile, targetFile: VirtualFile.T;
      sourcePage, targetPage: VirtualPage.T;
      size                  : CARDINAL;
      data                  : PageData.T;
    BEGIN
      TRY
        size := Super.T.fileSize(source, sourceName, sourcelocal);
      EXCEPT
        Super.FatalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "PersistentGraphPool.CopyPoolFile",
                                "Super.FatalError", info));
      | PageFile.NoAccess (msg) =>
          RAISE InternalError(
                  ErrorSupport.Create("PersistentGraphPool.CopyPoolFile",
                                      "PageFile.NoAccess:" & msg));
      END;

      TRY
        IF sourcelocal THEN
          sourceFile :=
            NEW(VirtualLocalFile.T).open(
              source, sourceName, FALSE (*new*), FALSE (*composeName*));
        ELSE
          sourceFile := NEW(VirtualRemoteFile.T).open(
                          source, sourceName, source.getAccessMode(),
                          Access.Kind.Log, new := FALSE);
        END;                     (* IF sourcelocal *)
      EXCEPT
        PageFile.NoAccess (msg) =>
          RAISE InternalError(
                  ErrorSupport.Create("PersistentGraphPool.CopyPoolFile",
                                      "PageFile.NoAccess: " & msg));
      | Access.Denied (msg) =>
          RAISE InternalError(
                  ErrorSupport.Create("PersistentGraphPool.CopyPoolFile",
                                      "Access.Denied: " & msg));
      END;

      TRY
        IF targetlocal THEN
          targetFile :=
            NEW(VirtualLocalFile.T).open(
              target, targetName, TRUE (*new*), FALSE (*composeName*));
        ELSE
          targetFile := NEW(VirtualRemoteFile.T).open(
                          target, targetName, target.getAccessMode(),
                          Access.Kind.Log, new := TRUE);
        END;                     (* IF targetlocal *)
      EXCEPT
        PageFile.NoAccess (msg) =>
          ErrClose(sourceFile);
          RAISE InternalError(
                  ErrorSupport.Create("PersistentGraphPool.CopyPoolFile",
                                      "PageFile.NoAccess: " & msg));
      | Access.Denied (msg) =>
          RAISE InternalError(
                  ErrorSupport.Create("PersistentGraphPool.CopyPoolFile",
                                      "Access.Denied: " & msg));
      END;

      TRY
        FOR i := 1 TO size DO
          BeginTransaction();
          sourcePage := sourceFile.getPage(i - 1);
          targetPage := targetFile.getPage(i - 1);
          sourcePage.getAll(data);
          targetPage.putAll(data);
          CommitTransaction();
        END;
        sourceFile.close();
        targetFile.close();
      EXCEPT
        VirtualPage.FatalError (info) =>
          AbortTransaction();
          ErrClose(sourceFile);
          ErrClose(targetFile);
          RAISE InternalError(ErrorSupport.Propagate(
                                "PersistentGraphPool.CopyPoolFile",
                                "VirtualPage.FatalError", info));
      | Access.Locked =>
          AbortTransaction();
          ErrClose(sourceFile);
          ErrClose(targetFile);
          RAISE InternalError(
                  ErrorSupport.Create(
                    "PersistentGraphPool.CopyPoolFile", "Access.Locked"));
      | Super.FatalError (info) =>
          AbortTransaction();
          ErrClose(sourceFile);
          ErrClose(targetFile);
          RAISE InternalError(ErrorSupport.Propagate(
                                "PersistentGraphPool.CopyPoolFile",
                                "Super.FatalError", info));
      END;
    END CopyPoolFile;

  BEGIN
    TRY
      IF Super.T.getTransactionLevel(sourcePool) > Txn.EnvelopeLevel
           OR Super.T.getTransactionLevel(targetPool) > Txn.EnvelopeLevel THEN
        RAISE InTransaction;
      END;
      BeginTransaction();
      IF NOT sourcePool.graphs.existsGraph(sourceGraph, sourcelocal) THEN
        AbortTransaction();
        RAISE NotExistent;
      END;
      IF Super.T.fileInUse(sourcePool, sourceGraph, sourcelocal) THEN
        AbortTransaction();
        RAISE InUse;
      END;
      IF targetPool.graphs.existsGraph(targetGraph, targetlocal) THEN
        AbortTransaction();
        RAISE Existent;
      END;
      CommitTransaction();

      CopyPoolFile(sourcePool, sourceGraph, sourcelocal, targetPool,
                   targetGraph, targetlocal);

      BeginTransaction();
      targetPool.graphs.insertGraph(targetGraph);
      CommitTransaction();


    EXCEPT
      PersistentNames.InternalError (info) =>
        AbortTransaction();
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentGraphPool.CopyGraph",
                              "PersistentNames.InternalError", info));
    | Super.FatalError (info) =>
        AbortTransaction();
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentGraphPool.CopyGraph",
                                       "Super.FatalError", info));
    END;
  END CopyGraph;

PROCEDURE ExtRelName (graph1, graph2: CARDINAL): Pathname.T RAISES {} =
  CONST Prefix = ".external/rel_";
  BEGIN
    IF graph1 < graph2 THEN
      RETURN Prefix & Fmt.Int(graph1) & "_" & Fmt.Int(graph2);
    ELSE
      RETURN Prefix & Fmt.Int(graph2) & "_" & Fmt.Int(graph1);
    END;
  END ExtRelName;

PROCEDURE IsOpen (pool: T; graph: CARDINAL): BOOLEAN =
  BEGIN
    RETURN OGTableExists(pool.openGraphs, graph);
  END IsOpen;



(* ----------------------------------------------------------------- *)

(* LOCAL ADT INTERFACE ERTable *)

(* TYPE ERTable; *)

PROCEDURE ERTableInit (VAR tab: ERTable) =
  BEGIN
    tab := ERTable{NIL, ..};
  END ERTableInit;

PROCEDURE ERTableExists (READONLY tab: ERTable; g1, g2: CARDINAL):
  BOOLEAN =
  VAR
    i    : ERIndex;
    k    : ERKey;
    entry: REF ERTableEntry;
  BEGIN
    k := CreateERKey(g1, g2);
    i := HashERKey(k);
    RETURN SearchERCollList(tab[i], k, entry);
  END ERTableExists;

PROCEDURE ERTablePut (VAR tab           : ERTable;
                          graph1, graph2: CARDINAL;
                          ername        : Pathname.T;
                          storage       : ExtConnectorStorage.T) =
  VAR
    i    : ERIndex;
    k    : ERKey;
    entry: REF ERTableEntry;
  BEGIN
    k := CreateERKey(graph1, graph2);
    i := HashERKey(k);
    IF SearchERCollList(tab[i], k, entry) THEN
      entry^ := ERTableEntry{name := ername, graphs := k, storage :=
                             storage, next := entry^.next};
    ELSE
      entry := NEW(REF ERTableEntry, name := ername, graphs := k,
                   storage := storage, next := NIL);
      InsertInERCollList(tab[i], entry);
    END;
  END ERTablePut;

EXCEPTION ERTableNotFound;

PROCEDURE ERTableGet (READONLY tab           : ERTable;
                               graph1, graph2: CARDINAL;
                      VAR      ername        : Pathname.T;
                      VAR      storage       : ExtConnectorStorage.T)
  RAISES {ERTableNotFound} =
  VAR
    i    : ERIndex;
    k    : ERKey;
    entry: REF ERTableEntry;
  BEGIN
    k := CreateERKey(graph1, graph2);
    i := HashERKey(k);
    IF SearchERCollList(tab[i], k, entry) THEN
      ername := entry^.name;
      storage := entry^.storage;
    ELSE
      RAISE ERTableNotFound;
    END;
  END ERTableGet;

PROCEDURE ERTableRemove (VAR tab: ERTable; graph1, graph2: CARDINAL) =
  VAR
    i    : ERIndex;
    k    : ERKey;
    entry: REF ERTableEntry;
  BEGIN
    k := CreateERKey(graph1, graph2);
    i := HashERKey(k);
    IF SearchERCollList(tab[i], k, entry) THEN
      RemoveFromERCollList(tab[i], entry);
    END;
  END ERTableRemove;

(* END LOCAL ERTable; *)

(* MODULE LOCAL ADT ERTable *)

CONST ERTableSize = 97;

TYPE
  ERKey = ARRAY [0 .. 1] OF CARDINAL;
  ERIndex = [0 .. ERTableSize - 1];

  ERTable = ARRAY ERIndex OF REF ERTableEntry;

  ERTableEntry =
    RECORD
      name: Pathname.T;          (* Name of the external relation
                                    storage *)
      graphs: ERKey;             (* The graphs connected by the ERS *)

      storage: ExtConnectorStorage.T;  (* A handle to the ERS *)
      next   : REF ERTableEntry;       (* Collision list *)
    END;


PROCEDURE CreateERKey (g1, g2: CARDINAL): ERKey =
  BEGIN
    IF g1 < g2 THEN RETURN ERKey{g1, g2}; ELSE RETURN ERKey{g2, g1}; END;
  END CreateERKey;

PROCEDURE HashERKey (key: ERKey): ERIndex =
  BEGIN
    RETURN
      (key[0] MOD ERTableSize + key[1] MOD ERTableSize) MOD ERTableSize;
  END HashERKey;

PROCEDURE SearchERCollList (    list : REF ERTableEntry;
                                key  : ERKey;
                            VAR entry: REF ERTableEntry  ): BOOLEAN =
  BEGIN
    entry := list;
    WHILE entry # NIL DO
      IF entry^.graphs = key THEN RETURN TRUE END;
      entry := entry^.next;
    END;
    RETURN FALSE;
  END SearchERCollList;

PROCEDURE InsertInERCollList (VAR list : REF ERTableEntry;
                                  entry: REF ERTableEntry  ) =
  BEGIN
    entry^.next := list;
    list := entry;
  END InsertInERCollList;

PROCEDURE RemoveFromERCollList (VAR list : REF ERTableEntry;
                                    entry: REF ERTableEntry  ) =
  VAR prev, act: REF ERTableEntry;
  BEGIN
    IF list = NIL THEN RETURN END;
    IF list = entry THEN
      list := list^.next;
      entry^.next := NIL;
    ELSE
      prev := list;
      act := list^.next;
      WHILE act # NIL DO
        IF act = entry THEN
          prev^.next := act^.next;
          entry^.next := NIL;
          act := NIL;
        ELSE
          prev := act;
          act := act^.next;
        END;
      END;
    END;
  END RemoveFromERCollList;

(* END LOCAL ADT ERTable; *)

(* --------------------------------------------------------------- *)

(* LOCAL ADT INTERFACE OGTable *)

(* TYPE OGTable; *)

PROCEDURE OGTableInit (VAR tab: OGTable) =
  BEGIN
    tab := OGTable{NIL, ..};
  END OGTableInit;

PROCEDURE OGTableExists (READONLY tab: OGTable; g: CARDINAL): BOOLEAN =
  VAR
    i    : OGIndex;
    entry: REF OGTableEntry;
  BEGIN
    i := HashOGKey(g);
    RETURN SearchOGCollList(tab[i], g, entry);
  END OGTableExists;

PROCEDURE OGTablePut (VAR tab   : OGTable;
                          graph : CARDINAL;
                          grname: Pathname.T;
                          db    : Database.T;
                          count : CARDINAL;
                          erlist: ExtRelList  ) =
  VAR
    i    : OGIndex;
    entry: REF OGTableEntry;
  BEGIN
    i := HashOGKey(graph);
    IF SearchOGCollList(tab[i], graph, entry) THEN
      entry^ :=
        OGTableEntry{name := grname, graph := graph, db := db, count :=
                     count, list := erlist, next := entry^.next};
    ELSE
      entry := NEW(REF OGTableEntry, name := grname, graph := graph,
                   db := db, count := count, list := erlist, next := NIL);
      InsertInOGCollList(tab[i], entry);
    END;
  END OGTablePut;

EXCEPTION OGTableNotFound;

PROCEDURE OGTableGet (READONLY tab   : OGTable;
                               graph : CARDINAL;
                      VAR      grname: Pathname.T;
                      VAR      db    : Database.T;
                      VAR      count : CARDINAL;
                      VAR      erlist: ExtRelList  )
  RAISES {OGTableNotFound} =
  VAR
    i    : OGIndex;
    entry: REF OGTableEntry;
  BEGIN
    i := HashOGKey(graph);
    IF SearchOGCollList(tab[i], graph, entry) THEN
      grname := entry^.name;
      db := entry^.db;
      count := entry^.count;
      erlist := entry^.list;
    ELSE
      RAISE OGTableNotFound;
    END;
  END OGTableGet;

PROCEDURE OGTableRemove (VAR tab: OGTable; graph: CARDINAL) =
  VAR
    i    : OGIndex;
    entry: REF OGTableEntry;
  BEGIN
    i := HashOGKey(graph);
    IF SearchOGCollList(tab[i], graph, entry) THEN
      RemoveFromOGCollList(tab[i], entry);
    END;
  END OGTableRemove;

(* END LOCAL OGTable; *)

(* MODULE LOCAL ADT OGTable *)

CONST OGTableSize = 37;

TYPE
  OGKey = CARDINAL;
  OGIndex = [0 .. OGTableSize - 1];

  OGTable = ARRAY OGIndex OF REF OGTableEntry;

  OGTableEntry = RECORD
                   name: Pathname.T;  (* Name of the external graph *)
                   graph: OGKey;  (* The graph number (and hash key) *)

                   db   : Database.T;  (* A handle to the graph db *)
                   count: CARDINAL;    (* An open counter *)
                   list: ExtRelList;  (* A list of used external relation
                                         storages *)
                   next: REF OGTableEntry;  (* Collision list *)
                 END;


PROCEDURE HashOGKey (key: OGKey): OGIndex =
  BEGIN
    RETURN key MOD OGTableSize;
  END HashOGKey;

PROCEDURE SearchOGCollList (    list : REF OGTableEntry;
                                key  : OGKey;
                            VAR entry: REF OGTableEntry  ): BOOLEAN =
  BEGIN
    entry := list;
    WHILE entry # NIL DO
      IF entry^.graph = key THEN RETURN TRUE END;
      entry := entry^.next;
    END;
    RETURN FALSE;
  END SearchOGCollList;

PROCEDURE InsertInOGCollList (VAR list : REF OGTableEntry;
                                  entry: REF OGTableEntry  ) =
  BEGIN
    entry^.next := list;
    list := entry;
  END InsertInOGCollList;

PROCEDURE RemoveFromOGCollList (VAR list : REF OGTableEntry;
                                    entry: REF OGTableEntry  ) =
  VAR prev, act: REF OGTableEntry;
  BEGIN
    IF list = NIL THEN RETURN END;
    IF list = entry THEN
      list := list^.next;
      entry^.next := NIL;
    ELSE
      prev := list;
      act := list^.next;
      WHILE act # NIL DO
        IF act = entry THEN
          prev^.next := act^.next;
          entry^.next := NIL;
          act := NIL;
        ELSE
          prev := act;
          act := act^.next;
        END;
      END;
    END;
  END RemoveFromOGCollList;

(* END LOCAL ADT OGTable; *)

(* INTERFACE LOCAL ADT ERList *)
TYPE
  ExtRelList = REF RECORD
                     neighbour: CARDINAL;
                     next     : ExtRelList;
                   END;

PROCEDURE ERListInit (VAR list: ExtRelList) =
  BEGIN
    list := NIL;
  END ERListInit;

PROCEDURE ERListInsert (VAR list: ExtRelList; neighb: CARDINAL) =
  (* insert neighb sorted into list if it is not already there *)
  VAR
    prev, act: ExtRelList;
    found    : BOOLEAN;
  BEGIN
    IF list = NIL THEN
      list := NEW(ExtRelList, neighbour := neighb, next := list);
    ELSE
      prev := list;
      act := list;
      WHILE act # NIL AND NOT found DO
        IF act^.neighbour < neighb THEN
          prev := act;
          act := act^.next;
        ELSIF act^.neighbour = neighb THEN
          (* list already contains neighbour *)
          RETURN;
        ELSIF act^.neighbour > neighb THEN
          (* insert before act *)
          found := TRUE;
        END;
      END;
      IF act = list THEN
        list := NEW(ExtRelList, neighbour := neighb, next := list);
      ELSE
        prev^.next := NEW(ExtRelList, neighbour := neighb, next := act);
      END;
    END;
  END ERListInsert;

PROCEDURE ERListGetNext (list: ExtRelList): ExtRelList =
  BEGIN
    IF list # NIL THEN RETURN list^.next; ELSE RETURN NIL; END;
  END ERListGetNext;

BEGIN
END PersistentGraphPool.
