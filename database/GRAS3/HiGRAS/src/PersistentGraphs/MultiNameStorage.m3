MODULE MultiNameStorage;

(***************************************************************************)
(* The first time a name is used, it is stored in NameStorage and a tuple
   (entity, key1, key2) is stored in RelationStorage (to find all names for
   one entity).  If a name is stored more than once, RelationStorage is
   used to store (entity, key1, key2) and (key1, key2, entity) triples,
   where key1 and key2 are the unique keys for (name, tag) as returned by
   NameStorage. *)
(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.3  1998/01/21 12:40:33  roland
    Prettyprinting.

    Revision 1.2  1997/07/21 10:42:58  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1  1997/03/26 11:39:18  roland
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

    Revision 1.5  1996/11/20 12:23:01  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.4  1996/09/17 13:09:07  roland
    Adapted to new (names of) generic instances.
    Explicit call to Super.T.beginTransaction etc. to avoid conflicts with higher
    layers.

    Revision 1.3  1996/08/06 16:26:32  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.2  1996/07/24 09:20:06  rbnix
        Error handling adjusted: internal errors are now guarded by
        assertions rather than exceptions. This should simplify
        locating errors.

    Revision 1.2.2.1  1996/04/29 13:43:44  roland
    Changes for Page-Server. A graph is VirtualResource. ExceptionHandling
    improved.

# Revision 1.2  1995/06/08  14:50:36  grover
# removed some bugs about index - operations
#
# Revision 1.1  1994/01/20  18:41:31  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Access, ITPFile, NameStorage, RelationStorage,
       TaggedName, CardSet, TaggedNameSet, CardRelation;
IMPORT ErrorSupport, ConnectorStorage;


CONST
  IndexTree    = ConnectorStorage.IndexTree;
  EntityToKeys = 2;
  KeysToEntity = 4;


PROCEDURE PutName (file  : ITPFile.T;
                   entity: CARDINAL;
                   tag   : CARDINAL;
                   name  : TEXT       )
  RAISES {Access.Locked, TagUsed, InternalError} =
  VAR
    range      : RelationStorage.RelationRange;
    found      : BOOLEAN;
    key1, key2 : CARDINAL;
    otherEntity: CARDINAL;
    done       : BOOLEAN;
  BEGIN
    TRY
      (* check RelationStorage to see whether the tag is already in use *)
      range := RelationStorage.InitRelationRange(
                 file, IndexTree, EntityToKeys, entity);

      RelationStorage.GetFromRelationRange(
        file, IndexTree, range, found, key1, key2);
      WHILE (found) DO
        (* TagUsed: the tag is already used for this entity *)
        IF NameStorage.GetTagFromKeys(file, key1, key2) = tag THEN
          RAISE TagUsed
        END;

        RelationStorage.GetFromRelationRange(
          file, IndexTree, range, found, key1, key2);
      END;
    EXCEPT
      NameStorage.NameNotFound =>
        RAISE
          InternalError(ErrorSupport.Create("MultiNameStorage.PutName",
                                            "NameStorage.NameNotFound"));
    | NameStorage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("MultiNameStorage.PutName",
                                       "NameStorage.InternalError", info));
    | RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.PutName",
                              "RelationStorage.InternalError", info));
    END;

    (* try to store name in NameStorage *)
    TRY
      NameStorage.PutName(
        file, tag, name, entity, otherEntity, key1, key2, done);
    EXCEPT
      NameStorage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("MultiNameStorage.PutName",
                                       "NameStorage.InternalError", info));
    END;
    TRY
      RelationStorage.PutRelation(
        file, IndexTree, EntityToKeys, entity, key1, key2);
      IF (done) THEN RETURN; END;

      (* name already in use: store other relation *)
      RelationStorage.PutRelation(
        file, IndexTree, KeysToEntity, key1, key2, entity);

      (* if there has been only one entity for the name up to now, the
         corresponding relation for this entity has to be stored *)
      IF (otherEntity # 0) THEN
        RelationStorage.PutRelation(
          file, IndexTree, KeysToEntity, key1, key2, otherEntity);
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.PutName",
                              "RelationStorage.InternalError", info));
    END;
  END PutName;


PROCEDURE DeleteName (file  : ITPFile.T;
                      entity: CARDINAL;
                      tag   : CARDINAL;
                      name  : TEXT       )
  RAISES {Access.Locked, InternalError, NameNotFound} =
  VAR
    entityFound: CARDINAL;
    key1, key2 : CARDINAL;
    multiple   : BOOLEAN;
  BEGIN
    TRY
      (* try to delete from NameStorage *)
      NameStorage.DeleteName(
        file, tag, name, entityFound, key1, key2, multiple);
      RelationStorage.DeleteRelation(
        file, IndexTree, EntityToKeys, entity, key1, key2);
      IF (multiple) THEN UpdateRelations(file, entity, key1, key2); END;
    EXCEPT
      NameStorage.NameNotFound => RAISE NameNotFound;
    | NameStorage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("MultiNameStorage.DeleteName",
                                       "NameStorage.InternalError", info));
    | RelationStorage.RelationNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("MultiNameStorage.DeleteName",
                                    "RelationStorage.RelationNotFound"));
    | RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.DeleteName",
                              "RelationStorage.InternalError", info));
    END;
  END DeleteName;


PROCEDURE DeleteAllNamesForEntity (    file   : ITPFile.T;
                                       entity : CARDINAL;
                                   VAR deleted: TaggedNameSet.T)
  RAISES {Access.Locked, InternalError} =
  VAR
    deletedKeys: CardRelation.T;
    key1, key2 : CARDINAL;
    found      : BOOLEAN;
    multiple   : BOOLEAN;
    otherEntity: CARDINAL;
    name       : TEXT;
    tag        : CARDINAL;

  BEGIN
    TRY
      IF (NOT RelationStorage.IsARelation(
                file, IndexTree, 2, 75, 13827081, 1)) THEN
        key1 := 0;
      END;
      deleted := TaggedNameSet.New();

      (* delete all relations from entity to names and determine deleted
         keys *)
      RelationStorage.DeleteRelationsByEntity(
        file, IndexTree, EntityToKeys, entity, deletedKeys);

      (* delete names and relations from keys to entities *)
      deletedKeys.loop();
      deletedKeys.get(key1, key2, found);
      WHILE (found) DO
        (* store name and tag to be deleted *)
        NameStorage.GetNameAndTagFromKeys(file, key1, key2, name, tag);
        WITH deletedName = TaggedName.T{tag, name} DO
          deleted.insert(deletedName);
        END;

        (* now delete *)
        NameStorage.DeleteNameByKeys(
          file, key1, key2, otherEntity, multiple);
        IF (multiple) THEN UpdateRelations(file, entity, key1, key2); END;
        deletedKeys.get(key1, key2, found);
      END;
      deletedKeys.dispose();
    EXCEPT
      NameStorage.NameNotFound =>
        RAISE InternalError(ErrorSupport.Create(
                              "MultiNameStorage.DeleteAllNamesForEntity",
                              "NameStorage.NameNotFound"));
    | RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.DeleteAllNamesForEntity",
                              "RelationStorage.InternalError", info));
    | NameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.DeleteAllNamesForEntity",
                              "NameStorage.InternalError", info));
    END;
  END DeleteAllNamesForEntity;


PROCEDURE GetName (    file  : ITPFile.T;
                       entity: CARDINAL;
                       tag   : CARDINAL;
                   VAR found : BOOLEAN    ): TEXT
  RAISES {Access.Locked, InternalError} =
  VAR
    range     : RelationStorage.RelationRange;
    keyFound  : BOOLEAN;
    key1, key2: CARDINAL;
  BEGIN
    found := TRUE;
    TRY
      range := RelationStorage.InitRelationRange(
                 file, IndexTree, EntityToKeys, entity);

      RelationStorage.GetFromRelationRange(
        file, IndexTree, range, keyFound, key1, key2);
      WHILE (keyFound) DO
        TRY
          WITH foundTag = NameStorage.GetTagFromKeys(file, key1, key2) DO
            IF (foundTag = tag) THEN
              RETURN NameStorage.GetNameFromKeys(file, key1, key2);
            END;
          END;
        EXCEPT
          NameStorage.NameNotFound => (* ignore *)
        END;
        RelationStorage.GetFromRelationRange(
          file, IndexTree, range, keyFound, key1, key2);
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.GetName",
                              "RelationStorage.InternalError", info));
    | NameStorage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("MultiNameStorage.GetName",
                                       "NameStorage.InternalError", info));
    END;

    found := FALSE;
    RETURN "";
  END GetName;


PROCEDURE FindEntitiesForName (file: ITPFile.T; tag: CARDINAL; name: TEXT):
  CardSet.T RAISES {Access.Locked, InternalError} =
  VAR
    entity    : CARDINAL;
    key1, key2: CARDINAL;
    found     : BOOLEAN;
    multiple  : BOOLEAN;
    result    : CardSet.T;
  BEGIN
    TRY
      (* search name in NameStorage *)
      NameStorage.FindName(
        file, tag, name, entity, key1, key2, found, multiple);

      IF (found AND multiple) THEN
        (* more than one entity: return set from RelationStorage *)
        RETURN RelationStorage.GetThirdComponents(
                 file, IndexTree, KeysToEntity, key1, key2);
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.FindEntitiesForName",
                              "RelationStorage.InternalError", info));
    | NameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.FindEntitiesForName",
                              "NameStorage.InternalError", info));
    END;

    result := CardSet.New();
    IF (found) THEN result.insert(entity); END;

    RETURN result;
  END FindEntitiesForName;


PROCEDURE FindTagsForEntity (file: ITPFile.T; entity: CARDINAL): CardSet.T
  RAISES {Access.Locked, InternalError} =
  VAR
    range     : RelationStorage.RelationRange;
    key1, key2: CARDINAL;
    found     : BOOLEAN;
    result    : CardSet.T;
    tag       : CARDINAL;
  BEGIN
    result := CardSet.New();
    TRY
      range := RelationStorage.InitRelationRange(
                 file, IndexTree, EntityToKeys, entity);

      RelationStorage.GetFromRelationRange(
        file, IndexTree, range, found, key1, key2);
      WHILE (found) DO
        TRY
          tag := NameStorage.GetTagFromKeys(file, key1, key2);
          result.insert(tag);
        EXCEPT
          NameStorage.NameNotFound => (* ignore *)
        END;
        RelationStorage.GetFromRelationRange(
          file, IndexTree, range, found, key1, key2);
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.FindTagsForEntity",
                              "RelationStorage.InternalError", info));
    | NameStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.FindTagsForEntity",
                              "NameStorage.InternalError", info));
    END;

    RETURN result;
  END FindTagsForEntity;


(* Delete relations if a name was stored more than once. *)
PROCEDURE UpdateRelations (file      : ITPFile.T;
                           entity    : CARDINAL;
                           key1, key2: CARDINAL   )
  RAISES {Access.Locked, InternalError} =
  VAR found: BOOLEAN;
  BEGIN
    TRY
      RelationStorage.DeleteRelation(
        file, IndexTree, KeysToEntity, key1, key2, entity);
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.UpdateRelations",
                              "RelationStorage.InternalError", info));
    | RelationStorage.RelationNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("MultiNameStorage.UpdateRelations",
                                    "RelationStorage.RelationNotFound"));
    END;

    (* how many names remain? *)
    TRY
      WITH noOfNames = RelationStorage.NoOfThirdComponents(
                         file, IndexTree, KeysToEntity, key1, key2) DO
        IF (noOfNames > 1) THEN RETURN; END;
      END;
    EXCEPT
      RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.UpdateRelations",
                              "RelationStorage.InternalError", info));
    END;

    (* only one name remaning: store in NameStorage *)
    TRY
      WITH remainingEntity = RelationStorage.GetFirstThirdComponent(
                               file, IndexTree, KeysToEntity, key1, key2,
                               found) DO
        IF NOT (found) THEN
          RAISE InternalError(
                  ErrorSupport.Create(
                    "MultiNameStorage.UpdateRelations", "Not found"))
        END;
        NameStorage.PutEntity(file, key1, key2, remainingEntity);
        RelationStorage.DeleteRelation(
          file, IndexTree, KeysToEntity, key1, key2, remainingEntity);
      END;
    EXCEPT
      NameStorage.NameNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("MultiNameStorage.UpdateRelations",
                                    "NameStorage.NameNotFound"));
    | RelationStorage.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "MultiNameStorage.UpdateRelations",
                              "RelationStorage.InternalError", info));
    | NameStorage.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("MultiNameStorage.UpdateRelations",
                                       "NameStorage.InternalError", info));
    | RelationStorage.RelationNotFound =>
        RAISE InternalError(
                ErrorSupport.Create("MultiNameStorage.UpdateRelations",
                                    "RelationStorage.RelationNotFound"));
    END;
  END UpdateRelations;

BEGIN
END MultiNameStorage.
