MODULE PersistentNames;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.6  1998/05/19 10:20:40  roland
    Bugfixes for local graphs and pretty-printing.

    Revision 1.5  1998/03/18 13:39:36  roland
    More slight modifications to local parameters (default values and
    parameter ordering)

    Revision 1.4  1998/03/17 14:14:20  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.3  1997/07/25 15:18:09  renehuel
    Removed bug in procedure removegraph.

    Revision 1.2  1997/07/21 10:43:07  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1  1997/03/26 11:39:38  roland
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

IMPORT Pathname, TextCursorSet, Access;
IMPORT ErrorSupport, VirtualResource;
IMPORT Text, Fmt;
IMPORT Names;

CONST
  (* Collections *)
  GraphCollName  = "Graphs";
  ExtRelCollName = "ExternalRelation";

  (* Relations *)
  ToExtRelName = "GraphToExtRel";

  (* Collection attributes *)
  UniqueNumberName = "GrasUniqueNumber";

REVEAL
  T = Public BRANDED OBJECT
        (* These components hold the ids of collections, attributes, and
           relations *)
        graphs, extrel: CARDINAL;
        toExtRel      : CARDINAL;
        uniqueNumber  : CARDINAL;
      OVERRIDES
        insertGraph         := InsertGraph;
        removeGraph         := RemoveGraph;
        renameGraph         := RenameGraph;
        copy                := Copy;
        getGraphNumber      := GetGraphNumber;
        getGraphName        := GetGraphName;
        insertExtRelation   := InsertExtRelation;
        existsExtRelation   := ExistsExtRelation;
        getExternalRelation := GetExternalRelation;
        getAllNeighbours    := GetAllNeighbours;
        uniqueName          := UniqueName;
        existsGraph         := ExistsGraph;
        getGraphs           := GetGraphs;

        login := Login;
      END;

CONST
  (* Constant attribute values *)
  NullText = "\000\000\000\000";

PROCEDURE TextToInt (t: TEXT): INTEGER =
  VAR
    b: ARRAY [0 .. 3] OF CHAR;
    i: INTEGER                := 0;
  BEGIN
    Text.SetChars(b, Text.Sub(t, 0, 4));
    i := ORD(b[3]);
    FOR k := 2 TO 0 BY -1 DO i := i * 256 + ORD(b[k]) END;
    RETURN i;
  END TextToInt;

PROCEDURE IntToText (x: INTEGER): TEXT =
  VAR b: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    FOR k := 0 TO 2 DO b[k] := VAL(x MOD 256, CHAR); x := x DIV 256; END;
    b[3] := VAL(x, CHAR);
    RETURN Text.FromChars(b);
  END IntToText;


PROCEDURE CreateGraph (names: T; graph: Pathname.T; local: BOOLEAN := FALSE)
  RAISES {Access.Locked, Names.InternalError} =
  BEGIN
    names.insert(graph, local);
    names.insert(graph, local, collection := names.graphs);
  END CreateGraph;

PROCEDURE ExternalRelation (    names : T;
                                g1, g2: Pathname.T;
                                local : BOOLEAN;
                            VAR rel   : Pathname.T  ): BOOLEAN
  RAISES {Unknown, Access.Locked, InternalError} =
  VAR
    er1, er2: TextCursorSet.T;
    found   : BOOLEAN         := FALSE;
  BEGIN
    TRY
      er1 := names.targets(g1, local, names.toExtRel);
      er2 := names.targets(g2, local, names.toExtRel);
      er1.intersection(er2);
      IF er1.card() > 0 THEN rel := er1.extractAnyElement(found); END;
      er1.dispose();
      er2.dispose();
      RETURN found;
    EXCEPT
      Names.Unknown => RAISE Unknown;
    | Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.ExternalRelation",
                                       "Names.InternalError", info));
    END;
  END ExternalRelation;

(* Everything we use here will be declared at Login.  If the application
   does not login, every call to Names will fail anyway. *)
<* FATAL Names.Undeclared *>

PROCEDURE Login (names         : T;
                 resource      : VirtualResource.T;
                 collectionname: Pathname.T         )
  RAISES {Access.Locked, Names.InternalError} =
  VAR test: TEXT;
  <* FATAL Names.Unknown *>
  BEGIN
    Names.T.login(names, resource, collectionname);
    (* Get Ids for collections and relations *)
    names.graphs := names.declareCollection(GraphCollName);
    names.extrel := names.declareCollection(ExtRelCollName);

    names.toExtRel := names.declareRelation(ToExtRelName);

    names.uniqueNumber :=
      names.declareCollectionAttribute(UniqueNumberName);
    test := names.getCollectionAttribute(names.uniqueNumber);
    IF test = NIL THEN
      (* This is a brand new Names instance *)
      names.setCollectionAttribute(names.uniqueNumber, NullText);
    END;
  END Login;


(* --- Building administration information --- *)

PROCEDURE InsertGraph (names: T; name: Pathname.T; local: BOOLEAN := FALSE)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      CreateGraph(names, name, local);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.InsertGraph",
                                       "Names.InternalError", info));
    END;
  END InsertGraph;

PROCEDURE RemoveGraph (    names  : T;
                           name   : Pathname.T;
                           local  : BOOLEAN;
                       VAR extRels: TextCursorSet.T)
  RAISES {Access.Locked, InternalError} =
  VAR
    found: BOOLEAN;
    erel : Pathname.T;
  BEGIN
    TRY
      extRels := names.targets(name, local, names.toExtRel);
      extRels.loop();
      erel := extRels.get(found);
      WHILE found DO
        names.remove(erel, local);
        erel := extRels.get(found);
      END;
      names.remove(name, local);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.RemoveGraph",
                                       "Names.InternalError", info));
    | Names.Unknown =>
        (* graph does not exists, be quiet about that. *)
        extRels := TextCursorSet.New();
    END;
  END RemoveGraph;

PROCEDURE RenameGraph (names: T;
                       local: BOOLEAN      := FALSE;
                       old  : Pathname.T;
                       new  : Pathname.T             )
  RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      names.change(local, old, new);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.RenameGraph",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END RenameGraph;


PROCEDURE UniqueName (names: T): TEXT
  RAISES {Access.Locked, InternalError} =
  <* FATAL Names.Unknown *>
  VAR number: CARDINAL;
  BEGIN
    TRY
      number :=
        TextToInt(names.getCollectionAttribute(names.uniqueNumber));
      INC(number);
      names.setCollectionAttribute(names.uniqueNumber, IntToText(number));
      RETURN Fmt.Int(number);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.MkIndirect",
                                       "Names.InternalError", info));
    END;
  END UniqueName;


PROCEDURE Copy (             names : T;
                <* UNUSED *> source: Pathname.T;
                             target: Pathname.T  )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      CreateGraph(names, target);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "PersistentNames.Copy", "Names.InternalError", info))
    END;
  END Copy;

PROCEDURE GetGraphNumber (names: T; graph: Pathname.T; local: BOOLEAN):
  CARDINAL RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      RETURN names.id(graph, local, collection := names.graphs);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.GetGraphNumber",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetGraphNumber;

PROCEDURE GetGraphName (names: T; number: CARDINAL; local: BOOLEAN):
  Pathname.T RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      RETURN names.name(number, local, collection := names.graphs);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.GetGraphNumber",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetGraphName;

PROCEDURE InsertExtRelation (names   : T;
                             graph1  : Pathname.T;
                             graph2  : Pathname.T;
                             local   : BOOLEAN;
                             relation: Pathname.T  )
  RAISES {Access.Locked, InternalError, Unknown, Existent} =
  VAR ername: TEXT;
  BEGIN
    TRY
      (* check whether graph1 and graph2 are already connected *)
      IF NOT ExternalRelation(names, graph1, graph2, local, ername) THEN
        names.insert(relation, local);
        names.insert(relation, local, collection := names.extrel);
        names.insertInRelation(graph1, relation, local, names.toExtRel);
        names.insertInRelation(graph2, relation, local, names.toExtRel);
      ELSE
        RAISE Existent;
      END;
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.InsertExtRelation",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END InsertExtRelation;

PROCEDURE ExistsExtRelation (names : T;
                             graph1: Pathname.T;
                             graph2: Pathname.T;
                             local : BOOLEAN     ): BOOLEAN
  RAISES {Access.Locked, InternalError, Unknown} =
  VAR dummy: TEXT;
  BEGIN
    RETURN ExternalRelation(names, graph1, graph2, local, dummy);
  END ExistsExtRelation;

PROCEDURE GetExternalRelation (names : T;
                               graph1: Pathname.T;
                               graph2: Pathname.T;
                               local : BOOLEAN     ): Pathname.T
  RAISES {Access.Locked, InternalError, Unknown} =
  VAR name: Pathname.T;
  BEGIN
    IF ExternalRelation(names, graph1, graph2, local, name) THEN
      RETURN name;
    ELSE
      RAISE Unknown;
    END;
  END GetExternalRelation;

PROCEDURE GetAllNeighbours (names: T; graph: Pathname.T; local: BOOLEAN):
  TextCursorSet.T RAISES {Access.Locked, InternalError, Unknown} =
  VAR
    er, neighs, res: TextCursorSet.T;
    extrel         : Pathname.T;
    found          : BOOLEAN;
  BEGIN
    TRY
      res := TextCursorSet.New();
      er := names.targets(graph, local, names.toExtRel);
      er.loop();
      extrel := er.get(found);
      WHILE found DO
        neighs := names.sources(extrel, local, names.toExtRel);
        IF neighs.card() # 2 THEN
          RAISE InternalError(
                  ErrorSupport.Create("PersistentNames.GetAllNeighbours",
                                      "Corrupt names storage."));
        END;
        (* Store graph and its neighbour.  We will remove graph from the
           set later. *)
        res.union(neighs);
        extrel := er.get(found);
      END;
      er.dispose();
      res.deleteElement(graph, found);
      RETURN res;
    EXCEPT
      Names.Unknown => RAISE Unknown;
    | Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.GetAllNeighbours",
                                       "Names.InternalError", info));
    END
  END GetAllNeighbours;

(* --- Queries --- *)

PROCEDURE ExistsGraph (names: T; name: Pathname.T; local: BOOLEAN): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN names.contained(name, local, collection := names.graphs);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("PersistentNames.ExistsGraph",
                                       "Names.InternalError", info));
    END;
  END ExistsGraph;


PROCEDURE GetGraphs (names: T; local: BOOLEAN): TextCursorSet.T
  RAISES {Access.Locked, InternalError} =
  VAR all: TextCursorSet.T;
  BEGIN
    TRY
      all := names.getAllEntries(local, collection := names.graphs);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "PersistentNames.GetGraphs",
                              "Names.InternalError", info));
    END;
    RETURN all;
  END GetGraphs;


BEGIN
END PersistentNames.
