GENERIC MODULE Relation(
Elem1, Elem2, Elem1Set, Elem2Set, RelationElement,
RelationElementSet, InternRelationElementSet);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.5  1998/01/21 14:15:22  roland
    Debugging code added (in comments).

    Revision 1.4  1997/09/18 14:33:59  roland
    Cartesian product does not return a relation but inserts the cartesian
    product of to sets in a relation.

    Revision 1.3  1997/08/15 11:20:18  roland
    Copying of relations now works by using an internal interface of CursorSet.

    Revision 1.2  1997/07/21 10:29:35  roland
    New implementation of sets using red-black trees.

    Revision 1.1  1997/03/21 16:42:52  roland
    New generic module Relation realizes binary relations between objects
    of two (distinct) abstract data types.

*)
(***************************************************************************)

IMPORT Journal, Variant, Fmt;

REVEAL
  T = Public BRANDED OBJECT
        nextFree: T;
      OVERRIDES
        copy                   := Copy;
        fromElem1Set           := FromElem1Set;
        fromElem2Set           := FromElem2Set;
        cartesian              := Cartesian;
        projection1            := Projection1;
        projection2            := Projection2;
        queryProjection1       := QueryProjection1;
        queryProjection2       := QueryProjection2;
        singleQueryProjection1 := SingleQueryProjection1;
        singleQueryProjection2 := SingleQueryProjection2;
        get                    := Get;
        extractAnyElement      := ExtractAnyElement;
        deleteElement          := DeleteElement;
        gotoElement            := GotoElement;
        insert                 := Insert;
        in                     := In;
        dispose                := DisposeRel;
      END;


VAR
  FreeRels                : T        := NIL;
  MaxFreeRels, FreeRelSize: CARDINAL := 0;

PROCEDURE New (): T =
  VAR rel: T;
  BEGIN
    IF FreeRels # NIL THEN
      rel := FreeRels;
      FreeRels := FreeRels.nextFree;
      DEC(FreeRelSize);
    ELSE
      rel := NEW(T);
    END;
    RETURN rel.init();
  END New;

PROCEDURE Copy(rel: T): RelationElementSet.T RAISES {} =
  VAR target: T;
  BEGIN
    target := New();
    InternRelationElementSet.InternCopySet(rel, target);
    RETURN target;
  END Copy;

  
PROCEDURE DisposeRel (rel: T; insertInFreeList: BOOLEAN := TRUE) =
(**
  PROCEDURE AssertNotInList() =
    VAR h: T;
    BEGIN
      h := FreeRels;
      WHILE h # NIL AND h # rel DO h := h.nextFree; END;
      IF h = rel THEN <* ASSERT FALSE *> END;
    END AssertNotInList;
*)
  BEGIN
    (* AssertNotInList();*)
    RelationElementSet.T.dispose(rel, insertInFreeList := FALSE);
    IF insertInFreeList THEN
      rel.nextFree := FreeRels;
      FreeRels := rel;
      INC(FreeRelSize);
      IF Variant.FreeMemoryListLog > 0 THEN
        IF MaxFreeRels < FreeRelSize THEN
          MaxFreeRels := FreeRelSize;
          IF MaxFreeRels MOD Variant.FreeMemoryListLog = 0 THEN
            Journal.Add(RelationElement.Brand & "Relation free memory list "
                        & Fmt.Int(MaxFreeRels));
          END;
        END;
      END;
    END;
  END DisposeRel;


PROCEDURE FromElem1Set (relation: T;
                        source  : Elem1Set.T;
                        comp2   : Elem2.T      := Elem2.Null): T
  RAISES {} =
  VAR
    found: BOOLEAN;
    n    : Elem1.T;
  BEGIN
    source.loop();
    n := source.get(found);
    WHILE found DO
      RelationElementSet.T.insert(relation, RelationElement.T{n, comp2});
      n := source.get(found);
    END;
    RETURN relation;
  END FromElem1Set;

PROCEDURE FromElem2Set (relation: T;
                        source  : Elem2Set.T;
                        comp1   : Elem1.T      := Elem1.Null): T
  RAISES {} =
  VAR
    found: BOOLEAN;
    n    : Elem2.T;
  BEGIN
    source.loop();
    n := source.get(found);
    WHILE found DO
      RelationElementSet.T.insert(relation, RelationElement.T{comp1, n});
      n := source.get(found);
    END;
    RETURN relation;
  END FromElem2Set;

PROCEDURE Cartesian (relation: T; comp1: Elem1Set.T; comp2: Elem2Set.T)
  RAISES {} =
  VAR
    element1      : Elem1.T;
    element2      : Elem2.T;
    found1, found2: BOOLEAN;
  BEGIN
    comp1.loop();
    REPEAT
      element1 := comp1.get(found1);

      (* search matching tuples *)
      IF (found1) THEN
        comp2.loop();
        REPEAT
          element2 := comp2.get(found2);
          IF (found2) THEN
            RelationElementSet.T.insert(
              relation, RelationElement.T{element1, element2});
          END;
        UNTIL NOT found2;
      END;
    UNTIL NOT found1;
  END Cartesian;


PROCEDURE Projection1 (relation: T): Elem1Set.T RAISES {} =
  VAR
    element: RelationElement.T;
    found  : BOOLEAN;
    result : Elem1Set.T;

  BEGIN
    result := NEW(Elem1Set.T).init();
    relation.loop();
    REPEAT
      element := RelationElementSet.T.get(relation, found);

      IF (found) THEN result.insert(element.m); END;
    UNTIL NOT found;
    RETURN result;
  END Projection1;


PROCEDURE Projection2 (relation: T): Elem2Set.T RAISES {} =
  VAR
    element: RelationElement.T;
    found  : BOOLEAN;
    result : Elem2Set.T;
  BEGIN
    result := NEW(Elem2Set.T).init();
    relation.loop();
    REPEAT
      element := RelationElementSet.T.get(relation, found);

      IF (found) THEN result.insert(element.n); END;
    UNTIL NOT found;
    RETURN result;
  END Projection2;


PROCEDURE QueryProjection1 (relation: T; comp2: Elem2.T): Elem1Set.T
  RAISES {} =
  VAR
    element: RelationElement.T;
    found  : BOOLEAN;
    result : Elem1Set.T;
  BEGIN
    result := NEW(Elem1Set.T).init();
    relation.loop();
    REPEAT
      element := RelationElementSet.T.get(relation, found);
      IF found AND Elem2.Compare(comp2, element.n) = 0 THEN
        result.insert(element.m);
      END;
    UNTIL NOT found;
    RETURN result;
  END QueryProjection1;


PROCEDURE QueryProjection2 (relation: T; comp1: Elem1.T): Elem2Set.T
  RAISES {} =
  VAR
    element: RelationElement.T;
    found  : BOOLEAN;
    result : Elem2Set.T;
  BEGIN
    result := NEW(Elem2Set.T).init();
    relation.loop();
    REPEAT
      element := RelationElementSet.T.get(relation, found);
      IF found AND Elem1.Compare(comp1, element.m) = 0 THEN
        result.insert(element.n);
      END;
    UNTIL NOT found;
    RETURN result;
  END QueryProjection2;


PROCEDURE SingleQueryProjection1 (    relation: T;
                                      comp2   : Elem2.T;
                                  VAR found   : BOOLEAN  ): Elem1.T
  RAISES {} =
  VAR element: RelationElement.T;
  BEGIN
    relation.loop();
    REPEAT
      element := RelationElementSet.T.get(relation, found);
      IF found AND Elem2.Compare(comp2, element.n) = 0 THEN
        RETURN element.m
      END;
    UNTIL NOT found;
    RETURN Elem1.Null;
  END SingleQueryProjection1;


PROCEDURE SingleQueryProjection2 (    relation: T;
                                      comp1   : Elem1.T;
                                  VAR found   : BOOLEAN  ): Elem2.T
  RAISES {} =
  VAR element: RelationElement.T;
  BEGIN
    relation.loop();
    REPEAT
      element := RelationElementSet.T.get(relation, found);
      IF found AND Elem1.Compare(comp1, element.m) = 0 THEN
        RETURN element.n
      END;
    UNTIL NOT found;
    RETURN Elem2.Null;
  END SingleQueryProjection2;


PROCEDURE Get (    relation: T;
               VAR comp1   : Elem1.T;
               VAR comp2   : Elem2.T;
               VAR found   : BOOLEAN  ) RAISES {} =
  VAR element: RelationElement.T;

  BEGIN
    element := RelationElementSet.T.get(relation, found);
    IF (found) THEN comp1 := element.m; comp2 := element.n; END;
  END Get;


PROCEDURE ExtractAnyElement (    relation: T;
                             VAR comp1   : Elem1.T;
                             VAR comp2   : Elem2.T;
                             VAR found   : BOOLEAN  ) RAISES {} =
  VAR element: RelationElement.T;

  BEGIN
    element := RelationElementSet.T.extractAnyElement(relation, found);
    IF (found) THEN comp1 := element.m; comp2 := element.n; END;
  END ExtractAnyElement;


PROCEDURE DeleteElement (relation: T; comp1: Elem1.T; comp2: Elem2.T)
  RAISES {} =
  VAR found: BOOLEAN;

  BEGIN
    RelationElementSet.T.deleteElement(
      relation, RelationElement.T{comp1, comp2}, found);
  END DeleteElement;


PROCEDURE GotoElement (    relation: T;
                           comp1   : Elem1.T;
                           comp2   : Elem2.T;
                       VAR found   : BOOLEAN  ) RAISES {} =
  BEGIN
    RelationElementSet.T.gotoElement(
      relation, RelationElement.T{comp1, comp2}, found);
  END GotoElement;


PROCEDURE Insert (relation: T; comp1: Elem1.T; comp2: Elem2.T) RAISES {} =
  BEGIN
    RelationElementSet.T.insert(relation, RelationElement.T{comp1, comp2});
  END Insert;


PROCEDURE In (relation: T; comp1: Elem1.T; comp2: Elem2.T): BOOLEAN
  RAISES {} =
  BEGIN
    RETURN
      RelationElementSet.T.in(relation, RelationElement.T{comp1, comp2});
  END In;

BEGIN
END Relation.
