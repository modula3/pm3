GENERIC MODULE NTree(Element);

(***************************************************************************)
(** Created by:  Reiner Gombert                                            *)


(***************************************************************************)


TYPE
  NodePtr = REF Node;            (* pointer to a NTree element *)

  Node = RECORD                  (* a NTree element: *)
           data          : Element.T;
           leftMostChild : NodePtr;
           rightMostChild: NodePtr;
           actSon        : NodePtr;
           rightSibling  : NodePtr;
           leftSibling   : NodePtr;
           father        : NodePtr;
           noOfSons      : CARDINAL;
           actSonNo      : CARDINAL;
         END;


REVEAL
  T = Public BRANDED OBJECT
        root    : NodePtr := NIL;
        current : NodePtr := NIL;
        iterator: NodePtr := NIL;
      OVERRIDES
        init                := NTreeInit;
        isEmpty             := NTreeIsEmpty;
        noOfSons            := NTreeNoOfSons;
        sonNo               := NTreeSonNo;
        actSonNo            := NTreeActSonNo;
        depth               := NTreeDepth;
        heightActSons       := NTreeHeightActSons;
        goFather            := NTreeGoFather;
        goRoot              := NTreeGoRoot;
        goIthSon            := NTreeGoIthSon;
        goNextSon           := NTreeGoNextSon;
        goPrevSon           := NTreeGoPrevSon;
        goActSon            := NTreeGoActSon;
        getCurrent          := NTreeGetCurrent;
        loop                := NTreeLoop;
        loopFromCurrent     := NTreeLoopCurrent;
        forward             := NTreeForward;
        backward            := NTreeBackward;
        goIterator          := NTreeGoIterator;
        getIteratorValue    := NTreeGetIteratorValue;
        createRoot          := NTreeCreateRoot;
        addSon              := NTreeAddSon;
        removeRootToCurrent := NTreeRemoveRootToCurrent;
        deleteCurrent       := NTreeDeleteCurrent;
        insertSubtree       := NTreeInsertSubtree;
        setCurrent          := NTreeSetCurrent;
        atRoot              := NTreeAtRoot;
      END;

PROCEDURE NTreeInit (tree: T): T =
  BEGIN
    RETURN tree;
  END NTreeInit;

(* --- Structural operations --- *)

PROCEDURE NTreeCreateRoot (tree: T; READONLY data: Element.T) =
  BEGIN
    IF tree.root = NIL THEN
      tree.root :=
        NEW(NodePtr, data := data, father := NIL, leftMostChild := NIL,
            rightMostChild := NIL, rightSibling := NIL, leftSibling := NIL,
            actSon := NIL, actSonNo := 0, noOfSons := 0);
    END;
    tree.current := tree.root;
  END NTreeCreateRoot;

PROCEDURE NTreeAddSon (tree: T; READONLY data: Element.T) RAISES {Empty} =
  VAR help: NodePtr;
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;

    (* set values of new node *)
    help := NEW(NodePtr, data := data, father := tree.current,
                leftMostChild := NIL, rightMostChild := NIL, actSon := NIL,
                leftSibling := NIL, rightSibling := NIL, noOfSons := 0,
                actSonNo := 0);

    INC(tree.current.noOfSons);
    tree.current.actSonNo := tree.current.noOfSons;
    tree.current.actSon := help;
    IF tree.current^.leftMostChild = NIL THEN
      (* Insert as first element of children list *)
      tree.current^.leftMostChild := help;
      tree.current^.rightMostChild := help;
    ELSE
      (* Insert at end of children list *)
      help^.leftSibling := tree.current.rightMostChild;
      tree.current.rightMostChild^.rightSibling := help;
      tree.current.rightMostChild := help;
    END;

    (* new node becomes current *)
    tree.current := help;
  END NTreeAddSon;

PROCEDURE NTreeRemoveRootToCurrent (tree: T) RAISES {Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;

    (* current becomes new root *)
    tree.root := tree.current;

    (* cut connections from and to the father *)
    IF (tree.current^.father # NIL) THEN
      tree.current^.father^.actSon := NIL;
      tree.current^.father^.leftMostChild := NIL;
      tree.current^.father^.rightMostChild := NIL;
    END;
    tree.current^.father := NIL;

    (* cut connections from and to the brothers *)
    IF (tree.current^.rightSibling # NIL) THEN
      tree.current^.rightSibling^.leftSibling := NIL;
    END;
    IF (tree.current^.leftSibling # NIL) THEN
      tree.current^.leftSibling^.rightSibling := NIL;
    END;
    tree.current^.rightSibling := NIL;
    tree.current^.leftSibling := NIL;
  END NTreeRemoveRootToCurrent;

PROCEDURE NTreeDeleteCurrent (tree: T) RAISES {Empty} =
  VAR fp: NodePtr;
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;

    (* Delete the following *)
    tree.current^.leftMostChild := NIL;
    tree.current^.rightMostChild := NIL;
    tree.current^.actSon := NIL;

    WITH actual = tree.current,
         father = tree.current^.father DO

      IF actual^.leftSibling # NIL AND actual^.rightSibling # NIL THEN
        (* actual node has both siblings -> it is not root, and it is not
           start or end of children list *)
        father^.actSon := actual^.leftSibling;
        DEC(father^.actSonNo);
        actual^.leftSibling^.rightSibling := actual^.rightSibling;
        actual^.rightSibling^.leftSibling := actual^.leftSibling;
      ELSIF actual^.leftSibling = NIL AND actual^.rightSibling # NIL THEN
        (* actual node is the first child of its father *)
        father^.actSon := actual^.rightSibling;
        INC(father^.actSonNo);
        father^.leftMostChild := actual.rightSibling;
        actual^.rightSibling^.leftSibling := NIL;
      ELSIF actual^.leftSibling # NIL AND actual^.rightSibling = NIL THEN
        (* actual node is the last child of its father *)
        father^.actSon := actual^.leftSibling;
        DEC(father^.actSonNo);
        father^.rightMostChild := actual^.leftSibling;
        actual^.leftSibling^.rightSibling := actual^.rightSibling;
      ELSE
        (* actual node was the only child of the father, if not root *)
        IF father # NIL THEN
          father^.leftMostChild := NIL;
          father^.rightMostChild := NIL;
          father^.actSonNo := 0;
        END;
      END;

      DEC(father^.noOfSons);
      (* delete the values of current *)
      actual^.rightSibling := NIL;
      actual^.leftSibling := NIL;
      actual.noOfSons := 0;
      fp := father;
      father := NIL;

      (* go to the father *)
      tree.current := fp;
    END;
  END NTreeDeleteCurrent;


PROCEDURE NTreeIsEmpty (tree: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN tree.root = NIL;
  END NTreeIsEmpty;

PROCEDURE NTreeAtRoot (tree: T): BOOLEAN RAISES {Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    RETURN tree.current = tree.root;
  END NTreeAtRoot;

PROCEDURE NTreeNoOfSons (tree: T): CARDINAL RAISES {Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    RETURN tree.current.noOfSons;
  END NTreeNoOfSons;

PROCEDURE NTreeSonNo (tree: T): CARDINAL RAISES {Empty, NotExist} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.current^.father = NIL THEN RAISE NotExist; END;
    RETURN tree.current^.father^.actSonNo;
  END NTreeSonNo;

PROCEDURE NTreeActSonNo (tree: T): CARDINAL RAISES {Empty, NotExist} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.current^.actSon = NIL THEN RAISE NotExist; END;
    RETURN tree.current^.actSonNo;
  END NTreeActSonNo;

PROCEDURE NTreeDepth (tree: T): CARDINAL RAISES {Empty} =
  VAR
    ancestors: CARDINAL;
    ap       : NodePtr;
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    ancestors := 0;
    ap := tree.current^.father;
    WHILE ap # NIL DO ap := ap^.father; INC(ancestors); END;
    RETURN ancestors;
  END NTreeDepth;

PROCEDURE NTreeHeightActSons (tree: T): CARDINAL RAISES {Empty} =
  VAR
    successors: CARDINAL;
    sp        : NodePtr;
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    successors := 0;
    sp := tree.current^.actSon;
    WHILE sp # NIL DO sp := sp^.actSon; INC(successors); END;
    RETURN successors;
  END NTreeHeightActSons;

PROCEDURE NTreeGoFather (tree: T) RAISES {Empty, NotExist} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.current^.father = NIL THEN
      RAISE NotExist;
    ELSE
      tree.current := tree.current^.father;
    END;
  END NTreeGoFather;

PROCEDURE NTreeGoActSon (tree: T) RAISES {Empty, NotExist} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.current^.actSon = NIL THEN
      RAISE NotExist;
    ELSE
      tree.current := tree.current^.actSon;
    END;
  END NTreeGoActSon;

PROCEDURE NTreeGoRoot (tree: T) RAISES {Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    tree.current := tree.root;
  END NTreeGoRoot;

PROCEDURE NTreeGoIthSon (tree: T; READONLY i: CARDINAL)
  RAISES {NotExist, Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.current.noOfSons < i OR i = 0 THEN RAISE NotExist; END;
    tree.current := tree.current^.leftMostChild;
    FOR j := 1 TO i - 1 DO
      tree.current := tree.current^.rightSibling;
    END;
    tree.current^.father^.actSon := tree.current;
    tree.current^.father^.actSonNo := i;
  END NTreeGoIthSon;

PROCEDURE NTreeGoNextSon (tree: T) RAISES {NotExist, Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.current^.actSon = NIL THEN RAISE NotExist END;
    IF tree.current^.actSon^.rightSibling = NIL THEN RAISE NotExist END;
    tree.current^.actSon := tree.current^.actSon^.rightSibling;
    INC(tree.current^.actSonNo);
    tree.current := tree.current^.actSon;
  END NTreeGoNextSon;

PROCEDURE NTreeGoPrevSon (tree: T) RAISES {NotExist, Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.current^.actSon = NIL THEN RAISE NotExist END;
    IF tree.current^.actSon^.leftSibling = NIL THEN RAISE NotExist END;
    tree.current^.actSon := tree.current^.actSon^.leftSibling;
    DEC(tree.current^.actSonNo);
    tree.current := tree.current^.actSon;
  END NTreeGoPrevSon;

PROCEDURE NTreeGetCurrent (tree: T; VAR data: Element.T) RAISES {Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    data := tree.current^.data;
  END NTreeGetCurrent;

PROCEDURE NTreeLoop (tree: T) RAISES {Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    tree.iterator := tree.root;
  END NTreeLoop;

PROCEDURE NTreeLoopCurrent (tree: T) RAISES {Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    tree.iterator := tree.current;
  END NTreeLoopCurrent;

PROCEDURE NTreeForward (tree: T): BOOLEAN RAISES {Empty, NotExist} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.iterator = NIL THEN RAISE NotExist END;

    IF tree.iterator^.leftMostChild # NIL THEN
      (* preorder: step down to first child if possible *)
      tree.iterator := tree.iterator^.leftMostChild;
    ELSIF tree.iterator^.rightSibling # NIL THEN
      (* if no childs, step to siblings *)
      tree.iterator := tree.iterator^.rightSibling;
    ELSE
      (* step up as long as we can and no siblings exist *)
      tree.iterator := tree.iterator^.father;
      WHILE tree.iterator # NIL AND tree.iterator^.rightSibling = NIL DO
        tree.iterator := tree.iterator^.father;
      END;
      IF tree.iterator # NIL THEN
        tree.iterator := tree.iterator^.rightSibling;
      END;
    END;
    RETURN tree.iterator # NIL;
  END NTreeForward;

PROCEDURE NTreeBackward (tree: T): BOOLEAN RAISES {Empty, NotExist} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.iterator = NIL THEN RAISE NotExist END;

    IF tree.iterator^.rightMostChild # NIL THEN
      (* preorder: step down to first child if possible *)
      tree.iterator := tree.iterator^.rightMostChild;
    ELSIF tree.iterator^.leftSibling # NIL THEN
      (* if no childs, step to siblings *)
      tree.iterator := tree.iterator^.leftSibling;
    ELSE
      (* step up as long as we can and no siblings exist *)
      tree.iterator := tree.iterator^.father;
      WHILE tree.iterator # NIL AND tree.iterator^.leftSibling = NIL DO
        tree.iterator := tree.iterator^.father;
      END;
      IF tree.iterator # NIL THEN
        tree.iterator := tree.iterator^.leftSibling;
      END;
    END;
    RETURN tree.iterator # NIL;
  END NTreeBackward;

PROCEDURE NTreeGoIterator (tree: T) RAISES {NotExist, Empty} =
  VAR son, father: NodePtr;
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.iterator = NIL THEN RAISE NotExist END;
    tree.current := tree.iterator;

    (* adjust actSon pointers *)
    son := tree.iterator;
    father := tree.iterator^.father;
    WHILE father # NIL AND father^.actSon # son DO
      father^.actSon := son;
      (* determine number of actSon *)
      father^.actSonNo := 1;
      WHILE son^.leftSibling # NIL DO
        son := son^.leftSibling;
        INC(father^.actSonNo);
      END;
      son := father;
      father := father^.father;
    END;

  END NTreeGoIterator;

PROCEDURE NTreeGetIteratorValue (tree: T; VAR data: Element.T)
  RAISES {NotExist, Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    IF tree.iterator = NIL THEN RAISE NotExist END;
    data := tree.iterator^.data;
  END NTreeGetIteratorValue;

PROCEDURE NTreeInsertSubtree (tree: T; stree: T)
  RAISES {} =
  VAR
    data       : Element.T;
    it, current: NodePtr;
    <* FATAL Empty, NotExist *>
  BEGIN
    (* Is anything to do at all ? *)
    IF stree.root = NIL THEN RETURN END;
    it := stree.root;

    data := it^.data;
    IF tree.root = NIL THEN
      NTreeCreateRoot(tree, data);
    ELSE
      NTreeAddSon(tree, data);
    END;
    (* save root of new subtree for restoring current pointer *)
    current := tree.current;

    WHILE it # NIL DO
      IF it^.leftMostChild # NIL THEN
        (* preorder: step down to first child if possible *)
        it := it^.leftMostChild;
        NTreeAddSon(tree, it^.data);
      ELSIF it^.rightSibling # NIL THEN
        (* if no childs, step to siblings *)
        it := it^.rightSibling;
        NTreeGoFather(tree);
        NTreeAddSon(tree, it^.data);
      ELSE
        (* step up as long as possible and no siblings exist *)
        it := it^.father;
        NTreeGoFather(tree);
        WHILE it # NIL AND it^.rightSibling = NIL DO
          it := it^.father;
          NTreeGoFather(tree);
        END;
        IF it # NIL THEN
          it := it^.rightSibling;
          IF it # NIL THEN NTreeGoFather(tree); NTreeAddSon(tree, it^.data); END;
        END;
      END;
    END;

  END NTreeInsertSubtree;

PROCEDURE NTreeSetCurrent (tree: T; READONLY data: Element.T)
  RAISES {Empty} =
  BEGIN
    IF tree.root = NIL THEN RAISE Empty END;
    tree.current^.data := data;
  END NTreeSetCurrent;

BEGIN
END NTree.
