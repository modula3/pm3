GENERIC INTERFACE NTree(Element);

(***************************************************************************)
(* This module implements a NTree of elements of Element.T.  It requires
   ElemType to export a type T *)
(***************************************************************************)
(** Created by:  Reiner Gombert                                            *)


(***************************************************************************)


EXCEPTION
  Empty;                         (* empty tree *)
  NotExist;                      (* not existing node accessed *)

TYPE
  T <: Public;

  Public =
    <*TRANSIENT*> ROOT OBJECT
    METHODS
      (* --- Organization --- *)

      init (): T;
            (* Initialize a NTree created by a NEW(T) command.  After
               initialization, the NTree is empty.  Before inserting nodes,
               one has to create a root. *)


      (* --- Structural operations --- *)

      createRoot (READONLY data: Element.T) RAISES {};

      addSon (READONLY data: Element.T) RAISES {Empty};
              (* Insert Son of Current with value data, son becomes
                 current *)

      deleteCurrent () RAISES {Empty};
                     (* delete the current node of tree together with its
                        subtree, afterwards the father is current node *)

      removeRootToCurrent () RAISES {Empty};
                           (* current becomes root, all thats above is no
                              longer accessible *)


      (* --- Data access --- *)

      getCurrent (VAR data: Element.T) RAISES {Empty};
                  (* Yields the data of the current node *)

      setCurrent (READONLY data: Element.T) RAISES {Empty};
                  (* update the data of the current node *)

      (* --- Queries --- *)

      isEmpty (): BOOLEAN RAISES {};
               (* Yields TRUE if the NTree is empty. *)

      atRoot (): BOOLEAN RAISES {Empty};
              (* TRUE iff root is the current node *)

      noOfSons (): CARDINAL RAISES {Empty};
                (* Yields number of sons of current *)

      sonNo (): CARDINAL RAISES {Empty, NotExist};
             (* Yields i, if current is i-th son of his father*)

      actSonNo (): CARDINAL RAISES {Empty, NotExist};
                (* Yields i, if the actual marked son of current is the
                   ith *)

      depth (): CARDINAL RAISES {Empty};
             (* Yields the path length from Root to current *)

      heightActSons (): CARDINAL RAISES {Empty};
                     (* Yields the path length from current to a leaf,
                        following the actual successors *)

      (* --- Navigation --- *)

      goFather () RAISES {NotExist, Empty};
                (* go to the father of the current *)

      goRoot () RAISES {Empty};
              (* go to the root of the NTree *)

      goIthSon (READONLY i: CARDINAL)
                RAISES {NotExist, Empty};
                (* go to the i-th son of the current *)

      goActSon () RAISES {NotExist, Empty};
                (* go in direction of actual son *)

      goNextSon () RAISES {NotExist, Empty};
                 (* go to the next son *)

      goPrevSon () RAISES {NotExist, Empty};
                 (* go to the previous son *)

      (* --- Iteration --- *)

      (* Iterations don't change the current node.  With forward, the nodes
         are traversed in a preorder fashion, with backward in inverse
         preorder (rigth to left - not postorder).  Note: After
         t.forward(); t.backward(); will not return to the start node. *)
      loop () RAISES {Empty};
            (* iteration starts with the root node *)

      loopFromCurrent () RAISES {Empty};
                       (* iteration starts at the current node *)

      forward (): BOOLEAN RAISES {Empty, NotExist};
               (* go to the next element and get it.  The return parameter
                  states whether more nodes can be reached during this
                  loop.  Calling forward without loop or loopFromCurrent or
                  after forard() returned FALSE causes NotExist to be
                  raised. *)

      backward (): BOOLEAN RAISES {Empty, NotExist};
                (* Get some element and go to the previous.  See forward
                   for details. *)

      getIteratorValue (VAR data: Element.T) RAISES {NotExist, Empty};
                        (* Get the stored data for the actual node of the
                           iteration *)

      goIterator () RAISES {NotExist, Empty};
                  (* set the current element to the actual node of the
                     iteration *)

      (* --- Copy --- *)

      insertSubtree (stree: T) RAISES {};
                     (* Copy stree and insert it as subtree of the current
                        node (or root if tree is empty).  The actual son of
                        the tree will be the root of the new subtree. *)

    END;

END NTree.
