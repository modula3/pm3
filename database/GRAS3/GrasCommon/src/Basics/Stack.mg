GENERIC MODULE Stack(ElemType);

(***************************************************************************)
(** Created by:  Reiner Gombert                                            *)


(***************************************************************************)


TYPE
  NodeTypePtr = BRANDED REF NodeType; (* pointer to a stack element *)

  NodeType = RECORD              (* a stack element: *)
               Next   : NodeTypePtr;  (* - Successor *)
               DataPtr: ElemType.T;   (* - Data *)
             END;


REVEAL
  T = Public BRANDED Brand OBJECT
        TopElem: NodeTypePtr;    (* first stack element *)
        IsDef  : BOOLEAN       := FALSE; (* stack is initialized *)
        size   : CARDINAL      := 0;

      OVERRIDES
        init    := StackInit;
        isEmpty := StackIsEmpty;
        isFull  := StackIsFull;
        depth   := StackDepth;
        top     := StackTop;
        pop     := StackPop;
        push    := StackPush;
        clear   := StackClear;
      END;

PROCEDURE StackIsEmpty (stack: T): BOOLEAN RAISES {Undefined} =
  BEGIN
    IF (NOT stack.IsDef) THEN RAISE Undefined; END;
    RETURN (stack.TopElem^.Next = NIL);
  END StackIsEmpty;

PROCEDURE StackIsFull (stack: T): BOOLEAN RAISES {Undefined} =
  BEGIN
    IF (NOT stack.IsDef) THEN RAISE Undefined; END;
    RETURN FALSE;
  END StackIsFull;

PROCEDURE StackDepth (stack: T): CARDINAL RAISES {Undefined} =
  BEGIN
    IF (NOT stack.IsDef) THEN RAISE Undefined; END;
    RETURN stack.size;
  END StackDepth;

PROCEDURE StackPush (stack: T; READONLY data: ElemType.T)
  RAISES {Undefined, Full} =
  VAR help: NodeTypePtr;
  BEGIN
    IF (NOT stack.IsDef) THEN RAISE Undefined; END;
    IF StackIsFull(stack) THEN RAISE Full END;
    help := NEW(NodeTypePtr);
    help^.DataPtr := data;
    help^.Next := stack.TopElem;
    stack.TopElem := help;
    INC(stack.size);
  END StackPush;

PROCEDURE StackClear (stack: T) RAISES {Undefined} =
  BEGIN
    IF (NOT stack.IsDef) THEN RAISE Undefined; END;
    WHILE stack.TopElem^.Next # NIL DO
      stack.TopElem := stack.TopElem^.Next;
    END;
    stack.size := 0;
  END StackClear;

PROCEDURE StackTop (stack: T): ElemType.T RAISES {Undefined, Empty} =
  BEGIN
    IF (NOT stack.IsDef) THEN RAISE Undefined; END;
    IF (stack.TopElem^.Next = NIL) THEN RAISE Empty; END;
    RETURN stack.TopElem^.DataPtr;
  END StackTop;



PROCEDURE StackPop (stack: T): ElemType.T RAISES {Undefined, Empty} =
  VAR data: ElemType.T;
  BEGIN
    IF (NOT stack.IsDef) THEN RAISE Undefined; END;
    IF (stack.isEmpty()) THEN RAISE Empty; END;
    data := stack.TopElem^.DataPtr;
    stack.TopElem := stack.TopElem^.Next;
    DEC(stack.size);
    RETURN data;
  END StackPop;


PROCEDURE StackInit (stack: T): T =
  BEGIN
    stack.TopElem := NEW(NodeTypePtr);
    stack.TopElem^.Next := NIL;
    stack.IsDef := TRUE;
    stack.size := 0;
    RETURN stack;
  END StackInit;


BEGIN
END Stack.
