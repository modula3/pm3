GENERIC MODULE CursorList(Element);

(***************************************************************************)
(** Created by:  Peter Klein                                               *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.4  1996/08/06 16:22:35  roland
    Merge of PAGESERVER and main branch.

# Revision 1.3  1994/03/30  17:18:03  pk
# New shape for generic parameters according to 3.1.
#
# Revision 1.2  1993/10/31  18:08:53  pk
# InsertBefore/BehindCursor won't crash if list is empty and no cursor
# is defined.
#
# Revision 1.1  1993/10/23  19:24:50  pk
# Initial revision
#
*)
(***************************************************************************)


TYPE
  Node = BRANDED REF RECORD      (* a list element: *)
                       previous: Node;       (* - Predecessor *)
                       next    : Node;       (* - Successor *)
                       data    : Element.T;  (* - Data *)
                     END;


REVEAL
  T = Public BRANDED OBJECT
        top         : Node;      (* first list element *)
        bottom      : Node;      (* last list element *)
        cursor      : Node;      (* current (cursor) element *)
        noOfElements: CARDINAL;  (* no.  of elements *)
        currentPosition: CARDINAL;  (* current cursor position *)
        isInitialized               := FALSE; (* list is initialized *)

      OVERRIDES
        init                        := Init;
        length                      := Length;
        atTop                       := AtTop;
        atBottom                    := AtBottom;
        cursorPosition              := CursorPosition;
        isEmpty                     := IsEmpty;
        gotoTop                     := GotoTop;
        gotoBottom                  := GotoBottom;
        gotoNext                    := GotoNext;
        gotoPrevious                := GotoPrevious;
        gotoElement                 := GotoElement;
        gotoPosition                := GotoPosition;
        step                        := Step;
        cyclicStep                  := CyclicStep;
        clear                       := Clear;
        deleteCurrent               := DeleteCurrent;
        deleteElement               := DeleteElement;
        extractCurrent              := ExtractCurrent;
        getCurrent                  := GetCurrent;
        replaceCurrent              := ReplaceCurrent;
        insertAtTop                 := InsertAtTop;
        insertAtBottom              := InsertAtBottom;
        insertBeforeCursor          := InsertBeforeCursor;
        insertBehindCursor          := InsertBehindCursor;
        sort                        := Sort;
        insertSorted                := InsertSorted;
        removeRange                 := RemoveRange;
        exchangeCurrentWithNext     := ExchangeCurrentWithNext;
        exchangeCurrentWithPrevious := ExchangeCurrentWithPrevious;
        exchangePosition            := ExchangePosition;
        moveCurrentToTop            := MoveCurrentToTop;
        moveCurrentToBottom         := MoveCurrentToBottom;
        moveElementBeforePosition   := MoveElementBeforePosition;
        moveElementBehindPosition   := MoveElementBehindPosition;
        copyCurrentToTop            := CopyCurrentToTop;
        copyCurrentToBottom         := CopyCurrentToBottom;
        copyElementBeforePosition   := CopyElementBeforePosition;
        copyElementBehindPosition   := CopyElementBehindPosition;
        copy                        := Copy;
        insertListBeforeCursor      := InsertListBeforeCursor;
        insertListBehindCursor      := InsertListBehindCursor;
        splitListBeforeCursor       := SplitListBeforeCursor;
        splitListBehindCursor       := SplitListBehindCursor;
        loop                        := Loop;
        get                         := Get;
        invert                      := Invert;
        pack                        := Pack;
        checkConsistency            := CheckConsistency;
      END;


PROCEDURE Init (list: T): T =
  BEGIN
    (* create dummy top and bottom nodes *)
    list.top := NEW(Node);
    list.bottom := NEW(Node);
    list.top.previous := NIL;
    list.bottom.next := NIL;
    list.top.next := list.bottom;
    list.bottom.previous := list.top;
    list.currentPosition := 0;
    list.noOfElements := 0;
    list.isInitialized := TRUE;
    RETURN list;
  END Init;


PROCEDURE Length (list: T): CARDINAL RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    RETURN list.noOfElements;
  END Length;


PROCEDURE AtTop (list: T): BOOLEAN RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    RETURN (list.currentPosition < 2);
  END AtTop;


PROCEDURE AtBottom (list: T): BOOLEAN RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    RETURN (list.currentPosition = list.noOfElements);
  END AtBottom;


PROCEDURE CursorPosition (list: T): CARDINAL RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    RETURN list.currentPosition;
  END CursorPosition;


PROCEDURE IsEmpty (list: T): BOOLEAN RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    RETURN (list.noOfElements = 0);
  END IsEmpty;


PROCEDURE Clear (list: T) RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    list.cursor := list.top.next;

    (* set cursor to undefined position *)
    list.currentPosition := 0;
    list.noOfElements := 0;

    (* make cyclic links *)
    list.top.next := list.bottom;
    list.bottom.previous := list.top;
  END Clear;


PROCEDURE GotoTop (list: T) RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.noOfElements = 0) THEN RETURN; END;
    list.cursor := list.top.next;
    list.currentPosition := 1;
  END GotoTop;


PROCEDURE GotoBottom (list: T) RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.noOfElements = 0) THEN RETURN; END;
    list.cursor := list.bottom.previous;
    list.currentPosition := list.noOfElements;
  END GotoBottom;


PROCEDURE GotoPrevious (list: T) RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    (* set cursor undefined if at top *)
    IF (list.currentPosition < 2) THEN
      list.currentPosition := 0;
    ELSE
      list.cursor := list.cursor.previous;
      DEC(list.currentPosition);
    END;
  END GotoPrevious;


PROCEDURE GotoNext (list: T) RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    (* set cursor undefined if at bottom *)
    IF (list.currentPosition = list.noOfElements) THEN
      list.currentPosition := 0;
    ELSIF (list.currentPosition # 0) THEN
      list.cursor := list.cursor.next;
      INC(list.currentPosition);
    END;
  END GotoNext;


PROCEDURE GotoElement (         list : T;
                       READONLY data : Element.T;
                       VAR      found: BOOLEAN    )
  RAISES {NotInitialized} =
  VAR currentPosition: CARDINAL;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    found := FALSE;
    IF (list.noOfElements = 0) THEN RETURN; END;

    (* start at first element if cursor undefined *)
    IF (list.currentPosition = 0) THEN
      list.cursor := list.top.next;
      list.currentPosition := 1;
    END;

    (* remember current position *)
    currentPosition := list.currentPosition;
    (* loop until element found or start position reached *)
    REPEAT
      found := Element.Equal(data, list.cursor.data);
      (* cyclic next *)
      IF (NOT found) THEN
        IF (list.currentPosition = list.noOfElements) THEN
          list.cursor := list.top.next;
          list.currentPosition := 1;
        ELSE
          list.cursor := list.cursor.next;
          INC(list.currentPosition);
        END;
      END;
    UNTIL (found OR (list.currentPosition = currentPosition));
  END GotoElement;


PROCEDURE Step (list: T; steps: INTEGER)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN RAISE CursorError; END;

    (* step forward *)
    WHILE ((steps > 0) AND (list.cursor # list.bottom)) DO
      list.cursor := list.cursor.next;
      INC(list.currentPosition);
      DEC(steps);
    END;
    IF (list.cursor = list.bottom) THEN RAISE CursorError; END;

    (* step backward *)
    WHILE ((steps < 0) AND (list.cursor # list.top)) DO
      list.cursor := list.cursor.previous;
      DEC(list.currentPosition);
      INC(steps);
    END;
    IF (list.cursor = list.top) THEN RAISE CursorError; END;
  END Step;


PROCEDURE CyclicStep (list: T; steps: INTEGER)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN RAISE CursorError; END;

    (* step forward *)
    WHILE (steps > 0) DO
      list.cursor := list.cursor.next;
      (* cyclic next *)
      IF (list.cursor = list.bottom) THEN
        list.cursor := list.top.next;
        list.currentPosition := 1;
      ELSE
        INC(list.currentPosition);
      END;
      DEC(steps);
    END;

    (* step backward *)
    WHILE (steps < 0) DO
      list.cursor := list.cursor.previous;
      (* cyclic next *)
      IF (list.cursor = list.top) THEN
        list.cursor := list.bottom.previous;
        list.currentPosition := list.noOfElements;
      ELSE
        DEC(list.currentPosition);
      END;
      INC(steps);
    END;
  END CyclicStep;


PROCEDURE GotoPosition (list: T; position: CARDINAL)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (position = 0) OR (position > list.noOfElements) THEN
      RAISE CursorError;
    END;

    (* if cursor undefined set to list end *)
    IF (list.currentPosition = 0) THEN
      list.cursor := list.bottom.previous;
      list.currentPosition := list.noOfElements;
    END;

    (* determine distance from position to current, list top and list
       bottom *)
    WITH topDelta    = position - 1,
         cursorDelta = position - list.currentPosition,
         bottomDelta = position - list.noOfElements,

         topDistance    = ABS(topDelta),
         cursorDistance = ABS(cursorDelta),
         bottomDistance = ABS(bottomDelta)  DO

      (* determine which is next to position and step from there *)
      IF (topDistance <= MIN(cursorDistance, bottomDistance)) THEN
        (* it's list top *)
        list.cursor := list.top.next;
        list.currentPosition := 1;
        Step(list, topDelta);
      ELSIF (cursorDistance <= MIN(topDistance, bottomDistance)) THEN
        (* it's cursorPosition *)
        Step(list, cursorDelta);
      ELSE
        (* it's list bottom *)
        list.cursor := list.bottom.previous;
        list.currentPosition := list.noOfElements;
        Step(list, bottomDelta);
      END;
    END;
  END GotoPosition;


PROCEDURE DeleteElement (         list : T;
                         READONLY data : Element.T;
                         VAR      found: BOOLEAN    )
  RAISES {NotInitialized} =
  <* FATAL CursorError *>
  BEGIN
    GotoElement(list, data, found);
    IF (found) THEN DeleteCurrent(list); END;
  END DeleteElement;


PROCEDURE DeleteCurrent (list: T) RAISES {NotInitialized, CursorError} =
  VAR nextNode: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN RAISE CursorError; END;
    (* unlink current element *)
    list.cursor.previous.next := list.cursor.next;
    list.cursor.next.previous := list.cursor.previous;

    (* determine next position *)
    nextNode := list.cursor.next;
    IF (nextNode = list.bottom) THEN
      nextNode := list.cursor.previous;
      (* if no predecessor -> cursor undefined *)
      IF (nextNode = list.top) THEN
        list.currentPosition := 0;
      ELSE
        DEC(list.currentPosition);
      END;
    END;

    (* adjust list values *)
    DEC(list.noOfElements);
    list.cursor := nextNode;
  END DeleteCurrent;


PROCEDURE ExtractCurrent (list: T; VAR found: BOOLEAN): Element.T
  RAISES {NotInitialized} =
  VAR data: Element.T;
  <* FATAL CursorError *>

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN
      found := FALSE;
    ELSE
      found := TRUE;
      data := list.cursor.data;
      DeleteCurrent(list);
    END;
    RETURN data;
  END ExtractCurrent;


PROCEDURE GetCurrent (list: T; VAR found: BOOLEAN): Element.T
  RAISES {NotInitialized} =
  VAR data: Element.T;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN
      found := FALSE;
    ELSE
      found := TRUE;
      data := list.cursor.data;
    END;
    RETURN data;
  END GetCurrent;


PROCEDURE ReplaceCurrent (list: T; READONLY data: Element.T)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN RAISE CursorError; END;
    list.cursor.data := data;
  END ReplaceCurrent;


PROCEDURE InsertAtTop (list: T; READONLY data: Element.T)
  RAISES {NotInitialized} =
  <* FATAL CursorError *>
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    list.currentPosition := 1;
    list.cursor := list.top.next;
    InsertBeforeCursor(list, data);
  END InsertAtTop;


PROCEDURE InsertAtBottom (list: T; READONLY data: Element.T)
  RAISES {NotInitialized} =
  <* FATAL CursorError *>
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    list.currentPosition := list.noOfElements;
    list.cursor := list.bottom.previous;
    InsertBehindCursor(list, data);
  END InsertAtBottom;


PROCEDURE InsertBeforeCursor (list: T; READONLY data: Element.T)
  RAISES {NotInitialized, CursorError} =
  VAR newNode: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF ((list.currentPosition = 0) AND (list.noOfElements > 0)) THEN
      RAISE CursorError;
    END;

    (* create new list node *)
    newNode := NEW(Node);
    newNode.data := data;
    IF (list.noOfElements = 0) THEN
      list.cursor := list.bottom;
      list.currentPosition := 1;
    END;

    (* link new node *)
    newNode.next := list.cursor;
    newNode.previous := list.cursor.previous;
    list.cursor.previous.next := newNode;
    list.cursor.previous := newNode;

    (* update list-global values *)
    list.cursor := newNode;
    INC(list.noOfElements);
  END InsertBeforeCursor;


PROCEDURE InsertBehindCursor (list: T; READONLY data: Element.T)
  RAISES {NotInitialized, CursorError} =
  VAR newNode: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF ((list.currentPosition = 0) AND (list.noOfElements > 0)) THEN
      RAISE CursorError;
    END;

    (* create new list node *)
    newNode := NEW(Node);
    newNode.data := data;
    IF (list.noOfElements = 0) THEN list.cursor := list.top; END;

    (* link new node *)
    newNode.next := list.cursor.next;
    newNode.previous := list.cursor;
    list.cursor.next.previous := newNode;
    list.cursor.next := newNode;

    (* update list-global values *)
    list.cursor := newNode;
    INC(list.noOfElements);
    INC(list.currentPosition);
  END InsertBehindCursor;


PROCEDURE ExchangeCurrentWithPrevious (list: T)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition < 2) THEN RAISE CursorError; END;

    (* update next links *)
    list.cursor.previous.previous.next := list.cursor;
    list.cursor.previous.next := list.cursor.next;
    list.cursor.next := list.cursor.previous;

    (* update previous links *)
    list.cursor.previous := list.cursor.previous.previous;
    list.cursor.next.previous := list.cursor;
    list.cursor.next.next.previous := list.cursor.next;

    (* update current *)
    DEC(list.currentPosition);
  END ExchangeCurrentWithPrevious;


PROCEDURE ExchangeCurrentWithNext (list: T)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;

    IF (list.currentPosition = list.noOfElements) THEN
      RAISE CursorError;
    END;

    (* update next links *)
    list.cursor.next.next.previous := list.cursor;
    list.cursor.next.previous := list.cursor.previous;
    list.cursor.previous := list.cursor.next;

    (* update previous links *)
    list.cursor.next := list.cursor.next.next;
    list.cursor.previous.next := list.cursor;
    list.cursor.previous.previous.next := list.cursor.previous;

    (* update current *)
    INC(list.currentPosition);
  END ExchangeCurrentWithNext;


PROCEDURE ExchangePosition (list: T; position1, position2: CARDINAL)
  RAISES {NotInitialized, CursorError} =
  VAR cursor1, nextNode: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (position1 = position2) THEN RETURN; END;

    (* goto position1, remember pointer *)
    GotoPosition(list, position1);
    cursor1 := list.cursor;
    (* goto position2 *)
    GotoPosition(list, position2);

    (* nodes are neighbours *)
    IF (cursor1.next = list.cursor) THEN
      ExchangeCurrentWithPrevious(list);
      RETURN;
    ELSIF (cursor1 = list.cursor.next) THEN
      ExchangeCurrentWithNext(list);
      RETURN;
    END;

    (* adjust links *)
    list.cursor.next.previous := cursor1;
    cursor1.previous.next := list.cursor;
    list.cursor.previous.next := cursor1;
    cursor1.next.previous := list.cursor;
    nextNode := cursor1.next;
    cursor1.next := list.cursor.next;
    list.cursor.next := nextNode;
    nextNode := cursor1.previous;
    cursor1.previous := list.cursor.previous;
    list.cursor.previous := nextNode;

    (* update current *)
    list.currentPosition := position1;
  END ExchangePosition;


PROCEDURE MoveCurrentToTop (list: T) RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN RAISE CursorError; END;
    MoveCurrentBefore(list, list.top.next);
    list.currentPosition := 1;
  END MoveCurrentToTop;


PROCEDURE MoveCurrentToBottom (list: T)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN RAISE CursorError; END;
    MoveCurrentBehind(list, list.bottom.previous);
    list.currentPosition := list.noOfElements;
  END MoveCurrentToBottom;


PROCEDURE MoveElementBeforePosition (list: T; from, to: CARDINAL)
  RAISES {NotInitialized, CursorError} =
  VAR targetNode: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    (* goto to and remember pointer *)
    GotoPosition(list, to);
    targetNode := list.cursor;
    (* goto From *)
    GotoPosition(list, from);

    MoveCurrentBefore(list, targetNode);
    IF (from < to) THEN
      list.currentPosition := to - 1;
    ELSE
      list.currentPosition := to;
    END;
  END MoveElementBeforePosition;


PROCEDURE MoveElementBehindPosition (list: T; from, to: CARDINAL)
  RAISES {NotInitialized, CursorError} =
  VAR targetNode: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    (* goto to and remember pointer *)
    GotoPosition(list, to);
    targetNode := list.cursor;
    (* goto From *)
    GotoPosition(list, from);

    MoveCurrentBehind(list, targetNode);
    IF (from < to) THEN
      list.currentPosition := to;
    ELSE
      list.currentPosition := to - 1;
    END;
  END MoveElementBehindPosition;


PROCEDURE CopyCurrentToTop (list: T) RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN RAISE CursorError; END;

    WITH tmpData = list.cursor.data DO
      list.currentPosition := 1;
      list.cursor := list.top.next;
      InsertBeforeCursor(list, tmpData);
    END;
  END CopyCurrentToTop;


PROCEDURE CopyCurrentToBottom (list: T)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN RAISE CursorError; END;

    WITH tmpData = list.cursor.data DO
      list.currentPosition := list.noOfElements;
      list.cursor := list.bottom.previous;
      InsertBehindCursor(list, tmpData);
    END;
  END CopyCurrentToBottom;


PROCEDURE CopyElementBeforePosition (list: T; from, to: CARDINAL)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    GotoPosition(list, from);
    WITH tmpData = list.cursor.data DO
      GotoPosition(list, to);
      InsertBeforeCursor(list, tmpData);
    END;
  END CopyElementBeforePosition;


PROCEDURE CopyElementBehindPosition (list: T; from, to: CARDINAL)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    GotoPosition(list, from);
    WITH tmpData = list.cursor.data DO
      GotoPosition(list, to);
      InsertBehindCursor(list, tmpData);
    END;
  END CopyElementBehindPosition;


PROCEDURE Copy (source: T): T RAISES {NotInitialized} =
  VAR
    currentNode, newNode: Node;
    target              : T;

  BEGIN
    IF (NOT source.isInitialized) THEN RAISE NotInitialized; END;

    (* create target list *)
    target := NEW(T).init();

    currentNode := source.top.next;
    WHILE (currentNode # source.bottom) DO
      (* create new target node at list end *)
      newNode := NEW(Node);
      newNode.data := currentNode.data;
      newNode.previous := target.bottom.previous;
      newNode.next := target.bottom;
      target.bottom.previous.next := newNode;
      target.bottom.previous := newNode;
      (* remember source list cursor *)
      IF (currentNode = source.cursor) THEN target.cursor := newNode; END;
      currentNode := currentNode.next;
    END;

    (* set target list cursor *)
    target.currentPosition := source.currentPosition;
    target.noOfElements := source.noOfElements;
    RETURN target;
  END Copy;


PROCEDURE InsertListBeforeCursor (first: T; VAR second: T)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT first.isInitialized) OR (NOT second.isInitialized) THEN
      RAISE NotInitialized;
    END;
    IF (first.currentPosition = 0) THEN RAISE CursorError; END;

    (* second list is emtpy *)
    IF (second.noOfElements = 0) THEN second := NIL; RETURN; END;

    (* link in second list before cursor element *)
    first.cursor.previous.next := second.top.next;
    second.top.next.previous := first.cursor.previous;
    first.cursor.previous := second.bottom.previous;
    second.bottom.previous.next := first.cursor;

    (* update current position *)
    INC(first.noOfElements, second.noOfElements);
    INC(first.currentPosition, second.noOfElements);
    second := NIL;
  END InsertListBeforeCursor;


PROCEDURE InsertListBehindCursor (first: T; VAR second: T)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT first.isInitialized) OR (NOT second.isInitialized) THEN
      RAISE NotInitialized;
    END;
    IF (first.currentPosition = 0) THEN RAISE CursorError; END;

    (* second list is empty *)
    IF (second.noOfElements = 0) THEN second := NIL; RETURN; END;

    (* link in second list before cursor element *)
    first.cursor.next.previous := second.bottom.previous;
    second.bottom.previous.next := first.cursor.next;
    first.cursor.next := second.top.next;
    second.top.next.previous := first.cursor;
    INC(first.noOfElements, second.noOfElements);
    second := NIL;
  END InsertListBehindCursor;


PROCEDURE SplitListBeforeCursor (source: T; tail: T)
  RAISES {NotInitialized, CursorError} =
  VAR tmpNode: Node;

  BEGIN
    IF (NOT source.isInitialized) OR (NOT tail.isInitialized) THEN
      RAISE NotInitialized;
    END;
    IF (source.currentPosition = 0) THEN RAISE CursorError; END;

    (* update links *)
    tmpNode := tail.bottom.previous;
    source.cursor.previous.next := source.bottom;
    tail.bottom.previous.next := source.cursor;
    source.bottom.previous.next := tail.bottom;
    tail.bottom.previous := source.bottom.previous;
    source.bottom.previous := source.cursor.previous;
    source.cursor.previous := tmpNode;

    (* update current values *)
    tail.currentPosition := tail.noOfElements + 1;
    INC(
      tail.noOfElements, source.noOfElements + 1 - source.currentPosition);
    source.noOfElements := source.currentPosition - 1;
    source.currentPosition := source.noOfElements;
    tail.cursor := source.cursor;
    source.cursor := source.bottom.previous;
  END SplitListBeforeCursor;


PROCEDURE SplitListBehindCursor (source: T; tail: T)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    IF (NOT source.isInitialized) OR (NOT tail.isInitialized) THEN
      RAISE NotInitialized;
    END;
    IF (source.currentPosition = 0) THEN RAISE CursorError; END;
    IF (source.currentPosition = source.noOfElements) THEN RETURN; END;

    (* update links *)
    source.bottom.previous.next := tail.bottom;
    tail.bottom.previous.next := source.cursor.next;
    source.cursor.next.previous := tail.bottom.previous;
    tail.bottom.previous := source.bottom.previous;
    source.bottom.previous := source.cursor;
    source.cursor.next := source.bottom;

    (* update current values *)
    tail.currentPosition := tail.noOfElements + 1;
    INC(tail.noOfElements, source.noOfElements - source.currentPosition);
    source.noOfElements := source.currentPosition;
    source.currentPosition := source.noOfElements;
    tail.cursor := source.cursor;
    source.cursor := source.bottom.previous;
  END SplitListBehindCursor;


PROCEDURE RemoveRange (list: T; position1, position2: CARDINAL)
  RAISES {NotInitialized, CursorError} =
  VAR current: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;

    (* position1 = position2 -> delete element *)
    IF (position1 = position2) THEN
      GotoPosition(list, position1);
      DeleteCurrent(list);
      RETURN;
    END;

    (* sort positions to get position1 < position2 *)
    IF (position1 > position2) THEN
      VAR tmp := position1;
      BEGIN
        position1 := position2;
        position2 := tmp;
      END;
    END;

    (* goto position1, remember pointer *)
    GotoPosition(list, position1);
    current := list.cursor;
    GotoPosition(list, position2);

    (* unlink range from current to list.cursor *)
    current.previous.next := list.cursor.next;
    list.cursor.next.previous := current.previous;
    list.cursor := list.cursor.next;
    list.currentPosition := position1;

    (* determine new current *)
    IF (list.cursor = list.bottom) THEN
      list.cursor := list.cursor.previous;
      (* cursor will be undefined if position1 = 1, position2 =
         noOfElements *)
      DEC(list.currentPosition);
    END;
    DEC(list.noOfElements, position2 - position1 + 1);
  END RemoveRange;


PROCEDURE Loop (list: T) RAISES {NotInitialized} =
  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.noOfElements = 0) THEN RETURN; END;
    list.currentPosition := 1;
    list.cursor := list.top.next;
  END Loop;


PROCEDURE Get (list: T; VAR found: BOOLEAN): Element.T
  RAISES {NotInitialized} =
  VAR data: Element.T;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.currentPosition = 0) THEN
      found := FALSE;
    ELSE
      found := TRUE;
      data := list.cursor.data;
      list.cursor := list.cursor.next;
      INC(list.currentPosition);

      (* set cursor undefined if outside list *)
      IF (list.currentPosition > list.noOfElements) THEN
        list.currentPosition := 0;
      END;
    END;
    RETURN data;
  END Get;


PROCEDURE Invert (list: T) RAISES {NotInitialized} =
  VAR current, next: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;

    (* swap all pointers *)
    current := list.top;
    WHILE (current # NIL) DO
      next := current.next;
      current.next := current.previous;
      current.previous := next;
      current := current.previous;
    END;

    (* swap top and bottom *)
    next := list.top;
    list.top := list.bottom;
    list.bottom := next;

    (* update current position *)
    IF (list.currentPosition # 0) THEN
      list.currentPosition := list.noOfElements + 1 - list.currentPosition;
    END
  END Invert;


PROCEDURE Pack (list: T) RAISES {NotInitialized} =
  VAR
    currentElement: Element.T;
    currentNode   : Node;
  <* FATAL CursorError *>

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.noOfElements < 2) THEN RETURN; END;

    currentElement := list.top.next.data;
    list.cursor := list.top.next.next;
    WHILE (list.cursor # list.bottom) DO
      currentNode := list.cursor.next;
      IF (Element.Equal(currentElement, list.cursor.data)) THEN
        DeleteCurrent(list);
      ELSE
        currentElement := list.cursor.data;
      END;
      list.cursor := currentNode;
    END;

    list.cursor := list.top.next;
    list.currentPosition := 1;
  END Pack;


PROCEDURE Sort (list: T; ascending: BOOLEAN) RAISES {NotInitialized} =
  (* Merge-Sort for lists - see G.H.  Gonnet, "Handbook of Algorithms and
     Data Structures", Addison-Wesley *)
  VAR last: Node;

  PROCEDURE MergeLists (a, b: Node): Node =
    VAR first, last, current: Node;

    BEGIN
      first := NIL;
      WHILE (b # NIL) DO
        IF (a = NIL) THEN
          a := b;
          b := NIL;
        ELSE
          WITH compare = Element.Compare(b.data, a.data) DO
            IF (((compare = 1) AND ascending)
                  OR ((compare = -1) AND (NOT ascending))) THEN
              current := a;
              a := a.next;
            ELSE
              current := b;
              b := b.next;
            END;
          END;
          current.next := NIL;
          IF (first = NIL) THEN
            first := current;
          ELSE
            last.next := current;
          END;
          last := current;
        END;
      END;
      IF (first = NIL) THEN first := a; ELSE last.next := a; END;
      last := a;
      RETURN first;
    END MergeLists;

  PROCEDURE MergeSort (VAR root: Node; noOfElements: CARDINAL): Node =
    VAR oldRoot, a, b: Node;

    BEGIN
      IF (root = NIL) THEN
        RETURN NIL;
      ELSIF (noOfElements > 1) THEN
        a := MergeSort(root, noOfElements DIV 2);
        b := MergeSort(root, (noOfElements + 1) DIV 2);
        RETURN MergeLists(a, b);
      ELSE
        oldRoot := root;
        root := root.next;
        oldRoot.next := NIL;
        RETURN oldRoot;
      END;
    END MergeSort;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    IF (list.noOfElements < 2) THEN RETURN; END;

    list.top.next := MergeSort(list.top.next, list.noOfElements);

    (* update previous links *)
    last := list.top;
    list.cursor := list.top.next;
    WHILE (list.cursor # NIL) DO
      list.cursor.previous := last;
      last := list.cursor;
      list.cursor := list.cursor.next;
    END;
    last.next := list.bottom;
    list.bottom.previous := last;

    (* set cursor to first element *)
    list.currentPosition := 1;
    list.cursor := list.top.next;
  END Sort;


PROCEDURE InsertSorted (         list     : T;
                        READONLY data     : Element.T;
                                 ascending: BOOLEAN    )
  RAISES {NotInitialized} =

  <* FATAL CursorError *>

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;
    list.cursor := list.top.next;
    list.currentPosition := 1;

    (* find insertion position *)
    LOOP
      IF (list.cursor = list.bottom) THEN EXIT; END;
      WITH compare = Element.Compare(list.cursor.data, data) DO
        IF (((compare = 1) AND ascending)
              OR ((compare = -1) AND (NOT ascending))) THEN
          InsertBeforeCursor(list, data);
          RETURN;
        END;
      END;
      list.cursor := list.cursor.next;
      INC(list.currentPosition);
    END;

    (* insert at end *)
    list.currentPosition := list.noOfElements;
    list.cursor := list.bottom.previous;
    InsertBehindCursor(list, data);
  END InsertSorted;


PROCEDURE CheckConsistency (list: T) RAISES {NotInitialized, CursorError} =
  VAR current: Node;

  BEGIN
    IF (NOT list.isInitialized) THEN RAISE NotInitialized; END;

    current := list.top;
    FOR i := 1 TO 1 + list.noOfElements DO
      IF (list.currentPosition # 0) THEN
        IF (current = list.cursor) THEN
          IF (i - 1 # list.currentPosition) THEN RAISE CursorError; END;
        END;
      END;
      IF (current = NIL) THEN RAISE CursorError; END;
      current := current.next;
    END;
    IF (current # list.bottom) THEN RAISE CursorError; END;

    FOR i := 1 TO 1 + list.noOfElements DO
      IF (current = NIL) THEN RAISE CursorError; END;
      current := current.previous;
    END;
    IF (current # list.top) THEN RAISE CursorError; END;
  END CheckConsistency;


PROCEDURE MoveCurrentBefore (list: T; to: Node)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    (* Current already is before to *)
    IF (list.cursor.next = to) THEN RETURN; END;

    (* Current is behind to *)
    IF (list.cursor.previous = to) THEN
      ExchangeCurrentWithPrevious(list);
      RETURN;
    END;

    list.cursor.next.previous := list.cursor.previous;
    list.cursor.previous.next := list.cursor.next;
    list.cursor.next := to;
    list.cursor.previous := to.previous;
    to.previous.next := list.cursor;
    to.previous := list.cursor;
  END MoveCurrentBefore;


(* move the current element behind To *)
PROCEDURE MoveCurrentBehind (list: T; to: Node)
  RAISES {NotInitialized, CursorError} =
  BEGIN
    (* Current already is behind to *)
    IF (list.cursor.previous = to) THEN RETURN; END;

    (* Current is before to *)
    IF (list.cursor.next = to) THEN
      ExchangeCurrentWithNext(list);
      RETURN;
    END;

    list.cursor.next.previous := list.cursor.previous;
    list.cursor.previous.next := list.cursor.next;
    list.cursor.previous := to;
    list.cursor.next := to.next;
    to.next.previous := list.cursor;
    to.next := list.cursor;
  END MoveCurrentBehind;

BEGIN
END CursorList.
