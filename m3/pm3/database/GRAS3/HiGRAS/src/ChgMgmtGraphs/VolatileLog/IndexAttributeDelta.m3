MODULE IndexAttributeDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.4  1998/05/19 10:18:03  roland
    Support for log-groups implemented.

    Revision 1.3  1998/01/21 12:33:34  roland
    Distinction between forward and backward delta necessary.

    Revision 1.2  1997/07/21 10:39:35  roland
    A few bug-fixes and implementation of free memory list logging.

    Revision 1.1  1997/05/30 07:51:45  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

IMPORT Node, GraphCommand, Text;
IMPORT Journal, Variant, Fmt;

TYPE State = {Undefined, OldDeleted, OldPut, NewPut, NewDeleted};

(** Each forward IndexAttributeDelta.T changes its state according to the
    following finite automaton:
                                _delete__
                               v         \
             delete-> OldDeleted        OldPut
              /                \___put___^
             /
     Undefined
             \              ____put_____
              \            v            \
               put---> NewPut         NewDeleted
                           \___delete____^
*)

REVEAL
  T = Public BRANDED OBJECT
        node              : Node.T;
        index             : CARDINAL;
        forward           : BOOLEAN;
        putvalue, delvalue: TEXT;
        state             : State;
        loopCount         : CARDINAL;
        nextFree          : T;
      OVERRIDES
        init        := Init;
        put         := Put;
        delete      := Delete;
        costs       := Costs;
        loop        := Loop;
        getNext     := GetNext;
        reverseLoop := ReverseLoop;
        getPrev     := GetPrev;
      END;

PROCEDURE Init (id: T; node: Node.T; indexno: CARDINAL; forward: BOOLEAN):
  T =
  BEGIN
    id.node := node;
    id.index := indexno;
    id.forward := forward;
    id.state := State.Undefined;
    id.putvalue := NIL;
    id.delvalue := NIL;
    id.loopCount := 0;
    RETURN id;
  END Init;

PROCEDURE Put (id: T; value: TEXT) =
  BEGIN
    IF id.forward OR id.state = State.Undefined THEN
      (* store all values for forward deltas and only the first for
         backward deltas *)
      id.putvalue := value;
    END;
    CASE id.state OF
      State.Undefined => id.state := State.NewPut;
    | State.OldDeleted => id.state := State.OldPut;
    | State.NewDeleted => id.state := State.NewPut;
    ELSE
      <* ASSERT FALSE *>
    END;
  END Put;

PROCEDURE Delete (id: T; value: TEXT) =
  BEGIN
    IF NOT id.forward OR id.state = State.Undefined THEN
      (* store all values for backward deltas and only the first for
         forward deltas. *)
      id.delvalue := value;
    END;
    CASE id.state OF
      State.Undefined => id.state := State.OldDeleted;
    | State.OldPut => id.state := State.OldDeleted;
      (* check Text.Equal(id.putvalue, value) needed? *)
    | State.NewPut => id.state := State.NewDeleted;
      (* check Text.Equal(id.putvalue, value) needed? *)
    ELSE
      <* ASSERT FALSE *>
    END;
  END Delete;

PROCEDURE Costs (id: T): CARDINAL =
  BEGIN
    CASE id.state OF
      State.Undefined, State.NewDeleted => RETURN 0;
    | State.OldDeleted, State.NewPut => RETURN 1;
    | State.OldPut => RETURN 2;
    END;
  END Costs;


PROCEDURE Loop (id: T) =
  BEGIN
    <* ASSERT id.state # State.Undefined *>
    id.loopCount := 2;
  END Loop;

PROCEDURE GetNext (id: T; VAR com: GraphCommand.T): BOOLEAN =
  BEGIN
    <* ASSERT id.state # State.Undefined *>
    IF id.forward THEN
      IF id.loopCount = 2 THEN
        DEC(id.loopCount);
        CASE id.state OF
          State.OldDeleted, State.OldPut =>
            GraphCommand.DeleteIndex(com, id.node, id.index,
                                     Text.Length(id.delvalue), id.delvalue);
            RETURN TRUE;
        | State.NewPut =>
            GraphCommand.PutIndex(com, id.node, id.index,
                                  Text.Length(id.putvalue), id.putvalue);
            RETURN TRUE;
        | State.NewDeleted => RETURN FALSE;
        ELSE
          RETURN FALSE;
        END;
      ELSIF id.loopCount = 1 THEN
        DEC(id.loopCount);
        IF id.state = State.OldPut THEN
          GraphCommand.PutIndex(
            com, id.node, id.index, Text.Length(id.putvalue), id.putvalue);
          RETURN TRUE;
        ELSE
          (* everything done during first loop *)
          RETURN FALSE;
        END;
      ELSE
        RETURN FALSE;
      END;
    ELSE
      IF id.loopCount = 2 THEN
        DEC(id.loopCount);
        CASE id.state OF
          State.OldDeleted, State.NewDeleted =>
            GraphCommand.DeleteIndex(com, id.node, id.index,
                                     Text.Length(id.delvalue), id.delvalue);
            RETURN TRUE;
        | State.NewPut =>
            GraphCommand.PutIndex(com, id.node, id.index,
                                  Text.Length(id.putvalue), id.putvalue);
            RETURN TRUE;
        | State.OldPut => RETURN FALSE;
        ELSE
          RETURN FALSE;
        END;
      ELSIF id.loopCount = 1 THEN
        DEC(id.loopCount);
        IF id.state = State.NewDeleted THEN
          GraphCommand.PutIndex(
            com, id.node, id.index, Text.Length(id.putvalue), id.putvalue);
          RETURN TRUE;
        ELSE
          (* everything done during first loop *)
          RETURN FALSE;
        END;
      ELSE
        RETURN FALSE;
      END;
    END;
  END GetNext;

PROCEDURE ReverseLoop (id: T) =
  BEGIN
    <* ASSERT id.state # State.Undefined *>
    id.loopCount := 2;
  END ReverseLoop;

PROCEDURE GetPrev (id: T; VAR com: GraphCommand.T): BOOLEAN =
  BEGIN
    <* ASSERT id.state # State.Undefined *>
    IF id.forward THEN
      IF id.loopCount = 2 THEN
        DEC(id.loopCount);
        CASE id.state OF
        | State.OldDeleted =>
            GraphCommand.DeleteIndex(com, id.node, id.index,
                                     Text.Length(id.delvalue), id.delvalue);
            RETURN TRUE;
        | State.NewPut, State.OldPut =>
            GraphCommand.PutIndex(com, id.node, id.index,
                                  Text.Length(id.putvalue), id.putvalue);
            RETURN TRUE;
        | State.NewDeleted => RETURN FALSE;
        ELSE
          RETURN FALSE;
        END;
      ELSIF id.loopCount = 1 THEN
        DEC(id.loopCount);
        IF id.state = State.OldPut THEN
          GraphCommand.DeleteIndex(
            com, id.node, id.index, Text.Length(id.delvalue), id.delvalue);
          RETURN TRUE;
        ELSE
          (* everything done during first loop *)
          RETURN FALSE;
        END;
      ELSE
        RETURN FALSE;
      END;
    ELSE
      IF id.loopCount = 2 THEN
        DEC(id.loopCount);
        CASE id.state OF
        | State.OldDeleted =>
            GraphCommand.DeleteIndex(com, id.node, id.index,
                                     Text.Length(id.delvalue), id.delvalue);
            RETURN TRUE;
        | State.NewPut, State.NewDeleted =>
            GraphCommand.PutIndex(com, id.node, id.index,
                                  Text.Length(id.putvalue), id.putvalue);
            RETURN TRUE;
        | State.OldPut => RETURN FALSE;
        ELSE
          RETURN FALSE;
        END;
      ELSIF id.loopCount = 1 THEN
        DEC(id.loopCount);
        IF id.state = State.NewDeleted THEN
          GraphCommand.DeleteIndex(
            com, id.node, id.index, Text.Length(id.delvalue), id.delvalue);
          RETURN TRUE;
        ELSE
          (* everything done during first loop *)
          RETURN FALSE;
        END;
      ELSE
        RETURN FALSE;
      END;
    END;
  END GetPrev;

VAR
  FreeDeltas                  : T        := NIL;
  FreeDeltaSize, MaxFreeDeltas: CARDINAL := 0;

PROCEDURE New (): T =
  VAR res: T := FreeDeltas;
  BEGIN
    IF res = NIL THEN
      RETURN NEW(T);
    ELSE
      FreeDeltas := FreeDeltas.nextFree;
      DEC(FreeDeltaSize);
      RETURN res;
    END;
  END New;

PROCEDURE Dispose (id: T) =
  BEGIN
    id.nextFree := FreeDeltas;
    FreeDeltas := id;
    INC(FreeDeltaSize);
    IF Variant.FreeMemoryListLog > 0 THEN
      IF MaxFreeDeltas < FreeDeltaSize THEN
        MaxFreeDeltas := FreeDeltaSize;
        IF MaxFreeDeltas MOD Variant.FreeMemoryListLog = 0 THEN
          Journal.Add("IndexAttributeDelta free delta list "
                        & Fmt.Int(MaxFreeDeltas));
        END;
      END;
    END;
  END Dispose;

BEGIN
END IndexAttributeDelta.
