MODULE VolatileDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.4  1998/05/19 10:18:05  roland
    Support for log-groups implemented.

    Revision 1.3  1997/07/21 10:39:38  roland
    A few bug-fixes and implementation of free memory list logging.

    Revision 1.2  1997/05/30 07:51:50  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

IMPORT GraphCommand, Node, NodeDelta, EdgeDelta;
IMPORT Variant, Journal, Fmt;

REVEAL
  T = Public BRANDED OBJECT
        edgeDelta    : EdgeDelta.T := NIL;
        nodeDelta    : NodeDelta.T := NIL;
        forward      : BOOLEAN;
        loopNodeDelta: BOOLEAN;
        next         : T;          (* only for free memory list *)
      OVERRIDES
        init           := Init;
        addCommand     := AddCommand;
        costs          := Costs;
        loop           := Loop;
        getNextCommand := GetNextCommand;
        reverseLoop    := ReverseLoop;
        getPrevCommand := GetPrevCommand;
      END;

PROCEDURE Init (delta: T; forward: BOOLEAN): T RAISES {} =
  BEGIN
    delta.nodeDelta := NodeDelta.New().init(forward);
    delta.edgeDelta := EdgeDelta.New().init();
    delta.forward := forward;
    RETURN delta;
  END Init;

PROCEDURE AddCommand (delta: T; READONLY com: GraphCommand.T) RAISES {} =
  BEGIN
    CASE com.operation OF
    | GraphCommand.Operation.CreateNode =>
        delta.nodeDelta.create(
          Node.T{com.args[0], com.args[1]}, com.args[2]);
    | GraphCommand.Operation.DeleteNode =>
        VAR deleteEdgeOps: BOOLEAN;
        BEGIN
          delta.nodeDelta.delete(
            Node.T{com.args[0], com.args[1]}, deleteEdgeOps);
          IF deleteEdgeOps THEN
            delta.edgeDelta.deleteNode(Node.T{com.args[0], com.args[1]});
          END;
        END;
    | GraphCommand.Operation.PutNodeLabel =>
        delta.nodeDelta.putLabel(
          Node.T{com.args[0], com.args[1]}, com.args[2]);
    | GraphCommand.Operation.PutAttribute =>
        delta.nodeDelta.putAttribute(Node.T{com.args[0], com.args[1]},
                                     com.args[2], com.from, com.text)
    | GraphCommand.Operation.DeleteAttribute =>
        delta.nodeDelta.truncateAttribute(
          Node.T{com.args[0], com.args[1]}, com.args[2], 0)
    | GraphCommand.Operation.TruncateAttribute =>
        delta.nodeDelta.truncateAttribute(
          Node.T{com.args[0], com.args[1]}, com.args[2], com.args[3])
    | GraphCommand.Operation.PutIndex =>
        delta.nodeDelta.putIndex(
          Node.T{com.args[0], com.args[1]}, com.args[2], com.text)
    | GraphCommand.Operation.DeleteIndex =>
        delta.nodeDelta.deleteIndex(
          Node.T{com.args[0], com.args[1]}, com.args[2], com.text)
    | GraphCommand.Operation.CreateEdge =>
        IF delta.nodeDelta.notifyEdgeOp(Node.T{com.args[0], com.args[1]},
                                        Node.T{com.args[2], com.args[3]}) THEN
          delta.edgeDelta.create(
            Node.T{com.args[0], com.args[1]},
            Node.T{com.args[2], com.args[3]}, com.args[4]);
        END;
    | GraphCommand.Operation.DeleteEdge =>
        IF delta.nodeDelta.notifyEdgeOp(Node.T{com.args[0], com.args[1]},
                                        Node.T{com.args[2], com.args[3]}) THEN
          delta.edgeDelta.delete(
            Node.T{com.args[0], com.args[1]},
            Node.T{com.args[2], com.args[3]}, com.args[4]);
        END;
    ELSE
    END;
  END AddCommand;

PROCEDURE Costs (delta: T): CARDINAL RAISES {} =
  BEGIN
    RETURN delta.nodeDelta.costs() + delta.edgeDelta.costs();
  END Costs;

PROCEDURE Loop (delta: T) =
  BEGIN
    delta.nodeDelta.loop();
    delta.loopNodeDelta := TRUE;
  END Loop;

PROCEDURE GetNextCommand (delta: T; VAR c: GraphCommand.T; VAR ok: BOOLEAN) =
  BEGIN
    IF delta.loopNodeDelta THEN
      (* get a node delta command *)
      IF NOT delta.nodeDelta.getNext(c) THEN
        (* node delta is completely traversed, get first edge delta
           command *)
        delta.loopNodeDelta := FALSE;
        delta.edgeDelta.loop();
        ok := delta.edgeDelta.getNext(c);
      ELSE
        ok := TRUE;
      END;
    ELSE
      (* get next edge delta command *)
      ok := delta.edgeDelta.getNext(c);
    END;
  END GetNextCommand;

PROCEDURE ReverseLoop (delta: T) =
  BEGIN
    delta.edgeDelta.loop();
    delta.loopNodeDelta := FALSE;
  END ReverseLoop;

PROCEDURE GetPrevCommand (delta: T; VAR c: GraphCommand.T; VAR ok: BOOLEAN) =
  BEGIN
    IF NOT delta.loopNodeDelta THEN
      (* get next edge delta command *)
      IF NOT delta.edgeDelta.getNext(c) THEN
        (* edge delta is completely traversed, get first node delta
           command *)
        delta.loopNodeDelta := TRUE;
        delta.nodeDelta.reverseLoop();
        ok := delta.nodeDelta.getPrev(c);
      ELSE
        ok := TRUE;
      END;
    ELSE
      (* get a node delta command *)
      ok := delta.nodeDelta.getPrev(c);
    END;
  END GetPrevCommand;

PROCEDURE New (): T =
  BEGIN
    IF EmptyFVDList() THEN RETURN NEW(T); ELSE RETURN PopFVDList(); END;
  END New;

PROCEDURE Free (delta: T) =
  BEGIN
    EdgeDelta.Dispose(delta.edgeDelta);
    NodeDelta.Dispose(delta.nodeDelta);
    PushFVDList(delta);
  END Free;

(* List of free OptVolatileDelta.Ts *)

VAR
  FreeVDList         : T        := NIL;
  FreeListSz, MaxFree: CARDINAL := 0;

PROCEDURE EmptyFVDList (): BOOLEAN =
  BEGIN
    RETURN FreeVDList = NIL;
  END EmptyFVDList;

PROCEDURE PopFVDList (): T =
  VAR d: T;
  BEGIN
    <* ASSERT FreeVDList # NIL *>
    d := FreeVDList;
    FreeVDList := FreeVDList.next;
    DEC(FreeListSz);
    RETURN d;
  END PopFVDList;

PROCEDURE PushFVDList (d: T) =
  BEGIN
    d.next := FreeVDList;
    FreeVDList := d;
    INC(FreeListSz);
    IF Variant.FreeMemoryListLog > 0 THEN
      IF MaxFree < FreeListSz THEN
        MaxFree := FreeListSz;
        IF MaxFree MOD Variant.FreeMemoryListLog = 0 THEN
          Journal.Add("VolatileDelta free memory list " & Fmt.Int(MaxFree));
        END;
      END;
    END;
  END PushFVDList;


BEGIN
END VolatileDelta.
