MODULE EdgeDelta;

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

    Revision 1.3  1997/07/21 10:39:34  roland
    A few bug-fixes and implementation of free memory list logging.

    Revision 1.2  1997/07/07 15:37:05  roland
    Much faster loop through the delta with additional linear linking of
    elements.

    Revision 1.1  1997/05/30 07:51:43  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)


IMPORT Node, GraphCommand;
IMPORT Word;
IMPORT Variant, Journal, Fmt;

(* Edge operations are kept in two hashtables.  One is the view of the
   source node and the other the view of the target node.  This is
   necessary to have efficient access to all operations concerning one
   node.  Hashkey is the node identifier, so that all operations in which
   node n is source (target) are kept at the same hash position and must be
   stored in a ordered collision list.  The order resprects all components
   of an edge operation, i.e.  source, target, and label (in that order) of
   the edge.  So, the operations in which n is source (target) are all kept
   together within the collision lists. *)

CONST HashTableSize = 503;

TYPE
  Operation = {Create, Delete};
  Incidence = {Source, Target};
  NodeArray = ARRAY Incidence OF Node.T;
  RefArray = ARRAY Incidence OF REF EdgeOperation;

  HashIndex = [0 .. HashTableSize - 1];

  EdgeOperation = RECORD
                    operation: Operation;
                    nodes    : NodeArray;
                    label    : CARDINAL;
                    next     : RefArray;
                    prev     : RefArray;
                    listNext : REF EdgeOperation;
                    listPrev : REF EdgeOperation;
                  END;

  EdgeTable = ARRAY HashIndex OF REF EdgeOperation;

REVEAL
  T = Public BRANDED OBJECT
        sources, targets: EdgeTable;
        actOp           : REF EdgeOperation;
        dcosts          : CARDINAL;
        listHead        : REF EdgeOperation;
        listTail        : REF EdgeOperation;
        nextFree        : T;
      OVERRIDES
        init       := Init;
        create     := Create;
        delete     := Delete;
        deleteNode := DeleteNode;
        costs      := Costs;
        loop       := Loop;
        getNext    := GetNext;
      END;

PROCEDURE Init (ed: T): T =
  BEGIN
    ed.sources := EdgeTable{NIL, ..};
    ed.targets := EdgeTable{NIL, ..};
    ed.nextFree := NIL;
    ed.listHead := NIL;
    ed.listTail := NIL;
    ed.actOp := NIL;
    ed.dcosts := 0;
    ed.actOp := NIL;
    RETURN ed;
  END Init;

PROCEDURE Create (ed: T; READONLY source, target: Node.T; label: CARDINAL) =
  VAR op: REF EdgeOperation;
  BEGIN
    WITH sind = Hash(source),
         tind = Hash(target)  DO
      op := NewEdgeOp();
      op^.nodes[Incidence.Source] := source;
      op^.nodes[Incidence.Target] := target;
      op^.label := label;
      op^.operation := Operation.Create;
      WITH dual = FindDual(ed.sources[sind], op, Incidence.Source) DO
        IF dual # NIL THEN
          DisposeEdgeOp(op);
          Remove(ed.sources[sind], dual, Incidence.Source);
          Remove(ed.targets[tind], dual, Incidence.Target);
          RemoveFromList(ed.listHead, ed.listTail, dual);
          DisposeEdgeOp(dual);
          DEC(ed.dcosts);
        ELSE
          Insert(ed.sources[sind], op, Incidence.Source);
          Insert(ed.targets[tind], op, Incidence.Target);
          InsertInList(ed.listHead, ed.listTail, op);
          INC(ed.dcosts);
        END;
      END;
    END;
  END Create;

PROCEDURE Delete (ed: T; READONLY source, target: Node.T; label: CARDINAL) =
  VAR op: REF EdgeOperation;
  BEGIN
    WITH sind = Hash(source),
         tind = Hash(target)  DO
      op := NewEdgeOp();
      op^.nodes[Incidence.Source] := source;
      op^.nodes[Incidence.Target] := target;
      op^.label := label;
      op^.operation := Operation.Delete;
      WITH dual = FindDual(ed.sources[sind], op, Incidence.Source) DO
        (* dual operations neutralize each other *)
        IF dual # NIL THEN
          DisposeEdgeOp(op);
          Remove(ed.sources[sind], dual, Incidence.Source);
          Remove(ed.targets[tind], dual, Incidence.Target);
          RemoveFromList(ed.listHead, ed.listTail, dual);
          DisposeEdgeOp(dual);
          DEC(ed.dcosts);
        ELSE
          Insert(ed.sources[sind], op, Incidence.Source);
          Insert(ed.targets[tind], op, Incidence.Target);
          InsertInList(ed.listHead, ed.listTail, op);
          INC(ed.dcosts);
        END;
      END;
    END;
  END Delete;

PROCEDURE DeleteNode (ed: T; node: Node.T) =
  VAR
    op, h        : REF EdgeOperation;
    ind, otherind: HashIndex;
  BEGIN
    ind := Hash(node);
    (* remove all operations which have node as source *)
    op := FindNode(ed.sources[ind], node, Incidence.Source);
    WHILE op # NIL AND op^.nodes[Incidence.Source] = node DO
      otherind := Hash(op^.nodes[Incidence.Target]);
      h := op;
      op := op^.next[Incidence.Source];
      Remove(ed.sources[ind], h, Incidence.Source);
      Remove(ed.targets[otherind], h, Incidence.Target);
      RemoveFromList(ed.listHead, ed.listTail, h);
      DisposeEdgeOp(h);
      DEC(ed.dcosts);
    END;
    (* remove all operations which have node as target *)
    op := FindNode(ed.targets[ind], node, Incidence.Target);
    WHILE op # NIL AND op^.nodes[Incidence.Target] = node DO
      otherind := Hash(op^.nodes[Incidence.Source]);
      h := op;
      op := op^.next[Incidence.Target];
      Remove(ed.targets[ind], h, Incidence.Target);
      Remove(ed.sources[otherind], h, Incidence.Source);
      RemoveFromList(ed.listHead, ed.listTail, h);
      DisposeEdgeOp(h);
      DEC(ed.dcosts);
    END;
  END DeleteNode;

PROCEDURE Costs (ed: T): CARDINAL =
  BEGIN
    RETURN ed.dcosts;
  END Costs;

PROCEDURE Loop (ed: T) =
  BEGIN
    ed.actOp := ed.listHead;
  END Loop;

PROCEDURE GetNext (ed: T; VAR com: GraphCommand.T): BOOLEAN =
  BEGIN
    IF ed.actOp = NIL THEN
      RETURN FALSE;
    ELSE
      IF ed.actOp^.operation = Operation.Create THEN
	GraphCommand.CreateEdge(
	  com, ed.actOp^.nodes[Incidence.Source],
	  ed.actOp^.nodes[Incidence.Target], ed.actOp^.label);
      ELSE
	GraphCommand.DeleteEdge(
	  com, ed.actOp^.nodes[Incidence.Source],
	  ed.actOp^.nodes[Incidence.Target], ed.actOp^.label);
      END;
      ed.actOp := ed.actOp^.listNext;
      RETURN TRUE;
    END;
  END GetNext;


(* Internal *)

PROCEDURE NodeLess (READONLY a, b: Node.T): BOOLEAN =
  BEGIN
    IF a.graph < b.graph THEN
      RETURN TRUE;
    ELSIF a.graph = b.graph THEN
      RETURN a.entity < b.entity;
    ELSE
      RETURN TRUE;
    END;
  END NodeLess;

PROCEDURE OpLess (a, b: REF EdgeOperation; inc: Incidence): BOOLEAN =
  BEGIN
    IF NodeLess(a^.nodes[inc], b^.nodes[inc]) THEN
      RETURN TRUE;
    ELSIF a^.nodes[inc] = b^.nodes[inc] THEN
      WITH oinc = VAL(1 - ORD(inc), Incidence) DO
        IF NodeLess(a^.nodes[oinc], b^.nodes[oinc]) THEN
          RETURN TRUE;
        ELSIF a^.nodes[oinc] = b^.nodes[oinc] THEN
          IF a^.label < b.label THEN RETURN TRUE; ELSE RETURN FALSE; END;
        ELSE
          RETURN FALSE;
        END;
      END;
    ELSE
      RETURN FALSE;
    END;
  END OpLess;

PROCEDURE Dual (a, b: REF EdgeOperation): BOOLEAN =
  (* Operations are dual if source, target, and label fields are equal and
     the operation fields are not. *)
  BEGIN
    RETURN a.nodes = b.nodes AND a.label = b.label
             AND a.operation # b.operation;
  END Dual;

PROCEDURE FindDual (list: REF EdgeOperation;
                    op  : REF EdgeOperation;
                    inc : Incidence          ): REF EdgeOperation =
  (* If a dual (create <-> delete) operation to the one given by the exists
     in list, it will be removed an the result is TRUE.  Otherwise the
     result is FALSE. *)
  VAR act: REF EdgeOperation;
  BEGIN
    act := list;
    (* we only have to search the list as long as the actual element is
       greater or equal to op. *)
    WHILE act # NIL AND NOT OpLess(act, op, inc) DO
      IF Dual(act, op) THEN
        (* remove actual *)
        RETURN act;
      ELSE
        (* search further *)
        act := act^.next[inc];
      END;
    END;
    (* nothing found *)
    RETURN NIL;
  END FindDual;

PROCEDURE FindNode (         list: REF EdgeOperation;
                    READONLY node: Node.T;
                             inc : Incidence          ):
  REF EdgeOperation =
  VAR act: REF EdgeOperation;
  BEGIN
    act := list;
    (* we only have to search the list as long as the node of the actual
       element is greater or equal to node. *)
    WHILE act # NIL AND NOT NodeLess(act^.nodes[inc], node) DO
      IF node = act^.nodes[inc] THEN
        RETURN act;
      ELSE
        (* search further *)
        act := act^.next[inc];
      END;
    END;
    (* nothing found *)
    RETURN NIL;
  END FindNode;

PROCEDURE Insert (VAR list: REF EdgeOperation;
                      op  : REF EdgeOperation;
                      inc : Incidence          ) =
  VAR act, prev: REF EdgeOperation;
  BEGIN
    IF list = NIL THEN
      (* empty list *)
      list := op;
      op^.next[inc] := NIL;
      op^.prev[inc] := NIL;
    ELSE
      act := list;
      prev := NIL;
      WHILE act # NIL AND OpLess(act, op, inc) DO
        prev := act;
        act := act^.next[inc];
      END;
      IF act = list THEN
        (* insert as new first element *)
        op^.next[inc] := list;
        op^.prev[inc] := NIL;
        list^.prev[inc] := op;
        list := op;
      ELSIF act = NIL THEN
        (* insert as last element *)
        prev^.next[inc] := op;
        op^.next[inc] := NIL;
        op.prev[inc] := prev;
      ELSE
        op^.next[inc] := act;
        op^.prev[inc] := prev;
        prev^.next[inc] := op;
        act^.prev[inc] := op;
      END;
    END;
  END Insert;

PROCEDURE Remove (VAR list: REF EdgeOperation;
                      op  : REF EdgeOperation;
                      inc : Incidence          ) =
  BEGIN
    IF op = list THEN
      (* remove from beginning of list *)
      list := op^.next[inc];
    END;
    IF op^.next[inc] # NIL THEN
      op^.next[inc]^.prev[inc] := op^.prev[inc];
    END;
    IF op^.prev[inc] # NIL THEN
      op^.prev[inc]^.next[inc] := op^.next[inc];
    END;
  END Remove;

PROCEDURE InsertInList(VAR list, tail: REF EdgeOperation; element: REF EdgeOperation) =
  BEGIN
    (* insert as head of list *)
    IF list = NIL THEN
      list := element;
      tail := element;
      element^.listNext := NIL;
      element^.listPrev := NIL;
    ELSE
      element^.listNext := list;
      element^.listPrev := NIL;
      list^.listPrev := element;
      list := element;
    END;
  END InsertInList;

PROCEDURE RemoveFromList(VAR list, tail: REF EdgeOperation; element: REF EdgeOperation) =
  BEGIN
    IF element^.listNext # NIL THEN
      element^.listNext^.listPrev := element^.listPrev;
    ELSE
      tail := element^.listPrev;
    END;
    IF element^.listPrev # NIL THEN
      element^.listPrev^.listNext := element^.listNext;
    ELSE
      (* element is head of list *)
      list := element^.listNext;
    END;
  END RemoveFromList;
  
PROCEDURE Hash (node: Node.T): HashIndex =
  CONST One31 = 16_EFFF;
  BEGIN
    (* (graph*(2^8) + entity) MOD HashTableSize *)
    RETURN
      Word.Mod(Word.Plus(Word.And(One31, Word.LeftShift(node.graph, 8)),
                         node.entity), HashTableSize);
  END Hash;

VAR
  FreeDeltas : T                 := NIL;
  FreeDeltaSize, MaxFreeDeltas: CARDINAL := 0;
  FreeEdgeOps: REF EdgeOperation := NIL;
  FreeEdgeOpSize, MaxFreeEdgeOps: CARDINAL := 0;

CONST
  NullEdgeOp = EdgeOperation{
                 operation := Operation.Create, nodes :=
                 NodeArray{Node.T{0, 0}, ..}, label := 0, next :=
                 RefArray{NIL, NIL}, prev := RefArray{NIL, NIL},
                 listPrev := NIL, listNext := NIL};


PROCEDURE NewEdgeOp (): REF EdgeOperation =
  VAR res: REF EdgeOperation := FreeEdgeOps;
  BEGIN
    IF res = NIL THEN
      res := NEW(REF EdgeOperation);
    ELSE
      FreeEdgeOps := FreeEdgeOps.listNext;
      DEC(FreeEdgeOpSize);
    END;
    res^ := NullEdgeOp;
    RETURN res;
  END NewEdgeOp;

PROCEDURE DisposeEdgeOp (op: REF EdgeOperation) =
  BEGIN
    op.listNext := FreeEdgeOps;
    FreeEdgeOps := op;
    INC(FreeEdgeOpSize);
    IF Variant.FreeMemoryListLog > 0 THEN
      IF MaxFreeEdgeOps < FreeEdgeOpSize THEN
        MaxFreeEdgeOps := FreeEdgeOpSize;
        IF MaxFreeEdgeOps MOD Variant.FreeMemoryListLog = 0 THEN
          Journal.Add("EdgeDelta free edge ops " & Fmt.Int(MaxFreeEdgeOps));
        END;
      END;
    END;
  END DisposeEdgeOp;

PROCEDURE New (): T =
  VAR res: T := FreeDeltas;
  BEGIN
    IF res = NIL THEN
      RETURN Init(NEW(T));
    ELSE
      FreeDeltas := FreeDeltas.nextFree;
      DEC(FreeDeltaSize);
      RETURN Init(res);
    END;
  END New;

PROCEDURE Dispose (ed: T) =
  BEGIN
    IF ed.listHead # NIL THEN      
      (* dispose all entries at once *)
      ed.listTail.listNext := FreeEdgeOps;
      FreeEdgeOps := ed.listHead;
      FreeEdgeOpSize := FreeEdgeOpSize + ed.dcosts;
      IF Variant.FreeMemoryListLog > 0 THEN
        IF MaxFreeEdgeOps < FreeEdgeOpSize THEN
          MaxFreeEdgeOps := FreeEdgeOpSize;
          IF MaxFreeEdgeOps MOD Variant.FreeMemoryListLog = 0 THEN
            Journal.Add("EdgeDelta free edge ops " & Fmt.Int(MaxFreeEdgeOps));
          END;
        END;
      END;
    END;
    ed.nextFree := FreeDeltas;
    FreeDeltas := ed;
    INC(FreeDeltaSize);
    IF Variant.FreeMemoryListLog > 0 THEN
      IF MaxFreeDeltas < FreeDeltaSize THEN
        MaxFreeDeltas := FreeDeltaSize;
        IF MaxFreeDeltas MOD Variant.FreeMemoryListLog = 0 THEN
          Journal.Add("EdgeDelta free delta list " & Fmt.Int(MaxFreeDeltas));
        END;
      END;
    END;
  END Dispose;

BEGIN
END EdgeDelta.
