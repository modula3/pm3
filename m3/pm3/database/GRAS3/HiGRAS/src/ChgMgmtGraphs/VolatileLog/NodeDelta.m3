MODULE NodeDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.5  1998/08/27 12:25:52  roland
    Bugfix: Costs must check if AttributeDeltas exist.

    Revision 1.4  1998/05/19 10:18:04  roland
    Support for log-groups implemented.

    Revision 1.3  1998/01/21 12:34:07  roland
    Bugfixes in GetPrev and GetNext.

    Revision 1.2  1997/07/21 10:39:36  roland
    A few bug-fixes and implementation of free memory list logging.

    Revision 1.1  1997/05/30 07:51:47  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

IMPORT Node, GraphCommand, AttributeDelta, AttributeDeltaTbl,
       IndexAttributeDelta, IndexAttributeDeltaTbl, NodeDeltaInfo,
       NodeDeltaInfoTbl;
IMPORT Variant, Journal, Fmt;

TYPE
  IterationState = {None, DeletedNodes, CreatedNodes, Operations};

  SubIterationState =
    {Get, AttributeGet, AttributeLoop, IndexGet, IndexLoop};

  NodeOperation = {Create, Delete, Modify};
REVEAL
  T = Public BRANDED OBJECT
        nextFree  : T;
        nodeStates: NodeDeltaInfoTbl.T;
        forward   : BOOLEAN;
        (* all remaining is iteration information *)
        itState   : IterationState;
        itSubState: SubIterationState;
        infoIt    : NodeDeltaInfoTbl.Iterator;
        adIt      : AttributeDeltaTbl.Iterator;
        idIt      : IndexAttributeDeltaTbl.Iterator;
        adelta    : AttributeDelta.T;
        idelta    : IndexAttributeDelta.T;
      OVERRIDES
        init              := Init;
        create            := Create;
        delete            := Delete;
        putLabel          := PutLabel;
        putAttribute      := PutAttribute;
        truncateAttribute := TruncateAttribute;
        putIndex          := PutIndex;
        deleteIndex       := DeleteIndex;
        notifyEdgeOp      := NotifyEdgeOp;
        costs             := Costs;
        loop              := Loop;
        getNext           := GetNext;
        reverseLoop       := ReverseLoop;
        getPrev           := GetPrev;
      END;

PROCEDURE Init (nd: T; forward: BOOLEAN): T =
  BEGIN
    nd.nodeStates := NEW(NodeDeltaInfoTbl.Default).init();
    nd.itState := IterationState.None;
    nd.forward := forward;
    nd.nextFree  := NIL;
    nd.itState   := IterationState.None;
    nd.itSubState:= SubIterationState.Get;
    nd.infoIt    := NIL;
    nd.adIt      := NIL;
    nd.idIt      := NIL;
    nd.adelta    := NIL;
    nd.idelta    := NIL;
    RETURN nd;
  END Init;

PROCEDURE Create (nd: T; READONLY node: Node.T; label: CARDINAL) =
  VAR
    info : NodeDeltaInfo.T;
    store: BOOLEAN         := FALSE;
  BEGIN
    GetDeltaInfo(nd, node, info, store);
    IF nd.forward THEN
      Step(info.state, NodeOperation.Create);
    ELSE
      Step(info.state, NodeOperation.Delete);
    END;
    info.label := label;
    EVAL nd.nodeStates.put(node, info);
  END Create;

PROCEDURE Delete (nd: T; READONLY node: Node.T; VAR deleteEdgeOps: BOOLEAN) =
  VAR
    info : NodeDeltaInfo.T;
    store: BOOLEAN         := FALSE;
  BEGIN
    deleteEdgeOps := FALSE;
    GetDeltaInfo(nd, node, info, store);
    IF nd.forward THEN
      IF info.state IN NodeDeltaInfo.ExistingStates THEN
        (* remove everything logged so far for this node *)
        DisposeAttributeDeltas(info.attributeDeltas);
        DisposeIndexDeltas(info.indexDeltas);
        info.changedLabel := FALSE;
        deleteEdgeOps := TRUE;
      END;
      Step(info.state, NodeOperation.Delete);
    ELSE
      Step(info.state, NodeOperation.Create);
    END;
    EVAL nd.nodeStates.put(node, info);
  END Delete;

PROCEDURE PutLabel (nd: T; node: Node.T; label: CARDINAL) =
  VAR
    info : NodeDeltaInfo.T;
    store: BOOLEAN         := FALSE;
  BEGIN
    GetDeltaInfo(nd, node, info, store);
    Step(info.state, NodeOperation.Modify);
    (* log only, if this is a forward delta or the node wasn't created in
       this delta *)
    IF nd.forward OR NOT info.state IN NodeDeltaInfo.CreatedStates THEN
      info.label := label;
      IF NOT info.state IN NodeDeltaInfo.CreatedStates THEN
        info.changedLabel := TRUE;
      END;
      store := TRUE;
    END;
    IF store THEN EVAL nd.nodeStates.put(node, info) END;
  END PutLabel;

PROCEDURE PutAttribute (nd   : T;
                        node : Node.T;
                        attr : CARDINAL;
                        start: CARDINAL;
                        value: TEXT      ) =
  VAR
    info : NodeDeltaInfo.T;
    store: BOOLEAN          := FALSE;
    delta: AttributeDelta.T;
  BEGIN
    GetDeltaInfo(nd, node, info, store);
    Step(info.state, NodeOperation.Modify);
    (* log only, if this is a forward delta or the node wasn't created in
       this delta *)
    IF nd.forward OR NOT info.state IN NodeDeltaInfo.CreatedStates THEN
      GetAttributeDelta(node, attr, nd.forward, info, store, delta);
      delta.putAttribute(start, value);
    END;
    IF store THEN EVAL nd.nodeStates.put(node, info); END;
  END PutAttribute;

PROCEDURE TruncateAttribute (nd    : T;
                             node  : Node.T;
                             attr  : CARDINAL;
                             length: CARDINAL  ) =
  VAR
    info : NodeDeltaInfo.T;
    store: BOOLEAN          := FALSE;
    delta: AttributeDelta.T;
  BEGIN
    GetDeltaInfo(nd, node, info, store);
    Step(info.state, NodeOperation.Modify);
    (* log only, if this is a forward delta or the node wasn't created in
       this delta *)
    IF nd.forward OR NOT info.state IN NodeDeltaInfo.CreatedStates THEN
      GetAttributeDelta(node, attr, nd.forward, info, store, delta);
      delta.truncateAttribute(length);
    END;
    IF store THEN EVAL nd.nodeStates.put(node, info); END;
  END TruncateAttribute;

PROCEDURE PutIndex (         nd   : T;
                    READONLY node : Node.T;
                             ind  : CARDINAL;
                             value: TEXT      ) =
  VAR
    info : NodeDeltaInfo.T;
    store: BOOLEAN               := FALSE;
    delta: IndexAttributeDelta.T;
  BEGIN
    GetDeltaInfo(nd, node, info, store);
    Step(info.state, NodeOperation.Modify);
    (* log only, if this is a forward delta or the node wasn't created in
       this delta *)
    IF nd.forward OR NOT info.state IN NodeDeltaInfo.CreatedStates THEN
      GetIndexDelta(node, ind, nd.forward, info, store, delta);
      delta.put(value);
    END;
    IF store THEN EVAL nd.nodeStates.put(node, info); END;
  END PutIndex;

PROCEDURE DeleteIndex (         nd   : T;
                       READONLY node : Node.T;
                                ind  : CARDINAL;
                                value: TEXT      ) =
  VAR
    info : NodeDeltaInfo.T;
    store: BOOLEAN               := FALSE;
    delta: IndexAttributeDelta.T;
  BEGIN
    GetDeltaInfo(nd, node, info, store);
    Step(info.state, NodeOperation.Modify);
    (* log only, if this is a forward delta or the node wasn't created in
       this delta *)
    IF nd.forward OR NOT info.state IN NodeDeltaInfo.CreatedStates THEN
      GetIndexDelta(node, ind, nd.forward, info, store, delta);
      delta.delete(value);
    END;
    IF store THEN EVAL nd.nodeStates.put(node, info); END;
  END DeleteIndex;

PROCEDURE NotifyEdgeOp (nd: T; source, target: Node.T): BOOLEAN =
  VAR
    tinfo, sinfo  : NodeDeltaInfo.T;
    sstore, tstore: BOOLEAN         := FALSE;
  BEGIN
    GetDeltaInfo(nd, source, sinfo, sstore);
    GetDeltaInfo(nd, target, tinfo, tstore);
    Step(sinfo.state, NodeOperation.Modify);
    Step(tinfo.state, NodeOperation.Modify);
    IF sstore THEN EVAL nd.nodeStates.put(source, sinfo); END;
    IF tstore THEN EVAL nd.nodeStates.put(target, tinfo); END;
    (* log only, if this is a forward delta or neither source nor target
       were created in this delta *)
    RETURN
      nd.forward OR NOT (sinfo.state IN NodeDeltaInfo.CreatedStates
                           OR tinfo.state IN NodeDeltaInfo.CreatedStates);
  END NotifyEdgeOp;

PROCEDURE Costs (nd: T): CARDINAL =
  VAR
    costs: CARDINAL                        := 0;
    nsit : NodeDeltaInfoTbl.Iterator;
    node : Node.T;
    info : NodeDeltaInfo.T;
    adit : AttributeDeltaTbl.Iterator;
    idit : IndexAttributeDeltaTbl.Iterator;
    no   : CARDINAL;
    ad   : AttributeDelta.T;
    id   : IndexAttributeDelta.T;
  BEGIN
    nsit := nd.nodeStates.iterate();
    (* add costs for each node and each sub delta *)
    WHILE nsit.next(node, info) DO
      CASE info.state OF
      | NodeDeltaInfo.State.NewDeleted,
          NodeDeltaInfo.State.OldExistent => (* no costs for node ops *)
      | NodeDeltaInfo.State.OldDeleted, NodeDeltaInfo.State.NewCreated =>
          INC(costs);            (* node was deleted / created *)
      | NodeDeltaInfo.State.OldNew =>
          INC(costs, 2);         (* node was deleted and then created *)
      ELSE
      END;
      IF info.attributeDeltas # NIL THEN
        adit := info.attributeDeltas.iterate();
        WHILE adit.next(no, ad) DO costs := costs + ad.costs(); END;
      END;
      IF info.indexDeltas # NIL THEN
        idit := info.indexDeltas.iterate();
        WHILE idit.next(no, id) DO costs := costs + id.costs(); END;
      END;
    END;
    RETURN costs;
  END Costs;

PROCEDURE Loop (nd: T) =
  BEGIN
    nd.itState := IterationState.DeletedNodes;
    nd.infoIt := nd.nodeStates.iterate();
  END Loop;

PROCEDURE GetNext (nd: T; VAR com: GraphCommand.T): BOOLEAN =
  VAR
    found, finished: BOOLEAN         := FALSE;
    node           : Node.T;
    info           : NodeDeltaInfo.T;
    no             : CARDINAL;
  BEGIN
    (* Depending on iteration state and sub iteration state, the sub deltas
       are taversed.  Iteration state belongs to nd.nodeStates, the sub
       iteration state determines what kind of operations (attribute or
       index) are traversed. *)
    WHILE NOT found AND NOT finished DO
      CASE nd.itState OF
        IterationState.DeletedNodes =>
          (* first, write out all nodes that were deleted in this delta *)
          IF nd.infoIt.next(node, info) THEN
            IF (nd.forward AND info.state IN NodeDeltaInfo.DeletedStates)
                 OR (NOT nd.forward
                       AND info.state IN NodeDeltaInfo.CreatedStates) THEN
              found := TRUE;
              GraphCommand.DeleteNode(com, node);
            END;
          ELSE
            (* no more nodes to visit, next iteration through nodeStates
               concerns created nodes. *)
            nd.itState := IterationState.CreatedNodes;
            nd.infoIt := nd.nodeStates.iterate();
          END;
      | IterationState.CreatedNodes =>
          (* then, write out all nodes that were created in this delta *)
          IF nd.infoIt.next(node, info) THEN
            IF (nd.forward AND info.state IN NodeDeltaInfo.CreatedStates)
                 OR (NOT nd.forward
                       AND info.state IN NodeDeltaInfo.DeletedStates) THEN
              found := TRUE;
              GraphCommand.CreateNode(com, node, info.label);
            END;
          ELSE
            (* no more nodes to visit, next iteration through nodeStates
               concerns operations on nodes. *)
            nd.itState := IterationState.Operations;
            nd.infoIt := nd.nodeStates.iterate();
            nd.itSubState := SubIterationState.Get;
          END;
      | IterationState.Operations =>
          (* we reached the operations.  for each node, write out the
             attributes delta and the index delta. *)
          CASE nd.itSubState OF
            SubIterationState.Get =>
              (* get iterators for index and attibute operations on node *)
              IF nd.infoIt.next(node, info) THEN
                nd.adIt := NIL;
                nd.idIt := NIL;
                IF info.attributeDeltas # NIL THEN
                  (* if there are any attribute deltas, they are next *)
                  nd.adIt := info.attributeDeltas.iterate();
                  nd.itSubState := SubIterationState.AttributeGet;
                END;
                IF info.indexDeltas # NIL THEN
                  nd.idIt := info.indexDeltas.iterate();
                  IF nd.itSubState = SubIterationState.Get THEN
                    (* no attribute deltas, index deltas are next *)
                    nd.itSubState := SubIterationState.IndexGet;
                  END;
                ELSIF info.attributeDeltas = NIL THEN
                  (* this node has neither index nor attribute deltas, so
                     try the next node *)
                  nd.itSubState := SubIterationState.Get;
                END;
                (* Check if label has changed.  If so return the putLabel
                   command. *)
                IF info.changedLabel THEN
                  GraphCommand.PutNodeLabel(com, node, info.label);
                  found := TRUE;
                END;
              ELSE
                (* no more nodes to visit -> we're done *)
                finished := TRUE;
                nd.itState := IterationState.None;
              END;
          | SubIterationState.AttributeGet =>
              (* initialize next attribute delta for a loop *)
              IF nd.adIt.next(no, nd.adelta) THEN
                nd.adelta.loop();
                nd.itSubState := SubIterationState.AttributeLoop;
              ELSE
                (* no more attribute deltas for node, try index *)
                IF nd.idIt # NIL THEN
                  nd.itSubState := SubIterationState.IndexGet;
                ELSE
                  (* no index deltas, get next node *)
                  nd.itSubState := SubIterationState.Get;
                END;
              END;
          | SubIterationState.AttributeLoop =>
              IF nd.adelta.getNext(com) THEN
                found := TRUE;
              ELSE
                (* done with this attribte delta, get next *)
                nd.itSubState := SubIterationState.AttributeGet;
              END;
          | SubIterationState.IndexGet =>
              (* initialize next index delta for a loop *)
              IF nd.idIt.next(no, nd.idelta) THEN
                nd.idelta.loop();
                nd.itSubState := SubIterationState.IndexLoop;
              ELSE
                (* no more index deltas for node, try next node *)
                nd.itSubState := SubIterationState.Get;
              END;
          | SubIterationState.IndexLoop =>
              IF nd.idelta.getNext(com) THEN
                found := TRUE;
              ELSE
                (* done with this index delta, get next *)
                nd.itSubState := SubIterationState.IndexGet;
              END;
          END;
      | IterationState.None => finished := TRUE;
      END;
    END;
    RETURN found;
  END GetNext;

PROCEDURE ReverseLoop (nd: T) =
  BEGIN
    nd.itState := IterationState.Operations;
    nd.itSubState := SubIterationState.Get;
    nd.infoIt := nd.nodeStates.iterate();
  END ReverseLoop;

PROCEDURE GetPrev (nd: T; VAR com: GraphCommand.T): BOOLEAN =
  VAR
    found, finished: BOOLEAN         := FALSE;
    node           : Node.T;
    info           : NodeDeltaInfo.T;
    no             : CARDINAL;
  BEGIN
    (* Depending on iteration state and sub iteration state, the sub deltas
       are taversed.  Iteration state belongs to nd.nodeStates, the sub
       iteration state determines what kind of operations (attribute or
       index) are traversed. *)
    WHILE NOT found AND NOT finished DO
      CASE nd.itState OF
        IterationState.DeletedNodes =>
          (* write out all nodes that were deleted in this delta *)
          IF nd.infoIt.next(node, info) THEN
            IF (nd.forward AND info.state IN NodeDeltaInfo.DeletedStates)
                 OR (NOT nd.forward
                       AND info.state IN NodeDeltaInfo.CreatedStates) THEN
              found := TRUE;
              GraphCommand.DeleteNode(com, node);
            END;
          ELSE
            (* no more nodes to visit, loop ends. *)
            nd.itState := IterationState.None;
            finished := TRUE;
          END;
      | IterationState.CreatedNodes =>
          (* write out all nodes that were created in this delta *)
          IF nd.infoIt.next(node, info) THEN
            IF (nd.forward AND info.state IN NodeDeltaInfo.CreatedStates)
                 OR (NOT nd.forward
                       AND info.state IN NodeDeltaInfo.DeletedStates) THEN
              found := TRUE;
              GraphCommand.CreateNode(com, node, info.label);
            END;
          ELSE
            (* no more nodes to visit, next iteration through nodeStates
               concerns deleted nodes. *)
            nd.itState := IterationState.DeletedNodes;
            nd.infoIt := nd.nodeStates.iterate();
          END;
      | IterationState.Operations =>
          (* we reached the operations.  for each node, write out the
             attributes delta and the index delta. *)
          CASE nd.itSubState OF
            SubIterationState.Get =>
              (* get iterators for index and attibute operations on node *)
              IF nd.infoIt.next(node, info) THEN
                nd.adIt := NIL;
                nd.idIt := NIL;
                IF info.attributeDeltas # NIL THEN
                  (* if there are any attribute deltas, they are next *)
                  nd.adIt := info.attributeDeltas.iterate();
                  nd.itSubState := SubIterationState.AttributeGet;
                END;
                IF info.indexDeltas # NIL THEN
                  nd.idIt := info.indexDeltas.iterate();
                  IF nd.itSubState = SubIterationState.Get THEN
                    (* no attribute deltas, index deltas are next *)
                    nd.itSubState := SubIterationState.IndexGet;
                  END;                  
                ELSIF info.attributeDeltas = NIL THEN
                  (* this node has neither index nor attribute deltas, so
                     try the next node *)
                  nd.itSubState := SubIterationState.Get;
                END;
                (* Check if label has chenged.  If so return the putLabel
                   command. *)
                IF info.changedLabel THEN
                  GraphCommand.PutNodeLabel(com, node, info.label);
                  found := TRUE;
                END;
              ELSE
                (* no more nodes to visit -> next are created nodes *)
                nd.itState := IterationState.CreatedNodes;
                nd.infoIt := nd.nodeStates.iterate();
              END;
          | SubIterationState.AttributeGet =>
              (* initialize next attribute delta for a loop *)
              IF nd.adIt.next(no, nd.adelta) THEN
                nd.adelta.reverseLoop();
                nd.itSubState := SubIterationState.AttributeLoop;
              ELSE
                (* no more attribute deltas for node, try index *)
                IF nd.idIt # NIL THEN
                  nd.itSubState := SubIterationState.IndexGet;
                ELSE
                  (* no index deltas, get next node *)
                  nd.itSubState := SubIterationState.Get;
                END;
              END;
          | SubIterationState.AttributeLoop =>
              IF nd.adelta.getPrev(com) THEN
                found := TRUE;
              ELSE
                (* done with this attribte delta, get next *)
                nd.itSubState := SubIterationState.AttributeGet;
              END;
          | SubIterationState.IndexGet =>
              (* initialize next index delta for a loop *)
              IF nd.idIt.next(no, nd.idelta) THEN
                nd.idelta.reverseLoop();
                nd.itSubState := SubIterationState.IndexLoop;
              ELSE
                (* no more index deltas for node, try next node *)
                nd.itSubState := SubIterationState.Get;
              END;
          | SubIterationState.IndexLoop =>
              IF nd.idelta.getPrev(com) THEN
                found := TRUE;
              ELSE
                (* done with this index delta, get next *)
                nd.itSubState := SubIterationState.IndexGet;
              END;
          END;
      | IterationState.None => finished := TRUE;
      END;
    END;
    RETURN found;
  END GetPrev;


VAR FreeDeltas: T := NIL;
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

PROCEDURE DisposeAttributeDeltas (VAR ads: AttributeDeltaTbl.T) =
  VAR
    iter : AttributeDeltaTbl.Iterator;
    no   : CARDINAL;
    delta: AttributeDelta.T;
    number, check: CARDINAL := 0;
  BEGIN
    IF ads # NIL THEN
      number := ads.size();
      iter := ads.iterate();
      WHILE iter.next(no, delta) DO AttributeDelta.Dispose(delta); INC(check) END;
      ads := NIL;
      <* ASSERT check = number *>
    END;
  END DisposeAttributeDeltas;

PROCEDURE DisposeIndexDeltas (VAR ids: IndexAttributeDeltaTbl.T) =
  VAR
    iter : IndexAttributeDeltaTbl.Iterator;
    no   : CARDINAL;
    delta: IndexAttributeDelta.T;
  BEGIN
    IF ids # NIL THEN
      iter := ids.iterate();
      WHILE iter.next(no, delta) DO
        IndexAttributeDelta.Dispose(delta);
      END;
    END;
    ids := NIL;
  END DisposeIndexDeltas;

PROCEDURE Dispose (nd: T) =
  VAR
    nsit: NodeDeltaInfoTbl.Iterator;
    node: Node.T;
    info: NodeDeltaInfo.T;
  BEGIN
    nsit := nd.nodeStates.iterate();
    WHILE nsit.next(node, info) DO
      DisposeAttributeDeltas(info.attributeDeltas);
      DisposeIndexDeltas(info.indexDeltas);
    END;
    nd.nextFree := FreeDeltas;
    FreeDeltas := nd;
    INC(FreeDeltaSize);
    IF Variant.FreeMemoryListLog > 0 THEN
      IF MaxFreeDeltas < FreeDeltaSize THEN
        MaxFreeDeltas := FreeDeltaSize;
        IF MaxFreeDeltas MOD Variant.FreeMemoryListLog = 0 THEN
          Journal.Add("NodeDelta free delta list " & Fmt.Int(MaxFreeDeltas));
        END;
      END;
    END;    
  END Dispose;

PROCEDURE GetDeltaInfo (         nd     : T;
                        READONLY node   : Node.T;
                        VAR      info   : NodeDeltaInfo.T;
                        VAR      newInfo: BOOLEAN          ) =
  BEGIN
    IF NOT nd.nodeStates.get(node, info) THEN
      (* node wasn't used yet *)
      info.state := NodeDeltaInfo.State.Undefined;
      info.changedLabel := FALSE;
      info.attributeDeltas := NIL;
      info.indexDeltas := NIL;
      newInfo := TRUE;
    END;
  END GetDeltaInfo;

PROCEDURE GetAttributeDelta (    node    : Node.T;
                                 attr    : CARDINAL;
                                 forward : BOOLEAN;
                             VAR info    : NodeDeltaInfo.T;
                             VAR newTable: BOOLEAN;
                             VAR delta   : AttributeDelta.T ) =
  BEGIN
    (* check whether info has an attribute delta table *)
    IF info.attributeDeltas = NIL THEN
      newTable := TRUE;
      info.attributeDeltas := NEW(AttributeDeltaTbl.Default).init();
    END;
    (* check for a delta for attribute attr *)
    IF NOT info.attributeDeltas.get(attr, delta) THEN
      delta := AttributeDelta.New().init(node, attr, forward);
      EVAL info.attributeDeltas.put(attr, delta);
    END;
  END GetAttributeDelta;

PROCEDURE GetIndexDelta (    node    : Node.T;
                             ind     : CARDINAL;
                             forward : BOOLEAN;
                         VAR info    : NodeDeltaInfo.T;
                         VAR newTable: BOOLEAN;
                         VAR delta   : IndexAttributeDelta.T) =
  BEGIN
    (* check whether info has an index attribute delta table *)
    IF info.indexDeltas = NIL THEN
      newTable := TRUE;
      info.indexDeltas := NEW(IndexAttributeDeltaTbl.Default).init();
    END;
    (* check for a delta for index ind *)
    IF NOT info.indexDeltas.get(ind, delta) THEN
      delta := IndexAttributeDelta.New().init(node, ind, forward);
      EVAL info.indexDeltas.put(ind, delta);
    END;
  END GetIndexDelta;

PROCEDURE Step (VAR st: NodeDeltaInfo.State; op: NodeOperation) =
  BEGIN
    (* realizes the transition relation of a node automatons *)
    CASE st OF
    | NodeDeltaInfo.State.Undefined =>
        CASE op OF
        | NodeOperation.Create => st := NodeDeltaInfo.State.NewCreated;
        | NodeOperation.Delete => st := NodeDeltaInfo.State.OldDeleted;
        | NodeOperation.Modify => st := NodeDeltaInfo.State.OldExistent;
        END;
    | NodeDeltaInfo.State.OldExistent =>
        CASE op OF
        | NodeOperation.Create => <* ASSERT FALSE *>
        | NodeOperation.Delete => st := NodeDeltaInfo.State.OldDeleted;
        | NodeOperation.Modify => st := NodeDeltaInfo.State.OldExistent;
        END;
    | NodeDeltaInfo.State.OldDeleted =>
        CASE op OF
        | NodeOperation.Create => st := NodeDeltaInfo.State.OldNew;
        | NodeOperation.Delete => <* ASSERT FALSE *>
        | NodeOperation.Modify => <* ASSERT FALSE *>
        END;
    | NodeDeltaInfo.State.OldNew =>
        CASE op OF
        | NodeOperation.Create => <* ASSERT FALSE *>
        | NodeOperation.Delete => st := NodeDeltaInfo.State.OldDeleted;
        | NodeOperation.Modify => st := NodeDeltaInfo.State.OldNew;
        END;
    | NodeDeltaInfo.State.NewCreated =>
        CASE op OF
        | NodeOperation.Create => <* ASSERT FALSE *>
        | NodeOperation.Delete => st := NodeDeltaInfo.State.NewDeleted;
        | NodeOperation.Modify => st := NodeDeltaInfo.State.NewCreated;
        END;
    | NodeDeltaInfo.State.NewDeleted =>
        CASE op OF
        | NodeOperation.Create => st := NodeDeltaInfo.State.NewCreated;
        | NodeOperation.Delete => <* ASSERT FALSE *>
        | NodeOperation.Modify => <* ASSERT FALSE *>
        END;
    END;
  END Step;

BEGIN
END NodeDelta.
