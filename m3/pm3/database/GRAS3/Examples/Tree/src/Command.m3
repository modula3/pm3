MODULE Command;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:26  hosking
    Initial revision

    Revision 1.1  1998/01/21 14:23:34  roland
    Tree example demonstrates derived attributes, triggers, and user-recovery.

*)
(***************************************************************************)

IMPORT TypedGraphPool, TypedGraph, Node, ChgMgmtGraph, NodeSet;
IMPORT ErrorSupport, Transaction, Access;
IMPORT TreeScheme, IO;
IMPORT Event, Trigger, Action, RuleEngine, ContextSet, EventType;
IMPORT GraphEvents, GraphEventPattern, LogContexts;

PROCEDURE Init (pool: TypedGraphPool.T; graph: TypedGraph.T) =
  VAR
    replay : ContextSet.Index;
    inh    : ContextSet.T;
    tid    : CARDINAL;
    trigger: Trigger.T;
  <* FATAL ContextSet.Unknown, EventType.Mismatch, EventType.Unknown *>
  BEGIN
    replay := ContextSet.ContextIndex(
                LogContexts.Names[LogContexts.Context.Replay]);
    inh := ContextSet.Insert(ContextSet.Empty(), replay);
    (* Activate a trigger for deletion of toSOn edge. *)
    WITH pattern = GraphEventPattern.Create(
                     GraphEvents.Operation.DeleteEdge),
         action = NEW(Action.Local).init(DeleteSubtree) DO
      (* specify as many attributes as possible *)
      GraphEventPattern.SetPoolName(pattern, pool.getBaseName());
      GraphEventPattern.SetPool(pattern, pool);
      GraphEventPattern.SetGraphNumber(pattern, graph.number());
      GraphEventPattern.SetGraph(pattern, graph);
      GraphEventPattern.SetPreEvent(pattern, FALSE);
      GraphEventPattern.SetTargetNodeExists(pattern, TRUE);
      GraphEventPattern.SetEdgeLabel(pattern, TreeScheme.toSon.entity);
      trigger := Trigger.Create(pattern, action,
                                coupling := Trigger.CouplingMode.Immediate,
                                priority := 1, inhibiting := inh,
                                permitting := ContextSet.Empty());
      tid := RuleEngine.RegisterTrigger(trigger, RuleEngine.Interest.Self);
    END;
    OldRoot := FindRoot(pool, graph);
  END Init;

VAR OldRoot: Node.T;
PROCEDURE FindRoot (pool: TypedGraphPool.T; graph: TypedGraph.T): Node.T =
  VAR
    nodes: NodeSet.T;
    found: BOOLEAN;
  <* FATAL TypedGraph.CyclicEvaluation, TypedGraph.InternalError *>
  <* FATAL TypedGraph.LogError, TypedGraph.NoIndex, Access.Locked *>
  <* FATAL TypedGraph.Unknown *>
  <* FATAL TypedGraphPool.InternalError, TypedGraphPool.CardinalityError *>
  <* FATAL TypedGraphPool.NotInTransaction *>
  BEGIN
    pool.beginTransaction();
    nodes := graph.getNodesWithAttribute(TreeScheme.isRoot, "TRUE");
    pool.commitTransaction();
    IF nodes.card() = 0 THEN
      RETURN Node.T{0, 0};
    ELSE
      nodes.loop();
      OldRoot := nodes.get(found);
      RETURN OldRoot;
    END;
  END FindRoot;


PROCEDURE ExecuteCommand (    pool   : TypedGraphPool.T;
                              graph  : TypedGraph.T;
                              command: UserCommand;
                              param  : CARDINAL;
                          VAR root   : Node.T            ) RAISES {Error} =
  <* FATAL TypedGraphPool.CardinalityError, Access.Locked *>
  <* FATAL TypedGraph.NotOwner, TypedGraph.Unknown, TypedGraph.NodeNotFound *>
  <* FATAL TypedGraph.WrongType, ChgMgmtGraph.NoLog *>
  <* FATAL ChgMgmtGraph.NotInTreeMode, TypedGraph.CardinalityError *>
  VAR
    node     : Node.T;
    replaycnt: CARDINAL;

  PROCEDURE ReadAttributes (graph: TypedGraph.T; node: Node.T)
    RAISES {TypedGraph.InternalError, TypedGraph.LogError} =
    <* FATAL TypedGraph.CyclicEvaluation *>
    BEGIN
      (* access attributes to trigger re-evaluation before checkpointing *)
      EVAL graph.getAttribute(node, TreeScheme.maxDegree, 0, 1);
      EVAL graph.getAttribute(node, TreeScheme.level, 0, 1);
    END ReadAttributes;

  BEGIN
    TRY
      CASE command OF
      | UserCommand.Quit =>
          WHILE pool.getTransactionLevel() > Transaction.EnvelopeLevel DO
            pool.commitTransaction();
          END;
      | UserCommand.CreateLeaf =>
          pool.beginTransaction();
          WITH father = Node.T{graph.number(), param} DO
            IF param = 0 THEN
              (* find root *)
              node := FindRoot(pool, graph);
              IF node = Node.T{0, 0} THEN
                node := graph.createNodeNumber(father);
                node := graph.createNode(TreeScheme.type);
                graph.putAttribute(node, TreeScheme.isRoot, 0, "TRUE");
                OldRoot := node;
                root := node;
                ReadAttributes(graph, node);
                pool.commitTransaction();
                graph.setCheckpoint();
              ELSE
                OldRoot := node;
                root := OldRoot;
                pool.commitTransaction();
              END;
            ELSIF graph.existsNode(father) THEN
              node := graph.createNodeNumber(father);
              node := graph.createNode(TreeScheme.type);
              graph.createEdge(father, node, TreeScheme.toSon);
              ReadAttributes(graph, OldRoot);
              root := OldRoot;
              pool.commitTransaction();
              graph.setCheckpoint();
            ELSE
              pool.commitTransaction();
              root := OldRoot;
              RAISE Error("Father not exists");
            END;
          END;
      | UserCommand.DeleteSubtree =>
          pool.beginTransaction();
          node := Node.T{graph.number(), param};
          IF graph.existsNode(node) THEN
            graph.deleteNodeNoInfo(node);
            IF node # OldRoot THEN
              ReadAttributes(graph, OldRoot);
            ELSE
              OldRoot := Node.T{0, 0};
            END;
          ELSE
            pool.commitTransaction();
            RAISE Error("Node not exists");
          END;
          pool.commitTransaction();
          graph.setCheckpoint();
      | UserCommand.BeginTransaction => pool.beginTransaction();
      | UserCommand.CommitTransaction => pool.commitTransaction();
      | UserCommand.AbortTransaction => pool.abortTransaction();
      | UserCommand.Undo => replaycnt := 1; graph.undo(replaycnt);
      | UserCommand.Redo => replaycnt := 1; graph.redo(replaycnt);
      | UserCommand.RedoPrev => graph.redoPrev();
      | UserCommand.RedoNext => graph.redoNext();
      | UserCommand.RedoIth => graph.redoIth(param);
      | UserCommand.Backstep => replaycnt := 1; graph.backstep(replaycnt);
      | UserCommand.Forstep => replaycnt := 1; graph.forstep(replaycnt);
      | UserCommand.LabelCheckpoint => graph.setCheckpointLabel(param);
      | UserCommand.GotoCheckpoint => graph.gotoCheckpointLabel(param);
      END;
      IF OldRoot = Node.T{0, 0} THEN OldRoot := FindRoot(pool, graph); END;
      root := OldRoot;
    EXCEPT
      TypedGraph.InternalError (info) =>
        RAISE Error(ErrorSupport.ToText(info))
    | ChgMgmtGraph.InternalError (info) =>
        RAISE Error(ErrorSupport.ToText(info))
    | TypedGraphPool.InternalError (info) =>
        RAISE Error(ErrorSupport.ToText(info))
    | TypedGraph.LogError (info) => RAISE Error(ErrorSupport.ToText(info))
    | ChgMgmtGraph.LogError (info) =>
        RAISE Error(ErrorSupport.ToText(info))
    | ChgMgmtGraph.NoSuchCheckpoint => RAISE Error("No such checkpoint")
    | TypedGraphPool.NotInTransaction => RAISE Error("Not in transaction");
    END;
  END ExecuteCommand;

PROCEDURE DeleteSubtree (             event   : Event.T;
                         <* UNUSED *> context : ContextSet.T;
                         <* UNUSED *> local   : BOOLEAN;
                         <* UNUSED *> userdata: REFANY        ) =
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    TRY
      WITH graph  = NARROW(GraphEvents.GetGraph(event), TypedGraph.T),
           target = GraphEvents.GetTargetNode(event)                   DO
        graph.deleteNodeNoInfo(target);
      END;
    EXCEPT
    | TypedGraph.InternalError (e) =>
        IO.Put("Graph: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
    | TypedGraph.LogError (e) =>
        IO.Put("Graph: Log Error\n" & ErrorSupport.ToText(e) & "\n");
    | Access.Locked => IO.Put("Deadlock!\n");
    | TypedGraph.NotOwner => IO.Put("Graph is not owner of node!\n");
    END;
  END DeleteSubtree;

BEGIN
END Command.
