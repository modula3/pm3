(************************************************************************)
(**									*)
(**   GRAS - A Graph-Oriented Database System for SE Applications	*)
(**   Copyright (C) 1987-1992  Lehrstuhl Informatik III, RWTH Aachen	*)
(**                                                                     *)
(**   This library is free software; you can redistribute it and/or	*)
(**   modify it under the terms of the GNU Library General Public	*)
(**   License as published by the Free Software Foundation; either	*)
(**   version 2 of the License, or (at your option) any later version.	*)
(**    								        *)
(**   This library is distributed in the hope that it will be useful,	*)
(**   but WITHOUT ANY WARRANTY; without even the implied warranty of	*)
(**   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(**   Library General Public License for more details.			*)
(**    								        *)
(**   You should have received a copy of the GNU Library General Public *)
(**   License along with this library; if not, write to the Free	*)
(**   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.*)
(**    								        *)
(**   Contact Adresses:						        *)
(**    								        *)
(**    Roland Baumann,                                                  *)
(**    Lehrstuhl f"ur Informatik III,					*)
(**    University of Technology Aachen (RWTH Aachen),			*)
(**    Ahornstr. 55,							*)
(**    D-52074 Aachen							*)
(**    								        *)
(**   Email to								*)
(**    							 	        *)
(**	roland@i3.informatik.rwth-aachen.de				*)
(**									*)
(************************************************************************)

MODULE Simple2 EXPORTS Main;

IMPORT PersistentGraph, PersistentGraphPool, PersistentGraphSystem;
IMPORT ErrorSupport, Access, PageFile, Node;
IMPORT Event, Trigger, Action, RuleEngine, ContextSet, EventType;
IMPORT GraphEvents, GraphEventPattern;
IMPORT IO, Process, Fmt, Text;

(* A small example showing how to use the Eventhandling mechanism of GRAS.
   A binary tree is build up by a recursive procedure and deleted with the
   help of an 'action'. *)

CONST
  (* some edgelabels *)
  LEFT_TREE  = 1;
  RIGHT_TREE = 2;

  (* the nodelabel of all nodes of the tree *)
  TREE_NODE = 1;

  (* depth of the tree *)
  DEPTH_OF_TREE = 10;

  INDEX_ATT = 1;

VAR
  pool                     : PersistentGraphPool.T;
  graph                    : PersistentGraph.T;
  root                     : Node.T;
  extName                  : TEXT;
  noOfNodes                : CARDINAL              := 0;
  noOfEdges                : CARDINAL              := 0;
  noOfDelNodes             : CARDINAL              := 0;
  ltId, rtId               : CARDINAL;
  leftTrigger, rightTrigger: Trigger.T;
  found                    : BOOLEAN;

PROCEDURE CreateSubtree (graph : PersistentGraph.T;
                         parent: Node.T;
                         depth : CARDINAL           )
  RAISES {Access.Locked, PersistentGraph.NotOwner,
          PersistentGraph.InternalError, PersistentGraph.NodeNotFound} =
  VAR leftson, rightson: Node.T;
  BEGIN
    IF depth > 0 THEN
      leftson := graph.createNodeNumber(parent);
      leftson := graph.createNode(TREE_NODE);
      graph.createEdge(parent, leftson, LEFT_TREE);
      INC(noOfNodes);
      INC(noOfEdges);
      CreateSubtree(graph, leftson, depth - 1);

      rightson := graph.createNodeNumber(parent);
      rightson := graph.createNode(TREE_NODE);
      graph.createEdge(parent, rightson, RIGHT_TREE);
      INC(noOfNodes);
      INC(noOfEdges);
      CreateSubtree(graph, rightson, depth - 1);
    END;
  END CreateSubtree;

(* Actionprocedure which delete one node and the two outgoing edges (if
   existent). *)
<* FATAL EventType.Mismatch, EventType.Unknown *>

PROCEDURE DeleteSubtree (             event   : Event.T;
                         <* UNUSED *> context : ContextSet.T;
                         <* UNUSED *> local   : BOOLEAN;
                         <* UNUSED *> userdata: REFANY        ) =
  BEGIN
    TRY
      WITH graph  = NARROW(GraphEvents.GetGraph(event), PersistentGraph.T),
           target = GraphEvents.GetTargetNode(event)                        DO
        graph.deleteNodeNoInfo(target);
        INC(noOfDelNodes);
      END;
    EXCEPT
    | PersistentGraph.InternalError (e) =>
        IO.Put("Graph: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
    | Access.Locked => IO.Put("Deadlock!\n");
    | PersistentGraph.NotOwner => IO.Put("Graph is not owner of node!\n");
    END;
  END DeleteSubtree;

BEGIN
  TRY

    (* Initialize GRAS. *)
    PersistentGraphSystem.Login("/tmp");

    (* Open graphpool with name 'ExamplePool' *)
    IF NOT PersistentGraphSystem.ExistsPool("ExamplePool") THEN
      IO.Put("Graphpool 'ExamplePool' does not exists! \n");
      IO.Put("Use 'Simple1' to create it. \n");
    ELSE
      TRY
        pool :=
          NEW(PersistentGraphPool.T).open(
            "ExamplePool", Access.Mode.ReadWriteExclusive, new := FALSE);
        IO.Put("Graphpool 'ExamplePool' opened. \n");
      EXCEPT
        PageFile.NoAccess (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      | Access.Denied (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      END;

      TRY
        (* Open graph with name 'ExampleGraph'. *)
        graph := NEW(PersistentGraph.T).open(
                   pool, "ExampleGraph",
                   PersistentGraph.AccessMode.ReadWriteExclusive,
                   new := FALSE, local := FALSE, errorChecks := FALSE);
        IO.Put("Graph 'ExampleGraph' opened. \n");
      EXCEPT
        (* Exceptions raised by PersistentGraphPool.open *)
        PageFile.NoAccess (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      | Access.Denied (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      | PersistentGraph.NotExistent =>
          IO.Put("Graph not existent!\n");
          Process.Exit(1);
      | PersistentGraph.InUse =>
          IO.Put("Graph in use!\n");
          Process.Exit(1);
      END;


      pool.beginTransaction();

      (* Create the rootnode. *)
      root := graph.createNodeNumber(Node.T{graph.number(), 0});
      root := graph.createNode(TREE_NODE);
      INC(noOfNodes);

      (* Build up the tree. *)
      CreateSubtree(graph, root, DEPTH_OF_TREE);

      IO.Put("\nCreated: " & Fmt.Int(noOfNodes) & " nodes and "
               & Fmt.Int(noOfEdges) & " edges. \n");

      (* test external names *)
      graph.putIndex(root, INDEX_ATT, "External Name");
      extName := graph.getIndex(root, INDEX_ATT, found);
      IF found THEN
        IO.Put("Index attribute of the root node: " & extName & "\n");
        IO.Put("Length of the index attribute   : "
                 & Fmt.Int(Text.Length(extName)) & "\n");
        graph.deleteIndex(root, INDEX_ATT, extName);
      ELSE
        IO.Put("Error! No index attribute found!\n");
      END;

      (* Activate two triggers, one for LEFT_TREE and the other for
         RIGHT_TREE. *)
      WITH leftPattern = GraphEventPattern.Create(
                       GraphEvents.Operation.DeleteEdge),
           rightPattern = GraphEventPattern.Create(
                       GraphEvents.Operation.DeleteEdge),
           action = NEW(Action.Local).init(DeleteSubtree) DO
        (* specify as many attributes as possible *)
        GraphEventPattern.SetPoolName(leftPattern, "ExamplePool");
        GraphEventPattern.SetPool(leftPattern, pool);
        GraphEventPattern.SetGraphNumber(leftPattern, graph.number());
        GraphEventPattern.SetGraph(leftPattern, graph);
        GraphEventPattern.SetPreEvent(leftPattern, FALSE);
        GraphEventPattern.SetTargetNodeExists(leftPattern, TRUE);
        GraphEventPattern.SetEdgeLabel(leftPattern, LEFT_TREE);
        leftTrigger :=
          Trigger.Create(
            leftPattern, action, coupling := Trigger.CouplingMode.Immediate,
            priority := 1, inhibiting := ContextSet.Empty(),
            permitting := ContextSet.Empty());
        ltId := RuleEngine.RegisterTrigger(
                  leftTrigger, RuleEngine.Interest.Self);

        (* specify as many attributes as possible *)
        GraphEventPattern.SetPoolName(rightPattern, "ExamplePool");
        GraphEventPattern.SetPool(rightPattern, pool);
        GraphEventPattern.SetGraphNumber(rightPattern, graph.number());
        GraphEventPattern.SetGraph(rightPattern, graph);
        GraphEventPattern.SetPreEvent(rightPattern, FALSE);
        GraphEventPattern.SetTargetNodeExists(rightPattern, TRUE);
        GraphEventPattern.SetEdgeLabel(rightPattern, LEFT_TREE);
        GraphEventPattern.SetEdgeLabel(rightPattern, RIGHT_TREE);
        rightTrigger :=
          Trigger.Create(
            rightPattern, action, coupling := Trigger.CouplingMode.Immediate,
            priority := 1, inhibiting := ContextSet.Empty(),
            permitting := ContextSet.Empty());
        rtId := RuleEngine.RegisterTrigger(
                  rightTrigger, RuleEngine.Interest.Self);
      END;

      (* Delete the rootnode, then the rest of the tree is deleted
         'automatically'. *)
      graph.deleteNodeNoInfo(root);
      INC(noOfDelNodes);

      IO.Put("\nDeleted: " & Fmt.Int(noOfDelNodes)
               & " nodes and their edges. \n");

      pool.commitTransaction();
      graph.close();
      pool.close();
    END;
  EXCEPT
    PersistentGraphPool.InternalError (e) =>
      IO.Put("Pool: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
  | PersistentGraph.InternalError (e) =>
      IO.Put("Graph: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
  | PersistentGraphSystem.InternalError (e) =>
      IO.Put("System: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
  | Access.Locked => IO.Put("Deadlock!\n");
  | PersistentGraph.NotOwner => IO.Put("Graph is not owner of node!\n");
  | PersistentGraph.NodeNotFound => IO.Put("Node not found in graph!\n");
  | PersistentGraph.IndexUsed => IO.Put("Index in use!\n");
  | PersistentGraph.IndexUnused => IO.Put("Index not in use!\n");
  | PersistentGraphPool.NotInTransaction =>
      IO.Put("Commit: Not in transaction!\n");
  END;

END Simple2.
