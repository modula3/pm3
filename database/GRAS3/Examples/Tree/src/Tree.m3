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

MODULE Tree EXPORTS Main;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:26  hosking
    Initial revision

    Revision 1.2  1998/05/19 09:38:50  roland
    Added local parameter for open operations.

    Revision 1.1  1998/01/21 14:23:37  roland
    Tree example demonstrates derived attributes, triggers, and user-recovery.

*)
(***************************************************************************)

(* This program allows to interactively manipulate a GRAS graph.  The graph
   models a tree.  The nodes of the tree can have an arbitrary number of
   successors.  A small panel displays the number of nodes in the tree, the
   root node of the tree, the height of the tree, and the maximum degree of
   a node in the tree.  The latter two values are computed by using GRAS'
   derived attribute mechanism.  The number of nodes is updated using GRAS'
   event trigger mechanism.  User interaction includes creation of leafs,
   deletion of sub-trees and the whole range of GRAS user-recovery
   commands. *)

IMPORT TypedGraph, TypedGraphPool, TypedGraphSystem, Scheme;
IMPORT ChgMgmtGraph, AttributeValue, Node, NodeSet;
IMPORT Access, PageFile, ErrorSupport, GrasParams, Transaction;
IMPORT IO, Process, Word, Text, Fmt;
IMPORT Command, TreeScheme, Panel;
IMPORT Event, Trigger, Action, RuleEngine, ContextSet, EventType;
IMPORT GraphEvents, GraphEventPattern;

CONST
  PoolName   = "TreePool";
  GraphName  = "Tree";
  SchemeName = "TreeScheme";

EXCEPTION Error(TEXT);

PROCEDURE BuildScheme (scheme: Scheme.T) RAISES {Error} =
  VAR class: Scheme.ID;
  <* FATAL Scheme.AttributeNameClash, Scheme.AttributePlacementClash *>
  BEGIN
    TRY
      class := scheme.declareNodeClass(TreeScheme.TreeNodeClass);
      TreeScheme.level :=
        scheme.declareAttribute(
          class, TreeScheme.LevelAttribute, Scheme.AttributeKind.Derived,
          Scheme.IndexProperties.Normal, AttributeValue.IntegerTypeCode,
          Scheme.ExactlyOneCard, TRUE, 4);
      TreeScheme.maxDegree :=
        scheme.declareAttribute(
          class, TreeScheme.DegreeAttribute, Scheme.AttributeKind.Derived,
          Scheme.IndexProperties.Normal, AttributeValue.IntegerTypeCode,
          Scheme.ExactlyOneCard, TRUE, 4);
      TreeScheme.isRoot :=
        scheme.declareAttribute(
          class, TreeScheme.IsRootAttribute,
          Scheme.AttributeKind.Intrinsic, Scheme.IndexProperties.Index,
          AttributeValue.TextTypeCode, Scheme.ExactlyOneCard, FALSE, 0);
      TreeScheme.toSon :=
        scheme.declareEdgeType(TreeScheme.TreeEdge, class, class,
                               Scheme.ArbitraryCard, Scheme.AtMostOneCard);
      scheme.setEvaluationFunction(
        class, TreeScheme.level, TreeScheme.LevelEval);
      scheme.setEvaluationFunction(
        class, TreeScheme.maxDegree, TreeScheme.DegreeEval);
      scheme.declareDependency(
        class, TreeScheme.level, TreeScheme.level,
        Scheme.DependencyKind.Outgoing, TreeScheme.toSon);
      scheme.declareDependency(
        class, TreeScheme.maxDegree, TreeScheme.maxDegree,
        Scheme.DependencyKind.Outgoing, TreeScheme.toSon);
      TreeScheme.type := scheme.declareNodeType(TreeScheme.TreeNodeType);
      scheme.appendNodeType(TreeScheme.type, class);
    EXCEPT
      Scheme.InternalError (info) =>
        RAISE Error("BuildScheme: " & ErrorSupport.ToText(info) & "\n");
    | Scheme.AlreadyDeclared =>
        RAISE Error("BuildScheme: Item already declared\n");
    | Scheme.NotDeclared =>
        RAISE Error("BuildScheme: Item not declared\n");
    END;
  END BuildScheme;


PROCEDURE TextToInt (t: TEXT): INTEGER =
  VAR
    res: INTEGER := 0;
    buf          := ARRAY [0 .. 3] OF CHAR{'\000', '\000', '\000', '\000'};
  BEGIN
    IF t # NIL THEN
      Text.SetChars(buf, t);
      res := Word.Insert(res, ORD(buf[0]), 0, 8);
      res := Word.Insert(res, ORD(buf[1]), 8, 8);
      res := Word.Insert(res, ORD(buf[2]), 16, 8);
      res := Word.Insert(res, ORD(buf[3]), 24, 8);
    END;
    RETURN res;
  END TextToInt;

PROCEDURE IntToText (val: INTEGER): TEXT =
  VAR buf: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    buf[0] := VAL(Word.Extract(val, 0, 8), CHAR);
    buf[1] := VAL(Word.Extract(val, 8, 8), CHAR);
    buf[2] := VAL(Word.Extract(val, 16, 8), CHAR);
    buf[3] := VAL(Word.Extract(val, 24, 8), CHAR);
    RETURN Text.FromChars(buf);
  END IntToText;

VAR
  DegreeEvaluator := NEW(TypedGraph.Evaluator, apply := ComputeDegree);
  LevelEvaluator  := NEW(TypedGraph.Evaluator, apply := ComputeLevel);

PROCEDURE ComputeDegree (<* UNUSED *> eval : TypedGraph.Evaluator;
                                      graph: TypedGraph.T;
                                      node : Node.T;
                                      attr : Scheme.ID             )
  RAISES {TypedGraph.CyclicEvaluation, Access.Locked,
          TypedGraph.InternalError, TypedGraph.LogError} =
  VAR
    sons : NodeSet.T;
    deg  : CARDINAL;
    son  : Node.T;
    found: BOOLEAN;
  <* FATAL TypedGraph.Unknown, TypedGraph.NodeNotFound *>
  <* FATAL TypedGraph.WrongType, TypedGraph.NotOwner *>
  BEGIN
    sons := graph.getTargets(node, TreeScheme.toSon);
    deg := sons.card();
    sons.loop();
    son := sons.get(found);
    WHILE found DO
      deg := MAX(deg, TextToInt(graph.getAttribute(
                                  son, TreeScheme.maxDegree, 0, 4)));
      son := sons.get(found);
    END;
    sons.dispose();
    graph.putAttribute(node, attr, 0, IntToText(deg));
  END ComputeDegree;


PROCEDURE ComputeLevel (<* UNUSED *> eval : TypedGraph.Evaluator;
                                     graph: TypedGraph.T;
                                     node : Node.T;
                                     attr : Scheme.ID             )
  RAISES {TypedGraph.CyclicEvaluation, Access.Locked,
          TypedGraph.InternalError, TypedGraph.LogError} =
  VAR
    sons : NodeSet.T;
    level: CARDINAL;
    son  : Node.T;
    found: BOOLEAN;
  <* FATAL TypedGraph.Unknown, TypedGraph.NodeNotFound *>
  <* FATAL TypedGraph.WrongType, TypedGraph.NotOwner *>
  BEGIN
    sons := graph.getTargets(node, TreeScheme.toSon);
    level := 1;
    sons.loop();
    son := sons.get(found);
    WHILE found DO
      level := MAX(level, TextToInt(graph.getAttribute(
                                      son, TreeScheme.level, 0, 4)) + 1);
      son := sons.get(found);
    END;
    sons.dispose();
    graph.putAttribute(node, attr, 0, IntToText(level));
  END ComputeLevel;


PROCEDURE ReadScheme (scheme: Scheme.T) RAISES {Error} =
  BEGIN
    TRY
      TreeScheme.type := scheme.getNodeTypeNumber(TreeScheme.TreeNodeType);
      TreeScheme.level := scheme.getAttributeNumber(
                            TreeScheme.type, TreeScheme.LevelAttribute);
      TreeScheme.maxDegree :=
        scheme.getAttributeNumber(
          TreeScheme.type, TreeScheme.DegreeAttribute);
      TreeScheme.isRoot := scheme.getAttributeNumber(
                             TreeScheme.type, TreeScheme.IsRootAttribute);
      TreeScheme.toSon := scheme.getEdgeTypeNumber(TreeScheme.TreeEdge);
    EXCEPT
      Scheme.InternalError (info) =>
        RAISE Error("BuildScheme: " & ErrorSupport.ToText(info) & "\n");
    | Scheme.NotDeclared =>
        RAISE Error("BuildScheme: Item not declared\n");
    END;
  END ReadScheme;

PROCEDURE UpdateNodeNumber (             event   : Event.T;
                            <* UNUSED *> context : ContextSet.T;
                            <* UNUSED *> local   : BOOLEAN;
                            <* UNUSED *> userdata: REFANY        ) =
  <* FATAL EventType.Unknown *>
  BEGIN
    IF GraphEvents.GetOperation(event) = GraphEvents.Operation.DeleteNode THEN
      DEC(noOfNodes);
    ELSE
      INC(noOfNodes);
    END;
    Panel.SetNodes(noOfNodes);
  END UpdateNodeNumber;

PROCEDURE InstallTrigger (pool: TypedGraphPool.T; graph: TypedGraph.T) =
  VAR
    tid    : CARDINAL;
    trigger: Trigger.T;
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    (* We use coupling decoupled here to avoid mismatch of noOfNodes with
       real number of nodes in graph in case of a transaction abort *)
    (* Activate a trigger for deletion of a node. *)
    WITH pattern = GraphEventPattern.Create(
                     GraphEvents.Operation.DeleteNode),
         action = NEW(Action.Local).init(UpdateNodeNumber) DO
      (* specify as many attributes as possible *)
      GraphEventPattern.SetPoolName(pattern, pool.getBaseName());
      GraphEventPattern.SetPool(pattern, pool);
      GraphEventPattern.SetGraphNumber(pattern, graph.number());
      GraphEventPattern.SetGraph(pattern, graph);
      GraphEventPattern.SetPreEvent(pattern, FALSE);
      GraphEventPattern.SetNodeLabel(pattern, TreeScheme.type.entity);
      trigger :=
        Trigger.Create(
          pattern, action, coupling := Trigger.CouplingMode.Decoupled,
          priority := 1, inhibiting := ContextSet.Empty(),
          permitting := ContextSet.Empty());
      tid := RuleEngine.RegisterTrigger(trigger, RuleEngine.Interest.Self);
    END;
    (* Activate a trigger for creation of a node. *)
    WITH pattern = GraphEventPattern.Create(
                     GraphEvents.Operation.CreateNode),
         action = NEW(Action.Local).init(UpdateNodeNumber) DO
      (* specify as many attributes as possible *)
      GraphEventPattern.SetPoolName(pattern, pool.getBaseName());
      GraphEventPattern.SetPool(pattern, pool);
      GraphEventPattern.SetGraphNumber(pattern, graph.number());
      GraphEventPattern.SetGraph(pattern, graph);
      GraphEventPattern.SetPreEvent(pattern, FALSE);
      GraphEventPattern.SetNodeLabel(pattern, TreeScheme.type.entity);
      trigger :=
        Trigger.Create(
          pattern, action, coupling := Trigger.CouplingMode.Decoupled,
          priority := 1, inhibiting := ContextSet.Empty(),
          permitting := ContextSet.Empty());
      tid := RuleEngine.RegisterTrigger(trigger, RuleEngine.Interest.Self);
    END;
  END InstallTrigger;

PROCEDURE ErrorAbort () =
  BEGIN
    TRY
      WHILE pool.getTransactionLevel() > Transaction.EnvelopeLevel DO
        pool.abortTransaction();
      END;
      graph.close(keepLog := TRUE);
      pool.close();
    EXCEPT
    ELSE
    END;
  END ErrorAbort;

PROCEDURE GetNextCommand (): Command.UserCommand =
  BEGIN
    RETURN Panel.QueryCommand();
  END GetNextCommand;

PROCEDURE GetParam (name: TEXT; VAR p: CARDINAL): BOOLEAN =
  VAR ok: BOOLEAN;
  BEGIN
    Panel.QueryParameter(name, p, ok);
    RETURN ok;
  END GetParam;

PROCEDURE DisplayError (msg: TEXT) =
  BEGIN
    IO.Put(msg & "\n");
  END DisplayError;

PROCEDURE PrintTree (graph: TypedGraph.T; root: Node.T)
  RAISES {TypedGraph.InternalError} =

  PROCEDURE RecPrintTree (graph: TypedGraph.T; subtree: Node.T)
    RAISES {TypedGraph.InternalError} =
    VAR
      sons : NodeSet.T;
      deg  : CARDINAL;
      son  : Node.T;
      found: BOOLEAN;
    <* FATAL TypedGraph.Unknown, TypedGraph.NodeNotFound *>
    <* FATAL TypedGraph.NotOwner *>
    <* FATAL Access.Locked  *>
    BEGIN
      IO.Put(Fmt.Int(subtree.entity) & " -> ");
      sons := graph.getTargets(subtree, TreeScheme.toSon);
      deg := sons.card();
      sons.loop();
      son := sons.get(found);
      WHILE found DO
        IO.Put(Fmt.Int(son.entity) & " ");
        son := sons.get(found);
      END;
      IO.Put("\n");
      sons.loop();
      son := sons.get(found);
      WHILE found DO RecPrintTree(graph, son); son := sons.get(found); END;
      sons.dispose();
    END RecPrintTree;

  BEGIN
    IO.Put("Root ");
    RecPrintTree(graph, root);
    IO.Put("\n\n");
  END PrintTree;

PROCEDURE ReadAttribute (graph: TypedGraph.T; node: Node.T; attr: Scheme.ID):
  CARDINAL RAISES {Error, TypedGraph.LogError, TypedGraph.InternalError} =
  <* FATAL TypedGraph.CyclicEvaluation *>
  <* FATAL TypedGraph.NodeNotFound, TypedGraph.NotOwner *>
  <* FATAL TypedGraph.Unknown, TypedGraph.WrongType, Access.Locked *>
  VAR res: CARDINAL;
  BEGIN
    IF graph.existsNode(node) THEN
      res := TextToInt(graph.getAttribute(node, attr, 0, 4));
    ELSE
      RAISE Error("Node " & Fmt.Int(node.entity) & " not found.");
    END;
    RETURN res;
  END ReadAttribute;

VAR
  rootpath, nameserver, serverid: TEXT;
  cachesize                     : CARDINAL;
  rootvalid                     : BOOLEAN;
  pool                          : TypedGraphPool.T;
  graph                         : TypedGraph.T;
  command                       : Command.UserCommand;
  param                         : CARDINAL;
  root                          : Node.T;
  rootMaxDegree, rootLevel      : CARDINAL;
  noOfNodes                     : CARDINAL;

<* FATAL TypedGraph.InUse, TypedGraph.NotExistent, TypedGraph.NoScheme *>
<* FATAL TypedGraph.AlreadyBound *>
BEGIN
  TRY
    GrasParams.ParseComandLine(
      rootpath, rootvalid, cachesize, serverid, nameserver);
    IF NOT rootvalid THEN IO.Put("Need root path."); Process.Exit(1); END;
    (* GRAS system login *)
    TypedGraphSystem.Login(rootpath, cachesize, serverid, nameserver);

    (* open/create pool *)
    WITH exists = TypedGraphSystem.ExistsPool(PoolName) DO
      pool :=
        NEW(TypedGraphPool.T).open(
          PoolName, Access.Mode.ReadWriteExclusive, new := NOT exists);
    END;

    (* Check for scheme and build if not existent *)
    pool.beginTransaction();
    IF NOT pool.existsScheme(SchemeName) THEN
      WITH scheme = NEW(Scheme.T).open(
                      pool, SchemeName,
                      access := Scheme.AccessMode.ReadWriteExclusive,
                      new := TRUE, local := FALSE) DO
        BuildScheme(scheme);
        scheme.close();
      END;
    END;
    pool.commitTransaction();

    (* Open/create graph *)
    pool.beginTransaction();
    WITH existent = pool.existsGraph(GraphName) DO
      graph := NEW(TypedGraph.T).open(
                 pool, GraphName, TypedGraph.AccessMode.ReadWriteExclusive,
                 new := NOT existent, errorChecks := TypedGraph.AllChecks,
                 local := FALSE, log := ChgMgmtGraph.LogMode.Tree,
                 scheme := SchemeName);
      ReadScheme(graph.getScheme());
      graph.bindEvaluator(TreeScheme.LevelEval, LevelEvaluator);
      graph.bindEvaluator(TreeScheme.DegreeEval, DegreeEvaluator);
      IF existent THEN
        WITH nds = graph.getAllNodesOfType(TreeScheme.type) DO
          noOfNodes := nds.card();
          nds.dispose();
        END;
      ELSE
        noOfNodes := 0;
      END;
    END;
    pool.commitTransaction();

    IF NOT Panel.Open() THEN Process.Exit(1); END;
    Command.Init(pool, graph);

    (* Set up trigger for number of nodes display *)
    Panel.SetNodes(noOfNodes);
    InstallTrigger(pool, graph);

    (* Enter command execution cycle *)
    REPEAT
      command := GetNextCommand();
      IF NOT Command.NeedsParam[command]
           OR GetParam(Command.ParamName[command], param) THEN
        TRY
          Command.ExecuteCommand(pool, graph, command, param, root);
          pool.beginTransaction();
          rootMaxDegree :=
            ReadAttribute(graph, root, TreeScheme.maxDegree);
          rootLevel := ReadAttribute(graph, root, TreeScheme.level);
          PrintTree(graph, root);
          pool.commitTransaction();
          Panel.SetRoot(root.entity);
          Panel.SetMaxDegree(rootMaxDegree);
          Panel.SetLevel(rootLevel);
        EXCEPT
          Command.Error (msg) => DisplayError(msg);
        | Error (msg) =>
            TRY pool.abortTransaction(); EXCEPT ELSE END;
            DisplayError(msg);
        END;
      END;
    UNTIL command = Command.UserCommand.Quit;
    Panel.Close();

    graph.close(keepLog := TRUE);
    pool.close();
  EXCEPT
  | Error (msg) => ErrorAbort(); IO.Put(msg);
  | Scheme.Existent => ErrorAbort(); IO.Put("Scheme existent!\n");
  | Scheme.InUse => ErrorAbort(); IO.Put("Scheme in use!\n");
  | Scheme.NotExistent => ErrorAbort(); IO.Put("Scheme not existent!\n");
  | TypedGraphSystem.InternalError (info) =>
      IO.Put(ErrorSupport.ToText(info));
  | Scheme.InternalError (info) =>
      ErrorAbort();
      IO.Put(ErrorSupport.ToText(info));
  | TypedGraphPool.InternalError (info) =>
      ErrorAbort();
      IO.Put(ErrorSupport.ToText(info));
  | TypedGraphPool.CardinalityError =>
      ErrorAbort();
      IO.Put("Cardinality error!\n");
  | TypedGraphPool.NotInTransaction =>
      ErrorAbort();
      IO.Put("Not in transaction!\n");
  | TypedGraph.InternalError (info) =>
      ErrorAbort();
      IO.Put(ErrorSupport.ToText(info));
  | TypedGraph.LogError (info) =>
      ErrorAbort();
      IO.Put(ErrorSupport.ToText(info));
  | Access.Denied (msg) => IO.Put("Access denied: " & msg & "\n");
  | Access.Locked => IO.Put("Access locked!\n");
  | PageFile.NoAccess (msg) => IO.Put("No access: " & msg & "\n");
  | Scheme.NoValidScheme => IO.Put("Invalid scheme!\n");
  END;
END Tree.
