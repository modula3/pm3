MODULE ExecuteDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.3  1998/05/19 10:17:34  roland
    Support for log-groups implemented.

    Revision 1.2  1998/01/21 12:26:14  roland
    Node number is tested when executing create node command.

    Revision 1.1  1997/04/23 14:09:48  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

*)
(***************************************************************************)

IMPORT Delta, GraphCommand, Node, ChgMgmtOpenGraphs, PersistentGraph;
IMPORT Journal, Variant, ErrorSupport, Access;
IMPORT Fmt;


PROCEDURE F (og: ChgMgmtOpenGraphs.T; d: Delta.T)
  RAISES {Access.Locked, NotOpen, PersistentGraph.NodeNotFound, InternalError} =

  PROCEDURE ExecuteCommand (READONLY com  : GraphCommand.T)
    RAISES {Access.Locked, NotOpen, PersistentGraph.NodeNotFound, InternalError} =
    (* realizes execution of the command com on the graph *)

    PROCEDURE FmtNode (g, n: CARDINAL): TEXT =
      BEGIN
        RETURN "(" & Fmt.Int(g) & "," & Fmt.Int(n) & ")";
      END FmtNode;
      VAR graph: PersistentGraph.T;

    BEGIN
      TRY
        CASE com.operation OF
        | GraphCommand.Operation.CreateNode =>
            VAR nodeNo: Node.T;
            BEGIN
              IF Variant.TestChgMgmt THEN
                Journal.Add("Create node with number "
                              & FmtNode(com.args[0], com.args[1])
                              & " and label " & Fmt.Int(com.args[2]) & ".");
              END;
              graph := og.getGraph(com.args[0]);
              nodeNo := PersistentGraph.T.createNodeNumber(
                          graph, Node.T{com.args[0], com.args[1]});
              IF nodeNo.entity # com.args[1] THEN
                RAISE InternalError(
                        ErrorSupport.Create("ExecuteDelta.ExecuteCommand",
                                            "Cannot create node!"));
              END;
              nodeNo := PersistentGraph.T.createNode(graph, com.args[2])
            END;
        | GraphCommand.Operation.DeleteNode =>
            IF Variant.TestChgMgmt THEN
              Journal.Add("Delete node with number "
                            & FmtNode(com.args[0], com.args[1]) & ".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.deleteNodeNoInfo(
              graph, Node.T{com.args[0], com.args[1]})
        | GraphCommand.Operation.PutNodeLabel =>
            IF Variant.TestChgMgmt THEN
              Journal.Add(
                "Put node label at node "
                  & FmtNode(com.args[0], com.args[1]) & ". New label is "
                  & Fmt.Int(com.args[2]) & ".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.putNodeLabel(
              graph, Node.T{com.args[0], com.args[1]}, com.args[2])
        | GraphCommand.Operation.PutAttribute =>
            IF Variant.TestChgMgmt THEN
              Journal.Add(
                "Put attribute no. " & Fmt.Int(com.args[2]) & " at node "
                  & FmtNode(com.args[0], com.args[1]) & ", starting from "
                  & Fmt.Int(com.from) & " ranging to " & Fmt.Int(com.to)
                  & ". Value is \"" & com.text & "\".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.putAttribute(
              graph, Node.T{com.args[0], com.args[1]}, com.args[2],
              com.from, com.text)
        | GraphCommand.Operation.DeleteAttribute =>
            IF Variant.TestChgMgmt THEN
              Journal.Add(
                "Delete attribute no. " & Fmt.Int(com.args[2])
                  & " at node " & FmtNode(com.args[0], com.args[1]) & ".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.deleteAttribute(
              graph, Node.T{com.args[0], com.args[1]}, com.args[2])
        | GraphCommand.Operation.TruncateAttribute =>
            IF Variant.TestChgMgmt THEN
              Journal.Add(
                "Truncate attribute no. " & Fmt.Int(com.args[2])
                  & " at node " & FmtNode(com.args[0], com.args[1])
                  & ", to length " & Fmt.Int(com.args[3]) & ".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.truncateAttribute(
              graph, Node.T{com.args[0], com.args[1]}, com.args[2],
              com.args[3])
        | GraphCommand.Operation.PutIndex =>
            IF Variant.TestChgMgmt THEN
              Journal.Add(
                "Put index no. " & Fmt.Int(com.args[2]) & " at node "
                  & FmtNode(com.args[0], com.args[1]) & ". Value is \""
                  & com.text & "\".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.putIndex(
              graph, Node.T{com.args[0], com.args[1]}, com.args[2],
              com.text)
        | GraphCommand.Operation.DeleteIndex =>
            IF Variant.TestChgMgmt THEN
              Journal.Add(
                "Delete index no. " & Fmt.Int(com.args[2]) & " at node "
                  & FmtNode(com.args[0], com.args[1]) & ". Value is \""
                  & com.text & "\".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.deleteIndex(
              graph, Node.T{com.args[0], com.args[1]}, com.args[2],
              com.text)
        | GraphCommand.Operation.CreateEdge =>
            IF Variant.TestChgMgmt THEN
              Journal.Add(
                "Create edge from node "
                  & FmtNode(com.args[0], com.args[1]) & " to node "
                  & FmtNode(com.args[2], com.args[3]) & ". Label is \""
                  & Fmt.Int(com.args[4]) & "\".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.createEdge(
              graph, Node.T{com.args[0], com.args[1]},
              Node.T{com.args[2], com.args[3]}, com.args[4])
        | GraphCommand.Operation.DeleteEdge =>
            IF Variant.TestChgMgmt THEN
              Journal.Add(
                "Delete edge from node "
                  & FmtNode(com.args[0], com.args[1]) & " to node "
                  & FmtNode(com.args[2], com.args[3]) & ". Labe is \""
                  & Fmt.Int(com.args[4]) & "\".");
            END;
            graph := og.getGraph(com.args[0]);
            PersistentGraph.T.deleteEdge(
              graph, Node.T{com.args[0], com.args[1]},
              Node.T{com.args[2], com.args[3]}, com.args[4])
        ELSE
        END;
      EXCEPT
        PersistentGraph.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "LogTools.ExecuteCommand",
                                "PersistentGraph.InternalError", info));
      | PersistentGraph.NotOwner =>
          RAISE
            InternalError(ErrorSupport.Create("LogTools.ExecuteCommand",
                                              "PersistentGraph.NotOwner"));
      | PersistentGraph.IndexUsed =>
          RAISE InternalError(ErrorSupport.Create(
                                "LogTools.ExecuteCommand",
                                "PersistentGraph.IndexUsed"));
      | PersistentGraph.IndexUnused =>
          RAISE InternalError(
                  ErrorSupport.Create("LogTools.ExecuteCommand",
                                      "PersistentGraph.IndexUnused"));
      | ChgMgmtOpenGraphs.NotOpen => RAISE NotOpen;
      END;

    END ExecuteCommand;

  VAR
    com: GraphCommand.T;
    ok : BOOLEAN;
  BEGIN
    TRY
      d.loop();
      d.getNextCommand(com, ok);
      WHILE ok DO
        ExecuteCommand(com);
        d.getNextCommand(com, ok);
      END;
    EXCEPT
      Delta.Error (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                         "ExecuteDelta.F", "Delta.Error", info));
    END;
  END F;

BEGIN
END ExecuteDelta.
