MODULE ChgMgmtGraph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.11  1998/08/27 12:31:00  roland
    Included simple mechanism to monitor log size.

    Revision 1.10  1998/07/30 15:19:36  roland
    Bugfix: DeleteNode did not write to forward log.

    Revision 1.9  1998/05/19 10:17:27  roland
    Support for log-groups implemented.

    Revision 1.8  1998/03/18 09:27:07  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.7  1998/03/17 14:13:51  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.6  1998/01/21 12:29:15  roland
    Several bugfixes in recovery operations and logging of graph commands.

    Revision 1.5  1997/12/02 17:56:29  roland
    New event types and event contexts for user recovery operations
    introduced.

    Revision 1.4  1997/07/21 10:37:24  roland
    Adaptions to new set-implementation (no more SetExceptions). Now use free
    memory management of deltas and sets.

    Revision 1.3  1997/05/30 07:55:10  roland
    Small bugfix in logging putIndex-commands.

    Revision 1.2  1997/04/24 14:29:08  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 14:09:10  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

    Revision 1.15  1997/02/28 14:26:47  roland
    Bugfix: readonly access will not create or use log

    Revision 1.14  1997/02/04 11:15:13  roland
    It is now possible to disable logging in ChgMgmtGraph completely.

    Revision 1.13  1996/12/23 10:34:54  roland
    Implementation of ChgMgmtGraphSystem separated from ChgMgmtGraph.

    Revision 1.12  1996/12/20 17:30:46  roland
    First version of ChgMgmtGraphSystem. Difference to
    PersistentGraphSystem: Graphs might be connected via deltas
    (DeltaCopyGraph) to allow efficient versioning.

    Revision 1.11  1996/12/03 13:17:39  roland
    Bugfixes regarding attribute handling for undo/redo and severe bug in
    ExecuteCommand (CreateNode) fixed.

    Revision 1.10  1996/12/03 09:52:16  roland
    Bugfix. Undo and Redo for PutAttribute should now work properly.

    Revision 1.9  1996/11/25 08:21:14  roland
    Exception PendingTransaction is raised when client tries to close a
    graph and has a running transaction.

    Revision 1.8  1996/11/22 16:06:02  roland
    Exception-Handling in PersistentGraph changed so that no exceptions of
    VirtualResource are passed to calling procedures.

    Revision 1.7  1996/11/20 12:20:15  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.6  1996/11/14 16:04:10  roland
    CommitTransaction must be aware of missing persistent log.

    Revision 1.5  1996/11/14 15:46:58  roland
    CommitTransaction must be aware of missing persistent log.

    Revision 1.4  1996/11/14 14:16:49  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Access in mode ReadOnlyShared is now considered when opening graphs.

    Revision 1.3  1996/09/23 08:34:22  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:58:15  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:56:42  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)


IMPORT PersistentGraph AS Super;

IMPORT AttributeDescriptionSet, AttributeDescription, TaggedNameSet,
       TaggedName, NodeLabelRelation, CardSet, Node;
IMPORT ErrorSupport;
IMPORT Log, VolatileDelta, GraphCommand, Delta, DeltaList,
       ChgMgmtGraphPool, InternChgMgmtGraphPool, ExecuteDelta,
       PersistentGraphPool;
IMPORT LogEvents, LogContexts, RuleEngine;
IMPORT PageFile, Access;
IMPORT Pathname, Text;
IMPORT Variant, Fmt, Journal;

REVEAL
  T = Public BRANDED OBJECT
        pool     : ChgMgmtGraphPool.T;
        logging  : BOOLEAN;
        logmode  : LogMode;
        loghandle: CARDINAL;
        fwcount, bwcount: CARDINAL := 0;
      OVERRIDES

        (* graph managing operations *)
        open  := Open;
        close := Close;

        (* overridden graph-changing operations *)
        getLogMode        := GetLogMode;
        createNode        := CreateNode;
        deleteNode        := DeleteNode;
        deleteNodeNoInfo  := DeleteNodeNoInfo;
        putNodeLabel      := PutNodeLabel;
        putAttribute      := PutAttribute;
        deleteAttribute   := DeleteAttribute;
        truncateAttribute := TruncateAttribute;
        putIndex          := PutIndex;
        deleteIndex       := DeleteIndex;
        createEdge        := CreateEdge;
        deleteEdge        := DeleteEdge;

        (* user-recovery operations *)
        setCheckpoint         := SetCheckpoint;
        setCheckpointLabel    := SetCPLabel;
        removeCheckpointLabel := RmCPLabel;
        undo                  := Undo;
        redo                  := Redo;
        redoNext              := RedoNext;
        redoPrev              := RedoPrev;
        redoIth               := RedoIth;
        backstep              := Backstep;
        forstep               := Forstep;
        gotoCheckpointLabel   := GotoCPLabel;

      END;

PROCEDURE Open (graph           : T;
                pool            : ChgMgmtGraphPool.T;
                baseName        : Pathname.T;
                access          : AccessMode;
                new, errorChecks: BOOLEAN;
                local           : BOOLEAN;
                log             : LogMode              := LogMode.Linear;
                logGroup        : Pathname.T           := NIL             ):
  T RAISES {InUse, Access.Locked, NotExistent, PageFile.NoAccess,
            InternalError, Access.Denied} =

  PROCEDURE Mode (access: AccessMode): Super.AccessMode =
    BEGIN
      RETURN VAL(ORD(access), Super.AccessMode);
    END Mode;

  VAR abort: BOOLEAN := FALSE;
  <* FATAL ChgMgmtGraphPool.NotAllowed *>
  BEGIN
    graph.pool := pool;
    TRY
      TRY
        PersistentGraphPool.T.beginTransaction(pool);
        IF new THEN
          (* check if log group exists *)
          IF logGroup # NIL THEN
            IF NOT graph.pool.existsLogGroup(logGroup) THEN
              abort := TRUE;
              RAISE NotExistent;
            END;
          END;
          (* look if the database already exists *)
          IF pool.existsGraph(baseName, local) THEN
            (* is someone using it? *)
            IF pool.graphInUse(baseName, local) THEN
              abort := TRUE;
              RAISE InUse;
            END;
            (* ok, delete the old graph *)
            pool.deleteGraph(baseName, local);
          END;
        ELSE
          (* graph is not new, logGroup parameter is ignored *)
          IF NOT pool.existsGraph(baseName, local) THEN
            abort := TRUE;
            RAISE NotExistent;
          END;
          logGroup := pool.getLogGroup(baseName);
        END;
        (* open persistent graph *)
        graph :=
          pool.openPG(baseName, Mode(access), errorChecks, local, graph);
        (* Open Log *)
        graph.logmode := log;
        graph.loghandle := pool.openLog(baseName, logGroup, local,
                                        Mode(access), graph.logmode);

        graph.logging := graph.logmode # LogMode.None;
        IF new AND logGroup # NIL THEN
          pool.addToLogGroup(logGroup, baseName);
        END;
      EXCEPT
        ChgMgmtGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE InternalError(ErrorSupport.Propagate(
                                "PersistentGraph.Open",
                                "ChgMgmtGraphPool.InternalError", info));
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.Open",
                            "PersistentGraphPool.InternalError", info));
      | ChgMgmtGraphPool.InUse => abort := TRUE; RAISE InUse;
      | ChgMgmtGraphPool.NotExistent =>
          abort := TRUE;
          RAISE InternalError(ErrorSupport.Create(
                                "PersistentGraph.Open",
                                "ChgMgmtGraphPool.NotExistent"));
      | Access.Denied (msg) => abort := TRUE; RAISE Access.Denied(msg);
      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
    RETURN graph;
  END Open;


PROCEDURE Close (graph: T; keepLog: BOOLEAN) RAISES {InternalError} =
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    TRY
      TRY
        PersistentGraphPool.T.beginTransaction(graph.pool);
        IF Variant.LogCount AND graph.logging THEN
          Journal.Add("Graph " & graph.baseName());
          Journal.Add("  fwcount    = " & Fmt.Int(graph.fwcount));
          Journal.Add("  bwcount    = " & Fmt.Int(graph.bwcount));
          VAR fwdl, bwdl: DeltaList.T;
              fw, bw: Delta.T;
              fcosts, bcosts: CARDINAL := 0;
              ok: BOOLEAN;
              <* FATAL Log.InternalError, Access.Locked *>
              <* FATAL Delta.Error, DeltaList.NotInitialized *>
          BEGIN
            graph.pool.getLog(graph.loghandle).getActualPath(fwdl, bwdl);
            fwdl.loop();
            bwdl.loop();
            bw := bwdl.get(ok);
            fw := fwdl.get(ok);
            WHILE ok DO
              fcosts := fcosts + fw.costs();
              bcosts := bcosts + bw.costs();
              fw := fwdl.get(ok);
              bw := bwdl.get(ok);
            END;
            Journal.Add("  optfwcount = " & Fmt.Int(fcosts));
            Journal.Add("  optbwcount = " & Fmt.Int(bcosts));
          END;            
        END;
        graph.pool.closePG(graph);
        graph.pool.closeLog(graph.loghandle, keepLog);
      EXCEPT
        ChgMgmtGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE InternalError(ErrorSupport.Propagate(
                                "ChgMgmtGraph.Close",
                                "ChgMgmtGraphPool.InternalError", info));
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.Close",
                            "PersistentGraphPool.InternalError", info));
      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
  END Close;



(***************************************************)

PROCEDURE GetLogMode (graph: T): LogMode =
  BEGIN
    RETURN graph.logmode;
  END GetLogMode;

(***************************************************)
(* overridden graph changing Operations *)
(***************************************************)

PROCEDURE CreateNode (graph: T; label: CARDINAL): Node.T
  RAISES {Access.Locked, LogError, InternalError} =
  VAR
    nodeNo    : Node.T;
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    TRY
      nodeNo := Super.T.createNode(graph, label);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraph.CreateNode", "Super.InternalError", info));
    END;

    IF graph.logging THEN
      fw := VolatileDelta.New().init(forward := TRUE);
      bw := VolatileDelta.New().init(forward := FALSE);
      (* write forward log *)
      GraphCommand.CreateNode(fcom, nodeNo, label);
      fw.addCommand(fcom);
      INC(graph.fwcount);

      (* write backward log *)
      GraphCommand.DeleteNode(bcom, nodeNo);
      bw.addCommand(bcom);
      INC(graph.bwcount);
      TRY
        graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
        VolatileDelta.Free(fw);
        VolatileDelta.Free(bw);
      EXCEPT
        Log.InternalError (info) =>
          RAISE
            LogError(ErrorSupport.Propagate("ChgMgmtGraph.CreateNode",
                                            "Log.InternalError", info));
      END;
    END;
    RETURN nodeNo;
  END CreateNode;

PROCEDURE DeleteNodeNoInfo (graph: T; node: Node.T)
  RAISES {Super.NotOwner, Access.Locked, LogError, InternalError} =
  VAR
    atr   : AttributeDescriptionSet.T;
    idx   : TaggedNameSet.T;
    oe, ie: NodeLabelRelation.T;
  BEGIN
    DeleteNode(graph, node, atr, idx, oe, ie);
    atr.dispose();
    idx.dispose();
    oe.dispose();
    ie.dispose();
  END DeleteNodeNoInfo;

PROCEDURE DeleteNode (    graph            : T;
                          node             : Node.T;
                      VAR attributes       : AttributeDescriptionSet.T;
                      VAR indexes          : TaggedNameSet.T;
                      VAR outEdges, inEdges: NodeLabelRelation.T        )
  RAISES {Super.NotOwner, Access.Locked, LogError, InternalError} =
  VAR
    attribute     : AttributeDescription.T;
    index         : TaggedName.T;
    label, edgelab: CARDINAL;
    target        : Node.T;
    found         : BOOLEAN;
    fcom, bcom    : GraphCommand.T;
    fw, bw        : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>

  BEGIN

    TRY
      (* delete the node *)
      RuleEngine.DelayActionExecution(graph.pool.getRuleEngineID());
      PersistentGraphPool.T.beginTransaction(graph.pool);
      TRY
        IF graph.logging THEN
          label := Super.T.getNodeLabel(graph, node)
        END;
        Super.T.deleteNode(
          graph, node, attributes, indexes, outEdges, inEdges);
      EXCEPT
        Super.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "ChgMgmtGraph.DeleteNode",
                                "Super.InternalError", info));
      | Super.NodeNotFound =>    (* ignore *)
      END;

      IF graph.logging THEN
        fw := VolatileDelta.New().init(forward := TRUE);
        bw := VolatileDelta.New().init(forward := FALSE);
        (* write to forward log *)
        GraphCommand.DeleteNode(fcom, node);
        fw.addCommand(fcom);
        INC(graph.fwcount);

        (* write backward log *)
        (* log all index attributes *)
        indexes.loop();
        index := indexes.get(found);
        WHILE found DO
          GraphCommand.PutIndex(
            bcom, node, index.tag, Text.Length(index.name), index.name);
          bw.addCommand(bcom);
          INC(graph.bwcount);
          index := indexes.get(found);
        END;
        (* log all attributes *)
        attributes.loop();
        attribute := attributes.get(found);
        WHILE found DO
          GraphCommand.PutAttribute(
            bcom, node, attribute.number, 0, Text.Length(attribute.value),
            attribute.value);
          bw.addCommand(bcom);
          INC(graph.bwcount);
          attribute := attributes.get(found);
        END;

        WITH graphNumber = graph.number() DO
          inEdges.loop();
          inEdges.get(target, edgelab, found);
          WHILE found DO
            IF graph.pool.sameLogGroup(target.graph, graphNumber) THEN
              GraphCommand.CreateEdge(bcom, target, node, edgelab);
              bw.addCommand(bcom);
              INC(graph.bwcount);
            END;
            inEdges.get(target, edgelab, found);
          END;
          (* log all outgoing edges *)
          outEdges.loop();
          outEdges.get(target, edgelab, found);
          WHILE found DO
            IF graph.pool.sameLogGroup(target.graph, graphNumber) THEN
              GraphCommand.CreateEdge(bcom, node, target, edgelab);
              bw.addCommand(bcom);
              INC(graph.bwcount);
            END;
            outEdges.get(target, edgelab, found);
          END;
        END;
        (* log the node *)
        GraphCommand.CreateNode(bcom, node, label);
        bw.addCommand(bcom);
        INC(graph.bwcount);

        (* really writing *)
        TRY
          graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
          VolatileDelta.Free(fw);
          VolatileDelta.Free(bw);
        EXCEPT
          Log.InternalError (info) =>
            RAISE
              LogError(ErrorSupport.Propagate("ChgMgmtGraph.DeleteNode",
                                              "Log.InternalError ", info));
        END;
      END;
      PersistentGraphPool.T.commitTransaction(graph.pool);
      RuleEngine.ReleaseActionExecution(graph.pool.getRuleEngineID());
    EXCEPT
      PersistentGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraph.DeleteNode",
                              "PersistentGraphPool.InternalError", info));
    | ChgMgmtGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraph.DeleteNode",
                              "ChgMgmtGraphPool.InternalError", info));
    | PersistentGraphPool.NotInTransaction =>
        RAISE InternalError(ErrorSupport.Create(
                              "ChgMgmtGraph.DeleteNode",
                              "PersistentGraphPool.NotInTransaction"));
    END;
  END DeleteNode;

PROCEDURE PutNodeLabel (graph: T; nodeNo: Node.T; label: CARDINAL)
  RAISES {Super.NotOwner, Access.Locked, LogError, InternalError,
          Super.NodeNotFound} =
  VAR
    oldLabel  : CARDINAL;
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    TRY
      IF graph.logging THEN
        oldLabel := Super.T.getNodeLabel(graph, nodeNo);
      END;
      Super.T.putNodeLabel(graph, nodeNo, label);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraph.PutNodeLabel",
                              "Super.InternalError", info));
    END;

    IF graph.logging THEN
      fw := VolatileDelta.New().init(forward := TRUE);
      bw := VolatileDelta.New().init(forward := FALSE);
      (* write forward log *)
      GraphCommand.PutNodeLabel(fcom, nodeNo, label);
      fw.addCommand(fcom);
      INC(graph.fwcount);

      (* write backward log *)
      GraphCommand.PutNodeLabel(bcom, nodeNo, oldLabel);
      bw.addCommand(bcom);
      INC(graph.bwcount);

      TRY
        graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
        VolatileDelta.Free(fw);
        VolatileDelta.Free(bw);
      EXCEPT
        Log.InternalError (info) =>
          RAISE
            LogError(ErrorSupport.Propagate("ChgMgmtGraph.PutNodeLabel",
                                            "Log.InternalError ", info));
      END;
    END;
  END PutNodeLabel;

PROCEDURE PutAttribute (graph      : T;
                        node       : Node.T;
                        attributeNo: CARDINAL;
                        start      : CARDINAL;
                        attribute  : TEXT      )
  RAISES {Super.NotOwner, Access.Locked, Super.NodeNotFound, LogError,
          InternalError} =
  VAR
    len, olen : CARDINAL;
    old       : TEXT;
    attrNos   : CardSet.T;
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    len := Text.Length(attribute);
    IF len > 0 THEN
      TRY
        IF graph.logging THEN
          old :=
            Super.T.getAttribute(graph, node, attributeNo, start, len);
          attrNos := Super.T.getAllAttributeNumbers(graph, node);
          olen := Text.Length(old);
        END;
        Super.T.putAttribute(graph, node, attributeNo, start, attribute);
      EXCEPT
        Super.InternalError (info) =>
          RAISE InternalError(
                  ErrorSupport.Propagate("ChgMgmtGraph.PutAttribute",
                                         "Super.InternalError", info));
      END;

      IF graph.logging THEN
        fw := VolatileDelta.New().init(forward := TRUE);
        bw := VolatileDelta.New().init(forward := FALSE);
        (* write forward log *)
        GraphCommand.PutAttribute(
          fcom, node, attributeNo, start, len, attribute);
        fw.addCommand(fcom);
        INC(graph.fwcount);

        (* write backward log, exists the attribute ? *)
        IF attrNos.in(attributeNo) THEN

          (* if it exists then ....*)
          IF olen < len THEN
            (* ...  cut, if the older one is shorter and ...*)
            GraphCommand.TruncateAttribute(
              bcom, node, attributeNo, start + olen);
            bw.addCommand(bcom);
            INC(graph.bwcount);
            (* if the attribute is shorter than start -> olen=0 and
               truncation to start doesn't change anything. *)
          END;
          IF olen > 0 THEN
            (* ...put old text between start and olen *)
            GraphCommand.PutAttribute(
              bcom, node, attributeNo, start, olen, old);
            bw.addCommand(bcom);
            INC(graph.bwcount);
          END;
        ELSE
          (* if no attribute existed before, delete the inserted when
             undoing *)
          GraphCommand.DeleteAttribute(bcom, node, attributeNo);
          bw.addCommand(bcom);
          INC(graph.bwcount);
        END;
        attrNos.dispose();

        (* really writing *)
        TRY
          graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
          VolatileDelta.Free(fw);
          VolatileDelta.Free(bw);
        EXCEPT
          Log.InternalError (info) =>
            RAISE
              LogError(ErrorSupport.Propagate("ChgMgmtGraph.PutAttribute",
                                              "Log.InternalError ", info));
        END;
      END;
    END;
  END PutAttribute;

PROCEDURE DeleteAttribute (graph: T; node: Node.T; attributeNo: CARDINAL)
  RAISES {Super.NotOwner, Access.Locked, Super.NodeNotFound, LogError,
          InternalError} =
  VAR
    olen      : CARDINAL       := LAST(CARDINAL);
    old       : TEXT;
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    TRY
      IF Super.T.getAllAttributeNumbers(graph, node).in(attributeNo) THEN
        IF graph.logging THEN
          old := Super.T.getAttribute(graph, node, attributeNo, 0, olen);
          olen := Text.Length(old);
        END;
        Super.T.deleteAttribute(graph, node, attributeNo);

        IF graph.logging THEN
          fw := VolatileDelta.New().init(forward := TRUE);
          bw := VolatileDelta.New().init(forward := FALSE);
          (* write forward log *)
          GraphCommand.DeleteAttribute(fcom, node, attributeNo);
          fw.addCommand(fcom);
          INC(graph.fwcount);

          (* write backward log*)
          GraphCommand.PutAttribute(bcom, node, attributeNo, 0, olen, old);
          bw.addCommand(bcom);
          INC(graph.bwcount);

          TRY
            graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
            VolatileDelta.Free(fw);
            VolatileDelta.Free(bw);
          EXCEPT
            Log.InternalError (info) =>
              RAISE LogError(ErrorSupport.Propagate(
                               "ChgMgmtGraph.DeleteAttribute",
                               "Log.InternalError ", info));
          END;
        END;
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraph.DeleteAttribute",
                                       "Super.InternalError", info));
    END;
  END DeleteAttribute;

PROCEDURE TruncateAttribute (graph      : T;
                             node       : Node.T;
                             attributeNo: CARDINAL;
                             size       : CARDINAL  )
  RAISES {Super.NotOwner, Access.Locked, Super.NodeNotFound, LogError,
          InternalError} =
  VAR
    olen      : CARDINAL       := LAST(CARDINAL) - size;
    old       : TEXT;
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    (* get all text that will be thrown away *)
    TRY
      IF Super.T.getAllAttributeNumbers(graph, node).in(attributeNo) THEN
        IF graph.logging THEN
          old :=
            Super.T.getAttribute(graph, node, attributeNo, size, olen);
          olen := Text.Length(old);
        END;

        Super.T.truncateAttribute(graph, node, attributeNo, size);

        IF graph.logging AND olen > 0 THEN
          fw := VolatileDelta.New().init(forward := TRUE);
          bw := VolatileDelta.New().init(forward := FALSE);
          (* write forward log *)
          GraphCommand.TruncateAttribute(fcom, node, attributeNo, size);
          fw.addCommand(fcom);
          INC(graph.fwcount);

          (* write backward log *)
          GraphCommand.PutAttribute(
            bcom, node, attributeNo, size, olen, old);
          bw.addCommand(bcom);
          INC(graph.bwcount);

          TRY
            graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
            VolatileDelta.Free(fw);
            VolatileDelta.Free(bw);
          EXCEPT
            Log.InternalError (info) =>
              RAISE LogError(ErrorSupport.Propagate(
                               "ChgMgmtGraph.TruncateAttribute",
                               "Log.InternalError ", info));
          END;
        END;
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraph.TruncateAttribute",
                                       "Super.InternalError", info));
    END;
  END TruncateAttribute;


PROCEDURE PutIndex (graph: T; node: Node.T; indexNo: CARDINAL; index: TEXT)
  RAISES {Super.NotOwner, Access.Locked, Super.NodeNotFound, LogError,
          Super.IndexUsed, InternalError} =
  VAR
    old       : TEXT;
    found     : BOOLEAN;
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    TRY
      IF graph.logging THEN
        old := Super.T.getIndex(graph, node, indexNo, found);
      END;
      Super.T.putIndex(graph, node, indexNo, index);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraph.PutIndex", "Super.InternalError", info));
    END;

    IF graph.logging THEN
      fw := VolatileDelta.New().init(forward := TRUE);
      bw := VolatileDelta.New().init(forward := FALSE);
      (* write forward log *)
      GraphCommand.PutIndex(fcom, node, indexNo, Text.Length(index), index);
      fw.addCommand(fcom);
      INC(graph.fwcount);

      (* write backward log *)
      IF found THEN
        (* put old value *)
        GraphCommand.PutIndex(bcom, node, indexNo, Text.Length(old), old);
        bw.addCommand(bcom);
        INC(graph.bwcount);
      ELSE
        (* delete new index *)
        GraphCommand.DeleteIndex(
          bcom, node, indexNo, Text.Length(index), index);
        bw.addCommand(bcom);
        INC(graph.bwcount);
      END;

      TRY
        graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
        VolatileDelta.Free(fw);
        VolatileDelta.Free(bw);
      EXCEPT
        Log.InternalError (info) =>
          RAISE LogError(
                  ErrorSupport.Propagate(
                    "ChgMgmtGraph.PutIndex", "Log.InternalError ", info));
      END;
    END;
  END PutIndex;


PROCEDURE DeleteIndex (graph  : T;
                       node   : Node.T;
                       indexNo: CARDINAL;
                       index  : TEXT      )
  RAISES {Super.NotOwner, Access.Locked, Super.NodeNotFound,
          Super.IndexUnused, LogError, InternalError} =
  VAR
    old       : TEXT;
    len       : CARDINAL;
    indexFound: BOOLEAN        := TRUE;
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    IF graph.logging THEN
      TRY
        old := Super.T.getIndex(graph, node, indexNo, indexFound);
      EXCEPT
        Super.InternalError (info) =>
          RAISE InternalError(
                  ErrorSupport.Propagate("ChgMgmtGraph.DeleteIndex",
                                         "Super.InternalError", info));
      END;
    END;

    IF indexFound THEN
      TRY
        Super.T.deleteIndex(graph, node, indexNo, index);
      EXCEPT
        Super.InternalError (info) =>
          RAISE InternalError(
                  ErrorSupport.Propagate("ChgMgmtGraph.DeleteIndex",
                                         "Super.InternalError", info));
      END;

      IF graph.logging THEN
        fw := VolatileDelta.New().init(forward := TRUE);
        bw := VolatileDelta.New().init(forward := FALSE);
        len := Text.Length(index);

        (* write forward log *)
        GraphCommand.DeleteIndex(fcom, node, indexNo, len, index);
        fw.addCommand(fcom);
        INC(graph.fwcount);

        (* write backward log *)
        GraphCommand.PutIndex(bcom, node, indexNo, Text.Length(old), old);
        bw.addCommand(bcom);
        INC(graph.bwcount);

        TRY
          graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
          VolatileDelta.Free(fw);
          VolatileDelta.Free(bw);
        EXCEPT
          Log.InternalError (info) =>
            RAISE
              LogError(ErrorSupport.Propagate("ChgMgmtGraph.DeleteIndex",
                                              "Log.InternalError ", info));
        END;
      END;
    END;
  END DeleteIndex;


PROCEDURE CreateEdge (graph: T; source, target: Node.T; label: CARDINAL)
  RAISES {Super.NotOwner, Access.Locked, Super.NodeNotFound, LogError,
          InternalError} =
  VAR
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    TRY
      IF NOT Super.T.existsEdge(graph, source, target, label) THEN
        Super.T.createEdge(graph, source, target, label);

        IF graph.logging THEN
          fw := VolatileDelta.New().init(forward := TRUE);
          bw := VolatileDelta.New().init(forward := FALSE);
          (* write forward log *)
          IF graph.pool.sameLogGroup(source.graph, target.graph) THEN
            GraphCommand.CreateEdge(fcom, source, target, label);
            fw.addCommand(fcom);
            INC(graph.fwcount);

            (* write backward log *)
            GraphCommand.DeleteEdge(bcom, source, target, label);
            bw.addCommand(bcom);
            INC(graph.bwcount);

            TRY
              graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
              VolatileDelta.Free(fw);
              VolatileDelta.Free(bw);
            EXCEPT
              Log.InternalError (info) =>
                RAISE
                  LogError(
                    ErrorSupport.Propagate("ChgMgmtGraph.CreateEdge",
                                           "Log.InternalError ", info));
            END;
          END;
        END;
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraph.CreateEdge", "Super.InternalError", info));
    | ChgMgmtGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraph.CreateEdge",
                              "ChgMgmtGraphPool.InternalError", info));
    END;
  END CreateEdge;


PROCEDURE DeleteEdge (graph: T; source, target: Node.T; label: CARDINAL)
  RAISES {Super.NotOwner, Access.Locked, Super.NodeNotFound, LogError,
          InternalError} =
  VAR
    fcom, bcom: GraphCommand.T;
    fw, bw    : VolatileDelta.T;
  (* Volatile deltas never raise Delta.Error *)
  <* FATAL Delta.Error *>
  BEGIN
    TRY
      IF Super.T.existsEdge(graph, source, target, label) THEN
        Super.T.deleteEdge(graph, source, target, label);

        IF graph.logging THEN
          fw := VolatileDelta.New().init(forward := TRUE);
          bw := VolatileDelta.New().init(forward := FALSE);
          IF graph.pool.sameLogGroup(source.graph, target.graph) THEN

            (* write forward log *)
            GraphCommand.DeleteEdge(fcom, source, target, label);
            fw.addCommand(fcom);
            INC(graph.fwcount);

            (* write backward log *)
            GraphCommand.CreateEdge(bcom, source, target, label);
            bw.addCommand(bcom);
            INC(graph.bwcount);

            TRY
              graph.pool.getLog(graph.loghandle).appliedCommands(fw, bw);
              VolatileDelta.Free(fw);
              VolatileDelta.Free(bw);
            EXCEPT
              Log.InternalError (info) =>
                RAISE
                  LogError(
                    ErrorSupport.Propagate("ChgMgmtGraph.DeleteEdge",
                                           "Log.InternalError ", info));
            END;
          END;
        END;
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraph.DeleteEdge", "Super.InternalError", info));
    | ChgMgmtGraphPool.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraph.DeleteEdge",
                              "ChgMgmtGraphPool.InternalError", info));
    END;
  END DeleteEdge;


(***************************************************)
(* recovery - operations *)
(***************************************************)

PROCEDURE EndTransaction (graph: T; abort: BOOLEAN) =
  BEGIN
    TRY
      IF abort THEN
        PersistentGraphPool.T.abortTransaction(graph.pool);
      ELSE
        PersistentGraphPool.T.commitTransaction(graph.pool);
      END;
    EXCEPT
      PersistentGraphPool.InternalError,
          PersistentGraphPool.NotInTransaction => (* ignore *)
    END;
  END EndTransaction;

PROCEDURE SetCheckpoint (graph: T)
  RAISES {NoLog, Access.Locked, LogError, InternalError} =
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        LogEvents.SignalCheckpoint(
          graph.pool.getRuleEngineID(), graph.pool.getBaseName(),
          graph.pool, graph.number(), graph, TRUE,
          graph.pool.getTransactionLevel());

        PersistentGraphPool.T.beginTransaction(graph.pool);
        graph.pool.getLog(graph.loghandle).setCheckpoint();

        LogEvents.SignalCheckpoint(
          graph.pool.getRuleEngineID(), graph.pool.getBaseName(),
          graph.pool, graph.number(), graph, FALSE,
          graph.pool.getTransactionLevel());
      EXCEPT
        Log.InternalError (info) =>
          abort := TRUE;
          RAISE
            LogError(ErrorSupport.Propagate("ChgMgmtGraph.SetCheckpoint",
                                            "Log.InternalError", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.SetCheckpoint",
                            "PersistentGraphPool.InternalError", info));
      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
  END SetCheckpoint;

PROCEDURE SetCPLabel (graph: T; label: CARDINAL)
  RAISES {NoLog, Access.Locked, LogError, InternalError} =
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        PersistentGraphPool.T.beginTransaction(graph.pool);
        graph.pool.getLog(graph.loghandle).setCheckpointLabel(label);
      EXCEPT
        Log.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(
                  ErrorSupport.Propagate("ChgMgmtGraph.SetCPLabel",
                                         "Log.InternalError (al)", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.SetCPLabel",
                            "PersistentGraphPool.InternalError", info));
      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
  END SetCPLabel;

PROCEDURE RmCPLabel (graph: T)
  RAISES {NoLog, Access.Locked, LogError, InternalError} =
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        PersistentGraphPool.T.beginTransaction(graph.pool);
        graph.pool.getLog(graph.loghandle).removeCheckpointLabel();
      EXCEPT
        Log.InternalError (info) =>
          abort := TRUE;
          RAISE
            LogError(
              ErrorSupport.Propagate(
                "ChgMgmtGraph.RmCPLabel", "Log.InternalError", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.RmCPLabel",
                            "PersistentGraphPool.InternalError", info));
      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
  END RmCPLabel;


PROCEDURE Undo (graph: T; VAR N: CARDINAL)
  RAISES {NoLog, Access.Locked, LogError, InternalError} =
  VAR
    steps: CARDINAL := 0;
    bw   : Delta.T;
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        PersistentGraphPool.T.beginTransaction(graph.pool);
        WITH unit = graph.pool.getRuleEngineID() DO
          WHILE steps # N DO
            LogEvents.SignalUndo(
              unit, graph.pool.getBaseName(), graph.pool, graph.number(),
              graph, TRUE, graph.pool.getTransactionLevel());
            LogContexts.Set(unit, LogContexts.Context.Replay);
            LogContexts.Set(unit, LogContexts.Context.LinearReplay);

            TRY
              (* initialize undo of one checkpoint *)
              bw := graph.pool.getLog(graph.loghandle).undo();
              (* execute *)
              ExecuteDelta.F(graph.pool.getOpenGraphs(), bw);
              INC(steps);

              LogContexts.Clear(unit, LogContexts.Context.Replay);
              LogContexts.Clear(unit, LogContexts.Context.LinearReplay);
              LogEvents.SignalUndo(
                unit, graph.pool.getBaseName(), graph.pool, graph.number(),
                graph, FALSE, graph.pool.getTransactionLevel());
            EXCEPT
            | Log.NoSuchCheckpoint =>
                N := steps;
                LogContexts.Clear(unit, LogContexts.Context.Replay);
                LogContexts.Clear(unit, LogContexts.Context.LinearReplay);
            END;


          END;
          N := steps;
        END;
      EXCEPT
      | Super.NodeNotFound =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.Undo", "Super.NodeNotFound"));
      | Log.InternalError (info) =>
          abort := TRUE;
          RAISE
            LogError(ErrorSupport.Propagate(
                       "ChgMgmtGraph.Undo", "Log.InternalError", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.Undo",
                            "PersistentGraphPool.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate(
                           "ChgMgmtGraph.Undo",
                           "ExecuteDelta.InternalError", info));
      | ExecuteDelta.NotOpen =>
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.Undo", "Graph not open!"));
      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
  END Undo;

PROCEDURE Redo (graph: T; VAR N: CARDINAL)
  RAISES {NoLog, Access.Locked, LogError, InternalError} =
  VAR
    steps: CARDINAL := 0;
    fw   : Delta.T;
  VAR abort: BOOLEAN := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        WITH unit = graph.pool.getRuleEngineID() DO
          PersistentGraphPool.T.beginTransaction(graph.pool);
          WHILE steps # N DO
            LogEvents.SignalRedo(
              unit, graph.pool.getBaseName(), graph.pool, graph.number(),
              graph, TRUE, graph.pool.getTransactionLevel());
            LogContexts.Set(unit, LogContexts.Context.Replay);
            LogContexts.Set(unit, LogContexts.Context.LinearReplay);

            TRY
              (* initialize redo one operation *)
              fw := graph.pool.getLog(graph.loghandle).redo();
              (* execute them *)
              ExecuteDelta.F(graph.pool.getOpenGraphs(), fw);
              INC(steps);

              LogContexts.Clear(unit, LogContexts.Context.Replay);
              LogContexts.Clear(unit, LogContexts.Context.LinearReplay);
              LogEvents.SignalRedo(
                unit, graph.pool.getBaseName(), graph.pool, graph.number(),
                graph, FALSE, graph.pool.getTransactionLevel());
            EXCEPT
            | Log.NoSuchCheckpoint =>
                N := steps;
                LogContexts.Clear(unit, LogContexts.Context.Replay);
                LogContexts.Clear(unit, LogContexts.Context.LinearReplay);
            END;

          END;
          N := steps;
        END;
      EXCEPT
        Super.NodeNotFound =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.Redo", "Super.NodeNotFound"));
      | Log.InternalError (info) =>
          abort := TRUE;
          RAISE
            LogError(ErrorSupport.Propagate(
                       "ChgMgmtGraph.Redo", "Log.InternalError", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.Redo",
                            "PersistentGraphPool.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate(
                           "ChgMgmtGraph.Redo",
                           "ExecuteDelta.InternalError", info));
      | ExecuteDelta.NotOpen =>
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.Redo", "Graph not open!"));
      END;
    FINALLY
      EndTransaction(graph, abort);
    END;
  END Redo;

PROCEDURE RedoNext (graph: T)
  RAISES {NoLog, Access.Locked, LogError, NoSuchCheckpoint, NotInTreeMode,
          InternalError} =
  VAR
    fw   : Delta.T;
    abort: BOOLEAN := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        WITH unit = graph.pool.getRuleEngineID() DO
          PersistentGraphPool.T.beginTransaction(graph.pool);

          LogEvents.SignalRedoNext(
            unit, graph.pool.getBaseName(), graph.pool, graph.number(),
            graph, TRUE, graph.pool.getTransactionLevel());
          LogContexts.Set(unit, LogContexts.Context.Replay);

          (* initialize redo of one operation *)
          fw := graph.pool.getLog(graph.loghandle).redoNext();
          (* execute it *)
          ExecuteDelta.F(graph.pool.getOpenGraphs(), fw);

          LogContexts.Clear(unit, LogContexts.Context.Replay);
          LogEvents.SignalRedoNext(
            unit, graph.pool.getBaseName(), graph.pool, graph.number(),
            graph, FALSE, graph.pool.getTransactionLevel());
        END;
      EXCEPT
        Super.NodeNotFound =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.RedoNext", "Super.NodeNotFound"));
      | Log.NoSuchCheckpoint => abort := TRUE; RAISE NoSuchCheckpoint;
      | Log.ModeError => abort := TRUE; RAISE NotInTreeMode;
      | Log.InternalError (info) =>
          abort := TRUE;
          RAISE
            LogError(ErrorSupport.Propagate(
                       "ChgMgmtGraph.RedoNext", "Log.InternalError", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.RedoNext",
                            "PersistentGraphPool.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate(
                           "ChgMgmtGraph.RedoNext",
                           "ExecuteDelta.InternalError", info));
      | ExecuteDelta.NotOpen =>
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.RedoNext", "Graph not open!"));
      END;
    FINALLY
      LogContexts.Clear(
        graph.pool.getRuleEngineID(), LogContexts.Context.Replay);
      EndTransaction(graph, abort);
    END;
  END RedoNext;

PROCEDURE RedoPrev (graph: T)
  RAISES {NoLog, Access.Locked, LogError, NoSuchCheckpoint, NotInTreeMode,
          InternalError} =
  VAR
    fw   : Delta.T;
    abort: BOOLEAN := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        WITH unit = graph.pool.getRuleEngineID() DO
          PersistentGraphPool.T.beginTransaction(graph.pool);

          LogEvents.SignalRedoPrev(
            unit, graph.pool.getBaseName(), graph.pool, graph.number(),
            graph, TRUE, graph.pool.getTransactionLevel());
          LogContexts.Set(unit, LogContexts.Context.Replay);

          (* initialize redo of one operation *)
          fw := graph.pool.getLog(graph.loghandle).redoPrev();
          (* execute it *)
          ExecuteDelta.F(graph.pool.getOpenGraphs(), fw);

          LogContexts.Clear(unit, LogContexts.Context.Replay);
          LogEvents.SignalRedoPrev(
            unit, graph.pool.getBaseName(), graph.pool, graph.number(),
            graph, FALSE, graph.pool.getTransactionLevel());
        END;
      EXCEPT
        Super.NodeNotFound =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.RedoPrev", "Super.NodeNotFound"));
      | Log.NoSuchCheckpoint => abort := TRUE; RAISE NoSuchCheckpoint;
      | Log.ModeError => abort := TRUE; RAISE NotInTreeMode;
      | Log.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate("ChgMgmtGraph.RedoPrev",
                                                "Log.InternalErro", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.RedoPrev",
                            "PersistentGraphPool.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate(
                           "ChgMgmtGraph.RedoPrev",
                           "ExecuteDelta.InternalError", info));
      | ExecuteDelta.NotOpen =>
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.RedoPrev", "Graph not open!"));
      END;
    FINALLY
      LogContexts.Clear(
        graph.pool.getRuleEngineID(), LogContexts.Context.Replay);
      EndTransaction(graph, abort);
    END;
  END RedoPrev;

PROCEDURE RedoIth (graph: T; i: CARDINAL)
  RAISES {NoLog, Access.Locked, LogError, NoSuchCheckpoint, NotInTreeMode,
          InternalError} =
  VAR
    fw   : Delta.T;
    abort: BOOLEAN := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        WITH unit = graph.pool.getRuleEngineID() DO
          PersistentGraphPool.T.beginTransaction(graph.pool);

          LogEvents.SignalRedoIth(
            unit, graph.pool.getBaseName(), graph.pool, graph.number(),
            graph, TRUE, graph.pool.getTransactionLevel(), i);
          LogContexts.Set(unit, LogContexts.Context.Replay);

          (* initialize redo of one operation *)
          fw := graph.pool.getLog(graph.loghandle).redoIth(i);
          (* execute it *)
          ExecuteDelta.F(graph.pool.getOpenGraphs(), fw);

          LogContexts.Clear(unit, LogContexts.Context.Replay);
          LogEvents.SignalRedoIth(
            unit, graph.pool.getBaseName(), graph.pool, graph.number(),
            graph, FALSE, graph.pool.getTransactionLevel(), i);
        END;
      EXCEPT
        Super.NodeNotFound =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.RedoIth", "Super.NodeNotFound"));
      | Log.NoSuchCheckpoint => abort := TRUE; RAISE NoSuchCheckpoint;
      | Log.ModeError => abort := TRUE; RAISE NotInTreeMode;
      | Log.InternalError (info) =>
          abort := TRUE;
          RAISE
            LogError(ErrorSupport.Propagate(
                       "ChgMgmtGraph.RedoIth", "Log.InternalError", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.RedoIth",
                            "PersistentGraphPool.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate(
                           "ChgMgmtGraph.RedoIth",
                           "ExecuteDelta.InternalError", info));
      | ExecuteDelta.NotOpen =>
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.RedoIth", "Graph not open!"));
      END;
    FINALLY
      LogContexts.Clear(
        graph.pool.getRuleEngineID(), LogContexts.Context.Replay);
      EndTransaction(graph, abort);
    END;
  END RedoIth;

PROCEDURE Backstep (graph: T; VAR N: CARDINAL)
  RAISES {NoLog, Access.Locked, LogError, NoSuchCheckpoint, NotInTreeMode,
          InternalError} =
  VAR
    steps: CARDINAL := 0;
    delta: Delta.T;
    abort: BOOLEAN  := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        WITH unit = graph.pool.getRuleEngineID() DO
          PersistentGraphPool.T.beginTransaction(graph.pool);
          WHILE N > 0 DO

            LogEvents.SignalBackstep(
              unit, graph.pool.getBaseName(), graph.pool, graph.number(),
              graph, TRUE, graph.pool.getTransactionLevel());
            LogContexts.Set(unit, LogContexts.Context.Replay);

            delta := graph.pool.getLog(graph.loghandle).backstep();
            (* execute it *)
            ExecuteDelta.F(graph.pool.getOpenGraphs(), delta);
            INC(steps);
            DEC(N);

            LogContexts.Clear(unit, LogContexts.Context.Replay);
            LogEvents.SignalBackstep(
              unit, graph.pool.getBaseName(), graph.pool, graph.number(),
              graph, FALSE, graph.pool.getTransactionLevel());

          END;
          N := steps;
        END;
      EXCEPT
        Super.NodeNotFound =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.Backstep", "Super.NodeNotFound"));
      | Log.NoSuchCheckpoint => abort := TRUE; RAISE NoSuchCheckpoint;
      | Log.ModeError => abort := TRUE; RAISE NotInTreeMode;
      | Log.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(
                  ErrorSupport.Propagate(
                    "ChgMgmtGraph.Backstep", "Log.InternalError ", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.Backstep",
                            "PersistentGraphPool.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate(
                           "ChgMgmtGraph.Backstep",
                           "ExecuteDelta.InternalError", info));
      | ExecuteDelta.NotOpen =>
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.Backstep", "Graph not open!"));
      END;
    FINALLY
      LogContexts.Clear(
        graph.pool.getRuleEngineID(), LogContexts.Context.Replay);
      EndTransaction(graph, abort);
    END;
  END Backstep;

PROCEDURE Forstep (graph: T; VAR N: CARDINAL)
  RAISES {NoLog, Access.Locked, LogError, NoSuchCheckpoint, NotInTreeMode,
          InternalError} =
  VAR
    steps: CARDINAL := 0;
    delta: Delta.T;
    abort: BOOLEAN  := FALSE;
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        WITH unit = graph.pool.getRuleEngineID() DO
          PersistentGraphPool.T.beginTransaction(graph.pool);
          WHILE N > 0 DO

            LogEvents.SignalForstep(
              unit, graph.pool.getBaseName(), graph.pool, graph.number(),
              graph, TRUE, graph.pool.getTransactionLevel());
            LogContexts.Set(unit, LogContexts.Context.Replay);

            delta := graph.pool.getLog(graph.loghandle).forstep();
            (* execute it *)
            ExecuteDelta.F(graph.pool.getOpenGraphs(), delta);
            INC(steps);
            DEC(N);

            LogContexts.Clear(unit, LogContexts.Context.Replay);
            LogEvents.SignalForstep(
              unit, graph.pool.getBaseName(), graph.pool, graph.number(),
              graph, FALSE, graph.pool.getTransactionLevel());

          END;
          N := steps;
        END;
      EXCEPT
        Super.NodeNotFound =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.Forstep", "Super.NodeNotFound"));
      | Log.NoSuchCheckpoint => abort := TRUE; RAISE NoSuchCheckpoint;
      | Log.ModeError => abort := TRUE; RAISE NotInTreeMode;
      | Log.InternalError (info) =>
          abort := TRUE;
          RAISE
            LogError(ErrorSupport.Propagate(
                       "ChgMgmtGraph.Forstep", "Log.InternalError", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.Forstep",
                            "PersistentGraphPool.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate(
                           "ChgMgmtGraph.Forstep",
                           "ExecuteDelta.InternalError", info));
      | ExecuteDelta.NotOpen =>
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.Forstep", "Graph not open!"));
      END;
    FINALLY
      LogContexts.Clear(
        graph.pool.getRuleEngineID(), LogContexts.Context.Replay);
      EndTransaction(graph, abort);
    END;
  END Forstep;

PROCEDURE GotoCPLabel (graph: T; label: CARDINAL) RAISES {NoLog,
                                                          Access.Locked,
                                                          LogError,
                                                          NoSuchCheckpoint,
                                                          InternalError} =
  VAR
    dl   : DeltaList.T;
    delta: Delta.T;
    ok   : BOOLEAN;
    abort: BOOLEAN     := FALSE;
  (* Log initializes list *)
  <* FATAL DeltaList.NotInitialized *>
  BEGIN
    IF NOT graph.logging THEN RAISE NoLog END;
    TRY
      TRY
        WITH unit = graph.pool.getRuleEngineID() DO
          PersistentGraphPool.T.beginTransaction(graph.pool);
          dl := graph.pool.getLog(graph.loghandle).gotoCPLabel(label);
          LogContexts.Set(unit, LogContexts.Context.Replay);
          dl.loop();
          delta := dl.get(ok);
          WHILE ok DO
            ExecuteDelta.F(graph.pool.getOpenGraphs(), delta);
            delta := dl.get(ok);
          END;
          LogContexts.Clear(unit, LogContexts.Context.Replay);
        END;
      EXCEPT
        Super.NodeNotFound =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Create("ChgMgmtGraph.GotoCPLabel",
                                             "Super.NodeNotFound"));
      | Log.NoSuchCheckpoint => abort := TRUE; RAISE NoSuchCheckpoint;
      | Log.InternalError (info) =>
          abort := TRUE;
          RAISE
            LogError(ErrorSupport.Propagate("ChgMgmtGraph.GotoCPLabel",
                                            "Log.InternalError ", info));
      | Access.Locked => abort := TRUE; RAISE Access.Locked;
      | PersistentGraphPool.InternalError (info) =>
          abort := TRUE;
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraph.GotoCPLabel",
                            "PersistentGraphPool.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          abort := TRUE;
          RAISE LogError(ErrorSupport.Propagate(
                           "ChgMgmtGraph.GotoCPLabel",
                           "ExecuteDelta.InternalError", info));
      | ExecuteDelta.NotOpen =>
          RAISE LogError(ErrorSupport.Create(
                           "ChgMgmtGraph.GotoCPLabel", "Graph not open!"));
      END;
    FINALLY
      LogContexts.Clear(
        graph.pool.getRuleEngineID(), LogContexts.Context.Replay);
      EndTransaction(graph, abort);
    END;
  END GotoCPLabel;

BEGIN
END ChgMgmtGraph.
