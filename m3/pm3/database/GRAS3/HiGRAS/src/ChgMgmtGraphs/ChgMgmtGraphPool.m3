MODULE ChgMgmtGraphPool EXPORTS ChgMgmtGraphPool, InternChgMgmtGraphPool;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.20  1998/09/14 08:14:46  roland
    Modified code to remove compiler warnings.

    Revision 1.19  1998/08/27 16:12:17  roland
    Bugfixes for IntraCopyGraph.

    Revision 1.18  1998/08/27 12:31:02  roland
    Included simple mechanism to monitor log size.

    Revision 1.17  1998/08/12 10:58:06  roland
    Bugfix in RemoveGraph.

    Revision 1.16  1998/05/19 10:17:29  roland
    Support for log-groups implemented.

    Revision 1.15  1998/03/18 13:39:20  roland
    More slight modifications to local parameters (default values and
    parameter ordering)

    Revision 1.14  1998/03/18 12:13:16  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.13  1998/03/18 09:27:10  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.12  1998/03/17 14:13:53  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.11  1998/01/21 12:27:11  roland
    No log when access mode is read-only.

    Revision 1.10  1997/10/31 14:22:31  roland
    Adapted to new RuleEngine.

    Revision 1.9  1997/10/24 14:06:02  renehuel
    Bugfix in CopyGraph.

    Revision 1.8  1997/10/17 16:45:01  renehuel
    bugfix in deltaCopyGraph.

    Revision 1.7  1997/10/08 14:57:26  roland
    Bugfix for backward copy in DeltaCopyGraph.

    Revision 1.6  1997/09/18 08:22:25  roland
    Delta-Disposal cleaned up.

    Revision 1.5  1997/07/21 10:37:26  roland
    Adaptions to new set-implementation (no more SetExceptions). Now use free
    memory management of deltas and sets.

    Revision 1.4  1997/05/06 13:22:29  roland
    Bugfix: consider transaction when closing a graph.

    Revision 1.3  1997/05/05 10:48:49  roland
    Bugfix in open routine for graphs. OpenIntern added to overrides list.

    Revision 1.2  1997/04/24 14:29:13  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 14:09:22  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

*)
(***************************************************************************)

IMPORT ErrorSupport;
IMPORT Pathname, Text, IntIntTbl;


IMPORT ChgMgmtNames, Names, ChgMgmtOpenGraphs;
IMPORT TextCursorSet, PageFile, Access, Transaction;
IMPORT ClientInfoSeq, VersionDelta, Delta;
IMPORT PersistentGraphPool AS Super;
IMPORT PersistentGraph, Log, LogStack, VolatileLog, DeltaList,
       PersistentLog, VirtualResource, InternPersistentGraphPool,
       VolatileDelta, ExecuteDelta;

REVEAL
  T = Internal BRANDED OBJECT
        graphs                 : ChgMgmtNames.T;
        openGraphs             : ChgMgmtOpenGraphs.T;
        openLogs               : OLIArray;
        chgMgmtTransactionDepth: CARDINAL;
        groupCache             : IntIntTbl.T;
      OVERRIDES
        (* public *)
        open              := Open;
        close             := Close;
        beginTransaction  := BeginTransaction;
        commitTransaction := CommitTransaction;
        abortTransaction  := AbortTransaction;

        deleteGraph   := DeleteGraph;
        copyGraph     := IntraCopyGraph;
        renameGraph   := RenameGraph;
        existsGraph   := ExistsGraph;
        graphNumber   := GraphNumber;
        graphInUse    := GraphInUse;
        getGraphUser  := GetGraphUser;
        getGraphs     := GetGraphs;
        getNeighbours := GetNeighbours;

        createLogGroup     := CreateLogGroup;
        deleteLogGroup     := DeleteLogGroup;
        existsLogGroup     := ExistsLogGroup;
        addToLogGroup      := AddToLogGroup;
        removeFromLogGroup := RemoveFromLogGroup;
        logGroupMember     := LogGroupMember;
        getLogGroup        := GetLogGroup;

        checkOutGraph  := CheckOutGraph;
        checkInGraph   := CheckInGraph;
        deltaCopyGraph := DeltaCopyGraph;
        reorganisation := Reorganisation;

        getTransactionLevel := GetTransactionLevel;

        (* internal *)
        openPG         := OpenPG;
        closePG        := ClosePG;
        openLog        := OpenLog;
        closeLog       := CloseLog;
        getLog         := GetLog;
        openIntern     := OpenIntern;
        loginToNames   := LoginToNames;
        sameLogGroup   := SameLogGroup;
        getOpenGraphs  := GetOpenGraphs;
        bindGraphToLog := BindGraphToLog;
      END;

CONST Exclusive = Access.Mode.ReadWriteExclusive;

PROCEDURE ErrAbort (pool: T) =
  BEGIN
    TRY
      Super.T.abortTransaction(pool);
    EXCEPT
      Super.NotInTransaction =>  (* ignore *)
    | Super.InternalError =>     (* ignore *)
    END;
  END ErrAbort;

PROCEDURE PGMode (pool: T; access: PersistentGraph.AccessMode):
  Access.Mode =
  BEGIN
    CASE access OF
      PersistentGraph.AccessMode.Inherit => RETURN pool.getAccessMode();
    | PersistentGraph.AccessMode.ReadWriteShared =>
        RETURN Access.Mode.ReadWriteShared;
    | PersistentGraph.AccessMode.ReadOnlyShared =>
        RETURN Access.Mode.ReadOnlyShared;
    | PersistentGraph.AccessMode.ReadWriteExclusive =>
        RETURN Access.Mode.ReadWriteExclusive;
    END;
  END PGMode;

PROCEDURE DeltaName (path: Pathname.T; from, to: Pathname.T): Pathname.T =
  BEGIN
    RETURN Pathname.Join(
             path, Pathname.Last(from) & "_to_" & Pathname.Last(to), NIL);
  END DeltaName;

PROCEDURE GroupLogName (group: Pathname.T): Pathname.T =
  BEGIN
    RETURN group & ".group.log0.";
  END GroupLogName;

PROCEDURE GraphLogName (graph: Pathname.T): Pathname.T =
  BEGIN
    RETURN graph & ".graph.log0.";
  END GraphLogName;

PROCEDURE ExistsGroupLog (pool: T; groupname: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {InternalError} =
  BEGIN
    TRY
      RETURN
        PersistentLog.LogFilesExist(pool, GroupLogName(groupname), local);
    EXCEPT
      Log.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraphPool.ExistsGroupLog",
                                       "Log.InternalError", info));
    END;
  END ExistsGroupLog;

PROCEDURE ExistsGraphLog (pool: T; graph: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {InternalError} =
  BEGIN
    TRY
      RETURN PersistentLog.LogFilesExist(pool, GraphLogName(graph), local);

    EXCEPT
      Log.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraphPool.ExistsGraphLog",
                                       "Log.InternalError", info));
    END;
  END ExistsGraphLog;

PROCEDURE OpenIntern (pool  : T;
                      name  : Pathname.T;
                      access: Access.Mode;
                      new   : BOOLEAN      ): T
  RAISES {Access.Denied, PageFile.NoAccess} =
  BEGIN
    pool := Super.T.openIntern(pool, name, access, new);
    pool.chgMgmtTransactionDepth := 0;
    pool.openGraphs := NEW(ChgMgmtOpenGraphs.T).init();
    OLIInit(pool.openLogs);
    GroupCacheInit(pool.groupCache);
    RETURN pool;
  END OpenIntern;

PROCEDURE LoginToNames (pool: T; names: ChgMgmtNames.T)
  RAISES {Access.Locked, InternalError} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      pool.graphs := names;
      Super.T.loginToNames(pool, names);
      ChgMgmtNames.T.login(names, pool, ".GRAS");
      Super.T.commitTransaction(pool);
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraphPool.LoginToNames",
                                       "Names.InternalError", info));
    | Super.InternalError (info) =>
        ErrAbort(pool);
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraphPool.Open", "Super.InternalError", info));
    | Access.Locked => ErrAbort(pool); RAISE Access.Locked;
    END;
  END LoginToNames;


(* Implementation of ChgMgmtGraphPool-Interface *)

PROCEDURE Open (pool  : T;
                name  : Pathname.T;
                access: Access.Mode;
                new   : BOOLEAN      ): T
  RAISES {InternalError, Access.Locked, Access.Denied, PageFile.NoAccess} =
  BEGIN
    pool := OpenIntern(pool, name, access, new);
    LoginToNames(pool, NEW(ChgMgmtNames.T));
    RETURN pool;
  END Open;

PROCEDURE Close (pool: T) RAISES {InternalError} =
  BEGIN
    TRY
      IF pool.graphs # NIL THEN pool.graphs.logout(); END;
      Super.T.close(pool);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.Close",
                              "PersistentGraphPool.FatalError", info));
    END;
  END Close;

<* FATAL LogStack.Full, LogStack.Undefined *>

PROCEDURE GetTransactionLevel (pool: T): Transaction.Level =
  BEGIN
    RETURN pool.chgMgmtTransactionDepth;
  END GetTransactionLevel;

PROCEDURE BeginTransaction (pool: T) RAISES {InternalError} =
  VAR
    h   : INTEGER;
    mode: LogMode;
    log : Log.T;
  (* Volatile Logs never raise these exceptions *)
  <* FATAL Access.Locked, Log.InternalError *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      INC(pool.chgMgmtTransactionDepth);
      OLILoop(pool.openLogs);
      h := OLINext(pool.openLogs);
      WHILE h # -1 DO
        mode := OLIGetLogMode(pool.openLogs, h);
        IF mode # LogMode.None THEN
          WITH stack = OLIGetLogStack(pool.openLogs, h) DO
            log := OLIGetLog(pool.openLogs, h);
            stack.push(log);
            log := NEW(VolatileLog.T).init();
            IF mode = LogMode.Tree THEN
              log.setMode(Log.Mode.Tree);
            ELSE
              log.setMode(Log.Mode.Sequence);
            END;
            OLIPutLog(pool.openLogs, h, log);
          END;
        END;
        h := OLINext(pool.openLogs);
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.BeginTransaction",
                              "PersistentGraphPool.InternalError", info));
    END;
  END BeginTransaction;

PROCEDURE CommitOneLog (pool: T; handle: CARDINAL) RAISES {InternalError} =
  VAR
    slog, log: Log.T;
    fwl, bwl : DeltaList.T;
    fwd, bwd : Delta.T;
    ok       : BOOLEAN;
    stack    : LogStack.T;
  <* FATAL LogStack.Empty, DeltaList.NotInitialized *>
  BEGIN
    TRY
      IF OLIGetLogMode(pool.openLogs, handle) # LogMode.None THEN
        stack := OLIGetLogStack(pool.openLogs, handle);
        log := OLIGetLog(pool.openLogs, handle);
        IF NOT stack.isEmpty() THEN
          slog := stack.pop();
          IF slog # NIL AND log # NIL THEN
            log.getActualPath(fwl, bwl);
            (* read fwl from top, bwl from bottom *)
            fwl.loop();
            bwl.gotoBottom();
            bwd := bwl.getCurrent(ok);
            fwd := fwl.get(ok);
            WHILE ok DO
              slog.appliedCommands(fwd, bwd);
              IF ISTYPE(fwd, VolatileDelta.T) THEN
                VolatileDelta.Free(fwd);
                VolatileDelta.Free(bwd);
              END;
              bwl.gotoPrevious();
              bwd := bwl.getCurrent(ok);
              fwd := fwl.get(ok);
            END;
          END;
          OLIPutLog(pool.openLogs, handle, slog);
        END;
      END;
    EXCEPT
      Log.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraphPool.CommitOneLog",
                                       "Log.InternalError", info));
    | Access.Locked =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ChgMgmtGraphPool.CommitOneLog", "Access.Locked"));
    END;
  END CommitOneLog;

PROCEDURE CommitTransaction (pool: T)
  RAISES {InternalError, NotInTransaction} =
  VAR h: INTEGER;
  BEGIN
    TRY
      IF pool.chgMgmtTransactionDepth = 0 THEN RAISE NotInTransaction END;
      DEC(pool.chgMgmtTransactionDepth);
      OLILoop(pool.openLogs);
      h := OLINext(pool.openLogs);
      WHILE h # -1 DO
        CommitOneLog(pool, h);
        h := OLINext(pool.openLogs);
      END;
      (* if stack was empty, the call to Super.T.commitTransaction will
         fail *)
      Super.T.commitTransaction(pool);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.CommitTransaction",
                              "Super.InternalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    END;
  END CommitTransaction;

PROCEDURE AbortTransaction (pool: T)
  RAISES {InternalError, NotInTransaction} =
  VAR
    stack    : LogStack.T;
    log, slog: Log.T;
    h        : INTEGER;
    fwl, bwl : DeltaList.T;
    fwd, bwd : Delta.T;
    ok       : BOOLEAN;
  <* FATAL LogStack.Empty *>
  <* FATAL Access.Locked, Log.InternalError *>
  <* FATAL DeltaList.NotInitialized *>
  BEGIN
    TRY
      IF pool.chgMgmtTransactionDepth = 0 THEN RAISE NotInTransaction END;
      DEC(pool.chgMgmtTransactionDepth);
      OLILoop(pool.openLogs);
      h := OLINext(pool.openLogs);
      WHILE h # -1 DO
        IF OLIGetLogMode(pool.openLogs, h) # LogMode.None THEN
          log := OLIGetLog(pool.openLogs, h);
          IF log # NIL THEN
            log.getActualPath(fwl, bwl);
            (* read fwl from top, bwl from bottom *)
            fwl.loop();
            bwl.gotoBottom();
            bwd := bwl.getCurrent(ok);
            fwd := fwl.get(ok);
            WHILE ok DO
              IF ISTYPE(fwd, VolatileDelta.T) THEN
                VolatileDelta.Free(fwd);
                VolatileDelta.Free(bwd);
              END;
              bwl.gotoPrevious();
              bwd := bwl.getCurrent(ok);
              fwd := fwl.get(ok);
            END;
          END;
          stack := OLIGetLogStack(pool.openLogs, h);
          IF NOT stack.isEmpty() THEN
            slog := stack.pop();
            OLIPutLog(pool.openLogs, h, slog);
          END;
        END;
        h := OLINext(pool.openLogs);
      END;
      Super.T.abortTransaction(pool);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraphPool.AbortTransaction",
                                       "Super.InternalError", info));
    | Super.NotInTransaction => RAISE NotInTransaction;
    END;
  END AbortTransaction;


PROCEDURE DeleteGraph (pool    : T;
                       baseName: Pathname.T;
                       local   : BOOLEAN      := FALSE)
  RAISES {NotExistent, InUse, InternalError, Access.Locked} =
  VAR
    ind, outd : TextCursorSet.T;
    otcd, ctod: Pathname.T      := NIL;
    d, invname: TEXT;
    ok        : BOOLEAN;
    extRels   : TextCursorSet.T;

  PROCEDURE InvisibleName (name: TEXT): TEXT
    RAISES {ChgMgmtNames.InternalError, Access.Locked} =
    CONST InvisiblePath = ".invisible";
    BEGIN
      RETURN Pathname.Join(InvisiblePath, Pathname.Last(name),
                           pool.graphs.uniqueName());
    END InvisibleName;

  BEGIN
    TRY
      IF Super.T.graphInUse(pool, baseName, local) THEN RAISE InUse END;

      IF pool.graphs.existsGraph(baseName, local) THEN
        IF pool.graphs.isCheckoutOriginal(baseName, local) THEN
          RAISE NotExistent;
        END;

        (* Check if graph belongs to a log group.  If so, it can only be
           deleted if no log for this group exists or the graph is the only
           member of the group.  If allowed, remove from group and delete
           logs if necessary. *)
        VAR
          groupname: Pathname.T;
          members  : TextCursorSet.T;
        BEGIN
          groupname := pool.graphs.getLogGroup(baseName, local);
          IF groupname # NIL THEN
            (* graph belongs to a log group *)
            IF ExistsGroupLog(pool, groupname, local) THEN
              members := pool.graphs.getLogGroupMembers(groupname, local);
              IF members.card() > 1 THEN
                members.dispose();
                RAISE InUse;
              END;
              members.dispose();
              PersistentLog.DeleteOldLogFiles(
                pool, GroupLogName(groupname), local);
            END;
            pool.graphs.delFromLogGroup(groupname, baseName, local);
          ELSE
            (* graph is not in a log group *)
            IF ExistsGraphLog(pool, baseName, local) THEN
              PersistentLog.DeleteOldLogFiles(
                pool, GraphLogName(baseName), local);
            END;
          END;
        END;

        (* Find all outgoing deltas that are not checkout-deltas *)
        outd := pool.graphs.getOutDeltas(baseName, local);
        IF pool.graphs.isCheckoutCopy(baseName, local) THEN
          pool.graphs.getCheckoutDeltas(baseName, local, otcd, ctod);
          outd.deleteElement(ctod, ok);
        END;
        IF NOT outd.isEmpty() THEN
          (* The graph is used for reconstruction, just make it and all its
             outgoing deltas invisible *)
          pool.graphs.mkInvisible(baseName, local);
          invname := InvisibleName(baseName);
          Super.T.renameGraph(pool, baseName, invname, local);
          outd.loop();
          d := outd.get(ok);
          WHILE ok DO
            invname := InvisibleName(d);
            Super.T.renameGraph(pool, d, invname, local);
            d := outd.get(ok);
          END;
          outd.dispose();

        ELSE
          (* Remove all incoming deltas *)
          ind := pool.graphs.getInDeltas(baseName, local);
          ind.loop();
          d := ind.get(ok);
          WHILE ok DO
            Super.T.deleteFile(pool, d, local);
            pool.graphs.removeDelta(d, local);
            d := ind.get(ok);
          END;
          ind.dispose();

          IF pool.graphs.isDirect(baseName, local) THEN
            Super.T.deleteGraph(pool, baseName, local);
          ELSE
            pool.graphs.removeGraph(baseName, local, extRels);
          END;
        END;
        (* Finally, remove any checkout deltas *)
        IF ctod # NIL THEN pool.graphs.removeDelta(ctod, local); END;
        IF otcd # NIL THEN pool.graphs.removeDelta(otcd, local); END;
      ELSE
        RAISE NotExistent;
      END;
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphSystem.DeleteGraph",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown =>
        RAISE InternalError(ErrorSupport.Create(
                              "ChgMgmtGraphSystem.DeleteGraph",
                              "ChgMgmtNames.Unknown"));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.DeleteGraph",
                              "PersistentGraphPool.InternalError", info));
    | Super.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ChgMgmtGraphPool.InvisibleName", "Super.NotExistent"));
    | Super.Existent =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ChgMgmtGraphPool.InvisibleName", "Super.Existent"));
    | Super.InUse => RAISE InUse;
    | PageFile.NoAccess (msg) =>
        RAISE
          InternalError(ErrorSupport.Create("ChgMgmtGraphPool.DeleteGraph",
                                            "PageFile.NoAccess:" & msg));
    | Log.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraphPool.DeleteGraph",
                                       "Log.InternalError", info));
    | VirtualResource.FatalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.DeleteGraph",
                                   "VirtualResource.FatalError", info));
    END;
  END DeleteGraph;

PROCEDURE IntraCopyGraph (pool      : T;
                          sourceName: Pathname.T;
                          destName  : Pathname.T;
                          embedded  : BOOLEAN;
                          local     : BOOLEAN      := FALSE)
  RAISES {InUse, NotExistent, Existent, InternalError, Access.Locked} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.T.beginTransaction(pool);
      IF pool.graphs.existsGraph(sourceName, local) THEN
        IF NOT pool.graphs.existsGraph(destName, local) THEN
          IF pool.graphs.isDirect(sourceName, local) THEN
            Super.T.copyGraph(pool, sourceName, destName, embedded, local);
          ELSE                   (* source is indirect *)
            Reconstruct(pool, sourceName, destName, local);
          END;
          pool.graphs.copy(sourceName, destName, local);
        ELSE
          ErrAbort(pool);
          RAISE Existent;
        END;
      ELSE
        ErrAbort(pool);
        RAISE NotExistent;
      END;
      Super.T.commitTransaction(pool);
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
      ErrAbort(pool);
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphSystem.CopyGraph",
                                   "ChgMgmtNames.InternalError", info));
    | Super.InternalError (info) =>
      ErrAbort(pool);
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.CopyGraph",
                              "PersistentGraphPool.InternalError", info));
    | Super.NotExistent =>
      ErrAbort(pool);
        RAISE
          InternalError(ErrorSupport.Create("ChgMgmtGraphPool.CopyGraph",
                                            "Super.NotExistent"));
    | Super.Existent =>
      ErrAbort(pool);
        RAISE
          InternalError(ErrorSupport.Create(
                          "ChgMgmtGraphPool.CopyGraph", "Super.Existent"));
    | Super.InUse => ErrAbort(pool); RAISE InUse;
    END;
  END IntraCopyGraph;

PROCEDURE RenameGraph (pool   : T;
                       oldName: Pathname.T;
                       newName: Pathname.T;
                       local  : BOOLEAN      := FALSE)
  RAISES {NotExistent, Existent, Access.Locked, InUse, InternalError} =
  BEGIN
    TRY
      IF NOT pool.graphs.existsGraph(oldName, local) THEN
        RAISE NotExistent;
      ELSIF pool.graphs.existsGraph(newName, local) THEN
        RAISE Existent;
      ELSE
        IF Super.T.graphInUse(pool, oldName, local) THEN RAISE InUse END;
        IF pool.graphs.isDirect(oldName, local) THEN
          Super.T.renameGraph(pool, oldName, newName, local);
        ELSE
          pool.graphs.renameGraph(oldName, newName, local);
        END;
      END;
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphSystem.RenameGraph",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown =>
        RAISE InternalError(ErrorSupport.Create(
                              "ChgMgmtGraphSystem.RenameGraph",
                              "ChgMgmtNames.Unknown"));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.RenameGraph",
                              "PersistentGraphPool.InternalError", info));
    | Super.NotExistent =>
        RAISE
          InternalError(ErrorSupport.Create("ChgMgmtGraphPool.RenameGraph",
                                            "Super.NotExistent"));
    | Super.Existent =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ChgMgmtGraphPool.RenameGraph", "Super.Existent"));
    | Super.InUse => RAISE InUse;
    END;
  END RenameGraph;

PROCEDURE CreateLogGroup (pool     : T;
                          groupname: Pathname.T;
                          local    : BOOLEAN      := FALSE)
  RAISES {Existent, Access.Locked, InternalError} =
  BEGIN
    TRY
      IF pool.graphs.existsLogGroup(groupname, local) THEN
        RAISE Existent;
      ELSE
        pool.graphs.insertLogGroup(groupname, local);
      END;
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.CreateLogGroup",
                                   "ChgMgmtNames.InternalError", info));
    END;
  END CreateLogGroup;

PROCEDURE DeleteLogGroup (pool     : T;
                          groupname: Pathname.T;
                          local    : BOOLEAN      := FALSE)
  RAISES {InUse, Access.Locked, InternalError} =
  VAR mem: TextCursorSet.T;
  <* FATAL ChgMgmtNames.Unknown, ChgMgmtNames.NotEmpty *>
  BEGIN
    TRY
      IF pool.graphs.existsLogGroup(groupname, local) THEN
        mem := pool.graphs.getLogGroupMembers(groupname, local);
        IF mem.card() > 0 THEN
          mem.dispose();
          RAISE InUse;
        ELSE
          mem.dispose();
          pool.graphs.removeLogGroup(groupname, local);
        END;
      END;
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.DeleteLogGroup",
                                   "ChgMgmtNames.InternalError", info));
    END;
  END DeleteLogGroup;

PROCEDURE ExistsLogGroup (pool     : T;
                          groupname: Pathname.T;
                          local    : BOOLEAN      := FALSE): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN pool.graphs.existsLogGroup(groupname, local);
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.ExistsLogGroup",
                                   "ChgMgmtNames.InternalError", info));
    END;
  END ExistsLogGroup;

PROCEDURE AddToLogGroup (pool            : T;
                         groupname, graph: Pathname.T;
                         local           : BOOLEAN      := FALSE)
  RAISES {NotExistent, NotAllowed, Access.Locked, InternalError} =
  VAR mem, neighs: TextCursorSet.T;
  <* FATAL ChgMgmtNames.Unknown, Super.NotExistent *>
  BEGIN
    TRY
      (* Check if group and graph exists *)
      IF NOT pool.graphs.existsLogGroup(groupname, local) THEN
        RAISE NotExistent
      END;
      IF NOT pool.graphs.existsGraph(graph, local) THEN
        RAISE NotExistent
      END;
      (* Check whether graph belongs to a different group already *)
      IF pool.graphs.getLogGroup(graph, local) # NIL THEN
        RAISE NotAllowed;
      END;
      (* Check if graph has no log *)
      IF ExistsGraphLog(pool, graph, local) THEN RAISE NotAllowed END;
      (* Check if log group has no log *)
      IF ExistsGroupLog(pool, groupname, local) THEN
        (* log exists, check if graph has external relations to any of the
           groups members *)
        mem := pool.graphs.getLogGroupMembers(groupname, local);
        neighs := Super.T.getNeighbours(pool, graph, local);
        neighs.intersection(mem);
        mem.dispose();
        IF neighs.card() # 0 THEN neighs.dispose(); RAISE NotAllowed; END;
        neighs.dispose();
      END;
      (* All checks successful.  Insert graph in group *)
      pool.graphs.addToLogGroup(groupname, graph, local);
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.DeleteLogGroup",
                                   "ChgMgmtNames.InternalError", info));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.DeleteLogGroup",
                              "PersistentGraphPool.InternalError", info));
    END;
  END AddToLogGroup;

PROCEDURE RemoveFromLogGroup (pool            : T;
                              groupname, graph: Pathname.T;
                              local           : BOOLEAN      := FALSE)
  RAISES {NotExistent, NotAllowed, Access.Locked, InternalError} =
  <* FATAL Names.Undeclared, Names.Unknown *>
  BEGIN
    TRY
      (* Check if group and graph exists *)
      IF NOT pool.graphs.existsLogGroup(groupname, local) THEN
        RAISE NotExistent
      END;
      IF NOT pool.graphs.existsGraph(graph, local) THEN
        RAISE NotExistent
      END;
      (* Check if log group has no log *)
      IF ExistsGroupLog(pool, groupname, local) THEN RAISE NotAllowed; END;
      pool.graphs.delFromLogGroup(groupname, graph, local);
      GroupCacheRemove(pool.groupCache, pool.graphs.id(graph, local));
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.RemoveFromLogGroup",
                              "ChgMgmtNames.InternalError", info));
    | Names.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.RemoveFromLogGroup",
                              "Names.InternalError", info));
    END;
  END RemoveFromLogGroup;

PROCEDURE LogGroupMember (pool     : T;
                          groupname: Pathname.T;
                          local    : BOOLEAN      := FALSE):
  TextCursorSet.T RAISES {NotExistent, Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN pool.graphs.getLogGroupMembers(groupname, local);
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.LogGroupMember",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown => RAISE NotExistent;
    END;
  END LogGroupMember;

PROCEDURE GetLogGroup (pool: T; graph: Pathname.T; local: BOOLEAN := FALSE):
  Pathname.T RAISES {NotExistent, Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN pool.graphs.getLogGroup(graph, local);
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.LogGroupMember",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown => RAISE NotExistent;
    END;
  END GetLogGroup;

PROCEDURE SameLogGroup (pool: T; g1, g2: CARDINAL): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    IF g1 = g2 THEN
      RETURN TRUE
    ELSE
      WITH lg1 = FindGroup(pool, g1),
           lg2 = FindGroup(pool, g2)  DO
        RETURN lg1 = lg2 AND lg1 # 0;
      END;
    END;
  END SameLogGroup;

PROCEDURE CheckOutGraph (pool : T;
                         name : Pathname.T;
                         as   : Pathname.T;
                         local: BOOLEAN      := FALSE)
  RAISES {Existent, NotExistent, InUse, InternalError, Access.Locked} =
  VAR
    otcdelta, ctodelta, path: Pathname.T;
    otcv, ctov              : VersionDelta.T;
  BEGIN
    TRY
      IF NOT pool.graphs.existsGraph(name, local) THEN
        RAISE NotExistent;
      ELSIF pool.graphs.existsGraph(as, local) THEN
        RAISE Existent;
      ELSE
        IF pool.graphs.isDirect(name, local) THEN
          Super.T.copyGraph(
            pool, name, as, embedded := TRUE, local := local);
        ELSE
          Reconstruct(pool, name, as, local);
        END;
        (* Create VersionDeltas *)
        path := Pathname.Prefix(as);
        otcdelta := DeltaName(path, name, as);
        ctodelta := DeltaName(path, as, name);
        pool.graphs.checkOut(name, as, otcdelta, ctodelta, local);
        otcv := NEW(VersionDelta.T).open(
                  pool, otcdelta, Exclusive, new := TRUE, local := local);
        otcv.close();
        ctov := NEW(VersionDelta.T).open(
                  pool, ctodelta, Exclusive, new := TRUE, local := local);
        ctov.close();
      END;
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphSystem.CheckOutGraph",
                                   "ChgMgmtNames.InternalError", info));
    | Access.Denied (msg) =>
        RAISE InternalError(ErrorSupport.Create(
                              "ChgMgmtGraphPool.CheckOutGraph",
                              "Access.Denied:" & msg));
    | ChgMgmtNames.Unknown =>
        RAISE InternalError(
                ErrorSupport.Create("ChgMgmtGraphSystem.CheckOutGraph",
                                    "ChgMgmtNames.Unknown"));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.CheckOutGraph",
                              "PersistentGraphPool.InternalError", info));
    | Delta.Error (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtGraphSystem.CheckOutGraph", "Delta.Error", info));
    | Super.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ChgMgmtGraphPool.CheckOutGraph", "Super.NotExistent"));
    | Super.Existent =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ChgMgmtGraphPool.CheckOutGraph", "Super.Existent"));
    | Super.InUse => RAISE InUse;
    END;
  END CheckOutGraph;

PROCEDURE CheckInGraph (pool: T; name: Pathname.T; local: BOOLEAN := FALSE)
  RAISES {Access.Locked, NotExistent, InUse, InternalError, NoCheckOutCopy} =
  VAR
    indeltas, outdeltas              : TextCursorSet.T;
    cvdelta, checkoutdelta           : VersionDelta.T  := NIL;
    cname, otcname, ctoname, origname: Pathname.T;
    found                            : BOOLEAN;

  BEGIN
    TRY
      IF NOT pool.graphs.isCheckoutCopy(name, local) THEN
        RAISE NoCheckOutCopy;
      END;

      IF Super.T.graphInUse(pool, name, local) THEN RAISE InUse; END;
      origname := pool.graphs.getOriginal(name, local);

      IF Super.T.graphInUse(pool, origname, local) THEN RAISE InUse; END;

      (* Apply changes *)
      Super.T.deleteGraph(pool, origname, local);
      Super.T.copyGraph(
        pool, name, origname, embedded := TRUE, local := local);

      (* Adjust deltas of original *)
      indeltas := pool.graphs.getInDeltas(origname, local);
      outdeltas := pool.graphs.getOutDeltas(origname, local);
      IF NOT (indeltas.isEmpty() AND outdeltas.isEmpty()) THEN
        pool.graphs.getCheckoutDeltas(name, local, otcname, ctoname);
        (* otcname and ctoname need not be adjustet *)
        indeltas.deleteElement(ctoname, found);
        outdeltas.deleteElement(otcname, found);

        checkoutdelta :=
          NEW(VersionDelta.T).open(
            pool, ctoname, Exclusive, new := FALSE, local := local);
        (* Adjust all incoming deltas *)
        indeltas.loop();
        cname := indeltas.get(found);
        WHILE found DO
          (* append commands to forward delta *)
          cvdelta :=
            NEW(VersionDelta.T).open(
              pool, cname, Exclusive, new := FALSE, local := local);
          cvdelta.append(checkoutdelta);
          pool.graphs.addDeltaCosts(cname, local, checkoutdelta.costs());
          cvdelta.close();
          cvdelta := NIL;
          cname := indeltas.get(found);
        END;
        checkoutdelta.close();


        checkoutdelta :=
          NEW(VersionDelta.T).open(
            pool, otcname, Exclusive, new := FALSE, local := local);
        (* Adjust all outgoing deltas *)
        outdeltas.loop();
        cname := outdeltas.get(found);
        WHILE found DO
          (* prepend commands to backward delta *)
          cvdelta :=
            NEW(VersionDelta.T).open(
              pool, cname, Exclusive, new := FALSE, local := local);
          cvdelta.prepend(checkoutdelta);
          pool.graphs.addDeltaCosts(cname, local, checkoutdelta.costs());
          cvdelta.close();
          cname := outdeltas.get(found);
        END;
        checkoutdelta.close();
        checkoutdelta := NIL;

      END;
      indeltas.dispose();
      outdeltas.dispose();
    EXCEPT
    | Access.Denied (msg) =>
        RAISE InternalError(ErrorSupport.Create(
                              "ChgMgmtGraphPool.CheckInGraph",
                              "Access.Denied: " & msg));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.CheckInGraph",
                              "PersistentGraphPool.InternalError", info));
    | ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphSystem.CheckInGraph",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown =>
        RAISE InternalError(
                ErrorSupport.Create("ChgMgmtGraphSystem.CheckInGraph",
                                    "ChgMgmtNames.Unknown"));
    | Delta.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraphSystem.CheckInGraph", "Delta.Error", info));
    | Super.NotExistent => RAISE NotExistent;
    | Super.Existent =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ChgMgmtGraphPool.CheckInGraph", "Super.Existent"));
    | Super.InUse => RAISE InUse;
    END;
  END CheckInGraph;

PROCEDURE DeltaCopyGraph (pool   : T;
                          source : Pathname.T;
                          target : Pathname.T;
                          forward: BOOLEAN;
                          local  : BOOLEAN      := FALSE)
  RAISES {Access.Locked, InUse, Existent, NotExistent, InternalError} =
  VAR
    delta : Pathname.T;
    vdelta: VersionDelta.T;
  BEGIN
    TRY
      IF NOT pool.graphs.existsGraph(source, local) THEN
        RAISE NotExistent;
      ELSIF pool.graphs.existsGraph(target, local) THEN
        RAISE Existent;
      ELSIF Super.T.graphInUse(pool, source, local) THEN
        RAISE InUse;
      ELSE
        IF forward THEN
          pool.graphs.copy(source, target, local);
          delta := DeltaName(Pathname.Prefix(target), source, target);
          pool.graphs.insertDelta(source, target, delta, local);
        ELSE
          IF pool.graphs.isDirect(source, local) THEN
            VirtualResource.T.renameFile(pool, source, target, local);
          END;
          pool.graphs.copy(source, target, local);
          delta := DeltaName(Pathname.Prefix(source), target, source);
          (* Note: order is target, source *)
          pool.graphs.insertDelta(target, source, delta, local);
        END;
        vdelta := NEW(VersionDelta.T).open(
                    pool, delta, Exclusive, new := TRUE, local := local);
        vdelta.close();
      END;
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.DeltaCopyGraph",
                                   "ChgMgmtNames.InternalError", info));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.DeltaCopyGraph",
                              "PersistentGraphPool.InternalError", info));
    | Access.Denied (msg) =>
        RAISE InternalError(
                ErrorSupport.Create("ChgMgmtGraphPool.DeltaCopyGraph",
                                    "Access.Denied: " & msg));
    | ChgMgmtNames.Unknown =>
        RAISE InternalError(
                ErrorSupport.Create("ChgMgmtGraphSystem.DeltaCopyGraph",
                                    "ChgMgmtNames.Unknown"));
    | VirtualResource.FatalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphSystem.DeltaCopyGraph",
                                   "VirtualResource.FatalError", info));
    | Delta.Error =>
        RAISE InternalError(
                ErrorSupport.Create(
                  "ChgMgmtGraphSystem.DeltaCopyGraph", "Delta.Error"));
    | PageFile.NoAccess (msg) =>
        RAISE InternalError(
                ErrorSupport.Create("ChgMgmtGraphPool.DeltaCopyGraph",
                                    "PageFile.NoAccess : " & msg));
    END;
  END DeltaCopyGraph;

(* operations on the collection of graphs and deltas *)

PROCEDURE Reorganisation (<* UNUSED *> pool: T) RAISES {} =
  BEGIN
  END Reorganisation;
(* Reorganisation of the administration.  For example the informations
   about deleted graphs, which are no longer needed for the reconstruction
   of other graphs, may be deleted.  This Operation can be applied to the
   system at arbitrary times and time intervalls.  It will only clean up
   bookkeeping information and has no other effects on the system. *)

PROCEDURE ExistsGraph (pool    : T;
                       baseName: Pathname.T;
                       local   : BOOLEAN      := FALSE): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN pool.graphs.existsGraph(baseName, local);
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphSystem.ExistsGraph",
                                   "ChgMgmtNames.InternalError", info));
    END;
  END ExistsGraph;

PROCEDURE GraphInUse (pool    : T;
                      baseName: Pathname.T;
                      local   : BOOLEAN      := FALSE): BOOLEAN
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN Super.T.graphInUse(pool, baseName, local);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.GraphInUse",
                              "PersistentGraphPool.InternalError", info));
    END;
  END GraphInUse;

PROCEDURE GetGraphUser (pool: T; baseName: Pathname.T): ClientInfoSeq.T
  RAISES {InternalError} =
  BEGIN
    TRY
      RETURN Super.T.getGraphUser(pool, baseName);
    EXCEPT
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphSystem.GetGraphUser",
                              "PersistentGraphPool.InternalError", info));
    END;
  END GetGraphUser;

PROCEDURE GetNeighbours (pool : T;
                         graph: Pathname.T;
                         local: BOOLEAN      := FALSE): TextCursorSet.T
  RAISES {NotExistent, InternalError, Access.Locked} =
  BEGIN
    TRY
      RETURN Super.T.getNeighbours(pool, graph, local);
    EXCEPT
      Super.NotExistent => RAISE NotExistent;
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.GetNeighbours",
                              "PersistentGraphPool.InternalError", info));
    END;
  END GetNeighbours;

PROCEDURE GetGraphs (pool: T; local: BOOLEAN := FALSE): TextCursorSet.T
  RAISES {InternalError, Access.Locked} =
  BEGIN
    TRY
      RETURN pool.graphs.getGraphs(local, st := ChgMgmtNames.Visible);
    EXCEPT
      ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphSystem.GetGraphs",
                                   "ChgMgmtNames.InternalError", info));
    END;
  END GetGraphs;

PROCEDURE GraphNumber (pool: T; name: Pathname.T; local: BOOLEAN := FALSE):
  CARDINAL RAISES {InternalError, NotExistent, Access.Locked} =
  BEGIN
    TRY
      RETURN Super.T.graphNumber(pool, name, local);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.GetGraphNumber",
                              "PersistentGraphPool.InternalError", info));
    | Super.NotExistent => RAISE NotExistent;
    END;
  END GraphNumber;

PROCEDURE CopyGraph (sourcePool : T;
                     sourceGraph: Pathname.T;
                     sourcelocal: BOOLEAN;
                     targetPool : T;
                     targetGraph: Pathname.T;
                     targetlocal: BOOLEAN     )
  RAISES {InUse, NotExistent, Existent, InTransaction, InternalError,
          Access.Locked} =
  <* FATAL Super.NotInTransaction *>
  BEGIN
    TRY
      Super.CopyGraph(sourcePool, sourceGraph, sourcelocal, targetPool,
                      targetGraph, targetlocal);
      Super.T.beginTransaction(targetPool);
      targetPool.graphs.insertGraph(targetGraph, targetlocal);
      Super.T.commitTransaction(targetPool);
    EXCEPT
      Super.InUse => RAISE InUse;
    | Super.Existent => RAISE Existent;
    | Super.NotExistent => RAISE NotExistent;
    | Super.InTransaction => RAISE InTransaction;
    | Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtGraphPool.CopyGraph",
                                       "Super.InternalError", info));
    | ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.CopyGraph",
                                   "ChgMgmtNames.InternalError", info));
    END;
  END CopyGraph;


(* Intern interface *)

PROCEDURE Reconstruct (pool   : T;
                       what   : Pathname.T;
                       newname: Pathname.T;
                       local  : BOOLEAN     ) RAISES {InternalError} =
  VAR gr: PersistentGraph.T := NIL;

  PROCEDURE RecReconstruct (what: Pathname.T) RAISES {InternalError} =
    VAR
      delta, source: Pathname.T;
      vd           : VersionDelta.T;
    BEGIN
      TRY
        delta := pool.graphs.getMinDelta(what, local);
        source := pool.graphs.getDeltaSource(delta, local);
        IF pool.graphs.isDirect(source, local) THEN
          Super.T.copyGraph(
            pool, source, newname, TRUE (*embedded*), local);
          gr :=
            NEW(PersistentGraph.T).open(
              pool, newname, PersistentGraph.AccessMode.ReadWriteExclusive,
              new := FALSE, local := FALSE, errorChecks := TRUE);
        ELSE
          RecReconstruct(source);
        END;
        vd := NEW(VersionDelta.T).open(
                pool, delta, Exclusive, new := FALSE, local := FALSE);
        ExecuteDelta.F(pool.openGraphs, vd);
        vd.close();
        vd := NIL;
      EXCEPT
      | Access.Denied (msg) =>
          RAISE InternalError(
                  ErrorSupport.Create("ChgMgmtGraphPool.RecReconstruct",
                                      "Access.Denied: " & msg));
      | PersistentGraph.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "ChgMgmtGraphPool.RecReconstruct",
                                "PersistentGraph.InternalError", info));
      | Super.InternalError (info) =>
          RAISE
            InternalError(ErrorSupport.Propagate(
                            "ChgMgmtGraphPool.Reconstruct",
                            "PersistentGraphPool.InternalError", info));
      | ChgMgmtNames.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "ChgMgmtGraphPool.Reconstruct",
                                "ChgMgmtNames.InternalError", info));
      | ExecuteDelta.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "ChgMgmtGraphPool.Reconstruct",
                                "ExecuteDelta.InternalError", info));
      | ChgMgmtNames.Unknown =>
          RAISE InternalError(ErrorSupport.Create(
                                "ChgMgmtGraphPool.Reconstruct",
                                "ChgMgmtNames.Unknown"));
      | Access.Locked =>
          RAISE InternalError(
                  ErrorSupport.Create(
                    "ChgMgmtGraphPool.Reconstruct", "Access.Locked"));
      | PageFile.NoAccess (msg) =>
          RAISE InternalError(
                  ErrorSupport.Create("ChgMgmtGraphPool.Reconstruct",
                                      "PageFile.NoAccess: " & msg));
      | Delta.Error (info) =>
          RAISE InternalError(
                  ErrorSupport.Propagate(
                    "ChgMgmtGraphPool.Reconstruct", "Delta.Error", info));
      | PersistentGraph.NodeNotFound =>
          RAISE InternalError(
                  ErrorSupport.Create("ChgMgmtGraphPool.RecReconstruct",
                                      "PersistentGraph.NodeNotFound"));
      | PersistentGraph.InUse =>
          RAISE InternalError(
                  ErrorSupport.Create("ChgMgmtGraphPool.RecReconstruct",
                                      "PersistentGraph.InternalError"));
      | PersistentGraph.NotExistent =>
          RAISE InternalError(
                  ErrorSupport.Create("ChgMgmtGraphPool.RecReconstruct",
                                      "PersistentGraph.NotExistent"));
      | Super.NotExistent =>
          RAISE InternalError(ErrorSupport.Create(
                                "ChgMgmtGraphPool.RecReconstruct",
                                "Super.NotExistent"));
      | Super.Existent =>
          RAISE InternalError(
                  ErrorSupport.Create(
                    "ChgMgmtGraphPool.RecReconstruct", "Super.Existent"));
      | Super.InUse =>
          RAISE InternalError(
                  ErrorSupport.Create(
                    "ChgMgmtGraphPool.RecReconstruct", "Super.InUse"));
      | ExecuteDelta.NotOpen =>
          RAISE InternalError(
                  ErrorSupport.Create("ChgMgmtGraphPool.RecReconstruct",
                                      "ExecuteDelta.NotOpen"));
      END;
    END RecReconstruct;

  BEGIN
    TRY
      RecReconstruct(what);
      PersistentGraph.T.close(gr);
    EXCEPT
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.Reconstruct",
                              "PersistentGraph.InternalError", info));
    END;
  END Reconstruct;


(* Interface procedures of internal interface *)
PROCEDURE OpenPG (pool       : T;
                  baseName   : Pathname.T;
                  access     : PersistentGraph.AccessMode;
                  errorChecks: BOOLEAN;
                  local      : BOOLEAN;
                  graph      : PersistentGraph.T           ):
  PersistentGraph.T
  RAISES {Access.Denied, InternalError, PageFile.NoAccess, Access.Locked} =
  VAR
    workname: Pathname.T;
    gnumber : CARDINAL;

  PROCEDURE IsOpen (pool: T; name: Pathname.T; local: BOOLEAN): BOOLEAN
    RAISES {Access.Locked, ChgMgmtNames.InternalError,
            ChgMgmtNames.Unknown, Super.InternalError} =
    BEGIN
      WITH wname = pool.graphs.getWorkingName(name, local) DO
        IF wname = NIL THEN
          (* just to be sure: check name *)
          RETURN Super.T.graphInUse(pool, name, local);
        ELSE
          RETURN Super.T.graphInUse(pool, wname, local);
        END;
      END;
    END IsOpen;

  <* FATAL Names.Undeclared, Names.Unknown, ChgMgmtOpenGraphs.NotOpen *>
  BEGIN
    TRY
      IF NOT Super.T.existsGraph(pool, baseName, local) THEN
        (* A new graph (never needs reconstruction) *)
        graph := PersistentGraph.T.open(
                   graph, pool, baseName, access, new := TRUE,
                   local := local, errorChecks := errorChecks);
        pool.graphs.insertGraph(baseName, local);
        pool.graphs.setWorkingName(baseName, local, baseName);
        gnumber := pool.graphs.id(baseName, local);
        pool.openGraphs.createEntry(gnumber, graph, baseName, baseName, 1);
      ELSE
        gnumber := pool.graphs.id(baseName, local);
        IF pool.openGraphs.exists(gnumber) THEN
          (* An existing graph that was opened by this client before *)
          WITH counter = pool.openGraphs.getOpenCounter(gnumber) DO
            pool.openGraphs.putOpenCounter(gnumber, counter + 1);
          END;
          graph := pool.openGraphs.getGraph(gnumber);
        ELSIF IsOpen(pool, baseName, local) THEN
          (* An existing graph that was opened by some other client *)
          workname := pool.graphs.getWorkingName(baseName, local);
          graph := PersistentGraph.T.open(
                     graph, pool, workname, access, new := FALSE,
                     local := local, errorChecks := errorChecks);
          pool.openGraphs.createEntry(
            gnumber, graph, baseName, workname, 1);
        ELSE
          (* An existing graph, we are the first to open it *)
          IF NOT pool.graphs.isDirect(baseName, local) THEN
            (* find a unique name for reconstructed copy *)
            REPEAT
              workname :=
                baseName & ".reconstructed." & pool.graphs.uniqueName();
            UNTIL NOT Super.T.existsFile(pool, workname, local);
            Reconstruct(pool, baseName, workname, local);
          ELSE
            workname := baseName;
          END;
          pool.graphs.setWorkingName(baseName, local, workname);
          graph := PersistentGraph.T.open(
                     graph, pool, workname, access, new := FALSE,
                     local := local, errorChecks := errorChecks);
          pool.openGraphs.createEntry(
            gnumber, graph, baseName, workname, 1);
        END;
      END;
      RETURN graph;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraphPool.OpenPG", "Super.InternalError", info));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.OpenPG",
                              "PersistentGraph.InternalError", info));
    | PersistentGraph.InUse =>
        RAISE InternalError(ErrorSupport.Create("ChgMgmtGraphPool.OpenPG",
                                                "PersistentGraph.InUse"));
    | PersistentGraph.NotExistent =>
        RAISE InternalError(ErrorSupport.Create(
                              "ChgMgmtGraphPool.OpenPG",
                              "PersistentGraph.NotExistent"));
    | ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.OpenPG",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown =>
        RAISE InternalError(ErrorSupport.Create("ChgMgmtGraphPool.OpenPG",
                                                "ChgMgmtNames.Unknown"));
    | VirtualResource.FatalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.OpenPG",
                                   "VirtualResource.FatalError", info));
    | Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraphPool.OpenPG", "Names.InternalError", info));
    END;
  END OpenPG;

PROCEDURE ClosePG (pool: T; graph: PersistentGraph.T)
  RAISES {InternalError} =
  VAR
    indeltas, outdeltas     : TextCursorSet.T;
    cvdelta                 : VersionDelta.T;
    actfwdpath, actbwdpath  : DeltaList.T;
    actdelta                : Delta.T;
    workname, visname, cname: Pathname.T;
    found, deltafound       : BOOLEAN;
    loghandle               : CARDINAL;
    log                     : Log.T;
    logMode                 : LogMode;
    local                   : BOOLEAN;
    gnumber                 : CARDINAL;
    logname                 : Pathname.T;

  <* FATAL DeltaList.NotInitialized *>

  BEGIN
    TRY
      local := graph.local();
      gnumber := graph.number();
      workname := pool.openGraphs.getWorkingName(gnumber);
      visname := pool.openGraphs.getVisibleName(gnumber);
      loghandle := pool.openGraphs.getLogHandle(gnumber);
      log := OLIGetLog(pool.openLogs, loghandle);
      logMode := OLIGetLogMode(pool.openLogs, loghandle);
      logname := OLIGetName(pool.openLogs, loghandle);

      (* Adapt version deltas only if graph is not in a log group *)
      IF NOT (Text.Equal(visname, logname)) AND logMode # LogMode.None
           AND log # NIL THEN
        IF local OR Super.T.getGraphUser(pool, workname).size() = 1 THEN
          (* Last one adapts version deltas *)
          indeltas := pool.graphs.getInDeltas(visname, local);
          outdeltas := pool.graphs.getOutDeltas(visname, local);
          IF NOT (indeltas.isEmpty() AND outdeltas.isEmpty()) THEN
            log.getActualPath(actfwdpath, actbwdpath);

            (* Adjust all incoming deltas *)
            indeltas.loop();
            cname := indeltas.get(found);
            WHILE found DO
              (* append commands to forward delta *)
              cvdelta :=
                NEW(VersionDelta.T).open(
                  pool, cname, Exclusive, new := FALSE, local := local);
              actfwdpath.loop();
              actdelta := actfwdpath.get(deltafound);
              WHILE deltafound DO
                cvdelta.append(actdelta);
                pool.graphs.addDeltaCosts(cname, local, actdelta.costs());
                actdelta := actfwdpath.get(deltafound);
              END;
              cvdelta.close();
              cvdelta := NIL;
              cname := indeltas.get(found);
            END;

            (* Adjust all outgoing deltas *)
            outdeltas.loop();
            cname := outdeltas.get(found);
            WHILE found DO
              (* prepend commands to backward delta *)
              cvdelta :=
                NEW(VersionDelta.T).open(
                  pool, cname, Exclusive, new := FALSE, local := local);
              actbwdpath.loop();
              actdelta := actbwdpath.get(deltafound);
              WHILE deltafound DO
                cvdelta.prepend(actdelta);
                pool.graphs.addDeltaCosts(cname, local, actdelta.costs());
                actdelta := actbwdpath.get(deltafound);
              END;
              cvdelta.close();
              cvdelta := NIL;
              cname := outdeltas.get(found);
            END;
          END;
          indeltas.dispose();
          outdeltas.dispose();
        END;
      END;

      PersistentGraph.T.close(graph);
      IF NOT pool.graphs.isDirect(visname, local) THEN
        IF NOT Super.T.graphInUse(pool, workname, local) THEN
          (* Last one removes reconstructed copy *)
          Super.T.deleteGraph(pool, workname, local);
          pool.graphs.setWorkingName(visname, local, NIL);
        END;
      END;

      WITH openCount = pool.openGraphs.getOpenCounter(gnumber) DO
        IF openCount = 1 THEN
          pool.openGraphs.free(gnumber);
        ELSE
          pool.openGraphs.putOpenCounter(gnumber, openCount - 1);
        END;
      END;
      graph := NIL;

    EXCEPT
      Super.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtGraphPool.ClosePG", "Super.InternalError", info));
    | Log.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate("ChgMgmtGraphPool.ClosePG",
                                               "Log.InternalError", info));
    | Access.Denied (msg) =>
        RAISE InternalError(ErrorSupport.Create("ChgMgmtGraphPool.ClosePG",
                                                "Access.Denied: " & msg));
    | Access.Locked =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "ChgMgmtGraphPool.ClosePG", "Access.Locked"));
    | Delta.Error (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtGraphPool.ClosePG", "Delta.Error", info));
    | ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.ClosePG",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown =>
        RAISE InternalError(ErrorSupport.Create("ChgMgmtGraphPool.ClosePG",
                                                "ChgMgmtNames.Unknown"));
    | PersistentGraph.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtGraphPool.ClosePG",
                              "PersistentGraph.InternalError", info));
    | Super.InUse =>
        RAISE
          InternalError(ErrorSupport.Create("ChgMgmtGraphPool.ClosePG",
                                            "PersistentGraphPool.InUse"));
    | Super.NotExistent =>
        RAISE InternalError(
                ErrorSupport.Create("ChgMgmtGraphPool.ClosePG",
                                    "PersistentGraphPool.NotExistent"));
    END;
  END ClosePG;

PROCEDURE BindGraphToLog (pool: T; graph: CARDINAL; loghandle: CARDINAL) =
  BEGIN
    pool.openGraphs.putLogHandle(graph, loghandle);
  END BindGraphToLog;

PROCEDURE GetOpenGraphs (pool: T): ChgMgmtOpenGraphs.T =
  BEGIN
    RETURN pool.openGraphs;
  END GetOpenGraphs;

PROCEDURE OpenLog (    pool           : T;
                       graph, logGroup: Pathname.T;
                       local          : BOOLEAN;
                       access         : PersistentGraph.AccessMode;
                   VAR mode           : LogMode                     ):
  CARDINAL
  RAISES {Access.Locked, PageFile.NoAccess, Access.Denied, InternalError} =
  VAR
    log        : Log.T;
    logstack   : LogStack.T;
    lmOrd      : CARDINAL;
    logMode    : LogMode;
    logfilename: Pathname.T;
    loghandle  : CARDINAL;
    qname      : Pathname.T;


  PROCEDURE InsertInOLIList (pool    : T;
                             name    : Pathname.T;
                             filename: Pathname.T;
                             local   : BOOLEAN;
                             mode    : LogMode;
                             log     : Log.T;
                             stack   : LogStack.T  ): CARDINAL =
    VAR handle: CARDINAL;
    BEGIN
      OLIReserve(pool.openLogs, handle);
      OLIPutName(pool.openLogs, handle, name);
      OLIPutFilename(pool.openLogs, handle, filename);
      OLIPutLocal(pool.openLogs, handle, local);
      OLIPutLogMode(pool.openLogs, handle, mode);
      OLIPutLog(pool.openLogs, handle, log);
      OLIPutLogStack(pool.openLogs, handle, stack);
      OLIPutOpenCounter(pool.openLogs, handle, 1);
      RETURN handle;
    END InsertInOLIList;

  PROCEDURE LogAlreadyOpen (    pool  : T;
                                log   : Pathname.T;
                                local : BOOLEAN;
                            VAR handle: CARDINAL    ): BOOLEAN RAISES {} =
    VAR
      found: BOOLEAN;
      h    : INTEGER;
    BEGIN
      OLILoop(pool.openLogs);
      h := OLINext(pool.openLogs);
      found := FALSE;
      WHILE NOT found AND h # -1 DO
        IF Text.Equal(OLIGetName(pool.openLogs, h), log)
             AND OLIGetLocal(pool.openLogs, h) = local THEN
          handle := h;
          found := TRUE;
        END;
        h := OLINext(pool.openLogs);
      END;
      RETURN found;
    END LogAlreadyOpen;

  PROCEDURE LogIsOpen (pool: T; log: Pathname.T; local: BOOLEAN): BOOLEAN
    RAISES {Access.Locked, ChgMgmtNames.InternalError} =
    VAR mode: INTEGER;
    <* FATAL ChgMgmtNames.Unknown *>
    BEGIN
      mode := pool.graphs.getLogMode(log, local);
      RETURN mode > 0;
    END LogIsOpen;

  PROCEDURE CreateLogAndLogStack (    logfilename: Pathname.T;
                                  VAR log        : Log.T;
                                  VAR stack      : LogStack.T;
                                  VAR mode       : LogMode;
                                      new        : BOOLEAN;
                                      local      : BOOLEAN;
                                      setMode    : BOOLEAN     )
    RAISES {Access.Locked, Log.InternalError, PageFile.NoAccess,
            Access.Denied} =
    BEGIN
      IF mode # LogMode.None THEN
        stack := NEW(LogStack.T).init();
        WITH accessMode = PGMode(pool, access) DO
          IF accessMode = Access.Mode.ReadOnlyShared THEN
            log := NIL;
          ELSE
            log := NEW(PersistentLog.T).open(
                     pool, logfilename, accessMode, new, local);
          END;
        END;
        IF log = NIL THEN
          mode := LogMode.None;
          stack := NIL;
        ELSE
          IF setMode THEN
            IF mode = LogMode.Tree THEN
              log.setMode(Log.Mode.Tree);
            ELSE
              log.setMode(Log.Mode.Sequence);
            END;
          ELSE
            IF log.getMode() = Log.Mode.Tree THEN
              mode := LogMode.Tree;
            ELSE
              mode := LogMode.Linear;
            END;
          END;
          (* push log on stack and create as many VolatileLogs as
             transaction depth requires. *)
          FOR i := 1 TO pool.chgMgmtTransactionDepth DO
            stack.push(log);
            log := NEW(VolatileLog.T).init();
            IF mode = LogMode.Tree THEN
              log.setMode(Log.Mode.Tree);
            ELSE
              log.setMode(Log.Mode.Sequence);
            END;
          END;
        END;
      ELSE
        stack := NIL;
        log := NIL;
      END;
    END CreateLogAndLogStack;

  BEGIN                          (* OpenLog *)
    TRY
      IF logGroup = NIL THEN
        logfilename := GraphLogName(graph);
        qname := graph;          (* use graphname for queries *)
      ELSE
        logfilename := GroupLogName(logGroup);
        qname := logGroup;       (* use groupname for queries *)
      END;
      IF LogAlreadyOpen(pool, qname, local, loghandle) THEN
        (* An existing log that was opened by this client before *)
        logMode := OLIGetLogMode(pool.openLogs, loghandle);
        WITH counter = OLIGetOpenCounter(pool.openLogs, loghandle) DO
          OLIPutOpenCounter(pool.openLogs, loghandle, counter + 1);
        END;
      ELSIF LogIsOpen(pool, qname, local) THEN
        (* An existing log that was opened by some other client *)
        lmOrd := pool.graphs.getLogMode(qname, local) - 1;
        (* logMode is set correctly here *)
        logMode := VAL(lmOrd, LogMode);
        CreateLogAndLogStack(
          logfilename, log, logstack, logMode, new := FALSE,
          local := local, setMode := FALSE);
        loghandle := InsertInOLIList(pool, qname, logfilename, local,
                                     logMode, log, logstack);
      ELSE
        (* We are the first to open the log *)
        logMode := mode;
        pool.graphs.setLogMode(qname, local, ORD(logMode) + 1);
        CreateLogAndLogStack(logfilename, log, logstack, logMode,
                             new := FALSE, local := local, setMode := TRUE);
        loghandle := InsertInOLIList(pool, qname, logfilename, local,
                                     logMode, log, logstack);
      END;
      mode := logMode;
      RETURN loghandle;
    EXCEPT
    | ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.OpenLog",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown =>
        RAISE InternalError(ErrorSupport.Create("ChgMgmtGraphPool.OpenLog",
                                                "ChgMgmtNames.Unknown"));
    | Log.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate("ChgMgmtGraphPool.OpenLog",
                                               "Log.InternalError", info));
    END;
  END OpenLog;

PROCEDURE CloseLog (pool: T; loghandle: CARDINAL; keep: BOOLEAN)
  RAISES {InternalError} =
  VAR
    log        : Log.T;
    logMode    : LogMode;
    local      : BOOLEAN;
    openCount  : CARDINAL;
    logfilename: Pathname.T;
    logname    : Pathname.T;

  BEGIN
    TRY
      openCount := OLIGetOpenCounter(pool.openLogs, loghandle);

      IF openCount = 1 THEN
        (* Final close for this client *)
        (* commit running transaction intern *)
        FOR d := pool.chgMgmtTransactionDepth TO 1 BY -1 DO
          CommitOneLog(pool, loghandle);
        END;
        log := OLIGetLog(pool.openLogs, loghandle);
        logMode := OLIGetLogMode(pool.openLogs, loghandle);
        logfilename := OLIGetFilename(pool.openLogs, loghandle);
        logname := OLIGetName(pool.openLogs, loghandle);
        local := OLIGetLocal(pool.openLogs, loghandle);
        IF logMode # LogMode.None THEN
          (* close method of PersistentLog will delete the log files only
             if no one else uses them. *)
          NARROW(log, PersistentLog.T).close(
            pool, logfilename, NOT keep, local);
          IF NOT PersistentLog.LogFilesInUse(pool, logfilename, local) THEN
            pool.graphs.setLogMode(logname, local, -1);
          END;
        END;
        OLIFree(pool.openLogs, loghandle);
      ELSE
        OLIPutOpenCounter(pool.openLogs, loghandle, openCount - 1);
      END;
    EXCEPT
    | Log.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate("ChgMgmtGraphPool.CloseLog",
                                               "Log.InternalError", info));
    | ChgMgmtNames.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate("ChgMgmtGraphPool.CloseLog",
                                   "ChgMgmtNames.InternalError", info));
    | ChgMgmtNames.Unknown =>
        RAISE
          InternalError(ErrorSupport.Create("ChgMgmtGraphPool.CloseLog",
                                            "ChgMgmtNames.Unknown"));
    | Access.Locked =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "ChgMgmtGraphPool.CloseLog", "Access.Locked"));
    END;
  END CloseLog;

PROCEDURE GetLog (pool: T; loghandle: CARDINAL): Log.T =
  BEGIN
    RETURN OLIGetLog(pool.openLogs, loghandle);
  END GetLog;


(* LOCAL INTERFACE ADT OpenLogInfoArray *)
TYPE
  OpenLogInfo = RECORD
                  name     : Pathname.T;
                  filename : Pathname.T;
                  local    : BOOLEAN;
                  openCount: CARDINAL;
                  logmode  : LogMode;
                  logstack : LogStack.T;
                  log      : Log.T;
                  next     : INTEGER;
                END;

  OLIEntries = REF ARRAY OF OpenLogInfo;

  OLIArray = RECORD
               entries            : OLIEntries;
               first, free, cursor: INTEGER;
               size               : CARDINAL;
             END;

PROCEDURE OLIInit (VAR list: OLIArray) =
  BEGIN
    list.entries := NEW(OLIEntries, 10);
    list.free := 0;
    list.first := -1;
    list.cursor := -1;
    list.size := 0;
    FOR i := 0 TO LAST(list.entries^) - 1 DO
      list.entries^[i] := NullLogInfo;
      list.entries^[i].next := i + 1
    END;
    list.entries^[LAST(list.entries^)] := NullLogInfo;
  END OLIInit;

PROCEDURE OLIReserve (VAR list: OLIArray; VAR handle: CARDINAL) =
  BEGIN
    IF list.free = -1 THEN OLIExpand(list); END;
    handle := list.free;
    (* remove from free list *)
    list.free := list.entries^[handle].next;
    (* prepend to used list *)
    list.entries^[handle].next := list.first;
    list.first := handle;
    INC(list.size);
  END OLIReserve;

PROCEDURE OLIFree (VAR list: OLIArray; handle: CARDINAL) =
  VAR prev: CARDINAL;
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    (* remove from used list *)
    IF list.first = handle THEN
      list.first := list.entries^[list.first].next;
    ELSE
      prev := list.first;
      WHILE prev # -1 AND list.entries^[prev].next # handle DO
        prev := list.entries^[prev].next;
      END;
      <* ASSERT prev # -1 *>
      list.entries^[prev].next := list.entries^[handle].next;
    END;
    (* prepend to free list *)
    list.entries^[handle] := NullLogInfo;
    list.entries^[handle].next := list.free;
    list.free := handle;
    DEC(list.size);
  END OLIFree;

(*--- Procedure is not used yet --- PROCEDURE OLISize (READONLY list:
   OLIArray): CARDINAL = BEGIN RETURN list.size; END OLISize; *)

PROCEDURE OLILoop (VAR list: OLIArray) =
  BEGIN
    list.cursor := list.first;
  END OLILoop;

PROCEDURE OLINext (VAR list: OLIArray): INTEGER =
  (* returns current handle and advances cursor *)
  VAR res: INTEGER;
  BEGIN
    res := list.cursor;
    IF res # -1 THEN list.cursor := list.entries^[res].next; END;
    RETURN res;
  END OLINext;

PROCEDURE OLIPutLog (VAR list: OLIArray; handle: CARDINAL; log: Log.T) =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    list.entries^[handle].log := log;
  END OLIPutLog;

PROCEDURE OLIPutLogMode (VAR list  : OLIArray;
                             handle: CARDINAL;
                             mode  : LogMode   ) =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    list.entries^[handle].logmode := mode;
  END OLIPutLogMode;

PROCEDURE OLIPutLogStack (VAR list  : OLIArray;
                              handle: CARDINAL;
                              stack : LogStack.T) =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    list.entries^[handle].logstack := stack;
  END OLIPutLogStack;

PROCEDURE OLIPutName (VAR list  : OLIArray;
                          handle: CARDINAL;
                          name  : Pathname.T) =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    list.entries^[handle].name := name;
  END OLIPutName;

PROCEDURE OLIPutFilename (VAR list  : OLIArray;
                              handle: CARDINAL;
                              fname : Pathname.T) =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    list.entries^[handle].filename := fname;
  END OLIPutFilename;

PROCEDURE OLIPutLocal (VAR list: OLIArray; handle: CARDINAL; local: BOOLEAN) =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    list.entries^[handle].local := local;
  END OLIPutLocal;

PROCEDURE OLIPutOpenCounter (VAR list  : OLIArray;
                                 handle: CARDINAL;
                                 count : CARDINAL  ) =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    list.entries^[handle].openCount := count;
  END OLIPutOpenCounter;

PROCEDURE OLIGetLog (READONLY list: OLIArray; handle: CARDINAL): Log.T =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    RETURN list.entries^[handle].log;
  END OLIGetLog;

PROCEDURE OLIGetLogMode (READONLY list: OLIArray; handle: CARDINAL):
  LogMode =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    RETURN list.entries^[handle].logmode;
  END OLIGetLogMode;

PROCEDURE OLIGetLogStack (READONLY list: OLIArray; handle: CARDINAL):
  LogStack.T =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    RETURN list.entries^[handle].logstack;
  END OLIGetLogStack;

PROCEDURE OLIGetName (READONLY list: OLIArray; handle: CARDINAL):
  Pathname.T =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    RETURN list.entries^[handle].name;
  END OLIGetName;

PROCEDURE OLIGetFilename (READONLY list: OLIArray; handle: CARDINAL):
  Pathname.T =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    RETURN list.entries^[handle].filename;
  END OLIGetFilename;

PROCEDURE OLIGetLocal (READONLY list: OLIArray; handle: CARDINAL):
  BOOLEAN =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    RETURN list.entries^[handle].local;
  END OLIGetLocal;

PROCEDURE OLIGetOpenCounter (READONLY list: OLIArray; handle: CARDINAL):
  CARDINAL =
  BEGIN
    <* ASSERT handle < NUMBER(list.entries^) *>
    RETURN list.entries^[handle].openCount;
  END OLIGetOpenCounter;

(* END LOCAL ADT OpenGraphInfoArray *)

(* LOCAL MODULE ADT OpenGraphInfoArray *)

CONST
  NullLogInfo = OpenLogInfo{name := NIL, filename := NIL, local := FALSE,
                            openCount := 0, logmode := LogMode.None,
                            logstack := NIL, log := NIL, next := -1};



PROCEDURE OLIExpand (VAR list: OLIArray) =
  VAR
    new: OLIEntries;
    l  : CARDINAL;
  BEGIN
    WITH oldlen = NUMBER(list.entries^) DO
      new := NEW(OLIEntries, 2 * oldlen);
      SUBARRAY(new^, 0, oldlen) := list.entries^;
      FOR i := oldlen TO LAST(new^) - 1 DO
        new^[i] := NullLogInfo;
        new^[i].next := i + 1;
      END;
      new^[LAST(new^)] := NullLogInfo;
      new^[LAST(new^)].next := -1;
      IF list.free = -1 THEN
        (* No free entry in old list.  First entry of new area becomes
           free. *)
        list.free := oldlen;
      ELSE
        (* Connect end of free list to start of new area *)
        l := list.free;
        WHILE list.entries^[l].next >= 0 DO l := list.entries^[l].next END;
        list.entries^[l].next := oldlen;
      END;
      list.entries := new;
    END;
  END OLIExpand;

(* END LOCAL ADT OpenLogInfoArray *)

(* LOCAL ADT GroupCache *)

PROCEDURE GroupCacheInit (VAR cache: IntIntTbl.T) =
  BEGIN
    cache := NEW(IntIntTbl.Default).init();
  END GroupCacheInit;

PROCEDURE GroupCacheLookup (    cache: IntIntTbl.T;
                                graph: CARDINAL;
                            VAR group: CARDINAL     ): BOOLEAN =
  VAR
    v    : INTEGER;
    found: BOOLEAN;
  BEGIN
    found := cache.get(graph, v);
    group := v;
    RETURN found;
  END GroupCacheLookup;

PROCEDURE GroupCacheInsert (cache: IntIntTbl.T; graph, group: CARDINAL) =
  BEGIN
    EVAL cache.put(graph, group);
  END GroupCacheInsert;

PROCEDURE GroupCacheRemove (cache: IntIntTbl.T; graph: CARDINAL) =
  VAR group: INTEGER;
  BEGIN
    EVAL cache.delete(graph, group);
  END GroupCacheRemove;

<* UNUSED *>
PROCEDURE GroupCacheClear (VAR cache: IntIntTbl.T) =
  BEGIN
    cache := NEW(IntIntTbl.Default).init();
  END GroupCacheClear;

(* END LOCAL ADT GroupCache *)

PROCEDURE FindGroup (pool: T; graph: CARDINAL): CARDINAL
  RAISES {InternalError, Access.Locked} =
  VAR
    group  : Pathname.T;
    groupid: CARDINAL;
  BEGIN
    IF NOT GroupCacheLookup(pool.groupCache, graph, groupid) THEN
      TRY
        IF pool.graphs.isRemoteId(graph) THEN
          group :=
            pool.graphs.getLogGroup(pool.graphs.name(graph, FALSE), FALSE);
        ELSE
          group :=
            pool.graphs.getLogGroup(pool.graphs.name(graph, FALSE), FALSE);
        END;
        GroupCacheInsert(pool.groupCache, graph, groupid);
      EXCEPT
        ChgMgmtNames.InternalError (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "ChgMgmtGraphPool.FindGroup",
                                "ChgMgmtNames.InternalError", info));
      | ChgMgmtNames.Unknown =>
          RAISE
            InternalError(ErrorSupport.Create("ChgMgmtGraphPool.FindGroup",
                                              "ChgMgmtNames.Unknown"));
      | Names.Unknown =>
          RAISE InternalError(
                  ErrorSupport.Create(
                    "ChgMgmtGraphPool.FindGroup", "Names.Unknown"));
      | Names.Undeclared =>
          RAISE
            InternalError(ErrorSupport.Create("ChgMgmtGraphPool.FindGroup",
                                              "Names.Undeclared"));
      | Names.InternalError (info) =>
          RAISE InternalError(
                  ErrorSupport.Propagate("ChgMgmtGraphPool.FindGroup",
                                         "Names.InternalError", info));
      END;
    END;
    RETURN groupid;
  END FindGroup;

BEGIN
END ChgMgmtGraphPool.
