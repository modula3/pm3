MODULE Log EXPORTS Log, InternalLog;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.5  1998/09/14 08:14:55  roland
    Modified code to remove compiler warnings.

    Revision 1.4  1998/03/09 14:58:23  roland
    Forward stack must be cleared during redo.

    Revision 1.3  1998/01/21 12:30:09  roland
    Set checkpoint if necessary before executing gotoCPLabel.

    Revision 1.2  1997/05/30 07:54:03  roland
    Backward loop added to deltas to efficiently implement copying of
    backward deltas.

    Revision 1.1  1997/04/23 13:32:56  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.4  1996/11/20 12:20:44  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/23 08:34:52  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:58:35  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:21  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT Delta, DeltaList, CheckpointTree, LabelTable, Checkpoint,
       GraphCommand, Access, MetaOpStack;
IMPORT CardSeq;
IMPORT ErrorSupport;
(* Import of GraphCommandStack only to convince run-time system not to
   throw improper type code.  Don't ask me why. *)
IMPORT GraphCommandStack; <* NOWARN *>

REVEAL

  Internal =
    Private BRANDED OBJECT
      tree: CheckpointTree.T;
      backward, forward: Delta.T;  (* These deltas collect the commands
                                      until the next checkpoint is set. *)
      labels        : LabelTable.T;
      fstack, bstack: MetaOpStack.T;
    OVERRIDES
      initialize    := Initialize;
      getComponents := GetComponents;
    END;

  T = Public BRANDED OBJECT
      OVERRIDES
        getMode               := GetMode;
        setMode               := SetMode;
        setCheckpoint         := SetCheckpoint;
        setCheckpointLabel    := SetCheckpointLabel;
        getCheckpointLabel    := GetCheckpointLabel;
        removeCheckpointLabel := RemoveCheckpointLabel;
        appliedCommands       := AppliedCommands;
        getActualPath         := GetActualPath;
        getActSon             := GetActSon;
        getNumberOfSons       := GetNumberOfSons;
        undo                  := Undo;
        redo                  := Redo;
        gotoCPLabel           := GotoCPLabel;
        redoPrev              := RedoPrev;
        redoNext              := RedoNext;
        redoIth               := RedoIth;
        backstep              := Backstep;
        forstep               := Forstep;
      END;

(* The initialization methods for the subclasses of Log.T all create the
   root checkpoint in the checkpoint tree. *)
<* FATAL CheckpointTree.Empty *>

PROCEDURE CheckTreeMode (log: T)
  RAISES {ModeError, InternalError, Access.Locked} =
  BEGIN
    TRY
      IF log.tree.getMode() = CheckpointTree.Mode.Sequence THEN
        RAISE ModeError;
      END;
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Log.CheckTreeMode",
                              "CheckpointTree.InternalError", info));
    END;
  END CheckTreeMode;

(* Internal interface procedures *)
PROCEDURE Initialize (log           : Internal;
                      tree          : CheckpointTree.T;
                      labels        : LabelTable.T;
                      fstack, bstack: MetaOpStack.T     )
  RAISES {Access.Locked, InitializeError} =
  BEGIN
    log.tree := tree;
    log.labels := labels;
    log.fstack := fstack;
    log.bstack := bstack;
    TRY
      log.tree.createRoot();
      log.tree.getWriteDeltas(log.forward, log.backward);
    EXCEPT
      CheckpointTree.InternalError => RAISE InitializeError;
    END;
  END Initialize;

PROCEDURE GetComponents (    log           : Internal;
                         VAR tree          : CheckpointTree.T;
                         VAR forw, backw   : Delta.T;
                         VAR labels        : LabelTable.T;
                         VAR fstack, bstack: MetaOpStack.T     ) =
  BEGIN
    tree := log.tree;
    forw := log.forward;
    backw := log.backward;
    labels := log.labels;
    fstack := log.fstack;
    bstack := log.bstack;
  END GetComponents;

(* public interface procedures *)

PROCEDURE GetMode (log: T): Mode RAISES {InternalError, Access.Locked} =
  VAR mode: Mode;
  BEGIN
    TRY
      mode := VAL(ORD(log.tree.getMode()), Mode);
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.GetMode", "CheckpointTree.InternalError", info));
    END;
    RETURN mode;
  END GetMode;

PROCEDURE SetMode (log: T; m: Mode) RAISES {InternalError, Access.Locked} =
  BEGIN
    TRY
      log.tree.setMode(VAL(ORD(m), CheckpointTree.Mode));
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.SetMode", "CheckpointTree.InternalError", info));
    END;
  END SetMode;

PROCEDURE SetCheckpoint (log: Internal)
  RAISES {InternalError, Access.Locked} =
  BEGIN
    TRY
      log.tree.createSon(log.forward, log.backward);
      log.fstack.clear();
      log.bstack.push(0);
      log.tree.getWriteDeltas(log.forward, log.backward);
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Log.SetCheckpoint",
                              "CheckpointTree.InternalError", info));
    | MetaOpStack.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.SetCheckpoint", "MetaOpStack.InternalError", info));
    END;
  END SetCheckpoint;

PROCEDURE SetCheckpointLabel (log: T; label: CARDINAL)
  RAISES {InternalError, Access.Locked} =
  BEGIN
    TRY
      log.tree.setLabel(label);
      log.labels.insert(label, log.tree.getPath());
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Log.SetCheckpointLabel",
                              "CheckpointTree.InternalError", info));
    | LabelTable.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Log.SetCheckpointLabel",
                                       "LabelTable.InternalError", info));
    END;
  END SetCheckpointLabel;

PROCEDURE GetCheckpointLabel (log: T; VAR l: CARDINAL; VAR ok: BOOLEAN)
  RAISES {InternalError, Access.Locked} =
  VAR hl: INTEGER;
  BEGIN
    TRY
      log.tree.getLabel(hl, ok);
      IF hl = Checkpoint.NoLabel THEN
        ok := FALSE
      ELSE
        ok := TRUE;
        l := hl
      END;
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Log.GetCheckpointLabel",
                              "CheckpointTree.InternalError", info));
    END;
  END GetCheckpointLabel;

PROCEDURE RemoveCheckpointLabel (log: T)
  RAISES {InternalError, Access.Locked} =
  VAR
    l : INTEGER;
    ok: BOOLEAN;
  BEGIN
    TRY
      log.tree.getLabel(l, ok);
      IF l # Checkpoint.NoLabel THEN
        log.tree.setLabel(Checkpoint.NoLabel);
        log.labels.remove(l);
      END;
    EXCEPT
      LabelTable.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Log.RemoveCheckpointLabel",
                                       "LabelTable.InternalError", info));
    | CheckpointTree.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Log.RemoveCheckpointLabel",
                              "CheckpointTree.InternalError", info));
    END;
  END RemoveCheckpointLabel;

PROCEDURE AppliedCommands (log: T; forward, backward: Delta.T)
  RAISES {InternalError, Access.Locked} =

  PROCEDURE AppendDelta (    delta, append: Delta.T;
                             forward      : BOOLEAN;
                         VAR changed      : BOOLEAN  )
    RAISES {InternalError, Access.Locked} =
    VAR
      c : GraphCommand.T;
      ok: BOOLEAN;
    BEGIN
      TRY
        IF forward THEN
          append.loop();
          append.getNextCommand(c, ok);
          WHILE ok DO
            delta.addCommand(c);
            append.getNextCommand(c, ok);
            changed := TRUE;
          END;
        ELSE
          append.reverseLoop();
          append.getPrevCommand(c, ok);
          WHILE ok DO
            delta.addCommand(c);
            append.getPrevCommand(c, ok);
            changed := TRUE;
          END;
        END;
      EXCEPT
        Delta.Error (info) =>
          RAISE InternalError(ErrorSupport.Propagate(
                                "Log.AppendDelta", "Delta.Error", info));
      END;
    END AppendDelta;

  VAR changed: BOOLEAN := FALSE;
  BEGIN
    AppendDelta(log.forward, forward, forward := TRUE, changed := changed);
    AppendDelta(
      log.backward, backward, forward := FALSE, changed := changed);
    TRY
      IF changed THEN log.tree.changes() END;
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "Log.AppendDelta", "CheckpointTree.InternalError", info));
    END;
  END AppliedCommands;

PROCEDURE GetActualPath (log: T; VAR fwd, bwd: DeltaList.T)
  RAISES {InternalError, Access.Locked} =
  VAR
    path         : CardSeq.T;
    forwd, backwd: Delta.T;
  <* FATAL DeltaList.NotInitialized, CheckpointTree.NoSuchNode *>
  BEGIN
    TRY
      fwd := NEW(DeltaList.T).init();
      bwd := NEW(DeltaList.T).init();
      path := log.tree.getPath();
      log.tree.gotoRoot();
      WHILE NOT path.size() = 0 DO
        log.tree.gotoNthSon(path.remhi());
        log.tree.getDeltas(forwd, backwd);
        fwd.insertAtBottom(forwd);
        bwd.insertAtTop(backwd);
      END;
      (* the latest applied commands *)
      bwd.insertAtTop(log.backward);
      fwd.insertAtBottom(log.forward);
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Log.GetActualPath",
                              "CheckpointTree.InternalError", info));
    END;
  END GetActualPath;

PROCEDURE GetActSon (log: T): CARDINAL
  RAISES {InternalError, Access.Locked} =
  VAR son: CARDINAL;
  BEGIN
    TRY
      son := log.tree.actSon();
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.GetActSon", "CheckpointTree.InternalError", info));
    END;
    RETURN son;
  END GetActSon;

PROCEDURE GetNumberOfSons (log: T): CARDINAL
  RAISES {InternalError, Access.Locked} =
  VAR sons: CARDINAL;
  BEGIN
    TRY
      sons := log.tree.noSons();
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Log.GetNumberOfSons",
                              "CheckpointTree.InternalError", info));
    END;
    RETURN sons;
  END GetNumberOfSons;

PROCEDURE Undo (log: T): Delta.T
  RAISES {InternalError, NoSuchCheckpoint, Access.Locked} =
  VAR f, b: Delta.T;
  BEGIN
    TRY
      IF log.tree.needsCheckpoint() THEN log.setCheckpoint(); END;
      log.tree.getDeltas(f, b);
      log.tree.gotoFather();
      log.bstack.push(log.tree.actSon());
      log.fstack.clear();
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.Undo", "CheckpointTree.InternalError", info));
    | CheckpointTree.NoSuchNode => RAISE NoSuchCheckpoint;
    | MetaOpStack.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Log.Undo", "MetaOpStack.InternalError", info));
    END;
    RETURN b;
  END Undo;

PROCEDURE Redo (log: T): Delta.T
  RAISES {InternalError, NoSuchCheckpoint, Access.Locked} =
  VAR f, b: Delta.T;
  BEGIN
    TRY
      log.tree.gotoSon(CheckpointTree.SonPosition.Actual);
      log.tree.getDeltas(f, b);
      log.bstack.push(0);
      log.fstack.clear();
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.Redo", "CheckpointTree.InternalError", info));
    | CheckpointTree.NoSuchNode => RAISE NoSuchCheckpoint;
    | MetaOpStack.InternalError (info) =>
        RAISE
          InternalError(ErrorSupport.Propagate(
                          "Log.Redo", "MetaOpStack.InternalError", info));
    END;
    RETURN f;
  END Redo;

PROCEDURE GotoCPLabel (log: T; l: CARDINAL): DeltaList.T
  RAISES {InternalError, NoSuchCheckpoint, Access.Locked} =
  VAR
    lpath, apath: CardSeq.T;
    fd, bd      : Delta.T     := NIL;
    res         : DeltaList.T;
    son         : CARDINAL;

  PROCEDURE RemoveCommonPrefix (a, b: CardSeq.T) =
    VAR ap, bp: CARDINAL;
    BEGIN
      ap := a.size();
      bp := b.size();
      WHILE ap # 0 AND bp # 0 AND a.gethi() = b.gethi() DO
        EVAL a.remhi();
        EVAL b.remhi();
        DEC(ap);
        DEC(bp);
      END;
    END RemoveCommonPrefix;
  <* FATAL DeltaList.NotInitialized, CheckpointTree.NoSuchNode *>
  BEGIN
    TRY
      IF log.tree.needsCheckpoint() THEN log.setCheckpoint(); END;
      res := NEW(DeltaList.T).init();
      lpath := log.labels.lookup(l);
      apath := log.tree.getPath();
      RemoveCommonPrefix(apath, lpath);
      WHILE NOT apath.size() = 0 DO
        log.tree.getDeltas(fd, bd);
        res.insertAtBottom(bd);
        log.tree.gotoFather();
        EVAL apath.remhi();
      END;

      WHILE NOT lpath.size() = 0 DO
        son := lpath.remhi();
        log.tree.gotoNthSon(son);
        log.tree.getDeltas(fd, bd);
        res.insertAtBottom(fd);
      END;
    EXCEPT
      LabelTable.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("Log.RemoveCommonPrefix",
                                       "LabelTable.InternalError", info));
    | CheckpointTree.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "Log.RemoveCommonPrefix",
                              "CheckpointTree.InternalError ", info));
    | LabelTable.NotFound => RAISE NoSuchCheckpoint;
    END;
    RETURN res;
  END GotoCPLabel;

PROCEDURE RedoPrev (log: T): Delta.T
  RAISES {InternalError, ModeError, NoSuchCheckpoint, Access.Locked} =
  VAR f, b: Delta.T;
  BEGIN
    TRY
      CheckTreeMode(log);
      log.tree.gotoSon(CheckpointTree.SonPosition.Previous);
      log.tree.getDeltas(f, b);
      log.bstack.push(0);
      log.fstack.clear();
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.RedoPrev", "CheckpointTree.InternalError", info));
    | MetaOpStack.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.RedoPrev", "MetaOpStack.InternalError", info));
    | CheckpointTree.NoSuchNode => RAISE NoSuchCheckpoint;
    END;
    RETURN f;
  END RedoPrev;

PROCEDURE RedoNext (log: T): Delta.T
  RAISES {InternalError, ModeError, NoSuchCheckpoint, Access.Locked} =
  VAR f, b: Delta.T;
  BEGIN
    TRY
      CheckTreeMode(log);
      log.tree.gotoSon(CheckpointTree.SonPosition.Next);
      log.tree.getDeltas(f, b);
      log.bstack.push(0);
      log.fstack.clear();
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.RedoNext", "CheckpointTree.InternalError", info));
    | MetaOpStack.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.RedoNext", "MetaOpStack.InternalError", info));
    | CheckpointTree.NoSuchNode => RAISE NoSuchCheckpoint;
    END;
    RETURN f;
  END RedoNext;

PROCEDURE RedoIth (log: T; READONLY i: CARDINAL): Delta.T
  RAISES {InternalError, ModeError, NoSuchCheckpoint, Access.Locked} =
  VAR f, b: Delta.T;
  BEGIN
    TRY
      CheckTreeMode(log);
      log.tree.gotoNthSon(i);
      log.tree.getDeltas(f, b);
      log.bstack.push(0);
      log.fstack.clear();
    EXCEPT
      CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.RedoIth", "CheckpointTree.InternalError", info));
    | MetaOpStack.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.RedoIth", "MetaOpStack.InternalError", info));
    | CheckpointTree.NoSuchNode => RAISE NoSuchCheckpoint;
    END;
    RETURN f;
  END RedoIth;

PROCEDURE Backstep (log: T): Delta.T
  RAISES {InternalError, ModeError, NoSuchCheckpoint, Access.Locked} =
  VAR
    goto        : CARDINAL;
    delta, dummy: Delta.T;
  BEGIN
    CheckTreeMode(log);
    TRY
      goto := log.bstack.pop();
      IF goto = 0 THEN
        IF log.tree.needsCheckpoint() THEN log.setCheckpoint(); END;
        log.tree.getDeltas(dummy, delta);
        log.tree.gotoFather();
        log.fstack.push(log.tree.actSon());
      ELSE
        log.tree.gotoNthSon(goto);
        log.tree.getDeltas(delta, dummy);
        log.fstack.push(0);
      END;
    EXCEPT
      MetaOpStack.Empty => RAISE NoSuchCheckpoint;
    | MetaOpStack.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.Backstep", "MetaOpStack.InternalError", info));
    | CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.Backstep", "CheckpointTree.InternalError", info));
    | CheckpointTree.NoSuchNode =>
        RAISE
          InternalError(ErrorSupport.Create(
                          "Log.Backstep", "CheckpointTree.NoSuchNode"));
    END;
    RETURN delta;
  END Backstep;

PROCEDURE Forstep (log: T): Delta.T
  RAISES {InternalError, ModeError, NoSuchCheckpoint, Access.Locked} =
  VAR
    goto        : CARDINAL;
    delta, dummy: Delta.T;
  BEGIN
    CheckTreeMode(log);
    TRY
      goto := log.fstack.pop();
      IF goto = 0 THEN
        log.tree.getDeltas(dummy, delta);
        log.tree.gotoFather();
        log.bstack.push(log.tree.actSon());
      ELSE
        log.tree.gotoNthSon(goto);
        log.tree.getDeltas(delta, dummy);
        log.bstack.push(0);
      END;
    EXCEPT
      MetaOpStack.Empty => RAISE NoSuchCheckpoint;
    | MetaOpStack.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.Forstep", "MetaOpStack.InternalError", info));
    | CheckpointTree.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "Log.Forstep", "CheckpointTree.InternalError", info));
    | CheckpointTree.NoSuchNode =>
        RAISE InternalError(ErrorSupport.Create(
                              "Log.Forstep", "CheckpointTree.NoSuchNode"));
    END;
    RETURN delta;
  END Forstep;

BEGIN
END Log.
