MODULE PersistentDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.3  1998/05/19 10:17:52  roland
    Support for log-groups implemented.

    Revision 1.2  1997/05/30 07:54:13  roland
    Backward loop added to deltas to efficiently implement copying of
    backward deltas.

    Revision 1.1  1997/04/23 13:34:11  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.5  1996/12/20 17:31:51  roland
    Deltas need not be closed for method loop.

    Revision 1.4  1996/11/20 12:21:10  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/23 08:35:27  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:59:06  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:57  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT GraphCommand, GraphCommandStream, FilePos, Delta, Access, PCIStream,
       DeltaInfo, PersistentCheckpointInfo;
IMPORT ErrorSupport;

REVEAL
  T = Public BRANDED OBJECT
        pciStream: PCIStream.T;
        commands : GraphCommandStream.T;
        forward  : BOOLEAN;
        pos      : FilePos.T;
      OVERRIDES
        init           := Init;
        addCommand     := AddCommand;
        costs          := Costs;
        loop           := Loop;
        getNextCommand := GetNextCommand;
        reverseLoop    := ReverseLoop;
        getPrevCommand := GetPrevCommand;
      END;

PROCEDURE ReadPersistentCheckpointInfo (delta: T;
                                        VAR pci: PersistentCheckpointInfo.T)
  RAISES {Access.Locked, Delta.Error} =
  VAR oldpos: FilePos.T;
  BEGIN
    TRY
      delta.pciStream.getPosition(oldpos);
      delta.pciStream.setPosition(delta.pos);
      delta.pciStream.read(pci);
      delta.pciStream.setPosition(oldpos);
    EXCEPT
      PCIStream.EOS =>
        RAISE Delta.Error(ErrorSupport.Create(
                            "PersistentDelta.GetData", "PCIStream.EOS"));
    | PCIStream.ElementError =>
        RAISE Delta.Error(ErrorSupport.Create("PersistentDelta.GetData",
                                              "PCIStream.ElementError"));
    | PCIStream.InternalError (info) =>
        RAISE Delta.Error(ErrorSupport.Propagate(
                            "PersistentDelta.ReadPersistentCheckpointInfo",
                            "PCIStream.InternalError", info));
    END;
  END ReadPersistentCheckpointInfo;

PROCEDURE WritePersistentCheckpointInfo (delta: T;
                                         READONLY pci: PersistentCheckpointInfo.T)
  RAISES {Access.Locked, Delta.Error} =
  VAR oldpos: FilePos.T;
  BEGIN
    TRY
      delta.pciStream.getPosition(oldpos);
      delta.pciStream.setPosition(delta.pos);
      delta.pciStream.write(pci, overwrite := FALSE);
      delta.pciStream.setPosition(oldpos);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE
          Delta.Error(ErrorSupport.Propagate(
                        "PersistentDelta.WritePersistentCheckpointInfo",
                        "PCIStream.InternalError", info));
    END;
  END WritePersistentCheckpointInfo;

(* Interface procedures *)

PROCEDURE Init (delta      : T;
                checkpoints: PCIStream.T;
                commands   : GraphCommandStream.T;
                forward    : BOOLEAN;
                cppos      : FilePos.T             ): T RAISES {} =
  BEGIN
    delta.commands := commands;
    delta.pciStream := checkpoints;
    delta.forward := forward;
    delta.pos := cppos;
    RETURN delta;
  END Init;

PROCEDURE AddCommand (delta: T; READONLY c: GraphCommand.T)
  RAISES {Access.Locked, Delta.Error} =
  VAR pci: PersistentCheckpointInfo.T;
  BEGIN
    TRY
      (* Write to the current end position of the delta in the commands. *)
      ReadPersistentCheckpointInfo(delta, pci);
      IF NOT pci.deltas.state = DeltaInfo.State.Open THEN
        RAISE Delta.Error(ErrorSupport.Create("PersistentDelta.AddCommand",
                                              "Delta not open."))
      END;
      IF delta.forward THEN
        delta.commands.setPosition(pci.deltas.info[DeltaInfo.Forward].end);
      ELSE
        delta.commands.setPosition(
          pci.deltas.info[DeltaInfo.Backward].start);
      END;
      delta.commands.write(c, overwrite := TRUE);

      (* keep delta positions up-to-date with stream *)
      IF delta.forward THEN
        delta.commands.getPosition(pci.deltas.info[DeltaInfo.Forward].end);
        INC(pci.deltas.info[DeltaInfo.Forward].costs);
      ELSE
        delta.commands.getPosition(
          pci.deltas.info[DeltaInfo.Backward].start);
        INC(pci.deltas.info[DeltaInfo.Backward].costs);
      END;
      WritePersistentCheckpointInfo(delta, pci);
    EXCEPT
    | GraphCommandStream.InternalError (info) =>
        RAISE Delta.Error(ErrorSupport.Propagate(
                            "PersistentDelta.AddCommand",
                            "GraphCommandStream.InternalError", info));
    END;
  END AddCommand;

PROCEDURE Costs (delta: T): CARDINAL RAISES {Access.Locked, Delta.Error} =
  VAR pci: PersistentCheckpointInfo.T;
  BEGIN
    ReadPersistentCheckpointInfo(delta, pci);
    RETURN pci.deltas.info[delta.forward].costs;
  END Costs;

PROCEDURE Loop (delta: T) RAISES {Access.Locked, Delta.Error} =
  VAR pci: PersistentCheckpointInfo.T;
  BEGIN
    TRY
      ReadPersistentCheckpointInfo(delta, pci);
      IF NOT pci.deltas.state = DeltaInfo.State.Closed THEN
        (* Ignore.  This should only happen when an indirect graph is
           closed by its last user. *)
        (**
        RAISE Delta.Error(ErrorSupport.Create(
                            "PersistentDelta.Loop", "Delta not closed."))
         *)
      END;

      (* initialize forward loop *)
      delta.commands.setPosition(pci.deltas.info[delta.forward].start);
      pci.deltas.info[delta.forward].current :=
        pci.deltas.info[delta.forward].start;
      WritePersistentCheckpointInfo(delta, pci);
    EXCEPT
    | GraphCommandStream.InternalError (info) =>
        RAISE Delta.Error(ErrorSupport.Propagate(
                            "PersistentDelta.Loop",
                            "GraphCommandStream.InternalError", info));
    END;
  END Loop;

PROCEDURE GetNextCommand (delta: T; VAR c: GraphCommand.T; VAR ok: BOOLEAN)
  RAISES {Delta.Error, Access.Locked} =
  VAR
    curr: FilePos.T;
    pci : PersistentCheckpointInfo.T;
  BEGIN
    TRY
      ReadPersistentCheckpointInfo(delta, pci);
      curr := pci.deltas.info[delta.forward].current;
      IF FilePos.Compare(curr, pci.deltas.info[delta.forward].start) >= 0
           AND FilePos.Compare(curr, pci.deltas.info[delta.forward].end)
                 < 0 THEN
        delta.commands.setPosition(curr);
        delta.commands.read(c);
        delta.commands.getPosition(pci.deltas.info[delta.forward].current);
        WritePersistentCheckpointInfo(delta, pci);
        ok := TRUE;
      ELSE
        ok := FALSE;
      END;
    EXCEPT
      GraphCommandStream.EOS => ok := FALSE;
    | GraphCommandStream.ElementError =>
        RAISE Delta.Error(
                ErrorSupport.Create("PersistentDelta.Loop",
                                    "GraphCommandStream.ElementError"));
    | GraphCommandStream.InternalError (info) =>
        RAISE Delta.Error(ErrorSupport.Propagate(
                            "PersistentDelta.GetNextCommand",
                            "GraphCommandStream.InternalError", info));
    END;
  END GetNextCommand;

PROCEDURE ReverseLoop (delta: T) RAISES {Access.Locked, Delta.Error} =
  VAR pci: PersistentCheckpointInfo.T;
  BEGIN
    TRY
      ReadPersistentCheckpointInfo(delta, pci);
      IF NOT pci.deltas.state = DeltaInfo.State.Closed THEN
        (* Ignore.  This should only happen when an indirect graph is
           closed by its last user. *)
        (**
        RAISE Delta.Error(ErrorSupport.Create(
                            "PersistentDelta.Loop", "Delta not closed."))
         *)
      END;

      (* initialize backward loop *)
      delta.commands.setPosition(pci.deltas.info[delta.forward].end);
      pci.deltas.info[delta.forward].current :=
        pci.deltas.info[delta.forward].end;
      WritePersistentCheckpointInfo(delta, pci);
    EXCEPT
    | GraphCommandStream.InternalError (info) =>
        RAISE Delta.Error(ErrorSupport.Propagate(
                            "PersistentDelta.Loop",
                            "GraphCommandStream.InternalError", info));
    END;
  END ReverseLoop;

PROCEDURE GetPrevCommand (delta: T; VAR c: GraphCommand.T; VAR ok: BOOLEAN)
  RAISES {Delta.Error, Access.Locked} =
  VAR
    curr: FilePos.T;
    pci : PersistentCheckpointInfo.T;
  BEGIN
    TRY
      ReadPersistentCheckpointInfo(delta, pci);
      curr := pci.deltas.info[delta.forward].current;
      IF FilePos.Compare(curr, pci.deltas.info[delta.forward].start) > 0
           AND FilePos.Compare(curr, pci.deltas.info[delta.forward].end)
                 <= 0 THEN
        delta.commands.setPosition(curr);
        delta.commands.backward();
        delta.commands.read(c);
        delta.commands.backward();
        delta.commands.getPosition(pci.deltas.info[delta.forward].current);
        WritePersistentCheckpointInfo(delta, pci);
        ok := TRUE;
      ELSE
        ok := FALSE;
      END;
    EXCEPT
      GraphCommandStream.EOS => ok := FALSE;
    | GraphCommandStream.ElementError =>
        RAISE Delta.Error(
                ErrorSupport.Create("PersistentDelta.Loop",
                                    "GraphCommandStream.ElementError"));
    | GraphCommandStream.InternalError (info) =>
        RAISE Delta.Error(ErrorSupport.Propagate(
                            "PersistentDelta.GetNextCommand",
                            "GraphCommandStream.InternalError", info));
    END;
  END GetPrevCommand;

BEGIN
END PersistentDelta.
