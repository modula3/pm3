MODULE PersistentCheckpointTree;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.3  1998/01/21 12:32:28  roland
    After open of an old tree, the stream must be positioned at the
    current checkpoint.

    Revision 1.2  1997/04/24 14:30:17  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:07  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.4  1996/11/20 12:21:08  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/09/23 08:35:24  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.2  1996/09/20 13:59:03  roland
    Implementation backstep/forstep. All redo commands as well as
    backstep/forstep testet.
    Persistent deltas should now be correct in multi-user mode - though
    this is not tested.

    Revision 1.1  1996/09/17 12:57:54  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT CheckpointTree AS Super;
IMPORT GraphCommandStream, PCIStream, PersistentCheckpointInfo,
       PersistentDelta, DeltaInfo, FilePos, Delta, Checkpoint;
IMPORT PageFile, Access, VirtualResource, CardSeq;
IMPORT Pathname, Word;
IMPORT ErrorSupport;

REVEAL
  T = Public BRANDED OBJECT
        cpinfo     : PCIStream.T          := NIL;
        forw, backw: GraphCommandStream.T := NIL;
      OVERRIDES
        open            := Open;
        close           := Close;
        getMode         := GetMode;
        setMode         := SetMode;
        createRoot      := CreateRoot;
        isEmpty         := IsEmpty;
        changes         := Changes;
        needsCheckpoint := NeedsCheckpoint;
        createSon       := CreateSon;
        noSons          := NoSons;
        actSon          := ActSon;
        setLabel        := SetLabel;
        getLabel        := GetLabel;
        getDeltas       := GetDeltas;
        getWriteDeltas  := GetWriteDeltas;
        getPath         := GetPath;
        gotoRoot        := GotoRoot;
        gotoFather      := GotoFather;
        gotoSon         := GotoSon;
        gotoNthSon      := GotoNthSon;
      END;


PROCEDURE EmptyDeltas (fpos, bpos: FilePos.T): DeltaInfo.T =
  VAR di: DeltaInfo.T;
  BEGIN
    di.state := DeltaInfo.State.Open;
    di.info[DeltaInfo.Forward] :=
      DeltaInfo.SingleDelta{
        costs := 0, start := fpos, end := fpos, current := fpos};
    di.info[DeltaInfo.Backward] :=
      DeltaInfo.SingleDelta{
        costs := 0, start := bpos, end := bpos, current := bpos};
    RETURN di;
  END EmptyDeltas;


PROCEDURE InternGotoRoot (tree: T; VAR oldpos: FilePos.T)
  RAISES {Access.Locked, Super.InternalError} =
  VAR start: FilePos.T;
  BEGIN
    TRY
      tree.cpinfo.getPosition(oldpos);
      tree.cpinfo.getFirstPosition(start);
      tree.cpinfo.setPosition(start);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE
          Super.InternalError(ErrorSupport.Propagate(
                                "PersistentCheckpointTree.InternGotoRoot",
                                "PCIStream.InternalError", info));
    END;
  END InternGotoRoot;

PROCEDURE ReadRoot (VAR tree: T; VAR info: PersistentCheckpointInfo.T)
  RAISES {Super.InternalError, Super.Empty, Access.Locked} =
  VAR curr: FilePos.T;
  BEGIN
    TRY
      InternGotoRoot(tree, curr);
      tree.cpinfo.read(info);
      tree.cpinfo.setPosition(curr);
    EXCEPT
      PCIStream.EOS => RAISE Super.Empty;
    | PCIStream.ElementError =>
        RAISE Super.InternalError(
                ErrorSupport.Create("PersistentCheckpointTree.ReadRoot",
                                    "PCIStream.ElementError"));
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentCheckpointTree.ReadRoot",
                                       "PCIStream.InternalError", info));
    END;
  END ReadRoot;

PROCEDURE WriteRoot (VAR tree: T; VAR info: PersistentCheckpointInfo.T)
  RAISES {Access.Locked, Super.InternalError} =
  VAR curr: FilePos.T;
  BEGIN
    TRY
      InternGotoRoot(tree, curr);
      tree.cpinfo.write(info, overwrite := FALSE);
      tree.cpinfo.setPosition(curr);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentCheckpointTree.WriteRoot",
                                    "PCIStream.InternalError", info));
    END;
  END WriteRoot;

PROCEDURE SetLabelMode (x: INTEGER; mode: Super.Mode): INTEGER =
  BEGIN
    IF mode = Super.Mode.Sequence THEN
      RETURN Word.Or(x, PersistentCheckpointInfo.ModeBit);
    ELSE
      RETURN Word.And(x, Word.Not(PersistentCheckpointInfo.ModeBit));
    END;
  END SetLabelMode;

PROCEDURE GetLabelMode (x: INTEGER): Super.Mode =
  BEGIN
    IF Word.And(x, PersistentCheckpointInfo.ModeBit) > 0 THEN
      RETURN Super.Mode.Sequence
    ELSE
      RETURN Super.Mode.Tree
    END;
  END GetLabelMode;

PROCEDURE SetLabelNeedsCheckpoint (x: INTEGER; need: BOOLEAN): INTEGER =
  BEGIN
    IF need THEN
      RETURN Word.Or(x, PersistentCheckpointInfo.NeedsCheckpointBit);
    ELSE
      RETURN
        Word.And(x, Word.Not(PersistentCheckpointInfo.NeedsCheckpointBit));
    END;
  END SetLabelNeedsCheckpoint;

PROCEDURE GetLabelNeedsCheckpoint (x: INTEGER): BOOLEAN =
  BEGIN
    RETURN Word.And(x, PersistentCheckpointInfo.NeedsCheckpointBit) > 0;
  END GetLabelNeedsCheckpoint;

(* Streams always move when reading or writing.  Therefore, we have to step
   back after reading or writing to ensure that the stream position always
   points to current node. *)
PROCEDURE ReadActual (tree: T; VAR info: PersistentCheckpointInfo.T)
  RAISES {Super.InternalError, Super.Empty, Access.Locked} =
  VAR curr: FilePos.T;
  BEGIN
    TRY
      tree.cpinfo.getPosition(curr);
      tree.cpinfo.read(info);
      tree.cpinfo.setPosition(curr);
    EXCEPT
      PCIStream.EOS => RAISE Super.Empty;
    | PCIStream.ElementError =>
        RAISE Super.InternalError(
                ErrorSupport.Create("PersistentCheckpointTree.WriteRoot",
                                    "PCIStream.ElementError"));
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentCheckpointTree.ReadActual",
                                    "PCIStream.InternalError", info));
    END;
  END ReadActual;

PROCEDURE WriteActual (tree: T; READONLY info: PersistentCheckpointInfo.T)
  RAISES {Access.Locked, Super.InternalError} =
  VAR curr: FilePos.T;
  BEGIN
    TRY
      tree.cpinfo.getPosition(curr);
      tree.cpinfo.write(info, overwrite := FALSE);
      tree.cpinfo.setPosition(curr);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentCheckpointTree.WriteActual",
                                    "PCIStream.InternalError", info));
    END;
  END WriteActual;

(* Interface procedures *)

PROCEDURE Open (tree    : T;
                resource: VirtualResource.T;
                path    : Pathname.T;
                access  : Access.Mode;
                new     : BOOLEAN;
                fw, bw  : GraphCommandStream.T): T
  RAISES {Access.Locked, PageFile.NoAccess, Super.InternalError,
          Access.Denied} =
  BEGIN
    TRY
      tree.cpinfo := NEW(PCIStream.T).open(
                       resource, path, access, new, forward := TRUE);
    EXCEPT
      PCIStream.DirectionMismatch =>
        RAISE Super.InternalError(
                ErrorSupport.Create("PersistentCheckpointTree.WriteActual",
                                    "PCIStream.DirectionMismatch"));
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentCheckpointTree.Open",
                                       "PCIStream.InternalError", info));
    END;
    tree.forw := fw;
    tree.backw := bw;
    RETURN tree;
  END Open;

PROCEDURE Close (tree: T) RAISES {Super.InternalError} =
  BEGIN
    TRY
      tree.cpinfo.close();
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentCheckpointTree.Close",
                                       "PCIStream.InternalError", info));
    END;
  END Close;

PROCEDURE GetMode (tree: T): Super.Mode
  RAISES {Access.Locked, Super.Empty, Super.InternalError} =
  VAR rootinfo: PersistentCheckpointInfo.T;
  BEGIN
    ReadRoot(tree, rootinfo);
    RETURN GetLabelMode(rootinfo.label);
  END GetMode;

PROCEDURE SetMode (tree: T; m: Super.Mode)
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR rootinfo: PersistentCheckpointInfo.T;
  BEGIN
    ReadRoot(tree, rootinfo);
    rootinfo.label := SetLabelMode(rootinfo.label, m);
    WriteRoot(tree, rootinfo);
  END SetMode;

PROCEDURE CreateRoot (tree: T)
  RAISES {Access.Locked, Super.InternalError} =
  VAR
    curr      : FilePos.T;
    rootinfo  : PersistentCheckpointInfo.T;
    fpos, bpos: FilePos.T;
    found     : BOOLEAN                    := TRUE;
    label     : INTEGER;
  BEGIN
    InternGotoRoot(tree, curr);
    TRY
      ReadActual(tree, rootinfo);
    EXCEPT
      Super.Empty, Super.InternalError => found := FALSE;
    END;
    IF NOT found THEN
      (* root does not exist -> create *)
      TRY
        tree.forw.getLastPosition(fpos);
        tree.backw.getFirstPosition(bpos);
        label := SetLabelMode(0, Super.Mode.Tree);
        label := SetLabelNeedsCheckpoint(label, TRUE);
        rootinfo :=
          PersistentCheckpointInfo.T{
            label := label, noOfSons := 0, actSonNo := 0, deltas :=
            EmptyDeltas(fpos, bpos), leftMostChild := FilePos.ZeroPos,
            rightSibling := FilePos.ZeroPos, father := FilePos.ZeroPos,
            actSon := FilePos.ZeroPos};
        WriteActual(tree, rootinfo);
      EXCEPT
      | GraphCommandStream.InternalError (info) =>
          RAISE Super.InternalError(
                  ErrorSupport.Propagate(
                    "PersistentCheckpointTree.CreateRoot",
                    "GraphCommandStream.InternalError", info));
      END;
    ELSE
      TRY
        (* restore current checkpoint *)
        tree.cpinfo.setPosition(curr);
      EXCEPT
        PCIStream.InternalError (info) =>
          RAISE Super.InternalError(ErrorSupport.Propagate(
                                "PersistentCheckpointTree.CreateRoot",
                                "PCIStream.InternalError", info));
      END;
    END;
  END CreateRoot;

PROCEDURE IsEmpty (tree: T): BOOLEAN
  RAISES {Super.InternalError, Access.Locked} =
  VAR
    rootinfo: PersistentCheckpointInfo.T;
    empty   : BOOLEAN                    := FALSE;
  BEGIN
    TRY ReadRoot(tree, rootinfo); EXCEPT Super.Empty => empty := TRUE; END;
    RETURN empty;
  END IsEmpty;

PROCEDURE Changes (tree: T)
  RAISES {Access.Locked, Super.Empty, Super.InternalError} =
  VAR root: PersistentCheckpointInfo.T;
  BEGIN
    ReadRoot(tree, root);
    root.label := SetLabelNeedsCheckpoint(root.label, TRUE);
    WriteRoot(tree, root);
  END Changes;

PROCEDURE NeedsCheckpoint (tree: T): BOOLEAN
  RAISES {Access.Locked, Super.Empty, Super.InternalError} =
  VAR root: PersistentCheckpointInfo.T;
  BEGIN
    ReadRoot(tree, root);
    RETURN GetLabelNeedsCheckpoint(root.label);
  END NeedsCheckpoint;

PROCEDURE CreateSon (             tree    : T;
                     <* UNUSED *> forward : Delta.T;
                     <* UNUSED *> backward: Delta.T  )
  RAISES {Super.InternalError, Super.Empty, Access.Locked} =
  VAR
    info, hinfo, root: PersistentCheckpointInfo.T;
    insertat         : FilePos.T;
    fpos, bpos       : FilePos.T;
  BEGIN
    TRY
      (* Initialize checkpoint info for new son *)
      (* 1.  relatives *)
      tree.cpinfo.getPosition(info.father);
      info.leftMostChild := FilePos.ZeroPos;
      info.rightSibling := FilePos.ZeroPos;
      info.actSon := FilePos.ZeroPos;
      info.actSonNo := 0;
      info.noOfSons := 0;
      (* 2.  checkpoint information *)
      info.label := Checkpoint.NoLabel;
      (* take actual deltas from root and create new delta information for
         root *)
      ReadRoot(tree, root);
      info.deltas := root.deltas;
      info.deltas.state := DeltaInfo.State.Closed;
      IF GetLabelMode(root.label) = Super.Mode.Sequence THEN
        tree.forw.truncate();
        tree.backw.truncate();
      END;
      tree.forw.getLastPosition(fpos);
      tree.backw.getFirstPosition(bpos);
      root.deltas := EmptyDeltas(fpos, bpos);
      root.label := SetLabelNeedsCheckpoint(root.label, FALSE);
      WriteRoot(tree, root);

      (* where will new son be placed ? *)
      tree.cpinfo.getLastPosition(insertat);

      (* Insert son.  First, read in the father information. *)
      ReadActual(tree, hinfo);
      INC(hinfo.noOfSons);
      hinfo.actSonNo := hinfo.noOfSons;
      hinfo.actSon := insertat;
      (* Has the father sons ? *)
      IF hinfo.leftMostChild = FilePos.ZeroPos THEN
        (* No, the new son is the first son. *)
        hinfo.leftMostChild := insertat;
        WriteActual(tree, hinfo);
      ELSE
        (* We have to write father information back (noOfSons etc.
           changed) *)
        WriteActual(tree, hinfo);

        (* Traverse all sons and insert as rightSibling of the right most
           son. *)
        tree.cpinfo.setPosition(hinfo.leftMostChild);
        ReadActual(tree, hinfo);
        WHILE hinfo.rightSibling # FilePos.ZeroPos DO
          tree.cpinfo.setPosition(hinfo.rightSibling);
          ReadActual(tree, hinfo);
        END;
        hinfo.rightSibling := insertat;
        WriteActual(tree, hinfo);
      END;
      (* Finally, write the new checkpoint to the stream. *)
      tree.cpinfo.setPosition(insertat);
      WriteActual(tree, info);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentCheckpointTree.CreateSon",
                                    "PCIStream.InternalError", info));
    | GraphCommandStream.InternalError (info) =>
        RAISE
          Super.InternalError(ErrorSupport.Propagate(
                                "PersistentCheckpointTree.CreateSon",
                                "GraphCommandStream.InternalError", info));
    END;
  END CreateSon;

PROCEDURE NoSons (tree: T): CARDINAL
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR info: PersistentCheckpointInfo.T;
  BEGIN
    ReadActual(tree, info);
    RETURN info.noOfSons;
  END NoSons;

PROCEDURE ActSon (tree: T): CARDINAL
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR info: PersistentCheckpointInfo.T;
  BEGIN
    ReadActual(tree, info);
    RETURN info.actSonNo;
  END ActSon;

PROCEDURE SetLabel (tree: T; l: INTEGER)
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR info: PersistentCheckpointInfo.T;
  BEGIN
    ReadActual(tree, info);
    info.label := l;
    WriteActual(tree, info);
  END SetLabel;

PROCEDURE GetLabel (tree: T; VAR l: INTEGER; VAR ok: BOOLEAN)
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR info: PersistentCheckpointInfo.T;
  BEGIN
    ReadActual(tree, info);
    ok := FALSE;
    IF info.label > Checkpoint.NoLabel THEN ok := TRUE; END;
    l := info.label;
  END GetLabel;

PROCEDURE GetDeltas (tree: T; VAR forward, backward: Delta.T)
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR
    info: PersistentCheckpointInfo.T;
    pos : FilePos.T;
  BEGIN
    TRY
      tree.cpinfo.getPosition(pos);
      ReadActual(tree, info);
      forward := NEW(PersistentDelta.T).init(
                   tree.cpinfo, tree.forw, forward := TRUE, cppos := pos);
      backward :=
        NEW(PersistentDelta.T).init(
          tree.cpinfo, tree.backw, forward := FALSE, cppos := pos);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentCheckpointTree.GetDeltas",
                                    "PCIStream.InternalError", info));
    END;
  END GetDeltas;

PROCEDURE GetWriteDeltas (tree: T; VAR forward, backward: Delta.T)
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR
    info: PersistentCheckpointInfo.T;
    pos : FilePos.T;
  BEGIN
    TRY
      tree.cpinfo.getFirstPosition(pos);
      ReadRoot(tree, info);
      forward := NEW(PersistentDelta.T).init(
                   tree.cpinfo, tree.forw, forward := TRUE, cppos := pos);
      backward :=
        NEW(PersistentDelta.T).init(
          tree.cpinfo, tree.backw, forward := FALSE, cppos := pos);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE
          Super.InternalError(ErrorSupport.Propagate(
                                "PersistentCheckpointTree.GetWriteDeltas",
                                "PCIStream.InternalError", info));
    END;
  END GetWriteDeltas;

PROCEDURE GetPath (tree: T): CardSeq.T
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR
    path: CardSeq.T;
    cpi : PersistentCheckpointInfo.T;
    act : FilePos.T;
  BEGIN
    TRY
      path := NEW(CardSeq.T).init(0);
      tree.cpinfo.getPosition(act);
      tree.cpinfo.read(cpi);
      (* go up until root is reached and collect the numbers of the actual
         sons *)
      WHILE NOT cpi.father = FilePos.ZeroPos DO
        tree.cpinfo.setPosition(cpi.father);
        tree.cpinfo.read(cpi);
        path.addhi(cpi.actSonNo);
      END;

      (* go back down *)
      tree.cpinfo.setPosition(act);
    EXCEPT
      PCIStream.EOS => RAISE Super.Empty;
    | PCIStream.ElementError =>
        RAISE
          Super.InternalError(
            ErrorSupport.Create("PersistentCheckpointTree.GetWriteDeltas",
                                "PCIStream.ElementError"));
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentCheckpointTree.GetPath",
                                       "PCIStream.InternalError", info));
    END;
    RETURN path;
  END GetPath;

PROCEDURE GotoRoot (tree: T)
  RAISES {Super.Empty, Super.InternalError, Access.Locked} =
  VAR
    curr : FilePos.T;
    dummy: PersistentCheckpointInfo.T;
  BEGIN
    InternGotoRoot(tree, curr);
    (* Force an exception if tree is empty *)
    ReadActual(tree, dummy);
  END GotoRoot;

PROCEDURE GotoFather (tree: T) RAISES {Super.NoSuchNode,
                                       Super.InternalError, Super.Empty,
                                       Access.Locked} =
  VAR info: PersistentCheckpointInfo.T;
  BEGIN
    ReadActual(tree, info);
    IF info.father = FilePos.ZeroPos THEN
      (* we are at the root *)
      RAISE Super.NoSuchNode;
    END;
    TRY
      tree.cpinfo.setPosition(info.father);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentCheckpointTree.GotoFather",
                                    "PCIStream.InternalError", info));
    END;
  END GotoFather;

PROCEDURE GotoSon (tree: T; which: Super.SonPosition)
  RAISES {Super.NoSuchNode, Super.Empty, Super.InternalError, Access.Locked} =
  VAR
    finfo, sinfo: PersistentCheckpointInfo.T;
    son, father : FilePos.T;
  BEGIN
    TRY
      tree.cpinfo.getPosition(father);
      ReadActual(tree, finfo);
      CASE which OF
        Super.SonPosition.Previous =>
          IF finfo.actSonNo > 1 THEN
            DEC(finfo.actSonNo);
          ELSE
            RAISE Super.NoSuchNode;
          END;
      | Super.SonPosition.Actual =>
          IF finfo.actSonNo = 0 THEN RAISE Super.NoSuchNode; END;
      | Super.SonPosition.Next =>
          IF finfo.actSonNo < finfo.noOfSons THEN
            INC(finfo.actSonNo);
          ELSE
            RAISE Super.NoSuchNode;
          END;
      END;
      (* Go to the chosen son *)
      son := finfo.leftMostChild;
      FOR i := 1 TO finfo.actSonNo DO
        tree.cpinfo.setPosition(son);
        ReadActual(tree, sinfo);
        IF i < finfo.actSonNo THEN
          son := sinfo.rightSibling;
          IF son = FilePos.ZeroPos THEN
            RAISE Super.InternalError(
                    ErrorSupport.Create("PersistentCheckpointTree.GotoSon",
                                        "Corrupt chechpoint tree."))
          END;
        END;
      END;
      (* now son holds the position of the new actual son *)
      finfo.actSon := son;
      tree.cpinfo.setPosition(father);
      WriteActual(tree, finfo);
      tree.cpinfo.setPosition(son);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentCheckpointTree.GotoSon",
                                       "PCIStream.InternalError", info));
    END;
  END GotoSon;

PROCEDURE GotoNthSon (tree: T; which: CARDINAL)
  RAISES {Super.NoSuchNode, Super.Empty, Super.InternalError, Access.Locked} =
  VAR
    finfo, sinfo: PersistentCheckpointInfo.T;
    son, father : FilePos.T;
  BEGIN
    TRY
      tree.cpinfo.getPosition(father);
      ReadActual(tree, finfo);
      IF which < 1 OR which > finfo.noOfSons THEN
        RAISE Super.NoSuchNode;
      END;
      finfo.actSonNo := which;
      (* Go to the chosen son *)
      son := finfo.leftMostChild;
      FOR i := 1 TO finfo.actSonNo DO
        tree.cpinfo.setPosition(son);
        ReadActual(tree, sinfo);
        IF i < finfo.actSonNo THEN
          son := sinfo.rightSibling;
          IF son = FilePos.ZeroPos THEN
            RAISE
              Super.InternalError(
                ErrorSupport.Create("PersistentCheckpointTree.GotoNthSon",
                                    "Corrupt checkpoint tree."))
          END;
        END;
      END;
      (* now son holds the position of the new actual son *)
      finfo.actSon := son;
      tree.cpinfo.setPosition(father);
      WriteActual(tree, finfo);
      tree.cpinfo.setPosition(son);
    EXCEPT
    | PCIStream.InternalError (info) =>
        RAISE Super.InternalError(ErrorSupport.Propagate(
                                    "PersistentCheckpointTree.GotoNthSon",
                                    "PCIStream.InternalError", info));
    END;
  END GotoNthSon;

BEGIN
END PersistentCheckpointTree.
