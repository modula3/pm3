MODULE VolatileCheckpointTree;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1997/07/21 10:39:37  roland
    A few bug-fixes and implementation of free memory list logging.

    Revision 1.1  1997/04/23 13:35:05  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.3  1996/11/20 12:21:38  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.2  1996/09/23 08:35:50  roland
    Persistent Deltas are now stored as part of the checkpoint
    tree. Backstep and forstep work.
    Names will be used to hold information for ChgMgmtGraphSystem.

    Revision 1.1  1996/09/17 12:58:33  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT CheckpointTree AS Super;
IMPORT CINTree, Delta, Checkpoint, VolatileDelta;
IMPORT CardSeq;
IMPORT ErrorSupport;

REVEAL
  T = Public BRANDED OBJECT
        cpinfo  : CINTree.T    := NIL;
        mode    : Super.Mode;
        deltasCh: BOOLEAN      := TRUE;
      OVERRIDES
        init            := Init;
        getMode         := GetMode;
        setMode         := SetMode;
        createRoot      := CreateRoot;
        isEmpty         := IsEmpty;
        needsCheckpoint := NeedsCheckpoint;
        changes         := Changes;
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

(* Interface procedures *)


PROCEDURE Init (tree: T): T =
  BEGIN
    tree.cpinfo := NEW(CINTree.T).init();
    tree.mode := Super.Mode.Tree;
    tree.deltasCh := TRUE;
    RETURN tree;
  END Init;

PROCEDURE GetMode (tree: T): Super.Mode RAISES {} =
  BEGIN
    RETURN tree.mode;
  END GetMode;

PROCEDURE SetMode (tree: T; m: Super.Mode) RAISES {} =
  BEGIN
    tree.mode := m;
  END SetMode;

PROCEDURE CreateRoot (tree: T) RAISES {} =
  BEGIN
    tree.cpinfo.createRoot(
      Checkpoint.T{label := Checkpoint.NoLabel, forw := NIL, backw := NIL});
    tree.deltasCh := TRUE;
  END CreateRoot;

PROCEDURE IsEmpty (tree: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN tree.cpinfo.isEmpty();
  END IsEmpty;

PROCEDURE Changes (tree: T) RAISES {} =
  BEGIN
    tree.deltasCh := TRUE;
  END Changes;

PROCEDURE NeedsCheckpoint (tree: T): BOOLEAN RAISES {} =
  BEGIN
    RETURN tree.deltasCh;
  END NeedsCheckpoint;

PROCEDURE CreateSon (tree: T; forward, backward: Delta.T)
  RAISES {Super.Empty} =
  BEGIN
    TRY
      tree.cpinfo.addSon(Checkpoint.T{label := Checkpoint.NoLabel, forw :=
                                      forward, backw := backward});
      tree.deltasCh := FALSE;
    EXCEPT
      CINTree.Empty => RAISE Super.Empty;
    END;
  END CreateSon;

PROCEDURE NoSons (tree: T): CARDINAL RAISES {Super.Empty} =
  BEGIN
    TRY
      RETURN tree.cpinfo.noOfSons();
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    END;
  END NoSons;

PROCEDURE ActSon (tree: T): CARDINAL RAISES {Super.Empty} =
  BEGIN
    TRY
      RETURN tree.cpinfo.actSonNo();
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    | CINTree.NotExist => RETURN 0;
    END;
  END ActSon;

PROCEDURE SetLabel (tree: T; l: INTEGER) RAISES {Super.Empty} =
  VAR cp: Checkpoint.T;
  BEGIN
    TRY
      tree.cpinfo.getCurrent(cp);
      cp.label := l;
      tree.cpinfo.setCurrent(cp);
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    END;
  END SetLabel;

PROCEDURE GetLabel (tree: T; VAR l: INTEGER; VAR ok: BOOLEAN)
  RAISES {Super.Empty} =
  VAR cp: Checkpoint.T;
  BEGIN
    ok := FALSE;
    TRY
      tree.cpinfo.getCurrent(cp);
      l := cp.label;
      ok := TRUE;
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    END;
  END GetLabel;

PROCEDURE GetDeltas (tree: T; VAR forward, backward: Delta.T)
  RAISES {Super.Empty} =
  VAR cp: Checkpoint.T;
  BEGIN
    TRY
      tree.cpinfo.getCurrent(cp);
      forward := cp.forw;
      backward := cp.backw;
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    END;
  END GetDeltas;

PROCEDURE GetWriteDeltas (<* UNUSED *>     tree             : T;
                                       VAR forward, backward: Delta.T) =
  BEGIN
    forward := VolatileDelta.New().init(forward := TRUE);
    backward := VolatileDelta.New().init(forward := FALSE);
  END GetWriteDeltas;

PROCEDURE GetPath (tree: T): CardSeq.T
  RAISES {Super.Empty, Super.InternalError} =
  VAR
    path : CardSeq.T;
    level: CARDINAL;
  BEGIN
    TRY
      level := tree.cpinfo.depth();
      path := NEW(CardSeq.T).init(level);
      (* go up until root is reached and collect the numbers of the actual
         sons *)
      WHILE NOT tree.cpinfo.atRoot() DO
        tree.cpinfo.goFather();
        path.addhi(tree.cpinfo.actSonNo());
      END;

      (* go back down *)
      FOR i := level - 1 TO 0 BY -1 DO
        tree.cpinfo.goIthSon(path.get(i));
      END;
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    | CINTree.NotExist =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "VolatileCheckpointTree.GetPath", "CINTree.NotExist"));
    END;
    RETURN path;
  END GetPath;

PROCEDURE GotoRoot (tree: T) RAISES {Super.Empty} =
  BEGIN
    TRY
      tree.cpinfo.goRoot();
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    END;
  END GotoRoot;

PROCEDURE GotoFather (tree: T) RAISES {Super.NoSuchNode, Super.Empty} =
  BEGIN
    TRY
      tree.cpinfo.goFather();
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    | CINTree.NotExist => RAISE Super.NoSuchNode
    END;
  END GotoFather;

PROCEDURE GotoSon (tree: T; which: Super.SonPosition)
  RAISES {Super.NoSuchNode, Super.Empty} =
  BEGIN
    TRY
      CASE which OF
        Super.SonPosition.Previous => tree.cpinfo.goPrevSon()
      | Super.SonPosition.Actual => tree.cpinfo.goActSon()
      | Super.SonPosition.Next => tree.cpinfo.goNextSon();
      END;
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    | CINTree.NotExist => RAISE Super.NoSuchNode
    END;
  END GotoSon;

PROCEDURE GotoNthSon (tree: T; which: CARDINAL)
  RAISES {Super.NoSuchNode, Super.Empty} =
  BEGIN
    TRY
      tree.cpinfo.goIthSon(which);
    EXCEPT
      CINTree.Empty => RAISE Super.Empty
    | CINTree.NotExist => RAISE Super.NoSuchNode
    END;
  END GotoNthSon;

BEGIN
END VolatileCheckpointTree.
