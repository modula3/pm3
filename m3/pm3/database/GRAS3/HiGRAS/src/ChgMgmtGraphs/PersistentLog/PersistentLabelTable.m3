MODULE PersistentLabelTable;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.3  1998/01/21 12:31:12  roland
    Bugfixes in finding and reading path information.

    Revision 1.2  1997/04/24 14:30:23  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:34:16  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.2  1996/11/20 12:21:13  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.1  1996/09/17 12:58:01  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT LabelTable AS Super;
IMPORT LPIStream, LabelPathInfo, FilePos;
IMPORT Access, PageFile, VirtualResource, Pathname, CardSeq;
IMPORT ErrorSupport;

REVEAL
  T = Public BRANDED OBJECT
        tree: LPIStream.T := NIL;
      OVERRIDES
        open     := Open;
        close    := Close;
        insert   := Insert;
        remove   := Remove;
        contains := Contains;
        lookup   := Lookup;
      END;

(* If nodes of the tree must be deleted, they are not actually deleted in
   the stream.  All references to tehm are removed and they are simply
   wasted memory of the stream.  For this reason we need an extra root node
   that points to the actual root of the tree.  In that way we can handle
   the deletion of the root node of the tree. *)

PROCEDURE Find (    tab    : T;
                    lab    : CARDINAL;
                VAR lpi    : LabelPathInfo.T;
                VAR pos    : FilePos.T;
                VAR flpi   : LabelPathInfo.T;
                VAR father : FilePos.T;
                VAR leftSon: BOOLEAN          ): BOOLEAN
  RAISES {Super.InternalError} =
  VAR found, eos: BOOLEAN := FALSE;
  BEGIN
    TRY
      (* goto stream root *)
      tab.tree.getFirstPosition(pos);
      tab.tree.setPosition(pos);
      tab.tree.read(flpi);
    EXCEPT
      (* This node must exists, so if eos is signalled this is a severe
         error *)
      LPIStream.EOS =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLabelTable.Find", "LPIStream.EOS"));
    | Access.Locked =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLabelTable.Find", "Access.Locked"));
    | LPIStream.ElementError =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLabelTable.Find", "LPIStream.ElementError"));
    | LPIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentLabelTable.Find",
                                       "LPIStream.InternalError", info));
    END;
    (* now go to the root of the tree *)
    father := pos;
    pos := flpi.left;
    leftSon := TRUE;
    TRY
      IF pos # FilePos.ZeroPos THEN
        tab.tree.setPosition(pos);
      ELSE
        (* tree is empty *)
        RETURN FALSE;
      END;

      (* traverse the tree *)
      WHILE NOT found AND NOT eos DO
        tab.tree.read(lpi);
        IF lpi.label = lab THEN
          found := TRUE;
        ELSIF lab < lpi.label THEN
          flpi := lpi;
          father := pos;
          leftSon := TRUE;
          pos := lpi.left;
          IF lpi.left # FilePos.ZeroPos THEN
            tab.tree.setPosition(lpi.left);
          ELSE
            eos := TRUE;
          END;
        ELSE
          flpi := lpi;
          father := pos;
          leftSon := FALSE;
          pos := lpi.right;
          IF lpi.right # FilePos.ZeroPos THEN
            tab.tree.setPosition(lpi.right);
          ELSE
            eos := TRUE;
          END;
        END;
      END;
    EXCEPT
      LPIStream.EOS => eos := TRUE;
    | Access.Locked =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLabelTable.Find", "Access.Locked"));
    | LPIStream.ElementError =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLabelTable.Find", "LPIStream.ElementError"));
    | LPIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentLabelTable.Find",
                                       "LPIStream.InternalError", info));
    END;
    RETURN found;
  END Find;

(* Interface procedures *)

PROCEDURE Open (tab     : T;
                resource: VirtualResource.T;
                path    : Pathname.T;
                access  : Access.Mode;
                new     : BOOLEAN            ): T
  RAISES {Access.Denied, Access.Locked, PageFile.NoAccess,
          Super.InternalError} =
  BEGIN
    TRY
      tab.tree := NEW(LPIStream.T).open(
                    resource, path, access, new, forward := TRUE);
    EXCEPT
      LPIStream.DirectionMismatch =>
        RAISE Super.InternalError(
                ErrorSupport.Create("PersistentLabelTable.Open",
                                    "LPIStream.DirectionMismatch "));
    | LPIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentLabelTable.Open",
                                       "LPIStream.InternalError", info));
    END;

    IF new THEN
      VAR abort: BOOLEAN := FALSE;
      <* FATAL VirtualResource.NotInTransaction *>
      BEGIN
        TRY
          TRY
            (* insert the stream root *)
            VirtualResource.T.beginTransaction(resource);
            tab.tree.write(
              LabelPathInfo.T{label := 0, left := FilePos.ZeroPos, right :=
                              FilePos.ZeroPos, path := NIL},
              overwrite := FALSE);
          EXCEPT
            Access.Locked =>
              abort := TRUE;
              RAISE Super.InternalError(
                      ErrorSupport.Create(
                        "PersistentLabelTable.Open", "Access.Locked"));
          | LPIStream.InternalError (info) =>
              abort := TRUE;
              RAISE
                Super.InternalError(
                  ErrorSupport.Propagate("PersistentLabelTable.Open",
                                         "LPIStream.InternalError", info));
          | VirtualResource.FatalError (info) =>
              abort := TRUE;
              RAISE
                Super.InternalError(ErrorSupport.Propagate(
                                      "PersistentLabelTable.Open",
                                      "VirtualResource.FatalError", info));
          END;
        FINALLY
          TRY
            IF abort THEN
              VirtualResource.T.abortTransaction(resource);
            ELSE
              VirtualResource.T.commitTransaction(resource);
            END;
          EXCEPT
            VirtualResource.FatalError => (* ignore *)
          END;
        END;
      END;
    END;
    RETURN tab;
  END Open;

PROCEDURE Close (tab: T) RAISES {Super.InternalError} =
  BEGIN
    TRY
      tab.tree.close()
    EXCEPT
    | LPIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentLabelTable.Close",
                                       "LPIStream.InternalError", info));
    END;
  END Close;

PROCEDURE Insert (tab: T; key: CARDINAL; path: CardSeq.T)
  RAISES {Super.InternalError} =
  VAR
    fpos, pos, opos : FilePos.T;
    nlpi, olpi, flpi: LabelPathInfo.T;
    isLeftSon       : BOOLEAN;
  BEGIN
    nlpi.label := key;
    nlpi.path := path;
    IF Find(tab, key, olpi, opos, flpi, fpos, isLeftSon) THEN
      nlpi.left := olpi.left;
      nlpi.right := olpi.right;
    ELSE
      nlpi.left := FilePos.ZeroPos;
      nlpi.right := FilePos.ZeroPos;
    END;

    TRY
      (* write new node *)
      tab.tree.getLastPosition(pos);
      tab.tree.setPosition(pos);
      tab.tree.write(nlpi, overwrite := FALSE);

      (* update and write father *)
      IF isLeftSon THEN flpi.left := pos; ELSE flpi.right := pos; END;
      tab.tree.setPosition(fpos);
      tab.tree.write(flpi, overwrite := FALSE);
    EXCEPT
      Access.Locked =>
        RAISE Super.InternalError(
                ErrorSupport.Create(
                  "PersistentLabelTable.Insert", "Access.Locked"));
    | LPIStream.InternalError (info) =>
        RAISE Super.InternalError(
                ErrorSupport.Propagate("PersistentLabelTable.Insert",
                                       "LPIStream.InternalError", info));
    END;
    (* the old node (if it existed) is no longer accessible now *)
  END Insert;

PROCEDURE Remove (tab: T; key: CARDINAL) RAISES {Super.InternalError} =
  VAR
    fpos, pos, opos, spos : FilePos.T;
    nlpi, olpi, flpi, slpi: LabelPathInfo.T;
    isLeftSon             : BOOLEAN;
  BEGIN
    IF Find(tab, key, olpi, opos, flpi, fpos, isLeftSon) THEN
      IF olpi.left = FilePos.ZeroPos THEN
        IF isLeftSon THEN
          flpi.left := olpi.right;
        ELSE
          flpi.right := olpi.right;
        END;
      ELSIF olpi.right = FilePos.ZeroPos THEN
        IF isLeftSon THEN
          flpi.left := olpi.right;
        ELSE
          flpi.right := olpi.right;
        END;
      ELSE
        (* find the symmetric successor *)
        TRY
          (* pos points to the position of the symmetric successor, spos to
             its father *)
          spos := opos;
          slpi := olpi;
          (* one step right ... *)
          pos := slpi.right;
          tab.tree.setPosition(pos);
          tab.tree.read(nlpi);
          (* and then left as long as possible *)
          WHILE nlpi.left # FilePos.ZeroPos DO
            spos := pos;
            slpi := nlpi;
            pos := nlpi.left;
            tab.tree.setPosition(pos);
            tab.tree.read(nlpi);
          END;
          (* father of deleted node gets a new successor: *)
          IF isLeftSon THEN flpi.left := pos; ELSE flpi.right := pos; END;
          tab.tree.setPosition(fpos);
          tab.tree.write(flpi, overwrite := FALSE);

          (* now unchain symmetric successor from its father *)
          (* if spos = opos, we only went one step to the right and nothing
             has to be done, because olpi will never be accessed any more
             (we already changed the father to point to the new son) *)
          IF spos # opos THEN
            slpi.left := nlpi.right;
            tab.tree.setPosition(spos);
            tab.tree.write(slpi, overwrite := FALSE);
          END;

          (* finally, we have to assign the children of the delted node to
             the symmetrich successor *)
          nlpi.left := olpi.left;
          nlpi.right := olpi.right;
          tab.tree.setPosition(pos);
          tab.tree.write(nlpi, overwrite := FALSE);
        EXCEPT
          Access.Locked =>
            RAISE Super.InternalError(
                    ErrorSupport.Create(
                      "PersistentLabelTable.Remove", "Access.Locked"));
        | LPIStream.EOS =>
            RAISE Super.InternalError(
                    ErrorSupport.Create(
                      "PersistentLabelTable.Remove", "LPIStream.EOS"));
        | LPIStream.ElementError =>
            RAISE Super.InternalError(
                    ErrorSupport.Create("PersistentLabelTable.Remove",
                                        "LPIStream.ElementError"));
        | LPIStream.InternalError (info) =>
            RAISE Super.InternalError(ErrorSupport.Propagate(
                                        "PersistentLabelTable.Remove",
                                        "LPIStream.InternalError", info));
        END;
      END;
    END;
  END Remove;

PROCEDURE Contains (tab: T; key: CARDINAL): BOOLEAN
  RAISES {Super.InternalError} =
  VAR
    fpos, opos: FilePos.T;
    olpi, flpi: LabelPathInfo.T;
    isLeftSon : BOOLEAN;
  BEGIN
    RETURN Find(tab, key, olpi, opos, flpi, fpos, isLeftSon);
  END Contains;

PROCEDURE Lookup (tab: T; key: CARDINAL): CardSeq.T
  RAISES {Super.InternalError, Super.NotFound} =
  VAR
    fpos, opos: FilePos.T;
    olpi, flpi: LabelPathInfo.T;
    isLeftSon : BOOLEAN;
  BEGIN
    IF Find(tab, key, olpi, opos, flpi, fpos, isLeftSon) THEN
      RETURN olpi.path;
    ELSE
      RAISE Super.NotFound;
    END;
  END Lookup;


BEGIN
END PersistentLabelTable.
