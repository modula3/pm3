MODULE VolatileLabelTable;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/04/23 13:35:15  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/09/17 12:58:38  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT LabelTable AS Super;
IMPORT LabelPathTbl, CardSeq;

REVEAL T = Public BRANDED OBJECT
             tbl: LabelPathTbl.T := NIL;
           OVERRIDES
             init := Init;
             insert := Insert;
             remove := Remove;
             contains := Contains;
             lookup := Lookup;
           END;

PROCEDURE Init(tab: T): T =
  BEGIN
    tab.tbl := NEW(LabelPathTbl.Default).init(0);
    RETURN tab;
  END Init;

PROCEDURE Insert (tab: T; key: CARDINAL; path: CardSeq.T) =
  BEGIN
    EVAL tab.tbl.put(key, path);
  END Insert;

PROCEDURE Remove (tab: T; key: CARDINAL) =
  VAR dummy: CardSeq.T;
  BEGIN
    EVAL tab.tbl.delete(key, dummy);
  END Remove;

PROCEDURE Contains (tab: T; key: CARDINAL): BOOLEAN =
  VAR dummy: CardSeq.T;
  BEGIN
    RETURN tab.tbl.get(key, dummy);
  END Contains;

PROCEDURE Lookup (tab: T; key: CARDINAL): CardSeq.T RAISES {Super.NotFound} =
  VAR res: CardSeq.T := NIL;
  BEGIN
    IF tab.tbl.get(key, res) THEN
      RETURN res;
    ELSE
      RAISE Super.NotFound;
    END;
  END Lookup;

       
BEGIN
END VolatileLabelTable.
