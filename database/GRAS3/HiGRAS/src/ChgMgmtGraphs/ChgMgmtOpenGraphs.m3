MODULE ChgMgmtOpenGraphs;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1998/05/19 10:30:02  roland
    Support for log-groups implemented.

*)
(***************************************************************************)

IMPORT PersistentGraph, Pathname;

REVEAL
  T = Public BRANDED OBJECT
        tbl: OGITable;
      OVERRIDES
        init           := Init;
        createEntry    := CreateEntry;
        free           := Free;
        exists         := Exists;
        putLogHandle   := PutLogHandle;
        putOpenCounter := PutOpenCounter;
        getLogHandle   := GetLogHandle;
        getVisibleName := GetVisibleName;
        getGraph       := GetGraph;
        getWorkingName := GetWorkingName;
        getOpenCounter := GetOpenCounter;
      END;

CONST OGITblSize = 53;

TYPE
  OpenGraphInfo = RECORD
                    number     : CARDINAL;
                    graph      : PersistentGraph.T;
                    visibleName: TEXT;
                    workingName: TEXT;
                    openCount  : CARDINAL;
                    loghandle  : CARDINAL;
                    next       : REF OpenGraphInfo;
                  END;

  OGIIndex = [0 .. OGITblSize - 1];

  OGIEntries = ARRAY OGIIndex OF REF OpenGraphInfo;

  OGITable = OGIEntries;

PROCEDURE Init (og: T): T =
  BEGIN
    og.tbl := OGIEntries{NIL, ..};
    RETURN og;
  END Init;

PROCEDURE CreateEntry (og     : T;
                       number : CARDINAL;
                       graph  : PersistentGraph.T;
                       vname  : Pathname.T;
                       wname  : Pathname.T;
                       counter: CARDINAL           ) =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, number, pred := pred, entry := entry);
    IF entry = NIL THEN
      entry := NEW(REF OpenGraphInfo, number := number, graph := graph,
                   visibleName := vname, workingName := wname,
                   openCount := counter, loghandle := 0, next := NIL);
      IF pred = NIL THEN
        entry.next := og.tbl[OGIHash(number)];
        og.tbl[OGIHash(number)] := entry;
      ELSE
        entry.next := pred.next;
        pred.next := entry;
      END;
    ELSE
      <* ASSERT FALSE *>
    END;
  END CreateEntry;

PROCEDURE Free (og: T; graph: CARDINAL) =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    IF entry # NIL THEN
      IF pred = NIL THEN
        og.tbl[OGIHash(graph)] := entry.next;
      ELSE
        pred.next := entry.next;
      END;
      entry^ := NullGraphInfo;
    END;
  END Free;

PROCEDURE Exists (og: T; graph: CARDINAL): BOOLEAN =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    RETURN entry # NIL;
  END Exists;

PROCEDURE PutLogHandle (og: T; graph: CARDINAL; log: CARDINAL) =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    IF entry = NIL THEN
      <* ASSERT FALSE *>
    ELSE
      entry.loghandle := log;
    END;
  END PutLogHandle;

PROCEDURE PutOpenCounter (og: T; graph: CARDINAL; count: CARDINAL) =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    IF entry = NIL THEN
      <* ASSERT FALSE *>
    ELSE
      entry.openCount := count;
    END;
  END PutOpenCounter;

PROCEDURE GetLogHandle (og: T; graph: CARDINAL): CARDINAL =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    IF entry = NIL THEN
      <* ASSERT FALSE *>
    ELSE
      RETURN entry.loghandle;
    END;
  END GetLogHandle;

PROCEDURE GetVisibleName (og: T; graph: CARDINAL): Pathname.T =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    IF entry = NIL THEN
      <* ASSERT FALSE *>
    ELSE
      RETURN entry.visibleName;
    END;
  END GetVisibleName;

PROCEDURE GetGraph (og: T; graph: CARDINAL): PersistentGraph.T
  RAISES {NotOpen} =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    IF entry = NIL THEN RAISE NotOpen; ELSE RETURN entry.graph; END;
  END GetGraph;

PROCEDURE GetWorkingName (og: T; graph: CARDINAL): Pathname.T =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    IF entry = NIL THEN
      <* ASSERT FALSE *>
    ELSE
      RETURN entry.workingName;
    END;
  END GetWorkingName;

PROCEDURE GetOpenCounter (og: T; graph: CARDINAL): CARDINAL =
  VAR entry, pred: REF OpenGraphInfo;
  BEGIN
    OGIFindEntry(og.tbl, graph, pred := pred, entry := entry);
    IF entry = NIL THEN
      <* ASSERT FALSE *>
    ELSE
      RETURN entry.openCount;
    END;
  END GetOpenCounter;

CONST
  NullGraphInfo = OpenGraphInfo{number := 0, graph := NIL, visibleName :=
                                NIL, workingName := NIL, openCount := 0,
                                loghandle := 0, next := NIL};

PROCEDURE OGIHash (gnumber: CARDINAL): OGIIndex =
  BEGIN
    RETURN gnumber MOD OGITblSize;
  END OGIHash;

PROCEDURE OGIFindEntry (READONLY tbl        : OGITable;
                                 gnumber    : CARDINAL;
                        VAR      pred, entry: REF OpenGraphInfo) =
  VAR key: OGIIndex := OGIHash(gnumber);
  BEGIN
    pred := NIL;
    entry := tbl[key];
    WHILE entry # NIL AND entry.number < gnumber DO
      pred := entry;
      entry := entry.next;
    END;
    IF entry # NIL AND entry.number # gnumber THEN entry := NIL; END;
  END OGIFindEntry;

BEGIN
END ChgMgmtOpenGraphs.
