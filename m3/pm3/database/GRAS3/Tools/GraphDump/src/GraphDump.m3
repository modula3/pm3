MODULE GraphDump EXPORTS Main;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.8  1998/03/18 13:41:06  roland
    New command line option '-local' enables the tool to work with the
    client part of a pool.

    Revision 1.7  1998/01/21 16:07:05  roland
    GraphDump now uses XAttributeValue interfaces to decode attributes and
    print them in a readable form.

    Revision 1.6  1997/11/20 10:16:45  roland
    Subtle changes in ReadParams.

    Revision 1.5  1997/10/31 17:07:15  roland
    Adapted to new RuleEngine.

    Revision 1.4  1997/10/10 11:03:40  roland
    Forms description for Graphdump.

    Revision 1.3  1997/10/07 16:59:48  roland
    Graphical display od index trees implemented.

    Revision 1.2  1997/09/24 13:24:12  roland
    Dump-procedure written and flag and procedure for index tree and page
    usage dump added.

    Revision 1.1  1997/09/22 18:43:25  roland
    Simple tool to view the contents of a typed graph. Currently only
    statistical output is implemented.

*)
(***************************************************************************)

IMPORT ParseParams, IO, Fmt, Text, Process, Stdio, Pathname, IntSeq;
IMPORT Scheme, TypedGraphSystem, Access, ErrorSupport, PageFile,
       TypedGraphPool, TypedGraph, Node, NodeSet, ASCII;
IMPORT GrasParams;
IMPORT ITPFile, SystemPage, ITCFile, IndexTreeOrganization, IndexPage;
IMPORT EntityStoragePage, AttributeStoragePage, NameStoragePage,
       RelationStoragePage;
IMPORT GraphVBT, Trestle, TrestleComm, R2, PaintOp, Word, RefList, Thread,
       FormsVBT, VBT;
IMPORT GDForms, Bundle;
IMPORT IntegerAttributeValue, RealAttributeValue, TextAttributeValue,
       CharAttributeValue, BooleanAttributeValue, CardinalAttributeValue,
       AttributeValue;

VAR
  rootPath, graphName : TEXT;
  poolname            : TEXT;
  nameserver, serverid: TEXT;
  cachesize           : CARDINAL;
  graph               : TypedGraph.T;
  pool                : TypedGraphPool.T;
  statistics          : BOOLEAN          := FALSE;
  pages               : BOOLEAN          := FALSE;
  graphical           : BOOLEAN          := FALSE;
  together            : BOOLEAN          := FALSE;
  local               : BOOLEAN          := FALSE;
  file                : ITPFile.T;

PROCEDURE ReadParams () =
  CONST
    USAGE = "<pool> <graph> [-local] [-s| -p [-g [all]] ] [-root <rootPath>]";

  PROCEDURE ParseError (prog, err: TEXT) =
    BEGIN
      IO.Put(err & "\n");
      IO.Put(prog & USAGE & "\n");
      Process.Exit(1);
    END ParseError;
  VAR valid: BOOLEAN;
  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      TRY
        poolname := pp.getNext();
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Pool name is mandatory first parameter");
      END;
      IF Text.Equal("-root", poolname) THEN
        ParseError(Pathname.Last(pp.arg[0]),
                   "Pool name is mandatory first parameter");
      END;

      TRY
        graphName := pp.getNext();
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Graph name is mandatory second parameter");
      END;
      IF Text.Equal("-root", graphName) THEN
        ParseError(Pathname.Last(pp.arg[0]),
                   "Graph name is mandatory second parameter");
      END;

      IF pp.keywordPresent("-local") THEN local := TRUE END;
      IF pp.keywordPresent("-s") THEN statistics := TRUE END;
      IF pp.keywordPresent("-p") THEN pages := TRUE END;
      IF pp.keywordPresent("-g") THEN
        graphical := TRUE;
        IF pp.testNext("all") THEN together := TRUE; END;
      END;

      GrasParams.ParseComandLine(
        rootPath, valid, cachesize, serverid, nameserver);
      IF NOT valid THEN
        ParseError(Pathname.Last(pp.arg[0]), "Need root path.");
      END;
    END;
  END ReadParams;

PROCEDURE ErrorAbort () =
  BEGIN
    TRY pool.abortTransaction(); EXCEPT ELSE END;
  END ErrorAbort;


PROCEDURE DumpForStatistics (graph: TypedGraph.T)
  RAISES {TypedGraph.InternalError, Access.Locked, Scheme.InternalError,
          Scheme.NotDeclared} =
  VAR
    found, afound                       : BOOLEAN;
    attr, type, etype, class            : Scheme.ID;
    attrLength, incEdgeCard, outEdgeCard: REF ARRAY OF REAL;
    attrs, outETypes, inETypes          : REF ARRAY OF Scheme.ID;
    types                               : REF ARRAY OF Node.T;
    c                                   : CARDINAL;
    nodes                               : NodeSet.T;
    aname                               : TEXT;
    node                                : Node.T;
    scheme                              : Scheme.T;
  <* FATAL TypedGraph.LogError, TypedGraph.WrongType,
     TypedGraph.NodeNotFound, TypedGraph.Unknown,
     TypedGraph.NotOwner, TypedGraph.CyclicEvaluation *>
  BEGIN
    scheme := graph.getScheme();
    (* Get all types *)
    WITH nodeTypes = scheme.getAllNodeTypes() DO
      types := NEW(REF ARRAY OF Node.T, nodeTypes.card());

      nodeTypes.loop();
      type := nodeTypes.get(found);
      c := 0;
      WHILE found DO
        types[c] := type;
        type := nodeTypes.get(found);
        INC(c);
      END;
      nodeTypes.dispose();
    END;

    FOR t := 0 TO LAST(types^) DO
      (* how many attributes has this type ? *)
      WITH attributes = scheme.getAllAttributesOfNodeClassOrType(types[t]) DO
        attrLength := NEW(REF ARRAY OF REAL, attributes.card());
        attrs := NEW(REF ARRAY OF Scheme.ID, attributes.card());

        attributes.loop();
        attr := attributes.get(afound);
        c := 0;
        WHILE afound DO
          attrLength[c] := 0.0;
          attrs[c] := attr;
          attr := attributes.get(afound);
          INC(c);
        END;
        attributes.dispose();
      END;

      (* how many incoming edge types has this type ? *)
      WITH incoming = scheme.getIncomingEdgeTypes(types[t]) DO
        incEdgeCard := NEW(REF ARRAY OF REAL, incoming.card());
        inETypes := NEW(REF ARRAY OF Scheme.ID, incoming.card());

        incoming.loop();
        etype := incoming.get(afound);
        c := 0;
        WHILE afound DO
          incEdgeCard[c] := 0.0;
          inETypes[c] := etype;
          etype := incoming.get(afound);
          INC(c);
        END;
        incoming.dispose();
      END;

      (* how many outgoing edge types has this type ? *)
      WITH outgoing = scheme.getOutgoingEdgeTypes(types[t]) DO
        outEdgeCard := NEW(REF ARRAY OF REAL, outgoing.card());
        outETypes := NEW(REF ARRAY OF Scheme.ID, outgoing.card());

        outgoing.loop();
        etype := outgoing.get(afound);
        c := 0;
        WHILE afound DO
          outEdgeCard[c] := 0.0;
          outETypes[c] := etype;
          etype := outgoing.get(afound);
          INC(c);
        END;
        outgoing.dispose();
      END;

      (* get all nodes of this type *)
      nodes := graph.getAllNodesOfType(types[t]);
      (* print type name and number of nodes *)
      IO.Put("type \"" & scheme.getNodeTypeName(types[t]) & "\": "
               & Fmt.Int(nodes.card()) & "\n");

      nodes.loop();
      node := nodes.get(found);
      WHILE found DO
        (* read all attributes and sum up length *)
        FOR a := 0 TO LAST(attrs^) DO
          attrLength[a] :=
            attrLength[a]
              + FLOAT(Text.Length(graph.getAttribute(
                                    node, attrs[a], 0, LAST(CARDINAL))));
        END;
        (* collect all incoming edges by type *)
        FOR e := 0 TO LAST(inETypes^) DO
          WITH ins = graph.getSources(node, inETypes[e]) DO
            incEdgeCard[e] := incEdgeCard[e] + FLOAT(ins.card());
            ins.dispose();
          END;
        END;
        (* collect all outgoing edges by type *)
        FOR e := 0 TO LAST(outETypes^) DO
          WITH outs = graph.getTargets(node, outETypes[e]) DO
            outEdgeCard[e] := outEdgeCard[e] + FLOAT(outs.card());
            outs.dispose();
          END;
        END;
        node := nodes.get(found);
      END;

      IF nodes.card() > 0 THEN
        IO.Put(" attributes\n");
        FOR a := 0 TO LAST(attrs^) DO
          scheme.getAttributeNameAndClass(attrs[a], class, aname);
          IO.Put("  \"" & aname & "\": "
                   & Fmt.Real(attrLength[a] / FLOAT(nodes.card())) & "( "
                   & Fmt.Real(attrLength[a]) & ")\n");
        END;
        IO.Put(" outgoing\n");
        FOR e := 0 TO LAST(outETypes^) DO
          IO.Put("  \"" & scheme.getEdgeTypeName(outETypes[e]) & "\": "
                   & Fmt.Real(outEdgeCard[e] / FLOAT(nodes.card())) & "( "
                   & Fmt.Real(outEdgeCard[e]) & ")\n");
        END;
        IO.Put(" incoming\n");
        FOR e := 0 TO LAST(inETypes^) DO
          IO.Put("  \"" & scheme.getEdgeTypeName(inETypes[e]) & "\": "
                   & Fmt.Real(incEdgeCard[e] / FLOAT(nodes.card())) & "( "
                   & Fmt.Real(incEdgeCard[e]) & ")\n");
        END;
      END;

      nodes.dispose();
    END;
  END DumpForStatistics;

VAR
  MonitorFile  : ITPFile.T;
  MonitorPages : ARRAY [1 .. 4] OF IntSeq.T;
  MonitorUsages: ARRAY [1 .. 4] OF CARDINAL;

PROCEDURE Monitor (tree            : CARDINAL;
                   depth1, depth2  : CARDINAL;
                   relKey1, relKey2: CARDINAL;
                   node            : BOOLEAN;
                   keyOrPage       : CARDINAL  ) =
  VAR maxrec, usage: CARDINAL;
  BEGIN
    IO.Put("tree = " & Fmt.Int(tree) & ", d1 = " & Fmt.Int(depth1)
             & ", d2 = " & Fmt.Int(depth2) & ", rk1 = "
             & Fmt.Pad(Fmt.Int(relKey1, 2), depth1, '0') & ", rk2 = "
             & Fmt.Pad(Fmt.Int(relKey2, 2), depth2, '0') & ":");
    IF node THEN
      IO.Put(" splits along key " & Fmt.Int(keyOrPage) & "\n");
    ELSE
      IO.Put(" points to data page no " & Fmt.Int(keyOrPage) & "\n");
      TRY
        WITH page = ITCFile.T.getPage(MonitorFile, keyOrPage) DO
          CASE tree OF
            1 =>
              maxrec := EntityStoragePage.MaxNoOfRecords(page);
              usage := EntityStoragePage.Usage(page);
          | 2 =>
              maxrec := NameStoragePage.MaxNoOfRecords(page);
              usage := NameStoragePage.Usage(page);
          | 3 =>
              maxrec := RelationStoragePage.MaxNoOfRecords(page);
              usage := RelationStoragePage.Usage(page);
          | 4 =>
              maxrec := AttributeStoragePage.MaxNoOfRecords(page);
              usage := AttributeStoragePage.Usage(page);
          ELSE
            IO.Put("  Unknown tree ??");
            maxrec := 0;
            usage := 0;
          END;
        END;
        IO.Put("  page usage = " & Fmt.Int(usage) & "% (max = "
                 & Fmt.Int(maxrec) & ")\n");
        MonitorPages[tree].addhi(keyOrPage);
        INC(MonitorUsages[tree], usage);
      EXCEPT
      | EntityStoragePage.InternalError => IO.Put("  internal error\n");
      | NameStoragePage.InternalError => IO.Put("  internal error\n");
      | RelationStoragePage.InternalError => IO.Put("  internal error\n");
      | AttributeStoragePage.InternalError => IO.Put("  internal error\n");
      | Access.Locked => IO.Put("  access locked\n");
      END;
    END
  END Monitor;

TYPE
  Vertex = GraphVBT.Vertex OBJECT
             relKey1, relKey2: CARDINAL;
             depth1, depth2  : CARDINAL;
             splits          : BOOLEAN;
             keyOrPage       : CARDINAL;
             level           : CARDINAL;
           END;

PROCEDURE PaintNode (    vbt             : GraphVBT.T;
                     VAR root            : Vertex;
                         tree            : CARDINAL;
                         relKey1, relKey2: CARDINAL;
                         depth1, depth2  : CARDINAL;
                         splits          : BOOLEAN;
                         keyOrPage       : CARDINAL    ) =
  VAR
    (* max no of displayed levels in the tree *)
    lev       := Lev(PagesInTree[tree]);
    Yoff      := 1.0 / FLOAT(MAX(5, lev));
    Ysize     := Yoff / 5.0;
    Xsize     := MIN(Ysize, 1.0 / FLOAT(Word.LeftShift(1, lev - 1)));
    EdgeWidth := Xsize / 10.0;
    parent, node: Vertex;
    edge        : GraphVBT.Edge;
    leftSon     : BOOLEAN;

  PROCEDURE Lev (x: CARDINAL): CARDINAL =
    VAR
      res : CARDINAL := 0;
      null: BOOLEAN  := TRUE;
    BEGIN
      WHILE x # 0 DO
        IF null AND (Word.And(1, x) = 1) THEN null := FALSE; END;
        x := Word.RightShift(x, 1);
        INC(res);
      END;
      IF NOT null THEN INC(res); END;
      RETURN res;
    END Lev;

  PROCEDURE SonPos (ppos: R2.T; level: CARDINAL; left: BOOLEAN): R2.T =
    VAR res: R2.T;
    BEGIN
      res[1] := ppos[1] + Yoff;
      WITH xoff = 1.0 / FLOAT(Word.LeftShift(1, level + 1)) DO
        IF left THEN
          res[0] := ppos[0] - xoff;
        ELSE
          res[0] := ppos[0] + xoff;
        END;
      END;
      RETURN res;
    END SonPos;

  PROCEDURE KeysMatch (k1, k2, depth: CARDINAL): BOOLEAN =
    BEGIN
      WITH mask = Word.Extract(-1, 0, depth) DO
        RETURN Word.And(k1, mask) = Word.And(k2, mask);
      END;
    END KeysMatch;

  PROCEDURE FindParent (    root    : Vertex;
                            rk1, rk2: CARDINAL;
                            d1, d2  : CARDINAL;
                        VAR parent  : Vertex;
                        VAR left    : BOOLEAN   ) =
    VAR
      depth1, depth2: CARDINAL := 0;
      n             : CARDINAL;
      son           : Vertex;
    BEGIN
      parent := root;
      WHILE parent # NIL AND parent.splits DO
        IF parent.keyOrPage = 1 THEN
          left := Word.Extract(rk1, depth1, 1) = 0;
          INC(depth1);
        ELSE
          left := Word.Extract(rk2, depth2, 1) = 0;
          INC(depth2);
        END;

        IF depth1 = d1 AND depth2 = d2 THEN
          (* parent found *)
          RETURN;
        ELSE
          (* find son *)
          n := 0;
          LOOP
            IF n <= RefList.Length(parent.edges) - 1 THEN
              son :=
                NARROW(RefList.Nth(parent.edges, n), GraphVBT.Edge).vertex1;
              IF KeysMatch(rk1, son.relKey1, depth1)
                   AND KeysMatch(rk2, son.relKey2, depth2) THEN
                parent := son;
                EXIT;
              END;
              (* try next son *)
              INC(n);
            ELSE
              (* no more sons *)
              parent := NIL;
              EXIT;
            END;
          END;
        END;
      END;
      IF parent # NIL AND NOT parent.splits THEN parent := NIL; END;
    END FindParent;

  VAR
    color := ARRAY [1 .. 2] OF
               PaintOp.T{PaintOp.FromRGB(1.0, 0.0, 0.0),
                         PaintOp.FromRGB(0.0, 0.0, 1.0)};
    col: PaintOp.T;
  BEGIN
    IF splits THEN col := color[keyOrPage] ELSE col := PaintOp.Fg END;
    IF root = NIL THEN
      root :=
        NEW(Vertex, graph := vbt, pos := R2.T{0.5, 0.5 * Ysize},
            shape := GraphVBT.VertexShape.Ellipse,
            size := R2.T{Xsize, Ysize}, color := col, relKey1 := relKey1,
            relKey2 := relKey2, depth1 := depth1, depth2 := depth2,
            splits := splits, keyOrPage := keyOrPage, level := 0).init();
    ELSE
      FindParent(root, relKey1, relKey2, depth1, depth2, parent, leftSon);
      IF parent = NIL THEN
        IO.Put("Cannot find parent!");
      ELSE
        WITH newpos = SonPos(parent.pos, parent.level + 1, leftSon) DO
          node :=
            NEW(Vertex, graph := vbt, pos := newpos,
                shape := GraphVBT.VertexShape.Ellipse,
                size := R2.T{Xsize, Ysize}, color := col,
                relKey1 := relKey1, relKey2 := relKey2, depth1 := depth1,
                depth2 := depth2, splits := splits, keyOrPage := keyOrPage,
                level := parent.level + 1).init();
          edge := NEW(GraphVBT.Edge, vertex0 := parent, vertex1 := node,
                      width := EdgeWidth).init();
        END;
      END;
    END;
  END PaintNode;

PROCEDURE GraphicalMonitor (tree            : CARDINAL;
                            depth1, depth2  : CARDINAL;
                            relKey1, relKey2: CARDINAL;
                            node            : BOOLEAN;
                            keyOrPage       : CARDINAL  ) =
  VAR maxrec, usage: CARDINAL;
  BEGIN
    PaintNode(trees[tree].vbt, trees[tree].root, tree, relKey1, relKey2,
              depth1, depth2, node, keyOrPage);
    (**
    IO.Put("tree = " & Fmt.Int(tree) & ", d1 = " & Fmt.Int(depth1)
             & ", d2 = " & Fmt.Int(depth2) & ", rk1 = "
             & Fmt.Pad(Fmt.Int(relKey1, 2), depth1, '0') & ", rk2 = "
             & Fmt.Pad(Fmt.Int(relKey2, 2), depth2, '0') & ":"); *)
    IF node THEN
      (* IO.Put(" splits along key " & Fmt.Int(keyOrPage) & "\n"); *)
    ELSE
      (* IO.Put(" points to data page no " & Fmt.Int(keyOrPage) & "\n"); *)
      TRY
        WITH page = ITCFile.T.getPage(MonitorFile, keyOrPage) DO
          CASE tree OF
            1 =>
              maxrec := EntityStoragePage.MaxNoOfRecords(page);
              usage := EntityStoragePage.Usage(page);
          | 2 =>
              maxrec := NameStoragePage.MaxNoOfRecords(page);
              usage := NameStoragePage.Usage(page);
          | 3 =>
              maxrec := RelationStoragePage.MaxNoOfRecords(page);
              usage := RelationStoragePage.Usage(page);
          | 4 =>
              maxrec := AttributeStoragePage.MaxNoOfRecords(page);
              usage := AttributeStoragePage.Usage(page);
          ELSE
            IO.Put("  Unknown tree ??");
            maxrec := 0;
            usage := 0;
          END;
        END;
        MonitorPages[tree].addhi(keyOrPage);
        INC(MonitorUsages[tree], usage);
      EXCEPT
      | EntityStoragePage.InternalError => IO.Put("  internal error\n");
      | NameStoragePage.InternalError => IO.Put("  internal error\n");
      | RelationStoragePage.InternalError => IO.Put("  internal error\n");
      | AttributeStoragePage.InternalError => IO.Put("  internal error\n");
      | Access.Locked => IO.Put("  access locked\n");
      END;
    END;
  END GraphicalMonitor;

TYPE
  TreeInfo = RECORD
               vbt : GraphVBT.T;
               root: Vertex;
             END;
VAR
  trees      : ARRAY [1 .. NoOfTrees] OF TreeInfo;
  display    : Thread.T;
  panel      : FormsVBT.T;
  PagesInTree: ARRAY [1 .. NoOfTrees] OF CARDINAL;

PROCEDURE TreeApply (<* UNUSED *> cl: Thread.Closure): REFANY =
  BEGIN
    TRY
      Trestle.Install(panel, graphName & ": index trees");
      Trestle.AwaitDelete(panel);
    EXCEPT
      TrestleComm.Failure => graphical := FALSE;
    END;
    RETURN NIL;
  END TreeApply;

PROCEDURE Close (<* UNUSED *> fv       : FormsVBT.T;
                 <* UNUSED *> name     : TEXT;
                 <* UNUSED *> eventData: REFANY;
                 <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    Trestle.Delete(panel);
  END Close;

PROCEDURE OpenTreeDisplays (treeno: CARDINAL := 0) =
  BEGIN
    TRY
      IF together THEN
        panel :=
          NEW(FormsVBT.T).init(Bundle.Get(GDForms.Get(), "indextrees.fv"));
      ELSE
        panel :=
          NEW(FormsVBT.T).init(Bundle.Get(GDForms.Get(), "indextree.fv"));
      END;
      FormsVBT.AttachProc(panel, "close", Close);

      IF together THEN
        FOR i := 1 TO NoOfTrees DO
          trees[i].vbt := NEW(GraphVBT.T).init();
          trees[i].root := NIL;
          WITH title = "(Shape (Height 10) (Text \"" & Treename[i]
                         & "\")) (Glue 2)" DO
            EVAL FormsVBT.Insert(panel, Treename[i], title, 1);
          END;
          FormsVBT.InsertVBT(panel, Treename[i], trees[i].vbt, 2);
        END;
      ELSE
        trees[treeno].vbt := NEW(GraphVBT.T).init();
        trees[treeno].root := NIL;
        WITH title = "(Shape (Height 10) (Text \"" & Treename[treeno]
                       & "\")) (Glue 3)" DO
          EVAL FormsVBT.Insert(panel, "tree", title, 1);
          FormsVBT.InsertVBT(panel, "tree", trees[treeno].vbt, 2);
        END;
      END;
      display := Thread.Fork(NEW(Thread.Closure, apply := TreeApply));
    EXCEPT
      FormsVBT.Error => graphical := FALSE;
    END;
  END OpenTreeDisplays;

PROCEDURE AwaitTrestleDeletes () =
  BEGIN
    EVAL Thread.Join(display);
  END AwaitTrestleDeletes;


CONST
  NoOfTrees = 4;
  Treename = ARRAY [1 .. 4] OF
               TEXT{"entities", "names", "relations", "attributes"};

PROCEDURE DumpPages (file: ITPFile.T)
  RAISES {Access.Locked, SystemPage.InternalError, IndexPage.InternalError,
          ITPFile.InternalError, ITPFile.TreeUnknown} =
  VAR
    idxpages, total: CARDINAL;
    page           : IndexPage.T;
  BEGIN
    (* Initialize global data for preorder runs *)
    MonitorFile := file;
    FOR i := 1 TO NoOfTrees DO
      MonitorPages[i] := NEW(IntSeq.T).init();
      MonitorUsages[i] := 0;
    END;
    IF graphical AND together THEN OpenTreeDisplays() END;

    WITH syspage = ITCFile.T.getPage(
                     file, IndexTreeOrganization.SystemPageNo) DO
      total := SystemPage.NoOfPhysExistentPages(syspage);
      IO.Put("Pages total : " & Fmt.Int(total) & "\n");
      idxpages := SystemPage.NoOfIndexPages(syspage);
      IO.Put("Index pages : " & Fmt.Int(idxpages) & "\n");
      IO.Put("Unused pages: "
               & Fmt.Int(SystemPage.NoOfUnusedPages(syspage)) & "\n");

      IO.Put(
        "System page (index page 0): "
          & Fmt.Int(IndexPage.Size - IndexPage.NoOfFreeEntries(syspage))
          & "/" & Fmt.Int(IndexPage.Size) & " entries\n");
      FOR i := 2 TO idxpages DO
        page := ITCFile.T.getPage(file, i);
        IO.Put(
          "Index page no " & Fmt.Int(i - 1) & ": "
            & Fmt.Int(IndexPage.Size - IndexPage.NoOfFreeEntries(page))
            & "/" & Fmt.Int(IndexPage.Size) & "\n");
      END;

      FOR i := 1 TO NoOfTrees DO
        IO.Put("Tree " & Fmt.Int(i) & " (" & Treename[i] & ")");
        PagesInTree[i] := SystemPage.NoOfDataPagesInTree(syspage, i);
        IO.Put(" pages: " & Fmt.Int(PagesInTree[i]) & "\n");
        IO.Put("Preorder run\n");
        IF graphical THEN
          IF NOT together THEN OpenTreeDisplays(i) END;
          file.preorderRun(i, GraphicalMonitor);
          trees[i].vbt.redisplay();
          IF NOT together THEN EVAL Thread.Join(display) END;
        ELSE
          file.preorderRun(i, Monitor);
        END;
        IO.Put("Pages visited:");
        FOR j := 0 TO MonitorPages[i].size() - 1 DO
          IO.Put(" " & Fmt.Int(MonitorPages[i].get(j)));
        END;
        IO.Put(
          "\nAverage usage: "
            & Fmt.Int(MonitorUsages[i] DIV MonitorPages[i].size()) & "%\n");
      END;
    END;
    IF graphical THEN AwaitTrestleDeletes() END;
  END DumpPages;

PROCEDURE Printable (t: TEXT): TEXT =
  VAR
    ok       : BOOLEAN  := TRUE;
    act, prev: CARDINAL := 0;
    len      : CARDINAL := Text.Length(t);
    res      : TEXT     := "";
  BEGIN
    WHILE ok AND act < len DO
      ok := Text.GetChar(t, act) IN ASCII.Graphics;
      INC(act);
    END;
    IF NOT ok THEN
      act := 0;
      WHILE act < len DO
        prev := act;
        WHILE act < len AND Text.GetChar(t, act) IN ASCII.Graphics DO
          INC(act)
        END;
        IF act # prev THEN res := res & Text.Sub(t, prev, act - prev); END;
        WHILE act < len AND NOT Text.GetChar(t, act) IN ASCII.Graphics DO
          res := res & "\\" & Fmt.Pad(Fmt.Int(ORD(Text.GetChar(t, act)),
                                              base := 8), 3, '0');
          INC(act);
        END;
      END;
      RETURN res;
    ELSE
      RETURN t;
    END;
  END Printable;

PROCEDURE FmtNode (n: Node.T): TEXT =
  BEGIN
    RETURN "(" & Fmt.Int(n.graph) & "," & Fmt.Int(n.entity) & ")";
  END FmtNode;

PROCEDURE FmtTypedAttribute (graph  : TypedGraph.T;
                             node   : Node.T;
                             attr   : Scheme.ID;
                             valType: CARDINAL      ): TEXT
  RAISES {TypedGraph.InternalError, Access.Locked, TypedGraph.WrongType} =
  VAR val: AttributeValue.T;
  <* FATAL TypedGraph.LogError, TypedGraph.CardinalityError,
     TypedGraph.NodeNotFound, TypedGraph.Unknown,
     TypedGraph.NotOwner, TypedGraph.CyclicEvaluation *>
  BEGIN
    CASE valType OF
      AttributeValue.BooleanTypeCode => val := NEW(BooleanAttributeValue.T);
    | AttributeValue.IntegerTypeCode => val := NEW(IntegerAttributeValue.T);
    | AttributeValue.CardinalTypeCode => val := NEW(CardinalAttributeValue.T);
    | AttributeValue.RealTypeCode => val := NEW(RealAttributeValue.T);
    | AttributeValue.CharTypeCode => val := NEW(CharAttributeValue.T);
    | AttributeValue.TextTypeCode => val := NEW(TextAttributeValue.T);
    ELSE
      (* No basic type *)
      RETURN
        Printable(graph.getAttribute(node, attr, 0, LAST(CARDINAL)));
    END;
    TRY
      graph.getTypedAttribute(node, attr, val);
    EXCEPT
      TypedGraph.ValueUndefined, TypedGraph.WrongType =>
        RETURN
          Printable(graph.getAttribute(node, attr, 0, LAST(CARDINAL)));
    END;
    CASE valType OF
      AttributeValue.BooleanTypeCode =>
        RETURN Fmt.Bool(NARROW(val, BooleanAttributeValue.T).get());
    | AttributeValue.IntegerTypeCode =>
        RETURN Fmt.Int(NARROW(val, IntegerAttributeValue.T).get());
    | AttributeValue.CardinalTypeCode =>
        RETURN Fmt.Int(NARROW(val, CardinalAttributeValue.T).get());
    | AttributeValue.RealTypeCode =>
        RETURN Fmt.Real(NARROW(val, RealAttributeValue.T).get(), literal := TRUE);
    | AttributeValue.CharTypeCode =>
        RETURN "'" & Printable(Text.FromChar(
                                 NARROW(val, CharAttributeValue.T).get()))
                 & "\'"
    | AttributeValue.TextTypeCode =>
        RETURN
          "\"" & Printable(NARROW(val, TextAttributeValue.T).get()) & "\"";
    ELSE
      <* ASSERT FALSE *>
    END;
  END FmtTypedAttribute;

PROCEDURE Dump (graph: TypedGraph.T)
  RAISES {TypedGraph.InternalError, Access.Locked, Scheme.InternalError,
          Scheme.NotDeclared} =
  VAR
    found, afound             : BOOLEAN;
    attr, type, class, etype  : Scheme.ID;
    attrs, outETypes, inETypes: REF ARRAY OF Scheme.ID;
    types                     : REF ARRAY OF Node.T;
    attrType                  : REF ARRAY OF CARDINAL;
    c                         : CARDINAL;
    nodes                     : NodeSet.T;
    node, target              : Node.T;
    scheme                    : Scheme.T;
    aname                     : TEXT;
    kind                      : Scheme.AttributeKind;
    props                     : Scheme.IndexProperties;
    attCard                   : Scheme.Cardinality;
    constLength               : BOOLEAN;
    length                    : CARDINAL;
  <* FATAL TypedGraph.WrongType,
     TypedGraph.NodeNotFound,
     TypedGraph.NotOwner *>
  BEGIN
    scheme := graph.getScheme();
    (* Get all types *)
    WITH nodeTypes = scheme.getAllNodeTypes() DO
      types := NEW(REF ARRAY OF Node.T, nodeTypes.card());

      nodeTypes.loop();
      type := nodeTypes.get(found);
      c := 0;
      WHILE found DO
        types[c] := type;
        type := nodeTypes.get(found);
        INC(c);
      END;
      nodeTypes.dispose();
    END;

    FOR t := 0 TO LAST(types^) DO
      (* how many attributes has this type ? *)
      WITH attributes = scheme.getAllAttributesOfNodeClassOrType(types[t]) DO
        attrs := NEW(REF ARRAY OF Scheme.ID, attributes.card());
        attrType := NEW(REF ARRAY OF CARDINAL, attributes.card());

        attributes.loop();
        attr := attributes.get(afound);
        c := 0;
        WHILE afound DO
          attrs[c] := attr;
          scheme.showAttributeProps(
            attr, kind, props, attrType^[c], attCard, constLength, length);
          attr := attributes.get(afound);
          INC(c);
        END;
        attributes.dispose();
      END;

      (* get all nodes of this type *)
      nodes := graph.getAllNodesOfType(types[t]);

      nodes.loop();
      node := nodes.get(found);
      WHILE found DO
        IO.Put("n " & FmtNode(node) & ": "
                 & scheme.getNodeTypeName(types[t]) & "\n");
        (* print all attributes *)
        FOR a := 0 TO LAST(attrs^) DO
          scheme.getAttributeNameAndClass(attrs^[a], class, aname);
          IO.Put("  " & aname & " = ");
          IO.Put(FmtTypedAttribute(graph, node, attrs^[a], attrType^[a])
                     & "\n");
        END;
        node := nodes.get(found);
      END;

      nodes.dispose();
    END;

    FOR t := 0 TO LAST(types^) DO
      (* how many incoming edge types has this type ? *)
      WITH incoming = scheme.getIncomingEdgeTypes(types[t]) DO
        inETypes := NEW(REF ARRAY OF Scheme.ID, incoming.card());

        incoming.loop();
        etype := incoming.get(afound);
        c := 0;
        WHILE afound DO
          inETypes[c] := etype;
          etype := incoming.get(afound);
          INC(c);
        END;
        incoming.dispose();
      END;

      (* how many outgoing edge types has this type ? *)
      WITH outgoing = scheme.getOutgoingEdgeTypes(types[t]) DO
        outETypes := NEW(REF ARRAY OF Scheme.ID, outgoing.card());

        outgoing.loop();
        etype := outgoing.get(afound);
        c := 0;
        WHILE afound DO
          outETypes[c] := etype;
          etype := outgoing.get(afound);
          INC(c);
        END;
        outgoing.dispose();
      END;

      (* get all nodes of this type *)
      nodes := graph.getAllNodesOfType(types[t]);

      nodes.loop();
      node := nodes.get(found);
      WHILE found DO
        (* collect all outgoing edges *)
        WITH outs = graph.getAllOutEdges(node) DO
          outs.loop();
          outs.get(target, etype, found);
          WHILE found DO
            IO.Put(
              "e " & FmtNode(node) & " -" & scheme.getEdgeTypeName(etype)
                & "-> " & FmtNode(target) & "\n");
            outs.get(target, etype, found);
          END;
          outs.dispose();
        END;
        node := nodes.get(found);
      END;

      nodes.dispose();
    END;
  END Dump;

<* FATAL TypedGraphPool.CardinalityError *>
BEGIN
  ReadParams();
  TRY
    TypedGraphSystem.Login(rootPath, cachesize, serverid, nameserver);
    pool := NEW(TypedGraphPool.T).open(
              poolname, Access.Mode.ReadWriteShared, new := FALSE);
    IF statistics OR NOT pages THEN
      graph := NEW(TypedGraph.T).open(
                 pool, graphName, TypedGraph.AccessMode.ReadOnlyShared,
                 new := FALSE, errorChecks := TypedGraph.ErrorCheckSet{},
                 local := local);
      pool.beginTransaction();
      IF statistics THEN DumpForStatistics(graph); ELSE Dump(graph); END;
      pool.commitTransaction();
      graph.close(keepLog := FALSE);
    ELSE
      file :=
        NEW(ITPFile.T).open(pool, graphName, Access.Mode.ReadOnlyShared,
                            Access.Kind.Data, FALSE, FALSE, NoOfTrees);
      pool.beginTransaction();
      DumpPages(file);
      pool.commitTransaction();
      file.close();
    END;
    pool.close();
  EXCEPT
  | TypedGraphPool.InternalError (info) =>
      IO.Put(ErrorSupport.ToText(info));
  | Scheme.InternalError (info) =>
      ErrorAbort();
      IO.Put(ErrorSupport.ToText(info));
  | TypedGraphPool.NotInTransaction =>
      ErrorAbort();
      IO.Put("Not in transaction!\n");
  | Scheme.NotDeclared => ErrorAbort(); IO.Put("Not declared!\n");
  | Access.Denied (msg) => IO.Put("Access denied: " & msg & "\n");
  | Access.Locked => IO.Put("Access locked!\n");
  | PageFile.NoAccess (msg) => IO.Put("No access: " & msg & "\n");
  | TypedGraph.InUse => IO.Put("Graph in use!\n");
  | TypedGraph.NoScheme => IO.Put("Graph has no scheme!\n");
  | TypedGraph.NotExistent => IO.Put("Graph not exists!\n");
  | TypedGraph.InternalError (info) => IO.Put(ErrorSupport.ToText(info));
  | SystemPage.InternalError (info) => IO.Put(ErrorSupport.ToText(info));
  | ITPFile.InternalError (info) => IO.Put(ErrorSupport.ToText(info));
  | IndexPage.InternalError (info) => IO.Put(ErrorSupport.ToText(info));
  | ITPFile.TreeUnknown => IO.Put("Not a GRAS graph!\n");
  END
END GraphDump.
