MODULE DaVinciGraph;

(***************************************************************************)
(** Created by:  Markus Kluck						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1998/09/03 11:07:32  kluck
    Further modules used by PoolView to implement selection.

*)
(***************************************************************************)

IMPORT VirtualResource, Access, Names;
IMPORT IO, Fmt, TextCursorSet, ErrorSupport;
IMPORT DaVinci, DaVinciMsg, Panel, Globals;


PROCEDURE IconSelection (<* UNUSED *> handler: DaVinci.EventHandler;
                         <* UNUSED *> type   : DaVinci.MsgType;
                         <* UNUSED *> msg    : TEXT                  ) =
  BEGIN
    IF NOT Panel.Install() THEN
      IO.Put(">>> Unable to open SelectionPanel !\n");
    END;
  END IconSelection;


PROCEDURE MsgPrinter (<* UNUSED *> handler: DaVinci.EventHandler;
                      <* UNUSED *> type   : DaVinci.MsgType;
                                   msg    : TEXT                  ) =
  BEGIN
    IO.Put(msg & "\n");
  END MsgPrinter;


PROCEDURE IdToText (id: CARDINAL): TEXT =
  (* Converts the unique id into a TEXT object *)
  BEGIN
    RETURN "(" & Fmt.Int(id) & ")";
  END IdToText;


PROCEDURE RelIdToText (source: CARDINAL; rel: CARDINAL; target: CARDINAL):
  TEXT =
  (* Constructs a unique id-string of the form "(n,n,n)" *)
  BEGIN
    RETURN "(" & Fmt.Int(source) & "," & Fmt.Int(rel) & ","
             & Fmt.Int(target) & ")";
  END RelIdToText;


PROCEDURE View (res   : VirtualResource.T;
                names : Names.T;
                viewer: DaVinci.T;
                local : BOOLEAN             := FALSE;
                debug : BOOLEAN             := FALSE ) 
  RAISES {DaVinci.Error} =
  VAR
    msg                 : DaVinciMsg.T;
    target, relation    : TEXT;
    id, relId           : CARDINAL;
    firstNode, firstEdge: BOOLEAN;
    ok, relok, targetok : BOOLEAN;
    name, coll          : TEXT;
    visibleNames                       := TextCursorSet.New();
    invisibleNames                     := TextCursorSet.New();
    relationTargets                    := TextCursorSet.New();
  BEGIN
    msg := NEW(DaVinciMsg.T).init();
    msg.beginNewGraph();

    TRY
      (*--- create visibleNames set *)
      res.beginTransaction();
      Globals.visibleCollections.loop();
      coll := Globals.visibleCollections.get(ok);
      WHILE ok DO
        visibleNames.union(
          names.getAllEntries(
            local, collection := names.declareCollection(coll)));
        coll := Globals.visibleCollections.get(ok);
      END;
      res.commitTransaction();

      (*--- create invisibleNames set *)
      res.beginTransaction();
      Globals.invisibleCollections.loop();
      coll := Globals.invisibleCollections.get(ok);
      WHILE ok DO
        invisibleNames.union(
          names.getAllEntries(
            local, collection := names.declareCollection(coll)));
        coll := Globals.invisibleCollections.get(ok);
      END;
      res.commitTransaction();

      IF Globals.debug THEN
        Globals.PrintTextCursorSet(
          "Content of visibleNames:\n", visibleNames);
        Globals.PrintTextCursorSet(
          "Content of invisibleNames:\n", invisibleNames);
      END;

      invisibleNames.difference(visibleNames);
      visibleNames.difference(invisibleNames);

      IF debug THEN
        Globals.PrintTextCursorSet("Displayed entries:\n", visibleNames);
      END;

      (*--- Running through the set of visible names *)
      firstNode := TRUE;
      visibleNames.loop();
      name := visibleNames.get(ok);
      WHILE ok DO
        (*--- first insert the entry into DaVinci-graph: *)
        res.beginTransaction();
        id := names.id(name, local);
        res.commitTransaction();

        (*--- begin node declaration *)
        msg.declareNode(IdToText(id), label := name, first := firstNode);

        (*--- Running through the relation-set of each entry: *)
        res.beginTransaction();
        Globals.visibleRelations.loop();
        relation := Globals.visibleRelations.get(relok);
        firstEdge := TRUE;
        WHILE relok DO
          relId := names.declareRelation(relation);
          (*--- A set with the targets of the current relation going out
             from the selected name is read. *)
          relationTargets :=
            names.targets(name, local, names.declareRelation(relation));

          (*--- for each outgoing relation we have to draw an edge to its
             target: *)
          res.beginTransaction();
          relationTargets.loop();
          target := relationTargets.get(targetok);
          WHILE targetok DO
            IF visibleNames.in(target) THEN
              msg.declareEdge(
                RelIdToText(id, relId, names.id(target, local)),
                IdToText(names.id(target, local)), label := relation,
                first := firstEdge);
              firstEdge := FALSE;
            END;                 (* IF visibleNames.in ... *)
            target := relationTargets.get(targetok);
          END;                   (* WHILE targetok *)
          res.commitTransaction();
          relation := Globals.visibleRelations.get(relok);
        END;                     (* WHILE relok... *)

        res.commitTransaction();
        msg.endDeclareNode();
        firstNode := FALSE;
        name := visibleNames.get(ok);
      END;                       (* WHILE ok ... *)
    EXCEPT
    | VirtualResource.FatalError =>
        Globals.ErrorAbort(Globals.virtualResource);
        IO.Put("View: VirtualResource.FatalError!\n");
    | VirtualResource.NotInTransaction =>
        Globals.ErrorAbort(Globals.virtualResource);
        IO.Put("View: VirtualResource.NotInTransaction!\n");
    | Access.Locked => IO.Put("View: Access.Locked!\n");
    | Names.Unknown => IO.Put("View: Names.Unknown!\n");
    | Names.InternalError (info) =>
        Globals.ErrorAbort(Globals.virtualResource);
        IO.Put(ErrorSupport.ToText(info));
    | Names.Undeclared =>
        Globals.ErrorAbort(Globals.virtualResource);
        IO.Put("View: Names.Undeclared!\n");
    END;

    msg.endNewGraph();
    WITH t = msg.toText() DO
      IF debug THEN IO.Put(t); END;
      viewer.send(t);
    END;

    (*--- dispose sets *)
    visibleNames.dispose();
    invisibleNames.dispose();
    relationTargets.dispose();
  END View;


BEGIN
END DaVinciGraph.
