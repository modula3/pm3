MODULE Panel;

(***************************************************************************)
(** Created by:  Markus Kluck						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1998/09/03 11:07:35  kluck
    Further modules used by PoolView to implement selection.

*)
(***************************************************************************)

IMPORT FormsVBT, VBT, Thread, Rd, Rsrc, Trestle, TrestleComm,
       ResourceBundle, IO, Stdio;
IMPORT TextCursorSet, Lex, TextRd;
IMPORT DaVinci, Globals, DaVinciGraph;


VAR
  panel               : FormsVBT.T;
  visibleCollections               := TextCursorSet.New();
  invisibleCollections             := TextCursorSet.New();
  visibleRelations                 := TextCursorSet.New();
  invisibleRelations               := TextCursorSet.New();


(*---------------------------------------------------------*)
(*--- Procedures and message handlers for Control Panel ---*)
(*---------------------------------------------------------*)

PROCEDURE Install (): BOOLEAN =
  <* FATAL FormsVBT.Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted *>
  VAR path: Rsrc.Path := Rsrc.BuildPath(".", ResourceBundle.Get());
  BEGIN
    TRY
      panel := NEW(FormsVBT.T).initFromRsrc("SelectionPanel.fv", path);
      FormsVBT.AttachProc(panel, "QuitPanel", QuitPanel);
      FormsVBT.AttachProc(panel, "RefreshGraph", RefreshGraph);
      FormsVBT.AttachProc(
        panel, "DisplayAllCollections", DisplayAllCollections);
      FormsVBT.AttachProc(panel, "CollectionSel", CollectionSel);
      FormsVBT.AttachProc(panel, "HideCollection", HideCollection);
      FormsVBT.AttachProc(panel, "HideAllCollections", HideAllCollections);
      FormsVBT.AttachProc(
        panel, "DisplayAllRelations", DisplayAllRelations);
      FormsVBT.AttachProc(panel, "RelationSel", RelationSel);
      FormsVBT.AttachProc(panel, "HideRelation", HideRelation);
      FormsVBT.AttachProc(panel, "HideAllRelations", HideAllRelations);
      Trestle.Install(panel, "PoolView - Selection Panel");
    EXCEPT
      TrestleComm.Failure => IO.Put("Cannot open panel.\n"); RETURN FALSE;
    END;

    (*--- copy collections for temporary use *)
    visibleCollections := Globals.visibleCollections.copy();
    invisibleCollections := Globals.invisibleCollections.copy();

    (*--- copy relations for temporary use *)
    visibleRelations := Globals.visibleRelations.copy();
    invisibleRelations := Globals.invisibleRelations.copy();

    FillBrowsers(panel);

    Trestle.AwaitDelete(panel);
    RETURN TRUE;
  END Install;


(*----------------------------------*)
(*--- Procedure for data display ---*)
(*----------------------------------*)

PROCEDURE FillDisplayedCollectionsBrowser (form: FormsVBT.T) =
  VAR
    coll      : TEXT    := "";
    collString: TEXT    := "";
    ok        : BOOLEAN;
  BEGIN
    TRY
      visibleCollections.loop();
      coll := visibleCollections.get(ok);
      WHILE ok DO
        collString := collString & coll & "\n";
        coll := visibleCollections.get(ok);
      END;
      FormsVBT.PutTextProperty(
        form, "DisplayedCollections", "Items", collString);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    END;
  END FillDisplayedCollectionsBrowser;


PROCEDURE FillHiddenCollectionsBrowser (panel: FormsVBT.T) =
  VAR
    coll      : TEXT    := "";
    collString: TEXT    := "";
    ok        : BOOLEAN;
  BEGIN
    TRY
      invisibleCollections.loop();
      coll := invisibleCollections.get(ok);
      WHILE ok DO
        collString := collString & coll & "\n";
        coll := invisibleCollections.get(ok);
      END;
      FormsVBT.PutTextProperty(
        panel, "HiddenCollections", "Items", collString);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    END;
  END FillHiddenCollectionsBrowser;


PROCEDURE FillDisplayedRelationsBrowser (panel: FormsVBT.T) =
  VAR
    rel      : TEXT    := "";
    relString: TEXT    := "";
    ok       : BOOLEAN;
  BEGIN
    TRY
      visibleRelations.loop();
      rel := visibleRelations.get(ok);
      WHILE ok DO
        relString := relString & rel & "\n";
        rel := visibleRelations.get(ok);
      END;
      FormsVBT.PutTextProperty(
        panel, "DisplayedRelations", "Items", relString);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    END;
  END FillDisplayedRelationsBrowser;


PROCEDURE FillHiddenRelationsBrowser (panel: FormsVBT.T) =
  VAR
    rel      : TEXT    := "";
    relString: TEXT    := "";
    ok       : BOOLEAN;
  BEGIN
    TRY
      invisibleRelations.loop();
      rel := invisibleRelations.get(ok);
      WHILE ok DO
        relString := relString & rel & "\n";
        rel := invisibleRelations.get(ok);
      END;
      FormsVBT.PutTextProperty(
        panel, "HiddenRelations", "Items", relString);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    END;
  END FillHiddenRelationsBrowser;


PROCEDURE FillBrowsers (panel: FormsVBT.T) =
  BEGIN
    FillDisplayedCollectionsBrowser(panel);
    FillHiddenCollectionsBrowser(panel);
    FillDisplayedRelationsBrowser(panel);
    FillHiddenRelationsBrowser(panel);
  END FillBrowsers;


(*------------------------*)
(*--- Message handlers ---*)
(*------------------------*)

PROCEDURE RefreshGraph (<* UNUSED *> fv       : FormsVBT.T;
                        <* UNUSED *> name     : TEXT;
                        <* UNUSED *> eventData: REFANY;
                        <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    Globals.visibleCollections.dispose();
    Globals.invisibleCollections.dispose();
    Globals.visibleRelations.dispose();
    Globals.invisibleRelations.dispose();

    Globals.visibleCollections := visibleCollections.copy();
    Globals.invisibleCollections := invisibleCollections.copy();
    Globals.visibleRelations := visibleRelations.copy();
    Globals.invisibleRelations := invisibleRelations.copy();

    TRY
      DaVinciGraph.View(Globals.virtualResource, Globals.names,
                        Globals.viewer, Globals.local);
    EXCEPT
    | DaVinci.Error (msg) => IO.Put(msg & "\n");
    END;
  END RefreshGraph;


PROCEDURE QuitPanel (<* UNUSED *> fv       : FormsVBT.T;
                     <* UNUSED *> name     : TEXT;
                     <* UNUSED *> eventData: REFANY;
                     <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    Trestle.Delete(panel);
  END QuitPanel;


PROCEDURE DisplayAllCollections (             fv       : FormsVBT.T;
                                 <* UNUSED *> name     : TEXT;
                                 <* UNUSED *> eventData: REFANY;
                                 <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    (*--- mixing the sets *)
    visibleCollections.union(Globals.declaredCollectionsText);
    invisibleCollections.dispose();
    invisibleCollections := TextCursorSet.New();

    (*--- display new sets *)
    FillDisplayedCollectionsBrowser(fv);
    FillHiddenCollectionsBrowser(fv);
  END DisplayAllCollections;


PROCEDURE CollectionSel (             fv       : FormsVBT.T;
                         <* UNUSED *> name     : TEXT;
                         <* UNUSED *> eventData: REFANY;
                         <* UNUSED *> time     : VBT.TimeStamp) =
  VAR
    coll: TEXT;
    set : TextCursorSet.T;
  BEGIN
    TRY
      (*--- get selected element and change sets *)
      coll := FormsVBT.GetTextProperty(fv, "HiddenCollections", "Select");
      IF (coll # NIL) THEN
        set := ParseSelection(coll);
        visibleCollections.union(set);
        invisibleCollections.difference(set);
        set.dispose();

        (*--- display new sets *)
        FillDisplayedCollectionsBrowser(fv);
        FillHiddenCollectionsBrowser(fv);
      END;
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented\n");
    END;
  END CollectionSel;


PROCEDURE HideCollection (             fv       : FormsVBT.T;
                          <* UNUSED *> name     : TEXT;
                          <* UNUSED *> eventData: REFANY;
                          <* UNUSED *> time     : VBT.TimeStamp) =
  VAR
    coll: TEXT;
    set : TextCursorSet.T;
  BEGIN
    TRY
      (*--- get selected element and change sets *)
      coll :=
        FormsVBT.GetTextProperty(panel, "DisplayedCollections", "Select");
      IF (coll # NIL) THEN
        set := ParseSelection(coll);
        invisibleCollections.union(set);
        visibleCollections.difference(set);
        set.dispose();

        (*--- display new sets *)
        FillDisplayedCollectionsBrowser(fv);
        FillHiddenCollectionsBrowser(fv);
      END;
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented\n");
    END;
  END HideCollection;


PROCEDURE HideAllCollections (             fv       : FormsVBT.T;
                              <* UNUSED *> name     : TEXT;
                              <* UNUSED *> eventData: REFANY;
                              <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    (*--- mixing the sets *)
    invisibleCollections.union(Globals.declaredCollectionsText);
    visibleCollections.dispose();
    visibleCollections := TextCursorSet.New();

    (*--- display new sets *)
    FillDisplayedCollectionsBrowser(fv);
    FillHiddenCollectionsBrowser(fv);
  END HideAllCollections;


PROCEDURE DisplayAllRelations (             fv       : FormsVBT.T;
                               <* UNUSED *> name     : TEXT;
                               <* UNUSED *> eventData: REFANY;
                               <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    (*--- mixing the sets *)
    visibleRelations.union(Globals.declaredRelationsText);
    invisibleRelations.dispose();
    invisibleRelations := TextCursorSet.New();

    (*--- display new sets *)
    FillDisplayedRelationsBrowser(fv);
    FillHiddenRelationsBrowser(fv);
  END DisplayAllRelations;


PROCEDURE RelationSel (             fv       : FormsVBT.T;
                       <* UNUSED *> name     : TEXT;
                       <* UNUSED *> eventData: REFANY;
                       <* UNUSED *> time     : VBT.TimeStamp) =
  VAR
    rel: TEXT;
    set: TextCursorSet.T;
  BEGIN
    TRY
      (*--- get selected element and change sets *)
      rel := FormsVBT.GetTextProperty(fv, "HiddenRelations", "Select");
      IF (rel # NIL) THEN
        set := ParseSelection(rel);
        visibleRelations.union(set);
        invisibleRelations.difference(set);
        set.dispose();

        (*--- display new sets *)
        FillDisplayedRelationsBrowser(fv);
        FillHiddenRelationsBrowser(fv);
      END;
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented\n");
    END;
  END RelationSel;


PROCEDURE HideRelation (             fv       : FormsVBT.T;
                        <* UNUSED *> name     : TEXT;
                        <* UNUSED *> eventData: REFANY;
                        <* UNUSED *> time     : VBT.TimeStamp) =
  VAR
    rel: TEXT;
    set: TextCursorSet.T;
  BEGIN
    TRY
      (*--- get selected element and change sets *)
      rel :=
        FormsVBT.GetTextProperty(panel, "DisplayedRelations", "Select");
      IF (rel # NIL) THEN
        set := ParseSelection(rel);
        invisibleRelations.union(set);
        visibleRelations.difference(set);
        set.dispose();

        (*--- display new sets *)
        FillDisplayedRelationsBrowser(fv);
        FillHiddenRelationsBrowser(fv);
      END;
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented\n");
    END;
  END HideRelation;


PROCEDURE HideAllRelations (             fv       : FormsVBT.T;
                            <* UNUSED *> name     : TEXT;
                            <* UNUSED *> eventData: REFANY;
                            <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    (*--- mixing the sets *)
    invisibleRelations.union(Globals.declaredRelationsText);
    visibleRelations.dispose();
    visibleRelations := TextCursorSet.New();

    (*--- display new sets *)
    FillDisplayedRelationsBrowser(fv);
    FillHiddenRelationsBrowser(fv);
  END HideAllRelations;


PROCEDURE ParseSelection (READONLY sel: TEXT): TextCursorSet.T =
  CONST scan = SET OF CHAR{'a'.. 'z', 'A'.. 'Z', '/'};
  VAR
    selSet       := TextCursorSet.New();
    coll  : TEXT := "";
  <* FATAL Thread.Alerted, Rd.Failure *>
  BEGIN
    TRY
      WITH rd = TextRd.New(sel) DO
        WHILE NOT Rd.EOF(rd) DO
          coll := Lex.Scan(rd, scan);
          selSet.insert(coll);
          Lex.Skip(rd);
        END;
      END;
    EXCEPT
    END;
    RETURN selSet;
  END ParseSelection;


(*----------------------*)
(*--- Error messages ---*)
(*----------------------*)

PROCEDURE DisplayError (message: TEXT) =
  (* This procedure is used to display an error message in a popup
     window.*)
  BEGIN
    TRY
      FormsVBT.PutText(panel, "ErrorMessage", message);
      FormsVBT.PopUp(panel, "ErrorForm");
    EXCEPT
    | FormsVBT.Error =>
        IO.Put("FormsVBT Error while trying to display : " & message,
               Stdio.stderr);
    | FormsVBT.Unimplemented =>
        IO.Put("FormsVBT unimplemented while trying to display : "
                 & message, Stdio.stderr)
    END
  END DisplayError;


BEGIN
END Panel.
