MODULE Main;

(***************************************************************************)
(** Created by:  Rene Huelswitt						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.9  1998/08/06 10:41:45  kluck
    User verification message now modal. (mk)

    Revision 1.8  1998/08/06 09:18:09  kluck
    Deletion of entries and relations implemented. (mk)

    Revision 1.7  1998/03/17 14:14:46  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.6  1997/10/31 17:06:31  roland
    Adapted to new RuleEngine.

    Revision 1.5  1997/07/21 10:57:08  roland
    Adapted to new set implementation.

    Revision 1.4  1997/06/06 14:24:43  roland
    Some runtime configuration support added. A file-name can be given as
    command line parameter with '-config'. From this file the collection
    name and representation types for names attributes will be read.

    Revision 1.3  1997/04/23 14:49:44  roland
    NamesBrowser now uses GrasParams for command line parsing.

    Revision 1.2  1997/04/22 10:52:59  renehuel
    Removed a bug which occured closing the program.

    Revision 1.1  1997/04/22 10:45:53  renehuel
    These files implement the NamesBrowser program.
    It can be used to view the content of a graph pool with
    all contained names, the used attributes the incoming and
    outgoing relations for a chosen name.

*)
(***************************************************************************)

(* This program is usefull for displaying the contents of a graph pool.
   All names in this pool are displayed.  For each name the following
   informations are displayed :

   - the used attributes

   - the incoming relations

   - the outgoing relations *)

IMPORT Names, VirtualResourceSystem, VirtualResource, Access, ErrorSupport,
       PageFile, TextIdSet, TextCursorSet;

IMPORT FormsVBT, Trestle, TrestleComm, IO, Thread, Rd, Pathname, FileRd,
       ResourceBundle, Rsrc, VBT, TextSeq, ParseParams, Stdio, OSError;

IMPORT GrasParams, BrowserConfiguration;

IMPORT Text;

EXCEPTION
  NoEntrySelected;
  NoRelationSelected;
  NoSourceSelected;
  NoTargetSelected;

(*--- This procedure tests whether a graph was declared local or remote
   ---*)
PROCEDURE IsLocal (name: TEXT): BOOLEAN =
  BEGIN
    IF (name # NIL) THEN
      IF Text.Equal(Text.Sub(name, 0, 1), "l") THEN
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    ELSE
      RETURN FALSE;              (* dummy value *)
    END;
  END IsLocal;


(*--- This procedure returns the original graph name without leading mark
   ---*)
PROCEDURE GetOriginalGraphName (selectedName: TEXT): TEXT =
  BEGIN
    IF (selectedName # NIL) THEN
      RETURN Text.Sub(selectedName, 3, Text.Length(selectedName) - 3);
    ELSE
      RETURN NIL;
    END;
  END GetOriginalGraphName;


PROCEDURE DisplayError (message: TEXT) =
  (* This procedure is used to display an error message in a popup
     window.*)
  BEGIN
    TRY
      FormsVBT.PutText(form, "ErrorMessage", message);
      FormsVBT.PopUp(form, "ErrorForm");
    EXCEPT
    | FormsVBT.Error =>
        IO.Put("FormsVBT Error while trying to display : " & message,
               Stdio.stderr);
    | FormsVBT.Unimplemented =>
        IO.Put("FormsVBT unimplemented while trying to display : "
                 & message, Stdio.stderr)
    END
  END DisplayError;


PROCEDURE Close (<* UNUSED *> cl  : FormsVBT.Closure;
                              form: FormsVBT.T;
                 <* UNUSED *> name: TEXT;
                 <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    TRY
      (* This is a method handler which just closes the window, and all
         opened resources, and lets the program terminate.*)
      Trestle.Delete(form);
      IF names # NIL THEN names.logout() END;
      IF virtualResource # NIL THEN virtualResource.close() END;
    EXCEPT
    | VirtualResource.FatalError =>
        IO.Put("VirtualResource FatalError\n", Stdio.stderr);
    END
  END Close;


PROCEDURE SetFileName (<* UNUSED *> cl  : FormsVBT.Closure;
                                    form: FormsVBT.T;
                       <* UNUSED *> name: TEXT;
                       <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    (* SetFileName is an event handler which is executed when a new name
       for the collection in a resource is chosen.*)
    TRY
      BrowserConfiguration.SetCollectionName(
        FormsVBT.GetText(form, "collfilename"));
      FormsVBT.PopDown(form, "collfilename");
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    END;
  END SetFileName;


PROCEDURE OpenResource (<* UNUSED *> cl  : FormsVBT.Closure;
                                     form: FormsVBT.T;
                        <* UNUSED *> name: TEXT;
                        <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR
    resourceList            : TextSeq.T;
    listString, resourcePath: TEXT;
  BEGIN
    (* OpenResource is an event handler which is executed when a new
       resource is to be chosen.  (Using the 'select resource' entry from
       the file menu)*)
    TRY
      (* The resource selection window is brought up. *)
      FormsVBT.PopUp(form, "OpenResourceForm");
      (* All resources are read from the VirtualResourceSystem, and written
         into the selection listbox of the resource selection window. *)
      resourceList := VirtualResourceSystem.GetResources();
      listString := "";
      FOR i := 1 TO resourceList.size() DO
        resourcePath := resourceList.get(i - 1);
        listString := listString & resourcePath & "\n"
      END;
      FormsVBT.PutTextProperty(
        form, "BrowserResources", "Items", listString);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | PageFile.NoAccess (e) =>
        DisplayError("PageFile NoAccess : " & e & "\n");
    | VirtualResourceSystem.FatalError (e) =>
        IO.Put("VirtualResourceSystem FatalError : "
                 & ErrorSupport.ToText(e) & "\n", Stdio.stderr);
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    END;
  END OpenResource;


PROCEDURE FillValueBrowser (form: FormsVBT.T) =
  (* This procedure fills the attribute value browser.*)
  VAR
    selectedAttribute, selectedName: TEXT;
    attributeString                : TEXT    := "";
    local                          : BOOLEAN;
  BEGIN
    TRY
      (* First the selected attribute has to be figured out. *)
      selectedAttribute :=
        FormsVBT.GetTextProperty(form, "BrowserAttributes", "Select");
      (* Then the selected name has to be figured out too. *)
      selectedName :=
        FormsVBT.GetTextProperty(form, "BrowserNames", "Select");
      local := IsLocal(selectedName);
      selectedName := GetOriginalGraphName(selectedName);
      (* Only if both were selected, the attribute value can be read.*)
      IF (selectedName # NIL) AND (selectedAttribute # NIL) THEN
        virtualResource.beginTransaction();
        attributeString :=
          BrowserConfiguration.FormatAttribute(
            selectedAttribute,
            names.getAttribute(selectedName, local,
                               names.declareAttribute(selectedAttribute)));
        virtualResource.commitTransaction()
      END;
      (* The attribute value is written into the attribute value browser.*)
      FormsVBT.PutTextProperty(
        form, "BrowserAttributeValues", "Items", attributeString);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | Names.InternalError (e) =>
        DisplayError(
          "Names Internalerror : " & ErrorSupport.ToText(e) & "\n");
    | VirtualResource.FatalError (e) =>
        DisplayError(
          "VirtualResource FatalError : " & ErrorSupport.ToText(e) & "\n");
    | Access.Locked => DisplayError("Access locked\n");
    | VirtualResource.NotInTransaction =>
        DisplayError("VirtualResource NotInTransaction\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented\n");
    | Names.Undeclared => DisplayError("Names Undeclared\n");
    | Names.Unknown => DisplayError("Names Unknown\n");
    END
  END FillValueBrowser;


PROCEDURE FillAttributeBrowser (form: FormsVBT.T) =
  VAR
    attributeSet           : TextIdSet.T;
    attributeString        : TEXT        := "";
    attribute, selectedName: TEXT;
    ok                     : BOOLEAN;
    local                  : BOOLEAN;
  BEGIN
    (* All attributes which have a value are to be listed in the attribute
       browser.*)
    TRY
      (* The selected name has to be figured out. *)
      selectedName :=
        FormsVBT.GetTextProperty(form, "BrowserNames", "Select");
      local := IsLocal(selectedName);
      selectedName := GetOriginalGraphName(selectedName);
      (* If a name was selected, then all attributes can be retrieved and
         only those who have a value for this name are displayed in the
         value browser.*)
      IF selectedName # NIL THEN
        virtualResource.beginTransaction();
        (* Getting all declared attributes. *)
        attributeSet := names.getAllDeclAttributes();
        (* Running through the set of declared attributes. *)
        attributeSet.loop();
        attribute := attributeSet.get(ok).text;
        WHILE ok DO
          (* If the value of the current attribute is # NIL, then it is
             displayed in the attribute value browser. *)
          IF names.getAttribute(
               selectedName, local, names.declareAttribute(attribute))
               # NIL THEN
            attributeString := attributeString & attribute & "\n"
          END;
          attribute := attributeSet.get(ok).text;
        END;
        virtualResource.commitTransaction()
      END;
      (* The chosen attributes are displayed in the attribute browser. *)
      FormsVBT.PutTextProperty(
        form, "BrowserAttributes", "Items", attributeString);
      FillValueBrowser(form);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    | Names.InternalError (e) =>
        DisplayError(
          "Names Internalerror : " & ErrorSupport.ToText(e) & "\n");
    | VirtualResource.FatalError (e) =>
        DisplayError(
          "VirtualResource FatalError : " & ErrorSupport.ToText(e) & "\n");
    | Access.Locked => DisplayError("Access locked\n");
    | VirtualResource.NotInTransaction =>
        DisplayError("VirtualResource NotInTransaction\n");
    | Names.Undeclared => DisplayError("Names Undeclared\n");
    | Names.Unknown => DisplayError("Names Unknown\n");
    END
  END FillAttributeBrowser;


PROCEDURE FillNamesBrowser (form: FormsVBT.T) =
  VAR
    nameSet   : TextCursorSet.T;
    nameString: TEXT            := "";
    name      : TEXT;
    ok        : BOOLEAN;
  BEGIN
    (* All declared names of the opened resource are displayed in the names
       browser.*)
    TRY
      (*--- Get all locally delared names ---*)
      virtualResource.beginTransaction();
      nameSet := names.getAllEntries(local := TRUE);
      virtualResource.commitTransaction();
      (* Running through the set. *)
      nameSet.loop();
      name := nameSet.get(ok);
      WHILE ok DO
        nameString := nameString & "l: " & name & "\n";
        name := nameSet.get(ok);
      END;

      (*--- Get all remote delared names ---*)
      virtualResource.beginTransaction();
      nameSet := names.getAllEntries(local := FALSE);
      virtualResource.commitTransaction();
      (* Running through the set. *)
      nameSet.loop();
      name := nameSet.get(ok);
      WHILE ok DO
        nameString := nameString & "r: " & name & "\n";
        name := nameSet.get(ok);
      END;

      (* The found names are written to the names browser. *)
      FormsVBT.PutTextProperty(form, "BrowserNames", "Items", nameString);
      FillCollectionBrowser(form);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    | Names.InternalError (e) =>
        DisplayError(
          "Names Internalerror : " & ErrorSupport.ToText(e) & "\n");
    | VirtualResource.FatalError (e) =>
        DisplayError(
          "VirtualResource FatalError : " & ErrorSupport.ToText(e) & "\n");
    | Access.Locked => DisplayError("Access locked\n");
    | VirtualResource.NotInTransaction =>
        DisplayError("VirtualResource NotInTransaction\n");
    | Names.Undeclared => DisplayError("Names Undeclared\n");
    END
  END FillNamesBrowser;


PROCEDURE FillCollectionBrowser (form: FormsVBT.T) =
  (* All collections in which the selected name is contained are to be
     displayed in the collection browser. *)
  VAR
    collectionSet           : TextIdSet.T;
    collectionString        : TEXT        := "";
    selectedName, collection: TEXT;
    ok                      : BOOLEAN;
    local                   : BOOLEAN;
  BEGIN
    TRY
      (* The selected name has to be figured out. *)
      selectedName :=
        FormsVBT.GetTextProperty(form, "BrowserNames", "Select");
      (* determine whether it was a local graph or not *)
      local := IsLocal(selectedName);
      (* recreate original graph name without leading "r: " or "l: " *)
      selectedName := GetOriginalGraphName(selectedName);
      (* If a name was selected, the collections containing it can be
         displayed.*)
      IF selectedName # NIL THEN
        virtualResource.beginTransaction();
        (* All collections are read. *)
        collectionSet := names.getAllCollections();
        (* Running through the set. *)
        collectionSet.loop();
        collection := collectionSet.get(ok).text;
        WHILE ok DO
          (* Only if the selected name is contained in the current
             collection the collection will appear in the collection
             browser*)
          IF names.contained(
               selectedName, local, names.declareCollection(collection)) THEN
            collectionString := collectionString & collection & "\n"
          END;
          collection := collectionSet.get(ok).text;
        END;
        virtualResource.commitTransaction()
      END;
      (* The chosen collections are displayed in the collection browser. *)
      FormsVBT.PutTextProperty(
        form, "BrowserCollections", "Items", collectionString);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    | Names.InternalError (e) =>
        DisplayError(
          "Names Internalerror : " & ErrorSupport.ToText(e) & "\n");
    | VirtualResource.FatalError (e) =>
        DisplayError(
          "VirtualResource FatalError : " & ErrorSupport.ToText(e) & "\n");
    | Access.Locked => DisplayError("Access locked\n");
    | VirtualResource.NotInTransaction =>
        DisplayError("VirtualResource NotInTransaction\n");
    | Names.Undeclared => DisplayError("Names Undeclared\n");
    END
  END FillCollectionBrowser;


PROCEDURE FillRelationBrowsers (form: FormsVBT.T) =
  (* This procedure is used to find both : - all incoming relations - all
     outgoing relations *)
  VAR
    relationSet                                   : TextIdSet.T;
    incomingRelationString, outgoingRelationString: TEXT            := "";
    relation, selectedName                        : TEXT;
    ok                                            : BOOLEAN;
    local                                         : BOOLEAN;
    relationSources, relationTargets              : TextCursorSet.T;
  BEGIN
    TRY
      (* The selected name is to be figured out. *)
      selectedName :=
        FormsVBT.GetTextProperty(form, "BrowserNames", "Select");
      local := IsLocal(selectedName);
      selectedName := GetOriginalGraphName(selectedName);
      (* Only if a name is selected the relations can be read. *)
      IF selectedName # NIL THEN
        virtualResource.beginTransaction();
        (* A set with all relations is read. *)
        relationSet := names.getAllRelations();
        (* Running through the set. *)
        relationSet.loop();
        relation := relationSet.get(ok).text;
        WHILE ok DO
          (* A set with the sources of the current relation coming in to
             the selected name is read.*)
          relationSources :=
            names.sources(
              selectedName, local, names.declareRelation(relation));
          (* A set with the targets of the current relation going out from
             the select name is read. *)
          relationTargets :=
            names.targets(
              selectedName, local, names.declareRelation(relation));
          (* If the source set is not empty, the relation is displayed in
             the incoming relation browser. *)
          IF NOT relationSources.isEmpty() THEN
            incomingRelationString :=
              incomingRelationString & relation & "\n"
          END;
          (* If the target set is not empty, the relation is displayed in
             the outgoing relation browser. *)
          IF NOT relationTargets.isEmpty() THEN
            outgoingRelationString :=
              outgoingRelationString & relation & "\n";
          END;
          relation := relationSet.get(ok).text
        END;
        virtualResource.commitTransaction();
      END;
      (* The results of the chosing process above are displayed in the
         incoming and outgoing relation browsers. *)
      FormsVBT.PutTextProperty(
        form, "BrowserIncomingRelations", "Items", incomingRelationString);
      FormsVBT.PutTextProperty(
        form, "BrowserOutgoingRelations", "Items", outgoingRelationString);
      FillRelationMemberBrowser(form, TRUE);
      FillRelationMemberBrowser(form, FALSE);
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    | Names.InternalError (e) =>
        DisplayError(
          "Names Internalerror : " & ErrorSupport.ToText(e) & "\n");
    | VirtualResource.FatalError (e) =>
        DisplayError(
          "VirtualResource FatalError : " & ErrorSupport.ToText(e) & "\n");
    | Access.Locked => DisplayError("Access locked\n");
    | VirtualResource.NotInTransaction =>
        DisplayError("VirtualResource NotInTransaction\n");
    | Names.Undeclared => DisplayError("Names Undeclared\n");
    | Names.Unknown => DisplayError("Names Unknown\n");
    END
  END FillRelationBrowsers;


PROCEDURE NewResource (<* UNUSED *> cl  : FormsVBT.Closure;
                                    form: FormsVBT.T;
                       <* UNUSED *> name: TEXT;
                       <* UNUSED *> time: VBT.TimeStamp     ) =
  VAR resourcePath: TEXT;
  BEGIN
    TRY
      (* This method handler is executed when a new resource from the
         resource selection window has been selected.*)
      (* The selected resource has to be figured out. *)
      resourcePath :=
        FormsVBT.GetTextProperty(form, "BrowserResources", "Select");
      (* Only if a resource has really been selected (that means when the
         selection browser was'nt empty and an entry whithin this box was
         highlighted) , the resource can be opened. *)
      IF resourcePath # NIL THEN
        (* The resource selection window is closed. *)
        FormsVBT.PopDown(form, "OpenResourceForm");
        (* The old resources are closed. *)
        IF names # NIL THEN names.logout() END;
        IF virtualResource # NIL THEN virtualResource.close() END;
        (* A new VirtualResource is created with the selected resource
           path *)
        virtualResource :=
          NEW(VirtualResource.T).open(
            resourcePath, Access.Mode.ReadWriteShared, new := FALSE);
        virtualResource.beginTransaction();
        (* With the new VirtualResource one can log into names. *)
        names := NEW(Names.T);
        names.login(
          virtualResource, BrowserConfiguration.GetCollectionName());
        virtualResource.commitTransaction();
        FillNamesBrowser(form);
        FillAttributeBrowser(form);
        FillRelationBrowsers(form);
      END
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | PageFile.NoAccess (e) =>
        DisplayError("PageFile NoAccess : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    | Names.InternalError (e) =>
        DisplayError(
          "Names Internalerror : " & ErrorSupport.ToText(e) & "\n");
    | VirtualResource.FatalError (e) =>
        DisplayError(
          "VirtualResource FatalError : " & ErrorSupport.ToText(e) & "\n");
    | Access.Denied (e) => DisplayError("Access denied : " & e & "\n");
    | Access.Locked => DisplayError("Access locked\n");
    | VirtualResource.NotInTransaction =>
        DisplayError("VirtualResource NotInTransaction\n");
    END
  END NewResource;


PROCEDURE NewAttribute (<* UNUSED *> cl  : FormsVBT.Closure;
                                     form: FormsVBT.T;
                        <* UNUSED *> name: TEXT;
                        <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    (* This method handler is executed when a new attribute was selected in
       the attribute browser.  Now the attribute value browser has to be
       filled again.*)
    FillValueBrowser(form);
  END NewAttribute;


PROCEDURE NewIncomingRelation (<* UNUSED *> cl  : FormsVBT.Closure;
                                            form: FormsVBT.T;
                               <* UNUSED *> name: TEXT;
                               <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    (* This method handler is executed when a new incoming relation was
       selected in the incoming relation browser.  Now the relation sources
       browser has to be filled again.*)
    FillRelationMemberBrowser(form, TRUE);
  END NewIncomingRelation;


PROCEDURE NewOutgoingRelation (<* UNUSED *> cl  : FormsVBT.Closure;
                                            form: FormsVBT.T;
                               <* UNUSED *> name: TEXT;
                               <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    (* This method handler is executed when a new outgoing relation was
       selected in the outgoing relation browser.  Now the relation targets
       browser has to be filled again.*)
    FillRelationMemberBrowser(form, FALSE);
  END NewOutgoingRelation;


PROCEDURE FillRelationMemberBrowser (form: FormsVBT.T; sources: BOOLEAN) =
  VAR
    memberString                          : TEXT            := "";
    selectedRelation, selectedName, member: TEXT;
    memberSet                             : TextCursorSet.T;
    ok                                    : BOOLEAN;
    local                                 : BOOLEAN;
  BEGIN
    (* This procedure is used for filling either the - relation sources
       browser or the - relation targets browser, determined by the boolean
       parameter 'sources'.*)
    TRY
      (* Determined by 'sources' either the selected incoming or the
         selected outgoing relation is figured out.*)
      IF sources THEN
        selectedRelation := FormsVBT.GetTextProperty(
                              form, "BrowserIncomingRelations", "Select");
      ELSE
        selectedRelation := FormsVBT.GetTextProperty(
                              form, "BrowserOutgoingRelations", "Select");
      END;
      (* If there was a selected relation then, the targets respectively
         the sources can be read. *)
      IF selectedRelation # NIL THEN
        (* The selected name is to be figured out. *)
        selectedName :=
          FormsVBT.GetTextProperty(form, "BrowserNames", "Select");
        local := IsLocal(selectedName);
        selectedName := GetOriginalGraphName(selectedName);
        (* If a name was selected, the incoming/outgoing relations can be
           read. *)
        IF selectedName # NIL THEN
          virtualResource.beginTransaction();
          (* Determined by the parameter 'sources' all targets of the
             selected outgoing relation or all sources of the selected
             incoming relation are read. *)
          IF sources THEN
            memberSet :=
              names.sources(selectedName, local,
                            names.declareRelation(selectedRelation))
          ELSE
            memberSet :=
              names.targets(selectedName, local,
                            names.declareRelation(selectedRelation))
          END;
          (* Running through the set. *)
          memberSet.loop();
          member := memberSet.get(ok);
          WHILE ok DO
            memberString := memberString & member & "\n";
            member := memberSet.get(ok);
          END;
          virtualResource.commitTransaction();
        END
      END;
      (* Determined by the parameter 'sources' the results are written into
         the relation targets or the relation sources browser. *)
      IF sources THEN
        FormsVBT.PutTextProperty(
          form, "BrowserRelationSources", "Items", memberString);
      ELSE
        FormsVBT.PutTextProperty(
          form, "BrowserRelationTargets", "Items", memberString);
      END
    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT unimplemented\n");
    | Names.InternalError (e) =>
        DisplayError(
          "Names Internalerror : " & ErrorSupport.ToText(e) & "\n");
    | VirtualResource.FatalError (e) =>
        DisplayError(
          "VirtualResource FatalError : " & ErrorSupport.ToText(e) & "\n");
    | Access.Locked => DisplayError("Access locked\n");
    | VirtualResource.NotInTransaction =>
        DisplayError("VirtualResource NotInTransaction\n");
    | Names.Undeclared => DisplayError("Names Undeclared\n");
    | Names.Unknown => DisplayError("Names Unknown\n");
    END
  END FillRelationMemberBrowser;


PROCEDURE NewName (<* UNUSED *> cl  : FormsVBT.Closure;
                                form: FormsVBT.T;
                   <* UNUSED *> name: TEXT;
                   <* UNUSED *> time: VBT.TimeStamp     ) =
  BEGIN
    (* This event handler is executed when a new name was selected from the
       names browser.  All the other browsers have to be refilled. *)
    FillCollectionBrowser(form);
    FillAttributeBrowser(form);
    FillRelationBrowsers(form);
  END NewName;


PROCEDURE DeleteEntry (<*UNUSED*> cl  : FormsVBT.Closure;
                                  form: FormsVBT.T;
                       <*UNUSED*> name: TEXT;
                       <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR
    selectedName: TEXT    := "";
    local       : BOOLEAN;
  BEGIN
    TRY
      (*--- The selected name has to be figured out. *)
      selectedName :=
        FormsVBT.GetTextProperty(form, "BrowserNames", "Select");

      IF (selectedName = NIL) THEN RAISE NoEntrySelected; END;

      (*--- determine whether it was a local entry or not *)
      local := IsLocal(selectedName);

      (*--- recreate original name without leading "r: " or "l: " *)
      selectedName := GetOriginalGraphName(selectedName);

      (*--- User Verification of entry deletion *)
      FormsVBT.MakeDormant(form, "Filter");
      selectedName := "'" & selectedName & "'";
      FormsVBT.PutText(form, "EntryToDelete", selectedName);
      FormsVBT.PutText(form, "DeleteFormTitle", "Verify Entry Deletion");
      FormsVBT.PutText(
        form, "DeleteFormString", "Do you really want to delete entry ?");
      FormsVBT.PopUp(form, "DeleteForm");
    EXCEPT
      FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented!\n");
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | NoEntrySelected => DisplayError("Select an entry first!\n");
    END;
  END DeleteEntry;


PROCEDURE DeleteIncomingRelation (<*UNUSED*> cl  : FormsVBT.Closure;
                                             form: FormsVBT.T;
                                  <*UNUSED*> name: TEXT;
                                  <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR
    selectedName    : TEXT    := "";
    selectedRelation: TEXT    := "";
    selectedSource  : TEXT    := "";
    local           : BOOLEAN;
  BEGIN
    TRY
      (*--- The selected name has to be figured out. *)
      selectedName :=
        FormsVBT.GetTextProperty(form, "BrowserNames", "Select");
      IF (selectedName = NIL) THEN RAISE NoEntrySelected; END;

      (*--- find out marked relation *)
      selectedRelation := FormsVBT.GetTextProperty(
                            form, "BrowserIncomingRelations", "Select");
      IF (selectedRelation = NIL) THEN RAISE NoRelationSelected; END;

      (*--- find out marked target *)
      selectedSource :=
        FormsVBT.GetTextProperty(form, "BrowserRelationSources", "Select");
      IF (selectedSource = NIL) THEN RAISE NoSourceSelected; END;

      (*--- determine whether it was a local entry or not *)
      local := IsLocal(selectedName);

      (*--- recreate original name without leading "r: " or "l: " *)
      selectedName := GetOriginalGraphName(selectedName);

      (*--- User Verification of deletion *)
      selectedName := "'" & selectedName & "'";
      FormsVBT.PutText(form, "EntryToDelete",
                       "'" & selectedSource & "' - '" & selectedRelation
                         & "' -> " & selectedName);
      FormsVBT.PutText(
        form, "DeleteFormTitle", "Verify Incoming Relation Deletion");
      FormsVBT.PutText(form, "DeleteFormString",
                       "Do you really want to delete incoming relation ?");
      FormsVBT.PopUp(form, "DeleteForm");
    EXCEPT
      NoEntrySelected => DisplayError("Select an entry first!\n");
    | NoRelationSelected =>
        DisplayError("No incoming relation selected!\n");
    | NoSourceSelected => DisplayError("No source selected!\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented!\n");
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    END;
  END DeleteIncomingRelation;


PROCEDURE DeleteOutgoingRelation (<*UNUSED*> cl  : FormsVBT.Closure;
                                             form: FormsVBT.T;
                                  <*UNUSED*> name: TEXT;
                                  <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR
    selectedName    : TEXT    := "";
    selectedRelation: TEXT    := "";
    selectedTarget  : TEXT    := "";
    local           : BOOLEAN;
  BEGIN
    TRY
      (*--- The selected name has to be figured out. *)
      selectedName :=
        FormsVBT.GetTextProperty(form, "BrowserNames", "Select");
      IF (selectedName = NIL) THEN RAISE NoEntrySelected; END;

      (*--- find out marked relation *)
      selectedRelation := FormsVBT.GetTextProperty(
                            form, "BrowserOutgoingRelations", "Select");
      IF (selectedRelation = NIL) THEN RAISE NoRelationSelected; END;

      (*--- find out marked target *)
      selectedTarget :=
        FormsVBT.GetTextProperty(form, "BrowserRelationTargets", "Select");
      IF (selectedTarget = NIL) THEN RAISE NoTargetSelected; END;

      (*--- determine whether it was a local entry or not *)
      local := IsLocal(selectedName);

      (*--- recreate original name without leading "r: " or "l: " *)
      selectedName := GetOriginalGraphName(selectedName);

      (*--- User Verification of entry deletion *)
      selectedName := "'" & selectedName & "'";
      FormsVBT.PutText(
        form, "EntryToDelete", selectedName & " - '" & selectedRelation
                                 & "' -> '" & selectedTarget & "'");
      FormsVBT.PutText(
        form, "DeleteFormTitle", "Verify Outgoing Relation Deletion");
      FormsVBT.PutText(form, "DeleteFormString",
                       "Do you really want to delete outgoing relation ?");
      FormsVBT.PopUp(form, "DeleteForm");
    EXCEPT
      NoEntrySelected => DisplayError("Select an entry first!\n");
    | NoRelationSelected =>
        DisplayError("No outgoing relation selected!\n");
    | NoTargetSelected => DisplayError("No target selected!\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented!\n");
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    END;
  END DeleteOutgoingRelation;


PROCEDURE ButtonOk (<*UNUSED*> cl  : FormsVBT.Closure;
                               form: FormsVBT.T;
                               name: TEXT;
                    <*UNUSED*> time: VBT.TimeStamp     ) =
  VAR
    selectedName: TEXT    := NIL;
    source      : TEXT;
    target      : TEXT;
    t           : TEXT;
    inRelation  : TEXT;
    outRelation : TEXT;
    local       : BOOLEAN;
  BEGIN
    TRY
      (*--- The selected name has to be figured out. *)
      selectedName :=
        FormsVBT.GetTextProperty(form, "BrowserNames", "Select");

      (*--- recreate original name without leading "r: " or "l: " *)
      selectedName := GetOriginalGraphName(selectedName);

      (*--- determine whether it was a local entry or not *)
      local := IsLocal(selectedName);

      (*--- distinguish between entry and relation deletion : *)
      t := FormsVBT.GetText(form, "DeleteFormTitle");

      IF Text.Equal(t, "Verify Entry Deletion") THEN
        (* delete entry in names *)
        virtualResource.beginTransaction();
        names.remove(selectedName, local);
        virtualResource.commitTransaction();
      ELSE
        IF Text.Equal(t, "Verify Incoming Relation Deletion") THEN
          (* delete incoming relation in names *)
          inRelation := FormsVBT.GetTextProperty(
                          form, "BrowserIncomingRelations", "Select");
          source := FormsVBT.GetTextProperty(
                      form, "BrowserRelationSources", "Select");
          virtualResource.beginTransaction();
          names.removeFromRelation(
            source, selectedName, local, names.declareRelation(inRelation));
          virtualResource.commitTransaction();
        ELSE
          (* delete outgoing relation in names *)
          outRelation := FormsVBT.GetTextProperty(
                           form, "BrowserOutgoingRelations", "Select");
          target := FormsVBT.GetTextProperty(
                      form, "BrowserRelationTargets", "Select");
          virtualResource.beginTransaction();
          names.removeFromRelation(selectedName, target, local,
                                   names.declareRelation(outRelation));
          virtualResource.commitTransaction();
        END;
      END;
      (*--- pop down deletion window and renew display *)
      FormsVBT.PopDown(form, name);
      FormsVBT.MakeActive(form, "Filter");
      FillNamesBrowser(form);
      FillAttributeBrowser(form);
      FillRelationBrowsers(form);
    EXCEPT
      FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented!\n");
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | Access.Locked =>
        DisplayError("Access locked for graph '" & selectedName & "'\n");
    | Names.InternalError (e) =>
        DisplayError(
          "Names Internalerror : " & ErrorSupport.ToText(e) & "\n");
    | Names.Undeclared => DisplayError("Names Undeclared\n");
    | VirtualResource.FatalError =>
        IO.Put("VirtualResource FatalError\n", Stdio.stderr);
    | VirtualResource.NotInTransaction =>
        DisplayError("VirtualResource NotInTransaction\n");
    END;
  END ButtonOk;


PROCEDURE ButtonCancel (<*UNUSED*> cl  : FormsVBT.Closure;
                                   form: FormsVBT.T;
                                   name: TEXT;
                        <*UNUSED*> time: VBT.TimeStamp     ) =
  BEGIN
    TRY
      (*--- pop down deletion window *)
      FormsVBT.PopDown(form, name);
      FormsVBT.MakeActive(form, "Filter");
    EXCEPT
      FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    END;
  END ButtonCancel;


PROCEDURE CreateMainForm (): FormsVBT.T =
  VAR
    form: FormsVBT.T;
    setFilenameClosure, closeClosure, openResourceClosure,
    newResourceClosure, newAttributeClosure, newNameClosure,
    newIncomingRelationClosure, newOutgoingRelationClosure: FormsVBT.Closure;
    deleteEntryClosure        : FormsVBT.Closure;
    deleteInRelationClosure   : FormsVBT.Closure;
    deleteOutRelationClosure  : FormsVBT.Closure;
    doEntryDeletionClosure    : FormsVBT.Closure;
    cancelEntryDeletionClosure: FormsVBT.Closure;
  BEGIN
    (* This procedure is used to create and initialize the main window.*)
    TRY
      (* All closures which are used to connect an event with an
         implemented procedure are created and connected to the appropriate
         procedure. *)
      closeClosure := NEW(FormsVBT.Closure, apply := Close);
      setFilenameClosure := NEW(FormsVBT.Closure, apply := SetFileName);
      openResourceClosure := NEW(FormsVBT.Closure, apply := OpenResource);
      newResourceClosure := NEW(FormsVBT.Closure, apply := NewResource);
      newAttributeClosure := NEW(FormsVBT.Closure, apply := NewAttribute);
      newNameClosure := NEW(FormsVBT.Closure, apply := NewName);
      newIncomingRelationClosure :=
        NEW(FormsVBT.Closure, apply := NewIncomingRelation);
      newOutgoingRelationClosure :=
        NEW(FormsVBT.Closure, apply := NewOutgoingRelation);
      deleteEntryClosure := NEW(FormsVBT.Closure, apply := DeleteEntry);
      deleteInRelationClosure :=
        NEW(FormsVBT.Closure, apply := DeleteIncomingRelation);
      deleteOutRelationClosure :=
        NEW(FormsVBT.Closure, apply := DeleteOutgoingRelation);
      doEntryDeletionClosure := NEW(FormsVBT.Closure, apply := ButtonOk);
      cancelEntryDeletionClosure :=
        NEW(FormsVBT.Closure, apply := ButtonCancel);

      (* The mainform is created. *)
      form := NEW(FormsVBT.T).initFromRsrc("Browser.fv", BrowserBundle);

      (* The closures are attached to the events of the windows. *)
      FormsVBT.Attach(form, "MenuClose", closeClosure);
      FormsVBT.Attach(form, "MenuOpenResource", openResourceClosure);
      FormsVBT.Attach(form, "collfilename", setFilenameClosure);
      FormsVBT.Attach(form, "ButtonSelectResource", newResourceClosure);
      FormsVBT.Attach(form, "BrowserResources", newResourceClosure);
      FormsVBT.Attach(form, "BrowserAttributes", newAttributeClosure);
      FormsVBT.Attach(form, "BrowserNames", newNameClosure);
      FormsVBT.Attach(
        form, "BrowserIncomingRelations", newIncomingRelationClosure);
      FormsVBT.Attach(
        form, "BrowserOutgoingRelations", newOutgoingRelationClosure);
      FormsVBT.Attach(form, "MenuDeleteEntry", deleteEntryClosure);
      FormsVBT.Attach(
        form, "MenuDeleteInRelation", deleteInRelationClosure);
      FormsVBT.Attach(
        form, "MenuDeleteOutRelation", deleteOutRelationClosure);
      FormsVBT.Attach(form, "ButtonOkDelete", doEntryDeletionClosure);
      FormsVBT.Attach(
        form, "ButtonCancelDelete", cancelEntryDeletionClosure);

      FormsVBT.PutText(
        form, "collfilename", BrowserConfiguration.GetCollectionName());

    EXCEPT
    | FormsVBT.Error (e) => DisplayError("FormsVBT Error : " & e & "\n");
    | FormsVBT.Unimplemented => DisplayError("FormsVBT Unimplemented!\n");
    | Thread.Alerted => DisplayError("Thread Alerted\n");
    | Rd.Failure => DisplayError("Rd Failure\n");
    | Rsrc.NotFound => DisplayError("Resource not found\n");
    END;
    RETURN form
  END CreateMainForm;


PROCEDURE ReadOwnParams () =
  VAR pp := NEW(ParseParams.T).init(Stdio.stderr);

  PROCEDURE ParseError (err: TEXT) =
    BEGIN
      IO.Put(Pathname.Last(pp.arg[0]) & ": " & err & "\n", Stdio.stderr);
    END ParseError;

  BEGIN
    TRY
      IF pp.keywordPresent("-config") THEN configFile := pp.getNext(); END;
    EXCEPT
      ParseParams.Error =>
        ParseError("Parameter '-config' requieres an argument.");
    END;
  END ReadOwnParams;


(*----- global Variables ------------------------------------------------*)

VAR
  names               : Names.T;
  virtualResource     : VirtualResource.T;
  form                : FormsVBT.T;
  BrowserBundle       : Rsrc.Path;
  rootPath            : TEXT;
  rootValid           : BOOLEAN;
  serverId, nameserver: TEXT;
  cacheSize           : CARDINAL;
  configFile          : TEXT              := NIL;

BEGIN
  BrowserConfiguration.SetCollectionName(".GRAS");
  GrasParams.ParseComandLine(
    rootPath, rootValid, cacheSize, serverId, nameserver);
  ReadOwnParams();

  BrowserBundle := Rsrc.BuildPath(ResourceBundle.Get());
  (* read standard configuration for GRAS_3 graph pools *)
  TRY
    WITH rd = Rsrc.Open("GrasConfig", BrowserBundle) DO
      BrowserConfiguration.ReadConfiguration(rd);
    END;
  EXCEPT
    Rsrc.NotFound =>
      IO.Put("Can't read standard configuration!\n", Stdio.stderr);
  END;

  IF configFile # NIL THEN
    (* declarations in a config-file overwrite standard declarations *)
    TRY
      WITH rd = FileRd.Open(configFile) DO
        BrowserConfiguration.ReadConfiguration(rd);
      END;
    EXCEPT
      OSError.E =>
        IO.Put("Can't open configuration-file \"" & configFile
                 & "\" for reading.\n", Stdio.stderr);
    END;
  END;

  IF rootValid THEN
    TRY
      VirtualResourceSystem.Login(
        rootPath, cacheSize, serverId, nameserver);
      form := CreateMainForm();
      Trestle.Install(form);
      Trestle.AwaitDelete(form);
    EXCEPT
    | TrestleComm.Failure => DisplayError("TrestleComm Failure\n");
    END
  END;
END Main.
