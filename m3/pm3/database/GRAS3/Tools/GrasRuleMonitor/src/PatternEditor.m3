MODULE PatternEditor;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/11/07 08:58:11  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

*)
(***************************************************************************)

IMPORT EventPattern, EventType, EventTypes, Patterns, InfoPanel;
IMPORT FormsVBT, Trestle, TrestleComm, Rsrc, MonitorBundle, IO, Lex, Rd,
       VBT, Thread, Text;

TYPE
  AttrType = {Bool, Int, Text, RefAny};
  AttrInfo = REF RECORD
                   pat  : EventPattern.T;
                   etype: EventType.T;
                   no   : CARDINAL;
                   type : AttrType;
                 END;

VAR
  AlreadyOpen             := FALSE;
  panel      : FormsVBT.T;
  path       : Rsrc.Path  := Rsrc.BuildPath(".", MonitorBundle.Get());

PROCEDURE WildBoolName (typ: EventType.T; no: CARDINAL): TEXT =
  <* FATAL EventType.Unknown *>
  BEGIN
    RETURN typ.getAttributeName(no) & "Bool";
  END WildBoolName;

PROCEDURE ValueName (typ: EventType.T; no: CARDINAL): TEXT =
  <* FATAL EventType.Unknown *>
  BEGIN
    RETURN typ.getAttributeName(no) & "Value";
  END ValueName;

PROCEDURE FilterName (typ: EventType.T; no: CARDINAL): TEXT =
  <* FATAL EventType.Unknown *>
  BEGIN
    RETURN typ.getAttributeName(no) & "Filter";
  END FilterName;

PROCEDURE TrueName (typ: EventType.T; no: CARDINAL): TEXT =
  <* FATAL EventType.Unknown *>
  BEGIN
    RETURN typ.getAttributeName(no) & "True";
  END TrueName;

PROCEDURE FalseName (typ: EventType.T; no: CARDINAL): TEXT =
  <* FATAL EventType.Unknown *>
  BEGIN
    RETURN typ.getAttributeName(no) & "False";
  END FalseName;

PROCEDURE Open (type: CARDINAL) =
  CONST
    BoolVal                = ARRAY BOOLEAN OF TEXT{"FALSE", "TRUE"};
    NoWildCardPlaceholder  = "IsNoWildcard";
    AttrNamePlaceholder    = "AttrName";
    FilterStatePlaceholder = "FilterState";

  PROCEDURE FormFor (typ: AttrType; name: TEXT; isWildcard: BOOLEAN):
    TEXT =
    CONST
      AtSet   = SET OF CHAR{'@'};
      AllSet  = SET OF CHAR{'\000'.. '\377'};
      ScanSet = AllSet - AtSet;
    VAR
      rd         : Rd.T;
      res        : TEXT := "";
      placeholder: TEXT;
    <* FATAL Rsrc.NotFound *>
    BEGIN
      CASE typ OF
        AttrType.Bool => rd := Rsrc.Open("booledit.fv", path);
      | AttrType.Int => rd := Rsrc.Open("intedit.fv", path);
      | AttrType.Text => rd := Rsrc.Open("textedit.fv", path);
      | AttrType.RefAny => rd := Rsrc.Open("refanyedit.fv", path);
      END;
      TRY
        (* replace any place-holders *)
        WHILE NOT Rd.EOF(rd) DO
          Lex.Skip(rd, AtSet);
          res := res & Lex.Scan(rd, ScanSet);
          Lex.Skip(rd, AtSet);
          IF NOT Rd.EOF(rd) THEN
            placeholder := Lex.Scan(rd, ScanSet);
            IF Text.Equal(placeholder, NoWildCardPlaceholder) THEN
              res := res & BoolVal[NOT isWildcard];
            ELSIF Text.Equal(placeholder, FilterStatePlaceholder) THEN
              res := res & BoolVal[isWildcard];
            ELSIF Text.Equal(placeholder, AttrNamePlaceholder) THEN
              res := res & name;
            ELSE
              IO.Put("Unknown place holder: '" & placeholder & "'\n");
            END;
            Lex.Skip(rd, AtSet);
          END;
        END;
      EXCEPT
        Rd.Failure, Thread.Alerted => RETURN "";
      END;
      RETURN res;
    END FormFor;

  PROCEDURE AttributeForm (    pat : EventPattern.T;
                               typ : EventType.T;
                               ano : CARDINAL;
                           VAR form: TEXT;
                           VAR info: AttrInfo        ) =
    <* FATAL EventType.Unknown *>
    BEGIN
      info := NEW(AttrInfo, pat := pat, no := ano, etype := typ);
      IF typ.isBoolAttribute(ano) THEN
        info.type := AttrType.Bool;
      ELSIF typ.isIntAttribute(ano) THEN
        info.type := AttrType.Int;
      ELSIF typ.isTextAttribute(ano) THEN
        info.type := AttrType.Text;
      ELSE
        info.type := AttrType.RefAny;
      END;
      form :=
        FormFor(info.type, typ.getAttributeName(ano), pat.isWildcard(ano));
    END AttributeForm;


  VAR
    attrinfo: AttrInfo;
    form    : TEXT;
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    TRY
      WITH etype = EventTypes.Get(type),
           pat   = Patterns.Get(type)    DO
        IF AlreadyOpen THEN Close(); END;
        panel := NEW(FormsVBT.T).initFromRsrc("EditorPanel.fv", path);
        FormsVBT.PutText(panel, "type", etype.getName());
        FOR a := 1 TO etype.getNumberOfAttributes() DO
          AttributeForm(pat, etype, a, form, attrinfo);
          EVAL FormsVBT.Insert(panel, "attributes", form, a);
          FormsVBT.AttachProc(
            panel, WildBoolName(etype, a), ToggleWildCard, attrinfo);
          CASE attrinfo.type OF
            AttrType.Bool =>
              WITH val = attrinfo.pat.getBoolAttribute(attrinfo.no) DO
                SetPanelBoolValue(panel, attrinfo, val);
              END;
              FormsVBT.AttachProc(
                panel, TrueName(etype, a), ChangeValue, attrinfo);
              FormsVBT.AttachProc(
                panel, FalseName(etype, a), ChangeValue, attrinfo);
          | AttrType.Int =>
              WITH val = attrinfo.pat.getIntAttribute(attrinfo.no) DO
                SetPanelIntValue(panel, attrinfo, val);
              END;
              FormsVBT.AttachProc(
                panel, ValueName(etype, a), ChangeValue, attrinfo);
          | AttrType.Text =>
              WITH val = attrinfo.pat.getTextAttribute(attrinfo.no) DO
                SetPanelTextValue(panel, attrinfo, val);
              END;
              FormsVBT.AttachProc(
                panel, ValueName(etype, a), ChangeValue, attrinfo);
          ELSE                   (* nothing to do for refany *)
          END;
        END;
        FormsVBT.AttachProc(panel, "editok", OkClose, attrinfo);
        FormsVBT.AttachProc(panel, "editcancel", Cancel);
        FormsVBT.AttachProc(panel, "editshowinfo", ShowInfo, etype);
        Trestle.Install(panel, "RuleMonitor", "Pattern Editor");

        AlreadyOpen := TRUE;
      END;
    EXCEPT
      EventTypes.Unknown =>      (* unknown type, ignore *)
    | FormsVBT.Error => IO.Put("PatternEditor.Open: FormsVBT.Error\n")
    | Rd.Failure => IO.Put("PatternEditor.Open: Rd.Failure\n");
    | Rsrc.NotFound => IO.Put("PatternEditor.Open: Rsrc.NotFound\n");
    | Thread.Alerted => IO.Put("PatternEditor.Open: Thread.Alerted\n");
    | FormsVBT.Unimplemented =>
        IO.Put("PatternEditor.Open: FormsVBT.Unimplemented\n");
    | TrestleComm.Failure =>
        IO.Put("PatternEditor.Open: TrestleComm.Failure\n");
    END;
  END Open;

PROCEDURE Close () =
  BEGIN
    Trestle.Delete(panel);
    AlreadyOpen := FALSE;
  END Close;

PROCEDURE ChangeValue (<* UNUSED *> fv       : FormsVBT.T;
                                    name     : TEXT;
                                    eventData: REFANY;
                       <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    WITH info = NARROW(eventData, AttrInfo) DO
      CASE info.type OF
        AttrType.Bool =>
          IF Text.Equal(name, TrueName(info.etype, info.no)) THEN
            SetPanelBoolValue(panel, info, TRUE);
          ELSIF Text.Equal(name, FalseName(info.etype, info.no)) THEN
            SetPanelBoolValue(panel, info, FALSE);
          END;
          SetPatternBoolValue(panel, info);
      | AttrType.Int => SetPatternIntValue(panel, info);
      | AttrType.Text => SetPatternTextValue(panel, info);
      | AttrType.RefAny => SetPatternRefAnyValue(panel, info);
      END;
    END;
  END ChangeValue;

PROCEDURE ToggleWildCard (<* UNUSED *> fv       : FormsVBT.T;
                          <* UNUSED *> name     : TEXT;
                                       eventData: REFANY;
                          <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  <* FATAL EventType.Unknown *>
  BEGIN
    WITH info = NARROW(eventData, AttrInfo) DO
      IF NOT FormsVBT.GetBoolean(panel, WildBoolName(info.etype, info.no)) THEN
        FormsVBT.MakeDormant(panel, FilterName(info.etype, info.no));
        info.pat.setWildcard(info.no);
      ELSE
        FormsVBT.MakeActive(panel, FilterName(info.etype, info.no));
        (* set pattern to values shown on panel *)
        CASE info.type OF
          AttrType.Bool => SetPatternBoolValue(panel, info);
        | AttrType.Int => SetPatternIntValue(panel, info);
        | AttrType.Text => SetPatternTextValue(panel, info);
        | AttrType.RefAny => SetPatternRefAnyValue(panel, info);
        END;
      END;
    END;
  END ToggleWildCard;

PROCEDURE SetPanelBoolValue (panel: FormsVBT.T;
                             info : AttrInfo;
                             val  : BOOLEAN     ) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    FormsVBT.PutBoolean(panel, TrueName(info.etype, info.no), val);
    FormsVBT.PutBoolean(panel, FalseName(info.etype, info.no), NOT val);
  END SetPanelBoolValue;

PROCEDURE SetPanelIntValue (panel: FormsVBT.T; info: AttrInfo; val: INTEGER) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    FormsVBT.PutInteger(panel, ValueName(info.etype, info.no), val);
  END SetPanelIntValue;

PROCEDURE SetPanelTextValue (panel: FormsVBT.T; info: AttrInfo; val: TEXT) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    IF val # NIL THEN
      FormsVBT.PutText(panel, ValueName(info.etype, info.no), val);
    END;
  END SetPanelTextValue;

PROCEDURE SetPatternBoolValue (panel: FormsVBT.T; info: AttrInfo) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    (* read state of TrueValue *)
    WITH val = FormsVBT.GetBoolean(panel, TrueName(info.etype, info.no)) DO
      info.pat.setBoolAttribute(info.no, val);
    END;
  END SetPatternBoolValue;

PROCEDURE SetPatternTextValue (panel: FormsVBT.T; info: AttrInfo) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    WITH val = FormsVBT.GetText(panel, ValueName(info.etype, info.no)) DO
      info.pat.setTextAttribute(info.no, val);
    END;
  END SetPatternTextValue;

PROCEDURE SetPatternIntValue (panel: FormsVBT.T; info: AttrInfo) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    WITH val = FormsVBT.GetInteger(panel, ValueName(info.etype, info.no)) DO
      info.pat.setIntAttribute(info.no, val);
    END;
  END SetPatternIntValue;

PROCEDURE SetPatternRefAnyValue (<* UNUSED *> panel: FormsVBT.T;
                                              info : AttrInfo    ) =
  <* FATAL EventType.Mismatch, EventType.Unknown *>
  BEGIN
    info.pat.setRefAnyAttribute(info.no, NIL);
  END SetPatternRefAnyValue;

PROCEDURE OkClose (<* UNUSED *> fv       : FormsVBT.T;
                   <* UNUSED *> name     : TEXT;
                                eventData: REFANY;
                   <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    WITH info = NARROW(eventData, AttrInfo) DO
      (* write changes back *)
      Patterns.Set(info.pat.type(), info.pat);
    END;
    Close();
  END OkClose;

PROCEDURE Cancel (<* UNUSED *> fv       : FormsVBT.T;
                  <* UNUSED *> name     : TEXT;
                  <* UNUSED *> eventData: REFANY;
                  <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    (* Forget about changes *)
    Close();
  END Cancel;

PROCEDURE ShowInfo (<* UNUSED *> fv       : FormsVBT.T;
                    <* UNUSED *> name     : TEXT;
                                 eventData: REFANY;
                    <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL EventType.Unknown *>
  BEGIN
    InfoPanel.Open();
    WITH etype = NARROW(eventData, EventType.T) DO
      InfoPanel.PutType(etype.getName());
      InfoPanel.PutDescription(etype.getInfo());
      FOR a := 1 TO etype.getNumberOfAttributes() DO
        InfoPanel.PutAttributes(etype.getAttributeName(a) & ": ");
        IF etype.isBoolAttribute(a) THEN
          InfoPanel.PutAttributes("BOOLEAN\n");
        ELSIF etype.isIntAttribute(a) THEN
          InfoPanel.PutAttributes("INTEGER\n");
        ELSIF etype.isTextAttribute(a) THEN
          InfoPanel.PutAttributes("TEXT\n");
        ELSE
          InfoPanel.PutAttributes("REFANY\n");
        END;
      END;
    END;
  END ShowInfo;

BEGIN
END PatternEditor.
