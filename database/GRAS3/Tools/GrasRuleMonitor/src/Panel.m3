MODULE Panel;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:51  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:41  hosking
    Import of GRAS3 1.1

    Revision 1.3  1997/11/12 17:24:18  roland
    Start/Stop fixed. The listener really gets killed when stopping the
    monitor.

    Revision 1.2  1997/11/07 08:58:09  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

    Revision 1.1  1997/10/31 14:28:31  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

IMPORT Wr, Rd, FormsVBT, Rsrc, VBT, Thread, Trestle, TrestleComm, IO, Text;
IMPORT TextRd, Lex, Fmt, IntSeq, FloatMode, IntIntTbl;
IMPORT Views, MonitorBundle, MonitoredTypes, PatternEditor, RuleMonitor,
       TextView, ChartView, ChartVBT, InfoPanel;
IMPORT EventTypes, EventType, RuleEngine, Event, ContextSet;

TYPE ViewType = {Text, Chart};

VAR
  path         : Rsrc.Path  := Rsrc.BuildPath(".", MonitorBundle.Get());
  panel        : FormsVBT.T;
  SelectedViews             := SET OF ViewType{ViewType.Text};

PROCEDURE Install (): BOOLEAN =
  <* FATAL FormsVBT.Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted *>
  BEGIN
    TRY
      panel := NEW(FormsVBT.T).initFromRsrc("MonitorPanel.fv", path);
      FormsVBT.AttachProc(panel, "quit", Quit);
      FormsVBT.AttachProc(panel, "stop", StopMonitoring);
      FormsVBT.AttachProc(panel, "start", StartMonitoring);
      FormsVBT.AttachProc(panel, "reallyquit", ReallyQuit);
      FormsVBT.AttachProc(panel, "dontquit", DontQuit);
      FormsVBT.AttachProc(panel, "MonitorAll", MonitorAll);
      FormsVBT.AttachProc(panel, "MonitorSel", MonitorSel);
      FormsVBT.AttachProc(panel, "ShowInfo", ShowInfo);
      FormsVBT.AttachProc(panel, "UnmonitorSel", UnmonitorSel);
      FormsVBT.AttachProc(panel, "UnmonitorAll", UnmonitorAll);
      FormsVBT.AttachProc(panel, "textview", ToggleTextView);
      FormsVBT.AttachProc(panel, "chartview", ToggleChartView);

      FormsVBT.AttachProc(panel, "MonitoredTypes", EditPattern);
      FormsVBT.AttachProc(panel, "UnmonitoredTypes", EditPattern);

      FillBrowsers();
      Trestle.Install(panel, "RuleMonitor");
    EXCEPT
      TrestleComm.Failure => IO.Put("Cannot open panel.\n"); RETURN FALSE;
    END;
    Trestle.AwaitDelete(panel);
    RETURN TRUE;
  END Install;

PROCEDURE Quit (<* UNUSED *> fv       : FormsVBT.T;
                <* UNUSED *> name     : TEXT;
                <* UNUSED *> eventData: REFANY;
                <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    IF Views.Number() = 0 THEN
      Trestle.Delete(panel);
    ELSE
      FormsVBT.PopUp(panel, "warning");
    END;
  END Quit;

PROCEDURE StopMonitoring (<* UNUSED *> fv       : FormsVBT.T;
                          <* UNUSED *> name     : TEXT;
                          <* UNUSED *> eventData: REFANY;
                          <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    StopListener := TRUE;
    Thread.Alert(listener);
    EVAL Thread.Join(listener);
    listener := NIL;
    RuleMonitor.UninstallMonitor();
  END StopMonitoring;

PROCEDURE ViewClosed () =
  BEGIN
    IF Views.Number() = 0 THEN RuleMonitor.UninstallMonitor(); END;
  END ViewClosed;

PROCEDURE StartMonitoring (<* UNUSED *> fv       : FormsVBT.T;
                           <* UNUSED *> name     : TEXT;
                           <* UNUSED *> eventData: REFANY;
                           <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    IF ViewType.Text IN SelectedViews THEN
      TextViewWr := TextView.Open(ViewClosed);
    END;
    IF ViewType.Chart IN SelectedViews THEN
      WITH number = MonitoredTypes.Number(),
           seq    = MonitoredTypes.Get()     DO
        (* we have MonitoredTypes.Number() types to display *)
        ChartViewRects := ChartView.Open(number, ViewClosed);
        typeMap := NEW(IntIntTbl.Default).init(number);
        typeCount := NEW(IntIntTbl.Default).init(number);
        FOR i := 0 TO seq.size() - 1 DO
          EVAL typeMap.put(seq.get(i), i + 1);
          EVAL typeCount.put(seq.get(i), 0);
          ChartVBT.SetChartName(ChartViewRects, i + 1, Fmt.Int(seq.get(i)));
        END;
      END;
    END;
    IF SelectedViews # SET OF ViewType{} THEN
      RuleMonitor.InstallMonitor(
        actproc := MonitorAction, interest := RuleEngine.Interest.Others);
      IF listener = NIL THEN
        StopListener := FALSE;
        listener := Thread.Fork(NEW(Thread.Closure, apply := Listen));
      END;
    END;
  END StartMonitoring;

PROCEDURE DontQuit (<* UNUSED *> fv       : FormsVBT.T;
                    <* UNUSED *> name     : TEXT;
                    <* UNUSED *> eventData: REFANY;
                    <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    FormsVBT.PopDown(panel, "warning");
  END DontQuit;

PROCEDURE EditPattern (<* UNUSED *> fv       : FormsVBT.T;
                                    name     : TEXT;
                       <* UNUSED *> eventData: REFANY;
                       <* UNUSED *> time     : VBT.TimeStamp) =
  VAR
    sel  : TEXT;
    types: IntSeq.T;
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    IF Text.Equal(name, "MonitoredTypes") THEN
      sel := FormsVBT.GetTextProperty(panel, "MonitoredTypes", "Select");
    ELSIF Text.Equal(name, "UnmonitoredTypes") THEN
      sel := FormsVBT.GetTextProperty(panel, "UnmonitoredTypes", "Select");
    END;
    IF sel # NIL THEN
      types := ParseSelection(sel);
      IF types.size() > 0 THEN PatternEditor.Open(types.get(0)); END;
    END;
  END EditPattern;

PROCEDURE ReallyQuit (<* UNUSED *> fv       : FormsVBT.T;
                      <* UNUSED *> name     : TEXT;
                      <* UNUSED *> eventData: REFANY;
                      <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    Trestle.Delete(panel);
  END ReallyQuit;

PROCEDURE MonitorAll (<* UNUSED *> fv       : FormsVBT.T;
                      <* UNUSED *> name     : TEXT;
                      <* UNUSED *> eventData: REFANY;
                      <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    FOR i := 1 TO EventTypes.GetNumberOfTypes() DO
      MonitoredTypes.Insert(i);
    END;
    FillBrowsers();
  END MonitorAll;

PROCEDURE ToggleChartView (<* UNUSED *> fv       : FormsVBT.T;
                           <* UNUSED *> name     : TEXT;
                           <* UNUSED *> eventData: REFANY;
                           <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    IF FormsVBT.GetBoolean(panel, "chartview") THEN
      SelectedViews := SelectedViews + SET OF ViewType{ViewType.Chart};
    ELSE
      SelectedViews := SelectedViews - SET OF ViewType{ViewType.Chart};
    END;
    IF SelectedViews = SET OF ViewType{} THEN
      FormsVBT.MakeDormant(panel, "start");
    ELSE
      FormsVBT.MakeActive(panel, "start");
    END;
  END ToggleChartView;

PROCEDURE ToggleTextView (<* UNUSED *> fv       : FormsVBT.T;
                          <* UNUSED *> name     : TEXT;
                          <* UNUSED *> eventData: REFANY;
                          <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    IF FormsVBT.GetBoolean(panel, "textview") THEN
      SelectedViews := SelectedViews + SET OF ViewType{ViewType.Text};
    ELSE
      SelectedViews := SelectedViews - SET OF ViewType{ViewType.Text};
    END;
    IF SelectedViews = SET OF ViewType{} THEN
      FormsVBT.MakeDormant(panel, "start");
    ELSE
      FormsVBT.MakeActive(panel, "start");
    END;
  END ToggleTextView;

PROCEDURE MonitorSel (<* UNUSED *> fv       : FormsVBT.T;
                      <* UNUSED *> name     : TEXT;
                      <* UNUSED *> eventData: REFANY;
                      <* UNUSED *> time     : VBT.TimeStamp) =
  VAR
    sel: TEXT;
    new: IntSeq.T;
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    (* Put all selected unmonitored types in the monitored browser *)
    sel := FormsVBT.GetTextProperty(panel, "UnmonitoredTypes", "Select");
    IF sel # NIL THEN
      new := ParseSelection(sel);
      FOR i := 0 TO new.size() - 1 DO
        MonitoredTypes.Insert(new.get(i));
      END;
      FillBrowsers();
    END;
  END MonitorSel;

PROCEDURE UnmonitorAll (<* UNUSED *> fv       : FormsVBT.T;
                        <* UNUSED *> name     : TEXT;
                        <* UNUSED *> eventData: REFANY;
                        <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    FOR i := 1 TO EventTypes.GetNumberOfTypes() DO
      MonitoredTypes.Remove(i);
    END;
    FillBrowsers();
  END UnmonitorAll;

PROCEDURE UnmonitorSel (<* UNUSED *> fv       : FormsVBT.T;
                        <* UNUSED *> name     : TEXT;
                        <* UNUSED *> eventData: REFANY;
                        <* UNUSED *> time     : VBT.TimeStamp) =
  VAR
    sel: TEXT;
    new: IntSeq.T;
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    (* Put all selected unmonitored types in the monitored browser *)
    sel := FormsVBT.GetTextProperty(panel, "MonitoredTypes", "Select");
    IF sel # NIL THEN
      new := ParseSelection(sel);
      FOR i := 0 TO new.size() - 1 DO
        MonitoredTypes.Remove(new.get(i));
      END;
      FillBrowsers();
    END;
  END UnmonitorSel;

PROCEDURE ShowInfo (<* UNUSED *> fv       : FormsVBT.T;
                    <* UNUSED *> name     : TEXT;
                    <* UNUSED *> eventData: REFANY;
                    <* UNUSED *> time     : VBT.TimeStamp) =
  VAR
    selm, selu: TEXT;
    seq       : IntSeq.T;
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  <* FATAL EventTypes.Unknown, EventType.Unknown *>
  BEGIN
    selm := FormsVBT.GetTextProperty(panel, "MonitoredTypes", "Select");
    selu := FormsVBT.GetTextProperty(panel, "UnmonitoredTypes", "Select");
    IF selm # NIL THEN
      seq := ParseSelection(selm);
    ELSIF selu # NIL THEN
      seq := ParseSelection(selu);
    ELSE
      (* no selection *)
      RETURN
    END;
    IF seq.size() > 0 THEN
      InfoPanel.Open();
      WITH type = EventTypes.Get(seq.get(0)) DO
        InfoPanel.PutType(type.getName());
        InfoPanel.PutDescription(type.getInfo());
        FOR a := 1 TO type.getNumberOfAttributes() DO
          InfoPanel.PutAttributes(type.getAttributeName(a) & ": ");
          IF type.isBoolAttribute(a) THEN
            InfoPanel.PutAttributes("BOOLEAN\n");
          ELSIF type.isIntAttribute(a) THEN
            InfoPanel.PutAttributes("INTEGER\n");
          ELSIF type.isTextAttribute(a) THEN
            InfoPanel.PutAttributes("TEXT\n");
          ELSE
            InfoPanel.PutAttributes("REFANY\n");
          END;
        END;
      END;
    END;
  END ShowInfo;

PROCEDURE ParseSelection (sel: TEXT): IntSeq.T =
  CONST skip = SET OF CHAR{'\000'.. '\377'} - SET OF CHAR{'\n'};
  VAR res := NEW(IntSeq.T).init();
  <* FATAL Thread.Alerted, Rd.Failure *>
  BEGIN
    TRY
      WITH rd = TextRd.New(sel) DO
        WHILE NOT Rd.EOF(rd) DO
          Lex.Skip(rd);
          res.addhi(Lex.Int(rd));
          Lex.Skip(rd, skip);
        END;
      END;
    EXCEPT
      FloatMode.Trap, Lex.Error => (* silentlty ignore *)
    END;
    RETURN res;
  END ParseSelection;

PROCEDURE FillBrowsers () =
  VAR monitored: IntSeq.T;

  PROCEDURE TypeEntry (t: CARDINAL): TEXT =
    <* FATAL EventTypes.Unknown *>
    BEGIN
      RETURN Fmt.Pad(Fmt.Int(t), 3, ' ', Fmt.Align.Right) & " "
               & EventTypes.Get(t).getName() & "\n";
    END TypeEntry;

  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  VAR
    monEntries, unmonEntries: TEXT     := "";
    k                       : CARDINAL := 0;
  BEGIN
    monitored := MonitoredTypes.Get();
    FOR i := 1 TO EventTypes.GetNumberOfTypes() DO
      IF k < monitored.size() AND i = monitored.get(k) THEN
        monEntries := monEntries & TypeEntry(i);
        INC(k);
      ELSE
        unmonEntries := unmonEntries & TypeEntry(i);
      END;
    END;
    FormsVBT.PutTextProperty(panel, "MonitoredTypes", "Items", monEntries);
    FormsVBT.PutTextProperty(
      panel, "UnmonitoredTypes", "Items", unmonEntries);
  END FillBrowsers;

VAR
  TextViewWr    : Wr.T;
  ChartViewRects: ChartVBT.T;
  typeMap       : IntIntTbl.T;   (* maps type numbers to rect positions *)
  typeCount     : IntIntTbl.T;

PROCEDURE MonitorAction (             event  : Event.T;
                                      context: ContextSet.T;
                                      local  : BOOLEAN;
                         <* UNUSED *> data   : <*TRANSIENT*> REFANY) =
  CONST LocalText = ARRAY BOOLEAN OF TEXT{"remote ", "local "};
  VAR
    type       : EventType.T;
    attrType   : TEXT;
    attrVal    : TEXT;
    conText    : TEXT;
    rect, count: INTEGER;

  PROCEDURE Write (t: TEXT) =
    BEGIN
      TRY
        Wr.PutText(TextViewWr, t);
        Wr.Flush(TextViewWr);
      EXCEPT
        Wr.Failure, Thread.Alerted => (* ignore *)
      END;
    END Write;

  <* FATAL EventTypes.Unknown, EventType.Unknown, EventType.Mismatch *>
  BEGIN
    IF ViewType.Text IN SelectedViews THEN
      type := EventTypes.Get(event.type());
      Write(LocalText[local] & "event " & type.getName() & "\n");
      FOR a := 1 TO type.getNumberOfAttributes() DO
        IF type.isBoolAttribute(a) THEN
          attrType := "BOOLEAN";
          attrVal := Fmt.Bool(event.getBoolAttribute(a));
        ELSIF type.isIntAttribute(a) THEN
          attrType := "INTEGER";
          attrVal := Fmt.Int(event.getIntAttribute(a));
        ELSIF type.isTextAttribute(a) THEN
          attrType := "TEXT";
          attrVal := "\"" & event.getTextAttribute(a) & "\"";
        ELSE
          attrType := "REFANY";
          IF event.getRefAnyAttribute(a) = NIL THEN
            attrVal := "NIL";
          ELSE
            attrVal := "non NIL";
          END;
        END;
        Write("\t" & type.getAttributeName(a) & ": " & attrType & " = "
                & attrVal & "\n");
      END;

      conText := "";
      WITH cont = ContextSet.ToSeq(context) DO
        IF cont.size() > 0 THEN
          FOR i := 0 TO cont.size() - 2 DO
            conText := conText & cont.get(i) & ", ";
          END;
          conText := conText & cont.get(cont.size() - 1);
        END;
        Write("context = {" & conText & "}\n");
      END;
    END;

    IF ViewType.Chart IN SelectedViews THEN
      IF typeMap.get(event.type(), rect) THEN
        EVAL typeCount.get(event.type(), count);
        EVAL typeCount.put(event.type(), count + 1);
        ChartVBT.SetChart(ChartViewRects, rect, count + 1);
      END;
    END;
  END MonitorAction;

VAR
  listener    : Thread.T;
  lock        : MUTEX    := NEW(MUTEX);
  StopListener: BOOLEAN  := FALSE;

PROCEDURE Listen (<* UNUSED *> cl: Thread.Closure): REFANY =
  BEGIN
    TRY
      LOOP
	RuleEngine.WaitForRemoteActions(lock);
	RuleEngine.ExecuteRemoteActions();
	IF StopListener THEN EXIT END;
      END;
    EXCEPT
      Thread.Alerted => (* stop *)
    END;
    RETURN NIL;
  END Listen;

BEGIN
END Panel.
