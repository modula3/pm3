MODULE PanelHandling;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.3  1998/03/17 14:13:33  kluck
    Necessary adaptions to use local graphs. (MK)


    Revision 1.3 1997/01/24 m.kluck
    Option OpenMode realisiert
    
    Revision 1.2  1997/02/26 16:19:01  roland
    Batchbetrieb realisiert. Weitere Kommandozeilenoptionen fuer
    Cachegroesse und Nameserver.

    Revision 1.1  1997/02/20 16:08:55  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT FormsVBT, Thread, VBT, Bundle, IO, Stdio, Process, OSError, Fmt,
       Trestle, Text, Wr, FileWr;
IMPORT OO1Params, oo1Forms, OO1, BenchmarkLog, Perfmeter;
IMPORT ErrorSupport, LogView;

VAR
  PerfmeterProcess: Process.T;
  PerfmeterRunning: BOOLEAN        := FALSE;
  log             : BenchmarkLog.T := NIL;
  panel           : FormsVBT.T;

PROCEDURE CreatePanel (blog: BenchmarkLog.T): FormsVBT.T RAISES {} =
  BEGIN
    TRY
      log := blog;
      panel :=
        NEW(FormsVBT.T).init(Bundle.Get(oo1Forms.Get(), "oo1panel.fv"));
      (* The graph type *)
      CASE OO1Params.graphtype OF
        OO1.GraphType.Persistent =>
          FormsVBT.PutChoice(panel, "graphtype", "persistent");
      | OO1.GraphType.ChgMgmt =>
          FormsVBT.PutChoice(panel, "graphtype", "chgmgmt");
      | OO1.GraphType.Typed =>
          FormsVBT.PutChoice(panel, "graphtype", "typed");
      END;

      (*--- set OpenMode ---*)
      IF NOT OO1Params.local THEN
        FormsVBT.PutChoice (panel, "OpenMode", "remote");
      ELSE
        FormsVBT.PutChoice (panel, "OpenMode", "local");
      END;
      
      (* number of parts in the database *)
      FormsVBT.PutInteger(panel, "nodes", OO1Params.N);

      (* simple or attributed connects *)
      IF OO1Params.SimpleConnects THEN
        FormsVBT.PutChoice(panel, "connects", "simple");
      ELSE
        FormsVBT.PutChoice(panel, "connects", "attributed");
      END;

      (* transaction granularity *)
      IF OO1Params.Quick THEN
        FormsVBT.PutChoice(panel, "transactions", "fewlarge");
      ELSE
        FormsVBT.PutChoice(panel, "transactions", "manysmall");
      END;

      (* suite of benchmarks *)
      FormsVBT.PutBoolean(
        panel, "buildup", OO1.Benchmark.Load IN OO1Params.Performing);
      FormsVBT.PutBoolean(
        panel, "lookup", OO1.Benchmark.Lookup IN OO1Params.Performing);
      FormsVBT.PutBoolean(
        panel, "traversal", OO1.Benchmark.Traverse IN OO1Params.Performing);
      FormsVBT.PutBoolean(panel, "reverse", OO1.Benchmark.ReverseTraverse
                                              IN OO1Params.Performing);
      FormsVBT.PutBoolean(
        panel, "insert", OO1.Benchmark.Insert IN OO1Params.Performing);

      (* Use performance meter *)
      FormsVBT.PutBoolean(panel, "perfmeter", OO1Params.PerfMeter);

      (* Now attach the event handlers *)
      FormsVBT.AttachProc(panel, "start", Start);
      FormsVBT.AttachProc(panel, "quit", Quit);
      FormsVBT.AttachProc(panel, "stop", Stop);
      FormsVBT.AttachProc(panel, "continue", Continue);
      FormsVBT.AttachProc(panel, "perfmeter", PerfmeterHandler);
      FormsVBT.AttachProc(panel, "buildup", SuiteHandler);
      FormsVBT.AttachProc(panel, "lookup", SuiteHandler);
      FormsVBT.AttachProc(panel, "traversal", SuiteHandler);
      FormsVBT.AttachProc(panel, "reverse", SuiteHandler);
      FormsVBT.AttachProc(panel, "insert", SuiteHandler);
      FormsVBT.AttachProc(panel, "connects", ConnectionTypeHandler);
      FormsVBT.AttachProc(panel, "transactions", TransactionTypeHandler);
      FormsVBT.AttachProc(panel, "graphtype", GraphTypeHandler);
      FormsVBT.AttachProc(panel, "OpenMode", OpenModeHandler);
      FormsVBT.AttachProc(panel, "nodes", PartNumberHandler);
      FormsVBT.AttachProc(panel, "uselogfile", LogFileHandler);
      FormsVBT.AttachProc(panel, "logfile", LogFileHandler);
      FormsVBT.AttachProc(panel, "fbrowser", FileBrowserHandler);
      FormsVBT.AttachProc(panel, "open", FileBrowserHandler);
      FormsVBT.AttachProc(panel, "cancel", FileBrowserHandler);
    EXCEPT
      FormsVBT.Error =>
        IO.Put(
          "Forms initialization failed. FormsVBT.Error\n", Stdio.stderr);
        Process.Exit(1);
    | FormsVBT.Unimplemented =>
        IO.Put("Forms initialization failed. FormsVBT.Unimplemented\n",
               Stdio.stderr);
        Process.Exit(1);
    END;
    OO1.Init(NotifyFinish, NotifyError);
    RETURN panel;
  END CreatePanel;

PROCEDURE StartPerfmeter () RAISES {Thread.Alerted} =
  BEGIN
    IF PerfmeterRunning THEN StopPerfmeter() END;
    TRY
      PerfmeterProcess :=
        Perfmeter.Start(Perfmeter.DefaultQuantities, TRUE,
                        "oo1.perfmeter.log." & Fmt.Int(Process.GetMyID()));
      PerfmeterRunning := TRUE;
    EXCEPT
    | OSError.E (code) =>
        log.write("Warning: Can not start performance meter! ("
                    & ErrorSupport.Fmt(code) & ")\n");
        OO1Params.PerfMeter := FALSE;
    END;
  END StartPerfmeter;

PROCEDURE StopPerfmeter () =
  BEGIN
    IF PerfmeterRunning THEN
      Perfmeter.Stop(PerfmeterProcess);
      PerfmeterRunning := FALSE;
    END;
  END StopPerfmeter;

(* Event handler for OO1 control panel *)

PROCEDURE Start (<* UNUSED *> fv       : FormsVBT.T;
                 <* UNUSED *> name     : TEXT;
                 <* UNUSED *> eventData: REFANY;
                 <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    LogView.Close(log);
    IF PerfmeterRunning THEN StopPerfmeter(); END;
    TRY
      FormsVBT.MakeDormant(panel, "parameters");
      FormsVBT.MakeDormant(panel, "start");
      FormsVBT.MakeActive(panel, "stop");
      FormsVBT.MakeDormant(panel, "continue");
      FormsVBT.MakeDormant(panel, "quit");
    EXCEPT
      FormsVBT.Error =>
        IO.Put("Forms error. Aborting.\n");
        Process.Exit(1);
    END;
    LogView.Open(log);
    OpenLogFile(new := TRUE);
    TRY
      log.writeTime();
      IF OO1Params.PerfMeter THEN StartPerfmeter(); END;
      OO1.Start(log, OO1Params.graphtype, OO1Params.local, OO1Params.Performing,
                OO1Params.N, OO1Params.SimpleConnects, OO1Params.Quick);
    EXCEPT
      Thread.Alerted =>
        IO.Put("Interrupted\n", Stdio.stderr);
        Process.Exit(0);
    | OO1.StateError =>
        IO.Put("OO1 complains bad state.\n", Stdio.stderr);
        Process.Exit(1);
    END;
  END Start;

PROCEDURE Continue (<* UNUSED *> fv       : FormsVBT.T;
                    <* UNUSED *> name     : TEXT;
                    <* UNUSED *> eventData: REFANY;
                    <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    TRY
      FormsVBT.MakeDormant(panel, "parameters");
      FormsVBT.MakeDormant(panel, "start");
      FormsVBT.MakeActive(panel, "stop");
      FormsVBT.MakeDormant(panel, "continue");
      FormsVBT.MakeDormant(panel, "quit");
    EXCEPT
      FormsVBT.Error =>
        IO.Put("Forms error. Aborting.\n");
        Process.Exit(1);
    END;
    OpenLogFile(new := FALSE);
    TRY
      OO1.Continue();
    EXCEPT
      OO1.StateError =>
        IO.Put("Cannot continue with execution!\n", Stdio.stderr);
        Process.Exit(1);
    END;
  END Continue;

PROCEDURE Quit (<* UNUSED *> fv       : FormsVBT.T;
                <* UNUSED *> name     : TEXT;
                <* UNUSED *> eventData: REFANY;
                <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    TRY
      OO1.Quit();
    EXCEPT
      OO1.StateError =>          (* don't care *)
    END;
    LogView.Close(log);
    CloseLogFile();
    IF OO1Params.PerfMeter AND PerfmeterRunning THEN
      Perfmeter.Stop(PerfmeterProcess);
    END;
    Trestle.Delete(panel);
  END Quit;

PROCEDURE Stop (<* UNUSED *> fv       : FormsVBT.T;
                <* UNUSED *> name     : TEXT;
                <* UNUSED *> eventData: REFANY;
                <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    TRY
      OO1.Stop();
    EXCEPT
      OO1.StateError =>          (* this might due to an error situation *)
    END;
    CloseLogFile();
    TRY
      FormsVBT.MakeActive(panel, "parameters");
      FormsVBT.MakeActive(panel, "start");
      FormsVBT.MakeDormant(panel, "stop");
      FormsVBT.MakeActive(panel, "continue");
      FormsVBT.MakeActive(panel, "quit");
    EXCEPT
      FormsVBT.Error =>
        IO.Put("Forms error. Aborting.\n");
        Process.Exit(1);
    END
  END Stop;

PROCEDURE SuiteHandler (<* UNUSED *> fv       : FormsVBT.T;
                                     name     : TEXT;
                        <* UNUSED *> eventData: REFANY;
                        <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  VAR sub: OO1.Suite;
  BEGIN
    IF Text.Equal(name, "buildup") THEN
      sub := OO1.Suite{OO1.Benchmark.Load};
    ELSIF Text.Equal(name, "lookup") THEN
      sub := OO1.Suite{OO1.Benchmark.Lookup};
    ELSIF Text.Equal(name, "insert") THEN
      sub := OO1.Suite{OO1.Benchmark.Insert};
    ELSIF Text.Equal(name, "traversal") THEN
      sub := OO1.Suite{OO1.Benchmark.Traverse};
    ELSIF Text.Equal(name, "reverse") THEN
      sub := OO1.Suite{OO1.Benchmark.ReverseTraverse};
    END;
    IF FormsVBT.GetBoolean(panel, name) THEN
      OO1Params.Performing := OO1Params.Performing + sub;
    ELSE
      OO1Params.Performing := OO1Params.Performing - sub;
    END;
  END SuiteHandler;

PROCEDURE PerfmeterHandler (<* UNUSED *> fv       : FormsVBT.T;
                            <* UNUSED *> name     : TEXT;
                            <* UNUSED *> eventData: REFANY;
                            <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    OO1Params.PerfMeter := FormsVBT.GetBoolean(panel, "perfmeter");
  END PerfmeterHandler;

PROCEDURE GraphTypeHandler (<* UNUSED *> fv       : FormsVBT.T;
                            <* UNUSED *> name     : TEXT;
                            <* UNUSED *> eventData: REFANY;
                            <* UNUSED *> time     : VBT.TimeStamp) =
  VAR ch: TEXT;
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    ch := FormsVBT.GetChoice(panel, "graphtype");
    IF Text.Equal(ch, "persistent") THEN
      OO1Params.graphtype := OO1.GraphType.Persistent;
    ELSIF Text.Equal(ch, "chgmgmt") THEN
      OO1Params.graphtype := OO1.GraphType.ChgMgmt;
    ELSIF Text.Equal(ch, "typed") THEN
      OO1Params.graphtype := OO1.GraphType.Typed;
    END;
  END GraphTypeHandler;
  
PROCEDURE OpenModeHandler (<* UNUSED *> fv       : FormsVBT.T;
                           <* UNUSED *> name     : TEXT;
                           <* UNUSED *> eventData: REFANY;
                           <* UNUSED *> time     : VBT.TimeStamp) =
VAR om : TEXT;
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
BEGIN
  om := FormsVBT.GetChoice (panel, "OpenMode");
  IF Text.Equal (om, "remote") THEN
    OO1Params.local := FALSE;
  ELSE
    OO1Params.local := TRUE;
  END;
END OpenModeHandler;
  
PROCEDURE TransactionTypeHandler (<* UNUSED *> fv       : FormsVBT.T;
                                  <* UNUSED *> name     : TEXT;
                                  <* UNUSED *> eventData: REFANY;
                                  <* UNUSED *> time     : VBT.TimeStamp) =
  VAR ch: TEXT;
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    ch := FormsVBT.GetChoice(panel, "transactions");
    IF Text.Equal(ch, "fewlarge") THEN
      OO1Params.Quick := TRUE;
    ELSIF Text.Equal(ch, "manysmall") THEN
      OO1Params.Quick := FALSE;
    END;
  END TransactionTypeHandler;

PROCEDURE ConnectionTypeHandler (<* UNUSED *> fv       : FormsVBT.T;
                                 <* UNUSED *> name     : TEXT;
                                 <* UNUSED *> eventData: REFANY;
                                 <* UNUSED *> time     : VBT.TimeStamp) =
  VAR ch: TEXT;
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    ch := FormsVBT.GetChoice(panel, "connects");
    IF Text.Equal(ch, "attributed") THEN
      OO1Params.SimpleConnects := FALSE;
    ELSIF Text.Equal(ch, "simple") THEN
      OO1Params.SimpleConnects := TRUE;
    END;
  END ConnectionTypeHandler;

PROCEDURE PartNumberHandler (<* UNUSED *> fv       : FormsVBT.T;
                             <* UNUSED *> name     : TEXT;
                             <* UNUSED *> eventData: REFANY;
                             <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    OO1Params.N := FormsVBT.GetInteger(panel, "nodes");
  END PartNumberHandler;

VAR
  uselogfile: BOOLEAN  := FALSE;
  logfile   : TEXT     := "";
  LogWr     : FileWr.T := NIL;

PROCEDURE LogFileHandler (<* UNUSED *> fv       : FormsVBT.T;
                                       name     : TEXT;
                          <* UNUSED *> eventData: REFANY;
                          <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    IF Text.Equal(name, "uselogfile") THEN
      uselogfile := FormsVBT.GetBoolean(panel, "uselogfile");
      IF uselogfile THEN
        FormsVBT.MakeActive(panel, "logfile");
        logfile := FormsVBT.GetText(panel, "logfile");
      ELSE
        FormsVBT.MakeDormant(panel, "logfile");
      END;
    ELSIF Text.Equal(name, "logfile") THEN
      logfile := FormsVBT.GetText(panel, "logfile");
    END;
  END LogFileHandler;

PROCEDURE OpenLogFile (new: BOOLEAN) =
  BEGIN
    IF logfile # NIL AND NOT Text.Equal(logfile, "") THEN
      TRY
        IF new THEN
          LogWr := FileWr.Open(logfile);
        ELSE
          LogWr := FileWr.OpenAppend(logfile);
        END;
        log.addWriter(LogWr);
      EXCEPT
        OSError.E => IO.Put("Cannot open " & logfile & " as logfile.\n");
      END;
    END;
  END OpenLogFile;

PROCEDURE CloseLogFile () =
  BEGIN
    IF LogWr # NIL THEN
      log.removeWriter(LogWr);
      TRY
        Wr.Close(LogWr);
      EXCEPT
        Thread.Alerted, Wr.Failure => (* don't care *)
      END;
      LogWr := NIL;
    END;
  END CloseLogFile;

PROCEDURE FileBrowserHandler (<* UNUSED *> fv       : FormsVBT.T;
                                           name     : TEXT;
                              <* UNUSED *> eventData: REFANY;
                              <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    IF Text.Equal(name, "fbrowser") THEN
      logfile := FormsVBT.GetText(panel, "fbrowser");
      FormsVBT.PutText(panel, "logfile", logfile);
    ELSIF Text.Equal(name, "open") THEN
      logfile := FormsVBT.GetText(panel, "fbrowser");
      FormsVBT.PutText(panel, "logfile", logfile);
    ELSIF Text.Equal(name, "cancel") THEN
      (* discard changes in logfile *)
      logfile := FormsVBT.GetText(panel, "logfile");
    END;
    FormsVBT.PopDown(panel, "filebrowser");
  END FileBrowserHandler;

PROCEDURE NotifyFinish() =
  BEGIN
    TRY
      FormsVBT.MakeActive(panel, "parameters");
      FormsVBT.MakeActive(panel, "start");
      FormsVBT.MakeDormant(panel, "stop");
      FormsVBT.MakeDormant(panel, "continue");
      FormsVBT.MakeActive(panel, "quit");
    EXCEPT
      FormsVBT.Error =>
        IO.Put("Forms error. Aborting.\n");
        Process.Exit(1);
    END
  END NotifyFinish;

PROCEDURE NotifyError() =
  BEGIN
    TRY
      FormsVBT.MakeActive(panel, "parameters");
      FormsVBT.MakeActive(panel, "start");
      FormsVBT.MakeDormant(panel, "stop");
      FormsVBT.MakeDormant(panel, "continue");
      FormsVBT.MakeActive(panel, "quit");
    EXCEPT
      FormsVBT.Error =>
        IO.Put("Forms error. Aborting.\n");
        Process.Exit(1);
    END
  END NotifyError;
  
BEGIN
END PanelHandling.
