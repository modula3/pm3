MODULE Main;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.5  1998/08/03 09:45:44  roland
    Changed parameter 'nameserver' to 'agent' according to gras3 convention.
    Thread stack in OO1 set higher.

    Revision 1.4  1998/03/17 14:13:30  kluck
    Necessary adaptions to use local graphs. (MK)


    Revision 1.4 1997/10/24 m.kluck
    OpenMode implemented
    
    Revision 1.3  1997/10/14 09:16:42  roland
    Merge of HiGRAS and Main branch.

    Revision 1.2.2.1  1997/07/21 10:53:46  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.2  1997/02/26 16:18:58  roland
    Batchbetrieb realisiert. Weitere Kommandozeilenoptionen fuer
    Cachegroesse und Nameserver.

    Revision 1.1  1997/02/20 16:08:45  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

(** Implementation of th OO1 benchmark as reported in [1] for GRAS(3)
    Parts are nodes with 4 attributes

    Part = RECORD
             id: CARDINAL; -- node number
             type: ARRAY [0..9] OF CHAR;
             x, y: INTEGER;
             build: Date;
           END;

    Date = RECORD
             year, month, day: INTEGER;
             hour, minute, second: INTEGER;
           END;

    Connection = RECORD
                   to, from: INTEGER; -- Part ids
                   type: ARRAY [0..9] OF CHAR;
                   length: INTEGER;
                 END;

    [1] R.G.G. Cattell and J. Skeen, "Object Operations Benchmark",
        ACM Transactions on Database Systems, 1992, vol. 17, no. 1, pp 1--31

*)


IMPORT Text, Process, Scan, Lex, FloatMode, Stdio, ASCII, Pathname,
       ParseParams, IO, Env, Thread, Fmt, OSError;
IMPORT ErrorSupport;
IMPORT TypedGraphSystem, Config;
IMPORT FormsVBT, Trestle, TrestleComm, Perfmeter;

IMPORT BenchmarkLog;
IMPORT OO1, PanelHandling, OO1Params;

(* This programs default behavior is to open a control panel which lets the
   user choose run-time parameters.  Alternatively, all parameters can be
   given via command line switches. *)

VAR
  rootpath  : Pathname.T;
  nameserver: TEXT;
  cachesize : CARDINAL;
  UsePanel  : BOOLEAN    := TRUE;
  log                    := NEW(BenchmarkLog.T);

PROCEDURE ReadParam () =
  VAR type, txt: TEXT;

  PROCEDURE UpperCase (t: TEXT): TEXT =
    VAR
      b  : REF ARRAY OF CHAR;
      len: CARDINAL;
    BEGIN
      IF t # NIL THEN
        len := Text.Length(t);
        b := NEW(REF ARRAY OF CHAR, len);
        Text.SetChars(b^, t);
        FOR i := 0 TO len - 1 DO b^[i] := ASCII.Upper[b^[i]]; END;
        RETURN Text.FromChars(b^);
      ELSE
        RETURN t;
      END;
    END UpperCase;

  CONST
    USAGE = " [-root <rootpath>] [-N <no of nodes>] \n"
              & "\t[-T (PERSISTENT | CHGMGMT | TYPED)]\n"
              & "\t[-s] [-p] [-B] [-R] [-I] [+Q] [-help]\n"
              & "\t[-agent <hostname>] [-cachesize <nop>]";

  PROCEDURE ParseError (prog, err: TEXT) =
    BEGIN
      IO.Put(err & "\n", Stdio.stderr);
      IO.Put(prog & USAGE & "\n", Stdio.stderr);
      Process.Exit(1);
    END ParseError;

  BEGIN
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO

      IF pp.keywordPresent("-help") THEN
        IO.Put("USAGE: " & Pathname.Last(pp.arg[0]) & USAGE & "\n\n");
        IO.Put("Options meaning\n");
        IO.Put(
          "-root p : Specifies root directory of gras client to be p.\n");
        IO.Put("\t  A root path must be given either using this switch\n");
        IO.Put("\t  or using the environment variable GRAS3.\n");
        IO.Put("-N n    : The number of parts in the database. "
                 & "500 <= n <= 2,000,000\n");
        IO.Put("-T gt   : Where gt is one of \"persistent\", "
                 & "\"chgmgmt\", and \"typed\".\n");
        IO.Put("\t  This option determines the type of gras "
                 & "graph used to perform\n\t  the benchmark.\n");
        IO.Put("-s\t: Use simple connections rather than attributed.\n");
        IO.Put("-B\t: Skip build up phase of benchmark "
                 & "(database must already exist).\n");
        IO.Put("-R\t: Skip all read-only phases of benchmark.\n");
        IO.Put("-I\t: Skip insert phase of benchmark.\n");
        IO.Put("+Q\t: Use few coarse transactions instead "
                 & "of many small.\n");
        IO.Put("-p\t: Start a performance meter and log its output.\n");
        IO.Put("-batch\t: Don't show control panel, directly start "
                 & "benchmark.\n\t  Output is directed to stdout.");
        IO.Put("-help\t: Show this help text and exit.\n");
        IO.Put("-agent <host>\t: Use <host> to find GRAS-Server.\n");
        IO.Put("-cachesize <nop>\t: Use <nop> pages for client cache.\n\n");
        Process.Exit(0);
      END;

      TRY
        rootpath := Env.Get("GRAS3");
        IF pp.keywordPresent("-root") THEN rootpath := pp.getNext(); END;
        IF rootpath = NIL THEN
          ParseError(
            Pathname.Last(pp.arg[0]),
            "No information about root path.\n"
              & "Either use parameter root or environment variable GRAS3.");
        END;
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Parameter '-root' requieres an argument.");
      END;

      TRY
        cachesize := Config.DefaultCacheSize;
        IF pp.keywordPresent("-cachesize") THEN
          cachesize := pp.getNextInt(min := 0);
        END;
      EXCEPT
        ParseParams.Error =>
          ParseError(
            Pathname.Last(pp.arg[0]),
            "Parameter '-cachesize' requieres a numeric argument.");
      END;

      TRY
        nameserver := Config.DefaultNameServer;
        IF pp.keywordPresent("-agent") THEN
          nameserver := pp.getNext();
        END;
      EXCEPT
        ParseParams.Error =>
          ParseError(Pathname.Last(pp.arg[0]),
                     "Parameter '-agent' requieres an argument.");
      END;

      IF pp.keywordPresent("-N") THEN
        TRY
          txt := pp.getNext();
        EXCEPT
          ParseParams.Error =>
            ParseError(Pathname.Last(pp.arg[0]),
                       "Parameter '-N' requieres an argument.");
        END;
        IF txt # NIL THEN
          TRY
            OO1Params.N := Scan.Int(txt);
          EXCEPT
            FloatMode.Trap, Lex.Error => OO1Params.N := 0;
          END;
          IF OO1Params.N < OO1Params.MinParts
               OR OO1Params.N > OO1Params.MaxParts THEN
            ParseError(
              Pathname.Last(pp.arg[0]),
              "Number of parts must range between 500 and 2,000,000.");
          END;
        ELSE
          ParseError(
            Pathname.Last(pp.arg[0]), "Option -N requires an argument.\n");
        END;
      END;

      IF pp.keywordPresent("-T") THEN
        TRY
          txt := pp.getNext();
        EXCEPT
          ParseParams.Error =>
            ParseError(Pathname.Last(pp.arg[0]),
                       "Parameter '-T' requieres an argument.");
        END;
        IF txt # NIL THEN
          type := UpperCase(txt);
          IF Text.Equal(type, "PERSISTENT") THEN
            OO1Params.graphtype := OO1.GraphType.Persistent;
          ELSIF Text.Equal(type, "CHGMGMT") THEN
            OO1Params.graphtype := OO1.GraphType.ChgMgmt;
          ELSIF Text.Equal(type, "TYPED") THEN
            OO1Params.graphtype := OO1.GraphType.Typed;
          ELSE
            (* ignore *)
          END;
        ELSE
          ParseError(
            Pathname.Last(pp.arg[0]), "Option -T requires an argument.\n");
        END;
      END;

      IF pp.keywordPresent("-p") THEN OO1Params.PerfMeter := TRUE; END;

      IF pp.keywordPresent("-s") THEN
        OO1Params.SimpleConnects := TRUE;
      END;

      IF pp.keywordPresent("+Q") THEN OO1Params.Quick := TRUE; END;

      IF pp.keywordPresent("-B") THEN
        OO1Params.Performing :=
          OO1Params.Performing - OO1.Suite{OO1.Benchmark.Load};
      END;

      IF pp.keywordPresent("-R") THEN
        OO1Params.Performing :=
          OO1Params.Performing - OO1.Suite{OO1.Benchmark.Lookup..
                                           OO1.Benchmark.ReverseTraverse};
      END;

      IF pp.keywordPresent("-I") THEN
        OO1Params.Performing :=
          OO1Params.Performing - OO1.Suite{OO1.Benchmark.Insert};
      END;

      IF pp.keywordPresent("-batch") THEN UsePanel := FALSE; END;
    END;

  END ReadParam;


(*----- Some 'global' variables ---------------------------------------------------*)

VAR
  panel           : FormsVBT.T;
  Notifier        : MUTEX;
  Notification    : Thread.Condition;
  PerfmeterProcess: Process.T;


PROCEDURE NotifyFinish () =
  BEGIN
    Thread.Signal(Notification);
    IO.Put("Finish\n");
  END NotifyFinish;

PROCEDURE NotifyError () =
  BEGIN
    Thread.Signal(Notification);
    IO.Put("Error\n");
  END NotifyError;

BEGIN
  rootpath := NIL;
  ReadParam();
  TypedGraphSystem.Login(
      rootpath, nameserver := nameserver, cachesize := cachesize);
  IF UsePanel THEN
    panel := PanelHandling.CreatePanel(log);
    TRY
      Trestle.Install(panel, "GRAS Benchmark", "OO1");
    EXCEPT
      TrestleComm.Failure =>
        IO.Put("Can't connect to window system\n", Stdio.stderr);
    END;
    Trestle.AwaitDelete(panel);
  ELSE
    (* imediately start benchmark with given options. *)
    log.addWriter(Stdio.stdout);
    Notifier := NEW(MUTEX);
    Notification := NEW(Thread.Condition);
    OO1.Init(NotifyFinish, NotifyError);
    Thread.Pause(1.0D0);
    TRY
      log.writeTime();
      IF OO1Params.PerfMeter THEN
        TRY
          PerfmeterProcess :=
            Perfmeter.Start(
              Perfmeter.DefaultQuantities, TRUE,
              "oo1.perfmeter.log." & Fmt.Int(Process.GetMyID()));
        EXCEPT
        | OSError.E (code) =>
            log.write("Warning: Can not start performance meter! ("
                        & ErrorSupport.Fmt(code) & ")\n");
            OO1Params.PerfMeter := FALSE;
        END;
      END;
      OO1.Start(log, OO1Params.graphtype, OO1Params.local, OO1Params.Performing,
                OO1Params.N, OO1Params.SimpleConnects, OO1Params.Quick);
      LOCK Notifier DO Thread.AlertWait(Notifier, Notification) END;
      IF OO1Params.PerfMeter THEN
        Perfmeter.Stop(PerfmeterProcess);
      END;
    EXCEPT
      OO1.StateError =>
        IO.Put("OO1 complains wrong state\n", Stdio.stderr);
    | Thread.Alerted => IO.Put("Interrupt.\n");
    END;
    log.close();
  END;

END Main.
