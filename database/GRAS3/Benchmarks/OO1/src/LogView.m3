MODULE LogView;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.1  1997/02/20 16:08:43  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)
IMPORT FormsVBT, Bundle, IO, Stdio, Process, Trestle, TrestleComm,
       TypescriptVBT;
IMPORT oo1Forms, BenchmarkLog;

VAR
  LogViewOpen: BOOLEAN    := FALSE;
  logview    : FormsVBT.T;

PROCEDURE Open (log: BenchmarkLog.T) =
  BEGIN
    TRY
      Trestle.Install(logview, "GRAS Benchmark", "OO1 LogView");
      WITH view = FormsVBT.GetVBT(logview, "logview"),
           wr   = TypescriptVBT.GetWr(view)            DO
        TypescriptVBT.ClearHistory(view);
        log.addWriter(wr);
      END;
      LogViewOpen := TRUE;
    EXCEPT
      TrestleComm.Failure =>
        IO.Put("Cannot open logview.\n", Stdio.stderr);
    | FormsVBT.Error => IO.Put("Cannot open log.\n", Stdio.stderr);
    END;
  END Open;

PROCEDURE Close (log: BenchmarkLog.T) =
  BEGIN
    IF LogViewOpen THEN
      TRY
        WITH view = FormsVBT.GetVBT(logview, "logview") DO
          log.removeWriter(TypescriptVBT.GetWr(view));
        END;
      EXCEPT
        FormsVBT.Error => (* ignore *)
      END;
      Trestle.Delete(logview);
      LogViewOpen := FALSE;
    END;
  END Close;

BEGIN
  TRY
    logview :=
      NEW(FormsVBT.T).init(Bundle.Get(oo1Forms.Get(), "logview.fv"));
  EXCEPT
    FormsVBT.Error =>
      IO.Put("Error parsing S-expression of logview!\n", Stdio.stderr);
      Process.Exit(1);
  END;
END LogView.
