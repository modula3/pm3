MODULE ChartView;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.2  1997/11/07 08:58:04  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

    Revision 1.1  1997/10/31 14:28:27  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

IMPORT Rd, Trestle, FormsVBT, Rsrc, ChartVBT, VBT, TrestleComm, Thread;
IMPORT Views, MonitorBundle;

VAR
  vbt       : FormsVBT.T;
  open      : BOOLEAN         := FALSE;
  path      : Rsrc.Path       := Rsrc.BuildPath(".", MonitorBundle.Get());
  rects: ChartVBT.T;
  NotifyClose: PROCEDURE();

PROCEDURE Open (no: CARDINAL; notifyClose: PROCEDURE()): ChartVBT.T =
  <* FATAL FormsVBT.Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted *>
  <* FATAL TrestleComm.Failure *>
  BEGIN
    IF NOT open THEN      
      vbt := NEW(FormsVBT.T).initFromRsrc("MonitorView.fv", path);
      rects := NEW(ChartVBT.T).init(no);
      FormsVBT.InsertVBT(vbt, "viewbox", rects, 0);
      open := TRUE;
      FormsVBT.AttachProc(vbt, "viewclose", Close);
      FormsVBT.AttachProc(vbt, "viewok", ReallyClose);
      FormsVBT.AttachProc(vbt, "viewabort", DontClose);
      Trestle.Install(vbt, "RuleMonitor", "ChartView");
      Views.AddView();
      NotifyClose := notifyClose;
    END;
    RETURN rects;
  END Open;

PROCEDURE Close (<* UNUSED *> fv       : FormsVBT.T;
                 <* UNUSED *> name     : TEXT;
                 <* UNUSED *> eventData: REFANY;
                 <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    IF Views.Number() = 1 THEN
      FormsVBT.PopUp(vbt, "viewwarn");
    ELSE
      Trestle.Delete(vbt);
      open := FALSE;
      Views.RemoveView();
      NotifyClose();
    END;
  END Close;

PROCEDURE DontClose (<* UNUSED *> fv       : FormsVBT.T;
                     <* UNUSED *> name     : TEXT;
                     <* UNUSED *> eventData: REFANY;
                     <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    FormsVBT.PopDown(vbt, "viewwarn");
  END DontClose;

PROCEDURE ReallyClose (<* UNUSED *> fv       : FormsVBT.T;
                       <* UNUSED *> name     : TEXT;
                       <* UNUSED *> eventData: REFANY;
                       <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    Trestle.Delete(vbt);
    open := FALSE;
    Views.RemoveView();
    NotifyClose();
  END ReallyClose;

BEGIN
END ChartView.
