MODULE TextView;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:28:32  roland
    Graphical front end for rule monitoring.

*)
(***************************************************************************)

IMPORT Rd, Wr, Trestle, FormsVBT, Rsrc, TypescriptVBT, VBT, TrestleComm, Thread;
IMPORT Views, MonitorBundle;

VAR
  vbt       : FormsVBT.T;
  open      : BOOLEAN         := FALSE;
  path      : Rsrc.Path       := Rsrc.BuildPath(".", MonitorBundle.Get());
  typescript: TypescriptVBT.T;
  NotifyClose: PROCEDURE();

PROCEDURE Open (notifyClose: PROCEDURE()): Wr.T =
  CONST script = "(Typescript ReadOnly)";
  <* FATAL FormsVBT.Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted *>
  <* FATAL TrestleComm.Failure *>
  BEGIN
    IF NOT open THEN      
      vbt := NEW(FormsVBT.T).initFromRsrc("MonitorView.fv", path);
      typescript := FormsVBT.Insert(vbt, "viewbox", script, 0);
      open := TRUE;
      FormsVBT.AttachProc(vbt, "viewclose", Close);
      FormsVBT.AttachProc(vbt, "viewok", ReallyClose);
      FormsVBT.AttachProc(vbt, "viewabort", DontClose);
      Trestle.Install(vbt, "RuleMonitor", "TextView");
      Views.AddView();
      NotifyClose := notifyClose;
    END;
    RETURN TypescriptVBT.GetWr(typescript);
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
END TextView.
