MODULE InfoPanel;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1997/11/07 08:58:07  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

*)
(***************************************************************************)

IMPORT Rd, Wr, Trestle, FormsVBT, Rsrc, TypescriptVBT, VBT, TrestleComm,
       Thread;
IMPORT MonitorBundle;

VAR
  vbt       : FormsVBT.T;
  open      : BOOLEAN         := FALSE;
  path      : Rsrc.Path       := Rsrc.BuildPath(".", MonitorBundle.Get());
  infowr, attrwr: Wr.T;

PROCEDURE Open () =
  <* FATAL FormsVBT.Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted *>
  <* FATAL TrestleComm.Failure *>
  BEGIN
    IF NOT open THEN
      vbt := NEW(FormsVBT.T).initFromRsrc("InfoView.fv", path);
      WITH typescript = FormsVBT.GetVBT(vbt, "infoscript") DO
        TypescriptVBT.ClearHistory(typescript);
        infowr := TypescriptVBT.GetWr(typescript);
      END;
      WITH typescript = FormsVBT.GetVBT(vbt, "attrscript") DO
        TypescriptVBT.ClearHistory(typescript);
        attrwr := TypescriptVBT.GetWr(typescript);
      END;
      open := TRUE;
      FormsVBT.AttachProc(vbt, "close", Close);
      Trestle.Install(vbt, "RuleMonitor", "Info");
    ELSE
      WITH typescript = FormsVBT.GetVBT(vbt, "infoscript") DO
        TypescriptVBT.ClearHistory(typescript);
        infowr := TypescriptVBT.GetWr(typescript);
      END;
      WITH typescript = FormsVBT.GetVBT(vbt, "attrscript") DO
        TypescriptVBT.ClearHistory(typescript);
        attrwr := TypescriptVBT.GetWr(typescript);
      END;
    END;      
  END Open;

PROCEDURE PutType(t: TEXT) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    IF open THEN
      FormsVBT.PutText(vbt, "typename", "Event type " & t);
    END;
  END PutType; 
  
PROCEDURE PutDescription (t: TEXT) =
  BEGIN
    IF open THEN
      TRY
        Wr.PutText(infowr, t);
        Wr.Flush(infowr);
      EXCEPT
        Wr.Failure, Thread.Alerted => (* ignore *)
      END;
    END;
  END PutDescription; 

PROCEDURE PutAttributes (t: TEXT) =
  BEGIN
    IF open THEN
      TRY
        Wr.PutText(attrwr, t);
        Wr.Flush(attrwr);
      EXCEPT
        Wr.Failure, Thread.Alerted => (* ignore *)
      END;
    END;
  END PutAttributes; 

PROCEDURE Close (<* UNUSED *> fv       : FormsVBT.T;
                 <* UNUSED *> name     : TEXT;
                 <* UNUSED *> eventData: REFANY;
                 <* UNUSED *> time     : VBT.TimeStamp) =
  BEGIN
    Trestle.Delete(vbt);
    open := FALSE;
  END Close;


BEGIN
END InfoPanel.
