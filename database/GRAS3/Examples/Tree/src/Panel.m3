MODULE Panel;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:26  hosking
    Initial revision

    Revision 1.1  1998/01/21 14:23:36  roland
    Tree example demonstrates derived attributes, triggers, and user-recovery.

*)
(***************************************************************************)

IMPORT Rd, FormsVBT, Rsrc, VBT, Thread, Trestle, TrestleComm, IO, Text;
IMPORT Fmt;
IMPORT TreeBundle, Command;

VAR
  path : Rsrc.Path  := Rsrc.BuildPath(".", TreeBundle.Get());
  panel: FormsVBT.T;

PROCEDURE Open (): BOOLEAN =
  <* FATAL FormsVBT.Error, Rd.Failure, Rsrc.NotFound, Thread.Alerted *>
  BEGIN
    TRY
      panel := NEW(FormsVBT.T).initFromRsrc("TreePanel.fv", path);
      FOR c := FIRST(Command.UserCommand) TO LAST(Command.UserCommand) DO
        WITH desc = "(Button %" & Command.CommandName[c] & " \""
                      & Command.CommandName[c] & "\")" DO
          EVAL FormsVBT.Insert(panel, "buttons", desc);
          FormsVBT.AttachProc(panel, Command.CommandName[c], SignalCommand);
        END;
      END;
      FormsVBT.AttachProc(panel, "paramok", ParamOk);
      FormsVBT.AttachProc(panel, "paramcancel", ParamCancel);

      Trestle.Install(panel, "GRAS-Example: Tree");
    EXCEPT
      TrestleComm.Failure => IO.Put("Cannot open panel.\n"); RETURN FALSE;
    END;
    RETURN TRUE;
  END Open;

PROCEDURE Close () =
  BEGIN
    Trestle.Delete(panel);
  END Close;

PROCEDURE SetRoot(root: CARDINAL) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    FormsVBT.PutText(panel, "rootnode", "= " & Fmt.Int(root));
  END SetRoot; 
  
PROCEDURE SetMaxDegree(maxDegree: CARDINAL) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    FormsVBT.PutText(panel, "maxdegree", "= " & Fmt.Int(maxDegree));
  END SetMaxDegree; 
  
PROCEDURE SetLevel(level: CARDINAL) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    FormsVBT.PutText(panel, "height", "= " & Fmt.Int(level));
  END SetLevel; 
  
PROCEDURE SetNodes(nodes: CARDINAL) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    FormsVBT.PutText(panel, "nodes", "= " & Fmt.Int(nodes));
  END SetNodes; 
  
PROCEDURE SignalCommand (<* UNUSED *> fv       : FormsVBT.T;
                                      name     : TEXT;
                         <* UNUSED *> eventData: REFANY;
                         <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    FOR c := FIRST(Command.UserCommand) TO LAST(Command.UserCommand) DO
      IF Text.Equal(name, Command.CommandName[c]) THEN
        FormsVBT.MakeDormant(panel, "buttons");
        LOCK ComMutex DO
          Comm := c;
          CommandIsNew := TRUE;
          Thread.Signal(NewCommand);
        END;
        RETURN;
      END;
    END;
  END SignalCommand;

PROCEDURE ParamOk (<* UNUSED *> fv       : FormsVBT.T;
                   <* UNUSED *> name     : TEXT;
                   <* UNUSED *> eventData: REFANY;
                   <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    LOCK ComMutex DO
      CommandIsNew := TRUE;
      Cancelled := FALSE;
      Param := FormsVBT.GetInteger(panel, "param");
      Thread.Signal(NewCommand);
      FormsVBT.PopDown(panel, "paraminput");
    END;
  END ParamOk;

PROCEDURE ParamCancel (<* UNUSED *> fv       : FormsVBT.T;
                       <* UNUSED *> name     : TEXT;
                       <* UNUSED *> eventData: REFANY;
                       <* UNUSED *> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    LOCK ComMutex DO
      CommandIsNew := TRUE;
      Cancelled := TRUE;
      Thread.Signal(NewCommand);
      FormsVBT.PopDown(panel, "paraminput");
    END;
  END ParamCancel;

VAR
  ComMutex                          := NEW(MUTEX);
  NewCommand                        := NEW(Thread.Condition);
  Comm        : Command.UserCommand;
  Param       : CARDINAL;
  CommandIsNew: BOOLEAN;
  Cancelled   : BOOLEAN;

PROCEDURE QueryCommand (): Command.UserCommand =
  VAR res: Command.UserCommand;
  <* FATAL FormsVBT.Error *>
  BEGIN
    FormsVBT.MakeActive(panel, "buttons");
    LOCK ComMutex DO
      IF NOT CommandIsNew THEN Thread.Wait(ComMutex, NewCommand); END;
      CommandIsNew := FALSE;
      res := Comm;
    END;
    RETURN res;
  END QueryCommand;

PROCEDURE QueryParameter (name: TEXT; VAR res: CARDINAL; VAR ok: BOOLEAN) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    FormsVBT.PutText(panel, "paramtext", name);
    FormsVBT.PopUp(panel, "paraminput");
    LOCK ComMutex DO
      IF NOT CommandIsNew THEN Thread.Wait(ComMutex, NewCommand); END;
      CommandIsNew := FALSE;
      res := Param;
      ok := NOT Cancelled;
    END;
  END QueryParameter;

BEGIN
END Panel.
