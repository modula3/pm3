MODULE RuleMonitor;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.2  1997/11/12 17:24:18  roland
    Start/Stop fixed. The listener really gets killed when stopping the
    monitor.

    Revision 1.1  1997/11/07 08:58:15  roland
    It is possible to edit event patterns for the monitored event
    types. Additionally, information about event types can be displayed.

*)
(***************************************************************************)

IMPORT Action, RuleEngine, EventType, EventTypes, Trigger, ContextSet,
       Event, EventPattern, MonitoredTypes, Patterns;
IMPORT Wr, Stdio, Fmt, IntSeq, Thread;

FROM Trigger IMPORT CouplingMode;
FROM RuleEngine IMPORT Interest;

VAR
  triggers    : IntSeq.T;
  etypes      : IntSeq.T;
  writer      : Wr.T;
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
      Thread.Alerted =>          (* stop *)
    END;
    RETURN NIL;
  END Listen;

PROCEDURE InstallMonitor (wr      : Wr.T             := NIL;
                          interest: Interest         := Interest.All;
                          actproc : Action.Procedure := NIL;
                          coupling           := CouplingMode.Immediate;
                          priority: CARDINAL := LAST(CARDINAL)          ) =
  VAR
    pattern  : EventPattern.T;
    trigger  : Trigger.T;
    action   : Action.T;
    inh, perm                 := ContextSet.Empty();
  BEGIN
    IF wr = NIL THEN
      writer := Stdio.stdout;
      IF listener # NIL THEN StopListener := TRUE; END;
    ELSE
      writer := wr;
      IF listener = NIL THEN
        StopListener := FALSE;
        listener := Thread.Fork(NEW(Thread.Closure, apply := Listen));
      END;
    END;

    etypes := MonitoredTypes.Get();

    IF actproc = NIL THEN actproc := ActionProc; END;
    action := NEW(Action.Local).init(actproc);

    IF triggers # NIL THEN UninstallMonitor() END;
    triggers := NEW(IntSeq.T).init(etypes.size());

    (* install triggers for all required types *)
    FOR i := 0 TO etypes.size() - 1 DO
      pattern := Patterns.Get(etypes.get(i));
      trigger :=
        Trigger.Create(pattern, action, coupling, priority, inh, perm);
      triggers.addhi(RuleEngine.RegisterTrigger(trigger, interest, NIL));
    END;
  END InstallMonitor;

PROCEDURE UninstallMonitor () =
  BEGIN
    IF triggers # NIL THEN
      FOR i := 0 TO triggers.size() - 1 DO
        RuleEngine.UnregisterTrigger(triggers.get(i));
      END;
      IF listener # NIL THEN
        StopListener := TRUE;
        Thread.Alert(listener);
        EVAL Thread.Join(listener);
      END;
      listener := NIL;
      triggers := NIL;
    END;
  END UninstallMonitor;

PROCEDURE ActionProc (             event  : Event.T;
                                   context: ContextSet.T;
                                   local  : BOOLEAN;
                      <* UNUSED *> data   : REFANY        ) =
  CONST LocalText = ARRAY BOOLEAN OF TEXT{"remote ", "local "};
  VAR
    type    : EventType.T;
    attrType: TEXT;
    attrVal : TEXT;
    conText : TEXT;
  <* FATAL EventTypes.Unknown, EventType.Unknown, EventType.Mismatch *>
  BEGIN
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
  END ActionProc;

PROCEDURE Write (t: TEXT) =
  BEGIN
    TRY
      Wr.PutText(writer, t);
      Wr.Flush(writer);
    EXCEPT
      Wr.Failure, Thread.Alerted => (* ignore *)
    END;
  END Write;

BEGIN
END RuleMonitor.
