MODULE RuleClient;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.2  1997/11/03 12:40:37  roland
    New procedures to check connection to rule server.

    Revision 1.1  1997/10/31 14:05:17  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. The remote rule handler
    connects to a special server object to communicate with other rule engines.

*)
(***************************************************************************)

IMPORT NetObj, Thread, IntIntTbl, IntTextTbl, TextSeq;
IMPORT RuleEngineCallback, Event, Action, ContextSet, Trigger,
       RuleEngineServer;
IMPORT EventTranslation, RemoteTriggerStorage, TriggerMap;
IMPORT AtomList, Atom;

VAR
  Server   : RuleEngineServer.T;
  ClientId : CARDINAL;
  Connected: BOOLEAN            := FALSE;
  LastError: TEXT               := "Connection not requested.";


PROCEDURE Msg(err: AtomList.T): TEXT =
  VAR res: TEXT := "";
  BEGIN
    FOR i := 0 TO AtomList.Length(err)-1 DO
      res := res & " " & Atom.ToText(AtomList.Nth(err, i));
    END;
    RETURN res;
  END Msg;
  
PROCEDURE Connect (    host, id: TEXT;
                       callback: RuleEngineCallback.T;
                   VAR ok      : BOOLEAN;
                   VAR error   : TEXT                  ) =
  (* Tries to connect to RuleServer via name server on host *)
  VAR agent: NetObj.Address;
  BEGIN
    IF NOT Connected THEN
      ok := FALSE;
      (* 1.  Find name server *)
      TRY
        agent := NetObj.Locate(host);
      EXCEPT
        NetObj.Error =>
          LastError := "Cannot connect: netobjd running?";
          error := LastError;
          RETURN;
      | NetObj.Invalid =>
          LastError := "Cannot connect: invalid hostname '" & host & "'";
          error := LastError;
          RETURN;
      | Thread.Alerted =>
          LastError := "Cannot connect: thread alerted.";
          error := LastError;
          RETURN;
      END;

      (* find rule server *)
      TRY
        Server :=
          NetObj.Import(RuleEngineServer.ComposeServerId(id), agent);
      EXCEPT
        NetObj.Error =>
          LastError := "Cannot import: communication error";
          error := LastError;
          RETURN;
      | Thread.Alerted => error := "Cannot import: thread alerted"; RETURN;
      END;
      IF Server = NIL THEN
        LastError := "RuleServer with id '" & id & "' not running.";
        error := LastError;
        RETURN;
      END;

      (* register with rule server *)
      TRY
        ClientId := Server.register(callback);
      EXCEPT
        NetObj.Error (e) =>
          LastError := "Cannot register: " & Msg(e);
          error := LastError;
          RETURN;
      | Thread.Alerted =>
          LastError := "Cannot register: thread alerted";
          error := LastError;
          RETURN;
      END;

      ok := TRUE;
      Connected := TRUE;
      LastError := "No error";
      error := LastError;
    ELSE
      ok := TRUE;
      error := "Already connected";
    END;
  END Connect;

PROCEDURE Disconnect () =
  (* Unregisters client from rule server *)
  BEGIN
    IF Connected THEN
      Connected := FALSE;
      LastError := "Explicitly disconnected.";
      TRY
        Server.unregister(ClientId);
      EXCEPT
        NetObj.Error =>          (* ignore *)
      | Thread.Alerted =>        (* ignore *)
      END;
    END;
  END Disconnect;

PROCEDURE CheckConnection (VAR connected: BOOLEAN; VAR msg: TEXT) =
  BEGIN
    IF Connected THEN
      TRY
        Server.ping();
      EXCEPT
        NetObj.Error (e) => Connected := FALSE; LastError := Msg(e);
      | Thread.Alerted =>
          Connected := FALSE;
          LastError := "Server alerted!";
      END;
    END;
    connected := Connected;
    msg := LastError;
  END CheckConnection;

PROCEDURE RegisterTrigger (trigger : Trigger.T;
                           userdata: REFANY;
                           id      : CARDINAL   ) RAISES {CommError} =
  (* Registers trigger with the rule server.  The id identifies the trigger
     locally. *)
  VAR
    type       : TEXT;
    bools, ints: IntIntTbl.T;
    texts      : IntTextTbl.T;
    coupling   : CARDINAL;
    inh, perm  : TextSeq.T;
    remoteId   : CARDINAL;
  BEGIN
    IF Connected THEN
      EventTranslation.DecomposePattern(
        trigger.pattern(), type, bools, ints, texts);
      inh := ContextSet.ToSeq(trigger.inhibiting());
      perm := ContextSet.ToSeq(trigger.permitting());
      CASE trigger.coupling() OF
        Trigger.CouplingMode.Immediate =>
          coupling := RuleEngineCallback.ImmediateCoupling;
      | Trigger.CouplingMode.Deferred =>
          coupling := RuleEngineCallback.DeferredCoupling;
      | Trigger.CouplingMode.Decoupled =>
          coupling := RuleEngineCallback.DecoupledCoupling;
      END;
      TRY
        remoteId :=
          Server.registerTrigger(ClientId, type, bools, ints, texts,
                                 coupling, trigger.priority(), inh, perm);
      EXCEPT
        NetObj.Error (e) =>
          Connected := FALSE;
          LastError := Msg(e);
          RAISE CommError;
      | Thread.Alerted =>
          Connected := FALSE;
          LastError := "Thread alerted";
          RAISE CommError;
      END;
      TriggerMap.Bind(id, remoteId);
      RemoteTriggerStorage.Store(
        id, type, trigger.priority(), trigger.action(), userdata);
    ELSE
      RAISE CommError;
    END;
  END RegisterTrigger;

PROCEDURE UnregisterTrigger (id: CARDINAL) RAISES {CommError} =
  (* Unregisters trigger with id *)
  VAR remoteId: CARDINAL;
  BEGIN
    IF Connected THEN
      RemoteTriggerStorage.Remove(id);
      IF TriggerMap.GetRemote(id, remoteId) THEN
        TRY
          Server.unregisterTrigger(ClientId, remoteId);
        EXCEPT
          NetObj.Error (e) =>
            Connected := FALSE;
            LastError := Msg(e);
            RAISE CommError;
        | Thread.Alerted =>
            Connected := FALSE;
            LastError := "Thread alerted";
            RAISE CommError;
        END;
        TriggerMap.RemoveWithLocal(id);
      END;
    ELSE
      RAISE CommError;
    END;
  END UnregisterTrigger;

PROCEDURE SendAction (event  : Event.T;
                      context: ContextSet.T;
                      action : Action.T      ) RAISES {CommError} =
  (* Report action to server *)
  VAR
    type       : TEXT;
    bools, ints: IntIntTbl.T;
    texts      : IntTextTbl.T;
    contSeq    : TextSeq.T;
    remoteId   : CARDINAL;
  BEGIN
    IF Connected THEN
      IF ISTYPE(action, Action.Remote) THEN
        remoteId := NARROW(action, Action.Remote).trig();
        EventTranslation.DecomposeEvent(event, type, bools, ints, texts);
        contSeq := ContextSet.ToSeq(context);
        TRY
          Server.reportEvent(
            ClientId, remoteId, bools, ints, texts, contSeq);
        EXCEPT
          NetObj.Error (e) =>
            Connected := FALSE;
            LastError := Msg(e);
            RAISE CommError;
        | Thread.Alerted =>
            Connected := FALSE;
            LastError := "Thread alerted";
            RAISE CommError;
        END;
      END;
    ELSE
      RAISE CommError;
    END;
  END SendAction;

BEGIN
END RuleClient.
