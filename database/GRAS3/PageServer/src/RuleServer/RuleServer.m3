MODULE RuleServer;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:49  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:38  hosking
    Import of GRAS3 1.1

    Revision 1.2  1997/11/03 12:40:20  roland
    New procedures to check connection to rule server.

    Revision 1.1  1997/10/31 14:16:41  roland
    RuleServer implements communication between different rule engines.

*)
(***************************************************************************)

IMPORT Thread, NetObj;
IMPORT IntIntTransientTbl AS IntIntTbl,
       IntTextTransientTbl AS IntTextTbl,
       TextTransientSeq AS TextSeq,
       IntTransientSeq AS IntSeq;
IMPORT RuleEngineCallback, RuleEngineServer, IntCallbackTbl, Variant;
IMPORT Fmt, Journal;

TYPE
  Server =
    RuleEngineServer.T BRANDED OBJECT
      lock: MUTEX;
      clients: IntCallbackTbl.T;  (* map client ids to their callback
                                     ports *)
      deadClients     : IntSeq.T;
      hasDeadClients  : Thread.Condition;
      cleaner         : Thread.T;
      triggerClientMap: IntIntTbl.T;       (* map trigger ids to clients *)
      triggerList: RemoteTrigger;
      notifier   : Thread.T;
      eventBuffer: EventBuffer;
      nextClient : CARDINAL;
      nextTrigger: CARDINAL;
    METHODS
      init     (): Server := InitServer;
      shutDown ()         := ShutdownServer;
    OVERRIDES
      register          := RegisterClient;
      unregister        := UnregisterClient;
      registerTrigger   := RegisterTrigger;
      unregisterTrigger := UnregisterTrigger;
      reportEvent       := ReportEvent;
      ping              := Ping;
    END;


PROCEDURE InitServer (server: Server): Server =
  BEGIN
    server.lock := NEW(MUTEX);
    server.clients := NEW(IntCallbackTbl.Default).init();

    server.deadClients := NEW(IntSeq.T).init();
    server.hasDeadClients := NEW(Thread.Condition);
    WITH cl = NEW(CleanerClosure, server := server) DO
      server.cleaner := Thread.Fork(cl);
    END;

    server.triggerClientMap := NEW(IntIntTbl.Default).init();

    EventBufferInit(server.eventBuffer);
    WITH cl = NEW(NotifierClosure, server := server) DO
      server.notifier := Thread.Fork(cl);
    END;
    server.nextClient := 1;
    server.nextTrigger := 1;
    RETURN server;
  END InitServer;

PROCEDURE ShutdownServer (<* UNUSED *> server: Server) =
  BEGIN
    IF Variant.TestRuleServer THEN
      Journal.Add("RuleServer: Rule server shutdown ok.");
    END;
  END ShutdownServer;

PROCEDURE RegisterClient (server: Server; callback: RuleEngineCallback.T):
  CARDINAL RAISES {} =
  VAR id: CARDINAL;
  BEGIN
    LOCK server.lock DO
      id := server.nextClient;
      INC(server.nextClient);
      EVAL server.clients.put(id, callback);
      IF Variant.TestRuleServer THEN
        Journal.Add("RuleServer: New client no " & Fmt.Int(id));
      END;
      PropagateTriggers(server, id);
    END;
    RETURN id;
  END RegisterClient;

PROCEDURE UnregisterClient (server: Server; client: CARDINAL) RAISES {} =
  BEGIN
    LOCK server.lock DO CollectDeadClient(server, client); END;
  END UnregisterClient;

PROCEDURE RegisterTrigger (server   : Server;
                           client   : CARDINAL;
                           pType    : TEXT;
                           pBools   : IntIntTbl.T;
                           pInts    : IntIntTbl.T;
                           pTexts   : IntTextTbl.T;
                           coupling : CARDINAL;
                           priority : CARDINAL;
                           inh, perm: TextSeq.T     ): CARDINAL RAISES {} =
  VAR
    cb: RuleEngineCallback.T;
    id: CARDINAL;
  BEGIN
    LOCK server.lock DO
      IF server.clients.get(client, cb) THEN
        id := server.nextTrigger;
        INC(server.nextTrigger);
        EVAL server.triggerClientMap.put(id, client);
        IF Variant.TestRuleServer THEN
          Journal.Add("RuleServer: New trigger no " & Fmt.Int(id)
                        & " received from client " & Fmt.Int(client));
        END;
        RemoteTriggerListInsert(
          server.triggerList, id, client, pType, pBools, pInts, pTexts,
          coupling, priority, inh, perm);
        BroadcastTrigger(server, id, client, pType, pBools, pInts, pTexts,
                         coupling, priority, inh, perm);
      END;
    END;
    RETURN id;
  END RegisterTrigger;

PROCEDURE UnregisterTrigger (server: Server; client, trigger: CARDINAL)
  RAISES {} =
  VAR
    cb        : RuleEngineCallback.T;
    realClient: INTEGER;
  BEGIN
    LOCK server.lock DO
      IF server.clients.get(client, cb) THEN
        IF server.triggerClientMap.get(trigger, realClient) THEN
          IF client = realClient THEN
            IF Variant.TestRuleServer THEN
              Journal.Add(
                "RuleServer: Killing trigger no " & Fmt.Int(trigger));
            END;
            EVAL server.triggerClientMap.delete(trigger, realClient);
            RemoteTriggerListRemove(server.triggerList, trigger);
            BroadcastRemoveTrigger(server, trigger, client);
          END;
        END;
      END;
    END;
  END UnregisterTrigger;

(* Observed events *)

PROCEDURE ReportEvent (server : Server;
                       client : CARDINAL;
                       trigger: CARDINAL;
                       eBools : IntIntTbl.T;
                       eInts  : IntIntTbl.T;
                       eTexts : IntTextTbl.T;
                       context: TextSeq.T     ) RAISES {} =
  BEGIN
    LOCK server.lock DO
      IF Variant.TestRuleServer THEN
        Journal.Add(
          "RuleServer: Received event for trigger" & Fmt.Int(trigger)
            & " from client " & Fmt.Int(client));
      END;
      EventBufferPut(server.eventBuffer, client, trigger, eBools, eInts,
                     eTexts, context);
    END;
    Thread.Signal(server.eventBuffer.notEmpty);
  END ReportEvent;

PROCEDURE Ping (<* UNUSED *> server: Server) =
  BEGIN
  END Ping;

VAR
  ServerObj: Server;
  ServerId : TEXT;
  AgentHost: TEXT;

PROCEDURE Setup (host, id: TEXT) RAISES {Failure} =
  VAR netobjd: NetObj.Address;
  BEGIN
    TRY
      netobjd := NetObj.Locate(host);
    EXCEPT
      NetObj.Error => RAISE Failure("Cannot connect: netobjd running?");
    | NetObj.Invalid =>
        RAISE Failure("Cannot connect: invalid hostname '" & host & "'");
    | Thread.Alerted => RAISE Failure("Cannot connect: thread alerted.");
    END;
    AgentHost := host;

    ServerObj := NEW(Server).init();
    TRY
      NetObj.Export(
        RuleEngineServer.ComposeServerId(id), ServerObj, netobjd);
    EXCEPT
      NetObj.Error => RAISE Failure("Cannot export: communication error");
    | Thread.Alerted => RAISE Failure("Cannot export: thread alerted");
    END;
    ServerId := id;
  END Setup;

PROCEDURE Shutdown () =
  VAR netobjd: NetObj.Address;
  BEGIN
    TRY
      netobjd := NetObj.Locate(AgentHost);
      NetObj.Export("RuleServer " & ServerId, NIL, netobjd);
    EXCEPT
      NetObj.Error =>            (* ignore *)
    | NetObj.Invalid =>          (* ignore *)
    | Thread.Alerted =>          (* ignore *)
    END;

    ServerObj.shutDown();
  END Shutdown;

(* internal *)

(* EventBuffer implementation *)

TYPE
  EventBuffer = RECORD
                  notEmpty   : Thread.Condition;
                  first, last: BufferedEvent;
                END;

  BufferedEvent = <*TRANSIENT*> REF BufferedEventRec;
  BufferedEventRec = RECORD
                    client    : CARDINAL;
                    trigger   : CARDINAL;
                    eBools    : IntIntTbl.T;
                    eInts     : IntIntTbl.T;
                    eTexts    : IntTextTbl.T;
                    context   : TextSeq.T;
                    next, prev: BufferedEvent;
                  END;

PROCEDURE EventBufferInit (VAR buf: EventBuffer) =
  BEGIN
    buf.notEmpty := NEW(Thread.Condition);
  END EventBufferInit;

PROCEDURE EventBufferPut (VAR buf    : EventBuffer;
                              client : CARDINAL;
                              trigger: CARDINAL;
                              eBools : IntIntTbl.T;
                              eInts  : IntIntTbl.T;
                              eTexts : IntTextTbl.T;
                              context: TextSeq.T     ) =
  VAR ev := NewBufferedEvent();
  BEGIN
    ev^ := BufferedEventRec{
             client, trigger, eBools, eInts, eTexts, context, NIL, NIL};
    (* insert at end of list *)
    IF buf.last = NIL THEN
      (* buffer empty *)
      buf.first := ev;
      buf.last := ev;
    ELSE
      ev.prev := buf.last;
      buf.last.next := ev;
      buf.last := ev;
    END;
  END EventBufferPut;

PROCEDURE EventBufferEmpty (READONLY buf: EventBuffer): BOOLEAN =
  BEGIN
    RETURN buf.first = NIL;
  END EventBufferEmpty;

PROCEDURE EventBufferGet (VAR buf    : EventBuffer;
                          VAR client : CARDINAL;
                          VAR trigger: CARDINAL;
                          VAR eBools : IntIntTbl.T;
                          VAR eInts  : IntIntTbl.T;
                          VAR eTexts : IntTextTbl.T;
                          VAR context: TextSeq.T     ) =
  VAR ev: BufferedEvent;
  BEGIN
    ev := buf.first;
    buf.first := ev.next;
    IF buf.first # NIL THEN
      buf.first.prev := NIL;
    ELSE
      buf.last := NIL;
    END;
    client := ev.client;
    trigger := ev.trigger;
    eBools := ev.eBools;
    eInts := ev.eInts;
    eTexts := ev.eTexts;
    context := ev.context;
    DisposeBufferedEvent(ev);
  END EventBufferGet;

PROCEDURE EventBufferDeleteTrigger (VAR buf: EventBuffer; trigger: CARDINAL) =
  VAR h, d: BufferedEvent;
  BEGIN
    (* loop through the list and remove any entries for trigger *)
    h := buf.first;
    WHILE h # NIL DO
      IF h.trigger = trigger THEN
        d := h;
        IF h.next = NIL THEN
          buf.last := h.prev;
        ELSE
          h.next.prev := h.prev;
        END;
        IF h.prev = NIL THEN
          buf.first := h.next;
        ELSE
          h.prev.next := h.next;
        END;
        h := h.next;
        DisposeBufferedEvent(d);
      ELSE
        h := h.next;
      END;
    END;
  END EventBufferDeleteTrigger;

VAR FreeBufferedEvents: BufferedEvent;

PROCEDURE NewBufferedEvent (): BufferedEvent =
  VAR ev: BufferedEvent;
  BEGIN
    IF FreeBufferedEvents = NIL THEN
      RETURN NEW(BufferedEvent);
    ELSE
      ev := FreeBufferedEvents;
      FreeBufferedEvents := ev.next;
      RETURN ev;
    END;
  END NewBufferedEvent;

PROCEDURE DisposeBufferedEvent (ev: BufferedEvent) =
  BEGIN
    ev^ := BufferedEventRec{0, 0, NIL, NIL, NIL, NIL, NIL, NIL};
    ev.next := FreeBufferedEvents;
    FreeBufferedEvents := ev;
  END DisposeBufferedEvent;


(* Remote trigger list implementation *)

TYPE
  RemoteTrigger = <*TRANSIENT*> REF RemoteTriggerRec;
  RemoteTriggerRec = RECORD
                    trigger, client   : CARDINAL;
                    <*TRANSIENT*> type: TEXT;
                    bools, ints       : IntIntTbl.T;
                    texts             : IntTextTbl.T;
                    coupling, priority: CARDINAL;
                    inh, perm         : TextSeq.T;
                    next, prev        : RemoteTrigger;
                  END;


PROCEDURE RemoteTriggerListInsert (VAR list           : RemoteTrigger;
                                       trigger, client: CARDINAL;
                                       pType          : TEXT;
                                       pBools, pInts  : IntIntTbl.T;
                                       pTexts         : IntTextTbl.T;
                                   coupling, priority: CARDINAL;
                                   inh, perm         : TextSeq.T ) =
  VAR rt := NewRemoteTrigger();
  BEGIN
    rt^ :=
      RemoteTriggerRec{trigger := trigger, client := client, type := pType,
                       bools := pBools, ints := pInts, texts := pTexts,
                       coupling := coupling, priority := priority, inh := inh,
                       perm := perm, next := NIL, prev := NIL};
    rt.next := list;
    IF list # NIL THEN list.prev := rt; END;
    list := rt;
  END RemoteTriggerListInsert;

PROCEDURE RemoteTriggerListRemove (VAR list: RemoteTrigger;
                                       id  : CARDINAL           ) =
  VAR h: RemoteTrigger;
  BEGIN
    h := list;
    WHILE h # NIL AND h.trigger # id DO h := h.next END;
    IF h # NIL THEN
      IF h.prev = NIL THEN list := h.next; ELSE h.prev.next := h.next; END;
      IF h.next # NIL THEN h.next.prev := h.prev; END;
      DisposeRemoteTrigger(h);
    END;
  END RemoteTriggerListRemove;

PROCEDURE RemoteTriggerListDeleteClient (VAR list  : RemoteTrigger;
                                             client: CARDINAL           ) =
  VAR h, d: RemoteTrigger;
  BEGIN
    h := list;
    WHILE h # NIL DO
      IF h.client = client THEN
        d := h;
        IF h.next # NIL THEN h.next.prev := h.prev; END;
        IF h.prev = NIL THEN
          list := h.next;
        ELSE
          h.prev.next := h.next;
        END;
        h := h.next;
        DisposeRemoteTrigger(d);
      ELSE
        h := h.next;
      END;
    END;
  END RemoteTriggerListDeleteClient;

PROCEDURE RemoteTriggerListLast (list: RemoteTrigger):
  RemoteTrigger =
  VAR h: RemoteTrigger;
  BEGIN
    IF list = NIL THEN RETURN NIL END;
    h := list;
    WHILE h.next # NIL DO h := h.next; END;
    RETURN h;
  END RemoteTriggerListLast;

PROCEDURE RemoteTriggerListPrev (elem: RemoteTrigger):
  RemoteTrigger =
  BEGIN
    RETURN elem.prev;
  END RemoteTriggerListPrev;

VAR FreeRemoteTriggers: RemoteTrigger;

PROCEDURE NewRemoteTrigger (): RemoteTrigger =
  VAR rt: RemoteTrigger;
  BEGIN
    IF FreeRemoteTriggers = NIL THEN
      RETURN NEW(RemoteTrigger);
    ELSE
      rt := FreeRemoteTriggers;
      FreeRemoteTriggers := rt.next;
      RETURN rt;
    END;
  END NewRemoteTrigger;

PROCEDURE DisposeRemoteTrigger (rt: RemoteTrigger) =
  BEGIN
    rt^ :=
      RemoteTriggerRec{0, 0, NIL, NIL, NIL, NIL, 0, 0, NIL, NIL, NIL, NIL};
    rt.next := FreeRemoteTriggers;
    FreeRemoteTriggers := rt;
  END DisposeRemoteTrigger;

(* event notifier thread implementation *)

TYPE
  NotifierClosure = Thread.Closure OBJECT
                      server: Server;
                    OVERRIDES
                      apply := NotifierApply;
                    END;

PROCEDURE NotifierApply (n: NotifierClosure): REFANY =
  VAR
    client  : CARDINAL;
    trigger : CARDINAL;
    receiver: INTEGER;
    eBools  : IntIntTbl.T;
    eInts   : IntIntTbl.T;
    eTexts  : IntTextTbl.T;
    context : TextSeq.T;
    cb      : RuleEngineCallback.T;
    found   : BOOLEAN;
  BEGIN
    TRY
      LOOP
        LOCK n.server.lock DO
          (* get next event from buffer *)
          WHILE EventBufferEmpty(n.server.eventBuffer) DO
            Thread.AlertWait(n.server.lock, n.server.eventBuffer.notEmpty);
          END;
          EventBufferGet(n.server.eventBuffer, client, trigger, eBools,
                         eInts, eTexts, context);
          (* determine client to notify *)
          IF n.server.triggerClientMap.get(trigger, receiver) THEN
            found := n.server.clients.get(receiver, cb);
          ELSE
            found := FALSE;
          END;
        END;

        (* notify receiver of event occurrence *)
        TRY
          IF found THEN
            IF Variant.TestRuleServer THEN
              Journal.Add(
                "RuleServer: Notifying client " & Fmt.Int(receiver)
                  & " about fired trigger " & Fmt.Int(trigger)
                  & " from client" & Fmt.Int(client));
            END;
            cb.notifyEvent(trigger, client, eBools, eInts, eTexts, context);
          END;
        EXCEPT
          Thread.Alerted, NetObj.Error =>
            CollectDeadClient(n.server, receiver);
        END;
      END;
    EXCEPT
      Thread.Alerted => RETURN NIL;
    END;
  END NotifierApply;

PROCEDURE BroadcastTrigger (server            : Server;
                            trigger, client   : CARDINAL;
                            pType             : TEXT;
                            pBools, pInts     : IntIntTbl.T;
                            pTexts            : IntTextTbl.T;
                            coupling, priority: CARDINAL;
                            inh, perm         : TextSeq.T     ) =
  VAR
    cb : RuleEngineCallback.T;
    rec: INTEGER;
  BEGIN
    WITH it = server.clients.iterate() DO
      WHILE it.next(rec, cb) DO
        TRY
          IF client # rec THEN
            IF Variant.TestRuleServer THEN
              Journal.Add("RuleServer: Notifying client " & Fmt.Int(rec)
                            & " about new trigger " & Fmt.Int(trigger)
                            & " from client" & Fmt.Int(client));
            END;
            cb.registerTrigger(trigger, client, pType, pBools, pInts,
                               pTexts, coupling, priority, inh, perm);
          END;
        EXCEPT
          NetObj.Error, Thread.Alerted => CollectDeadClient(server, rec)
        END;
      END
    END;
  END BroadcastTrigger;

PROCEDURE BroadcastRemoveTrigger (server: Server; trigger, client: CARDINAL) =
  VAR
    cb : RuleEngineCallback.T;
    rec: INTEGER;
  BEGIN
    WITH it = server.clients.iterate() DO
      WHILE it.next(rec, cb) DO
        TRY
          IF client # rec THEN
            IF Variant.TestRuleServer THEN
              Journal.Add("RuleServer: Notifying client " & Fmt.Int(rec)
                            & " about removed trigger " & Fmt.Int(trigger)
                            & " from client" & Fmt.Int(client));
            END;
            cb.unregisterTrigger(trigger);
          END;
        EXCEPT
          NetObj.Error, Thread.Alerted => CollectDeadClient(server, rec)
        END;
      END
    END;
  END BroadcastRemoveTrigger;

PROCEDURE PropagateTriggers (server: Server; client: CARDINAL) =
  VAR
    rt: RemoteTrigger;
    cb: RuleEngineCallback.T;
  BEGIN
    IF server.clients.get(client, cb) THEN
      rt := RemoteTriggerListLast(server.triggerList);
      TRY
        WHILE rt # NIL DO
          IF Variant.TestRuleServer THEN
            Journal.Add("RuleServer: Notifying new client about trigger "
                          & Fmt.Int(rt.trigger) & " from client"
                          & Fmt.Int(rt.client));
          END;
          cb.registerTrigger(
            rt.trigger, rt.client, rt.type, rt.bools, rt.ints, rt.texts,
            rt.coupling, rt.priority, rt.inh, rt.perm);
          rt := RemoteTriggerListPrev(rt);
        END;
      EXCEPT
        NetObj.Error, Thread.Alerted => CollectDeadClient(server, client);
      END;
    END;
  END PropagateTriggers;

PROCEDURE InternUnregisterClient (server: Server; client: CARDINAL) =
  VAR
    cb              : RuleEngineCallback.T;
    oc              : INTEGER;
    triggers        : IntSeq.T := NEW(IntSeq.T).init();
    trig, trigClient: INTEGER;
  BEGIN
    IF server.clients.delete(client, cb) THEN
      IF Variant.TestRuleServer THEN
        Journal.Add("RuleServer: Killing client no " & Fmt.Int(client));
      END;
      (* Delete registered triggers *)
      RemoteTriggerListDeleteClient(server.triggerList, client);
      (* Delete buffered events for this client *)
      WITH it = server.triggerClientMap.iterate() DO
        WHILE it.next(trig, trigClient) DO
          IF trigClient = client THEN
            EventBufferDeleteTrigger(server.eventBuffer, trig);
            triggers.addhi(trig);
          END;
        END;
      END;
      (* delete entries in trigger map *)
      FOR i := 0 TO triggers.size() - 1 DO
        EVAL server.triggerClientMap.delete(triggers.get(i), trigClient);
      END;
      (* notify other clients *)
      WITH it = server.clients.iterate() DO
        WHILE it.next(oc, cb) DO
          TRY
            cb.killClient(client);
          EXCEPT
            NetObj.Error, Thread.Alerted => (* ignore *)
          END;
        END;
      END;
    END;
  END InternUnregisterClient;

PROCEDURE CollectDeadClient (server: Server; client: CARDINAL) =
  BEGIN
    server.deadClients.addhi(client);
    Thread.Signal(server.hasDeadClients);
  END CollectDeadClient;

TYPE
  CleanerClosure = Thread.Closure OBJECT
                     server: Server;
                   OVERRIDES
                     apply := CleanerApply;
                   END;

PROCEDURE CleanerApply (cl: CleanerClosure): REFANY =
  VAR client: CARDINAL;
  BEGIN
    TRY
      LOOP
        LOCK cl.server.lock DO
          Thread.AlertWait(cl.server.lock, cl.server.hasDeadClients);
          WHILE cl.server.deadClients.size() > 0 DO
            client := cl.server.deadClients.remlo();
            InternUnregisterClient(cl.server, client);
          END;
        END;
      END;
    EXCEPT
      Thread.Alerted => RETURN NIL;
    END;
  END CleanerApply;

BEGIN
END RuleServer.
