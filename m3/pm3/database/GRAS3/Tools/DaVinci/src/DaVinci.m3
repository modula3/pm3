MODULE DaVinci;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1998/08/06 10:48:31  roland
    A Modula-3 interface to the graph display tool daVinci.

*)
(***************************************************************************)

IMPORT FilePosix, FileRd, FileWr, OSError, OSErrorPosix, Pipe, Rd, Wr,
       Thread, Process, Fmt, Text, Env, Pathname;

REVEAL
  T = Public BRANDED OBJECT
        rd             : Rd.T;
        wr             : Wr.T;
        proc           : Process.T;
        rec            : Thread.T;
        notifier       : Thread.T;
        notifierClients: ClientList;
        buffer         : MsgBuffer;
      OVERRIDES
        init              := Init;
        send              := Send;
        quit              := Quit;
        registerHandler   := RegisterHandler;
        unregisterHandler := UnregisterHandler;
      END;

PROCEDURE Init (dv: T; dvhome: TEXT := NIL): T RAISES {Error} =
  CONST DVHOME_ENV = "DAVINCIHOME";
  VAR
    env    : REF ARRAY OF TEXT := NIL;
    nm, val: TEXT;
  BEGIN
    MsgBufferInit(dv.buffer);
    ClientListInit(dv.notifierClients);

    (* check environment variable DAVINCIHOME *)
    IF dvhome = NIL THEN
      (* no value for dvhome, is it set? *)
      dvhome := Env.Get(DVHOME_ENV);
      IF dvhome = NIL THEN
        RAISE Error("Environment variable DAVINCIHOME is not set!");
      END;
    ELSE
      (* dvhome has a value, we must ensure, that it is propagated to
         daVinci.  To be on the safe side, all other variables are copied,
         too. *)
      IF Env.Get(DVHOME_ENV) # NIL THEN
        (* override old value of DAVINCIHOME *)
        env := NEW(REF ARRAY OF TEXT, Env.Count);
        FOR i := 0 TO Env.Count - 1 DO
          Env.GetNth(i, nm, val);
          IF Text.Equal(nm, DVHOME_ENV) THEN val := dvhome; END;
          env[i] := nm & "=" & val;
        END;
      ELSE
        (* insert new value for DAVINCIHOME *)
        env := NEW(REF ARRAY OF TEXT, Env.Count + 1);
        FOR i := 0 TO Env.Count - 1 DO
          Env.GetNth(i, nm, val);
          IF Text.Equal(nm, DVHOME_ENV) THEN val := dvhome; END;
          env[i] := nm & "=" & val;
        END;
        env[LAST(env^)] := DVHOME_ENV & "=" & dvhome;
      END;
    END;

    StartDaVinci(dvhome, env, dv.rd, dv.wr, dv.proc);
    dv.rec := Thread.Fork(NEW(ReceiverClosure, dv := dv));
    WITH ok = MsgBufferGet(dv.buffer) DO
      IF NOT Text.Equal("ok", ok) THEN
        RAISE Error("Unexpected answer from DaVinci: " & ok);
      END;
    END;
    dv.notifier := Thread.Fork(NEW(NotifierClosure, dv := dv));
    RETURN dv;
  END Init;

PROCEDURE Send (dv: T; msg: TEXT) RAISES {Error} =
  BEGIN
    TRY
      Wr.PutText(dv.wr, msg);
      Wr.Flush(dv.wr);
    EXCEPT
      Wr.Failure => RAISE Error("Lost connection to DaVinci process");
    | Thread.Alerted => RAISE Error("Interrupt");
    END
  END Send;

PROCEDURE Quit (dv: T) =
  BEGIN
    TRY
      Wr.PutText(dv.wr, "quit\n");
      Wr.Flush(dv.wr);
      Wr.Close(dv.wr);
      EVAL Process.Wait(dv.proc);
      Rd.Close(dv.rd);
    EXCEPT
      Wr.Failure, Thread.Alerted, Rd.Failure => (* ignore *)
    END
  END Quit;

PROCEDURE RegisterHandler (dv: T; type: MsgType; handler: EventHandler) =
  BEGIN
    ClientListInsert(dv.notifierClients, type, handler);
  END RegisterHandler;

PROCEDURE UnregisterHandler (dv: T; type: MsgType; handler: EventHandler) =
  BEGIN
    ClientListRemove(dv.notifierClients, type, handler);
  END UnregisterHandler;


PROCEDURE StartDaVinci (dvhome: Pathname.T;
                        env : REF ARRAY OF TEXT;
                        VAR rd  : Rd.T;
                        VAR wr  : Wr.T;
                        VAR proc: Process.T          ) RAISES {Error} =
  VAR
    hwChild, hwSelf, hrSelf, hrChild: Pipe.T;
    parameters := ARRAY [0 .. 0] OF TEXT{"-pipe"};
  BEGIN
    TRY
      Pipe.Open(hr := hrChild, hw := hwSelf);
      Pipe.Open(hr := hrSelf, hw := hwChild);
      proc :=
        Process.Create(
          Pathname.Join(dvhome, "daVinci", NIL), parameters,
          env := env, stdin := hrChild, stdout := hwChild);
    EXCEPT
      OSError.E (e) =>
        RAISE Error("Cannot start DaVinci process: errno = "
                      & Fmt.Int(OSErrorPosix.AtomToErrno(e.head)));
    END;
    TRY hrChild.close(); hwChild.close() EXCEPT OSError.E => END;

    TRY
      wr := NEW(FileWr.T).init(hwSelf);
      rd := NEW(FileRd.T).init(hrSelf);
    EXCEPT
      OSError.E (e) =>
        RAISE Error("Cannot connect to DaVinci process: errno = "
                      & Fmt.Int(OSErrorPosix.AtomToErrno(e.head)));
    END;
  END StartDaVinci;

PROCEDURE DecodeMsgType (msg: TEXT): MsgType =
  CONST
    Prefixes = ARRAY MsgType OF
                 TEXT{"ok", "quit", "close", "communication_error",
                      "node_selections_label", "node_double_click",
                      "edge_selection_label", "edge_double_click",
                      "menu_selection", "icon_selection", "context",
                      "tcl_answer", "browser_answer", "disconnect", ""};
  BEGIN
    FOR i := FIRST(MsgType) TO LAST(MsgType) DO
      WITH pre = Text.Sub(msg, 0, Text.Length(Prefixes[i])) DO
        IF Text.Equal(pre, Prefixes[i]) THEN RETURN i; END;
      END;
    END;
    RETURN MsgType.Unknown;
  END DecodeMsgType;

TYPE
  ReceiverClosure = Thread.Closure OBJECT
                      dv : T;
                      msg: TEXT;
                    OVERRIDES
                      apply := ReceiverApply;
                    END;

  MsgBuffer = RECORD
                messages, last: REF Message;
                lock          : MUTEX;
                newMessage    : Thread.Condition;
                count         : CARDINAL;
              END;

  Message = RECORD
              text: TEXT;
              next: REF Message;
            END;

PROCEDURE MsgBufferInit (VAR buf: MsgBuffer) =
  BEGIN
    buf.lock := NEW(MUTEX);
    buf.newMessage := NEW(Thread.Condition);
    buf.count := 0;
    buf.messages := NIL;
    buf.last := NIL;
  END MsgBufferInit;

PROCEDURE MsgBufferGet (VAR buf: MsgBuffer): TEXT =
  VAR res: TEXT;
  BEGIN
    LOCK buf.lock DO
      IF buf.count = 0 THEN Thread.Wait(buf.lock, buf.newMessage); END;
      res := buf.messages.text;
      buf.messages := buf.messages.next;
      DEC(buf.count);
    END;
    RETURN res;
  END MsgBufferGet;

PROCEDURE MsgBufferPut (VAR buf: MsgBuffer; text: TEXT) =
  BEGIN
    LOCK buf.lock DO
      buf.messages := NEW(REF Message, text := text, next := buf.messages);
      IF buf.count = 0 THEN buf.last := buf.messages; END;
      INC(buf.count);
      Thread.Signal(buf.newMessage);
    END;
  END MsgBufferPut;

PROCEDURE ReceiverApply (cl: ReceiverClosure): REFANY =
  VAR msg: TEXT;
  BEGIN
    TRY
      LOOP
        msg := Rd.GetLine(cl.dv.rd);
        MsgBufferPut(cl.dv.buffer, msg);
      END;
    EXCEPT
      Rd.Failure, Thread.Alerted, Rd.EndOfFile => (* terminate silently *)
    END;
    RETURN NIL;
  END ReceiverApply;

TYPE
  NotifierClosure =
    Thread.Closure OBJECT dv: T;  OVERRIDES apply := NotifierApply; END;

  ClientList = RECORD
                 lock  : MUTEX;
                 lists : ARRAY MsgType OF REF Client;
                 cursor: REF Client;
               END;

  Client = RECORD
             handler: EventHandler;
             next   : REF Client;
           END;

PROCEDURE NotifierApply (cl: NotifierClosure): REFANY =
  VAR
    msg    : TEXT;
    type   : MsgType;
    handler: EventHandler;
  BEGIN
    LOOP
      (* wait for next message from daVinci *)
      msg := MsgBufferGet(cl.dv.buffer);
      (* determine type of message *)
      type := DecodeMsgType(msg);
      (* notify everyone registered for this kind of message *)
      ClientListLoop(cl.dv.notifierClients, type);
      WHILE ClientListNext(cl.dv.notifierClients, handler) DO
        handler.notify(type, msg);
      END;
    END;
  END NotifierApply;


PROCEDURE ClientListInit (VAR list: ClientList) =
  BEGIN
    list.lock := NEW(MUTEX);
    list.lists := ARRAY MsgType OF REF Client{NIL, ..};
    list.cursor := NIL;
  END ClientListInit;

PROCEDURE ClientListLoop (VAR list: ClientList; type: MsgType) =
  BEGIN
    LOCK list.lock DO list.cursor := list.lists[type]; END;
  END ClientListLoop;

PROCEDURE ClientListNext (VAR list: ClientList; VAR handler: EventHandler):
  BOOLEAN =
  BEGIN
    LOCK list.lock DO
      IF list.cursor # NIL THEN
        handler := list.cursor.handler;
        list.cursor := list.cursor.next;
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;
  END ClientListNext;

PROCEDURE ClientListInsert (VAR list   : ClientList;
                                type   : MsgType;
                                handler: EventHandler) =
  VAR h: REF Client;
  BEGIN
    LOCK list.lock DO
      h := list.lists[type];
      WHILE h # NIL DO
        IF h^.handler = handler THEN RETURN END;
        h := h^.next;
      END;
      h := NEW(REF Client, handler := handler, next := list.lists[type]);
      list.lists[type] := h;
    END;
  END ClientListInsert;

PROCEDURE ClientListRemove (VAR list   : ClientList;
                                type   : MsgType;
                                handler: EventHandler) =
  VAR h, p: REF Client;
  BEGIN
    LOCK list.lock DO
      h := list.lists[type];
      p := NIL;
      WHILE h # NIL DO
        IF h^.handler = handler THEN
          IF p = NIL THEN
            list.lists[type] := h^.next;
          ELSE
            p^.next := h^.next;
            h^.next := NIL;
            h := NIL;
          END;
        ELSE
          h := h^.next;
        END;
      END;
    END;
  END ClientListRemove;

BEGIN
END DaVinci.
