MODULE VersionDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.4  1998/05/19 10:17:36  roland
    Support for log-groups implemented.

    Revision 1.3  1997/05/30 07:53:51  roland
    Backward loop added to deltas to efficiently implement copying of
    backward deltas.

    Revision 1.2  1997/04/24 14:29:23  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/04/23 13:31:41  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.1  1996/12/20 17:31:01  roland
    First version of ChgMgmtGraphSystem. Difference to
    PersistentGraphSystem: Graphs might be connected via deltas
    (DeltaCopyGraph) to allow efficient versioning.

*)
(***************************************************************************)

IMPORT Delta AS Super;
IMPORT GraphCommandStream, GraphCommand, FilePos, GraphCommandSeq;
IMPORT VirtualResource, PersistentGraphPool, Access, PageFile, ErrorSupport;
IMPORT Pathname;

REVEAL
  T = Public BRANDED OBJECT
        resource: VirtualResource.T;
        stream  : GraphCommandStream.T;
      OVERRIDES
        open           := Open;
        close          := Close;
        addCommand     := AddCommand;
        costs          := Costs;
        loop           := Loop;
        getNextCommand := GetNextCommand;
        reverseLoop    := ReverseLoop;
        getPrevCommand := GetPrevCommand;
        append         := Append;
        prepend        := Prepend;
      END;


CONST StreamName = "commands";

PROCEDURE WriteCosts (stream: GraphCommandStream.T; costs: CARDINAL)
  RAISES {Access.Locked, Super.Error} =
  VAR curr, first: FilePos.T;
  (* The costs of the delta are stored in the first command of the stream.
     This command is a 'DeleteNode' command. *)
  BEGIN
    TRY
      (* Go to the start of the stream *)
      stream.getPosition(curr);
      stream.getFirstPosition(first);
      stream.setPosition(first);
      (* Write the costs *)
      stream.write(
        GraphCommand.T{operation := GraphCommand.Operation.DeleteNode,
                       args := ARRAY [0 .. 4] OF CARDINAL{costs, 0, ..},
                       text := NIL, length := 0, from := 0, to := 0},
        overwrite := FALSE);
      stream.setPosition(curr);
    EXCEPT
      GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.WriteCosts",
                            "GraphCommandStream.InternalError", info));
    END;
  END WriteCosts;

PROCEDURE ReadCosts (stream: GraphCommandStream.T; VAR costs: CARDINAL)
  RAISES {Access.Locked, Super.Error} =
  VAR
    curr, first: FilePos.T;
    com        : GraphCommand.T;
  BEGIN
    TRY
      (* Go to the start of the stream *)
      stream.getPosition(curr);
      stream.getFirstPosition(first);
      stream.setPosition(first);
      (* Read costs *)
      stream.read(com);
      stream.setPosition(curr);
      costs := com.args[0];
    EXCEPT
      GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.ReadCosts",
                            "GraphCommandStream.InternalError", info));
    | GraphCommandStream.EOS =>
        RAISE Super.Error(ErrorSupport.Create("VersionDelta.ReadCosts",
                                              "GraphCommandStream.EOS"));
    | GraphCommandStream.ElementError =>
        RAISE Super.Error(
                ErrorSupport.Create("VersionDelta.ReadCosts",
                                    "GraphCommandStream.ElementError"));
    END;
  END ReadCosts;

PROCEDURE IncCosts (stream: GraphCommandStream.T; n: CARDINAL)
  RAISES {Access.Locked, Super.Error} =
  VAR
    curr, first: FilePos.T;
    com        : GraphCommand.T;
  BEGIN
    TRY
      (* Go to the start of the stream *)
      stream.getPosition(curr);
      stream.getFirstPosition(first);
      stream.setPosition(first);
      (* Read costs *)
      stream.read(com);
      INC(com.args[0], n);
      stream.write(com, overwrite := FALSE);
      stream.setPosition(curr);
    EXCEPT
      GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.IncCosts",
                            "GraphCommandStream.InternalError", info));
    | GraphCommandStream.EOS =>
        RAISE Super.Error(ErrorSupport.Create("VersionDelta.IncCosts",
                                              "GraphCommandStream.EOS"));
    | GraphCommandStream.ElementError =>
        RAISE Super.Error(
                ErrorSupport.Create("VersionDelta.IncCosts",
                                    "GraphCommandStream.ElementError"));
    END;
  END IncCosts;

(* Interface *)

PROCEDURE Open (             delta : T;
                             pool  : PersistentGraphPool.T;
                             path  : Pathname.T;
                             access: Access.Mode;
                             new   : BOOLEAN;
                <* UNUSED *> local : BOOLEAN                ): T
  RAISES {Access.Locked, Super.Error, Access.Denied} =

  BEGIN
    TRY
      (* open stream *)
      delta.stream :=
        NEW(GraphCommandStream.T).open(
          pool, path & StreamName, access, new, forward := TRUE);
    EXCEPT
      GraphCommandStream.DirectionMismatch =>
        RAISE Super.Error(ErrorSupport.Create(
                            "VersionDelta.Open",
                            "GraphCommandStream.DirectionMismatch"));
    | GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.Open",
                            "GraphCommandStream.InternalError", info));
    | PageFile.NoAccess (msg) =>
        RAISE
          Super.Error(ErrorSupport.Create(
                        "VersionDelta.Open", "PageFile.NoAccess: " & msg));
    END;

    IF new THEN WriteCosts(delta.stream, 0); END;
    RETURN delta;
  END Open;

PROCEDURE Close (delta: T) RAISES {Super.Error} =
  BEGIN
    TRY
      delta.stream.close();
    EXCEPT
      GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.Close",
                            "GraphCommandStream.InternalError", info));
    END;
  END Close;

PROCEDURE AddCommand (delta: T; READONLY c: GraphCommand.T)
  RAISES {Access.Locked, Super.Error} =
  BEGIN
    TRY
      delta.stream.write(c, overwrite := TRUE);
      IncCosts(delta.stream, 1);
    EXCEPT
      GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.AddCommand",
                            "GraphCommandStream.InternalError", info));
    END;
  END AddCommand;


PROCEDURE Costs (delta: T): CARDINAL RAISES {Access.Locked, Super.Error} =
  VAR c: CARDINAL;
  BEGIN
    ReadCosts(delta.stream, c);
    RETURN c;
  END Costs;

PROCEDURE Loop (delta: T)
  RAISES {Access.Locked, Super.Error} =
  VAR pos: FilePos.T;
  BEGIN
    TRY
      (* Go to the start of the commands *)
      delta.stream.getFirstPosition(pos);
      delta.stream.setPosition(pos);
      (* skip costs *)
      delta.stream.forward();
    EXCEPT
      GraphCommandStream.EOS =>
        RAISE
          Super.Error(ErrorSupport.Create(
                        "VersionDelta.Loop", "GraphCommandStream.EOS"));
    | GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.Loop",
                            "GraphCommandStream.InternalError", info));
    END;
  END Loop;

PROCEDURE GetNextCommand (delta: T; VAR c: GraphCommand.T; VAR ok: BOOLEAN)
  RAISES {Super.Error, Access.Locked} =
  BEGIN
    TRY
      IF NOT delta.stream.endOfStream() THEN
        delta.stream.read(c);
        ok := TRUE;
      ELSE
        ok := FALSE;
      END;
    EXCEPT
      GraphCommandStream.EOS =>
        RAISE
          Super.Error(ErrorSupport.Create("VersionDelta.GetNextCommand",
                                          "GraphCommandStream.EOS"));
    | GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.GetNextCommand",
                            "GraphCommandStream.InternalError", info));
    | GraphCommandStream.ElementError =>
        RAISE Super.Error(
                ErrorSupport.Create(
                  "VersionDelta.Loop", "GraphCommandStream.ElementError"));
    END;
  END GetNextCommand;

PROCEDURE ReverseLoop (delta: T)
  RAISES {Access.Locked, Super.Error} =
  VAR pos: FilePos.T;
  BEGIN
    TRY
      (* Go to the end of the commands *)
      delta.stream.getLastPosition(pos);
      delta.stream.setPosition(pos);
    EXCEPT
    | GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.Loop",
                            "GraphCommandStream.InternalError", info));
    END;
  END ReverseLoop;

PROCEDURE GetPrevCommand (delta: T; VAR c: GraphCommand.T; VAR ok: BOOLEAN)
  RAISES {Super.Error, Access.Locked} =
  VAR first, act: FilePos.T;
  BEGIN
    TRY
      delta.stream.backward();
      delta.stream.getPosition(act);
      delta.stream.getFirstPosition(first);
      (* stop when costs are reached *)
      IF act = first THEN
        ok := FALSE
      ELSE
        delta.stream.read(c);
        delta.stream.backward();
        ok := TRUE;
      END;
    EXCEPT
      GraphCommandStream.EOS => ok := FALSE;
    | GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.GetNextCommand",
                            "GraphCommandStream.InternalError", info));
    | GraphCommandStream.ElementError =>
        RAISE Super.Error(
                ErrorSupport.Create(
                  "VersionDelta.Loop", "GraphCommandStream.ElementError"));
    END;
  END GetPrevCommand;

PROCEDURE Append (delta: T; suffix: Super.T)
  RAISES {Super.Error, Access.Locked} =
  VAR
    ok  : BOOLEAN;
    c   : GraphCommand.T;
    last: FilePos.T;
  BEGIN
    TRY
      delta.stream.getLastPosition(last);
      delta.stream.setPosition(last);
      suffix.loop();
      suffix.getNextCommand(c, ok);
      WHILE ok DO
        delta.stream.write(c, overwrite := TRUE);
        suffix.getNextCommand(c, ok);
      END;
      IncCosts(delta.stream, suffix.costs());
    EXCEPT
      GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.Append",
                            "GraphCommandStream.InternalError", info));
    END;
  END Append;

PROCEDURE Prepend (delta: T; prefix: Super.T)
  RAISES {Super.Error, Access.Locked} =
  VAR
    tmplist : GraphCommandSeq.T;
    ok      : BOOLEAN;
    c       : GraphCommand.T;
    oldcosts: CARDINAL;
    pos     : FilePos.T;
  BEGIN
    TRY
      (* create new command-list and temporarily store commands of delta
         there *)
      tmplist := NEW(GraphCommandSeq.T).init();
      ReadCosts(delta.stream, oldcosts);
      Loop(delta);
      GetNextCommand(delta, c, ok);
      WHILE ok DO tmplist.addhi(c); GetNextCommand(delta, c, ok); END;

      (* Initialize delta as empty *)
      delta.stream.getFirstPosition(pos);
      delta.stream.setPosition(pos);
      (* skip costs *)
      delta.stream.forward();

      (* Write prefix to delta *)
      prefix.loop();
      prefix.getNextCommand(c, ok);
      WHILE ok DO
        delta.stream.write(c, overwrite := TRUE);
        prefix.getNextCommand(c, ok);
      END;

      (* Now write old contents back to delta *)
      FOR i := 0 TO tmplist.size() - 1 DO
        c := tmplist.get(i);
        delta.stream.write(c, overwrite := TRUE);
      END;
      WriteCosts(delta.stream, oldcosts + prefix.costs());

    EXCEPT
      GraphCommandStream.InternalError (info) =>
        RAISE Super.Error(ErrorSupport.Propagate(
                            "VersionDelta.Prepend",
                            "GraphCommandStream.InternalError", info));
    | GraphCommandStream.EOS =>
        RAISE Super.Error(ErrorSupport.Create("VersionDelta.Prepend",
                                              "GraphCommandStream.EOS"));
    END;
  END Prepend;

BEGIN
END VersionDelta.
