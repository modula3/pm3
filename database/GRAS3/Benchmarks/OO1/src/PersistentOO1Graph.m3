MODULE PersistentOO1Graph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.6  1998/03/18 09:26:57  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.5  1998/03/17 14:13:35  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.4  1997/10/31 14:24:19  roland
    Adapted to new RuleEngine.

    Revision 1.3  1997/10/14 09:16:44  roland
    Merge of HiGRAS and Main branch.

    Revision 1.1.2.2  1997/07/21 10:53:48  roland
    Adapted to new set implementation (free memory lists and deleted
    SetExceptions)

    Revision 1.1.2.1  1997/06/19 14:38:30  roland
    Branch HiGRAS implements OO1 for HiGRAS. TypegGraph-Implementation of
    OO1Graph added.

    Revision 1.1  1997/02/20 16:08:59  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT Pathname, Text, AtomList, NetObj, Thread;
IMPORT OO1Graph;
IMPORT Access, PersistentGraph, CardSet, PageFile, ErrorSupport, Node,
       PersistentGraphPool, NodeSet;

CONST
  PartLabel       = 1;
  ConnectionLabel = 2;

  ConnectOutLabel = 3;
  ConnectInLabel  = 4;


REVEAL
  T = Public BRANDED OBJECT
        graph : PersistentGraph.T;
        pool  : PersistentGraphPool.T;
        grnum : CARDINAL;
        last  : Node.T                  := Node.T{0, 0};
        simple: BOOLEAN;
      OVERRIDES
        open  := Open;
        close := Close;

        beginTransaction  := BeginTransaction;
        commitTransaction := CommitTransaction;
        abortTransaction  := AbortTransaction;

        createPart := CreatePart;

        connect           := Connect;
        connected         := Connected;
        connectionInfo    := ConnectionInfo;
        sources           := Sources;
        targets           := Targets;
        putPartAttributes := PutPartAttributes;
        getPartAttributes := GetPartAttributes;
      END;

PROCEDURE Open (gr            : T;
                name          : Pathname.T;
                access        : Access.Mode;
                new           : BOOLEAN;
                local         : BOOLEAN;
                simpleConnects: BOOLEAN      ): T
  RAISES {OO1Graph.Failure} =
  <* FATAL PersistentGraphPool.NotInTransaction *>
  BEGIN
    TRY
      gr.pool := NEW(PersistentGraphPool.T).open(name, access, new);
      gr.graph := NEW(PersistentGraph.T).open(
                    gr.pool, name, PersistentGraph.AccessMode.Inherit, new,
                    local, errorChecks := FALSE);
      gr.pool.beginTransaction();
      gr.grnum := gr.pool.graphNumber(name, local);
      gr.pool.commitTransaction();
      gr.simple := simpleConnects;
      RETURN gr;
    EXCEPT
      PageFile.NoAccess (msg) =>
        RAISE OO1Graph.Failure("No access: " & msg);
    | Access.Denied (msg) =>
        RAISE OO1Graph.Failure("Access denied: " & msg);
    | Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraphPool.InternalError (info) =>
        RAISE OO1Graph.Failure(ErrorSupport.ToText(info));
    | PersistentGraph.InternalError (info) =>
        RAISE OO1Graph.Failure(ErrorSupport.ToText(info));
    | PersistentGraph.InUse => RAISE OO1Graph.Failure("Graph in use!");
    | PersistentGraph.NotExistent, PersistentGraphPool.NotExistent =>
        RAISE OO1Graph.Failure("Graph not exists!");
    END;
  END Open;

PROCEDURE Close (gr: T) =
  BEGIN
    TRY
      gr.graph.close();
      gr.pool.close();
    EXCEPT
    | PersistentGraph.InternalError => (* I don't care *)
    | PersistentGraphPool.InternalError => (* I don't care *)
    END;
  END Close;

PROCEDURE BeginTransaction (gr: T)
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      gr.pool.beginTransaction();
    EXCEPT
      PersistentGraphPool.InternalError (info) =>
        HandleInternalError(info);
    END;
  END BeginTransaction;

PROCEDURE CommitTransaction (gr: T)
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      gr.pool.commitTransaction();
    EXCEPT
      PersistentGraphPool.InternalError (info) => HandleInternalError(info)
    | PersistentGraphPool.NotInTransaction =>
        RAISE OO1Graph.Failure("Not in transaction");
    END;
  END CommitTransaction;

PROCEDURE AbortTransaction (gr: T)
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      gr.pool.abortTransaction();
    EXCEPT
      PersistentGraphPool.InternalError (info) =>
        HandleInternalError(info);
    | PersistentGraphPool.NotInTransaction =>
        RAISE OO1Graph.Failure("Not in transaction");
    END;
  END AbortTransaction;

PROCEDURE CreatePart (gr: T; n: CARDINAL): CARDINAL
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      gr.last := gr.graph.createNodeNumber(Node.T{gr.grnum, n});
      gr.last := gr.graph.createNode(PartLabel);
      RETURN gr.last.entity;
    EXCEPT
      Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraph.NotOwner => RAISE OO1Graph.Failure("Not owner!");
    | PersistentGraph.InternalError (info) =>
        HandleInternalError(info);
        RETURN 0;
    END;
  END CreatePart;

PROCEDURE Connect (gr            : T;
                   source, target: CARDINAL;
                   type          : OO1Graph.TypeString;
                   length        : CARDINAL             )
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  VAR nn: Node.T;
  BEGIN
    TRY
      IF NOT gr.graph.existsEdge(Node.T{gr.grnum, source},
                                 Node.T{gr.grnum, target}, ConnectionLabel) THEN
        gr.graph.createEdge(Node.T{gr.grnum, source},
                            Node.T{gr.grnum, target}, ConnectionLabel);
      END;
      IF NOT gr.simple THEN
        nn := gr.graph.createNodeNumber(Node.T{gr.grnum, source});
        nn := gr.graph.createNode(ConnectionLabel);
        (* Store length attribute *)
        gr.graph.putAttribute(nn, 0, 0, ConnectionInfoToText(type, length));
        gr.graph.createEdge(Node.T{gr.grnum, source}, nn, ConnectOutLabel);
        gr.graph.createEdge(Node.T{gr.grnum, target}, nn, ConnectInLabel);
      END;
    EXCEPT
      Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraph.NodeNotFound =>
        RAISE OO1Graph.Failure("Node not found");
    | PersistentGraph.InternalError (info) => HandleInternalError(info);
    | PersistentGraph.NotOwner => RAISE OO1Graph.Failure("Not owner!");
    END;
  END Connect;

PROCEDURE Connected (gr: T; source, target: CARDINAL): BOOLEAN
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      RETURN
        gr.graph.existsEdge(Node.T{gr.grnum, source},
                            Node.T{gr.grnum, target}, ConnectionLabel);
    EXCEPT
      Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraph.NodeNotFound =>
        RAISE OO1Graph.Failure("Node not found");
    | PersistentGraph.InternalError (info) =>
        HandleInternalError(info);
        RETURN FALSE;
    | PersistentGraph.NotOwner => RAISE OO1Graph.Failure("Not owner!");
    END;
  END Connected;

PROCEDURE ConnectionInfo (    gr            : T;
                              source, target: CARDINAL;
                          VAR type          : OO1Graph.TypeString;
                          VAR len           : CARDINAL;
                          VAR ok            : BOOLEAN              )
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  VAR
    inc : NodeSet.T;
    node: Node.T;
  BEGIN
    TRY
      ok := NOT gr.simple AND gr.graph.existsEdge(
                                Node.T{gr.grnum, source},
                                Node.T{gr.grnum, target}, ConnectionLabel);
      IF ok THEN
        inc :=
          gr.graph.getTargets(Node.T{gr.grnum, source}, ConnectOutLabel);
        inc.intersection(
          gr.graph.getTargets(Node.T{gr.grnum, target}, ConnectInLabel));
        node := inc.extractAnyElement(ok);
        IF ok THEN
          ConnectionInfoFromText(
            gr.graph.getAttribute(node, 0, 0, ConnectionInfoLength), type,
            len);
        END;
      END;
    EXCEPT
      Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraph.NodeNotFound =>
        RAISE OO1Graph.Failure("Node not found");
    | PersistentGraph.InternalError (info) => HandleInternalError(info);
    | PersistentGraph.NotOwner => RAISE OO1Graph.Failure("Not owner!");
    END;

  END ConnectionInfo;

PROCEDURE Sources (gr: T; target: CARDINAL): CardSet.T
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      WITH e = gr.graph.getSources(
                 Node.T{gr.grnum, target}, ConnectionLabel),
           res = Entities(e) DO
        e.dispose();
        RETURN res;
      END;
    EXCEPT
      Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraph.NodeNotFound =>
        RAISE OO1Graph.Failure("Node not found");
    | PersistentGraph.InternalError (info) =>
        HandleInternalError(info);
        RETURN NIL;
    | PersistentGraph.NotOwner => RAISE OO1Graph.Failure("Not owner!");
    END;
  END Sources;

PROCEDURE Targets (gr: T; source: CARDINAL): CardSet.T
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      WITH e = gr.graph.getTargets(
                 Node.T{gr.grnum, source}, ConnectionLabel),
           res = Entities(e) DO
        e.dispose();
        RETURN res;
      END;
    EXCEPT
      Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraph.NodeNotFound =>
        RAISE OO1Graph.Failure("Node not found");
    | PersistentGraph.InternalError (info) =>
        HandleInternalError(info);
        RETURN NIL;
    | PersistentGraph.NotOwner => RAISE OO1Graph.Failure("Not owner!");
    END;
  END Targets;

PROCEDURE PutPartAttributes (         gr   : T;
                                      part : CARDINAL;
                             READONLY type : OO1Graph.TypeString;
                                      x, y : INTEGER;
                             READONLY build: OO1Graph.Date        )
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      gr.graph.putAttribute(
        Node.T{gr.grnum, part}, 0, 0, PartInfoToText(type, x, y, build));
    EXCEPT
      Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraph.NodeNotFound =>
        RAISE OO1Graph.Failure("Node not found");
    | PersistentGraph.InternalError (info) => HandleInternalError(info);
    | PersistentGraph.NotOwner => RAISE OO1Graph.Failure("Not owner!");
    END;
  END PutPartAttributes;

PROCEDURE GetPartAttributes (    gr   : T;
                                 part : CARDINAL;
                             VAR type : OO1Graph.TypeString;
                             VAR x, y : INTEGER;
                             VAR build: OO1Graph.Date        )
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    TRY
      PartInfoFromText(
        gr.graph.getAttribute(
          Node.T{gr.grnum, part}, 0, 0, PartAttributeLength), type, x, y,
        build);
    EXCEPT
      Access.Locked => RAISE OO1Graph.Failure("Access locked");
    | PersistentGraph.NodeNotFound =>
        RAISE OO1Graph.Failure("Node not found");
    | PersistentGraph.InternalError (info) => HandleInternalError(info)
    | PersistentGraph.NotOwner => RAISE OO1Graph.Failure("Not owner!");
    END;
  END GetPartAttributes;

PROCEDURE HandleInternalError (info: AtomList.T)
  RAISES {OO1Graph.Failure, Thread.Alerted} =
  BEGIN
    IF AtomList.Member(info, NetObj.Alerted) THEN
      RAISE Thread.Alerted;
    ELSE
      RAISE
        OO1Graph.Failure("Internal error: " & ErrorSupport.ToText(info));
    END;
  END HandleInternalError;

PROCEDURE IntToAOC (int: INTEGER; VAR t: ARRAY [0 .. 3] OF CHAR) =
  BEGIN
    t[0] := VAL(int MOD 256, CHAR);
    int := int DIV 256;
    t[1] := VAL(int MOD 256, CHAR);
    int := int DIV 256;
    t[2] := VAL(int MOD 256, CHAR);
    int := int DIV 256;
    t[3] := VAL(int MOD 256, CHAR);
  END IntToAOC;

PROCEDURE AOCToInt (READONLY t: ARRAY [0 .. 3] OF CHAR): INTEGER =
  VAR res: INTEGER := 0;
  BEGIN
    res := ORD(t[3]);
    res := res * 256 + ORD(t[2]);
    res := res * 256 + ORD(t[1]);
    res := res * 256 + ORD(t[0]);
    RETURN res;
  END AOCToInt;


CONST PartAttributeLength = 10 + 2 * 4 + 6 * 4;
(* BYTESIZE(Typestring) + 2*BYTESIZE(INTEGER) + 6*BYTESIZE(CARDINAL) *)

PROCEDURE PartInfoToText (type: OO1Graph.TypeString;
                          x, y: INTEGER;
                          date: OO1Graph.Date        ): TEXT =
  VAR buf: ARRAY [0 .. PartAttributeLength - 1] OF CHAR;
  BEGIN
    SUBARRAY(buf, 0, 10) := type;
    IntToAOC(x, SUBARRAY(buf, 10, 4));
    IntToAOC(y, SUBARRAY(buf, 14, 4));
    IntToAOC(date.year, SUBARRAY(buf, 18, 4));
    IntToAOC(date.month, SUBARRAY(buf, 22, 4));
    IntToAOC(date.day, SUBARRAY(buf, 26, 4));
    IntToAOC(date.hour, SUBARRAY(buf, 30, 4));
    IntToAOC(date.minute, SUBARRAY(buf, 34, 4));
    IntToAOC(date.second, SUBARRAY(buf, 38, 4));
    RETURN Text.FromChars(buf);
  END PartInfoToText;

PROCEDURE PartInfoFromText (    t   : TEXT;
                            VAR type: OO1Graph.TypeString;
                            VAR x, y: INTEGER;
                            VAR date: OO1Graph.Date        ) =
  VAR buf: ARRAY [0 .. PartAttributeLength - 1] OF CHAR;
  BEGIN
    Text.SetChars(buf, t);
    type := SUBARRAY(buf, 0, 10);
    x := AOCToInt(SUBARRAY(buf, 10, 4));
    y := AOCToInt(SUBARRAY(buf, 14, 4));
    date.year := AOCToInt(SUBARRAY(buf, 18, 4));
    date.month := AOCToInt(SUBARRAY(buf, 22, 4));
    date.day := AOCToInt(SUBARRAY(buf, 26, 4));
    date.hour := AOCToInt(SUBARRAY(buf, 30, 4));
    date.minute := AOCToInt(SUBARRAY(buf, 34, 4));
    date.second := AOCToInt(SUBARRAY(buf, 38, 4));
  END PartInfoFromText;

CONST ConnectionInfoLength = 10 + 4;
(* BYTESIZE(OO1Graph.TypeString) + BYTESIZE(CARDINAL) *)

PROCEDURE ConnectionInfoToText (type: OO1Graph.TypeString; length: CARDINAL):
  TEXT =
  VAR buf: ARRAY [0 .. ConnectionInfoLength - 1] OF CHAR;
  BEGIN
    SUBARRAY(buf, 0, 10) := type;
    IntToAOC(length, SUBARRAY(buf, 10, 4));
    RETURN Text.FromChars(buf);
  END ConnectionInfoToText;

PROCEDURE ConnectionInfoFromText (    t     : TEXT;
                                  VAR type  : OO1Graph.TypeString;
                                  VAR length: CARDINAL             ) =
  VAR buf: ARRAY [0 .. ConnectionInfoLength - 1] OF CHAR;
  BEGIN
    Text.SetChars(buf, t);
    type := SUBARRAY(buf, 0, 10);
    length := AOCToInt(SUBARRAY(buf, 10, 4));
  END ConnectionInfoFromText;

PROCEDURE Entities (nodes: NodeSet.T): CardSet.T =
  VAR
    node : Node.T;
    found: BOOLEAN;
  BEGIN
    nodes.loop();
    WITH res = CardSet.New() DO
      node := nodes.get(found);
      WHILE found DO
        res.insert(node.entity);
        node := nodes.get(found);
      END;
      RETURN res;
    END;
  END Entities;

BEGIN
END PersistentOO1Graph.
