MODULE GraphCommand;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.2  1998/05/19 10:17:46  roland
    Support for log-groups implemented.

    Revision 1.1  1997/04/23 13:32:46  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary crossing
    edges. Main modules follow later.

    Revision 1.2  1996/12/03 09:52:34  roland
    Bugfix. Undo and Redo for PutAttribute should now work properly.

    Revision 1.1  1996/09/17 12:57:13  roland
    Replacement of RecoverableGraph. Changes were necessary to incorporate
    PageServer-Implementation.
    Undo/Redo/SetCheckpoint are testet
    RedoPrev/RedoNext/RedoIth should work
    Backstep/Forstep are not implemented yet

*)
(***************************************************************************)

IMPORT Type, Text, Node;

CONST numSize = BYTESIZE(CARDINAL);

TYPE
  ArgPattern = RECORD
                 args      : CARDINAL;
                 text, part: BOOLEAN;
               END;


VAR patternTable: ARRAY Operation OF ArgPattern;

PROCEDURE NumOfArgs (o: Operation): CARDINAL =
  BEGIN
    RETURN patternTable[o].args;
  END NumOfArgs;

PROCEDURE HasText (o: Operation): BOOLEAN =
  BEGIN
    RETURN patternTable[o].text;
  END HasText;

PROCEDURE HasPartialText (o: Operation): BOOLEAN =
  BEGIN
    RETURN patternTable[o].text AND patternTable[o].part;
  END HasPartialText;


PROCEDURE StoreNumber (VAR ba: Type.ByteArray; start, number: CARDINAL) =
  BEGIN
    FOR i := numSize - 1 TO 0 BY -1 DO
      ba[start + i] := number MOD 256;
      number := number DIV 256;
    END;
  END StoreNumber;

PROCEDURE GetNumber (READONLY ba    : Type.ByteArray;
                              start : CARDINAL;
                     VAR      number: CARDINAL        ) =
  BEGIN
    number := 0;
    FOR i := 0 TO numSize - 1 DO
      number := number * 256 + ba[start + i];
    END;
  END GetNumber;

PROCEDURE ToByteArray (READONLY co : T;
                       VAR      len: CARDINAL;
                       VAR      ba : REF Type.ByteArray) =
  VAR need, tl, idx: CARDINAL;
  BEGIN
    (* how many bytes do we need for storing the command ? *)

    (* One to store the operation type and numSize for each numerical
       argument *)
    need := 1 + numSize * patternTable[co.operation].args;
    IF patternTable[co.operation].text THEN
      (* the operation needs text -> numSize more bytes for
         length/startpos. *)
      INC(need, numSize);
      tl := co.length;
      IF patternTable[co.operation].part THEN
        (* text is not stored completely -> numSize more bytes for
           endpos. *)
        INC(need, numSize);
        tl := co.to - co.from;
      END;
      (* finally, space to store the text *)
      INC(need, tl);
    END;
    len := need;

    (* now make room for us *)
    ba := NEW(REF Type.ByteArray, len);
    (* ok -> fill the array *)
    ba[0] := ORD(co.operation);
    FOR i := 0 TO patternTable[co.operation].args - 1 DO
      StoreNumber(ba^, numSize * i + 1, co.args[i]);
    END;
    IF patternTable[co.operation].text THEN
      idx := 1 + numSize * patternTable[co.operation].args; (* next free
                                                               position *)
      IF patternTable[co.operation].part THEN
        StoreNumber(ba^, idx, co.from);
        StoreNumber(ba^, idx + numSize, co.to);
        FOR i := 0 TO tl - 1 DO
          ba[idx + 2 * numSize + i] := ORD(Text.GetChar(co.text, i));
        END;
      ELSE
        StoreNumber(ba^, idx, co.length);
        FOR i := 0 TO tl - 1 DO
          ba[idx + numSize + i] := ORD(Text.GetChar(co.text, i));
        END;
      END;
    END;
  END ToByteArray;

PROCEDURE FromByteArray (             READONLY ba : Type.ByteArray;
                         <* UNUSED *>          len: CARDINAL;
                                      VAR      co : T;
                                      VAR      ok : BOOLEAN         ) =
  VAR
    idx: CARDINAL;
    buf: REF ARRAY OF CHAR;
  BEGIN
    IF (ba[0] > ORD(LAST(Operation))) OR (ba[0] < ORD(FIRST(Operation))) THEN
      ok := FALSE;
    ELSE
      ok := TRUE;
      WITH p = patternTable[VAL(ba[0], Operation)] DO
        co.operation := VAL(ba[0], Operation);
        FOR i := 0 TO p.args - 1 DO
          GetNumber(ba, 1 + numSize * i, co.args[i]);
        END;
        IF p.text THEN
          idx := 1 + numSize * p.args;
          IF p.part THEN
            GetNumber(ba, idx, co.from);
            GetNumber(ba, idx + numSize, co.to);
            IF (co.to < co.from) THEN ok := FALSE; RETURN; END;
            buf := NEW(REF ARRAY OF CHAR, co.to - co.from);
            FOR i := 0 TO co.to - co.from - 1 DO
              buf^[i] := VAL(ba[idx + 2 * numSize + i], CHAR);
            END;
            co.text := Text.FromChars(buf^);
          ELSE
            GetNumber(ba, idx, co.length);
            buf := NEW(REF ARRAY OF CHAR, co.length);
            FOR i := 0 TO co.length - 1 DO
              buf^[i] := VAL(ba[idx + numSize + i], CHAR);
            END;
            co.text := Text.FromChars(buf^);
          END;
        END;
      END;
    END;
  END FromByteArray;


(* Procedures Eq and Lt are only used for generic instantiation of LISTs of
   Commands *)
PROCEDURE Equal (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN
      a.operation = b.operation AND a.args = b.args AND a.length = b.length
        AND a.from = b.from AND a.to = b.to AND Text.Equal(a.text, b.text);
  END Equal;

PROCEDURE Compare (READONLY a, b: T): [-1 .. 1] =
  BEGIN
    IF a.operation < b.operation THEN
      RETURN -1
    ELSIF a.operation > b.operation THEN
      RETURN 1
    ELSE
      RETURN 0
    END;
  END Compare;

PROCEDURE CreateNode (VAR com: T; node: Node.T; label: CARDINAL) =
  BEGIN
    com :=
      T{operation := Operation.CreateNode, args :=
        ARRAY [0 .. 4] OF CARDINAL{node.graph, node.entity, label, 0, 0},
        text := NIL, length := 0, from := 0, to := 0};
  END CreateNode;

PROCEDURE DeleteNode (VAR com: T; node: Node.T) =
  BEGIN
    com := T{operation := Operation.DeleteNode, args :=
             ARRAY [0 .. 4] OF CARDINAL{node.graph, node.entity, 0, 0, 0},
             text := NIL, length := 0, from := 0, to := 0};
  END DeleteNode;

PROCEDURE PutNodeLabel (VAR com: T; node: Node.T; label: CARDINAL) =
  BEGIN
    com :=
      T{operation := Operation.PutNodeLabel, args :=
        ARRAY [0 .. 4] OF CARDINAL{node.graph, node.entity, label, 0, 0},
        text := NIL, length := 0, from := 0, to := 0};
  END PutNodeLabel;

PROCEDURE PutAttribute (VAR com                  : T;
                            node                 : Node.T;
                            attrno, start, length: CARDINAL;
                            value                : TEXT      ) =
  BEGIN
    com :=
      T{operation := Operation.PutAttribute, args :=
        ARRAY [0 .. 4] OF CARDINAL{node.graph, node.entity, attrno, 0, 0},
        text := value, length := 0, from := start, to := start + length};
  END PutAttribute;

PROCEDURE TruncateAttribute (VAR com           : T;
                                 node          : Node.T;
                                 attrno, length: CARDINAL) =
  BEGIN
    com := T{operation := Operation.TruncateAttribute, args :=
             ARRAY [0 .. 4] OF
               CARDINAL{node.graph, node.entity, attrno, length, 0},
             text := NIL, length := 0, from := 0, to := 0};
  END TruncateAttribute;

PROCEDURE DeleteAttribute (VAR com: T; node: Node.T; attrno: CARDINAL) =
  BEGIN
    com :=
      T{operation := Operation.DeleteAttribute, args :=
        ARRAY [0 .. 4] OF CARDINAL{node.graph, node.entity, attrno, 0, 0},
        text := NIL, length := 0, from := 0, to := 0};
  END DeleteAttribute;

PROCEDURE PutIndex (VAR com            : T;
                        node           : Node.T;
                        indexno, length: CARDINAL;
                        value          : TEXT      ) =
  BEGIN
    com :=
      T{operation := Operation.PutIndex, args :=
        ARRAY [0 .. 4] OF CARDINAL{node.graph, node.entity, indexno, 0, 0},
        text := value, length := length, from := 0, to := 0};
  END PutIndex;

PROCEDURE DeleteIndex (VAR com            : T;
                           node           : Node.T;
                           indexno, length: CARDINAL;
                           value          : TEXT      ) =
  BEGIN
    com :=
      T{operation := Operation.DeleteIndex, args :=
        ARRAY [0 .. 4] OF CARDINAL{node.graph, node.entity, indexno, 0, 0},
        text := value, length := length, from := 0, to := 0};
  END DeleteIndex;

PROCEDURE CreateEdge (VAR com: T; source, target: Node.T; label: CARDINAL) =
  BEGIN
    com := T{operation := Operation.CreateEdge, args :=
             ARRAY [0 .. 4] OF
               CARDINAL{source.graph, source.entity, target.graph,
                        target.entity, label}, text := NIL, length := 0,
             from := 0, to := 0};
  END CreateEdge;

PROCEDURE DeleteEdge (VAR com: T; source, target: Node.T; label: CARDINAL) =
  BEGIN
    com := T{operation := Operation.DeleteEdge, args :=
             ARRAY [0 .. 4] OF
               CARDINAL{source.graph, source.entity, target.graph,
                        target.entity, label}, text := NIL, length := 0,
             from := 0, to := 0};
  END DeleteEdge;


BEGIN
  patternTable[Operation.NoOperation] := ArgPattern{0, FALSE, FALSE};
  patternTable[Operation.CreateNode] := ArgPattern{3, FALSE, FALSE};
  patternTable[Operation.DeleteNode] := ArgPattern{2, FALSE, FALSE};
  patternTable[Operation.PutNodeLabel] := ArgPattern{3, FALSE, FALSE};
  patternTable[Operation.PutAttribute] := ArgPattern{3, TRUE, TRUE};
  patternTable[Operation.TruncateAttribute] := ArgPattern{4, FALSE, FALSE};
  patternTable[Operation.PutIndex] := ArgPattern{3, TRUE, FALSE};
  patternTable[Operation.DeleteIndex] := ArgPattern{3, TRUE, FALSE};
  patternTable[Operation.CreateEdge] := ArgPattern{5, FALSE, FALSE};
  patternTable[Operation.DeleteEdge] := ArgPattern{5, FALSE, FALSE};
  patternTable[Operation.DeleteAttribute] := ArgPattern{3, FALSE, FALSE};
END GraphCommand.
