MODULE SchemeTestCrash EXPORTS Main;
(**
  - Schema declaration
  - List buildup
  - Traverse list printing all Attributes
  - Dependencies of derived attributes
  - Changing a derived attribute value
  - Searching nodes with index- and key values
  - Tracing the computation of derived attributes
  - Test the trace for Index-queries of non-valid
    derived attribute        *)

FROM RGGlobal IMPORT AttributeNumber, Cardinality, DependencyKind,
                     ExternNumber, GraphNumber, GraphType, GraphMode,
                     GroupType, GroupMode, NodeNumber, SimpleSet,
                     SchemeNumber, TStatus, TypeNumber, GraphPoolMode;

FROM RGRASGraph IMPORT AGAppendNodeType, AGBindEvaluationFunction,
                       AGCloseGraph, AGOpenGraph, AGCreateEdge,
                       AGCreateNode, AGCommitScheme, AGOpenGraphPool,
                       AGDeclareAttribute, AGDeclareDependency,
                       AGDeclareEdgeType, AGDeclareNodeClass,
                       AGDeclareNodeType, AGDeclareScheme, AGDeleteGraph,
                       AGDeleteGraphPool, AGGetAttributeSubstr, AGLogin,
                       AGLogout, AGPutAttributeSubstr, AGSetCreate,
                       AGSetEvaluationFunction, AGSetErrorCheckMode,
                       AGShowAllNodesWithIndex,
                       AGShowSourceNode, AGTestIncomingEdge,
                       AGTestAndShowTargetNode, AGStartTransaction,
                       AGCommitTransaction, AGDeleteScheme;

IMPORT IO, Fmt, Word, Text;
FROM Scheme IMPORT AttributeKind, IndexProperties;

CONST IntegerValue = 2;

VAR
  Node                               : TypeNumber;
  IntrAttr, KeyAttr, DerAttr, IndAttr: TypeNumber;
  Next                               : TypeNumber;
  NodeType1, NodeType2               : TypeNumber;

PROCEDURE TextToInt (t: TEXT): INTEGER =
  VAR
    res: INTEGER := 0;
    buf          := ARRAY [0 .. 3] OF CHAR{'\000', '\000', '\000', '\000'};
  BEGIN
    IF t # NIL THEN
      Text.SetChars(buf, t);
      res := Word.Insert(res, ORD(buf[0]), 0, 8);
      res := Word.Insert(res, ORD(buf[1]), 8, 8);
      res := Word.Insert(res, ORD(buf[2]), 16, 8);
      res := Word.Insert(res, ORD(buf[3]), 24, 8);
    END;
    RETURN res;
  END TextToInt;

PROCEDURE IntToText (val: INTEGER): TEXT =
  VAR buf: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    buf[0] := VAL(Word.Extract(val, 0, 8), CHAR);
    buf[1] := VAL(Word.Extract(val, 8, 8), CHAR);
    buf[2] := VAL(Word.Extract(val, 16, 8), CHAR);
    buf[3] := VAL(Word.Extract(val, 24, 8), CHAR);
    RETURN Text.FromChars(buf);
  END IntToText;

(******************************************************************************)
PROCEDURE FctOne (CurrGraph    : GraphNumber;
                  CurrNode     : NodeNumber;
                  CurrAttribute: AttributeNumber) =
  VAR AttrValue: INTEGER;

  BEGIN
    AttrValue := 1;
    AGPutAttributeSubstr(CurrGraph, CurrNode, CurrAttribute, 0,
                         BYTESIZE(AttrValue), IntToText(AttrValue), TRUE);
  END FctOne;

(*******************************************************************************)
PROCEDURE FctTwo (CurrGraph    : GraphNumber;
                  CurrNode     : NodeNumber;
                  CurrAttribute: AttributeNumber) =
  VAR AttrValue: INTEGER;

  BEGIN
    AttrValue := 2;
    AGPutAttributeSubstr(CurrGraph, CurrNode, CurrAttribute, 0,
                         BYTESIZE(AttrValue), IntToText(AttrValue), TRUE);
  END FctTwo;

(*******************************************************************************)
PROCEDURE FctInc (CurrGraph    : GraphNumber;
                  CurrNode     : NodeNumber;
                  CurrAttribute: AttributeNumber) =
  VAR
    SourceNode                 : NodeNumber;
    CurrAttrValue, SrcAttrValue: INTEGER;
    ReturnedLength             : INTEGER;
    value                      : TEXT;
  BEGIN
    IF AGTestIncomingEdge(CurrGraph, CurrNode, Next) THEN
      AGShowSourceNode(CurrGraph, CurrNode, Next, SourceNode);
      AGGetAttributeSubstr(CurrGraph, SourceNode, CurrAttribute, 0,
                           BYTESIZE(SrcAttrValue), value, ReturnedLength);
      IF ReturnedLength = 0 THEN
        IO.Put("No valid attribute there !!");
        IO.Put("\n");
        <* ASSERT FALSE *>
      END;
      SrcAttrValue := TextToInt(value);
      CurrAttrValue := SrcAttrValue + 1;

      AGPutAttributeSubstr(
        CurrGraph, CurrNode, CurrAttribute, 0, BYTESIZE(CurrAttrValue),
        IntToText(CurrAttrValue), TRUE);

      IO.Put("=====> DerivedNo-Attribute from node");
      IO.Put(Fmt.Pad(Fmt.Int(CurrNode.entity), 4));
      IO.Put(" computed :");
      IO.Put(Fmt.Pad(Fmt.Int(CurrAttrValue), 4));
      IO.Put("\n");
    ELSE
      CurrAttrValue := 1;

      AGPutAttributeSubstr(
        CurrGraph, CurrNode, CurrAttribute, 0, BYTESIZE(CurrAttrValue),
        IntToText(CurrAttrValue), TRUE);

      IO.Put("=====> DerivedNo-Attribute from node ");
      IO.Put(Fmt.Pad(Fmt.Int(CurrNode.entity), 4));
      IO.Put(" initialised : ");
      IO.Put(Fmt.Pad(Fmt.Int(CurrAttrValue), 4));
      IO.Put("\n");
    END;
  END FctInc;

(*******************************************************************************)
PROCEDURE FctMod (CurrGraph    : GraphNumber;
                  CurrNode     : NodeNumber;
                  CurrAttribute: AttributeNumber) =
  VAR
    AttrValue     : INTEGER;
    ReturnedLength: INTEGER;
    value         : TEXT;
  BEGIN
    AGGetAttributeSubstr(CurrGraph, CurrNode, DerAttr, 0,
                         BYTESIZE(AttrValue), value, ReturnedLength);

    IF ReturnedLength = 0 THEN
      IO.Put("No valid attribute there !!");
      IO.Put("\n");
      <* ASSERT FALSE *>
    END;
    AttrValue := TextToInt(value);

    (* A forbidden index query to check, whether GRAS detects this
       this error during runtime.     *)
    AGSetCreate(CurrGraph, NodeSet);
    AGShowAllNodesWithIndex(CurrGraph, IndAttr, 4, IntToText(2), NodeSet);

    AttrValue := AttrValue MOD 5;

    AGPutAttributeSubstr(CurrGraph, CurrNode, CurrAttribute, 0,
                         BYTESIZE(AttrValue), IntToText(AttrValue), TRUE);

    IO.Put("=======> IndexNo-Attribute from node");
    IO.Put(Fmt.Pad(Fmt.Int(CurrNode.entity), 4));
    IO.Put(" computed :");
    IO.Put(Fmt.Pad(Fmt.Int(AttrValue), 4));
    IO.Put("\n");
  END FctMod;

(*******************************************************************************)
PROCEDURE InitScheme () =
  VAR Scheme: SchemeNumber;

  BEGIN
    AGDeclareScheme("Scheme", Scheme, "SchemePool2");

    (** node_class NODE *)
    AGDeclareNodeClass(Scheme, "NODE", Node);

    (** intrinisic IntrinsicNo := 1; *)
    AGDeclareAttribute(
      Scheme, "NODE", "IntrinsicNo", AttributeKind.Intrinsic,
      IndexProperties.Normal, IntegerValue, Cardinality.OblUnique, TRUE,
      BYTESIZE(INTEGER), IntrAttr);
    AGSetEvaluationFunction(Scheme, "NODE", "IntrinsicNo", "FctOne");

    (** intrinsic key KeyNo : integer; *)
    AGDeclareAttribute(
      Scheme, "NODE", "KeyNo", AttributeKind.Intrinsic,
      IndexProperties.Key, IntegerValue, Cardinality.OblUnique, TRUE,
      BYTESIZE(INTEGER), KeyAttr);

    (** derived DerivedNo : integer = [ self.<-Next-.DerivedNo | 0 ] + 1; *)
    AGDeclareAttribute(
      Scheme, "NODE", "DerivedNo", AttributeKind.Derived,
      IndexProperties.Normal, IntegerValue, Cardinality.OblUnique, TRUE,
      BYTESIZE(INTEGER), DerAttr);

    (** derived index IndexNo = self.DerivedNo % 5; *)
    AGDeclareAttribute(
      Scheme, "NODE", "IndexNo", AttributeKind.Derived,
      IndexProperties.Index, IntegerValue, Cardinality.OblUnique, TRUE,
      BYTESIZE(INTEGER), IndAttr);
    AGDeclareDependency(Scheme, "NODE", "IndexNo", "DerivedNo", "",
                        DependencyKind.SelfDependent);
    AGSetEvaluationFunction(Scheme, "NODE", "IndexNo", "FctMod");

    (** edge_type Next : NODE [0:1] -> NODE [0:1]; *)
    AGDeclareEdgeType(Scheme, "NEXT", "NODE", Cardinality.OptUnique,
                      "NODE", Cardinality.OptUnique, Next);

    AGDeclareDependency(Scheme, "NODE", "DerivedNo", "DerivedNo", "NEXT",
                        DependencyKind.IncomingDependent);
    AGSetEvaluationFunction(Scheme, "NODE", "DerivedNo", "FctInc");

    (** node_type NodeType1 : NODE end; *)
    AGDeclareNodeType(Scheme, "NodeType1", NodeType1);
    AGAppendNodeType(Scheme, "NodeType1", "NODE");

    (** node_Type NodeType2 : NODE *)
    (** redef_intrinsic IntrinsicNo := 2; *)
    (** end; *)
    AGDeclareNodeType(Scheme, "NodeType2", NodeType2);
    AGAppendNodeType(Scheme, "NodeType2", "NODE");
    AGSetEvaluationFunction(Scheme, "NodeType2", "IntrinsicNo", "FctTwo");

    (** Scheme is ready defined and could be used *)
    AGCommitScheme(Scheme);
  END InitScheme;


(*******************************************************************************)
PROCEDURE BindEvaluationFunctions () =
  BEGIN
    AGBindEvaluationFunction(Graph, "FctOne", FctOne);
    AGBindEvaluationFunction(Graph, "FctInc", FctInc);
    AGBindEvaluationFunction(Graph, "FctMod", FctMod);
    AGBindEvaluationFunction(Graph, "FctTwo", FctTwo);
  END BindEvaluationFunctions;

(*******************************************************************************)
PROCEDURE WriteStatus (Status: TStatus) =
  BEGIN
    IO.Put("STATUS:  ");
    CASE Status OF
      TStatus.NoError => IO.Put("No Error");
    | TStatus.AlreadyExistent => IO.Put("Already Existent");
    | TStatus.AlreadyOpen => IO.Put("Already Open");
    | TStatus.NotExistent => IO.Put("Not Existent");
    | TStatus.NotEmpty => IO.Put("Not Empty");

    | TStatus.StillPendingTransaction => IO.Put("Pending Transaction");
    | TStatus.NoScheme => IO.Put("No scheme");
    | TStatus.StillOpen => IO.Put("Still open");
    | TStatus.StillUsed => IO.Put("Still in use");

    END;                         (** CASE *)
    IO.Put("\n");
  END WriteStatus;


(*******************************************************************************)
PROCEDURE CreateFirstNodePair (    Graph    : GraphNumber;
                               VAR FirstNode: NodeNumber;
                               VAR LastNode : NodeNumber   ) =
  VAR AttrValue: INTEGER;

  BEGIN
    (** production CreateFirstNodePair (out LastNode : NODE ) *)
    (** = *)
    (** ::= *)
    (** *)
    (** begin *)
    (** 1' : NodeType1; *)
    (** 2' : NodeType2; *)
    (** 1' -> 2' : Next; *)
    (** end *)
    (** *)
    (** transfer 1'.KeyNo := 1; *)
    (** transfer 1'.KeyNo := 2; *)
    (** *)
    (** return LastNode := 2'; *)
    (** end; *)

    FirstNode.graph := ExtNumber;
    FirstNode.entity := 0;
    LastNode.graph := ExtNumber;
    LastNode.entity := 0;
    AGCreateNode(Graph, NodeType1, FirstNode, FirstNode);
    AGCreateNode(Graph, NodeType2, LastNode, LastNode);
    AGCreateEdge(Graph, FirstNode, LastNode, Next);

    AttrValue := 1;
    AGPutAttributeSubstr(Graph, FirstNode, KeyAttr, 0, BYTESIZE(AttrValue),
                         IntToText(AttrValue), TRUE);

    AttrValue := 2;
    AGPutAttributeSubstr(Graph, LastNode, KeyAttr, 0, BYTESIZE(AttrValue),
                         IntToText(AttrValue), TRUE);

  END CreateFirstNodePair;



(*******************************************************************************)
PROCEDURE CreateNextNodePair (    Graph      : GraphNumber;
                                  LastNode   : NodeNumber;
                              VAR NewLastNode: NodeNumber   ) =
  VAR
    Node1, Node2, Node3: NodeNumber;
    AttrValue          : INTEGER;
    ReturnedLength     : INTEGER;
    value              : TEXT;
  BEGIN
    (** production CreateNextNodePair (LastNode : NODE; out
       NewLastNode:NODE) *)
    (** = *)
    (** begin *)
    (** '1 : NodeType2; *)
    (** end *)
    (** *)
    (** ::= *)
    (** *)
    (** begin *)
    (** 1' = '1 *)
    (** 2' : NodeType1; *)
    (** 3' : NodeType2; *)
    (** 1' -> 2' : Next; *)
    (** 2' -> 3' : Next; *)
    (** end *)
    (** *)
    (** transfer 2'.KeyNo := '1.KeyNo + 1; *)
    (** transfer 3'.KeyNo := '1.KeyNo + 2; *)
    (** *)
    (** return NewLastNode := 3'; *)
    (** end; *)

    Node1 := LastNode;

    AGCreateNode(Graph, NodeType1, Node1, Node2);
    AGCreateNode(Graph, NodeType2, Node2, Node3);
    AGCreateEdge(Graph, Node1, Node2, Next);
    AGCreateEdge(Graph, Node2, Node3, Next);

    AGGetAttributeSubstr(
      Graph, Node1, KeyAttr, 0, BYTESIZE(AttrValue), value, ReturnedLength);

    IF ReturnedLength = 0 THEN
      IO.Put("No valid attribute there !!");
      IO.Put("\n");
      <* ASSERT FALSE *>
    END;
    AttrValue := TextToInt(value);

    AttrValue := AttrValue + 1;
    AGPutAttributeSubstr(Graph, Node2, KeyAttr, 0, BYTESIZE(AttrValue),
                         IntToText(AttrValue), TRUE);

    AttrValue := AttrValue + 1;
    AGPutAttributeSubstr(Graph, Node3, KeyAttr, 0, BYTESIZE(AttrValue),
                         IntToText(AttrValue), TRUE);

    NewLastNode := Node3;
  END CreateNextNodePair;

(******************************************************************************)
PROCEDURE PrintNode (Graph: GraphNumber; Node: NodeNumber) =
  VAR
    AttrValue     : INTEGER;
    ReturnedLength: INTEGER;
    value         : TEXT;
  BEGIN
    IO.Put("Nodenumber : ");
    IO.Put(Fmt.Pad(Fmt.Int(Node.entity), 4));
    IO.Put("\n");

    (** KeyNo - Attribute *)
    AGGetAttributeSubstr(
      Graph, Node, KeyAttr, 0, BYTESIZE(AttrValue), value, ReturnedLength);

    IF ReturnedLength = 0 THEN
      IO.Put("No valid attribute there !!");
      IO.Put("\n");
      <* ASSERT FALSE *>
    END;
    AttrValue := TextToInt(value);
    IO.Put("     KeyNo :       ");
    IO.Put(Fmt.Pad(Fmt.Int(AttrValue), 4));
    IO.Put("\n");

    (** IntrinsicNo - Attribute *)
    AGGetAttributeSubstr(
      Graph, Node, IntrAttr, 0, BYTESIZE(AttrValue), value, ReturnedLength);
    IF ReturnedLength = 0 THEN
      IO.Put("No valid attribute there !!");
      IO.Put("\n");
      <* ASSERT FALSE *>
    END;
    AttrValue := TextToInt(value);
    IO.Put("     IntrinsicNo : ");
    IO.Put(Fmt.Pad(Fmt.Int(AttrValue), 4));
    IO.Put("\n");

    (** DerivedNo - Attribute *)
    AGGetAttributeSubstr(
      Graph, Node, DerAttr, 0, BYTESIZE(AttrValue), value, ReturnedLength);
    IF ReturnedLength = 0 THEN
      IO.Put("No valid attribute there !!");
      IO.Put("\n");
      <* ASSERT FALSE *>
    END;
    AttrValue := TextToInt(value);

    IO.Put("     DerivedNo :   ");
    IO.Put(Fmt.Pad(Fmt.Int(AttrValue), 4));
    IO.Put("\n");

    (** IndexNo - Attribute *)
    AGGetAttributeSubstr(
      Graph, Node, IndAttr, 0, BYTESIZE(AttrValue), value, ReturnedLength);
    IF ReturnedLength = 0 THEN
      IO.Put("No valid attribute there !!");
      IO.Put("\n");
      <* ASSERT FALSE *>
    END;
    AttrValue := TextToInt(value);

    IO.Put("     IndexNo :     ");
    IO.Put(Fmt.Pad(Fmt.Int(AttrValue), 4));
    IO.Put("\n");
  END PrintNode;


(******************************************************************************)

PROCEDURE PrintList (Graph: GraphNumber; FirstNode: NodeNumber) =
  VAR
    CurrNode  : NodeNumber;
    TargetNode: NodeNumber;
    TargetNrs : CARDINAL;

  BEGIN
    IO.Put("\n");
    IO.Put(" The content of the whole list :");
    IO.Put("\n");
    IO.Put("================================");
    IO.Put("\n");

    TargetNode := FirstNode;
    REPEAT
      (** get next node *)
      CurrNode := TargetNode;

      PrintNode(Graph, CurrNode);
      IO.Put("\n");

      (** test if there is a next node *)
      AGTestAndShowTargetNode(Graph, CurrNode, Next, TargetNrs, TargetNode);
    UNTIL TargetNrs # 1;

    IF TargetNrs # 0 THEN
      IO.Put(" A node in the list has more then one successor !");
      <* ASSERT FALSE *>
    END;
    IO.Put("\n");

  END PrintList;

(*******************************************************************************)
VAR
  ExtNumber: ExternNumber;
  FirstNode: NodeNumber;
  Graph    : GraphNumber;
  Type     : GraphType;
  LastNode : NodeNumber;
  NodeSet  : SimpleSet;
  Status   : TStatus;

BEGIN
  (** Start the GRAS-System *)
  AGLogin();

  AGOpenGraphPool("SchemePool2", GraphPoolMode.NewPool, Status);
  IF Status # TStatus.NoError THEN WriteStatus(Status); END;

  (** create the scheme for the graph "List" *)
  AGStartTransaction("SchemePool2");
  InitScheme();
  AGCommitTransaction("SchemePool2");
  Type := 1;
  AGOpenGraph("SchemePool2", "List", Type, GraphMode.NewGraph, "",
              GroupType.Operation, GroupMode.NewGroup, TRUE, FALSE, Status,
              ExtNumber, Graph, "Scheme");
  IF Status # TStatus.NoError THEN WriteStatus(Status); END;

  AGStartTransaction("SchemePool2");
  (** activate error-checking *)
  AGSetErrorCheckMode(Graph, TRUE);
  BindEvaluationFunctions();


  (**                                                                 *)
  (**   transaction MAIN                                              *)
  (**   =                                                             *)
  (**      use n := 20;                                               *)
  (**          Last : NODE                                            *)
  (**      do                                                         *)
  (**           CreateFirstNodePair( out Last )                       *)
  (**         & loop                                                  *)
  (**              when valid (n > 0)                                 *)
  (**              then                                               *)
  (**                   CreateNextNodePair( Last , out Last )         *)
  (**                 & n := (n - 1)                                  *)
  (**           end                                                   *)
  (**      end                                                        *)
  (**   end;                                                          *)
  (**                                                                 *)

  CreateFirstNodePair(Graph, FirstNode, LastNode);

  FOR i := 1 TO 5 DO CreateNextNodePair(Graph, LastNode, LastNode); END;

  (**                  *)
  (** testing the list *)
  (**                  *)

  (** write the content of the list to screen, so that the evaluation cycle is triggered *)
  PrintList(Graph, FirstNode);
 (* the same effect happens:                                 *)
 (* AGShowAllNodesWithIndex( Graph, IndAttr, 4, IntToText(2), NodeSet); *)

  AGCommitTransaction("SchemePool2");

  (**                                                 *)
  (** delete the whole graph and graphpool afterwards *)
  (**                                                 *)

  AGCloseGraph(Graph);

  AGDeleteGraph("SchemePool2", "List", FALSE, Status);
  IF Status # TStatus.NoError THEN WriteStatus(Status); END;

  AGDeleteScheme("SchemePool2", "Scheme", Status);
  IF Status # TStatus.NoError THEN WriteStatus(Status); END;

  AGDeleteGraphPool("SchemePool2", Status);
  IF Status # TStatus.NoError THEN WriteStatus(Status); END;

  AGLogout();
END SchemeTestCrash.
