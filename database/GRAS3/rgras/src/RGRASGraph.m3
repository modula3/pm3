MODULE RGRASGraph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:53  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:44  hosking
    Import of GRAS3 1.1

    Revision 1.13  1998/07/29 09:30:50  roland
    New parameter profile for AGLogin.

    Revision 1.12  1998/07/27 13:31:04  roland
    Procedure AGDeleteScheme added.

    Revision 1.11  1998/05/15 12:22:01  renehuel
    Adapted the RGRASGraph interface to handle events.

    Revision 1.10  1998/03/18 13:42:11  roland
    Adapted to 'local' parameters.

    Revision 1.9  1998/03/18 12:13:42  kluck
    Further adaptions referring to local parameter because of RGRAS
    interface (local = FALSE per definition).

    Revision 1.8  1998/03/06 14:11:11  renehuel
    Slight change in AGDeleteGraphPool.
    Its not neccessary to look at the contents of the pool with force = TRUE.

    Revision 1.7  1998/02/24 15:46:27  renehuel
    AGDeleteGraphPool now has a new parameter force, which forces it to
    delete the pool, disregarding the presence of graphs and schemes.
    AGShowAllNodesWithIndex and AGShowNodeWithKez now have a paramter
    attLength, determining the maximum length of the given attribute
    value.
    New procedure AGFileSize, returning the size of a given file of a pool
    in pages.

    Revision 1.6  1998/01/14 17:52:29  roland
    AGAppendNodeType only for schemes.
    Bugfix in AGPutAttributeSubstr and AGDeclareDependency.

    Revision 1.5  1997/12/23 12:41:30  renehuel
    The parameters poolName, graphName and schemeName now dont use the
    type TEXT in the interface any more, but the type PoolName, GraphName
    and SchemeName.

    Revision 1.4  1997/10/31 17:13:12  roland
    Adapted to new RuleEngine.
    Revision 1.3  1997/10/24 14:39:10  renehuel
    These files implement the new RGRASGraph interface.

    Revision 1.2  1997/05/13 09:30:05  roland
    Interface cleaned up a little.
    Implementation uses current modul names.

    Revision 1.1  1996/09/13 15:44:41  walkmark
    Interface to use old GRAS Implementations with new GRAS (basic
    procedures) (hope it will work)

    Implemented (or translated from old gras) basic procedures,
    that means, all procedures excepting:
      Attributes,
      Event Handling,
      Schemes,
      Node Classes,
      Node Types,
      AGLogin.
*)
(***************************************************************************)

FROM RGGlobal IMPORT GraphPoolMode, TStatus, ExternNumber,
                     SimpleSet, GraphNumber, SimpleElement, GraphType,
                     GraphMode, GroupMode, GroupName, GroupType,
                     NodeNumber, TypeNumber, AttributeNumber, RelSet,
                     SchemeNumber, Cardinality, AttributeKind,
                     IndexProperties, ValueTypeNumber, EvalFunction,
                     DependencyKind, MaxIndexAttributeLength, SchemeName,
                     GraphName, PoolName, GraphEvent, ActionPriority,
                     ActionProc, EventKind;

IMPORT TypedGraph AS Graph;
IMPORT TypedGraphPool AS GraphPool;
IMPORT TypedGraphSystem AS GraphSystem;
IMPORT ChgMgmtGraph, PoolList, Access, PageFile, Process, ErrorSupport,
       GraphList, SchemeList, TextCursorSet, Env, ChgMgmtGraphPool, Fmt,
       Word, Random, CardSet, Scheme, NodeTypeRelation, NodeSet, Text,
       RGRASNames, Names, InternTypedGraphPool, IO, PersistentGraph, Node,
       AttributeValue, TypedNames, SetList, RelSetList, VirtualResource,
       RuleEngine, DaemonList, Action, Event, ContextSet, Trigger,
       GraphEventPattern, GraphEvents, EventType, LogEvents,
       LogEventPattern, VirtualPageEvent, VirtualPageEventPattern,
       EventPattern, EventTypes, IntSeq;

(* Some internal types *)

TYPE

  ActionProcRecord = <*TRANSIENT*> REF RECORD actionProc: ActionProc END;

  RGRASEvaluator =
    Graph.Evaluator OBJECT
      RGRASEvaluationFunction: PROCEDURE (p0: GraphNumber;
                                          p1: NodeNumber;
                                          p2: AttributeNumber);
    OVERRIDES
      apply := Apply;
    END;

PROCEDURE Apply (self : RGRASEvaluator;
                 graph: Graph.T;
                 node : Node.T;
                 attr : Scheme.ID       ) =
  <* FATAL GraphList.EntryNotInList *>
  BEGIN
    (* Checking if a graph with the given handle is in the graphlist. *)
    IF graphList.isEntryByHandle(graph) THEN
      (* Getting its internal number from the graphlist. *)
      WITH internalGraphNumber = graphList.getEntryByHandle(graph).number DO
        (* Calling the evaluation function, if it was assigned. *)
        IF self.RGRASEvaluationFunction # NIL THEN
          self.RGRASEvaluationFunction(internalGraphNumber, node, attr);
        END;
      END;
    END;
  END Apply;

(* Some internal procedures *)

PROCEDURE DisplayGraveStone (moduleName, procedureName, errorMessage,
                               contactName: TEXT) =
  PROCEDURE DisplayFormattedGraveStoneInscription (header, message: TEXT) =
    CONST
      headerLength      = 11;
      inscriptionLength = 50;
    VAR
      printMessage: TEXT;
      cuttingIndex: INTEGER;
    BEGIN
      (* Removing all newline characters and replacing them with spaces. *)
      WHILE Text.FindChar(message, '\n') # -1 DO
        message := Text.Sub(message, 0, Text.FindChar(message, '\n')) & " "
                     & Text.Sub(message, Text.FindChar(message, '\n') + 1,
                                Text.Length(message));
      END;
      (* Creating the header like : " Error : " *)
      header :=
        Fmt.Pad(header, headerLength - 2, ' ', Fmt.Align.Left) & ": ";
      REPEAT
        (* The point of this algorithm is to find an index in the
           errormessage with a space character.  Then one can cut off the
           part of the text before that character, if the maximum length is
           not exceeded, and add it to the line which is to be printed
           next.*)
        IO.Put("*** ");
        (* Beginning of a new line. *)
        printMessage := "";
        (* No spaces found yet. *)
        cuttingIndex := -1;
        (* As long as the cut off word and the already collected line do
           not exceed the maximum length, we continue to find a new cut
           index.*)
        WHILE (cuttingIndex + Text.Length(printMessage) < inscriptionLength)
                AND (Text.Length(message) > 0) DO
          (* Cutting the first word off the message, and adding it to the
             line which is to be printed next. *)
          printMessage :=
            printMessage & Text.Sub(message, 0, cuttingIndex + 1);
          message :=
            Text.Sub(message, cuttingIndex + 1, Text.Length(message));
          (* Now looking for a new index where we can cut off a word. *)
          cuttingIndex := Text.FindChar(message, ' ');
          (* -1 indicates that no more spaces were found.  That means the
             whole message can be selected as next word. *)
          IF cuttingIndex = -1 THEN
            cuttingIndex := Text.Length(message);
          END;
          (* If the selected word is too long, then it is reduced to the
             maximun length. *)
          IF cuttingIndex > inscriptionLength - 1 THEN
            cuttingIndex := inscriptionLength - 1;
          END;
        END;
        (* Now formatting and printing the message, and setting the header
           to " ...  " *)
        printMessage := Fmt.Pad(printMessage, 50, ' ', Fmt.Align.Left);
        IO.Put(Fmt.Pad(header, headerLength - 2, ' ', Fmt.Align.Left)
                 & printMessage);
        IO.Put("***\n");
        header :=
          Fmt.Pad("", headerLength - 2, ' ', Fmt.Align.Left) & "  ";
      UNTIL Text.Equal(message, "");
    END DisplayFormattedGraveStoneInscription;

  (* This procedure simply displays the well known grave stone with the
     information about the error that occured. *)
  BEGIN
    IO.Put(
      "\n                     **************************                     \n");
    IO.Put(
      "               ******                          ******               \n");
    IO.Put(
      "         ******               R. I. P.               ******         \n");
    IO.Put(
      "   ******                                                  ******   \n");
    IO.Put(
      "***                       R G R A S - 9 8                        ***\n");
    IO.Put(
      "***                                                              ***\n");
    IO.Put(
      "***--------------------------------------------------------------***\n");
    IO.Put(
      "***                                                              ***\n");
    IO.Put(
      "***   Copyright 1989-1994 by:                                    ***\n");
    IO.Put(
      "***     Lehrstuhl Informatik III                                 ***\n");
    IO.Put(
      "***     RWTH Aachen                                              ***\n");
    IO.Put(
      "***                                                              ***\n");
    IO.Put(
      "***--------------------------------------------------------------***\n");
    DisplayFormattedGraveStoneInscription("Module", moduleName);
    DisplayFormattedGraveStoneInscription("Procedure", procedureName);
    DisplayFormattedGraveStoneInscription("Error", errorMessage);
    DisplayFormattedGraveStoneInscription("Contact", contactName);
    IO.Put(
      "***                                                              ***\n");
    IO.Put(
      "********************************************************************\n");
  END DisplayGraveStone;

PROCEDURE ErrorMessage (procedure, message: TEXT; halt: BOOLEAN := FALSE) =
  BEGIN
    (* prints a grave stone with the appropriate information. *)
    DisplayGraveStone(
      "RGRASGraph.m3", procedure, message, contactName := "Roland Baumann");
    IF halt THEN Process.Exit(0); END;
  END ErrorMessage;

PROCEDURE ConvertToNewCardinality (ca: Cardinality): Scheme.Cardinality =
  BEGIN
    (* converts the old cardinality to the new *)
    CASE ca OF
    | Cardinality.OptUnique => RETURN Scheme.AtMostOneCard;
    | Cardinality.OblUnique => RETURN Scheme.ExactlyOneCard;
    | Cardinality.OptSet => RETURN Scheme.ArbitraryCard;
    | Cardinality.OblSet => RETURN Scheme.AtLeastOneCard;
    END;
  END ConvertToNewCardinality;

PROCEDURE ConvertToOldCardinality (ca: Scheme.Cardinality): Cardinality =
  VAR result: Cardinality;
  BEGIN
    (* converts the new cardinality to the old *)
    IF ca = Scheme.AtMostOneCard THEN
      result := Cardinality.OptUnique
    ELSE
      IF ca = Scheme.ExactlyOneCard THEN
        result := Cardinality.OblUnique
      ELSE
        IF ca = Scheme.ArbitraryCard THEN
          result := Cardinality.OptSet
        ELSE
          IF ca = Scheme.AtLeastOneCard THEN
            result := Cardinality.OblSet
          ELSE
            ErrorMessage("ConvertToOldCardinality",
                         "Cannot convert this type of cardinality!")
          END;
        END;
      END;
    END;
    RETURN result;
  END ConvertToOldCardinality;

PROCEDURE ConvertToNewDependencyKind (dependencyKind: DependencyKind):
  Scheme.DependencyKind =
  BEGIN
    (* converts the old dependencykind to the new *)
    CASE dependencyKind OF
    | DependencyKind.SelfDependent => RETURN Scheme.DependencyKind.Self;
    | DependencyKind.IncomingDependent =>
        RETURN Scheme.DependencyKind.Incoming;
    | DependencyKind.OutgoingDependent =>
        RETURN Scheme.DependencyKind.Outgoing;
    END;
  END ConvertToNewDependencyKind;

PROCEDURE GetOpenGraph (    graphNumber         : GraphNumber;
                            callingProcedureName: TEXT;
                        VAR graph               : Graph.T      ): BOOLEAN =
  <* FATAL GraphList.EntryNotInList *>
  BEGIN
    (* This function is used to return a handle of an already open
       graph. *)
    (* First it is checked whether the graph is in the graphList. *)
    IF graphList.isEntry(graphNumber) THEN
      (* When the graph could be found in the list, it is open, and the
         handle can be received from the list. *)
      graph := graphList.getEntry(graphNumber).handle;
      RETURN TRUE;
    ELSE
      (* If the graph is not in the list, it is not open, and so it can not
         be referred to by an internal graphnumber.  An errormessage is
         displayed. *)
      ErrorMessage(callingProcedureName, "The graph is not open!");
      RETURN FALSE;
    END;
  END GetOpenGraph;

PROCEDURE GetOpenScheme (    schemeNumber        : SchemeNumber;
                             callingProcedureName: TEXT;
                         VAR scheme              : Scheme.T      ):
  BOOLEAN =
  <* FATAL SchemeList.EntryNotInList *>
  BEGIN
    (* This functions is used to return a handle of an already open
       scheme. *)
    (* First it is checked whether the scheme is in the schemeList. *)
    IF schemeList.isEntry(schemeNumber) THEN
      (* When the scheme could be found in the list, it is open, and the
         handle can be received from the list. *)
      scheme := schemeList.getEntry(schemeNumber).handle;
      RETURN TRUE;
    ELSE
      (* If the scheme is not in the list, it is not open, and so it can
         not be referred to by an internal schemenumber.  An errormessage
         is displayed. *)
      ErrorMessage(callingProcedureName, "The scheme is not open!");
      RETURN FALSE;
    END;
  END GetOpenScheme;

PROCEDURE GetOpenPool (    poolName            : PoolName;
                           callingProcedureName: TEXT;
                       VAR pool                : GraphPool.T): BOOLEAN =
  <* FATAL PoolList.EntryNotInList *>
  BEGIN
    (* This functions is used to return a handle of an already open
       pool. *)
    (* First it is checked whether the pool is in the poolList. *)
    IF poolList.isEntry(poolName) THEN
      (* When the pool could be found in the list, it is open, and the
         handle can be received from the list. *)
      pool := poolList.getEntry(poolName).handle;
      RETURN TRUE;
    ELSE
      (* If the pool is not in the list, it is not open.  An errormessage
         is displayed. *)
      ErrorMessage(callingProcedureName, "The pool is not open!");
      RETURN FALSE;
    END;
  END GetOpenPool;

PROCEDURE GetScheme (    graphOrSchemeNumber : GraphNumber;
                         isGraphNumber       : BOOLEAN;
                         callingProcedureName: TEXT;
                     VAR scheme              : Scheme.T     ): BOOLEAN =
  VAR
    existent: BOOLEAN := FALSE;
    graph   : Graph.T;
  BEGIN
    (* This function is used to get a handle to a scheme over the internal
       schemeNumber, or the graphNumber and the function getScheme().*)
    IF isGraphNumber THEN
      (* If the first parameter is a graph number, then the graph must be
         open, an can be read from the graphList. *)
      existent :=
        GetOpenGraph(graphOrSchemeNumber, callingProcedureName, graph);
      (* If the graph was in the list, the scheme can be received using the
         getScheme method of the graph. *)
      IF existent THEN scheme := graph.getScheme(); END;
    ELSE
      (* If the first parameter was a scheme number, then the scheme has to
         be open, and can be received from the schemeList. *)
      existent :=
        GetOpenScheme(graphOrSchemeNumber, callingProcedureName, scheme);
    END;
    RETURN existent;
  END GetScheme;

PROCEDURE ConvertCardSetToNodeSet (cs                  : CardSet.T;
                                   graphNumber         : GraphNumber;
                                   callingProcedureName: TEXT         ):
  SimpleSet =
  <* FATAL GraphList.EntryNotInList, PoolList.EntryNotInList *>
  VAR
    set      : NodeSet.T           := NIL;
    node     : NodeNumber;
    oldNode  : CARDINAL;
    ok       : BOOLEAN;
    pool     : GraphPool.T;
    graphInfo: GraphList.GraphInfo;
    result   : SimpleSet           := 0;
  BEGIN
    TRY
      IF cs # NIL THEN
        IF graphList.isEntry(graphNumber) THEN
          (* Retrieving the poolname of the graph from the graphlist. *)
          graphInfo := graphList.getEntry(graphNumber);
          (* Now getting a handle on the open pool. *)
          IF poolList.isEntry(graphInfo.poolName) THEN
            pool := poolList.getEntry(graphInfo.poolName).handle;
          ELSE
            pool :=
              NEW(GraphPool.T).open(
                graphInfo.poolName, defaultPoolAccessMode, new := FALSE);
          END;
          (* Creating a new empty set. *)
          set := NodeSet.New();
          (* Now reading the external scheme number of the graph.  This
             number is used as the 'graph' value of the nodes in the new
             nodeset. *)
          node.graph := pool.graphNumber(graphInfo.schemeName);
          cs.loop();
          oldNode := cs.get(ok);
          WHILE ok DO
            node.entity := oldNode;
            set.insert(node);
            oldNode := cs.get(ok);
          END;
          (* When the pool is not in the list, it has to be closed *)
          IF NOT poolList.isEntry(graphInfo.poolName) THEN
            pool.close()
          END;
          result := setList.addEntry(set);
        END;
      END;
    EXCEPT
    | Access.Denied (m) =>
        ErrorMessage(callingProcedureName, "Access.Denied : " & m);
    | Access.Locked => ErrorMessage(callingProcedureName, "Access.Locked");
    | PageFile.NoAccess (m) =>
        ErrorMessage(callingProcedureName, "PageFile.NoAccess : " & m);
    | GraphPool.InternalError (m) =>
        ErrorMessage(callingProcedureName,
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | ChgMgmtGraphPool.InternalError (m) =>
        ErrorMessage(callingProcedureName,
                     "ChgMgmtGraphPool.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | ChgMgmtGraphPool.NotExistent =>
        ErrorMessage(callingProcedureName, "ChgMgmtGraphPool.NotExistent");
    END;
    RETURN result;
  END ConvertCardSetToNodeSet;

PROCEDURE GetOpenPoolFromGraph (    graphNumber: GraphNumber;
                                VAR pool       : GraphPool.T  ): BOOLEAN
  RAISES {GraphList.EntryNotInList} = <* FATAL PoolList.EntryNotInList *>
  VAR poolName: PoolName;
  BEGIN
    (* The name of the pool can be read from the graphList. *)
    poolName := graphList.getEntry(graphNumber).poolName;
    (* The handle to the pool which has to be open can be received from the
       poolList. *)
    pool := poolList.getEntry(poolName).handle;
    RETURN pool # NIL;
  END GetOpenPoolFromGraph;

PROCEDURE CreateAndAddPool (poolName: PoolName; new: BOOLEAN) =
  <* FATAL PoolList.EntryAlreadyInList *>
  (* This procedure is responsible for opening a new pool and inserting it
     into the poolList. *)
  VAR
    newPool   : GraphPool.T;
    rgrasNames: RGRASNames.T;
  BEGIN
    TRY
      (* A new pool is opened with the given parameters. *)
      newPool :=
        NEW(GraphPool.T).openIntern(poolName, defaultPoolAccessMode, new);
      (* A new RGRASNames object is to be generated... *)
      rgrasNames := NEW(RGRASNames.T);
      (* ...  and the pool attached to it. *)
      newPool.loginToNames(rgrasNames);
      (* The RGRASNames object has a method which implements a unique
         counter. *)
      (* Now the names object is attached to the pool. *)
      newPool.beginTransaction();
      rgrasNames.login(newPool, ".GRAS");
      newPool.commitTransaction();

      (* The pool now can be entered into the poolList. *)
      poolList.addEntry(newPool, poolName);
    EXCEPT
    | Access.Denied (m) =>
        ErrorMessage("CreateAndAddPool", "Access.Denied : " & m);
    | Access.Locked => ErrorMessage("CreateAndAddPool", "Access.Locked");
    | PageFile.NoAccess (m) =>
        ErrorMessage("CreateAndAddPool", "PageFile.NoAccess : " & m);
    | GraphPool.InternalError (m) =>
        ErrorMessage("CreateAndAddPool",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | RGRASNames.InternalError (m) =>
        ErrorMessage("CreateAndAddPool", "RGRASNames.InternalError : "
                                           & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "CreateAndAddPool",
          "GraphPool.CardinalityError : pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("CreateAndAddPool", "GraphPool.NotInTransaction");
    END
  END CreateAndAddPool;

PROCEDURE CreateAndAddGraph (    graphName          : GraphName;
                                 new                : BOOLEAN;
                                 pool               : GraphPool.T;
                                 poolName           : PoolName;
                                 schemeName         : SchemeName;
                             VAR internalGraphNumber: GraphNumber;
                             VAR externalGraphNumber: ExternNumber;
                             VAR status             : TStatus       )
  RAISES {Access.Denied, Access.Locked, PageFile.NoAccess} =
  <* FATAL GraphList.EntryAlreadyInList *>
  (* This procedure is responsible for creating a new graph and inserting
     it into the graphList. *)
  VAR newGraph: Graph.T;
  BEGIN
    TRY
      (* If the specified scheme for a new graph does not exist, the status
         will be set do noScheme. *)
      IF new AND NOT pool.existsScheme(schemeName) THEN
        status := TStatus.NoScheme
      END;
      IF status = TStatus.NoError THEN
        (* The graph has to be opened. *)
        newGraph :=
          NEW(Graph.T).open(pool, graphName, defaultGraphAccessMode, new,
                            errorChecks := Graph.NoChecks, local := FALSE,
                            scheme := schemeName);
        (* The external number can be received from the pool to which the
           graph belongs. *)
        externalGraphNumber := pool.graphNumber(graphName);
        IF NOT new THEN schemeName := pool.getScheme(graphName) END;
        (* The internal number is returned by the addEntry method of the
           graphList. *)
        internalGraphNumber :=
          graphList.addEntry(newGraph, graphName, poolName, schemeName,
                             externalGraphNumber, internalGraphNumber);
      END;
    EXCEPT
    | Graph.InUse => ErrorMessage("CreateAndAddGraph", "Graph.InUse");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "CreateAndAddGraph",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NoScheme => status := TStatus.NoScheme;
    | Graph.NotExistent =>
        ErrorMessage("CreateAndAddGraph", "Graph.NotExistent");
    | ChgMgmtGraphPool.InternalError (m) =>
        ErrorMessage("CreateAndAddGraph",
                     "ChgMgmtGraphPool.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | ChgMgmtGraphPool.NotExistent =>
        ErrorMessage("CreateAndAddGraph", "ChgMgmtGraphPool.NotExistent");
    | GraphPool.InternalError (m) =>
        ErrorMessage("CreateAndAddGraph",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.NoGraph =>
        ErrorMessage("CreateAndAddGraph", "GraphPool.NoGraph");
    | GraphPool.NoScheme =>
        ErrorMessage("CreateAndAddGraph", "GraphPool.NoScheme");
    | GraphPool.NotExistent =>
        ErrorMessage("CreateAndAddGraph", "GraphPool.NotExistent");
    END;
  END CreateAndAddGraph;

PROCEDURE KillSetEntry (VAR set: SimpleSet) =
  <* FATAL SetList.EntryNotInList *>
  BEGIN
    IF setList.isEntry(set) THEN
      setList.getEntry(set).handle.dispose();
      setList.removeEntry(set);
    END;
    set := 0;
  END KillSetEntry;

PROCEDURE KillRelSetEntry (VAR relSet: RelSet) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(relSet) THEN
      relSetList.getEntry(relSet).handle.dispose();
      relSetList.removeEntry(relSet);
    END;
    relSet := 0;
  END KillRelSetEntry;

PROCEDURE ExecuteTriggeredAction (             e       : Event.T;
                                  <* UNUSED *> context : ContextSet.T;
                                  <* UNUSED *> local   : BOOLEAN;
                                               userdata: <*TRANSIENT*> REFANY) =

  PROCEDURE SetGraphEventInformation (VAR graphEvent       : GraphEvent;
                                          eventKind        : EventKind;
                                          TNo, TNode, SNode: NodeNumber;
                                      SNodeExists, TNodeExists: BOOLEAN) =
    BEGIN
      graphEvent.Kind := eventKind;
      graphEvent.TNo := TNo;
      graphEvent.TNode := TNode;
      graphEvent.TNodeExists := TNodeExists;
      graphEvent.SNode := SNode;
      graphEvent.SNodeExists := SNodeExists;
    END SetGraphEventInformation;
  PROCEDURE RetrieveSchemeNumber (e: Event.T): CARDINAL =
    VAR schemeNumber: CARDINAL := 0;
    BEGIN
      TRY
        pool := GraphEvents.GetPool(e);
        graph := GraphEvents.GetGraph(e);
        schemeNumber := pool.graphNumber(graph.getScheme().baseName());
      EXCEPT
        (* This exception is raised when e is not a graph event.  In this
           cases we do not need the scheme number, so let it be 0.*)
      | EventType.Unknown =>
      | Access.Locked =>
          ErrorMessage("ExecuteTriggeredAction", "Access.Locked");
      | ChgMgmtGraphPool.InternalError (e) =>
          ErrorMessage("ExecuteTriggeredAction",
                       "ChgMgmtGraphPool.InternalError : "
                         & ErrorSupport.ToText(e), halt := TRUE);
      | ChgMgmtGraphPool.NotExistent =>
          ErrorMessage(
            "ExecuteTriggeredAction", "ChgMgmtGraphPool.NotExistent");
      | EventType.Mismatch =>
          ErrorMessage("ExecuteTriggeredAction", "EventType.Mismatch");
      END;
      RETURN schemeNumber;
    END RetrieveSchemeNumber;
  VAR
    actionProcRecord        : ActionProcRecord;
    graphEvent              : GraphEvent;
    graphNumber             : GraphNumber      := 0;
    eventTypeName           : TEXT;
    eventType               : EventType.T;
    eventKind               : EventKind;
    nullNode                : NodeNumber       := NodeNumber{0, 0};
    TNo, SNode, TNode       : NodeNumber       := nullNode;
    SNodeExists, TNodeExists: BOOLEAN;
    schemeNumber            : CARDINAL;
    pool                    : GraphPool.T;
    graph                   : Graph.T;
  BEGIN
    TRY
      (* Getting the scheme number. *)
      schemeNumber := RetrieveSchemeNumber(e);
      (* Retrieving the record containing the action procedure from the
         userdata reference. *)
      actionProcRecord := userdata;
      (* Getting the name of the triggered event from the eventtypes
         storage. *)
      eventTypeName := EventTypes.Get(e.type()).getName();
      (* Switching over the event type name chosing how to set the
         properties TNode, Snode and TNo of the old style graphEvent. *)
      IF eventTypeName
           = GraphEvents.EventTypeName[GraphEvents.Operation.CreateNode] THEN
        TNo.entity := GraphEvents.GetNodeLabel(e);
        TNo.graph := schemeNumber;
        SNode := GraphEvents.GetNode(e);
        eventKind := EventKind.EVNewNode;
      ELSIF eventTypeName
              = GraphEvents.EventTypeName[GraphEvents.Operation.DeleteNode] THEN
        TNo.entity := GraphEvents.GetNodeLabel(e);
        TNo.graph := schemeNumber;
        SNode := GraphEvents.GetNode(e);
        eventKind := EventKind.EVDeleteNode;
      ELSIF eventTypeName
              = GraphEvents.EventTypeName[GraphEvents.Operation.CreateEdge] THEN
        TNo.entity := GraphEvents.GetEdgeLabel(e);
        TNo.graph := schemeNumber;
        SNode := GraphEvents.GetSourceNode(e);
        TNode := GraphEvents.GetTargetNode(e);
        eventKind := EventKind.EVNewEdge;
      ELSIF eventTypeName
              = GraphEvents.EventTypeName[GraphEvents.Operation.DeleteEdge] THEN
        TNo.entity := GraphEvents.GetEdgeLabel(e);
        TNo.graph := schemeNumber;
        SNode := GraphEvents.GetSourceNode(e);
        TNode := GraphEvents.GetTargetNode(e);
        eventKind := EventKind.EVDeleteEdge;
      ELSIF eventTypeName = GraphEvents.EventTypeName[
                              GraphEvents.Operation.PutAttribute] THEN
        eventKind := EventKind.EVModifyAttribute;
        TNo.entity := GraphEvents.GetAttributeNo(e);
        TNo.graph := schemeNumber;
        SNode := GraphEvents.GetNode(e);
      ELSIF eventTypeName = GraphEvents.EventTypeName[
                              GraphEvents.Operation.TruncateAttribute] THEN
        eventKind := EventKind.EVModifyAttribute;
        TNo.entity := GraphEvents.GetAttributeNo(e);
        TNo.graph := schemeNumber;
        SNode := GraphEvents.GetNode(e);
      ELSIF eventTypeName
              = GraphEvents.EventTypeName[GraphEvents.Operation.PutIndex] THEN
        eventKind := EventKind.EVIndexKeyModified;
        TNo.entity := GraphEvents.GetAttributeNo(e);
        TNo.graph := schemeNumber;
        SNode := GraphEvents.GetNode(e);
      ELSIF eventTypeName = GraphEvents.EventTypeName[
                              GraphEvents.Operation.DeleteIndex] THEN
        eventKind := EventKind.EVIndexKeyModified;
        TNo.entity := GraphEvents.GetAttributeNo(e);
        TNo.graph := schemeNumber;
        SNode := GraphEvents.GetNode(e);
      ELSIF eventTypeName = VirtualPageEvent.EventTypeName[
                              VirtualPageEvent.Operation.Begin] THEN
        eventKind := EventKind.EVTransactionStart;
      ELSIF eventTypeName = VirtualPageEvent.EventTypeName[
                              VirtualPageEvent.Operation.Commit] THEN
        eventKind := EventKind.EVTransactionEnd;
      ELSIF eventTypeName = VirtualPageEvent.EventTypeName[
                              VirtualPageEvent.Operation.Abort] THEN
        TNo.graph := 1;
        eventKind := EventKind.EVTransactionEnd;
      ELSIF eventTypeName = VirtualPageEvent.EventTypeName[
                              VirtualPageEvent.Operation.RemoteCommit] THEN
        eventKind := EventKind.EVTransactionEnd;
      ELSIF eventTypeName
              = LogEvents.EventTypeName[LogEvents.Operation.Checkpoint] THEN
        eventKind := EventKind.EVSetCheckpoint;
      ELSIF eventTypeName
              = LogEvents.EventTypeName[LogEvents.Operation.Undo] THEN
        TNo.entity := 1;
        eventKind := EventKind.EVUndo;
      ELSIF eventTypeName
              = LogEvents.EventTypeName[LogEvents.Operation.Redo] THEN
        TNo.entity := 1;
        eventKind := EventKind.EVRedo;
      ELSIF eventTypeName
              = LogEvents.EventTypeName[LogEvents.Operation.RedoNext] THEN
        TNo.entity := 1;
        eventKind := EventKind.EVRedo;
      ELSIF eventTypeName
              = LogEvents.EventTypeName[LogEvents.Operation.RedoPrev] THEN
        TNo.entity := 1;
        eventKind := EventKind.EVRedo;
      ELSIF eventTypeName
              = LogEvents.EventTypeName[LogEvents.Operation.RedoIth] THEN
        TNo.entity := LogEvents.GetSonNo(e);
        eventKind := EventKind.EVRedo;
      ELSIF eventTypeName
              = LogEvents.EventTypeName[LogEvents.Operation.Backstep] THEN
        (* nothing to be done *)
      ELSIF eventTypeName
              = LogEvents.EventTypeName[LogEvents.Operation.Forstep] THEN
        (* nothing to be done *)
      ELSIF eventTypeName = userDefinedEventTypeName THEN
        eventType :=
          EventTypes.Get(EventTypes.GetNumber(userDefinedEventTypeName));
        TNo.entity := e.getIntAttribute(eventType.getAttributeIndex("p1"));
        SNode.entity :=
          e.getIntAttribute(eventType.getAttributeIndex("p2"));
        TNode.entity :=
          e.getIntAttribute(eventType.getAttributeIndex("p3"));
        eventKind := EventKind.EVUserDefined;
      END;
      CASE eventKind OF
      | EventKind.EVNewNode, EventKind.EVDeleteNode, EventKind.EVNewEdge,
          EventKind.EVDeleteEdge, EventKind.EVModifyAttribute,
          EventKind.EVIndexKeyModified =>
          graphNumber := GraphEvents.GetGraphNo(e);
          IF (eventKind = EventKind.EVNewEdge)
               OR (eventKind = EventKind.EVDeleteEdge) THEN
            SNodeExists := GraphEvents.GetSourceNodeExists(e);
            TNodeExists := GraphEvents.GetTargetNodeExists(e);
          END;
      | EventKind.EVTransactionStart, EventKind.EVTransactionEnd =>
        (* Transactions are pool operations, so there is no graph
           number. *)
      | EventKind.EVSetCheckpoint, EventKind.EVUndo, EventKind.EVRedo =>
          graphNumber := LogEvents.GetGraphNo(e);
      | EventKind.EVOpenGraph, EventKind.EVCloseGraph =>
        (* These are not yet implemented *)
      | EventKind.EVUserDefined =>
        (* No graphnumber for user defined events. *)
      END;
      SetGraphEventInformation(
        graphEvent, eventKind, TNo, TNode, SNode, SNodeExists, TNodeExists);
      (* Calling the action procedure which was stored in the ruleengine
         with registering the trigger. *)
      actionProcRecord.actionProc(graphNumber, graphEvent);
    EXCEPT
    | EventType.Mismatch =>
        ErrorMessage("ExecuteTriggeredAction", "EventType.Mismatch");
    | EventType.Unknown =>
        ErrorMessage("ExecuteTriggeredAction", "EventType.Unknown");
    | EventTypes.Unknown =>
        ErrorMessage("ExecuteTriggeredAction", "EventTypes.Unknown");
    END;
  END ExecuteTriggeredAction;

PROCEDURE DefineUserEvent () =
  VAR userDefinedEventType: EventType.T;
  BEGIN
    TRY
      (* Creating a new user eventtype. *)
      userDefinedEventType :=
        EventTypes.NewEventType(userDefinedEventTypeName);
      (* Declaring 3 integer attributes for the 3 parameters of the user
         defined event. *)
      EVAL userDefinedEventType.addIntAttribute("p1");
      EVAL userDefinedEventType.addIntAttribute("p2");
      EVAL userDefinedEventType.addIntAttribute("p3");
      userDefinedEventType.finishInitialization();
    EXCEPT
    | EventType.Mismatch =>
        ErrorMessage("EventType.Mismatch", "DefineUserEvent");
    END;
  END DefineUserEvent;

(* End of internal procedures. *)

(***************************************************************************)
(**  **  **  **  RGRAS simulating procedures **  **  **  **  **  **  **  ***)
(***            ============================                              **)
(****                                                                      *)
(***************************************************************************)

PROCEDURE AGLogin (ClientRoot: TEXT         := NIL;
                   CacheSize : CARDINAL     := 0;
                   ServerId  : TEXT         := NIL;
                   Agent     : TEXT         := NIL) =
  (* The applicationname is ignored. *)
  BEGIN
    IF ClientRoot = NIL THEN
      IF rootPath = NIL THEN
        ErrorMessage(
            "AGLogin", "No ClientRoot specified and environment\n" &
            "variable GRAS3 not set!", halt := TRUE);
      ELSE
        ClientRoot := rootPath;
      END;
    END;
    IF NOT alreadyLogged THEN
      GraphSystem.Login(ClientRoot, nameserver := Agent,
                        cachesize := CacheSize, grasserver := ServerId);
      alreadyLogged := TRUE
    END
  END AGLogin;

PROCEDURE AGLogout () =
  <* FATAL PoolList.EntryNotInList, GraphList.EntryNotInList, SchemeList.EntryNotInList  *>
  VAR
    poolSet           : TextCursorSet.T;
    ok                : BOOLEAN;
    singlePool        : GraphPool.T;
    singlePoolName    : PoolName;
    singleGraph       : Graph.T;
    singleGraphNumber : CARDINAL;
    graphSet          : CardSet.T;
    schemeSet         : CardSet.T;
    singleScheme      : Scheme.T;
    singleSchemeNumber: CARDINAL;
  BEGIN
    TRY
      (* There is no logout procedure in GraphSystem.  Everything that has
         to be done is to close all open schemes, graphs and pools. *)
      (* First closing the schemes. *)
      schemeSet := schemeList.getAllEntries();
      schemeSet.loop();
      singleSchemeNumber := schemeSet.get(ok);
      WHILE ok DO
        singleScheme := schemeList.getEntry(singleSchemeNumber).handle;
        singleScheme.close();
        schemeList.removeEntry(singleSchemeNumber);
        singleSchemeNumber := schemeSet.get(ok);
      END;
      schemeSet.dispose();
      (* Then closing the graphs. *)
      graphSet := graphList.getAllEntries();
      graphSet.loop();
      singleGraphNumber := graphSet.get(ok);
      WHILE ok DO
        singleGraph := graphList.getEntry(singleGraphNumber).handle;
        singleGraph.close(keepLog := FALSE);
        graphList.removeEntry(singleGraphNumber);
        singleGraphNumber := graphSet.get(ok);
      END;
      graphSet.dispose();
      (* Finally closing the pools. *)
      poolSet := poolList.getAllEntries();
      poolSet.loop();
      singlePoolName := poolSet.get(ok);
      WHILE ok DO
        singlePool := poolList.getEntry(singlePoolName).handle;
        singlePool.close();
        poolList.removeEntry(singlePoolName);
        singlePoolName := poolSet.get(ok);
      END;
      poolSet.dispose();
    EXCEPT
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGLogout",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGLogout", "Graph.InternalError : " & ErrorSupport.ToText(m),
          halt := TRUE)
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGLogout", "Scheme.InternalError : " & ErrorSupport.ToText(m),
          halt := TRUE)
    END;
  END AGLogout;

(*************************************************************************)
(**                                                                      *)
(**                Operations for managing graph pools                   *)
(**                                                                      *)
(*************************************************************************)

PROCEDURE AGOpenGraphPool (    poolName: PoolName;
                               mode    : GraphPoolMode;
                           VAR status  : TStatus        ) =
  VAR newPool: BOOLEAN;
  BEGIN
    TRY
      IF poolList.isEntry(poolName) THEN
        (* When the pool is in the internal list, it is already open. *)
        status := TStatus.AlreadyOpen;
      ELSE
        status := TStatus.NoError;
        (* The parameter Mode can have one of the following values :
           NewPool, OldPool, GetPool.*)
        CASE mode OF
        | GraphPoolMode.NewPool =>
            (* When a new pool is to be created it has to be checked
               whether it already exists in the GraphPoolSystem. *)
            IF GraphSystem.ExistsPool(poolName) THEN
              status := TStatus.AlreadyExistent;
            ELSE
              (* Otherwise a new pool can be opened, and entered into the
                 list. *)
              newPool := TRUE;
            END;
        | GraphPoolMode.OldPool =>
            (* The pool has to exist in the GraphSystem. *)
            IF GraphSystem.ExistsPool(poolName) THEN
              newPool := FALSE
            ELSE
              (* Otherwise, the status is set to NotExistent. *)
              status := TStatus.NotExistent
            END;
        | GraphPoolMode.GetPool =>
            (* It has to be checked if the graph exists in the
               GraphSystem.*)
            IF GraphSystem.ExistsPool(poolName) THEN
              newPool := FALSE;
            ELSE
              newPool := TRUE;
            END;
        END;
        (* When no error occured, the pool can be opened with the name
           'Pool' and the parameter newPool. *)
        IF status = TStatus.NoError THEN
          CreateAndAddPool(poolName, newPool);
        END;
      END;
    EXCEPT
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGOpenGraphPool", "GraphSystem.InternalError : "
                                          & ErrorSupport.ToText(m),
                     halt := TRUE);
    END
  END AGOpenGraphPool;

PROCEDURE AGCloseGraphPool (poolName: PoolName) =
  <* FATAL PoolList.EntryNotInList *>
  VAR pool: GraphPool.T;
  (* This procedure closes a graphpool. *)
  BEGIN
    TRY
      (* When the pool is in the poolList, than the pool has to be closed,
         and removed from the poolList. *)
      IF GetOpenPool(poolName, "AGCloseGraphPool", pool) THEN
        pool.close();
        poolList.removeEntry(poolName);
      END;
    EXCEPT
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGCloseGraphPool",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    END
  END AGCloseGraphPool;

PROCEDURE AGDeleteGraphPool (    poolName: PoolName;
                             VAR status  : TStatus;
                                 force   : BOOLEAN   ) =
  <* FATAL PoolList.EntryNotInList *>
  (* This procedure deletes a graphpool *)
  VAR
    pool                     : GraphPool.T;
    containingGraphsOrSchemes: BOOLEAN;
  BEGIN
    TRY
      status := TStatus.NoError;
      (* First we have to check whether the pool exists. *)
      IF GraphSystem.ExistsPool(poolName) THEN
        IF force THEN
          (* With force = TRUE, the entire pool can be deleted. *)
          GraphSystem.DeletePool(poolName);
        ELSE
          (* Now we must check whether it is open. *)
          IF NOT poolList.isEntry(poolName) THEN
            (* When it is not open, we have to open it to get a handle. *)
            pool := NEW(GraphPool.T).open(
                      poolName, defaultPoolAccessMode, new := FALSE);
          ELSE
            (* Otherwise we can get the handle from the list. *)
            pool := poolList.getEntry(poolName).handle;
          END;
          (* Determining if there are graphs or schemes left in the
             pool. *)
          pool.beginTransaction();
          WITH containedGraphs  = pool.getGraphs(),
               containedSchemes = pool.getSchemes() DO
            containingGraphsOrSchemes :=
              (containedGraphs.card() # 0) OR (containedSchemes.card() # 0);
            containedGraphs.dispose();
            containedSchemes.dispose();
          END;
          pool.commitTransaction();
          (* If there are any graphs in the pool, it cannot be deleted. *)
          IF containingGraphsOrSchemes THEN
            status := TStatus.NotEmpty;
            (* When the pool is not in the list, it must be closed
               again. *)
            IF NOT poolList.isEntry(poolName) THEN pool.close(); END;
          ELSE
            (* Without containig any graph and schemes, it can be closed,
               removed from the list and deleted. *)
            pool.close();
            IF poolList.isEntry(poolName) THEN
              poolList.removeEntry(poolName)
            END;
            GraphSystem.DeletePool(poolName);
          END;
        END;
      ELSE
        status := TStatus.NotExistent;
      END;
    EXCEPT
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGDeleteGraphPool", "PageFile.NoAccess : " & m);
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGDeleteGraphPool", "GraphSystem.InternalError : "
                                            & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGDeleteGraphPool",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Access.Denied (m) =>
        ErrorMessage("AGDeleteGraphPool", "Access.Denied : " & m);
    | Access.Locked => ErrorMessage("AGDeleteGraphPool", "Access.Locked");
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGDeleteGraphPool",
          "GraphPool.CardinalityError : pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGDeleteGraphPool", "GraphPool.NotInTransaction");

    END
  END AGDeleteGraphPool;

(* The following three procedures are not yet implemented *)

(*
PROCEDURE AGReorganizeGraphPool (  poolName: PoolName;
                                 VAR      status  : TStatus) =
  BEGIN
  END AGReorganizeGraphPool;

PROCEDURE AGRecoverGraphPool ( poolName: PoolName; VAR status: TStatus) =
  BEGIN
  END AGRecoverGraphPool;

PROCEDURE AGResetGraphPool ( poolName: PoolName; VAR status: TStatus) =
  BEGIN
  END AGResetGraphPool;
*)

PROCEDURE AGGetGraphsInGraphPool (poolName: PoolName; VAR existent: BOOLEAN) =
  (* This procedure gets all graphs contained in the pool named 'Pool' and
     stores their external numbers in an internal global array. *)
  <* FATAL PoolList.EntryNotInList *>
  VAR
    pool     : GraphPool.T;
    ok       : BOOLEAN;
    graphText: TEXT;
    graphSet : TextCursorSet.T;
    i        : CARDINAL        := 0;
  BEGIN
    TRY
      (* When the pool is not known by the GraphSystem then the parameter
         Existent is set to FALSE.*)
      IF GraphSystem.ExistsPool(poolName) THEN
        existent := TRUE;
        (* When the graph exists, it has to be checked whether it is opend.
           If it is closed, it has to be opend. *)
        IF NOT poolList.isEntry(poolName) THEN
          pool := NEW(GraphPool.T).open(
                    poolName, defaultPoolAccessMode, new := FALSE);
        ELSE
          (* When the graph was already open it can be read from the
             list. *)
          pool := poolList.getEntry(poolName).handle;
        END;
        (* The set with the graphs is read. *)
        pool.beginTransaction();
        graphSet := pool.getGraphs();
        (* The set contains the names of the existing graphs.  Now an array
           of the sets size can be created to store the external graph
           numbers. *)
        graphNumberArray :=
          NEW((REF ARRAY OF ExternNumber), graphSet.card());
        (* All graphnumbers are entered into the global array. *)
        graphSet.loop();
        graphText := graphSet.get(ok);
        WHILE ok DO
          graphNumberArray[i] := pool.graphNumber(graphText);
          graphText := graphSet.get(ok);
          i := i + 1;
        END;
        graphSet.dispose();
        pool.commitTransaction();
        (* If the pool was closed before the operation, it has to be closed
           again. *)
        IF NOT poolList.isEntry(poolName) THEN pool.close() END;
      ELSE
        existent := FALSE;
      END;
    EXCEPT
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGGetGraphsInGraphPool",
                     "GraphSystem.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | Access.Denied (m) =>
        ErrorMessage("AGGetGraphsInGraphPool", "Access.Denied : " & m);
    | Access.Locked =>
        ErrorMessage("AGGetGraphsInGraphPool", "Access.Locked");
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGGetGraphsInGraphPool", "PageFile.NoAccess : " & m);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGGetGraphsInGraphPool",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | ChgMgmtGraphPool.InternalError (m) =>
        ErrorMessage("AGGetGraphsInGraphPool",
                     "ChgMgmtGraphPool.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | ChgMgmtGraphPool.NotExistent =>
        ErrorMessage(
          "AGGetGraphsInGraphPool", "ChgMgmtGraphPool.NotExistent");
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGGetGraphsInGraphPool",
          "GraphPool.CardinalityError : pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage(
          "AGGetGraphsInGraphPool", "GraphPool.NotInTransaction");
    END;
  END AGGetGraphsInGraphPool;

PROCEDURE AGGiveNextGraphInGraphPool (VAR extNumber: ExternNumber;
                                      VAR existent : BOOLEAN       ) =
  (* Returns the next graph in the graphNumberArray filled by
     AGGetGraphsInGraphPool and removes it from the array. *)
  VAR
    newArray: GraphNumberArray;
    length  : CARDINAL;
  BEGIN
    (* The actual length of the graphNumberArray is stored in length. *)
    IF graphNumberArray # NIL THEN
      length := NUMBER(graphNumberArray^);
      IF length # 0 THEN
        (* A new array is created with the length : 'length' - 1. *)
        newArray := NEW(GraphNumberArray, length - 1);
        (* All entries but the first are enterd into the new array. *)
        FOR i := 1 TO length - 1 DO
          newArray[i - 1] := graphNumberArray[i]
        END;
        (* The first entry of the old array is returned. *)
        extNumber := graphNumberArray[0];
        graphNumberArray := newArray;
        existent := TRUE;
      ELSE
        existent := FALSE
      END;
    ELSE
      existent := FALSE
    END;
  END AGGiveNextGraphInGraphPool;

PROCEDURE AGCopyGraph (    sourceGraph   : GraphNumber;
                           targetPoolName: PoolName;
                           targetGrName  : GraphName;
                       VAR status        : TStatus      ) =
  <* FATAL GraphList.EntryNotInList, PoolList.EntryNotInList *>
  (* Copies a graph with the internal number 'SourceGraph' to the pool
     named 'TargetPool' and names it 'TargetGrName'. *)
  VAR
    sourcePool, targetPool: GraphPool.T         := NIL;
    graphInfo             : GraphList.GraphInfo;
    extNr                 : CARDINAL;
    st                    : TStatus;
    schemeExists          : BOOLEAN;
    names                 : TypedNames.T;
  BEGIN
    TRY
      status := TStatus.NoError;
      (* Continuing depends on whether the sourcePool could be figured
         out. *)
      IF graphList.isEntry(sourceGraph) THEN
        (* Retrieving the information record of the source graph from the
           graphlist. *)
        graphInfo := graphList.getEntry(sourceGraph);
        IF GetOpenPoolFromGraph(sourceGraph, sourcePool) THEN
          (* We have to check whether the targetpool exists. *)
          IF GraphSystem.ExistsPool(targetPoolName) THEN
            (* When it is open, then the handle can be received from the
               poolList, otherwise it has to be opened, and closed again
               later. *)
            IF poolList.isEntry(targetPoolName) THEN
              targetPool := poolList.getEntry(targetPoolName).handle
            ELSE
              targetPool :=
                NEW(GraphPool.T).open(
                  targetPoolName, defaultPoolAccessMode, new := FALSE);
            END;
            targetPool.beginTransaction();
            IF targetPool.existsGraph(targetGrName) THEN
              status := TStatus.AlreadyExistent
            END;
            targetPool.commitTransaction();
            IF targetPool.getTransactionLevel() # 0 THEN
              status := TStatus.StillPendingTransaction;
            END;
            IF status = TStatus.NoError THEN
              (* Closing the graph, cause it has to be closed when copying
                 it. *)
              graphInfo.handle.close(keepLog := FALSE);
              (* Removing it from the list. *)
              graphList.removeEntry(graphInfo.number);
              (* Copying the graph *)
              GraphPool.CopyGraph(sourcePool, graphInfo.name, FALSE,
                                  targetPool, targetGrName, FALSE);
              (* Checking if the scheme exists in the target pool. *)
              targetPool.beginTransaction();
              (* Checking if the scheme of the source graph exists in the
                 target pool. *)
              schemeExists :=
                targetPool.existsScheme(graphInfo.schemeName);
              targetPool.commitTransaction();
              (* If the scheme does not exist in the target pool, it has to
                 be copied too. *)
              IF NOT schemeExists THEN
                GraphPool.CopyScheme(
                  sourcePool, graphInfo.schemeName, FALSE, targetPool,
                  graphInfo.schemeName, FALSE);
              END;
              (* Connecting the new graph with its scheme. *)
              targetPool.beginTransaction();
              names := targetPool.getNames();
              names.connectToScheme(
                targetGrName, graphInfo.schemeName, local := FALSE);
              targetPool.commitTransaction();
              (* Now opening the graph again, with the old internal
                 number. *)
              sourcePool.beginTransaction();
              CreateAndAddGraph(
                graphInfo.name, FALSE, sourcePool, graphInfo.poolName,
                graphInfo.schemeName, graphInfo.number, extNr, st);
              sourcePool.commitTransaction();
            END;
            (* When the targetPool was closed before the operation, it has
               to be closed again. *)
            IF NOT poolList.isEntry(targetPoolName) THEN
              targetPool.close()
            END;
          ELSE
            status := TStatus.NotExistent;
          END;
        ELSE
          status := TStatus.NotExistent;
        END;
      ELSE
        status := TStatus.NotExistent;
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGCopyGraph", "Access.Locked");
    | Access.Denied (m) =>
        ErrorMessage("AGCopyGraph", "Access.Denied : " & m);
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGCopyGraph", "PageFile.NoAccess : " & m);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGCopyGraph",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.Existent =>
        ErrorMessage("AGCopyGraph", "GraphPool.Existent");
    | GraphPool.NotExistent =>
        ErrorMessage("AGCopyGraph", "GraphPool.NotExistent");
    | GraphPool.InTransaction =>
        ErrorMessage("AGCopyGraph", "GraphPool.InTransaction");
    | GraphPool.InUse => ErrorMessage("AGCopyGraph", "GraphPool.InUse");
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGCopyGraph", "GraphSystem.InternalError : "
                                      & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGCopyGraph",
          "GraphPool.CardinalityErro, pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGCopyGraph", "GraphPool.NotInTransaction");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGCopyGraph", "Graph.InternalError : " & ErrorSupport.ToText(m),
          halt := TRUE);
    | TypedNames.InternalError (m) =>
        ErrorMessage("AGCopyGraph", "TypedNames.InternalError : "
                                      & ErrorSupport.ToText(m),
                     halt := TRUE);
    | TypedNames.Unknown =>
        ErrorMessage("AGCopyGraph", "TypedNames.Unknown");
    END;

  END AGCopyGraph;

PROCEDURE AGDeltaCopyGraph (    poolName    : PoolName;
                                sourceGrName: GraphName;
                                targetGrName: GraphName;
                                forward     : BOOLEAN;
                            VAR status      : TStatus    ) =
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* A handle to the open pool with the name 'poolName' has to be
         received from the poolList. *)
      IF GetOpenPool(poolName, "AGDeltaCopyGraph", pool) THEN
        status := TStatus.NoError;
        IF pool.getTransactionLevel() # 0 THEN
          status := TStatus.StillPendingTransaction;
        ELSE
          pool.beginTransaction();
          (* Now the existence of the sourcegraph has to be checked. *)
          IF NOT pool.existsGraph(sourceGrName) THEN
            status := TStatus.NotExistent
          END;
          (* Now the nonexistence of the targetgraph has to be checked. *)
          IF pool.existsGraph(targetGrName) THEN
            status := TStatus.AlreadyExistent
          END;
          (* With the status TStatus.NoError, the copy action can take
             place. *)
          IF status = TStatus.NoError THEN
            pool.deltaCopyGraph(sourceGrName, targetGrName, local := FALSE,
                                forward := forward);
          END;
          pool.commitTransaction();
        END;
      ELSE
        status := TStatus.NotExistent;
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGDeltaCopyGraph", "Access.Locked");
    | ChgMgmtGraphPool.Existent =>
        ErrorMessage("AGDeltaCopyGraph", "ChgMgmtGraphPool.Existent");
    | ChgMgmtGraphPool.InUse =>
        ErrorMessage("AGDeltaCopyGraph", "ChgMgmtGraphPool.InUse");
    | ChgMgmtGraphPool.NotExistent =>
        ErrorMessage("AGDeltaCopyGraph", "ChgMgmtGraphPool.NotExistent");
    | ChgMgmtGraphPool.InternalError (m) =>
        ErrorMessage("AGDeltaCopyGraph",
                     "ChgMgmtGraphPool.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGDeltaCopyGraph",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGDeltaCopyGraph",
          "GraphPool.CardinalityError, pool number : " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGDeltaCopyGraph", "GraphPool.NotInTransaction");
    END;
  END AGDeltaCopyGraph;

(* Since there are not group operations, the next three procedures are left
   unimplemented.  Also all group informations in the procedures in this
   module are left unused. *)

(*
PROCEDURE AGGetFirstGroupInfo ( Pool    : PoolName;
                                GrName  : GraphName;
                               VAR      OneGroup: BOOLEAN;
                               VAR      Existent: BOOLEAN;
                               VAR      Group   : GroupName;
                               VAR      LockMode: GroupType  ) =
  BEGIN
  END AGGetFirstGroupInfo;

PROCEDURE AGGetNextGroupInfo (VAR Existent: BOOLEAN;
                              VAR Group   : GroupName;
                              VAR LockMode: GroupType  ) =
  BEGIN
  END AGGetNextGroupInfo;

PROCEDURE AGGetInfoByGrNo (    Graph       : GraphNumber;
                           VAR IsOpen      : BOOLEAN;
                           VAR Pool        : PoolName;
                           VAR GrName      : GraphName;
                           VAR ExtNumber   : ExternNumber;
                           VAR GrType      : GraphType;
                           VAR Group       : GroupName;
                           VAR LockMode    : GroupType;
                           VAR SingleWriter: BOOLEAN       ) =
  BEGIN
  END AGGetInfoByGrNo;
*)

PROCEDURE AGGetInfoByExtNo (                 poolName : PoolName;
                                             extNumber: ExternNumber;
                                         VAR existent : BOOLEAN;
                                         VAR grName   : GraphName;
                            <* UNUSED *> VAR grType   : GraphType;
                                         VAR isOpen   : BOOLEAN;
                                         VAR graph    : GraphNumber   ) =
  <* FATAL PoolList.EntryNotInList, GraphList.EntryNotInList *>
  (* The procedure returns information about a graph via the external
     number. *)
  VAR
    pool     : GraphPool.T;
    graphSet : TextCursorSet.T;
    ok       : BOOLEAN;
    graphName: GraphName;
  BEGIN
    TRY
      (* We have to check whether the pool named 'Pool' exists. *)
      IF GraphSystem.ExistsPool(poolName) THEN
        (* When the pool is open, the handle can be received from the
           poolList, otherwise it has to be opened. *)
        IF poolList.isEntry(poolName) THEN
          pool := poolList.getEntry(poolName).handle
        ELSE
          pool := NEW(GraphPool.T).open(
                    poolName, defaultPoolAccessMode, new := FALSE);
        END;
        (* All graphs in the pool are stored in the graphSet. *)
        pool.beginTransaction();
        graphSet := pool.getGraphs();
        graphSet.loop();
        graphName := graphSet.get(ok);
        WHILE ok DO
          IF pool.graphNumber(graphName) = extNumber THEN
            (* The name of the graph with the desired external number is
               stored in GrName. *)
            grName := graphName;
          END;
          graphName := graphSet.get(ok);
        END;
        pool.commitTransaction();
        graphSet.dispose();
        (* When the name could be figured out, the graph exists. *)
        existent := grName # NIL;
        (* When the graph exists it can be checked whether it is open. *)
        IF existent THEN
          isOpen := graphList.isEntryByExternalNumber(extNumber);
          IF isOpen THEN
            (* When the graph is opend, the internal number can be received
               from the graphList. *)
            graph := graphList.getEntryByExternalNumber(extNumber).number;
          END;
        END;
        (* When the pool was closed before the operations, it has to be
           closed again. *)
        IF NOT poolList.isEntry(poolName) THEN pool.close(); END;
      ELSE
        existent := FALSE
      END;
    EXCEPT
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGGetInfoByExtNumber", "GraphSystem.InternalError : "
                                               & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Access.Denied (m) =>
        ErrorMessage("AGGetInfoByExtNo", "Access.Denied : " & m);
    | Access.Locked => ErrorMessage("AGGetInfoByExtNo", "Access.Locked");
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGGetInfoByExtNo", "PageFile.NoAccess : " & m);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGGetInfoByExtNo",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | ChgMgmtGraphPool.InternalError (m) =>
        ErrorMessage("AGGetInfoByExtNo",
                     "ChgMgmtGraphPool.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | ChgMgmtGraphPool.NotExistent =>
        ErrorMessage("AGGetInfoByExtNo", "ChgMgmtGraphPool.NotExistent");
    | GraphPool.CardinalityError =>
        ErrorMessage("AGGetInfoByExtNo", "GraphPool.CardinalityError");
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGGetInfoByExtNo", "GraphPool.NotInTransaction");
    END;
  END AGGetInfoByExtNo;

PROCEDURE AGGetInfoByName (               poolName   : PoolName;
                                          grName     : GraphName;
                                      VAR existent   : BOOLEAN;
                           <*UNUSED*> VAR grType     : GraphType;
                                      VAR extNumber  : ExternNumber;
                                      VAR isOpen     : BOOLEAN;
                                      VAR graphNumber: GraphNumber   ) =
  <* FATAL PoolList.EntryNotInList, GraphList.EntryNotInList *>
  (* This procedure returns some information about a graph via its name. *)
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* We have to check whether the pool exists. *)
      IF GraphSystem.ExistsPool(poolName) THEN
        (* Checking whether the pool is open.  If so, the handle can be
           received from the poolList, otherwise the pool has to be
           opened. *)
        IF NOT poolList.isEntry(poolName) THEN
          pool := NEW(GraphPool.T).open(
                    poolName, defaultPoolAccessMode, new := FALSE);
          pool.beginTransaction();
        ELSE
          pool := poolList.getEntry(poolName).handle
        END;
        (* Now we can check whether the graph exists. *)
        IF pool.existsGraph(grName) THEN
          (* When existing, it has to be checked if it is open. *)
          existent := TRUE;
          extNumber := pool.graphNumber(grName);
          isOpen := graphList.isEntryByExternalNumber(extNumber);
          (* The internal number from an open graph can be received from
             the graphList. *)
          IF isOpen THEN
            graphNumber :=
              graphList.getEntryByExternalNumber(extNumber).number
          END;
        ELSE
          existent := FALSE
        END;
        (* When the pool was closed before the operations, it has to be
           closed again. *)
        IF NOT poolList.isEntry(poolName) THEN
          pool.commitTransaction();
          pool.close();
        END;
      ELSE
        existent := FALSE
      END;
    EXCEPT
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGGetInfoByName", "GraphSystem.InternalError : "
                                          & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Access.Denied (m) =>
        ErrorMessage("AGGetInfoByName", "Access.Denied : " & m);
    | Access.Locked => ErrorMessage("AGGetInfoByName", "Access.Locked");
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGGetInfoByName", "PageFile.NoAccess : " & m);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGGetInfoByName",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | ChgMgmtGraphPool.InternalError (m) =>
        ErrorMessage("AGGetInfoByName", "ChgMgmtGraphPool.InternalError : "
                                          & ErrorSupport.ToText(m),
                     halt := TRUE);
    | ChgMgmtGraphPool.NotExistent =>
        ErrorMessage("AGGetInfoByName", "ChgMgmtGraphPool.NotExistent");
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGGetInfoByName",
          "GraphPool.CardinalityError, pool number : " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGGetInfoByName", "GraphPool.NotInTransaction");
    END;
  END AGGetInfoByName;

PROCEDURE AGOpenGraph (               poolName  : PoolName;
                                      grName    : GraphName;
                       <*UNUSED*> VAR grType    : GraphType;
                                      grMode    : GraphMode;
                       <*UNUSED*>     gpName    : GroupName;
                       <*UNUSED*>     gpType    : GroupType;
                       <*UNUSED*>     gpMode    : GroupMode;
                       <*UNUSED*>     oneGroup  : BOOLEAN;
                       <*UNUSED*>     noWait    : BOOLEAN;
                                  VAR status    : TStatus;
                                  VAR extNumber : ExternNumber;
                                  VAR grNumber  : GraphNumber;
                                      schemeName: SchemeName     := NIL) =
  <* FATAL PoolList.EntryNotInList, GraphList.EntryNotInList *>
  VAR
    pool     : GraphPool.T;
    newGraph : BOOLEAN;
    graphInfo: GraphList.GraphInfo;
  BEGIN
    TRY
      status := TStatus.NoError;
      (* We have to check whether the pool exists. *)
      IF GraphSystem.ExistsPool(poolName) THEN
        IF graphList.isEntryByName(grName, poolName) THEN
          (* When the graph is already open, it is closed to be opened
             again. *)
          (* Getting the graph information record from the graphlist. *)
          graphInfo := graphList.getEntryByName(grName, poolName);
          (* Closing the graph. *)
          graphInfo.handle.close(keepLog := FALSE);
          (* Removing the entry from the graphlist. *)
          graphList.removeEntry(graphInfo.number);
        END;
        (* When the pool is not open it hast to be opened, and entered into
           the poolList. *)
        IF NOT poolList.isEntry(poolName) THEN
          CreateAndAddPool(poolName, new := FALSE)
        END;
        (* A handle on the pool can be received from the poolList. *)
        pool := poolList.getEntry(poolName).handle;
        IF pool.getTransactionLevel() # 0 THEN
          status := TStatus.StillPendingTransaction;
        ELSE
          pool.beginTransaction();
          (* The parameter GrMode can have 3 values : NewGraph, GetGraph
             and OldGraph. *)
          CASE grMode OF
          | GraphMode.NewGraph =>
              (* When a new graph is to be created, it has to be checked
                 whether it already exists. *)
              IF pool.existsGraph(grName) THEN
                status := TStatus.AlreadyExistent
              ELSE
                (* When it does not exist, it can be created. *)
                newGraph := TRUE;
              END;
          | GraphMode.GetGraph =>
              (* First we must check whether the graph already exists, or
                 not. *)
              IF pool.existsGraph(grName) THEN
                (* When existing and closed, it has to be opened. *)
                newGraph := FALSE;
              ELSE
                (* When it does not exist it can be created. *)
                newGraph := TRUE;
              END;
          | GraphMode.OldGraph =>
              (* We must check whether the graph exists. *)
              IF pool.existsGraph(grName) THEN
                (* When existing and closed, it can be opened. *)
                newGraph := FALSE;
              ELSE
                status := TStatus.NotExistent;
              END;
          END;
          IF status = TStatus.NoError THEN
            (* When no error occured, the new graph can be opend. *)
            grNumber := 0;
            CreateAndAddGraph(grName, newGraph, pool, poolName, schemeName,
                              grNumber, extNumber, status);
          END;
          pool.commitTransaction();
        END;
      ELSE
        status := TStatus.NotExistent
      END;
    EXCEPT
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGOpenGraph", "GraphSystem.InternalError : "
                                      & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGOpenGraph",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Access.Denied (m) =>
        ErrorMessage("AGOpenGraph", "Access.Denied : " & m);
    | Access.Locked => ErrorMessage("AGOpenGraphPool", "Access.Locked");
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGOpenGraph", "PageFile.NoAccess : " & m);
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGOpenGraph", "Graph.InternalError : " & ErrorSupport.ToText(m),
          halt := TRUE);
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGOpenGraph",
          "GraphPool.CardinalityError, pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGOpenGraph", "GraphPool.NotInTransaction");
    END
  END AGOpenGraph;


PROCEDURE AGCloseGraph (graphNumber: GraphNumber) =
  <* FATAL GraphList.EntryNotInList, DaemonList.EntryNotInList *>
  VAR
    graph          : Graph.T;
    pool           : GraphPool.T;
    daemonListEntry: DaemonList.DLContent;
    triggerIDs     : IntSeq.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' has
         to be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGCloseGraph", graph) THEN
        IF GetOpenPoolFromGraph(graphNumber, pool) THEN
          IF pool.getTransactionLevel() # 0 THEN
            ErrorMessage("AGCloseGraph", "Pending transactions");
          ELSE
            (* All the daemons for this graph in the daemonlist have to be
               removed, and the according trigger has to be
               unregistered. *)
            WHILE daemonList.isEntryWithGraphNumber(graphNumber) DO
              daemonListEntry :=
                daemonList.getFirstEntryWithGraphNumber(graphNumber);
              daemonList.removeEntry(
                daemonListEntry.graphNumber, daemonListEntry.graphEvent,
                daemonListEntry.actionProc);
              triggerIDs := daemonListEntry.triggerIDs;
              FOR i := 1 TO triggerIDs.size() DO
                RuleEngine.UnregisterTrigger(triggerIDs.get(i - 1));
              END;
            END;
            (* Now the graph can be closed and removed from the list. *)
            graph.close(keepLog := FALSE);
            graphList.removeEntry(graphNumber);
          END;
        END;
      END;
    EXCEPT
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGCloseGraph",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGCloseGraph;

PROCEDURE AGDeleteGraph (                 poolName: PoolName;
                                          grName  : GraphName;
                         <* UNUSED *>     force   : BOOLEAN;
                                      VAR status  : TStatus    ) =
  <* FATAL PoolList.EntryNotInList *>
  (* Deletes a graph named 'GrName' in the pool 'Pool'. *)
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* We have to check whether the pool exists. *)
      IF GraphSystem.ExistsPool(poolName) THEN
        (* When the pool is open, the handle can be received from the list,
           otherwise it has to be opened. *)
        IF NOT poolList.isEntry(poolName) THEN
          pool := NEW(GraphPool.T).open(
                    poolName, defaultPoolAccessMode, new := FALSE);
        ELSE
          pool := poolList.getEntry(poolName).handle
        END;
        IF pool.getTransactionLevel() # 0 THEN
          status := TStatus.StillPendingTransaction;
        ELSE
          pool.beginTransaction();
          (* We have to check whether the graph exists in the pool. *)
          IF pool.existsGraph(grName) THEN
            (* When the graph is open, the status is set to StillOpen,
               otherwise the graph can be deleted. *)
            IF graphList.isEntryByName(grName, poolName) THEN
              status := TStatus.StillOpen;
            ELSE
              pool.deleteGraph(grName);
              status := TStatus.NoError;
            END;
            pool.commitTransaction();
          ELSE
            status := TStatus.NotExistent;
          END;
        END;
        (* When the pool was closed before the operations, it has to be
           closed again. *)
        IF NOT poolList.isEntry(poolName) THEN pool.close(); END;
      ELSE
        status := TStatus.NotExistent;
      END;
    EXCEPT
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGDeleteGraph", "GraphSystem.InternalError : "
                                        & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Access.Locked => ErrorMessage("AGDeleteGraph", "Access.Locked");
    | Access.Denied (m) =>
        ErrorMessage("AGDeleteGraph", "Access.Denied : " & m);
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGDeleteGraph", "PageFile.NoAccess : " & m);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGDeleteGraph",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.InUse => ErrorMessage("AGDeleteGraph", "GraphPool.InUse");
    | GraphPool.NoGraph =>
        ErrorMessage("AGDeleteGraph", "GraphPool.NoGraph");
    | GraphPool.NotExistent =>
        ErrorMessage("AGDeleteGraph", "GraphPool.NotExistent");
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGDeleteGraph",
          "GraphPool.CardinalityError, pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGDeleteGraph", "GraphPool.NotInTransaction");
    END;
  END AGDeleteGraph;

PROCEDURE AGDeleteScheme(                 poolName: PoolName;
                                          schema  : GraphName;
                                      VAR status  : TStatus    ) =
  <* FATAL PoolList.EntryNotInList *>
  (* Deletes a schema named 'schema' in the pool 'Pool'. *)
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* We have to check whether the pool exists. *)
      IF GraphSystem.ExistsPool(poolName) THEN
        (* When the pool is open, the handle can be received from the list,
           otherwise it has to be opened. *)
        IF NOT poolList.isEntry(poolName) THEN
          pool := NEW(GraphPool.T).open(
                    poolName, defaultPoolAccessMode, new := FALSE);
        ELSE
          pool := poolList.getEntry(poolName).handle
        END;
        IF pool.getTransactionLevel() # 0 THEN
          status := TStatus.StillPendingTransaction;
        ELSE
          pool.beginTransaction();
          (* We have to check whether the graph exists in the pool. *)
          IF pool.existsScheme(schema) THEN
            (* When the schema is open, the status is set to StillOpen.
               If there are graphs depending on the schema, the status
               is set to StillUsed. Otherwise it can be deleted. *)
            IF schemeList.isEntryByName(schema, poolName) THEN
              status := TStatus.StillOpen;
            ELSE
              WITH gr = pool.getGraphsWithScheme (schema) DO
                IF gr.card() > 0 THEN
                  status := TStatus.StillUsed;
                ELSE
                  pool.deleteScheme(schema);
                  status := TStatus.NoError;
                END;
                gr.dispose();
              END;
            END;
            pool.commitTransaction();
          ELSE
            status := TStatus.NotExistent;
          END;
        END;
        (* When the pool was closed before the operations, it has to be
           closed again. *)
        IF NOT poolList.isEntry(poolName) THEN pool.close(); END;
      ELSE
        status := TStatus.NotExistent;
      END;
    EXCEPT
    | GraphSystem.InternalError (m) =>
        ErrorMessage("AGDeleteScheme", "GraphSystem.InternalError : "
                                        & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Access.Locked => ErrorMessage("AGDeleteScheme", "Access.Locked");
    | Access.Denied (m) =>
        ErrorMessage("AGDeleteScheme", "Access.Denied : " & m);
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGDeleteScheme", "PageFile.NoAccess : " & m);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGDeleteScheme",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.InUse => ErrorMessage("AGDeleteScheme", "GraphPool.InUse");
    | GraphPool.NoScheme =>
        ErrorMessage("AGDeleteScheme", "GraphPool.NoScheme");
    | GraphPool.NotExistent =>
        ErrorMessage("AGDeleteScheme", "GraphPool.NotExistent");
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGDeleteScheme",
          "GraphPool.CardinalityError, pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGDeleteScheme", "GraphPool.NotInTransaction");
    END;
  END AGDeleteScheme; 

(* The next two procedures are left unimplemented. *)

(*
PROCEDURE AGRequestSingleWriter (    Graph : GraphNumber;
                                     Wait  : BOOLEAN;
                                 VAR Status: TStatus      ) =
  BEGIN
  END AGRequestSingleWriter;

PROCEDURE AGReleaseSingleWriter (Graph: GraphNumber) =
  BEGIN
  END AGReleaseSingleWriter;
*)

(*************************************************************************)
(**                                                                      *)
(**            Operations for Primitive Recovery Mechanisms              *)
(**                                                                      *)
(*************************************************************************)

PROCEDURE AGStartTransaction (poolName: PoolName) =
  (* A transaction on the open pool named 'Pool' is started. *)
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* The handle of the pool with the name 'poolName' is to be received
         from the poolList. *)
      IF GetOpenPool(poolName, "AGStartTransaction", pool) THEN
        (* The transaction is started. *)
        pool.beginTransaction();
      END;
    EXCEPT
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGStartTransaction",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    END;
  END AGStartTransaction;

PROCEDURE AGCommitToplevelTransaction (poolName: PoolName) =
  (* Committing all transactions that are currently running on the pool
     'Pool'. *)
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* The handle of the pool with the name 'poolName' is to be received
         from the poolList. *)
      IF GetOpenPool(poolName, "AGCommitTopLevelTransaction", pool) THEN
        (* Now committing all transactions down to transactionlevel 0 *)
        WHILE pool.getTransactionLevel() # 0 DO
          pool.commitTransaction();
        END;
      END;
    EXCEPT
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGCommitTopLevelTransaction",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGCommitTopLevelTransaction",
          "GraphPool.CardinalityError : pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage(
          "AGCommitTopLevelTransaction", "GraphPool.NotInTransaction");
    END;
  END AGCommitToplevelTransaction;

PROCEDURE AGCommitTransaction (poolName: PoolName) =
  (* Commits one single transaction. *)
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* The handle of the pool with the name 'poolName' is to be received
         from the poolList. *)
      IF GetOpenPool(poolName, "AGCommitTransaction", pool) THEN
        (* The transaction can be commited *)
        pool.commitTransaction();
      END;
    EXCEPT
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGCommitTransaction",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGCommitTransaction",
          "GraphPool.CardinalityError : pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGCommitTransaction", "GraphPool.NotInTransaction");
    END;
  END AGCommitTransaction;

PROCEDURE AGAbortToplevelTransaction (poolName: PoolName) =
  (* Aborts all transactions that are currently running on pool 'Pool'. *)
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* The handle of the pool with the name 'poolName' is to be received
         from the poolList. *)
      IF GetOpenPool(poolName, "AGAbortTopLevelTransaction", pool) THEN
        (* Aborting all transactions down to transactionlevel 0 *)
        WHILE pool.getTransactionLevel() # 0 DO
          pool.abortTransaction();
        END;
      END;
    EXCEPT
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGAbortTopLevelTransaction",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.NotInTransaction =>
        ErrorMessage(
          "AGAbortTopLevelTransaction", "GraphPool.NotInTransaction");
    END;
  END AGAbortToplevelTransaction;

PROCEDURE AGAbortTransaction (poolName: PoolName) =
  VAR pool: GraphPool.T;
  BEGIN
    TRY
      (* The handle of the pool with the name 'poolName' is to be received
         from the poolList. *)
      IF GetOpenPool(poolName, "AGAbortTransaction", pool) THEN
        (* Aborting one single transation. *)
        pool.abortTransaction();
      END;
    EXCEPT
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGAbortTransaction",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGAbortTransaction", "GraphPool.NotInTransaction");
    END;
  END AGAbortTransaction;

(*************************************************************************)
(**                                                                      *)
(**      Operations for Requests and Modifications on Single Graphs      *)
(**                                                                      *)
(** Note:                                                                *)
(**                                                                      *)
(**   The effects of these operations can be reset by aborting trans-    *)
(**   actions (but this does not change the state of local sets!)        *)
(**                                                                      *)
(*************************************************************************)

PROCEDURE AGCreateNode (    graphNumber: GraphNumber;
                            nType      : TypeNumber;
                            environment: NodeNumber;
                        VAR nodeNr     : NodeNumber   ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGCreateNode", graph) THEN
        (* The following two graph operations create a new node *)
        nodeNr := graph.createNodeNumber(environment);
        nodeNr := graph.createNode(nType);
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGCreateNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGCreateNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NotOwner => ErrorMessage("AGCreateNode", "Graph.NotOwner");
    | Graph.LogError (m) =>
        ErrorMessage(
          "AGCreateNode", "Graph.LogError : " & ErrorSupport.ToText(m));
    | Graph.Unknown => ErrorMessage("AGCreateNode", "Graph.Unknown");
    END;
  END AGCreateNode;

PROCEDURE AGCreateEdgeAndNode (    graphNumber: GraphNumber;
                                   sourceNode : NodeNumber;
                                   eType      : TypeNumber;
                                   nType      : TypeNumber;
                               VAR nodeNr     : NodeNumber   ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGCreateEdgeAndNode", graph) THEN
        (* First a new node is created with the following two
           operations. *)
        nodeNr := graph.createNodeNumber(sourceNode);
        nodeNr := graph.createNode(nType);
        (* Now the edge from the sourcenode to the new node can be
           created. *)
        graph.createEdge(sourceNode, nodeNr, eType);
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGCreateEdgeAndNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGCreateEdgeAndNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NotOwner =>
        ErrorMessage("AGCreateEdgeAndNode", "Graph.NotOwner");
    | Graph.LogError (m) =>
        ErrorMessage("AGCreateEdgeAndNode",
                     "Graph.LogError : " & ErrorSupport.ToText(m));
    | Graph.Unknown =>
        ErrorMessage("AGCreateEdgeAndNode", "Graph.Unknown");
    | Graph.CardinalityError =>
        ErrorMessage("AGCreateEdgeAndNode", "Graph.CardinalityError");
    | Graph.NodeNotFound =>
        ErrorMessage("AGCreateEdgeAndNode", "Graph.NodeNotFound");
    | Graph.WrongType =>
        ErrorMessage("AGCreateEdgeAndNode", "Graph.WrongType");
    END;
  END AGCreateEdgeAndNode;

PROCEDURE AGDeleteNodeAndEdges (graphNumber: GraphNumber; node: NodeNumber) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGDeleteNodeAndEdges", graph) THEN
        (* Now the node can be deleted *)
        graph.deleteNodeNoInfo(node);
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGDeleteNodeAndEdges", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGDeleteNodeAndEdges",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NotOwner =>
        ErrorMessage("AGDeleteNodeAndEdges", "Graph.NotOwner");
    | Graph.LogError (m) =>
        ErrorMessage("AGDeleteNodeAndEdges",
                     "Graph.LogError : " & ErrorSupport.ToText(m));
    END;
  END AGDeleteNodeAndEdges;

PROCEDURE AGGetUniqueNodeId (    graphNumber: GraphNumber;
                             VAR uniqueID   : CARDINAL     ) =
  <* FATAL GraphList.EntryNotInList, PoolList.EntryNotInList *>
  VAR
    graphName     : GraphName;
    rgrasNames    : RGRASNames.T;
    pools         : TextCursorSet.T;
    singlePoolName: PoolName;
    singlePool    : GraphPool.T;
    ok, found     : BOOLEAN         := FALSE;
  BEGIN
    TRY
      (* The graph has to be open, so this is checked at the beginning. *)
      IF graphList.isEntry(graphNumber) THEN
        (* The name of the graph can be read from the graphList. *)
        graphName := graphList.getEntry(graphNumber).name;
        (* The pool containing the graph is searched. *)
        pools := poolList.getAllEntries();
        pools.loop();
        singlePoolName := pools.get(ok);
        WHILE ok AND NOT found DO
          singlePool := poolList.getEntry(singlePoolName).handle;
          singlePool.beginTransaction();
          (* If the pool containing the graph is found, the searching can
             be stopped and its handle can be found in 'singlePool'. *)
          found := singlePool.existsGraph(graphName);
          singlePool.commitTransaction();
          singlePoolName := pools.get(ok);
        END;
        pools.dispose();
        (* The variable found determines whether the host pool could be
           found. *)
        IF found THEN
          (* Now the unique counter can be received via the getUniqueId()
             method of the names object attached to the pool. *)
          rgrasNames := singlePool.getNames();
          singlePool.beginTransaction();
          uniqueID := rgrasNames.getUniqueId();
          singlePool.commitTransaction();
        ELSE
          ErrorMessage("AGGetUniqueNodeId", "The pool could not be found!");
        END
      END;
    EXCEPT
    | RGRASNames.InternalError (m) =>
        ErrorMessage("AGGetUniqueNodeId", "RGRASNames.InternalError : "
                                            & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Access.Locked => ErrorMessage("AGGetUniqueNodeId", "Access.Locked");
    | Names.Undeclared =>
        ErrorMessage("AGGetUniqueNodeId", "Names.Undeclared");
    | Names.Unknown => ErrorMessage("AGGetUniqueNodeId", "Names.Unknown");
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGGetUniqueNodeId",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGGetUniqueNodeId",
          "GraphPool.CardinalityError : pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGGetUniqueNodeId", "GraphPool.NotInTransaction");
    END;
  END AGGetUniqueNodeId;

PROCEDURE AGShowAllNodesWithIndex (    graphNumber: GraphNumber;
                                       attNo      : TypeNumber;
                                       attLength  : Word.T;
                                       value      : TEXT;
                                   VAR setOfNodes : SimpleSet    ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGShowAllNodesWithIndex", graph) THEN
        value := Text.Sub(value, 0, attLength);
        setOfNodes :=
          setList.addEntry(graph.getNodesWithAttribute(attNo, value));
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGShowAllNodesWithIndex", "Access.Locked");
    | Graph.CyclicEvaluation =>
        ErrorMessage("AGShowAllNodesWithIndex", "Graph.CyclicEvaluation");
    | Graph.LogError (m) =>
        ErrorMessage("AGShowAllNodesWithIndex",
                     "Graph.LogError : " & ErrorSupport.ToText(m));
    | Graph.NoIndex =>
        ErrorMessage("AGShowAllNodesWithIndex", "Graph.NoIndex");
    | Graph.Unknown =>
        ErrorMessage("AGShowAllNodesWithIndex", "Graph.Unknown");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGShowAllNodesWithIndex",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGShowAllNodesWithIndex;

PROCEDURE AGShowNodeWithKey (    graphNumber: GraphNumber;
                                 attNo      : TypeNumber;
                                 attLength  : Word.T;
                                 value      : TEXT;
                             VAR exists     : BOOLEAN;
                             VAR node       : NodeNumber   ) =

  VAR
    graph     : Graph.T;
    setOfNodes: NodeSet.T;
    ok        : BOOLEAN;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGShowNodeWithKey", graph) THEN
        value := Text.Sub(value, 0, attLength);
        (* Getting a list of all nodes with attributes with the value
           'value' *)
        setOfNodes := graph.getNodesWithAttribute(attNo, value);
        (* If just one node got the attribute, than the node can be
           returned. *)
        IF setOfNodes.card() = 1 THEN
          exists := TRUE;
          setOfNodes.loop();
          node := setOfNodes.get(ok);
        ELSE
          exists := FALSE
        END;
        setOfNodes.dispose();
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGShowNodeWithKey", "Access.Locked");
    | Graph.CyclicEvaluation =>
        ErrorMessage("AGShowNodeWithKey", "Graph.CyclicEvaluation");
    | Graph.LogError (m) =>
        ErrorMessage("AGShowNodeWithKey",
                     "Graph.LogError : " & ErrorSupport.ToText(m));
    | Graph.NoIndex => ErrorMessage("AGShowNodeWithKey", "Graph.NoIndex");
    | Graph.Unknown => ErrorMessage("AGShowNodeWithKey", "Graph.Unknown");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGShowNodeWithKey",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGShowNodeWithKey;

PROCEDURE AGShowTypeOfNode (    graphNumber: GraphNumber;
                                node       : NodeNumber;
                            VAR nType      : TypeNumber   ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      nType.entity := 0;
      IF GetOpenGraph(graphNumber, "AGShowTypeOfNode", graph) THEN
        nType := graph.getNodeType(node);
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGShowTypeOfNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGShowTypeOfNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGShowTypeOfNode", "Graph.NodeNotFound");
    | Graph.NotOwner => ErrorMessage("AGShowTypeOfNode", "Graph.NotOwner");
    | Graph.Unknown => ErrorMessage("AGShowTypeOfNode", "Graph.Unknown");
    END;
  END AGShowTypeOfNode;

PROCEDURE AGCreateEdge (graphNumber: GraphNumber;
                        sourceNode : NodeNumber;
                        targetNode : NodeNumber;
                        eType      : TypeNumber   ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGCreateEdge", graph) THEN
        graph.createEdge(sourceNode, targetNode, eType);
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGCreateEdge", "Access.Locked");
    | Graph.CardinalityError =>
        ErrorMessage("AGCreateEdge", "Graph.CardinalityError");
    | Graph.LogError (m) =>
        ErrorMessage(
          "AGCreateEdge", "Graph.LogError : " & ErrorSupport.ToText(m));
    | Graph.NodeNotFound =>
        ErrorMessage("AGCreateEdge", "Graph.NodeNotFound");
    | Graph.NotOwner => ErrorMessage("AGCreateEdge", "Graph.NotOwner");
    | Graph.Unknown => ErrorMessage("AGCreateEdge", "Graph.Unkown");
    | Graph.WrongType => ErrorMessage("AGCreateEdge", "Graph.WrongType");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGCreateEdge",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGCreateEdge;

PROCEDURE AGDeleteEdge (graphNumber: GraphNumber;
                        sourceNode : NodeNumber;
                        targetNode : NodeNumber;
                        eType      : TypeNumber   ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGDeleteEdge", graph) THEN
        graph.deleteEdge(sourceNode, targetNode, eType);
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGDeleteEdge", "Access.Locked");
    | Graph.CardinalityError =>
        ErrorMessage("AGDeleteEdge", "Graph.CardinalityError");
    | Graph.NotOwner => ErrorMessage("AGDeleteEdge", "Graph.NotOwner");
    | Graph.NodeNotFound =>
        ErrorMessage("AGDeleteEdge", "Graph.NodeNotFound");
    | Graph.Unknown => ErrorMessage("AGDeleteEdge", "Graph.Unknown");
    | Graph.WrongType => ErrorMessage("AGDeleteEdge", "Graph.WrongType");
    | Graph.LogError (m) =>
        ErrorMessage(
          "AGDeleteEdge", "Graph.LogError : " & ErrorSupport.ToText(m));
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGDeleteEdge",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGDeleteEdge;

PROCEDURE AGEdgeExists (graphNumber: GraphNumber;
                        sourceNode : NodeNumber;
                        targetNode : NodeNumber;
                        eType      : TypeNumber   ): BOOLEAN =
  VAR
    graph : Graph.T;
    result: BOOLEAN := FALSE;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGEdgeExists", graph) THEN
        result := graph.existsEdge(sourceNode, targetNode, eType);
      END;
    EXCEPT
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGEdgeExists",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Access.Locked => ErrorMessage("AGEdgeExists", "Access.Locked");
    | Graph.NodeNotFound =>
        ErrorMessage("AGEdgeExists", "Graph.NodeNotFound");
    | Graph.NotOwner => ErrorMessage("AGEdgeExists", "Graph.NotOwner");
    | Graph.Unknown => ErrorMessage("AGEdgeExists", "Graph.Unknown");
    END;
    RETURN result;
  END AGEdgeExists;

PROCEDURE AGShowSourceNode (    graphNumber: GraphNumber;
                                targetNode : NodeNumber;
                                eType      : TypeNumber;
                            VAR sourceNode : NodeNumber   ) =
  VAR
    graph        : Graph.T;
    sourceNodeSet: NodeSet.T;
    ok           : BOOLEAN;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGShowSourceNode", graph) THEN
        (* All source nodes of the edge with the type 'eType' and the
           target 'targetNode' are collected. *)
        sourceNodeSet := graph.getSources(targetNode, eType);
        (* If it was only one corresponding node, it can be returned. *)
        IF sourceNodeSet.card() = 1 THEN
          sourceNodeSet.loop();
          sourceNode := sourceNodeSet.get(ok)
        ELSE
          ErrorMessage("AGShowSourceNode", "More than one source node!");
        END;
        sourceNodeSet.dispose();
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGShowSourceNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGShowSourceNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGShowSourceNode", "Graph.NodeNotFound");
    | Graph.Unknown => ErrorMessage("AGShowSourceNode", "Graph.Unknown");
    | Graph.NotOwner => ErrorMessage("AGShowSourceNode", "Graph.NotOwner");
    END;
  END AGShowSourceNode;

PROCEDURE AGTestAndShowSourceNode (    graphNumber: GraphNumber;
                                       targetNode : NodeNumber;
                                       eType      : TypeNumber;
                                   VAR sourceNrs  : CARDINAL;
                                   VAR sourceNode : NodeNumber   ) =
  VAR
    graph        : Graph.T;
    sourceNodeSet: NodeSet.T;
    ok           : BOOLEAN;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGTestAndShowSourceNode", graph) THEN
        (* All source nodes of the edge with the type 'eType' and the
           target 'targetNode' are collected. *)
        sourceNodeSet := graph.getSources(targetNode, eType);
        (* The number of nodes is stored in 'sourceNrs'. *)
        sourceNrs := sourceNodeSet.card();
        (* If it was only one node, the nodenumber can be returned. *)
        IF sourceNrs = 1 THEN
          sourceNodeSet.loop();
          sourceNode := sourceNodeSet.get(ok)
        END;
        sourceNodeSet.dispose();
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGTestAndShowSourceNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGTestAndShowSourceNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGTestAndShowSourceNode", "Graph.NodeNotFound");
    | Graph.Unknown =>
        ErrorMessage("AGTestAndShowSourceNode", "Graph.Unknown");
    | Graph.NotOwner =>
        ErrorMessage("AGTestAndShowSourceNode", "Graph.NotOwner");
    END;
  END AGTestAndShowSourceNode;

PROCEDURE AGShowAllSourceNodes (    graphNumber  : GraphNumber;
                                    targetNode   : NodeNumber;
                                    eType        : TypeNumber;
                                VAR sourceNodeSet: SimpleSet    ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGShowAllSourceNodes", graph) THEN
        (* All source nodes of the edge with the type 'eType' and the
           target 'targetNode' are collected. *)
        sourceNodeSet :=
          setList.addEntry(graph.getSources(targetNode, eType));
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGShowAllSourceNodes", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGShowAllSourceNodes",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGShowAllSourceNodes", "Graph.NodeNotFound");
    | Graph.Unknown =>
        ErrorMessage("AGShowAllSourceNodes", "Graph.Unknown");
    | Graph.NotOwner =>
        ErrorMessage("AGShowAllSourceNodes", "Graph.NotOwner");
    END;
  END AGShowAllSourceNodes;

PROCEDURE AGShowTargetNode (    graphNumber: GraphNumber;
                                sourceNode : NodeNumber;
                                eType      : TypeNumber;
                            VAR targetNode : NodeNumber   ) =
  VAR
    graph        : Graph.T;
    targetNodeSet: NodeSet.T;
    ok           : BOOLEAN;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGShowTargetNode", graph) THEN
        (* All target nodes of the edge with the type 'eType' and the
           source 'sourceNode' are collected. *)
        targetNodeSet := graph.getTargets(sourceNode, eType);
        (* If it was only one node, the nodenumber can be returned. *)
        IF targetNodeSet.card() = 1 THEN
          targetNodeSet.loop();
          targetNode := targetNodeSet.get(ok);
        ELSE
          ErrorMessage("AGShowTargetNode", "More than one target node!");
        END;
        targetNodeSet.dispose();
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGShowTargegNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGShowTargetNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGShowTargetNode", "Graph.NodeNotFound");
    | Graph.Unknown => ErrorMessage("AGShowTargetNode", "Graph.Unknown");
    | Graph.NotOwner => ErrorMessage("AGShowTargetNode", "Graph.NotOwner");
    END;
  END AGShowTargetNode;

PROCEDURE AGShowAllTargetNodes (    graphNumber  : GraphNumber;
                                    sourceNode   : NodeNumber;
                                    eType        : TypeNumber;
                                VAR targetNodeSet: SimpleSet    ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGShowAllTargetNodes", graph) THEN
        (* All target nodes of the edge with the type 'eType' and the
           source 'sourceNode' are collected. *)
        targetNodeSet :=
          setList.addEntry(graph.getTargets(sourceNode, eType));
      ELSE
        WITH emptySet = NodeSet.New() DO
          targetNodeSet := setList.addEntry(emptySet)
        END;
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGShowAllTargegNodes", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGShowAllTargetNodes",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGShowAllTargetNodes", "Graph.NodeNotFound");
    | Graph.Unknown =>
        ErrorMessage("AGShowAllTargetNodes", "Graph.Unknown");
    | Graph.NotOwner =>
        ErrorMessage("AGShowAllTargetNodes", "Graph.NotOwner");
    END;
  END AGShowAllTargetNodes;

PROCEDURE AGTestAndShowTargetNode (    graphNumber: GraphNumber;
                                       sourceNode : NodeNumber;
                                       eType      : TypeNumber;
                                   VAR targetNrs  : CARDINAL;
                                   VAR targetNode : NodeNumber   ) =
  VAR
    graph        : Graph.T;
    targetNodeSet: NodeSet.T;
    ok           : BOOLEAN;
  BEGIN
    TRY
      (* The handle of the graph with the internal number 'graphNumber' can
         be received from the graphList. *)
      IF GetOpenGraph(graphNumber, "AGTestAndShowTargetNode", graph) THEN
        (* All target nodes of the edge with the type 'eType' and the
           source 'sourceNode' are collected. *)
        targetNodeSet := graph.getTargets(sourceNode, eType);
        (* The number of the collected nodes is stored in targetNrs. *)
        targetNrs := targetNodeSet.card();
        (* If it was only one node, then its number can be returnde. *)
        IF targetNrs = 1 THEN
          targetNodeSet.loop();
          targetNode := targetNodeSet.get(ok);
        END;
        targetNodeSet.dispose();
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGTestAndShowTargegNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGTestAndShowTargetNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGTestAndShowTargetNode", "Graph.NodeNotFound");
    | Graph.Unknown =>
        ErrorMessage("AGTestAndShowTargetNode", "Graph.Unknown");
    | Graph.NotOwner =>
        ErrorMessage("AGTestAndShowTargetNode", "Graph.NotOwner");
    END;
  END AGTestAndShowTargetNode;

PROCEDURE AGTestIncomingEdge (graphNumber: GraphNumber;
                              targetNode : NodeNumber;
                              eType      : TypeNumber   ): BOOLEAN =
  VAR
    graph                               : Graph.T;
    incomingEdges                       : NodeTypeRelation.T;
    incomingEdgeType, incomingEdgeTarget: NodeNumber;
    ok, result                          : BOOLEAN            := FALSE;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGTestIncomingEdge", graph) THEN
        (* Now all incoming edges are collected. *)
        incomingEdges := graph.getAllInEdges(targetNode);
        incomingEdges.loop();
        incomingEdges.get(incomingEdgeTarget, incomingEdgeType, ok);
        WHILE ok DO
          (* Cycling through all incoming edges, the first incoming edge
             with the type 'eType' sets the result variable to TRUE.*)
          result := result OR incomingEdgeType = eType;
          incomingEdges.get(incomingEdgeTarget, incomingEdgeType, ok);
        END;
        incomingEdges.dispose();
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGTestIncomingEdge", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGTestIncomingEdge",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGTestIncomingEdge", "Graph.NodeNotFound");
    | Graph.NotOwner =>
        ErrorMessage("AGTestIncomingEdge", "Graph.NotOwner");
    END;
    RETURN result;
  END AGTestIncomingEdge;

PROCEDURE AGTestOutgoingEdge (graphNumber: GraphNumber;
                              sourceNode : NodeNumber;
                              eType      : TypeNumber   ): BOOLEAN =
  VAR
    graph                               : Graph.T;
    outgoingEdges                       : NodeTypeRelation.T;
    outgoingEdgeType, outgoingEdgeTarget: NodeNumber;
    ok, result                          : BOOLEAN            := FALSE;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGTestOutgoingEdge", graph) THEN
        (* Now all outgoing edges are collected. *)
        outgoingEdges := graph.getAllOutEdges(sourceNode);
        outgoingEdges.loop();
        outgoingEdges.get(outgoingEdgeTarget, outgoingEdgeType, ok);
        WHILE ok DO
          (* Cycling through the outgoing edges, the first outgoing edge
             with the type 'eType' sets the result variable to TRUE. *)
          result := result OR outgoingEdgeType = eType;
          outgoingEdges.get(outgoingEdgeTarget, outgoingEdgeType, ok);
        END;
        outgoingEdges.dispose();
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGTestOutgoingEdge", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGTestOutgoingEdge",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGTestOutgoingEdge", "Graph.NodeNotFound");
    | Graph.NotOwner =>
        ErrorMessage("AGTestOutgoingEdge", "Graph.NotOwner");
    END;
    RETURN result;
  END AGTestOutgoingEdge;

PROCEDURE AGGetInContextOfNode (    graphNumber: GraphNumber;
                                    targetNode : NodeNumber;
                                VAR tupleSet   : RelSet       ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGGetInContextOfNode", graph) THEN
        (* Now collecting all incoming edges with sourcenodes. *)
        tupleSet := relSetList.addEntry(graph.getAllInEdges(targetNode));
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGGetInContextOfNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGGetInContextOfNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGGetInContextOfNode", "Graph.NodeNotFound");
    | Graph.NotOwner =>
        ErrorMessage("AGGetInContextOfNode", "Graph.NotOwner");
    END;
  END AGGetInContextOfNode;

PROCEDURE AGGetOutContextOfNode (    graphNumber: GraphNumber;
                                     sourceNode : NodeNumber;
                                 VAR tupleSet   : RelSet       ) =

  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGGetOutContextOfNode", graph) THEN
        (* Now collecting all outgoing edges with targetnodes. *)
        tupleSet := relSetList.addEntry(graph.getAllOutEdges(sourceNode));
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGGetOutContextOfNode", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGGetOutContextOfNode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGGetOutContextOfNode", "Graph.NodeNotFound");
    | Graph.NotOwner =>
        ErrorMessage("AGGetOutContextOfNode", "Graph.NotOwner");
    END;
  END AGGetOutContextOfNode;

PROCEDURE AGPutAttributeSubstr (graphNumber: GraphNumber;
                                node       : NodeNumber;
                                attNo      : TypeNumber;
                                attBegin   : Word.T;
                                attLength  : Word.T;
                                attValue   : TEXT;
                                truncate   : BOOLEAN      ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist *)
      IF GetOpenGraph(graphNumber, "AGPutAttributeSubstr", graph) THEN
        (* Putting the attributevalue into the attribute. *)
        graph.putAttribute(node, attNo, attBegin, attValue);
        (* truncate if required *)
        IF truncate THEN
          graph.truncateAttribute(node, attNo, attBegin + attLength);
        END;
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGPutAttributeSubStr", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGPutAttributeSubStr",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.LogError (m) =>
        ErrorMessage("AGPutAttributeSubStr",
                     "Graph.LogError : " & ErrorSupport.ToText(m))
    | Graph.NodeNotFound =>
        ErrorMessage("AGPutAttributeSubStr", "Graph.NodeNotFound")
    | Graph.NotOwner =>
        ErrorMessage("AGPutAttributeSubStr", "Graph.NotOwner")
    | Graph.Unknown =>
        ErrorMessage("AGPutAttributeSubStr", "Graph.Unknown")
    | Graph.WrongType =>
        ErrorMessage("AGPutAttributeSubStr", "Graph.WrongType")
    END;
  END AGPutAttributeSubstr;

PROCEDURE AGDeleteAttribute (graphNumber: GraphNumber;
                             node       : NodeNumber;
                             attNo      : AttributeNumber) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGDeleteAttribute", graph) THEN
        (* The attribute is deleted, by truncating it to length 0. *)
        graph.truncateAttribute(node, attNo, 0);
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGDeleteAttribute", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGDeleteAttribute",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.WrongType =>
        ErrorMessage("AGDeleteAttribute", "Graph.WrongType");
    | Graph.Unknown => ErrorMessage("AGDeleteAttribute", "Graph.Unknown");
    | Graph.NotOwner =>
        ErrorMessage("AGDeleteAttribute", "Graph.NotOwner");
    | Graph.NodeNotFound =>
        ErrorMessage("AGDeleteAttribute", "Graph.NodeNotFound");
    | Graph.LogError (m) =>
        ErrorMessage("AGDeleteAttribute",
                     "Graph.LogError : " & ErrorSupport.ToText(m));
    END;
  END AGDeleteAttribute;

PROCEDURE AGGetAttributeSubstr (    graphNumber   : GraphNumber;
                                    node          : NodeNumber;
                                    attNo         : TypeNumber;
                                    attBegin      : Word.T;
                                    attLength     : Word.T;
                                VAR attValue      : TEXT;
                                VAR returnedLength: Word.T       ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGGetAttributeSubstr", graph) THEN
        (* Getting the attributevalue from the graph. *)
        attValue := graph.getAttribute(node, attNo, attBegin, attLength);
        (* Setting the parameter 'returnedLength' to the length of the
           attributevalue. *)
        IF attValue # NIL THEN
          returnedLength := Text.Length(attValue)
        ELSE
          returnedLength := 0
        END;
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGGetAttributeSubStr", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGGetAttributeSubStr",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.LogError (m) =>
        ErrorMessage("AGGetAttributeSubStr",
                     "Graph.LogError : " & ErrorSupport.ToText(m))
    | Graph.NodeNotFound =>
        ErrorMessage("AGGetAttributeSubStr", "Graph.NodeNotFound")
    | Graph.NotOwner =>
        ErrorMessage("AGGetAttributeSubStr", "Graph.NotOwner")
    | Graph.Unknown =>
        ErrorMessage("AGGetAttributeSubStr", "Graph.Unknown")
    | Graph.WrongType =>
        ErrorMessage("AGGetAttributeSubStr", "Graph.WrongType")
    | Graph.CyclicEvaluation =>
        ErrorMessage("AGGetAttributeSubStr", "Graph.CyclicEvaluation")
    END;
  END AGGetAttributeSubstr;

(*************************************************************************)
(**                                                                      *)
(** Operations for Event Handling                                        *)
(**                                                                      *)
(*************************************************************************)

PROCEDURE AGActivateDaemon (graphNumber: GraphNumber;
                            event      : GraphEvent <*NOWARN*>;
                            priority   : ActionPriority;
                            action     : ActionProc             ) =
  <* FATAL DaemonList.EntryAlreadyInList *>
  PROCEDURE SetGraphPatternAttributes (VAR eventPattern: EventPattern.T;
                                       graphInfo: GraphList.GraphInfo;
                                       pool     : GraphPool.T          )
    RAISES {EventType.Mismatch, EventType.Unknown} =
    BEGIN
      (* Trying to set the all available graph pattern attributes, and
         catching the eventually raised exceptions. *)
      TRY
        GraphEventPattern.SetSourceNode(eventPattern, event.SNode);
      EXCEPT
        EventType.Mismatch =>    (* ignore this one *)
      END;
      TRY
        GraphEventPattern.SetSourceNodeExists(
          eventPattern, event.SNodeExists);
      EXCEPT
        EventType.Mismatch =>    (* ignore this one *)
      END;
      TRY
        GraphEventPattern.SetTargetNode(eventPattern, event.TNode);
      EXCEPT
        EventType.Mismatch =>    (* ignore this one *)
      END;
      TRY
        GraphEventPattern.SetTargetNodeExists(
          eventPattern, event.TNodeExists);
      EXCEPT
        EventType.Mismatch =>    (* ignore this one *)
      END;
      (* Now the common graph pattern attributes are set. *)
      GraphEventPattern.SetPoolName(eventPattern, pool.getBaseName());
      GraphEventPattern.SetPool(eventPattern, pool);
      GraphEventPattern.SetGraphNumber(
        eventPattern, graphInfo.externalNumber);
      GraphEventPattern.SetGraph(eventPattern, graphInfo.handle);
    END SetGraphPatternAttributes;
  PROCEDURE SetLogPatternAttributes (VAR eventPattern: EventPattern.T;
                                     graphInfo: GraphList.GraphInfo;
                                     pool     : GraphPool.T          )
    RAISES {EventType.Mismatch, EventType.Unknown} =
    BEGIN
      (* Setting the common log pattern attributes. *)
      LogEventPattern.SetPoolName(eventPattern, pool.getBaseName());
      LogEventPattern.SetPool(eventPattern, pool);
      LogEventPattern.SetGraphNumber(
        eventPattern, graphInfo.externalNumber);
      LogEventPattern.SetGraph(eventPattern, graphInfo.handle);
    END SetLogPatternAttributes;
  PROCEDURE RegisterNewTrigger (eventPattern: EventPattern.T;
                                graphNumber : GraphNumber     )
    RAISES {DaemonList.EntryAlreadyInList} = <* FATAL DaemonList.EntryNotInList *>
    VAR
      actionProcRecord: ActionProcRecord := NEW(ActionProcRecord);
      act: Action.Local := NEW(Action.Local).init(ExecuteTriggeredAction);
      triggerID : CARDINAL;
      triggerIDs: IntSeq.T  := NEW(IntSeq.T).init();
      trigger   : Trigger.T;
    BEGIN
      (* Generating a new trigger object. *)
      trigger :=
        Trigger.Create(eventPattern, act, Trigger.CouplingMode.Immediate,
                       priority, inhibiting := ContextSet.Empty(),
                       permitting := ContextSet.Empty());
      (* Constructing a record containing only a handle to the action
         procedure. *)
      actionProcRecord.actionProc := action;
      (* Registering the new trigger with the record containing the
         reference to the action procedure. *)
      triggerID := RuleEngine.RegisterTrigger(
                     trigger, RuleEngine.Interest.All, actionProcRecord);
      (* Now the newly registered trigger can be stored in the daemonlist.
         When no daemonlist entry exists for that graphNumber/event/action
         triple then one has to be added with the new triggerid, or the
         triggerID can be added to the triggerID list of an existing
         entry. *)
      IF daemonList.isEntry(graphNumber, event, action) THEN
        daemonList.getTriggerIDs(graphNumber, event, action).addhi(
          triggerID);
      ELSE
        triggerIDs.addhi(triggerID);
        daemonList.addEntry(graphNumber, event, action, triggerIDs);
      END;
    END RegisterNewTrigger;
  VAR
    eventPattern: EventPattern.T;
    graphInfo   : GraphList.GraphInfo;
    pool        : GraphPool.T;
  BEGIN
    TRY
      (* Checking if an entry with graphNumber/event/action already exists
         in the daemonlist. *)
      IF NOT daemonList.isEntry(graphNumber, event, action) THEN
        (* Getting the graph information from the graphlist. *)
        graphInfo := graphList.getEntry(graphNumber);
        (* Getting a handle to the pool. *)
        IF GetOpenPoolFromGraph(graphNumber, pool) THEN
          (* Now switching over the eventkind of the new event. *)
          (* The actions of this CASE statement contain 3 statements : *)
          (* 1.  Creating the appropriate event pattern. *)
          (* 2.  Setting the needed attributes of the event pattern. *)
          (* 3.  Registering a new trigger in the ruleengine with the *)
          (* new event pattern, and storing the triggerid with the *)
          (* graphnumber event and action in the daemonlist. *)
          CASE event.Kind OF
          | EventKind.EVNewNode =>
              eventPattern :=
                GraphEventPattern.Create(GraphEvents.Operation.CreateNode);
              SetGraphPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVDeleteNode =>
              eventPattern :=
                GraphEventPattern.Create(GraphEvents.Operation.DeleteNode);
              SetGraphPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVNewEdge =>
              eventPattern :=
                GraphEventPattern.Create(GraphEvents.Operation.CreateEdge);
              SetGraphPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVDeleteEdge =>
              eventPattern :=
                GraphEventPattern.Create(GraphEvents.Operation.DeleteEdge);
              SetGraphPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVModifyAttribute =>
              eventPattern := GraphEventPattern.Create(
                                GraphEvents.Operation.PutAttribute);
              SetGraphPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
              eventPattern := GraphEventPattern.Create(
                                GraphEvents.Operation.TruncateAttribute);
              SetGraphPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVIndexKeyModified =>
              eventPattern :=
                GraphEventPattern.Create(GraphEvents.Operation.PutIndex);
              SetGraphPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
              eventPattern := GraphEventPattern.Create(
                                GraphEvents.Operation.DeleteIndex);
              SetGraphPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVTransactionStart =>
              eventPattern := VirtualPageEventPattern.Create(
                                VirtualPageEvent.Operation.Begin);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVTransactionEnd =>
              eventPattern := VirtualPageEventPattern.Create(
                                VirtualPageEvent.Operation.Commit);
              RegisterNewTrigger(eventPattern, graphNumber);
              eventPattern := VirtualPageEventPattern.Create(
                                VirtualPageEvent.Operation.Abort);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVSetCheckpoint =>
              eventPattern :=
                LogEventPattern.Create(LogEvents.Operation.Checkpoint);
              SetLogPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVUndo =>
              eventPattern :=
                LogEventPattern.Create(LogEvents.Operation.Undo);
              SetLogPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVRedo =>
              eventPattern :=
                LogEventPattern.Create(LogEvents.Operation.Redo);
              SetLogPatternAttributes(eventPattern, graphInfo, pool);
              RegisterNewTrigger(eventPattern, graphNumber);
          | EventKind.EVOpenGraph =>
            (* not yet implemented*)
          | EventKind.EVCloseGraph =>
            (* not yet implemented *)
          | EventKind.EVUserDefined =>
              eventPattern :=
                NEW(EventPattern.T).init(
                  EventTypes.GetNumber(userDefinedEventTypeName));
              RegisterNewTrigger(eventPattern, graphNumber);
          END;
        END;
      END;
    EXCEPT
    | GraphList.EntryNotInList =>
        ErrorMessage("AGActivateDaemon", "GraphList.EntryNotInList");
    | EventType.Mismatch =>
        ErrorMessage("AGActivateDaemon", "EventType.Mismatch");
    | EventType.Unknown =>
        ErrorMessage("AGActivateDaemon", "EventType.Unknown");
    | EventTypes.Unknown =>
        ErrorMessage("AGActivateDaemon", "EventTypes.Unknown");
    END;

  END AGActivateDaemon;

PROCEDURE AGKillDaemon (graphNumber: GraphNumber;
                        event      : GraphEvent <*NOWARN*>;
                        action     : ActionProc             ) =
  VAR triggerIDs: IntSeq.T;
  BEGIN
    TRY
      (* Getting the triggerID from the daemonlist. *)
      triggerIDs := daemonList.getTriggerIDs(graphNumber, event, action);
      (* Removing the entire daemonlist entry. *)
      daemonList.removeEntry(graphNumber, event, action);
      (* Now unregistering the triggers in the rule engine. *)
      FOR i := 1 TO triggerIDs.size() DO
        RuleEngine.UnregisterTrigger(triggerIDs.get(i - 1));
      END;
    EXCEPT
    | DaemonList.EntryNotInList =>
        ErrorMessage("AGKillDaemon", "DaemonList.EntryNotInList");
    END;
  END AGKillDaemon;


PROCEDURE AGRaiseEvent (graphNumber: GraphNumber; p1, p2, p3: CARDINAL) =
  VAR
    pool           : GraphPool.T;
    transactionUnit: CARDINAL;
    graphInfo      : GraphList.GraphInfo;
    event          : Event.T;
    eventType      : EventType.T;
    eventTypeNumber: CARDINAL;
  BEGIN
    TRY
      (* Getting the graph information record from the graphlist. *)
      graphInfo := graphList.getEntry(graphNumber);
      IF GetOpenPoolFromGraph(graphNumber, pool) THEN
        (* Reading the transaction unit id from the pool. *)
        transactionUnit := pool.getRuleEngineID();
        (* We need the number to the user defined event type. *)
        eventTypeNumber := EventTypes.GetNumber(userDefinedEventTypeName);
        eventType := EventTypes.Get(eventTypeNumber);
        (* Now creating a new event and setting the three integer numbers
           as attribute values. *)
        event := NEW(Event.T).init(eventTypeNumber);
        event.setIntAttribute(eventType.getAttributeIndex("p1"), p1);
        event.setIntAttribute(eventType.getAttributeIndex("p2"), p2);
        event.setIntAttribute(eventType.getAttributeIndex("p3"), p3);
        (* Firing the user defined event. *)
        RuleEngine.Signal(transactionUnit, event)
      END;
    EXCEPT
    | GraphList.EntryNotInList =>
        ErrorMessage("AGRaiseEvent", "GraphList.EntryNotInList");
    | EventTypes.Unknown =>
        ErrorMessage("AGRaiseEvent", "EventTypes.Unknown");
    | EventType.Unknown =>
        ErrorMessage("AGRaiseEvent", "EventType.Unknown");
    | EventType.Mismatch =>
        ErrorMessage("AGRaiseEvent", "EventType.Mismatch");
    END;
  END AGRaiseEvent;

PROCEDURE AGDelayActionExecution (poolName: PoolName) =
  VAR
    tu  : CARDINAL;
    pool: GraphPool.T;
  BEGIN
    (* Getting a handle to the open pool. *)
    IF GetOpenPool(poolName, "AGDelayActionExecution", pool) THEN
      (* Reading the transaction unit from the pool. *)
      tu := pool.getRuleEngineID();
      RuleEngine.DelayActionExecution(tu);
    END;
  END AGDelayActionExecution;

PROCEDURE AGReleaseActionExecution (poolName: PoolName) =
  VAR
    tu  : CARDINAL;
    pool: GraphPool.T;
  BEGIN
    (* Getting a handle to the open pool. *)
    IF GetOpenPool(poolName, "AGDelayActionExecution", pool) THEN
      (* Reading the transaction unit from the pool. *)
      tu := pool.getRuleEngineID();
      RuleEngine.ReleaseActionExecution(tu);
    END;
  END AGReleaseActionExecution;

(*************************************************************************)
(**                                                                      *)
(** Operations on Local Sets                                             *)
(**                                                                      *)
(** Note:                                                                *)
(**                                                                      *)
(** Aborting transactions does not effect local sets!  They are not      *)
(** reset into a previous state.                                         *)
(**                                                                      *)
(*************************************************************************)

PROCEDURE AGSetCreate (<* UNUSED *>     graphNumber: GraphNumber;
                                    VAR emptySet   : SimpleSet    ) =
  VAR newSet: NodeSet.T := NodeSet.New();
  BEGIN
    emptySet := setList.addEntry(newSet);
  END AGSetCreate;

PROCEDURE AGSetClear (<* UNUSED *>     graphNumber: GraphNumber;
                                   VAR clearSet   : SimpleSet    ) =
  VAR newSet: NodeSet.T := NodeSet.New();
  BEGIN
    IF setList.isEntry(clearSet) THEN
      KillSetEntry(clearSet);
      clearSet := setList.addEntry(newSet);
    ELSE
      ErrorMessage("AGSetClear", "The set has not been created!");
    END;
  END AGSetClear;

PROCEDURE AGSetKill (<* UNUSED *>     graphNumber: GraphNumber;
                                  VAR set        : SimpleSet    ) =
  BEGIN
    KillSetEntry(set);
  END AGSetKill;

PROCEDURE AGSetCopy (<* UNUSED *>     graphNumber: GraphNumber;
                                      sourceSet  : SimpleSet;
                                  VAR targetSet  : SimpleSet    ) =
  <* FATAL SetList.EntryNotInList *>
  VAR set: NodeSet.T;
  BEGIN
    IF setList.isEntry(sourceSet) THEN
      set := setList.getEntry(sourceSet).handle.copy();
      targetSet := setList.addEntry(set);
    ELSE
      ErrorMessage("AGSetCopy", "The sourceset has not been created!");
    END;
  END AGSetCopy;

PROCEDURE AGSetInsertElement (<* UNUSED *> graphNumber: GraphNumber;
                                           element    : SimpleElement;
                                           set        : SimpleSet      ) =
  <* FATAL SetList.EntryNotInList *>
  BEGIN
    IF setList.isEntry(set) THEN
      setList.getEntry(set).handle.insert(element);
    ELSE
      ErrorMessage("AGSetInsertElement", "The set has not been created!");
    END;
  END AGSetInsertElement;

PROCEDURE AGSetDeleteElement (<* UNUSED *> graphNumber: GraphNumber;
                                           element    : SimpleElement;
                                           set        : SimpleSet      ) =
  <* FATAL SetList.EntryNotInList *>

  VAR found: BOOLEAN;
  BEGIN
    IF setList.isEntry(set) THEN
      setList.getEntry(set).handle.deleteElement(element, found);
    ELSE
      ErrorMessage("AGSetDeleteElement", "The set has not been created!");
    END;
  END AGSetDeleteElement;

PROCEDURE AGSetRemoveAnyElement (<* UNUSED *> graphNumber: GraphNumber;
                                 VAR found  : BOOLEAN;
                                 VAR element: SimpleElement;
                                     set    : SimpleSet      ) =
  <* FATAL SetList.EntryNotInList *>
  BEGIN
    IF setList.isEntry(set) THEN
      element := setList.getEntry(set).handle.extractAnyElement(found);
    ELSE
      ErrorMessage(
        "AGSetRemoveAnyElement", "The set has not been created!");
    END;
  END AGSetRemoveAnyElement;

PROCEDURE AGSetRemoveRndElement (<* UNUSED *> graphNumber: GraphNumber;
                                 VAR found  : BOOLEAN;
                                 VAR element: SimpleElement;
                                     set    : SimpleSet      ) =
  <* FATAL SetList.EntryNotInList *>
  VAR
    index : CARDINAL;
    random: Random.Default := NEW(Random.Default).init(FALSE);
    s     : NodeSet.T;
  BEGIN
    (* To remove a random element from the set the index of the element has
       to be randomly chosen.  The value has to be between 1 and the number
       of elements in the set.*)
    IF setList.isEntry(set) THEN
      s := setList.getEntry(set).handle;
      IF s.card() # 0 THEN
        found := TRUE;
        index := random.integer(1, s.card());
        (* Now we have to move on to the element with the chosen index. *)
        s.loop();
        FOR i := 1 TO index DO element := s.get(found); END;
        (* The element with the chosen index has to be removed. *)
        s.deleteElement(element, found);
      END;
    ELSE
      ErrorMessage(
        "AGSetRemoveRndElement", "The set has not been created!");
    END;
  END AGSetRemoveRndElement;

PROCEDURE AGSetIsElement (<* UNUSED *> graphNumber: GraphNumber;
                                       element    : SimpleElement;
                                       set        : SimpleSet      ):
  BOOLEAN =
  <* FATAL SetList.EntryNotInList *>
  VAR isElement: BOOLEAN := FALSE;
  BEGIN
    IF setList.isEntry(set) THEN
      isElement := setList.getEntry(set).handle.in(element);
    ELSE
      ErrorMessage("AGSetIsElement", "The set has not been created!");
    END;
    RETURN isElement;
  END AGSetIsElement;

PROCEDURE AGSetSize (<* UNUSED *> graphNumber: GraphNumber; set: SimpleSet):
  Word.T =
  <* FATAL SetList.EntryNotInList *>
  VAR size: Word.T := 0;
  BEGIN
    IF setList.isEntry(set) THEN
      size := setList.getEntry(set).handle.card();
    ELSE
      ErrorMessage("AGSetSize", "The set has not been created!");
    END;
    RETURN size;
  END AGSetSize;

PROCEDURE AGSetUnion (<* UNUSED *> graphNumber: GraphNumber;
                                   sourceSet  : SimpleSet;
                                   targetSet  : SimpleSet    ) =
  <* FATAL SetList.EntryNotInList *>
  BEGIN
    IF setList.isEntry(targetSet) AND setList.isEntry(sourceSet) THEN
      setList.getEntry(targetSet).handle.union(
        setList.getEntry(sourceSet).handle);
    ELSE
      ErrorMessage(
        "AGSetUnion", "The target or sourceset has not been created!");
    END;
  END AGSetUnion;

PROCEDURE AGSetIntersection (<* UNUSED *> graphNumber: GraphNumber;
                                          sourceSet  : SimpleSet;
                                          targetSet  : SimpleSet    ) =
  <* FATAL SetList.EntryNotInList *>
  BEGIN
    IF setList.isEntry(targetSet) AND setList.isEntry(sourceSet) THEN
      setList.getEntry(targetSet).handle.intersection(
        setList.getEntry(sourceSet).handle);
    ELSE
      ErrorMessage("AGSetIntersection",
                   "The target or sourceset has not been created!");
    END;
  END AGSetIntersection;

PROCEDURE AGSetDifference (<* UNUSED *> graphNumber: GraphNumber;
                                        sourceSet  : SimpleSet;
                                        targetSet  : SimpleSet    ) =
  <* FATAL SetList.EntryNotInList *>
  BEGIN
    IF setList.isEntry(targetSet) AND setList.isEntry(sourceSet) THEN
      setList.getEntry(targetSet).handle.difference(
        setList.getEntry(sourceSet).handle);
    ELSE
      ErrorMessage(
        "AGSetDifference", "The target or sourceset has not been created!");
    END;
  END AGSetDifference;

(*************************************************************************)
(**                                                                      *)
(** Operations on Local Relations                                        *)
(**                                                                      *)
(** Note:                                                                *)
(**                                                                      *)
(** Aborting transactions does not effect local relations!  They are     *)
(** not reset into a previous state.                                     *)
(**                                                                      *)
(*************************************************************************)

PROCEDURE AGRelCreate (<* UNUSED *>     graphNumber: GraphNumber;
                                    VAR emptySet   : RelSet       ) =
  BEGIN
    (* Creating an empty set. *)
    emptySet := relSetList.addEntry(NodeTypeRelation.New());
  END AGRelCreate;

PROCEDURE AGRelClear (<* UNUSED *> Graph: GraphNumber; VAR ClearSet: RelSet) =
  BEGIN
    (* Clearing set by disposing the old, and creating a new empty set. *)
    IF relSetList.isEntry(ClearSet) THEN
      KillRelSetEntry(ClearSet);
      ClearSet := relSetList.addEntry(NodeTypeRelation.New());
    ELSE
      ErrorMessage("AGRelClear", "The set has not been created!");
    END;
  END AGRelClear;

PROCEDURE AGRelKill (<* UNUSED *> graphNumber: GraphNumber; VAR set: RelSet) =
  BEGIN
    KillRelSetEntry(set);
  END AGRelKill;

PROCEDURE AGRelCopy (<* UNUSED *>     graphNumber: GraphNumber;
                                      sourceSet  : RelSet;
                                  VAR targetSet  : RelSet       ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(sourceSet) THEN
      targetSet :=
        relSetList.addEntry(relSetList.getEntry(sourceSet).handle.copy());
    ELSE
      ErrorMessage("AGRelCopy", "The source set has not been created!");
    END;
  END AGRelCopy;

PROCEDURE AGRelInsertTuple (<* UNUSED *> graphNumber : GraphNumber;
                                         surr1, surr2: TypeNumber;
                                         set         : RelSet       ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(set) THEN
      relSetList.getEntry(set).handle.insert(surr1, surr2);
    ELSE
      ErrorMessage("AGRelInsertTuple", "The set has not been created!");
    END;
  END AGRelInsertTuple;

PROCEDURE AGRelDeleteTuple (<* UNUSED *> graphNumber : GraphNumber;
                                         surr1, surr2: TypeNumber;
                                         set         : RelSet       ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(set) THEN
      relSetList.getEntry(set).handle.deleteElement(surr1, surr2);
    ELSE
      ErrorMessage("AGRelDeleteTuple", "The set has not been created!");
    END;
  END AGRelDeleteTuple;

PROCEDURE AGRelRemoveAnyTuple (<* UNUSED *>     graphNumber : GraphNumber;
                                            VAR found       : BOOLEAN;
                                            VAR surr1, surr2: TypeNumber;
                                                set         : RelSet       ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(set) THEN
      relSetList.getEntry(set).handle.extractAnyElement(
        surr1, surr2, found);
    ELSE
      ErrorMessage("AGRelRemoveAnyTuple", "The set has not been created!");
    END;
  END AGRelRemoveAnyTuple;

PROCEDURE AGRelRemoveRndTuple (<* UNUSED *>     graphNumber : GraphNumber;
                                            VAR found       : BOOLEAN;
                                            VAR surr1, surr2: TypeNumber;
                                                set         : RelSet       ) =
  <* FATAL RelSetList.EntryNotInList *>
  VAR
    random: Random.Default     := NEW(Random.Default).init(FALSE);
    index : CARDINAL;
    s     : NodeTypeRelation.T;
  BEGIN
    IF relSetList.isEntry(set) THEN
      s := relSetList.getEntry(set).handle;
      (* Checking if the number of elements contained in the set is greater
         than 0. *)
      found := s.card() > 0;
      IF found THEN
        (* Now a random index between 1 and the number of elements in the
           set is chosen. *)
        index := random.integer(1, s.card());
        s.loop();
        (* Now we just have to move on to the element with the randomly
           chosen index. *)
        FOR i := 1 TO index DO s.get(surr1, surr2, found); END;
        (* The chosen element has to be removed from the set. *)
        s.deleteElement(surr1, surr2);
      END;
    ELSE
      ErrorMessage("AGRelRemoveRndTuple", "The set has not been created!");
    END;
  END AGRelRemoveRndTuple;

PROCEDURE AGRelIsElement (<* UNUSED *> graphNumber : GraphNumber;
                                       surr1, surr2: TypeNumber;
                                       set         : RelSet       ):
  BOOLEAN =
  <* FATAL RelSetList.EntryNotInList *>
  VAR result: BOOLEAN := FALSE;
  BEGIN
    IF relSetList.isEntry(set) THEN
      result := relSetList.getEntry(set).handle.in(surr1, surr2);
    ELSE
      ErrorMessage("AGRelIsElement", "The set has not been created!");
    END;
    RETURN result
  END AGRelIsElement;

PROCEDURE AGRelSize (<* UNUSED *> graphNumber: GraphNumber; set: RelSet):
  Word.T =
  <* FATAL RelSetList.EntryNotInList *>
  VAR result: Word.T := 0;
  BEGIN
    IF relSetList.isEntry(set) THEN
      result := relSetList.getEntry(set).handle.card();
    ELSE
      ErrorMessage("AGRelSize", "The set has not been created!");
    END;
    RETURN result
  END AGRelSize;

PROCEDURE AGRelUnion (<* UNUSED *> graphNumber: GraphNumber;
                                   sourceSet  : RelSet;
                                   targetSet  : RelSet       ) =
  <* FATAL RelSetList.EntryNotInList *>

  BEGIN
    IF relSetList.isEntry(sourceSet) AND relSetList.isEntry(targetSet) THEN
      relSetList.getEntry(targetSet).handle.union(
        relSetList.getEntry(sourceSet).handle);
    ELSE
      ErrorMessage(
        "AGRelUnion", "The source or targetset has not been vreated!");
    END;
  END AGRelUnion;

PROCEDURE AGRelIntersection (<* UNUSED *> graphNumber: GraphNumber;
                                          sourceSet  : RelSet;
                                          targetSet  : RelSet       ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(sourceSet) AND relSetList.isEntry(targetSet) THEN
      relSetList.getEntry(targetSet).handle.intersection(
        relSetList.getEntry(sourceSet).handle);
    ELSE
      ErrorMessage("AGRelIntersection",
                   "The source or targetset has not been created!");
    END;
  END AGRelIntersection;

PROCEDURE AGRelDifference (<* UNUSED *> graphNumber: GraphNumber;
                                        sourceSet  : RelSet;
                                        targetSet  : RelSet       ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(sourceSet) AND relSetList.isEntry(targetSet) THEN
      relSetList.getEntry(targetSet).handle.difference(
        relSetList.getEntry(sourceSet).handle);
    ELSE
      ErrorMessage(
        "AGRelDifference", "The source or targetset has not been created!");
    END;
  END AGRelDifference;

PROCEDURE AGCartesian (<* UNUSED *>     graphNumber: GraphNumber;
                                        sourceSet1 : SimpleSet;
                                        sourceSet2 : SimpleSet;
                                    VAR targetSet  : RelSet       ) =
  <* FATAL SetList.EntryNotInList *>
  BEGIN
    IF setList.isEntry(sourceSet1) AND setList.isEntry(sourceSet2) THEN
      WITH target = NodeTypeRelation.New() DO
        target.cartesian(setList.getEntry(sourceSet1).handle,
                         setList.getEntry(sourceSet2).handle);
        targetSet := relSetList.addEntry(target);
      END;
    ELSE
      ErrorMessage(
        "AGCartesian", "The source or targetset has not been created!");
    END;
  END AGCartesian;

PROCEDURE AGProjectionFirst (<* UNUSED *>     graphNumber: GraphNumber;
                                              sourceSet  : RelSet;
                                          VAR targetSet  : SimpleSet    ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(sourceSet) THEN
      targetSet := setList.addEntry(
                     relSetList.getEntry(sourceSet).handle.projection1());
    ELSE
      ErrorMessage(
        "AGProjectionFirst", "The source set has not been created!");
    END;
  END AGProjectionFirst;

PROCEDURE AGProjectionSecond (<* UNUSED *>     graphNumber: GraphNumber;
                                               sourceSet  : RelSet;
                                           VAR targetSet  : SimpleSet    ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(sourceSet) THEN
      targetSet := setList.addEntry(
                     relSetList.getEntry(sourceSet).handle.projection2());
    ELSE
      ErrorMessage(
        "AGProjectionSecond", "The source set has not been created!");
    END;
  END AGProjectionSecond;

PROCEDURE AGQueryProjection (<* UNUSED *>     graphNumber: GraphNumber;
                                              sourceSet  : RelSet;
                                              surr       : TypeNumber;
                                          VAR targetSet  : SimpleSet    ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(sourceSet) THEN
      targetSet :=
        setList.addEntry(
          relSetList.getEntry(sourceSet).handle.queryProjection2(surr));
    ELSE
      ErrorMessage(
        "AGQueryProjection", "The source set has not been created!");
    END;
  END AGQueryProjection;

PROCEDURE AGSingleQueryProjection (<* UNUSED *>     graph: GraphNumber;
                                                    set  : RelSet;
                                                    surr1: TypeNumber;
                                                VAR surr2: TypeNumber;
                                                VAR found: BOOLEAN      ) =
  <* FATAL RelSetList.EntryNotInList *>
  BEGIN
    IF relSetList.isEntry(set) THEN
      surr2 := relSetList.getEntry(set).handle.singleQueryProjection2(
                 surr1, found);
    ELSE
      ErrorMessage(
        "AGSingleQueryProjection", "The set has not been created!");
    END;
  END AGSingleQueryProjection;

PROCEDURE AGChange (<* UNUSED *> graphNumber: GraphNumber; VAR set: RelSet) =
  <* FATAL RelSetList.EntryNotInList *>
  VAR
    elem1, elem2: SimpleElement;
    ok          : BOOLEAN;
    s_old, s_new: NodeTypeRelation.T;
  BEGIN
    IF relSetList.isEntry(set) THEN
      s_old := relSetList.getEntry(set).handle;
      s_new := NodeTypeRelation.New();
      (* All relation tuples in the set have to be reverterd: (a,b) ->
         (b,a)*)
      s_old.loop();
      s_old.get(elem1, elem2, ok);
      WHILE ok DO
        s_new.insert(elem2, elem1);
        s_old.get(elem1, elem2, ok);
      END;
      KillRelSetEntry(set);
      set := relSetList.addEntry(s_new);
    ELSE
      ErrorMessage("AGChange", "The set has not been created!");
    END;
  END AGChange;

(*************************************************************************)
(**                                                                      *)
(** PROCEDUREs to work with Logs                                         *)
(**                                                                      *)
(*************************************************************************)

PROCEDURE AGSetCheckpoint (graphNumber: GraphNumber) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGSetCheckpoint", graph) THEN
        (* Setting the checkpoint. *)
        graph.setCheckpoint();
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGSetCheckpoint", "Access.Locked");
    | ChgMgmtGraph.InternalError (m) =>
        ErrorMessage("AGSetCheckPoint", "ChgMgmtGraph.InternalError : "
                                          & ErrorSupport.ToText(m),
                     halt := TRUE);
    | ChgMgmtGraph.LogError (m) =>
        ErrorMessage("AGSetCheckPoint",
                     "ChgMgmtGraph.LogError : " & ErrorSupport.ToText(m));
    | ChgMgmtGraph.NoLog =>
        ErrorMessage("AGSetCheckPoint", "ChgMgmtGraph.NoLog");
    END;
  END AGSetCheckpoint;

PROCEDURE AGUndo (graphNumber: GraphNumber; VAR n: CARDINAL) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGUndo", graph) THEN
        graph.undo(n);
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGUndo", "Access.Locked");
    | ChgMgmtGraph.InternalError (m) =>
        ErrorMessage("AGUndo", "ChgMgmtGraph.InternalError : "
                                 & ErrorSupport.ToText(m), halt := TRUE);
    | ChgMgmtGraph.LogError (m) =>
        ErrorMessage(
          "AGUndo", "ChgMgmtGraph.LogError : " & ErrorSupport.ToText(m));
    | ChgMgmtGraph.NoLog => ErrorMessage("AGUndo", "ChgMgmtGraph.NoLog");
    END;
  END AGUndo;

PROCEDURE AGRedo (graphNumber: GraphNumber; VAR n: CARDINAL) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGRedo", graph) THEN
        graph.redo(n);
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGRedo", "Access.Locked");
    | ChgMgmtGraph.InternalError (m) =>
        ErrorMessage("AGRedo", "ChgMgmtGraph.InternalError : "
                                 & ErrorSupport.ToText(m), halt := TRUE);
    | ChgMgmtGraph.LogError (m) =>
        ErrorMessage(
          "AGRedo", "ChgMgmtGraph.LogError : " & ErrorSupport.ToText(m));
    | ChgMgmtGraph.NoLog => ErrorMessage("AGRedo", "ChgMgmtGraph.NoLog");
    END;
  END AGRedo;

(*************************************************************************)
(**                                                                      *)
(** Scheme Management                                                    *)
(**                                                                      *)
(*************************************************************************)

(*************************************************************************)
(**                                                                      *)
(** Operations for managing schemes                                      *)
(**                                                                      *)
(*************************************************************************)

PROCEDURE AGDeclareScheme (    schemeName  : SchemeName;
                           VAR schemeNumber: SchemeNumber;
                               poolName    : PoolName      ) =
  VAR
    newScheme: Scheme.T;
    pool     : GraphPool.T;
    newOne   : BOOLEAN;
  <* FATAL SchemeList.EntryAlreadyInList *>
  BEGIN
    TRY
      (* Checking if the scheme is not already open. *)
      IF NOT schemeList.isEntryByName(schemeName, poolName) THEN
        (* Now getting the open pool from the poollist. *)
        IF GetOpenPool(poolName, "AGDeclareScheme", pool) THEN
          pool.beginTransaction();
          (* Testing if the scheme already exists, and storing the result
             in newOne. *)
          newOne := NOT pool.existsScheme(schemeName);
          pool.commitTransaction();
          (* Now the new scheme can be opened, and entered into the
             schemelist. *)
          newScheme := NEW(Scheme.T).open(pool, schemeName, new := newOne);
          schemeNumber :=
            schemeList.addEntry(newScheme, schemeName, poolName);
        END;
      ELSE
        (* If the scheme was already open, an error message is
           displayed. *)
        ErrorMessage("AGDeclareScheme", "The scheme is already open!");
      END;
    EXCEPT
    | Access.Denied (m) =>
        ErrorMessage("AGDeclareScheme", "Access.Denied : " & m);
    | Access.Locked => ErrorMessage("AGDeclareScheme", "Access.Locked");
    | Scheme.Existent =>
        ErrorMessage("AGDeclareScheme", "Scheme.Existent");
    | Scheme.InUse => ErrorMessage("AGDeclareScheme", "Scheme.InUse");
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGDeclareScheme",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGDeclareScheme",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Scheme.NoValidScheme =>
        ErrorMessage("AGDeclareScheme", "Scheme.NoValidScheme");
    | Scheme.NotExistent =>
        ErrorMessage("AGDeclareScheme", "Scheme.NotExistent");
    | GraphPool.CardinalityError (m) =>
        ErrorMessage(
          "AGDeclareScheme",
          "GraphPool.CardinalityError : pool number " & Fmt.Int(m));
    | GraphPool.NotInTransaction =>
        ErrorMessage("AGDeclareScheme", "GraphPool.NotInTransaction");

    END;
  END AGDeclareScheme;

PROCEDURE AGCommitScheme (schemeNumber: SchemeNumber) =
  <* FATAL SchemeList.EntryNotInList *>
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      IF GetOpenScheme(schemeNumber, "AGCommitScheme", scheme) THEN
        (* The committing can be done by closing the scheme, and removing
           it from the schemelist. *)
        scheme.close();
        schemeList.removeEntry(schemeNumber);
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGCommitScheme",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGCommitScheme;

PROCEDURE AGExistsScheme (poolName: PoolName; schemeName: SchemeName):
  BOOLEAN =
  VAR
    pool  : GraphPool.T;
    result: BOOLEAN     := FALSE;
  BEGIN
    TRY
      (* The open pool can be received from the poollist. *)
      IF GetOpenPool(poolName, "AGExistsScheme", pool) THEN
        (* Now it can be checked whether the scheme exists. *)
        result := pool.existsScheme(schemeName);
      END;
    EXCEPT
    | GraphPool.InternalError (m) =>
        ErrorMessage("AGExistsSheme",
                     "GraphPool.InternalError : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Access.Locked => ErrorMessage("AGExistsScheme", "Access.Locked");
    END;
    RETURN result;
  END AGExistsScheme;

PROCEDURE AGShowSchemeName (    schemeNumber: SchemeNumber;
                            VAR name        : SchemeName    ) =
  <* FATAL SchemeList.EntryNotInList *>
  BEGIN
    IF schemeList.isEntry(schemeNumber) THEN
      name := schemeList.getEntry(schemeNumber).name;
    ELSE
      ErrorMessage("AGShowSchemeName", "The scheme is not open!");
    END
  END AGShowSchemeName;

(* not implemented *)
(*
PROCEDURE AGSetSchemeVersion (          Scheme : SchemeNumber;
                               Version: TEXT          ) =
  BEGIN
  END AGSetSchemeVersion;

PROCEDURE AGShowSchemeVersion (     Scheme : SchemeNumber;
                                VAR Version: TEXT          ) =
  BEGIN
  END AGShowSchemeVersion;
*)

(******************************************************************************)
(**                                                                           *)
(** PROCEDUREs for managing node classes                                     *)
(**                                                                           *)
(******************************************************************************)

PROCEDURE AGDeclareNodeClass (    schemeNumber: SchemeNumber;
                                  className   : TEXT;
                              VAR nodeClass   : TypeNumber    ) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      IF GetOpenScheme(schemeNumber, "AGDeclareNodeClass", scheme) THEN
        (* Declaring the node class *)
        nodeClass := scheme.declareNodeClass(className);
      END;
    EXCEPT
    | Scheme.AlreadyDeclared =>
        ErrorMessage("AGDeclareNodeClass", "Scheme.AlreadyDeclared");
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGDeclareNodeClass",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGDeclareNodeClass;

PROCEDURE AGTestAndShowNodeClassNumber (graphOrSchemeNumber: GraphNumber;
                                        className          : TEXT;
                                        VAR existent : BOOLEAN;
                                        VAR nodeClass: TypeNumber;
                                        isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* Determining whether the scheme is in the schemelist, and getting a
         reference to it. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGTestAndShowNodeClassNumber", scheme);
      IF existent THEN
        (* If it was in the schemelist, the operation can take place. *)
        existent := scheme.existsNodeClassByName(className);
        IF existent THEN
          nodeClass := scheme.getNodeClassNumber(className);
        END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGTestAndShowNodeClassNumber",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGTestAndShowNodeClassNumber", "Scheme.NotDeclared");
    END;
  END AGTestAndShowNodeClassNumber;

PROCEDURE AGShowNodeClassName (    graphOrSchemeNumber: GraphNumber;
                                   nodeClass          : TypeNumber;
                               VAR className          : TEXT;
                               VAR existent           : BOOLEAN;
                               isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* Determining whether the scheme is in the schemelist, and getting a
         reference to it. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGShowNodeClassName", scheme);
      IF existent THEN
        (* If it was in the schemelist, the operation can take place. *)
        existent := scheme.existsNodeClassByNumber(nodeClass);
        IF existent THEN
          className := scheme.getNodeClassName(nodeClass);
        END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGShowNodeClassName",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowNodeClassName", "Scheme.NotDeclared");
    END;
  END AGShowNodeClassName;

PROCEDURE AGShowAllNodeTypes (    graphOrSchemeNumber: GraphNumber;
                                  nodeClass          : TypeNumber;
                              VAR setOfTypes         : SimpleSet;
                              isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      IF GetScheme(graphOrSchemeNumber, isGraphNumber,
                   "AGShowAllNodeTypes", scheme) THEN
        setOfTypes :=
          setList.addEntry(scheme.getAllNodeTypesOfClass(nodeClass));
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGShowAllNodeTypes",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowAllNodeTypes", "Scheme.NotDeclared");
    END;
  END AGShowAllNodeTypes;


PROCEDURE AGAppendNodeClass (schemeNumber  : SchemeNumber;
                             nodeClassName : TEXT;
                             superClassName: TEXT          ) =
  VAR
    scheme               : Scheme.T;
    superClass, nodeClass: TypeNumber;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      IF GetOpenScheme(schemeNumber, "AGAppendNodeClass", scheme) THEN
        (* First getting the numbers for the given classnames from the
           scheme. *)
        superClass := scheme.getNodeClassNumber(superClassName);
        nodeClass := scheme.getNodeClassNumber(nodeClassName);
        (* Now the operation can take place. *)
        scheme.appendNodeClass(nodeClass, superClass);
      END;
    EXCEPT
    | Scheme.NotDeclared =>
        ErrorMessage("AGAppendNodeClass", "Scheme.NodeDeclared");
    | Scheme.AttributeNameClash =>
        ErrorMessage("AGAppendNodeClass", "Scheme.AttributeNameClash");
    | Scheme.Cyclic => ErrorMessage("AGAppendNodeClass", "Scheme.Cyclic");
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGAppendNodeClass",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.AttributePlacementClash =>
        ErrorMessage("AGAppendNodeClass", "Scheme.AttributePlacementClash");
    END;
  END AGAppendNodeClass;

(******************************************************************************)
(**                                                                           *)
(** PROCEDUREs for managing node types                                        *)
(**                                                                           *)
(******************************************************************************)

PROCEDURE AGDeclareNodeType (    schemeNumber: SchemeNumber;
                                 name        : TEXT;
                             VAR nType       : TypeNumber    ) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The open scheme can be received from the schemeList. *)
      IF GetOpenScheme(schemeNumber, "AGDeclareNodeType", scheme) THEN
        (* Now it has to be checked whether the node type already
           exists. *)
        IF scheme.existsNodeTypeByName(name) THEN
          (* The new node type number is received from the scheme.. *)
          nType := scheme.getNodeTypeNumber(name)
        ELSE
          (* The new node type is created. *)
          nType := scheme.declareNodeType(name);
        END
      END;
    EXCEPT
    | Scheme.AlreadyDeclared =>
        ErrorMessage("AGDeclareNodeType", "Scheme.AlreadyDeclared");
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGDeclareNodeType",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGDeclareNodeType", "Scheme.NotDeclared");
    END
  END AGDeclareNodeType;

PROCEDURE AGTestAndShowNodeTypeNumber (    graphOrSchemeNumber: CARDINAL;
                                           name               : TEXT;
                                       VAR existent           : BOOLEAN;
                                       VAR nType: TypeNumber;
                                       isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* Checking whether the scheme is open *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGTestAndShowNodeTypeNumber", scheme);
      IF existent THEN
        (* When the scheme could bereceived, the existence of the nodetype
           is checked. *)
        IF scheme.existsNodeTypeByName(name) THEN
          (* If existing, then the type number is returned. *)
          nType := scheme.getNodeTypeNumber(name);
        ELSE
          existent := FALSE
        END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGTestAndShowNodeTypeNumber",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGTextAndShowNodeTypeNumber", "Scheme.NotDeclared");
    END;
  END AGTestAndShowNodeTypeNumber;

PROCEDURE AGShowNodeTypeName (    graphOrSchemeNumber: CARDINAL;
                                  nType              : TypeNumber;
                              VAR name               : TEXT;
                              VAR existent           : BOOLEAN;
                                  isGraphNumber      : BOOLEAN      := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* Checking if the scheme is open. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGShowNodeTypeName", scheme);
      IF existent THEN
        (* When the scheme could be received, the existence of the node
           type is checked. *)
        IF scheme.existsNodeTypeByNumber(nType) THEN
          (* If existent, its name is returned. *)
          name := scheme.getNodeTypeName(nType);
        ELSE
          existent := FALSE
        END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGShowNodeTypeName",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowNodeTypeName", "Scheme.NotDeclared");
    END;
  END AGShowNodeTypeName;

PROCEDURE AGAppendNodeType (SchemeNumber: CARDINAL;
                            nType       : TEXT;
                            nodeClass   : TEXT      ) =
  VAR
    scheme                         : Scheme.T;
    nodeTypeNumber, nodeClassNumber: NodeNumber;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      IF GetScheme(SchemeNumber, FALSE, "AGAppendNodeType", scheme) THEN
        (* Now converting the type and class names into typenumbers. *)
        nodeTypeNumber := scheme.getNodeTypeNumber(nType);
        nodeClassNumber := scheme.getNodeClassNumber(nodeClass);
        (* Now the operation can take place. *)
        scheme.appendNodeType(nodeTypeNumber, nodeClassNumber);
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGAppendNodeType",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGAppendNodeType", "Scheme.NotDeclared");
    | Scheme.AlreadyDeclared =>
        ErrorMessage("AGAppendNodeType", "Scheme.AlreadyDeclared");
    | Scheme.AttributePlacementClash =>
        ErrorMessage("AGAppendNodeType", "Scheme.AttributePlacementClash");
    END;
  END AGAppendNodeType;

(******************************************************************************)
(**                                                                           *)
(** PROCEDUREs for managing edge types                                        *)
(**                                                                           *)
(******************************************************************************)


PROCEDURE AGDeclareEdgeType (    schemeNumber         : SchemeNumber;
                                 name                 : TEXT;
                                 sourceNodeClassOrType: TEXT;
                                 sourceCardinality    : Cardinality;
                                 targetNodeClassOrType: TEXT;
                                 targetCardinality    : Cardinality;
                             VAR edge                 : TypeNumber    ) =
  VAR
    scheme                                    : Scheme.T;
    newSourceCardinality, newTargetCardinality: Scheme.Cardinality;
    sourceNodeClassOrTypeNumber, targetNodeClassOrTypeNumber: TypeNumber;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      IF GetOpenScheme(schemeNumber, "AGDeclareEdgeType", scheme) THEN
        (* The existence of the edgetype is checked *)
        IF scheme.existsEdgeTypeByName(name) THEN
          (* If already existent, the number of the existing edge type is
             returned. *)
          edge := scheme.getEdgeTypeNumber(name)
        ELSE
          (* First the cardinalities have to be converted into the new
             cardinality format. *)
          newSourceCardinality :=
            ConvertToNewCardinality(sourceCardinality);
          newTargetCardinality :=
            ConvertToNewCardinality(targetCardinality);
          (* The class/typenames have to be converted into their
             corresponding class/typenumbers. *)
          sourceNodeClassOrTypeNumber :=
            scheme.getTypeNumber(sourceNodeClassOrType);
          targetNodeClassOrTypeNumber :=
            scheme.getTypeNumber(targetNodeClassOrType);
          (* Finally the new edge type can be generated. *)
          edge := scheme.declareEdgeType(
                    name, sourceNodeClassOrTypeNumber,
                    targetNodeClassOrTypeNumber, newSourceCardinality,
                    newTargetCardinality);
        END
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGDeclareEdgeType",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGDeclareEdgeType", "Scheme.NotDeclared");
    | Scheme.AlreadyDeclared =>
        ErrorMessage("AGDeclareEdgeType", "Scheme.AlreadyDeclared");
    END;
  END AGDeclareEdgeType;

PROCEDURE AGTestAndShowEdgeTypeNumber (    graphOrSchemeNumber: CARDINAL;
                                           name               : TEXT;
                                       VAR existent           : BOOLEAN;
                                       VAR edge: TypeNumber;
                                       isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGTestAndShowEdgeTypeNumber", scheme);
      (* If the scheme could be received, getting the edge typenumber via
         the scheme. *)
      IF existent THEN
        existent := scheme.existsEdgeTypeByName(name);
        IF existent THEN edge := scheme.getEdgeTypeNumber(name); END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGTestAndShowEdgeTypeNumber",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGTestAndShowEdgeTypeNumber", "Scheme.NotDeclared");
    END;
  END AGTestAndShowEdgeTypeNumber;

PROCEDURE AGShowEdgeTypeName (    graphOrSchemeNumber: CARDINAL;
                                  edge               : TypeNumber;
                              VAR name               : TEXT;
                              VAR existent           : BOOLEAN;
                                  isGraphNumber      : BOOLEAN      := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* Checking the existence of the scheme, and getting it from the
         schemelist or the graph. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGShowEdgeTypeName", scheme);
      (* When the scheme could be found, the existence of the edge with the
         chosen type is checked. *)
      IF existent THEN
        existent := scheme.existsEdgeTypeByNumber(edge);
        (* When the edge exists, the name can be received from the
           scheme. *)
        IF existent THEN name := scheme.getEdgeTypeName(edge); END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage("AGShowEdgeTypeName",
                     "Scheme.InternalNumber : " & ErrorSupport.ToText(m),
                     halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowEdgeTypeName", "Scheme.NotDeclared");
    END;
  END AGShowEdgeTypeName;

PROCEDURE AGShowEdgeTypeProps (    graphOrSchemeNumber: CARDINAL;
                                   edge               : TypeNumber;
                               VAR sourceCT           : TypeNumber;
                               VAR sourceCardinality  : Cardinality;
                               VAR targetCT           : TypeNumber;
                               VAR targetCardinality  : Cardinality;
                               VAR existent           : BOOLEAN;
                               isGraphNumber: BOOLEAN := TRUE) =
  VAR
    scheme                                    : Scheme.T;
    newSourceCardinality, newTargetCardinality: Scheme.Cardinality;
  BEGIN
    TRY
      (* Checking the existence of the scheme, and getting it from the
         schemelist or the graph. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGShowEdgeTypeProps", scheme);
      (* When the scheme exists, the existence of the chosen edge has to be
         checked. *)
      IF existent THEN
        IF scheme.existsEdgeTypeByNumber(edge) THEN
          (* When the edge exists, its properties are read. *)
          scheme.showEdgeTypeProps(
            edge, sourceCT, targetCT, newSourceCardinality,
            newTargetCardinality);
          (* The cardinalities have to be converted to the old cardinality
             style. *)
          sourceCardinality :=
            ConvertToOldCardinality(newSourceCardinality);
          targetCardinality :=
            ConvertToOldCardinality(newTargetCardinality);
        ELSE
          existent := FALSE
        END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGShowEdgeTypeProps",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowEdgeTypeProps", "Scheme.NotDeclared");
    END;
  END AGShowEdgeTypeProps;

(******************************************************************************)
(**                                                                           *)
(** PROCEDUREs for managing attributes                                        *)
(**                                                                           *)
(******************************************************************************)

PROCEDURE AGDeclareAttribute (    schemeNumber    : SchemeNumber;
                                  classOrType     : TEXT;
                                  name            : TEXT;
                                  kind            : AttributeKind;
                                  props           : IndexProperties;
                                  valueType       : ValueTypeNumber;
                                  valueCardinality: Cardinality;
                                  constantLength  : BOOLEAN;
                                  length          : Word.T;
                              VAR attNo           : AttributeNumber  ) =
  VAR
    scheme       : Scheme.T;
    classOrTypeNr: TypeNumber;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      IF GetOpenScheme(schemeNumber, "AGDeclareAttribute", scheme) THEN
        (* Changing the class or type name into the according number. *)
        classOrTypeNr := scheme.getTypeNumber(classOrType);
        (* Now declaring the attribute. *)
        attNo := scheme.declareAttribute(
                   classOrTypeNr, name, kind, props, valueType,
                   ConvertToNewCardinality(valueCardinality),
                   constantLength, length);
      END;
    EXCEPT
    | Scheme.AlreadyDeclared =>
        ErrorMessage("AGDeclareAttribute", "Scheme.AlreadyDeclared");
    | Scheme.AttributeNameClash =>
        ErrorMessage("AGDeclareAttribute", "Scheme.AttributeNameClash");
    | Scheme.NotDeclared =>
        ErrorMessage("AGDeclareAttribute", "Scheme.NotDeclared");
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGDeclareAttribute",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.AttributePlacementClash =>
        ErrorMessage(
          "AGDeclareAttribute", "Scheme.AttributePlacementClash");
    END;
  END AGDeclareAttribute;

PROCEDURE AGTestAndShowAttributeNumber (graphOrSchemeNumber: GraphNumber;
                                        name               : TEXT;
                                        classOrType        : TypeNumber;
                                        VAR existent: BOOLEAN;
                                        VAR attNo   : AttributeNumber;
                                        isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* Checking the existence of the scheme, and getting it from the
         schemelist or the graph. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGTestAndShowAttributeNumber", scheme);
      IF existent THEN
        (* When the scheme exists, the existence of the attribute can be
           checked. *)
        existent := scheme.existsAttributeByName(classOrType, name);
        IF existent THEN
          (* When the attribute exists, its number can be received from the
             scheme. *)
          attNo := scheme.getAttributeNumber(classOrType, name);
        END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGTestAndShowAttributeNumber",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGTestAndShowAttributeNumber", "Scheme.NotDeclared");
    END;
  END AGTestAndShowAttributeNumber;

PROCEDURE AGShowAttributeName (    graphOrSchemeNumber: GraphNumber;
                                   attNo              : AttributeNumber;
                               VAR name               : TEXT;
                               VAR existent           : BOOLEAN;
                               isGraphNumber: BOOLEAN := TRUE) =
  VAR
    scheme: Scheme.T;
    class : Scheme.ID;
  BEGIN
    TRY
      (* Checking the existence of the scheme, and getting it from the
         schemelist or the graph. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGShowAttributeName", scheme);
      IF existent THEN
        (* When the scheme exists, the existence of the attribute can be
           checked. *)
        existent := scheme.existsAttributeByNumber(attNo);
        IF existent THEN
          (* When the attribute exists, its name and class can be received
             from the scheme. *)
          scheme.getAttributeNameAndClass(attNo, class, name)
        END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGShowAttributeName",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowAttributeName", "Scheme.NotDeclared");
    END;
  END AGShowAttributeName;

PROCEDURE AGShowAttributeProps (    graphOrSchemeNumber: GraphNumber;
                                    attNo              : AttributeNumber;
                                VAR kind               : AttributeKind;
                                VAR props              : IndexProperties;
                                VAR valueType          : ValueTypeNumber;
                                VAR attCard            : Cardinality;
                                VAR constLength        : BOOLEAN;
                                VAR length             : CARDINAL;
                                VAR existent           : BOOLEAN;
                                isGraphNumber: BOOLEAN := TRUE) =
  VAR
    scheme    : Scheme.T;
    newAttCard: Scheme.Cardinality;
  BEGIN
    TRY
      (* Checking the existence of the scheme, and getting it from the
         schemelist or the graph. *)
      existent := GetScheme(graphOrSchemeNumber, isGraphNumber,
                            "AGShowAttributeProps", scheme);
      IF existent THEN
        (* When the scheme exists, the existence of the attribute is
           checked. *)
        existent := scheme.existsAttributeByNumber(attNo);
        IF existent THEN
          (* When the attribute exists, its properties can be received from
             the scheme. *)
          scheme.showAttributeProps(
            attNo, kind, props, valueType, newAttCard, constLength, length);
          (* Converting the attribute cardinality to the old cardinality
             style. *)
          attCard := ConvertToOldCardinality(newAttCard);
        END;
      END;
    EXCEPT
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowAttributeProps", "Scheme.NotDeclared");
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGShowAttributeProps",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END
  END AGShowAttributeProps;

PROCEDURE AGPutMetaAttribute (schemeNumber: GraphNumber;
                              nType       : TypeNumber;
                              attNo       : AttributeNumber;
                              attBegin    : Word.T;
                              attLength   : Word.T;
                              attValue    : TEXT;
                              truncate    : BOOLEAN          ) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The scheme can be received from the schemelist. *)
      IF GetOpenScheme(schemeNumber, "AGPutMetaAttribute", scheme) THEN
        (* If truncate is TRUE the attribute value is truncated to
           attLength *)
        IF truncate THEN attValue := Text.Sub(attValue, 0, attLength); END;
        scheme.putMetaAttribute(nType, attNo, attBegin, attValue);
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGPutMetaAttribute",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGPutMetaAttribute", "Scheme.NotDeclared");
    END;
  END AGPutMetaAttribute;

PROCEDURE AGGetMetaAttribute (    graphOrSchemeNumber: GraphNumber;
                                  nType              : TypeNumber;
                                  attNo              : AttributeNumber;
                                  attBegin           : Word.T;
                                  attLength          : Word.T;
                              VAR attValue           : TEXT;
                              VAR returnedLength     : Word.T;
                              isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The scheme can be received from the schemelist or the graph. *)
      IF GetScheme(graphOrSchemeNumber, isGraphNumber,
                   "AGGetMetaAttribute", scheme) THEN
        (* Getting the attribute value from the scheme. *)
        attValue :=
          scheme.getMetaAttribute(nType, attNo, attBegin, attLength);
        (* Calculating the length of the attribute. *)
        IF attValue # NIL THEN
          returnedLength := Text.Length(attValue);
        ELSE
          returnedLength := 0;
        END;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGGetMetaAttribute",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGGetMetaAttribute", "Scheme.NotDeclared");
    END;
  END AGGetMetaAttribute;

PROCEDURE AGSetEvaluationFunction (schemeNumber   : SchemeNumber;
                                   nodeClassOrType: TEXT;
                                   attribute      : TEXT;
                                   name           : TEXT          ) =
  VAR
    scheme              : Scheme.T;
    cot, attributeNumber: TypeNumber;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist. *)
      IF GetOpenScheme(schemeNumber, "AGSetEvaluationFunction", scheme) THEN
        (* Changing the class or type name into its according number. *)
        cot := scheme.getTypeNumber(nodeClassOrType);
        (* Getting the attribute number from the scheme *)
        attributeNumber := scheme.getAttributeNumber(cot, attribute);
        (* Now setting the evaluation function. *)
        scheme.setEvaluationFunction(cot, attributeNumber, name);
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGSetEvaluationFunction",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGSetEvaluationFunction", "Scheme.NotDeclared");
    END;
  END AGSetEvaluationFunction;

PROCEDURE AGShowEvaluationFunction (graphOrSchemeNumber: GraphNumber;
                                    attNo              : AttributeNumber;
                                    nodeClassOrType    : TypeNumber;
                                    VAR name         : TEXT;
                                    VAR existent     : BOOLEAN;
                                        isGraphNumber: BOOLEAN   := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The scheme can be received from the schemelist or the graph. *)
      IF GetScheme(graphOrSchemeNumber, isGraphNumber,
                   "AGShowEvaluationFunction", scheme) THEN
        (* Reading the name of the evaluationfuncion. *)
        name := scheme.getEvaluationFunction(nodeClassOrType, attNo);
        (* If a name was returned, an evaluation function exists. *)
        existent := name # NIL;
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGShowEvaluationFunction",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowEvaluationFunction", "Scheme.NotDeclared");
    END;
  END AGShowEvaluationFunction;

PROCEDURE AGBindEvaluationFunction (graphNumber: GraphNumber;
                                    name       : TEXT;
                                    function   : EvalFunction ) =
  VAR
    graph    : Graph.T;
    evaluator: Graph.Evaluator;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGBindEvaluationFunction", graph) THEN
        (* Creating a new evaluator, which internally calls the given
           evaluation function. *)
        evaluator :=
          NEW(RGRASEvaluator, RGRASEvaluationFunction := function);
        (* Now binding the evaluation function. *)
        graph.bindEvaluator(name, evaluator);
      END;
    EXCEPT
    | Graph.AlreadyBound =>
        ErrorMessage("AGBindEvaluationFunction", "Graph.AlreadyBound");
    END;
  END AGBindEvaluationFunction;

PROCEDURE AGDeclareDependency (schemeNumber    : SchemeNumber;
                               nodeClassOrType : TEXT;
                               dependent       : TEXT;
                               dependsOn       : TEXT;
                               nameOfEdge      : TEXT;
                               kindOfDependency: DependencyKind) =
  VAR
    scheme                                               : Scheme.T;
    dependentAttribute, dependsOnAttribute, cot, edgeType: TypeNumber;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist *)
      IF GetOpenScheme(schemeNumber, "AGDeclareDependency", scheme) THEN
        (* Changing the class or type name into a number. *)
        cot := scheme.getTypeNumber(nodeClassOrType);
        (* Changing the edgetype name into a number. *)
        IF kindOfDependency # DependencyKind.SelfDependent THEN
          edgeType := scheme.getTypeNumber(nameOfEdge);
        END;
        (* Changing the dependent and depends on attribute name into the
           according numbers. *)
        dependentAttribute := scheme.getAttributeNumber(cot, dependent);
        dependsOnAttribute := scheme.getAttributeNumber(cot, dependsOn);
        (* Now declaring the dependency *)
        scheme.declareDependency(
          cot, dependentAttribute, dependsOnAttribute,
          ConvertToNewDependencyKind(kindOfDependency), edgeType);
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGDeclareDependency",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGDeclareDependency", "Scheme.NotDeclared");
    END;
  END AGDeclareDependency;

PROCEDURE AGDeleteDependencies (graphOrSchemeNumber: GraphNumber;
                                nodeClassOrType    : TEXT;
                                dependent          : TEXT;
                                <*UNUSED*> restoreInheritance: BOOLEAN;
                                isGraphNumber: BOOLEAN := TRUE) =
  VAR
    scheme         : Scheme.T;
    cot            : TypeNumber;
    attributeNumber: AttributeNumber;
  BEGIN
    TRY
      (* The open scheme can be received from the schemelist *)
      IF GetScheme(graphOrSchemeNumber, isGraphNumber,
                   "AGDeleteDependencies", scheme) THEN
        (* Changing the class or type name into its according number. *)
        cot := scheme.getTypeNumber(nodeClassOrType);
        (* Getting the attribute number from the scheme. *)
        attributeNumber := scheme.getAttributeNumber(cot, dependent);
        (* Now deleting all dependencies. *)
        scheme.deleteDependencies(cot, attributeNumber);
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGDeleteDependency",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGDeleteDependency", "Scheme.NotDeclared");
    END;
  END AGDeleteDependencies;

PROCEDURE AGShowAllAttributesOfNodeType (graphOrSchemeNumber: GraphNumber;
                                         nType              : TypeNumber;
                                         VAR attributes: SimpleSet;
                                         isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The open scheme can be retreived from the schemelist. *)
      IF GetScheme(graphOrSchemeNumber, isGraphNumber,
                   "AGShowAllAttributesOfNodeType", scheme) THEN
        (* Now getting all attributes from the scheme. *)
        attributes := setList.addEntry(
                        scheme.getAllAttributesOfNodeClassOrType(nType));
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGShowAllAttributesOfNodeType",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGShowAllAttributesOfNodeType", "Scheme.NotDeclared");
    END;
  END AGShowAllAttributesOfNodeType;

PROCEDURE AGGetAllNodesOfGraph (    graphNumber: GraphNumber;
                                VAR setOfNodes : SimpleSet    ) =
  VAR
    graph           : Graph.T;
    nodeTypes, nodes: NodeSet.T;
    singleNodeType  : TypeNumber;
    ok              : BOOLEAN;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist *)
      IF GetOpenGraph(graphNumber, "AGGetAllNodesOfGraph", graph) THEN
        (* Getting a set of all node types declared. *)
        nodeTypes := graph.getScheme().getAllNodeTypes();
        nodes := NodeSet.New();
        nodeTypes.loop();
        (* Running through the set, and collecting all nodes of each single
           nodetype in the result set. *)
        singleNodeType := nodeTypes.get(ok);
        WHILE ok DO
          WITH nodesOfType = graph.getAllNodesOfType(singleNodeType) DO
            nodes.union(nodesOfType);
            nodesOfType.dispose();
          END;
          singleNodeType := nodeTypes.get(ok);
        END;
        nodeTypes.dispose();
        setOfNodes := setList.addEntry(nodes);
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGGetAllNodesOfGraph",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Access.Locked =>
        ErrorMessage("AGGetAllNodesOfGraph", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "GAGGetAllNodesOfGraph",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGGetAllNodesOfGraph;

PROCEDURE AGGetAllNodeclasses (    graphOrSchemeNumber: GraphNumber;
                               VAR setOfClasses       : SimpleSet;
                               isGraphNumber: BOOLEAN := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The open scheme can be retrieverd from the schemelist. *)
      IF GetScheme(graphOrSchemeNumber, isGraphNumber,
                   "AGGetAllNodeclasses", scheme) THEN
        (* Getting the list of nodeclasses from the scheme. *)
        setOfClasses := setList.addEntry(scheme.getAllClasses());
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGGetAllNodeclasses",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGGetAllNodeclasses;

PROCEDURE AGGetAllEdgetypes (    graphOrSchemeNumber: GraphNumber;
                             VAR setOfEdgetypes     : SimpleSet;
                                 isGraphNumber      : BOOLEAN       := TRUE) =
  VAR scheme: Scheme.T;
  BEGIN
    TRY
      (* The open scheme can be retrieverd from the schemelist. *)
      IF GetScheme(
           graphOrSchemeNumber, isGraphNumber, "AGGetAllEdgetypes", scheme) THEN
        (* Getting all edgetypes from the scheme. *)
        setOfEdgetypes := setList.addEntry(scheme.getAllEdgeTypes());
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGGetAllEdgetypes",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGGetAllEdgetypes;

PROCEDURE AGGetAllNodesOfNodeClass (    graphNumber: GraphNumber;
                                        nodeClass  : TypeNumber;
                                    VAR setOfNodes : SimpleSet    ) =
  VAR
    allSubClasses, nodes: NodeSet.T;
    scheme              : Scheme.T;
    graph               : Graph.T;
    ok                  : BOOLEAN;
    singleType          : TypeNumber;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGGetAllNOdesOfNodeClass", graph) THEN
        (* getting a handle to the according scheme. *)
        scheme := graph.getScheme();
        nodes := NodeSet.New();
        (* Getting a set of all subtypes of the class. *)
        allSubClasses := scheme.getAllSubClasses(nodeClass);
        allSubClasses.loop();
        singleType := allSubClasses.get(ok);
        WHILE ok DO
          (* While cycling through the set of all subtypes, all nodes of
             the current subtype are collected in the result set.*)
          nodes.union(graph.getAllNodesOfType(singleType));
          singleType := allSubClasses.get(ok);
        END;
        setOfNodes := setList.addEntry(nodes);
      END;
    EXCEPT
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGGetAllNodesOfNodeClass",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGGetAllNodesOfNodeClass", "Scheme.NotDeclared");
    | Access.Locked =>
        ErrorMessage("AGGetAllNodesOfNodeClass", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGGetAllNodesOfNodeClass",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGGetAllNodesOfNodeClass;

PROCEDURE AGGetAllNodesOfNodeType (    graphNumber: GraphNumber;
                                       nType      : TypeNumber;
                                   VAR setOfNodes : SimpleSet    ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGGetAllNodesOfNodeType", graph) THEN
        (* Getting the nodes of the appropriate type from the graph. *)
        setOfNodes := setList.addEntry(graph.getAllNodesOfType(nType));
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGGetAllNodesOfNodeType", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGGetAllNodesOfNodeType",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGGetAllNodesOfNodeType;

PROCEDURE AGGetComplementOfNodeSet (    graphNumber: GraphNumber;
                                    VAR setOfNodes : SimpleSet    ) =
  <* FATAL SetList.EntryNotInList *>
  VAR allNodes, tempSet: SimpleSet;
  BEGIN
    IF setList.isEntry(setOfNodes) THEN
      (* Getting the complement of a node set, by using the difference
         method on a set of all defined nodes with the current set of
         nodes. *)
      AGGetAllNodesOfGraph(graphNumber, allNodes);
      IF setList.isEntry(allNodes) THEN
        setList.getEntry(allNodes).handle.difference(
          setList.getEntry(setOfNodes).handle);
        tempSet := setOfNodes;
        setOfNodes := allNodes;
        KillSetEntry(tempSet);
      END;
    ELSE
      ErrorMessage(
        "AGGetComplementOfNodeSet", "The set has not been created!");
    END;
  END AGGetComplementOfNodeSet;

PROCEDURE AGTestIsNodeOfNodeClass (graphNumber: GraphNumber;
                                   nodeClass  : TypeNumber;
                                   node       : TypeNumber   ): BOOLEAN =
  VAR
    graph : Graph.T;
    scheme: Scheme.T;
    result: BOOLEAN  := FALSE;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGTestIsNodeOfNodeClass", graph) THEN
        (* Now getting the scheme of the graph with the getscheme()
           method. *)
        scheme := graph.getScheme();
        (* Testing whether the node is of the given class by testing if its
           type is a subtype of the given class *)
        result :=
          scheme.isSubClassOrType(graph.getNodeType(node), nodeClass);
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGTestIsNodeOfNodeClass", "Access.Locked");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGTestIsNodeOfNodeClass",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGTestIsNodeOfNodeClass", "Graph.NodeNotFound");
    | Graph.NotOwner =>
        ErrorMessage("AGTestIsNodeOfNodeClass", "Graph.NotOwner");
    | Graph.Unknown =>
        ErrorMessage("AGTestIsNodeOfNodeClass", "Graph.Unknown");
    | Scheme.InternalError (m) =>
        ErrorMessage(
          "AGTestIsNodeOfNodeClass",
          "Scheme.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Scheme.NotDeclared =>
        ErrorMessage("AGTestisNodeOfNodeClass", "Scheme.NotDeclared");
    END;
    RETURN result;
  END AGTestIsNodeOfNodeClass;

(******************************************************************************)
(**                                                                           *)
(** PROCEDUREs for setting the error check mode                               *)
(**                                                                           *)
(******************************************************************************)

PROCEDURE AGSetErrorCheckMode (graphNumber : GraphNumber;
                               errorcheckOn: BOOLEAN      ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGSetErrorCheckMode", graph) THEN
        (* Depending on the parameter 'errorcheckOn' the errorchecks are
           set to the default, or to no errorchecks. *)
        IF errorcheckOn THEN
          graph.setErrorChecks(defaultGraphErrorChecks);
        ELSE
          graph.setErrorChecks(Graph.NoChecks);
        END;
      END;
    EXCEPT
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGSetErrorCheckMode",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    END;
  END AGSetErrorCheckMode;

PROCEDURE AGFileSize (poolName: PoolName; fileName: TEXT): CARDINAL =
  VAR
    pool  : GraphPool.T;
    result: CARDINAL    := 0;
  BEGIN
    TRY
      IF GetOpenPool(poolName, "AGFileSize", pool) THEN
        result := pool.fileSize(fileName, local := FALSE);
      END;
    EXCEPT
    | PageFile.NoAccess (m) =>
        ErrorMessage("AGFileSize", "PageFile.NoAccess : " & m);
    | VirtualResource.FatalError (m) =>
        ErrorMessage(
          "AGFileSize",
          "VirtualResource.FatalError : " & ErrorSupport.ToText(m),
          halt := TRUE);
    END;
    RETURN result;
  END AGFileSize;


(***********************************************************************)
(**                                                                    *)
(** Here starts the 'hack section'! The PROCEDUREs below are to be     *)
(** handled with care because they may lead to inconsistent states     *)
(** (if not carefully used) which may result in loss of data!          *)
(**                                                                    *)
(** The reasons for still offering these PROCEDUREs are twofold:       *)
(**    (1) Compatibility with older GRAS versions                      *)
(**    (2) Efficient usage of GRAS resources in the absence of a graph *)
(**        scheme                                                      *)
(**                                                                    *)
(**                               *****                                *)
(**                               *   *                                *)
(**                               *   *                                *)
(**                               *   *                                *)
(**                               *   *                                *)
(**                            ****   ****                             *)
(**                             *       *                              *)
(**                              *     *                               *)
(**                               *   *                                *)
(**                                * *                                 *)
(**                                 *                                  *)
(**                                                                    *)
(***********************************************************************)

PROCEDURE AGPutIndexAttribute (graphNumber: GraphNumber;
                               node       : NodeNumber;
                               attNo      : AttributeNumber;
                               attLength  : Word.T;
                               attValue   : TEXT             ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGPutIndexAttribute", graph) THEN
        (* Truncating the attribute value to the desired length. *)
        attValue := Text.Sub(attValue, 0, attLength);
        (* Now writing the index, using the entity value of the attribute
           number as the old format of the attribute number. *)
        graph.putIndex(node, attNo.entity, attValue);
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGPutIndexAttribute", "Access.Locked");
    | ChgMgmtGraph.InternalError (m) =>
        ErrorMessage("AGPutIndexAttribute", "ChgMgmtGraph.InternalError : "
                                              & ErrorSupport.ToText(m),
                     halt := TRUE);
    | PersistentGraph.NodeNotFound =>
        ErrorMessage("AGPutIndexAttribute", "PersistentGraph.NodeNotFound");
    | ChgMgmtGraph.LogError (m) =>
        ErrorMessage("AGPutIndexAttribute",
                     "ChgMgmtGraph.LogError : " & ErrorSupport.ToText(m));
    | PersistentGraph.IndexUsed =>
        ErrorMessage("AGPutIndexAttribute", "PersistentGraph.IndexUsed");
    | PersistentGraph.NotOwner =>
        ErrorMessage("AGPutIndexAttribute", "PersistentGraph.NotOwner");
    END;
  END AGPutIndexAttribute;

PROCEDURE AGGetIndexAttribute (    graphNumber: GraphNumber;
                                   node       : NodeNumber;
                                   attNo      : AttributeNumber;
                               VAR attValue   : TEXT;
                               VAR attLength  : Word.T           ) =
  VAR
    graph: Graph.T;
    found: BOOLEAN;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGGetIndexAttribute", graph) THEN
        (* Now reading the index, using the entity value of the attribute
           number as the old format of the attribute number. *)
        attValue := graph.getIndex(node, attNo.entity, found);
        (* Truncating the attribute value to the desired length. *)
        attLength := Text.Length(attValue);
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGGetIndexAttribute", "Access.Locked");
    | PersistentGraph.InternalError (m) =>
        ErrorMessage("AGGetIndexAttribute",
                     "PersistentGraph.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | PersistentGraph.NodeNotFound =>
        ErrorMessage("AGGetIndexAttribute", "PersistentGraph.NodeNotFound");
    | PersistentGraph.NotOwner =>
        ErrorMessage("AGGetIndexAttribute", "PersistentGraph.NotOwner");
    END;
  END AGGetIndexAttribute;

PROCEDURE AGDeleteIndexAttribute (graphNumber: GraphNumber;
                                  node       : NodeNumber;
                                  attNo      : AttributeNumber) =
  VAR
    graph   : Graph.T;
    attValue: TEXT;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGDeleteIndexAttribute", graph) THEN
        (* Reading the old attribute value. *)
        attValue :=
          graph.getAttribute(node, attNo, 0, MaxIndexAttributeLength);
        (* Now deleting the index, using the entity value of the attribute
           number as the old format of the attribute number. *)
        graph.deleteIndex(node, attNo.entity, attValue);
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGDeleteIndexAttribute", "Access.Locked");
    | ChgMgmtGraph.InternalError (m) =>
        ErrorMessage("AGDeleteIndexAttribute",
                     "ChgMgmtGraph.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | ChgMgmtGraph.LogError (m) =>
        ErrorMessage("AGDeleteIndexAttribute",
                     "ChgMgmtGraph.LogError : " & ErrorSupport.ToText(m));
    | PersistentGraph.IndexUnused =>
        ErrorMessage(
          "AGDeleteIndexAttribute", "PersistentGraph.IndexUnused");
    | PersistentGraph.NodeNotFound =>
        ErrorMessage(
          "AGDeleteIndexAttribute", "PersistentGraph.NodeNotFound");
    | PersistentGraph.NotOwner =>
        ErrorMessage("AGDeleteIndexAttribute", "PersistentGraph.NotOwner");
    | Graph.CyclicEvaluation =>
        ErrorMessage("AGDeleteIndexAttribute", "Graph.CyclicEvaluation");
    | Graph.InternalError (m) =>
        ErrorMessage(
          "AGDeleteIndexAttribute",
          "Graph.InternalError : " & ErrorSupport.ToText(m), halt := TRUE);
    | Graph.NodeNotFound =>
        ErrorMessage("AGDeleteIndexAttribute", "Graph.NodeNotFound");
    | Graph.LogError (m) =>
        ErrorMessage("AGDeleteIndexAttribute",
                     "Graph.LogError : " & ErrorSupport.ToText(m));
    | Graph.Unknown =>
        ErrorMessage("AGDeleteIndexAttribute", "Graph.Unknown");
    | Graph.WrongType =>
        ErrorMessage("AGDeleteIndexAttribute", "Graph.WrongType");
    | Graph.NotOwner =>
        ErrorMessage("AGDeleteIndexAttribute", "Graph.NotOwner");
    END;
  END AGDeleteIndexAttribute;

(*
PROCEDURE AGGetAllNodesWithUndefinedIndex (graphNumber: GraphNumber;
                                           attNo      : AttributeNumber;
                                           VAR setOfNodes: SimpleSet) =
  VAR graph: Graph.T;
  BEGIN
    TRY
    EXCEPT
    END;
  END AGGetAllNodesWithUndefinedIndex;
*)

PROCEDURE AGGetAllIndexAttributes (    graphNumber: GraphNumber;
                                       node       : NodeNumber;
                                   VAR indexSet   : SimpleSet    ) =
  VAR
    graph: Graph.T;
    found: BOOLEAN;
    set  : CardSet.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGGetAllIndexAttributes", graph) THEN
        (* Reading the set from the graph, and converting it into a
           nodeset. *)
        set := graph.getIndexNosForNode(node);
        (* The index attribute with the number 0 contains the type
           information of the node, and is for intern use only, so it is
           removed from the set *)
        IF set # NIL THEN set.deleteElement(0, found) END;
        indexSet := ConvertCardSetToNodeSet(
                      set, graphNumber, "AGGetAllIndexAttributes");
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGGetAllIndexAttributes", "Access.Locked");
    | PersistentGraph.InternalError (m) =>
        ErrorMessage("AGGetAllIndexAttributes",
                     "PersistentGraph.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | PersistentGraph.NodeNotFound =>
        ErrorMessage(
          "AGGetAllIndexAttributes", "PersistentGraph.NodeNotFound");
    | PersistentGraph.NotOwner =>
        ErrorMessage("AGGetAllIndexAttributes", "PersisentGraph.NotOwner");
    END;
  END AGGetAllIndexAttributes;

PROCEDURE AGGetAllDefinedAttributes (    graphNumber: GraphNumber;
                                         node       : NodeNumber;
                                     VAR attSet     : SimpleSet    ) =
  VAR graph: Graph.T;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGGetAllDefinedAttributes", graph) THEN
        (* Reading the set from the graph, and converting it into a
           nodeset. *)
        attSet := ConvertCardSetToNodeSet(
                    graph.getAllAttributeNumbers(node), graphNumber,
                    "AGGetAllDefinedAttributes");
      END;
    EXCEPT
    | Access.Locked =>
        ErrorMessage("AGGetAllDefinedAttributes", "Access.Locked");
    | PersistentGraph.InternalError (m) =>
        ErrorMessage("AGGetAllDefinedAttributes",
                     "PersistentGraph.InternalError : "
                       & ErrorSupport.ToText(m), halt := TRUE);
    | PersistentGraph.NodeNotFound =>
        ErrorMessage(
          "AGGetAllDefinedAttributes", "PersistentGraph.NodeNotFound");
    | PersistentGraph.NotOwner =>
        ErrorMessage(
          "AGGetAllDefinedAttributes", "PersistentGraph.NotOwner");
    END;
  END AGGetAllDefinedAttributes;

PROCEDURE AGChangeNodeType (graphNumber: GraphNumber;
                            node       : NodeNumber;
                            newType    : TypeNumber   ) =
  VAR
    graph: Graph.T;
    text : TEXT;
    found: BOOLEAN;
  BEGIN
    TRY
      (* The open graph can be received from the graphlist. *)
      IF GetOpenGraph(graphNumber, "AGChangeNodeType", graph) THEN
        (* Now setting the new type by setting the index attribute number
           0. *)
        (* Determining the old index value. *)
        text := ChgMgmtGraph.T.getIndex(graph, node, 0, found);
        IF found THEN
          (* Deleting the old index value. *)
          ChgMgmtGraph.T.deleteIndex(graph, node, 0, text);
          (* Calculating the new value for index attribute 0. *)
          text := AttributeValue.CardToText(newType.graph)
                    & AttributeValue.CardToText(newType.entity);
          (* Setting the new value. *)
          ChgMgmtGraph.T.putIndex(graph, node, 0, text);
        END;
      END;
    EXCEPT
    | Access.Locked => ErrorMessage("AGChangeNodeType", "Access.Locked");
    | ChgMgmtGraph.InternalError (m) =>
        ErrorMessage("AGChangeNodeType", "ChgMgmtGraph.InternalError : "
                                           & ErrorSupport.ToText(m),
                     halt := TRUE);
    | ChgMgmtGraph.LogError (m) =>
        ErrorMessage("AGChangeNodeType",
                     "ChgMgmtGraph.LogError : " & ErrorSupport.ToText(m));
    | PersistentGraph.NodeNotFound =>
        ErrorMessage("AGChangeNodeType", "PersistentGraph.NodeNotFound");
    | PersistentGraph.NotOwner =>
        ErrorMessage("AGChangeNodeType", "PersistentGraph.NotOwner");
    | PersistentGraph.IndexUsed =>
        ErrorMessage("AGChangeNodeType", "PersistentGraph.IndexUsed");
    | PersistentGraph.InternalError (m) =>
        ErrorMessage("AGChangeNodeType", "PersistentGraph.InternalError : "
                                           & ErrorSupport.ToText(m),
                     halt := TRUE);
    | PersistentGraph.IndexUnused =>
        ErrorMessage("AGChangeNodeType", "PersistentGraph.IndexUnused");
    END;
  END AGChangeNodeType;

CONST
  defaultPoolAccessMode  = Access.Mode.ReadWriteShared;
  defaultGraphAccessMode = Graph.AccessMode.ReadWriteShared;
  defaultGraphErrorChecks = Graph.ErrorCheckSet{
                              Graph.ErrorCheck.Node, Graph.ErrorCheck.Edge,
                              Graph.ErrorCheck.Type,
                              Graph.ErrorCheck.Attribute};
  userDefinedEventTypeName = "UserDefinedEventType";

TYPE GraphNumberArray = REF ARRAY OF ExternNumber;

VAR
  poolList        : PoolList.T;
  graphList       : GraphList.T;
  schemeList      : SchemeList.T;
  setList         : SetList.T;
  relSetList      : RelSetList.T;
  daemonList      : DaemonList.T;
  rootPath        : TEXT             := Env.Get("GRAS3");
  graphNumberArray: GraphNumberArray;
  alreadyLogged: BOOLEAN := FALSE; (* to prevent a more than one call of
                                      the login procedure *)
BEGIN
  poolList := NEW(PoolList.T).init();
  graphList := NEW(GraphList.T).init();
  schemeList := NEW(SchemeList.T).init();
  setList := NEW(SetList.T).init();
  relSetList := NEW(RelSetList.T).init();
  daemonList := NEW(DaemonList.T).init();
  DefineUserEvent();
END RGRASGraph.
