MODULE ChgMgmtNames;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.6  1998/05/19 10:17:32  roland
    Support for log-groups implemented.

    Revision 1.5  1998/03/17 14:13:56  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.4  1997/10/17 16:45:04  renehuel
    bugfix in deltaCopyGraph.

    Revision 1.3  1997/07/21 10:37:28  roland
    Adaptions to new set-implementation (no more SetExceptions). Now use free
    memory management of deltas and sets.

    Revision 1.2  1997/05/01 13:21:06  roland
    Changed raises sets of transaction procedures. Pretty printing.

    Revision 1.1  1997/04/23 14:09:41  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

    Revision 1.2  1997/01/20 08:57:42  roland
    Reconstruction procedures implemented.

    Revision 1.1  1996/12/20 17:30:53  roland
    First version of ChgMgmtGraphSystem. Difference to
    PersistentGraphSystem: Graphs might be connected via deltas
    (DeltaCopyGraph) to allow efficient versioning.

*)
(***************************************************************************)

IMPORT Pathname, TextCursorSet, PersistentGraphPool;
IMPORT Names, ErrorSupport, Access;
IMPORT PersistentNames AS Super;
IMPORT Text;

CONST
  (* Collections *)
  DeltaCollName = "Deltas";

  VisibleCollName   = "VisibleGraphs";
  InvisibleCollName = "InvisibleGraphs";
  DirectCollName    = "DirectGraphs";
  IndirectCollName  = "IndirectGraphs";

  LogGroupCollName = "LogGroups";

  (* Relations *)
  ToDeltaName   = "GraphToDelta";
  FromDeltaName = "DeltaToGraph";
  OriginalName  = "Original";
  MinDeltaName  = "MinDelta";

  ToGroupMemberName = "LogGroupMember";


  (* Attributes *)
  CostsName = "Costs";           (* Delta: length, Graph: reconstruction
                                    costs *)
  LogModeName     = "LogMode";
  WorkingNameName = "WorkingName";
  VersionName = "Version";       (* A counter to detect check-in
                                    conflicts *)


REVEAL
  T = Public BRANDED OBJECT
        (* These variables hold the ids of collections, attributes, and
           relations *)
        deltas, visibleGraphs, invisibleGraphs, directGraphs,
          indirectGraphs, logGroups: CARDINAL;
        toDelta, fromDelta, original, minDelta, toGroupMember: CARDINAL;
        costs, logModeAttr, version, workingNameAttr         : CARDINAL;
      OVERRIDES
        login       := Login;
        insertGraph := InsertGraph;
        removeGraph := RemoveGraph;
        renameGraph := RenameGraph;
        removeDelta := RemoveDelta;
        mkInvisible := MkInvisible;
        mkIndirect  := MkIndirect;
        uniqueName  := UniqueName;
        insertDelta := InsertDelta;

        insertLogGroup      := InsertLogGroup;
        removeLogGroup      := RemoveLogGroup;
        existsLogGroup      := ExistsLogGroup;
        addToLogGroup       := AddToLogGroup;
        delFromLogGroup     := DelFromLogGroup;
        getLogGroupMembers := GetLogGroupMembers;
        getLogGroup         := GetLogGroup;

        copy                         := Copy;
        checkOut                     := CheckOut;
        addDeltaCosts                := AddDeltaCosts;
        setLogMode                   := SetLogMode;
        getLogMode                   := GetLogMode;
        setWorkingName               := SetWorkingName;
        getWorkingName               := GetWorkingName;
        existsGraph                  := ExistsGraph;
        isDirect                     := IsDirect;
        isVisible                    := IsVisible;
        existsDelta                  := ExistsDelta;
        existsDeltaName              := ExistsDeltaName;
        isCheckoutCopy               := IsCheckoutCopy;
        isCheckoutOriginal           := IsCheckoutOriginal;
        getOriginal                  := GetOriginal;
        getAllCheckedOutCopies       := GetAllCheckedOutCopies;
        getCheckoutDeltas            := GetCheckoutDeltas;
        getSourceGraphs              := GetSourceGraphs;
        getDeltaName                 := GetDeltaName;
        getDeltaSource               := GetDeltaSource;
        getDeltaTarget               := GetDeltaTarget;
        getMinDelta                  := GetMinDelta;
        getOutDeltas                 := GetOutDeltas;
        getInDeltas                  := GetInDeltas;
        getGraphs                    := GetGraphs;
        getDeltas                    := GetDeltas;
        invisibleGraphsWithoutTarget := InvisibleGraphsWithoutTarget;
        invisibleDirectWithOneTarget := InvisibleDirectWithOneTarget;
      END;

CONST
  (* Constant attribute values *)
  NullText = "\000\000\000\000";

PROCEDURE TextToInt (t: TEXT): INTEGER =
  VAR
    b: ARRAY [0 .. 3] OF CHAR;
    i: INTEGER                := 0;
  BEGIN
    Text.SetChars(b, Text.Sub(t, 0, 4));
    i := ORD(b[3]);
    FOR k := 2 TO 0 BY -1 DO i := i * 256 + ORD(b[k]) END;
    RETURN i;
  END TextToInt;

PROCEDURE IntToText (x: INTEGER): TEXT =
  VAR b: ARRAY [0 .. 3] OF CHAR;
  BEGIN
    FOR k := 0 TO 2 DO b[k] := VAL(x MOD 256, CHAR); x := x DIV 256; END;
    b[3] := VAL(x, CHAR);
    RETURN Text.FromChars(b);
  END IntToText;

PROCEDURE CreateDelta (names: T; delta: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, Names.InternalError} =
  <* FATAL Names.Unknown *>
  BEGIN
    names.insert(delta, local);
    names.insert(delta, local, collection := names.deltas);
    names.setAttribute(delta, local, names.costs, NullText);
  END CreateDelta;

PROCEDURE CreateLogGroup (names: T; group: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, Names.InternalError} =
  BEGIN
    names.insert(group, local);
    names.insert(group, local, collection := names.logGroups);
  END CreateLogGroup;

PROCEDURE CreateGraph (names: T; graph: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  <* FATAL Names.Unknown *>
  BEGIN
    TRY
      Super.T.insertGraph(names, graph, local);
      names.insert(graph, local, collection := names.visibleGraphs);
      names.insert(graph, local, collection := names.directGraphs);
      names.setAttribute(graph, local, names.costs, NullText);
      names.setAttribute(graph, local, names.logModeAttr, NullText);
      names.setAttribute(graph, local, names.version, NullText);
    EXCEPT
      Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.CreateGraph", "Names.InternalError", info));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.CreateGraph",
                              "PersistentNames.InternalError", info));
    END;
  END CreateGraph;

PROCEDURE SetCosts (names: T;
                    name : Pathname.T;
                    local: BOOLEAN;
                    costs: CARDINAL    )
  RAISES {Access.Locked, Names.InternalError, Names.Unknown} =
  BEGIN
    names.setAttribute(name, local, names.costs, IntToText(costs));
  END SetCosts;

PROCEDURE GetCosts (names: T; name: Pathname.T; local: BOOLEAN): CARDINAL
  RAISES {Access.Locked, Names.InternalError, Names.Unknown} =
  BEGIN
    RETURN TextToInt(names.getAttribute(name, local, names.costs));
  END GetCosts;


(* Everything we use here will be declared at Login.  If the application
   does not login, every call to Names will fail anyway. *)
<* FATAL Names.Undeclared *>

PROCEDURE Login (names         : T;
                 pool          : PersistentGraphPool.T;
                 collectionname: TEXT                   )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      Super.T.login(names, pool, collectionname);
      (* Get Ids for collections and relations *)
      names.deltas := names.declareCollection(DeltaCollName);
      names.visibleGraphs := names.declareCollection(VisibleCollName);
      names.invisibleGraphs := names.declareCollection(InvisibleCollName);
      names.directGraphs := names.declareCollection(DirectCollName);
      names.indirectGraphs := names.declareCollection(IndirectCollName);
      names.logGroups := names.declareCollection(LogGroupCollName);

      names.toDelta := names.declareRelation(ToDeltaName);
      names.fromDelta := names.declareRelation(FromDeltaName);
      names.original := names.declareRelation(OriginalName);
      names.minDelta := names.declareRelation(MinDeltaName);
      names.toGroupMember := names.declareRelation(ToGroupMemberName);

      names.costs := names.declareAttribute(CostsName);
      names.logModeAttr := names.declareAttribute(LogModeName);
      names.workingNameAttr := names.declareAttribute(WorkingNameName);
      names.version := names.declareAttribute(VersionName);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.Login", "Names.InternalError", info));
    END;
  END Login;


(* --- Building administration information --- *)

PROCEDURE InsertGraph (names: T; name: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    CreateGraph(names, name, local);
  END InsertGraph;

PROCEDURE RemoveGraph (    names  : T;
                           name   : Pathname.T;
                           local  : BOOLEAN;
                       VAR extRels: TextCursorSet.T)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      Super.T.removeGraph(names, name, local, extRels);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.RemoveGraph",
                              "PersistentNames.InternalError", info));
    END;
  END RemoveGraph;

PROCEDURE RemoveDelta (names: T; name: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      Super.T.remove(names, name, local);
    EXCEPT
      Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.RemoveDelta", "Names.InternalError", info));
    END;
  END RemoveDelta;

PROCEDURE RenameGraph (names: T;
                       old  : Pathname.T;
                       new  : Pathname.T;
                       local: BOOLEAN     )
  RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      Super.T.change(names, local, old, new);
    EXCEPT
      Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.RenameGraph", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END RenameGraph;

PROCEDURE MkVisible (names: T; name: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      names.remove(name, local, collection := names.invisibleGraphs);
      names.insert(name, local, collection := names.visibleGraphs);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.MkVisible", "Names.InternalError", info));
    END;
  END MkVisible;

(**
  -------------------------------------------------------------------------------
  --- (mk)
  --- this procedure is disabled because it't not used yet...
  -------------------------------------------------------------------------------

PROCEDURE MkDirect ( names : T;
                     name  : Pathname.T;
                     local : BOOLEAN     )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      names.remove(name, local, collection := names.indirectGraphs);
      names.insert(name, local, collection := names.directGraphs);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.MkDirect", "Names.InternalError", info));
    END;
  END MkDirect;
*)

PROCEDURE MkInvisible (names: T; name: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      names.remove(name, local, collection := names.visibleGraphs);
      names.insert(name, local, collection := names.invisibleGraphs);
    EXCEPT
      Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.MkInvisible", "Names.InternalError", info));
    END;
  END MkInvisible;

PROCEDURE MkIndirect (names: T; name: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      names.remove(name, local, collection := names.directGraphs);
      names.insert(name, local, collection := names.indirectGraphs);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.MkIndirect", "Names.InternalError", info));
    END;
  END MkIndirect;

PROCEDURE UniqueName (names: T): TEXT
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN Super.T.uniqueName(names);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.MkIndirect",
                              "PersistentNames.InternalError", info));
    END;
  END UniqueName;

PROCEDURE InsertDelta (names : T;
                       source: Pathname.T;
                       target: Pathname.T;
                       delta : Pathname.T;
                       local : BOOLEAN     )
  RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      (* make target visible indirect *)
      MkVisible(names, target, local);
      MkIndirect(names, target, local);
      CreateDelta(names, delta, local);
      (* Connect source and target via delta *)
      names.insertInRelation(source, delta, local, names.toDelta);
      names.insertInRelation(delta, target, local, names.fromDelta);
      (* The new delta is the one with minimal reconstruction costs *)
      names.insertInRelation(target, delta, local, names.minDelta);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.DeltaCopy", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END InsertDelta;

(** PROCEDURE BackwardCopy (names : T; *)
(**                         source: Pathname.T; *)
(**                         target: Pathname.T; *)
(**                         delta : Pathname.T  ) *)
(**   RAISES {Access.Locked, InternalError, Unknown} = *)
(**   BEGIN *)
(**     TRY *)
(**       (* Insert target as visible direct *) *)
(**       CreateGraph(names, target); *)
(**       MkVisible(names, target); *)
(**       MkDirect(names, target); *)
(**       (* Remove source from direct and insert in indirect *) *)
(**       MkIndirect(names, source); *)
(**       CreateDelta(names, delta); *)
(**       (* Connect source and target via delta *) *)
(**       names.insertInRelation(target, delta, names.toDelta); *)
(**       names.insertInRelation(delta, source, names.fromDelta); *)
(**       (* The new delta is the one with minimal reconstruction costs *) *)
(**       names.insertInRelation(source, delta, names.minDelta); *)
(**     EXCEPT *)
(**       Names.InternalError (info) => *)
(**         RAISE InternalError(ErrorSupport.Propagate( *)
(**                               "ChgMgmtNames.BackwardCopy", *)
(**                               "Names.InternalError", info)); *)
(**     | Names.Unknown => RAISE Unknown; *)
(**     END; *)
(**   END BackwardCopy; *)

PROCEDURE SetLogMode (names  : T;
                      name   : Pathname.T;
                      local  : BOOLEAN;
                      logMode: INTEGER    )
  RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      names.setAttribute(
        name, local, names.logModeAttr, IntToText(logMode));
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.MarkAsOpen", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END SetLogMode;

PROCEDURE GetLogMode (names: T; name: Pathname.T; local: BOOLEAN): INTEGER
  RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      RETURN TextToInt(names.getAttribute(name, local, names.logModeAttr));
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.IsOpen", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetLogMode;

PROCEDURE SetWorkingName (names   : T;
                          name    : Pathname.T;
                          local   : BOOLEAN;
                          workname: Pathname.T  )
  RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      names.setAttribute(name, local, names.workingNameAttr, workname);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.SetWorkingName",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END SetWorkingName;

PROCEDURE GetWorkingName (names: T; name: Pathname.T; local: BOOLEAN):
  Pathname.T RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      RETURN names.getAttribute(name, local, names.workingNameAttr);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.SetWorkingName",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetWorkingName;

PROCEDURE AddDeltaCosts (names: T;
                         delta: Pathname.T;
                         local: BOOLEAN;
                         costs: CARDINAL    )
  RAISES {Access.Locked, InternalError, Unknown} =
  VAR newcosts: CARDINAL;

  PROCEDURE ReconstructionCosts (names: T;
                                 delta: Pathname.T;
                                 local: BOOLEAN     ): CARDINAL
    RAISES {Access.Locked, Names.InternalError, Names.Unknown,
            InternalError} =
    VAR
      costs : CARDINAL;
      source: Pathname.T;
      found : BOOLEAN;
    BEGIN
      costs := GetCosts(names, delta, local);
      source := names.sources(
                  delta, local, names.toDelta).extractAnyElement(found);
      IF NOT found THEN
        RAISE InternalError(
                ErrorSupport.Create("ChgMgmtNames.ReconstructionCosts",
                                    "Invalid graph structure."));
      END;
      INC(costs, GetCosts(names, source, local));
      RETURN costs;
    END ReconstructionCosts;

  PROCEDURE PropagateCosts (names: T; delta: Pathname.T; local: BOOLEAN)
    RAISES {Access.Locked, Names.InternalError, Names.Unknown,
            InternalError, Unknown} =
    VAR
      oc, mincosts                   : CARDINAL;
      mindelta, graph, ind, outd     : Pathname.T;
      minsources, indeltas, outdeltas: TextCursorSet.T;
      found                          : BOOLEAN;

    BEGIN
      (* If delta is a MinDelta we have to recompute minimal reconstruction
         costs.  Note that checkout deltas never have a MinDelta edge. *)
      minsources := names.sources(delta, local, names.minDelta);
      IF minsources.card() # 0 THEN
        graph := minsources.extractAnyElement(found);
        mincosts := LAST(CARDINAL);
        mindelta := delta;

        (* Find new minimal reconstruction costs *)
        indeltas := names.sources(graph, local, names.fromDelta);
        indeltas.loop();
        ind := indeltas.get(found);
        WHILE found DO
          IF NOT IsCheckoutDelta(names, ind, local) THEN
            oc := ReconstructionCosts(names, ind, local);
            IF oc < mincosts THEN mincosts := oc; mindelta := ind; END;
          END;
          ind := indeltas.get(found);
        END;
        indeltas.dispose();

        (* Reflect new costs *)
        IF NOT Text.Equal(mindelta, delta) THEN
          names.removeFromRelation(graph, delta, local, names.minDelta);
          names.insertInRelation(graph, mindelta, local, names.minDelta);
        END;
        SetCosts(names, graph, local, mincosts);

        (* Propagate new costs along outgoing deltas *)
        outdeltas := names.targets(graph, local, names.toDelta);
        outdeltas.loop();
        outd := outdeltas.get(found);
        WHILE found DO
          PropagateCosts(names, outd, local);
          outd := outdeltas.get(found);
        END;
        outdeltas.dispose();

      END;
    END PropagateCosts;

  BEGIN
    TRY
      newcosts := costs + GetCosts(names, delta, local);
      SetCosts(names, delta, local, newcosts);

      PropagateCosts(names, delta, local);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.AddDeltaCosts",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END AddDeltaCosts;

PROCEDURE InsertLogGroup (names: T; group: Pathname.T; local: BOOLEAN)
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      CreateLogGroup(names, group, local);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.InsertLogGroup",
                                       "Names.InternalError", info));
    END;
  END InsertLogGroup;

PROCEDURE RemoveLogGroup (names: T; group: Pathname.T; local: BOOLEAN)
  RAISES {NotEmpty, Unknown, Access.Locked, InternalError} =
  BEGIN
    TRY
      WITH members = names.targets(group, local, names.toGroupMember) DO
        IF members.card() # 0 THEN
          members.dispose();
          RAISE NotEmpty;
        ELSE
          members.dispose();
          names.remove(group, local);
        END;
      END;
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.RemoveLogGroup",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END RemoveLogGroup;

PROCEDURE ExistsLogGroup (names: T; name: Pathname.T; local: BOOLEAN): BOOLEAN
                      RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN names.contained(name, local, collection := names.logGroups);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.ExistsLogGroup",
                                       "Names.InternalError", info));
    END;
  END ExistsLogGroup;

PROCEDURE AddToLogGroup (names        : T;
                         group, member: Pathname.T;
                         local        : BOOLEAN     )
  RAISES {Unknown, Access.Locked, InternalError} =
  <* FATAL Names.Undeclared *>
  BEGIN
    TRY
      names.insertInRelation(group, member, local, names.toGroupMember);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.AddToLogGroup",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END AddToLogGroup;

PROCEDURE DelFromLogGroup (names        : T;
                           group, member: Pathname.T;
                           local        : BOOLEAN     )
  RAISES {Access.Locked, InternalError} =
  <* FATAL Names.Undeclared *>
  BEGIN
    TRY
      names.removeFromRelation(group, member, local, names.toGroupMember);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.DelFromLogGroup",
                                       "Names.InternalError", info));
    END
  END DelFromLogGroup;

PROCEDURE GetLogGroupMembers (names: T; group: Pathname.T; local: BOOLEAN):
  TextCursorSet.T RAISES {Unknown, Access.Locked, InternalError} =
  <* FATAL Names.Undeclared *>
  BEGIN
    TRY
      RETURN names.targets(group, local, names.toGroupMember);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.GetLogGroupMembers",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetLogGroupMembers;

PROCEDURE GetLogGroup (names: T; name: Pathname.T; local: BOOLEAN):
  Pathname.T RAISES {Unknown, Access.Locked, InternalError} =
  VAR
    groupname: Pathname.T;
    found    : BOOLEAN;
  <* FATAL Names.Undeclared *>
  BEGIN
    TRY
      WITH group = names.sources(name, local, names.toGroupMember) DO
        IF group.card() > 0 THEN
          group.loop();
          groupname := group.get(found);
        ELSE
          groupname := NIL;
        END;
        group.dispose();
      END;
    EXCEPT
      Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.GetLogGroup", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
    RETURN groupname;
  END GetLogGroup;

PROCEDURE Copy (             names : T;
                <* UNUSED *> source: Pathname.T;
                             target: Pathname.T;
                             local : BOOLEAN     )
  RAISES {Access.Locked, InternalError} =
  BEGIN
    (* Insert target as visible direct *)
    CreateGraph(names, target, local);
  END Copy;

PROCEDURE CheckOut (names: T;
                    name : Pathname.T;
                    as   : Pathname.T;
                    otc  : Pathname.T;
                    cto  : Pathname.T;
                    local: BOOLEAN     )
  RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      CreateGraph(names, as, local);
      CreateDelta(names, otc, local);
      CreateDelta(names, cto, local);
      (* Connect original and copy via deltas otc and cto *)
      names.insertInRelation(as, cto, local, names.toDelta);
      names.insertInRelation(cto, name, local, names.fromDelta);
      names.insertInRelation(name, otc, local, names.toDelta);
      names.insertInRelation(otc, as, local, names.fromDelta);
      names.insertInRelation(as, name, local, names.original);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.CheckOut", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END CheckOut;

(* --- Queries --- *)

PROCEDURE ExistsGraph (names: T; name: Pathname.T; local: BOOLEAN): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN Super.T.existsGraph(names, name, local);
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.ExistsGraph",
                              "PersistentNames.InternalError", info));
    END;
  END ExistsGraph;


PROCEDURE IsDirect (names: T; name: Pathname.T; local: BOOLEAN): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN names.contained(name, local, names.directGraphs)
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.IsDirect", "Names.InternalError", info));
    END;
  END IsDirect;

PROCEDURE IsVisible (names: T; name: Pathname.T; local: BOOLEAN): BOOLEAN
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN names.contained(name, local, names.visibleGraphs)
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.IsVisible", "Names.InternalError", info));
    END;
  END IsVisible;

PROCEDURE ExistsDelta (names : T;
                       source: Pathname.T;
                       target: Pathname.T;
                       local : BOOLEAN     ): BOOLEAN
  RAISES {Access.Locked, InternalError, Unknown} =
  VAR
    tod, fromd: TextCursorSet.T;
    res       : BOOLEAN         := FALSE;
  BEGIN
    TRY
      IF Super.T.existsGraph(names, source, local)
           AND Super.T.existsGraph(names, target, local) THEN
        tod := names.targets(source, local, names.toDelta);
        fromd := names.sources(target, local, names.fromDelta);
        tod.intersection(fromd);
        res := NOT tod.isEmpty();
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.ExistsDelta",
                              "PersistentNames.InternalError", info));
    | Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.ExistsDelta", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
    RETURN res;
  END ExistsDelta;


PROCEDURE ExistsDeltaName (names: T; delta: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN names.contained(delta, local, collection := names.deltas);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.ExistsDeltaName",
                                       "Names.InternalError", info));
    END;
  END ExistsDeltaName;

PROCEDURE IsCheckoutCopy (names: T; name: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      RETURN NOT names.targets(name, local, names.original).isEmpty();
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.IsCheckoutCopy",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END IsCheckoutCopy;

PROCEDURE IsCheckoutOriginal (names: T; name: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {Access.Locked, Unknown, InternalError} =
  BEGIN
    TRY
      RETURN NOT names.sources(name, local, names.original).isEmpty();
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.IsCheckoutOriginal",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END IsCheckoutOriginal;

PROCEDURE IsCheckoutDelta (names: T; name: Pathname.T; local: BOOLEAN):
  BOOLEAN RAISES {Access.Locked, Unknown, InternalError} =
  VAR
    source, target: Pathname.T;
    found         : BOOLEAN;
  BEGIN
    TRY
      source :=
        names.sources(name, local, names.toDelta).extractAnyElement(found);
      target := names.targets(
                  name, local, names.fromDelta).extractAnyElement(found);
      RETURN names.related(local, source, target, names.original)
               OR names.related(local, target, source, names.original);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.IsCheckoutOriginal",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END IsCheckoutDelta;

PROCEDURE GetOriginal (names: T; name: Pathname.T; local: BOOLEAN):
  Pathname.T RAISES {Access.Locked, Unknown, InternalError} =
  VAR found: BOOLEAN;
  BEGIN
    TRY
      RETURN names.targets(name, local, names.original).extractAnyElement(
               found);
    EXCEPT
      Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.GetOriginal", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetOriginal;


PROCEDURE GetCheckoutDeltas (    names             : T;
                                 name              : Pathname.T;
                                 local             : BOOLEAN;
                             VAR otcdelta, ctodelta: Pathname.T  )
  RAISES {Access.Locked, InternalError, Unknown} =
  VAR
    orig     : Pathname.T;
    found    : BOOLEAN;
    ind, outd: TextCursorSet.T;
  BEGIN
    otcdelta := NIL;
    ctodelta := NIL;
    TRY
      IF Super.T.existsGraph(names, name, local) THEN
        (* Find the original graph, if any *)
        orig := names.targets(
                  name, local, names.original).extractAnyElement(found);
        IF found THEN
          (* name is a checkout copy *)
          (* Find original to copy delta *)
          ind := names.sources(name, local, names.fromDelta);
          outd := names.targets(orig, local, names.toDelta);
          ind.intersection(outd);
          otcdelta := ind.extractAnyElement(found);
          IF NOT found THEN
            RAISE InternalError(
                    ErrorSupport.Create("ChgMgmtNames.GetCheckoutDeltas",
                                        "Invalid graph structure"));
          END;
          (* Find copy to original delta *)
          ind := names.sources(orig, local, names.fromDelta);
          outd := names.targets(name, local, names.toDelta);
          ind.intersection(outd);
          ctodelta := ind.extractAnyElement(found);
          IF NOT found THEN
            RAISE InternalError(
                    ErrorSupport.Create("ChgMgmtNames.GetCheckoutDeltas",
                                        "Invalid graph structure"));
          END;
        END;
      ELSE
        (* name is no graph *)
        RAISE Unknown;
      END;
    EXCEPT
      Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.GetCheckoutDeltas",
                              "PersistentNames.InternalError", info));
    | Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.GetCheckoutDeltas",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetCheckoutDeltas;


PROCEDURE GetAllCheckedOutCopies (names: T;
                                  name : Pathname.T;
                                  local: BOOLEAN     ): TextCursorSet.T
  RAISES {Access.Locked, InternalError, Unknown} =
  VAR res: TextCursorSet.T;
  BEGIN
    TRY
      res := names.sources(name, local, names.original);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.GetSourceGraphs",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
    RETURN res;
  END GetAllCheckedOutCopies;

PROCEDURE GetSourceGraphs (names: T; name: Pathname.T; local: BOOLEAN):
  TextCursorSet.T RAISES {Access.Locked, InternalError, Unknown} =
  VAR
    inDeltas, sources: TextCursorSet.T;
    ok               : BOOLEAN;
    ind              : TEXT;
  BEGIN
    TRY
      inDeltas := names.sources(name, local, names.fromDelta);
      sources := TextCursorSet.New();
      inDeltas.loop();
      ind := inDeltas.get(ok);
      WHILE ok DO
        WITH d = names.sources(ind, local, names.toDelta) DO
          sources.union(d);
          d.dispose();
        END;
        ind := inDeltas.get(ok);
      END;
      inDeltas.dispose();
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.GetSourceGraphs",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
    RETURN sources;
  END GetSourceGraphs;

PROCEDURE GetDeltaName (names : T;
                        source: Pathname.T;
                        target: Pathname.T;
                        local : BOOLEAN     ): Pathname.T
  RAISES {Access.Locked, InternalError, Unknown} =
  VAR
    tod, fromd: TextCursorSet.T;
    name      : TEXT            := NIL;
    found     : BOOLEAN;
  BEGIN
    TRY
      IF Super.T.existsGraph(names, source, local)
           AND Super.T.existsGraph(names, target, local) THEN
        tod := names.targets(source, local, names.toDelta);
        fromd := names.sources(target, local, names.fromDelta);
        tod.intersection(fromd);
        IF NOT tod.isEmpty() THEN
          name := tod.extractAnyElement(found);
        END;
      END;
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.GetDeltaName",
                              "Names.InternalError", info));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.GetDeltaName",
                              "PersistentNames.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
    RETURN name;
  END GetDeltaName;

PROCEDURE GetDeltaSource (names: T; delta: Pathname.T; local: BOOLEAN):
  Pathname.T RAISES {Access.Locked, InternalError, Unknown} =
  VAR found: BOOLEAN;
  BEGIN
    TRY
      RETURN names.sources(delta, local, names.toDelta).extractAnyElement(
               found);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.GetDeltaSource",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetDeltaSource;

PROCEDURE GetDeltaTarget (names: T; delta: Pathname.T; local: BOOLEAN):
  Pathname.T RAISES {Access.Locked, InternalError, Unknown} =
  VAR found: BOOLEAN;
  BEGIN
    TRY
      RETURN names.targets(
               delta, local, names.fromDelta).extractAnyElement(found);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate("ChgMgmtNames.GetDeltaTarget",
                                       "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetDeltaTarget;

PROCEDURE GetMinDelta (names: T; graph: Pathname.T; local: BOOLEAN):
  Pathname.T RAISES {Access.Locked, Unknown, InternalError} =
  VAR found: BOOLEAN;
  BEGIN
    TRY
      RETURN names.targets(graph, local, names.minDelta).extractAnyElement(
               found);
    EXCEPT
      Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.GetMinDelta", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;

  END GetMinDelta;

PROCEDURE GetOutDeltas (names: T; name: Pathname.T; local: BOOLEAN):
  TextCursorSet.T RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      RETURN names.targets(name, local, names.toDelta);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.GetOutDeltas",
                              "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetOutDeltas;

PROCEDURE GetInDeltas (names: T; name: Pathname.T; local: BOOLEAN):
  TextCursorSet.T RAISES {Access.Locked, InternalError, Unknown} =
  BEGIN
    TRY
      RETURN names.sources(name, local, names.fromDelta);
    EXCEPT
      Names.InternalError (info) =>
        RAISE
          InternalError(
            ErrorSupport.Propagate(
              "ChgMgmtNames.GetInDeltas", "Names.InternalError", info));
    | Names.Unknown => RAISE Unknown;
    END;
  END GetInDeltas;

PROCEDURE GetGraphs (names: T; local: BOOLEAN; st: GraphState := All):
  TextCursorSet.T RAISES {Access.Locked, InternalError} =
  VAR all: TextCursorSet.T;
  BEGIN
    TRY
      all := Super.T.getGraphs(names, local);
      IF st.visible = Ternary.Yes THEN
        all.difference(names.getAllEntries(local, names.invisibleGraphs));
      ELSIF st.visible = Ternary.No THEN
        all.difference(names.getAllEntries(local, names.visibleGraphs));
      END;
      IF st.direct = Ternary.Yes THEN
        all.difference(names.getAllEntries(local, names.indirectGraphs));
      ELSIF st.direct = Ternary.No THEN
        all.difference(names.getAllEntries(local, names.directGraphs));
      END;
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.GetGraphs", "Names.InternalError", info));
    | Super.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.GetGraphs",
                              "PersistentNames.InternalError", info));
    END;
    RETURN all;
  END GetGraphs;


PROCEDURE GetDeltas (names: T; local: BOOLEAN): TextCursorSet.T
  RAISES {Access.Locked, InternalError} =
  BEGIN
    TRY
      RETURN names.getAllEntries(local, names.deltas);
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.GetDeltas", "Names.InternalError", info));
    END;
  END GetDeltas;


(* --- Procedures for reorganisation --- *)

PROCEDURE InvisibleGraphsWithoutTarget (names: T; local: BOOLEAN):
  TextCursorSet.T RAISES {Access.Locked, InternalError} =
  VAR
    inv, res: TextCursorSet.T;
    found   : BOOLEAN;
    graph   : Pathname.T;
  <* FATAL Names.Unknown *>
  BEGIN
    TRY
      inv :=
        names.getAllEntries(local, collection := names.invisibleGraphs);
      res := TextCursorSet.New();
      inv.loop();
      graph := inv.get(found);
      WHILE found DO
        IF names.targets(graph, local, names.toDelta).isEmpty() THEN
          res.insert(graph);
        END;
        graph := inv.get(found);
      END;
      inv.dispose();
      RETURN res;
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(
                ErrorSupport.Propagate(
                  "ChgMgmtNames.names.invisibleGraphsWithoutTarget",
                  "Names.InternalError", info));
    END;
  END InvisibleGraphsWithoutTarget;

PROCEDURE InvisibleDirectWithOneTarget (names: T; local: BOOLEAN):
  TextCursorSet.T RAISES {Access.Locked, InternalError} =
  VAR
    inv, res: TextCursorSet.T;
    found   : BOOLEAN;
    graph   : Pathname.T;
  <* FATAL Names.Unknown *>
  BEGIN
    TRY
      inv :=
        names.getAllEntries(local, collection := names.invisibleGraphs);
      res := TextCursorSet.New();
      inv.loop();
      graph := inv.get(found);
      WHILE found DO
        IF names.targets(graph, local, names.toDelta).card() = 1 THEN
          res.insert(graph);
        END;
        graph := inv.get(found);
      END;
      RETURN res;
    EXCEPT
      Names.InternalError (info) =>
        RAISE InternalError(ErrorSupport.Propagate(
                              "ChgMgmtNames.InvisibleDirectWithOneTarget",
                              "Names.InternalError", info));
    END;
  END InvisibleDirectWithOneTarget;

BEGIN
END ChgMgmtNames.
