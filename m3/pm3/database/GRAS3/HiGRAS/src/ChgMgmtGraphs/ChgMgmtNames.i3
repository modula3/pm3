INTERFACE ChgMgmtNames;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.5  1998/05/19 10:17:31  roland
    Support for log-groups implemented.

    Revision 1.4  1998/03/17 14:13:54  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.3  1997/10/17 16:45:03  renehuel
    bugfix in deltaCopyGraph.

    Revision 1.2  1997/05/01 13:21:05  roland
    Changed raises sets of transaction procedures. Pretty printing.

    Revision 1.1  1997/04/23 14:09:36  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

    Revision 1.2  1997/01/20 08:57:41  roland
    Reconstruction procedures implemented.

    Revision 1.1  1996/12/20 17:30:51  roland
    First version of ChgMgmtGraphSystem. Difference to
    PersistentGraphSystem: Graphs might be connected via deltas
    (DeltaCopyGraph) to allow efficient versioning.

*)
(***************************************************************************)

(* A specialization of Names for ChgMgmt. *)

IMPORT Pathname, TextCursorSet, PersistentGraphPool;
IMPORT AtomList, Access;
IMPORT PersistentNames AS Super;

TYPE
  Ternary = {No, Maybe, Yes};
  GraphState = RECORD
                 visible: Ternary;
                 direct : Ternary;
               END;

CONST
  All = GraphState{visible := Ternary.Maybe, direct := Ternary.Maybe};
  VisibleDirect = GraphState{visible := Ternary.Yes, direct := Ternary.Yes};
  Visible = GraphState{visible := Ternary.Yes, direct := Ternary.Maybe};


TYPE
  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      login (pool: PersistentGraphPool.T; collection: Pathname.T)
             RAISES {InternalError, Access.Locked};

      insertGraph (name: Pathname.T; local: BOOLEAN)
                   RAISES {Access.Locked, InternalError};
                   (* Insertion of a new graph with name 'name'.  New
                      graphs are always visible direct. *)

      removeGraph (    name   : Pathname.T;
                       local  : BOOLEAN;
                   VAR extRels: TextCursorSet.T)
                   RAISES {Access.Locked, InternalError};
                   (* Deletion of the specified graph from Names. *)

      removeDelta (name: Pathname.T; local: BOOLEAN)
                   RAISES {Access.Locked, InternalError};
                   (* Delete a delta (deltas dont have external
                      relations) *)

      renameGraph (old: Pathname.T; new: Pathname.T; local: BOOLEAN)
                   RAISES {Access.Locked, InternalError, Unknown};
                   (* Change the name for graph old into new. *)

      mkInvisible (name: Pathname.T; local: BOOLEAN)
                   RAISES {Access.Locked, InternalError};

      mkIndirect (name: Pathname.T; local: BOOLEAN)
                  RAISES {Access.Locked, InternalError};

      uniqueName (): TEXT RAISES {Access.Locked, InternalError};
                  (* Returns a text containing a number which is unique for
                     this instance of ChgMgmtNames *)

      insertDelta (source: Pathname.T;
                   target: Pathname.T;
                   delta : Pathname.T;
                   local : BOOLEAN     )
                   RAISES {Access.Locked, InternalError, Unknown};
                   (* source and target must exist.  target will be made
                      indirect, ie must be reconstructed from source when
                      opening.  delta is created and inserted between
                      source and target. *)

      insertLogGroup (name: Pathname.T; local: BOOLEAN)
                      RAISES {Access.Locked, InternalError};
                      (* Create a new empty log group *)

      removeLogGroup (name: Pathname.T; local: BOOLEAN)
                      RAISES {NotEmpty, Unknown, Access.Locked, InternalError};
                      (* Delete a log group.  The group must be empty. *)

      existsLogGroup (name: Pathname.T; local: BOOLEAN): BOOLEAN
                      RAISES {Access.Locked, InternalError};
                      (* Check whether group exists. *)

      addToLogGroup (group, member: Pathname.T; local: BOOLEAN)
                     RAISES {Unknown, Access.Locked, InternalError};
                     (* Add a graph to a log group.  If two graphs g1 and
                        g2 belong to the same log group, also the external
                        relation between them does.  The new member must
                        not be member of a different log group already. *)

      delFromLogGroup (group, member: Pathname.T; local: BOOLEAN)
                       RAISES {Access.Locked, InternalError};
                       (* Remove member from group. *)

      getLogGroupMembers (group: Pathname.T; local: BOOLEAN): TextCursorSet.T
                           RAISES {Unknown, Access.Locked, InternalError};
                           (* Return all members of group. *)

      getLogGroup (name: Pathname.T; local: BOOLEAN): Pathname.T
                   RAISES {Unknown, Access.Locked, InternalError};
                   (* Return the log group name belongs to.  If
                      name does not belong to group, NIL is returned. *)


      copy (source: Pathname.T; target: Pathname.T; local: BOOLEAN)
            RAISES {Access.Locked, InternalError};
            (* Creates a new graph with name target in Names and connects
               it via an copyOf edge with source.  Copies are always direct
               visible.*)


      checkOut (name : Pathname.T;
                as   : Pathname.T;
                otc  : Pathname.T;
                cto  : Pathname.T;
                local: BOOLEAN     )
                RAISES {Access.Locked, InternalError, Unknown};


      addDeltaCosts (delta: Pathname.T; local: BOOLEAN; costs: CARDINAL)
                     RAISES {Access.Locked, Unknown, InternalError};
                     (* Changes deltas costs and propagates new costs along
                        MinDelta edges *)

      setLogMode (name: Pathname.T; local: BOOLEAN; logMode: INTEGER)
                  RAISES {Access.Locked, Unknown, InternalError};

      getLogMode (name: Pathname.T; local: BOOLEAN): INTEGER
                  RAISES {Access.Locked, Unknown, InternalError};

      setWorkingName (name    : Pathname.T;
                      local   : BOOLEAN;
                      workname: Pathname.T  )
                      RAISES {Access.Locked, Unknown, InternalError};

      getWorkingName (name: Pathname.T; local: BOOLEAN): Pathname.T
                      RAISES {Access.Locked, Unknown, InternalError};

      (* --- Queries --- *)

      existsGraph (name: Pathname.T; local: BOOLEAN): BOOLEAN
                   RAISES {Access.Locked, InternalError};
                   (* returns TRUE, if the graph with name name exists *)

      (*
        getState(name: Pathname.T): GraphState
        RAISES {Access.Locked, InternalError};
      *)

      isDirect (name: Pathname.T; local: BOOLEAN): BOOLEAN
                RAISES {Access.Locked, InternalError};

      isVisible (name: Pathname.T; local: BOOLEAN): BOOLEAN
                 RAISES {Access.Locked, InternalError};

      existsDelta (source: Pathname.T; target: Pathname.T; local: BOOLEAN):
                   BOOLEAN RAISES {Access.Locked, InternalError, Unknown};
                   (* returns TRUE, if a delta between the graphs with
                      names sourceName and targetName exists *)

      existsDeltaName (delta: Pathname.T; local: BOOLEAN): BOOLEAN
                       RAISES {Access.Locked, InternalError};
                       (* returns TRUE, if a delta with given deltaName
                          exists *)

      isCheckoutCopy (name: Pathname.T; local: BOOLEAN): BOOLEAN
                      RAISES {Access.Locked, Unknown, InternalError};
                      (* Returns TRUE, iff name was checked out from
                         another graph *)

      isCheckoutOriginal (name: Pathname.T; local: BOOLEAN): BOOLEAN
                          RAISES {Access.Locked, Unknown, InternalError};
                          (* Return TRUE, iff name has checked out
                             copies *)

      getOriginal (name: Pathname.T; local: BOOLEAN): Pathname.T
                   RAISES {Access.Locked, Unknown, InternalError};
                   (* Return the graph of which name is a checkout copy. *)

      getAllCheckedOutCopies (name: Pathname.T; local: BOOLEAN):
                              TextCursorSet.T RAISES {Access.Locked,
                                                      Unknown,
                                                      InternalError};
                              (* Return the set of all checkout copies of
                                 name. *)

      getCheckoutDeltas (    name              : Pathname.T;
                             local             : BOOLEAN;
                         VAR otcdelta, ctodelta: Pathname.T  )
                         RAISES {Access.Locked, Unknown, InternalError};
                         (* If name is a checkout copy of some other graph,
                            otcdelta and ctodelta are the names of the
                            deltas connection original with copy and vice
                            versa.  If not, both will be NIL. *)

      getSourceGraphs (name: Pathname.T; local: BOOLEAN): TextCursorSet.T
                       RAISES {Access.Locked, InternalError, Unknown};
                       (* returns all names of graphs, which are possible
                          sources to given graph*)

      getDeltaName (source: Pathname.T; target: Pathname.T; local: BOOLEAN):
                    Pathname.T
                    RAISES {Access.Locked, InternalError, Unknown};
                    (* returns the pathname of the delta between the graphs
                       with names sourceName and targetName*)

      getDeltaSource (delta: Pathname.T; local: BOOLEAN): Pathname.T
                      RAISES {Access.Locked, Unknown, InternalError};

      getDeltaTarget (delta: Pathname.T; local: BOOLEAN): Pathname.T
                      RAISES {Access.Locked, Unknown, InternalError};

      getMinDelta (graph: Pathname.T; local: BOOLEAN): Pathname.T
                   RAISES {Access.Locked, Unknown, InternalError};


      getOutDeltas (name: Pathname.T; local: BOOLEAN): TextCursorSet.T
                    RAISES {Access.Locked, InternalError, Unknown};
                    (* returns all deltaNames of deltas with sourceGraph
                       name *)

      getInDeltas (name: Pathname.T; local: BOOLEAN): TextCursorSet.T
                   RAISES {Access.Locked, InternalError, Unknown};
                   (* returns all deltaNames of deltas with targetGraph
                      name *)

      getGraphs (local: BOOLEAN; st: GraphState := All): TextCursorSet.T
                 RAISES {Access.Locked, InternalError};
                 (* returns the names of the graphs in administration that
                    conform to st. *)

      getDeltas (local: BOOLEAN): TextCursorSet.T
                 RAISES {Access.Locked, InternalError};
                 (* returns the names of the deltas in administration *)


      (* --- Queries for reorganisation --- *)


      invisibleGraphsWithoutTarget (local: BOOLEAN): TextCursorSet.T
                                    RAISES {Access.Locked, InternalError};

      invisibleDirectWithOneTarget (local: BOOLEAN): TextCursorSet.T
                                    RAISES {Access.Locked, InternalError};
    END;

EXCEPTION
  Unknown;
  NotEmpty;
  InternalError(AtomList.T);

END ChgMgmtNames.
