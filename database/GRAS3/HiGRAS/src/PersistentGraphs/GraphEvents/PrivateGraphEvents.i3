INTERFACE PrivateGraphEvents;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:33  hosking
    Initial revision

    Revision 1.2  1997/11/21 09:37:19  roland
    New GraphEvents PutAttribute and TruncateAttribute replace ModifyAttribute

    Revision 1.1  1997/11/10 10:46:33  roland
    Graph event type definitions and handling.

*)
(***************************************************************************)

IMPORT EventType, IntIntTbl;
FROM GraphEvents IMPORT Operation;

(* Attribute numbers are declared constant.  They are checked during progrm
   initialization. *)

CONST
  (* common to all graph event type *)
  PoolNameANo = 1;
  PoolANo     = 2;
  GraphNoANo  = 3;
  GraphANo    = 4;
  IsPreANo    = 5;
  LevelANo    = 6;

  (* specialized attributes *)
  FirstNodeANo = 7;              (* node ind node/attribute events, source
                                    in edge events *)
  FirstLabelANo = 8;             (* node/edge label in node/edge events,
                                    attribute no in attribute/index
                                    events *)
  FirstNodeExANo = 9;            (* sourceEx for edge events, nodeEx for
                                    attribute/index events *)
  TargetNodeANo = 10;            (* node ind node/attribute events, source
                                    in edge events *)
  TargetNodeExANo = 11;          (* sourceEx for edge events, nodeEx for
                                    attribute/index events *)
  TargetGraphANo = 12;           (* the number of the target graph for
                                    graph crossing edges *)
  SourceGraphANo = 13;           (* the number of the source graph for
                                    graph crossing edges *)
  TextANo = 10;                  (* the text in modify attribute, delete
                                    and put index events *)
  AttrStartANo = 11;             (* start for modify attribute events *)
  AttrLengthANo = 10;            (* length for truncate attribute *)

(* These variables are also used by GraphEventPattern.  They store the type
   numbers of GraphEvents *)

VAR
  EType     : ARRAY Operation OF EventType.T;
  TypeNumber: ARRAY Operation OF CARDINAL;

  TypeToOp: IntIntTbl.T;         (* A map from event.type() to Operation *)

END PrivateGraphEvents.
