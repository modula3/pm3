INTERFACE PrivateLogEvents;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1997/12/02 17:56:42  roland
    New event types and event contexts for user recovery operations
    introduced.

*)
(***************************************************************************)

IMPORT EventType, IntIntTbl;
FROM LogEvents IMPORT Operation;

(* Attribute numbers are declared constant.  They are checked during progrm
   initialization. *)

CONST
  (* common to all log event type *)
  PoolNameANo = 1;
  PoolANo     = 2;
  GraphNoANo  = 3;
  GraphANo    = 4;
  IsPreANo    = 5;
  LevelANo    = 6;

  (* specialized attributes *)
  SonNoANo = 7;              (* son to go to in redoIth events  *)

  (* These variables are also used by LogEventPattern.  They store the type
   numbers of LogEvents *)

VAR
  EType     : ARRAY Operation OF EventType.T;
  TypeNumber: ARRAY Operation OF CARDINAL;

  TypeToOp: IntIntTbl.T;         (* A map from event.type() to Operation *)

END PrivateLogEvents.
