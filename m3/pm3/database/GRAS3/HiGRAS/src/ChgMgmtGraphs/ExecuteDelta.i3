INTERFACE ExecuteDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.2  1998/05/19 10:17:33  roland
    Support for log-groups implemented.

    Revision 1.1  1997/04/23 14:09:46  roland
    ChgMgmtGraph adapted to HiGRAS, i.e with pools and graph boundary
    crossing edges.

*)
(***************************************************************************)

IMPORT Access, ChgMgmtOpenGraphs, PersistentGraph, Delta;
IMPORT AtomList;

(* Used bye ChgMgmtGraphPool as well as ChgMgmtGraph *)

PROCEDURE F (og: ChgMgmtOpenGraphs.T; d: Delta.T)
  RAISES {Access.Locked, PersistentGraph.NodeNotFound, NotOpen,
          InternalError};

EXCEPTION
  NotOpen;                       (* The delta needs to execute a command on
                                    a graph that is not open *)
  InternalError(AtomList.T);

END ExecuteDelta.
