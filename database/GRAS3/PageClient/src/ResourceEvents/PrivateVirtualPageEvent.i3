INTERFACE PrivateVirtualPageEvent;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:14:17  roland
    Subsystem introduces event and pattern types for virtual resources.

*)
(***************************************************************************)

FROM VirtualPageEvent IMPORT Operation;
IMPORT EventType, IntIntTbl;

(* These variables are also used by VirtualPageEventPattern.  They store
   the type and attribute numbers of VirtualPageEvents *)

VAR
  EType                               : ARRAY Operation OF EventType.T;
  TypeNumber                          : ARRAY Operation OF CARDINAL;
  Level, Resource, ResourceName, IsPre: ARRAY Operation OF CARDINAL;

  TypeToOp: IntIntTbl.T;         (* A map from event.type() to Operation *)

END PrivateVirtualPageEvent.
