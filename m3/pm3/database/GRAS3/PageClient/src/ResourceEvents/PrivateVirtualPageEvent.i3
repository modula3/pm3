INTERFACE PrivateVirtualPageEvent;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/10/31 14:14:17  roland
    Subsystem introduces event and pattern types for virtual resources.

*)
(***************************************************************************)

FROM VirtualPageEvent IMPORT Operation;
IMPORT EventType, IntIntTransientTbl AS IntIntTbl;

(* These variables are also used by VirtualPageEventPattern.  They store
   the type and attribute numbers of VirtualPageEvents *)

VAR
  EType                               : ARRAY Operation OF EventType.T;
  TypeNumber                          : ARRAY Operation OF CARDINAL;
  Level, Resource, ResourceName, IsPre: ARRAY Operation OF CARDINAL;

  TypeToOp: IntIntTbl.T;         (* A map from event.type() to Operation *)

END PrivateVirtualPageEvent.
