INTERFACE BaseObjectList;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

    Revision 1.1  1996/01/31 10:04:26  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)


(*
 * --- BasePageMedia ------------------------------------------------------
 * Basic abstract data type of a still unspecified object list.
 * 
 * This type is for subsystem use only avoiding cyclic imports.
 * ------------------------------------------------------------------------
 *)

TYPE
  T                     = <*TRANSIENT*> ROOT BRANDED OBJECT
    END;


END BaseObjectList.
