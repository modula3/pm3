INTERFACE BaseObjectList;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

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
  T                     = BRANDED OBJECT
    END;


END BaseObjectList.
