INTERFACE InternalBasePageHandle;

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

    Revision 1.2  1996/03/08 10:29:47  rbnix
    	PageHandles are now tagged by an internal id. Therefore the
    	method init is introduced and the method fmt is enhanced.

    Revision 1.1  1996/01/31 10:04:35  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- InternalBasePageHandle ---------------------------------------------
 * This interface gives an extended view on BasePageHandle for
 * subsystem use only. The extended view allows changing the related page.
 * ------------------------------------------------------------------------
 *)

IMPORT
  Page,
  ObjectListItem,
  BasePageHandle;

REVEAL
  BasePageHandle.Private= ObjectListItem.T BRANDED OBJECT
    END;

  BasePageHandle.T	<: Internal;

TYPE
  Internal              = BasePageHandle.Public OBJECT
    METHODS
      init		() :BasePageHandle.T;

      setPage		(        page		:Page.T);
    END;

  
END InternalBasePageHandle.
