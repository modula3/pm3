INTERFACE PageHandle;

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

    Revision 1.2  1996/02/29 17:40:02  rbnix
    	New methods getAll and copyData added.

    Revision 1.1  1996/01/31 10:04:51  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- PageHandle ---------------------------------------------------------
 * This abstract data type specializes a BasePageHandle as a swapable page
 * relating it to a storage position and media.
 *
 * This type represents not only an isolated entry but also it's relation
 * to the PageCache. Therefore swapping is made mostly transparent.
 * ------------------------------------------------------------------------
 *)

IMPORT BasePageHandle AS Super;
IMPORT
  Page,
  BasePageMedia;

<* PRAGMA SPEC *>

CONST
  Brand			="PageHandle";		(* for generic modules	*)

TYPE
  T                     <: Public;

  Public		= Super.T OBJECT
    METHODS
      setMedia		(        media		:BasePageMedia.T);
      getMedia		() :BasePageMedia.T;

      setPageNo		(        pageNo		:CARDINAL);
      getPageNo		() :CARDINAL;

      loadData          ();
      dropData		();

      accessPage	(): Page.T;

    (*
    OVERRIDES
      (* BasePageHandle *)
      putData		:= PutData;
      getData		:= GetData;
      getAll		:= GetAll;
      putAll		:= PutAll;
      copyData		:= CopyData;
      (*
        Access to page data MUST be done within following specification,
        see PageCache for further details:
      *)
      <* SPEC REQUIRES cacheUser = CURRENT *>
    *)
    END;
  

END PageHandle.
