INTERFACE PageMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:28  hosking
    Import of GRAS3 1.1

    Revision 1.1  1996/01/31 10:04:53  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- PageMedia ----------------------------------------------------------
 * The abstract data type PageMedia specifies an interface to a (persistent)
 * storage media for page data.
 *
 * The data is transfered between the media and the page via it's handle
 * by access methods of the media working as specified below.
 * ------------------------------------------------------------------------
 *)

IMPORT BasePageMedia AS Super;
IMPORT
  PageHandle,
  PageData;


<* PRAGMA SPEC *>

<*
   SPEC
   PRIVATE VAR storage :ARRAY OF PageData
*>


TYPE
  T                     = Super.T OBJECT
    METHODS
      loadData		(         handle         :PageHandle.T;
                         VAR      data           :PageData.T) := NIL;
        <*
          SPEC
          MODIFIES data
          ENSURES data' = storage[handle.pageNo]
        *>

      dropData		(         handle         :PageHandle.T;
                         READONLY data           :PageData.T) := NIL;
        <*
          SPEC
          REQUIRES handle.isChanged <==> (data # storage[handle.pageNo])
          MODIFIES storage[handle.pageNo]
          ENSURES storage[handle.pageNo]' = data
        *>
    END;
      

END PageMedia.
