INTERFACE PageMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

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
  PageHandle;


<* PRAGMA SPEC *>

<*
   SPEC
   PRIVATE VAR storage :ARRAY OF PageData
*>


TYPE
  T                     = Super.T OBJECT
    METHODS
      loadData		(        handle		:PageHandle.T) := NIL;
        <*
          SPEC
          MODIFIES handle.data
          ENSURES handle.data' = storage[handle.pageNo]
        *>

      dropData		(        handle		:PageHandle.T) := NIL;
        <*
          SPEC
          REQUIRES handle.isChanged <==> (handle.data # storage[handle.pageNo])
          MODIFIES storage[handle.pageNo]
          ENSURE storage[handle.pageNo]' = handle.data
        *>
    END;
      

END PageMedia.
