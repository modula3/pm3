INTERFACE BasePageHandle;

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

    Revision 1.3  1996/03/06 14:00:55  rbnix
    	New method fmt added to get a formatted representation of the
    	handle's value.

    Revision 1.2  1996/02/29 17:40:00  rbnix
    	New methods getAll and copyData added.

    Revision 1.1  1996/01/31 10:04:28  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- BasePageHandle -----------------------------------------------------
 * This abstract data type represents an access path to a temporary load
 * page. Changes can be recognized due to write access is monitored.
 * ------------------------------------------------------------------------
 *)

IMPORT
  Page,
  PageData;


TYPE
  T                     <: Public;

  Public		= Private OBJECT
    METHODS
      isLoad            () :BOOLEAN;

      isChanged		() :BOOLEAN;
      markChanged	();
      unmarkChanges	();

      getPage		() :Page.T;

      putData           (READONLY data		:PageData.Part;
			          pos		:= FIRST (PageData.Index));

      getData           (VAR      data		:PageData.Part;
			          pos		:= FIRST (PageData.Index));


      putAll		(         unswizzler    :PageData.Unswizzler);
                         	  
      getAll            (	  swizzler	:PageData.Swizzler);

      copyData		(         source	:PageData.Index;
                                  destination	:PageData.Index;
                                  length	:PageData.Index);

      fmt		() :TEXT;
    END;

  Private		<: <*TRANSIENT*> ROOT;


END BasePageHandle.
