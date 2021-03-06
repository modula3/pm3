MODULE NullMedia;

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

    Revision 1.1  1996/01/31 10:04:41  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

IMPORT
  PageHandle,
  PageData;


REVEAL
  T                     = Public BRANDED OBJECT
    OVERRIDES
      loadData		:= LoadData;
      dropData		:= DropData;
    END;


PROCEDURE LoadData      (<* UNUSED *> self	:T;
                         <* UNUSED *> handle    :PageHandle.T;
                         <* UNUSED *>
                         VAR          data      :PageData.T) =
  BEGIN
    (* empty *)
  END LoadData;

  
PROCEDURE DropData      (<* UNUSED *> self	:T;
                         <* UNUSED *> handle    :PageHandle.T;
                         <* UNUSED *>
                         READONLY     data      :PageData.T) =
  BEGIN
    (* empty *)
  END DropData;


BEGIN
END NullMedia.
