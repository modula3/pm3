MODULE NullMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/01/31 10:04:41  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

IMPORT
  PageHandle;


REVEAL
  T                     = Public BRANDED OBJECT
    OVERRIDES
      loadData		:= LoadData;
      dropData		:= DropData;
    END;


PROCEDURE LoadData      (<* UNUSED *> self	:T;
                         <* UNUSED *> handle	:PageHandle.T) =
  BEGIN
    (* empty *)
  END LoadData;

  
PROCEDURE DropData      (<* UNUSED *> self	:T;
                         <* UNUSED *> handle	:PageHandle.T) =
  BEGIN
    (* empty *)
  END DropData;


BEGIN
END NullMedia.
