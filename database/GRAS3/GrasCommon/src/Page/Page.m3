MODULE Page;

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

    Revision 1.4  1996/03/02 16:51:21  rbnix
    	Bug fixed: from argument in SUBARRAY adjusted.

    Revision 1.3  1996/02/29 17:35:05  rbnix
    	New method copyData added.

    Revision 1.2  1996/02/09 16:32:55  rbnix
    	Function getAll added to avoid multiple copying of arguments.

    Revision 1.1  1996/01/31 10:02:11  rbnix
    	Initial version for subsystem Page.

*)
(***************************************************************************)

(*
 * --- Page ---------------------------------------------------------------
 * 
 * ------------------------------------------------------------------------
 *)

IMPORT
  PageData;

REVEAL
  T                     = Public BRANDED OBJECT
    OVERRIDES
      putData		:= PutData;
      getData		:= GetData;
      copyData		:= CopyData;
    END;


PROCEDURE PutData	(         self		:T;
			 READONLY data		:PageData.Part;
			          pos		:= FIRST (PageData.Index)) =
  BEGIN
    WITH sub = SUBARRAY (self.data, pos-FIRST (self.data), NUMBER (data)) DO
      sub := data;
    END;
  END PutData;

  
PROCEDURE GetData	(         self		:T;
			 VAR      data		:PageData.Part;
			          pos		:= FIRST (PageData.Index)) =
  BEGIN
    data := SUBARRAY (self.data, pos-FIRST (self.data), NUMBER (data));
  END GetData;


PROCEDURE CopyData      (         self          :T;
                                  source	:PageData.Index;
                                  destination	:PageData.Index;
                                  length	:PageData.Index) =
  BEGIN
    SUBARRAY (self.data, destination-FIRST (PageData.Index), length) :=
        SUBARRAY (self.data, source-FIRST (PageData.Index), length);
  END CopyData;


(* Page *)
BEGIN
END Page.
