INTERFACE Page;

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

    Revision 1.3  1996/02/29 17:35:03  rbnix
    	New method copyData added.

    Revision 1.2  1996/02/09 16:32:54  rbnix
    	Function getAll added to avoid multiple copying of arguments.

    Revision 1.1  1996/01/31 10:02:09  rbnix
    	Initial version for subsystem Page.

*)
(***************************************************************************)

(*
 * --- Page ---------------------------------------------------------------
 * Abstract data type containing "raw" page data.
 * ------------------------------------------------------------------------
 *)

IMPORT
  PageData;

TYPE
  T                     <: Public;

  Public		= <*TRANSIENT*> ROOT OBJECT
      data		:PageData.T;

    METHODS
      putData           (READONLY data		:PageData.Part;
			          pos		:= FIRST (PageData.Index));

      getData           (VAR      data		:PageData.Part;
			          pos		:= FIRST (PageData.Index));

      copyData		(         source	:PageData.Index;
                                  destination	:PageData.Index;
                                  length	:PageData.Index);
    END;


END Page.
