INTERFACE RemoteFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/02/09 16:43:02  rbnix
    	First version of specification layer for network objects
    	added.

*)
(***************************************************************************)

(*
 | --- RemoteFile ---------------------------------------------------------
 This abstract data type identifies an allready opened file at the server
 side.
 | ------------------------------------------------------------------------
 *)
IMPORT
  NetObj;


TYPE
  T		= NetObj.T BRANDED "RemoteFile" OBJECT
    (* empty *)
    END;
  

END RemoteFile.
