INTERFACE CallbackClient;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1996/02/09 16:46:36  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- CallbackClient -----------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT CallbackPort AS Super;
IMPORT
  ScheduledClientRessource;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      init		(         ressource	:ScheduledClientRessource.T) :T;
    END;

  

END CallbackClient.
