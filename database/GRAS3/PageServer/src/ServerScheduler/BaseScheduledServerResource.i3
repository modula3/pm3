INTERFACE BaseScheduledServerResource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:49  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:38  hosking
    Import of GRAS3 1.1

    Revision 1.2  1996/03/01 13:02:43  rbnix
    	Method getBaseName moved from private to public parts of
    	subsystem interfaces.

    Revision 1.1  1996/02/26 17:59:31  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- BaseScheduledServerResource ----------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname;


TYPE
  T			<: Public;

  Public		= <*TRANSIENT*> ROOT OBJECT
    METHODS
      getBaseName	() :Pathname.T;
    END;

END BaseScheduledServerResource.
