INTERFACE ScheduledServerFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.1  1996/02/26 17:59:44  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- ScheduledServerFile ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT BaseScheduledServerFile AS Super;

CONST
  Brand			= "ScheduledServerFile";

TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      (* empty *)
    END;

END ScheduledServerFile.
