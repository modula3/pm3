INTERFACE BaseScheduledServerFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.2  1996/03/01 13:02:40  rbnix
    	Method getBaseName moved from private to public parts of
    	subsystem interfaces.

    Revision 1.1  1996/02/26 17:59:27  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- BaseScheduledServerFile --------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  RemoteFile;


TYPE
  T			<: Public;

  Public		= RemoteFile.T OBJECT
    METHODS
      getBaseName	() :Pathname.T;
    END;

END BaseScheduledServerFile.
