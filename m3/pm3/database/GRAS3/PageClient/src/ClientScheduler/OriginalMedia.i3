INTERFACE OriginalMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1996/02/09 16:46:50  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- OriginalMedia ------------------------------------------------------
 This abstract data type specializes an OriginalMedia as a PageMedia. This
 media does'nt transfer any data but releases locks if possible.
 | ------------------------------------------------------------------------
 *)
IMPORT PageMedia AS Super;
IMPORT
  RemoteFile,
  ScheduledClientFile;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      init		(         scheduledFile	:ScheduledClientFile.T;
                                  remoteFile	:RemoteFile.T) :T;

      getFile		() :RemoteFile.T;
    END;
  

END OriginalMedia.
