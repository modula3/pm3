INTERFACE SimpleMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1996/02/09 16:39:29  rbnix
    	First version of a new PageMedia added.

    	SimpleMedia represents a media transfering pages between the
    	cache and a simple flat file.

*)
(***************************************************************************)

(*
 | --- SimpleMedia --------------------------------------------------------
 This abstract data type uses a simple PageFile as paging media.
 | ------------------------------------------------------------------------
 *)

IMPORT PageMedia AS Super;
IMPORT
  PageFile;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      init		(         file		:PageFile.T) :T;

      getFile		() :PageFile.T;
    END;
  

END SimpleMedia.
