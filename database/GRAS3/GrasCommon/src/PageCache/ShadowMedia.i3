INTERFACE ShadowMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/06/17 17:00:49  roland
    ShadowMedia has moved from PageClient to GrasCommon, because server
    uses it also.

    Revision 1.1  1996/02/09 16:47:03  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- ShadowMedia --------------------------------------------------------
 This specialized media extends its super type by a collection of used page
 numbers. The page numbers are NOT supervised and must be released manually
 if not longer used.
 | ------------------------------------------------------------------------
 *)

IMPORT SimpleMedia AS Super;
IMPORT
  PageFile;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      init		(         file		:PageFile.T) :T;
      
      obtainPageNo	() :CARDINAL;
      freePageNo	(         pageNo	:CARDINAL);
    END;
  

END ShadowMedia.
