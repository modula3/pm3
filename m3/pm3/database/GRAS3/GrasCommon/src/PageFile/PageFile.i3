INTERFACE PageFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.6  1996/11/20 12:16:44  roland
    Method shutdown added to close a buffered file when server is forced
    to terminate.

    Revision 1.5  1996/03/06 16:13:51  rbnix
    	New methods isOpen and getFileName added.

    Revision 1.4  1996/03/02 16:55:08  rbnix
    	Bug fixed: variable file adjusted (used twice before).

    	Bug fixed: recognized that reading unknown pages returns
    	length zero.

    Revision 1.3  1996/02/23 14:57:41  rbnix
    	Method open replaced by init+open. This change allows
    	re-open the file after closing it.

    Revision 1.2  1996/02/21 13:45:48  rbnix
    	Desciptive parameter TEXT added to exception PageFile.NoAccess.

    Revision 1.1  1996/02/09 16:40:43  rbnix
    	First version of a page oriented file type added.

*)
(***************************************************************************)

(*
 | --- PageFile -----------------------------------------------------------
 This abstract data type represents a persistent storage of pages on a
 file system. 
 | ------------------------------------------------------------------------
 *)


IMPORT
  Pathname,
  PageData;


EXCEPTION
  NoAccess (TEXT);


TYPE
  T			<: Public;

  Public		= OBJECT
    METHODS
      init              (         fileName      :Pathname.T;
				  new		:BOOLEAN) :T;

      open		()
			RAISES {NoAccess};

      close             ();

      shutdown          (); (* close when program terminates *)

      truncate          (         newSize       :CARDINAL);

      flush		();

      getData		(         pageNo	:CARDINAL) :PageData.T;

      putData		(         pageNo	:CARDINAL;
                         READONLY data		:PageData.T);
                                 
      isOpen		() :BOOLEAN;
      getFileName	() :Pathname.T;
  END;


END PageFile.
