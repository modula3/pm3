INTERFACE InternalVirtualFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.2  1996/09/09 11:43:31  rbnix
    	Method getResource to relate files to their resource created
    	in base class. Therefore internal variables are removed.

    Revision 1.1  1996/02/29 17:44:12  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- InternalVirtualFile ------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  VirtualResource, VirtualFile, VirtualPage;


REVEAL
  VirtualFile.T		<: Internal;

TYPE
  Internal		= VirtualFile.Public OBJECT
    METHODS
      init		(         resource	:VirtualResource.T)
			:VirtualFile.T;
      
      createPage	(         pageNo	:CARDINAL)
			:VirtualPage.T
			:= NIL;
    END;


END InternalVirtualFile.
