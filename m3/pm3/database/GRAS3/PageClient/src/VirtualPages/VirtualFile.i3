INTERFACE VirtualFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.4  1998/01/21 14:12:12  roland
    New method baseName.

    Revision 1.3  1996/11/18 17:52:18  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.2  1996/09/09 11:43:33  rbnix
    	Method getResource to relate files to their resource created
    	in base class. Therefore internal variables are removed.

    Revision 1.1  1996/02/29 17:44:16  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualFile --------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  VirtualResource, VirtualPage;
IMPORT
  Pathname;

TYPE
  T			<: Public;

  Public		= OBJECT
    METHODS
      getBaseName       ()
                        :Pathname.T := NIL;
      
      getPage		(         pageNo	:CARDINAL)
			:VirtualPage.T;

      close		() RAISES {VirtualResource.FatalError}
			:= NIL;

      getResource	()
			:VirtualResource.T;
    END;


END VirtualFile.
