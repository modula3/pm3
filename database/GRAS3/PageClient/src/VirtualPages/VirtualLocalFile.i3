INTERFACE VirtualLocalFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.2  1997/11/13 14:14:15  roland
    New parameter composeName for VirtualLocalFile.Open determines whether
    fileName should be treated as absolute path or as relative to its
    resource path.

    Revision 1.1  1996/02/29 17:44:19  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualLocalFile ---------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT VirtualFile AS Super;
IMPORT
  Pathname,
  PageFile,
  VirtualResource;


TYPE
  T			<: Public;

  Public		= Super.T OBJECT
    METHODS
      open		(         resource	:VirtualResource.T;
                                  fileName	:Pathname.T;
                                  new		:BOOLEAN;
                                  composeName   :BOOLEAN)
			:T
			RAISES {PageFile.NoAccess};
    END;
  

END VirtualLocalFile.
