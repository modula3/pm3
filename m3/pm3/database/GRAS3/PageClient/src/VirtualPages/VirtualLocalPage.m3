MODULE VirtualLocalPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.1  1996/02/29 17:44:22  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualLocalPage ---------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageHandle, PageMedia, PageCache,
  InternalVirtualPage;


REVEAL
  T			= Public BRANDED OBJECT
      handle		:PageHandle.T;

    OVERRIDES
      init		:= Init;
      readAccess	:= Access;
      writeAccess	:= Access;
    END;


PROCEDURE Init		(         self		:T;
                                  pageNo	:CARDINAL;
                                  media		:PageMedia.T)
			:T =
  BEGIN
    self.handle := PageCache.GetPage (pageNo, media);

    RETURN self;
  END Init;


PROCEDURE Access	(         self		:T)
			:PageHandle.T =
  BEGIN
    RETURN self.handle
  END Access;


BEGIN
END VirtualLocalPage.
