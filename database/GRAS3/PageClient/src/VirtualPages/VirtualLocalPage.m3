MODULE VirtualLocalPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

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
      peekAccess        := PeekAccess;
      readAccess	:= ReadAccess;
      writeAccess	:= WriteAccess;
    END;


PROCEDURE Init		(         self		:T;
                                  pageNo	:CARDINAL;
                                  media		:PageMedia.T)
			:T =
  BEGIN
    self.handle := PageCache.GetPage (pageNo, media);

    RETURN self;
  END Init;


PROCEDURE PeekAccess	(         self		:T)
			:PageHandle.T =
  BEGIN
    RETURN self.handle
  END PeekAccess;


PROCEDURE ReadAccess	(         self		:T;
                         <*UNUSED*>
                              VAR pageAge       :CARDINAL)
			:PageHandle.T =
  BEGIN
    RETURN self.handle
  END ReadAccess;


PROCEDURE WriteAccess	(         self		:T;
                         <*UNUSED*>
                              VAR pageAge       :CARDINAL)
			:PageHandle.T =
  BEGIN
    RETURN self.handle
  END WriteAccess;


BEGIN
END VirtualLocalPage.
