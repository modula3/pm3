MODULE VirtualFile EXPORTS VirtualFile, InternalVirtualFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.2  1996/09/09 11:43:34  rbnix
    	Method getResource to relate files to their resource created
    	in base class. Therefore internal variables are removed.

    Revision 1.1  1996/02/29 17:44:17  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualFile --------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  VirtualResource, VirtualPage, VirtualPageTbl;


REVEAL
  T			= Internal BRANDED OBJECT
      pageTable		:VirtualPageTbl.Default;
      resource		:VirtualResource.T;

    OVERRIDES
      init		:= Init;
      getPage		:= GetPage;
      getResource	:= GetResource;
    END;


PROCEDURE Init		(         self		:T;
                                  resource	:VirtualResource.T)
			:T =
  BEGIN
    self.pageTable := NEW (VirtualPageTbl.Default).init ();
    self.resource := resource;

    RETURN self;
  END Init;


PROCEDURE GetPage	(         self		:T;
                                  pageNo	:CARDINAL)
			:VirtualPage.T =
  VAR
    page		:VirtualPage.T;
  BEGIN
    IF NOT self.pageTable.get (pageNo, page) THEN
      page := self.createPage (pageNo);
      EVAL self.pageTable.put (pageNo, page);
    END;

    RETURN page;
  END GetPage;


PROCEDURE GetResource	(         self		:T)
			:VirtualResource.T =
  BEGIN
    RETURN self.resource;
  END GetResource;


BEGIN
END VirtualFile.
