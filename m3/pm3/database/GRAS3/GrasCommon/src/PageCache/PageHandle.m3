MODULE PageHandle EXPORTS PageHandle, InternalPageHandle;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.5  1996/03/08 10:29:49  rbnix
    	PageHandles are now tagged by an internal id. Therefore the
    	method init is introduced and the method fmt is enhanced.

    Revision 1.4  1996/03/06 16:12:43  rbnix
    	Method fmt improved.

    Revision 1.3  1996/03/06 14:01:00  rbnix
    	New method fmt added to get a formatted representation of the
    	handle's value.

    Revision 1.2  1996/02/29 17:40:03  rbnix
    	New methods getAll and copyData added.

    Revision 1.1  1996/01/31 10:04:52  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- PageHandle ---------------------------------------------------------
 * To form a transparent view of swapping page data the handle checks
 * if the page is currently loaded. The relation between handle and page
 * will be established by the cache.
 * ------------------------------------------------------------------------
 *)
IMPORT BasePageHandle AS Super;
IMPORT Fmt AS StdFmt;
IMPORT
  PageData,
  InternalBasePageHandle,
  BasePageMedia,
  InternalPageCache;

REVEAL
  T                     = Internal BRANDED OBJECT
      pageNo		:CARDINAL;
      media             :BasePageMedia.T;

    OVERRIDES
      (* PageHandle *)
      init		:= Init;
      setMedia		:= SetMedia;
      getMedia		:= GetMedia;

      setPageNo		:= SetPageNo;
      getPageNo		:= GetPageNo;

      (* BasePageHandle *)
      putData		:= PutData;
      getData		:= GetData;
      getAll		:= GetAll;
      copyData		:= CopyData;
      fmt		:= Fmt;
    END;
      

PROCEDURE Init		(        self		:T) :T =
  BEGIN
    RETURN Super.T.init (self);
  END Init;


PROCEDURE SetMedia	(        self		:T;
                                 media		:BasePageMedia.T) =
  BEGIN
    self.media := media;
  END SetMedia;


PROCEDURE GetMedia	(        self		:T) :BasePageMedia.T =
  BEGIN
    RETURN self.media;
  END GetMedia;


PROCEDURE SetPageNo	(        self		:T;
                                 pageNo		:CARDINAL) =
  BEGIN
    self.pageNo := pageNo;
  END SetPageNo;


PROCEDURE GetPageNo	(        self		:T) :CARDINAL =
  BEGIN
    RETURN self.pageNo;
  END GetPageNo;


PROCEDURE PutData	(         self		:T;
			 READONLY data		:PageData.Part;
			          pos		:= FIRST (PageData.Index)) =
  BEGIN
    IF self.getPage () = NIL THEN
      InternalPageCache.LoadPage (self);
    ELSE
      InternalPageCache.RecognizeAccess (self);
    END;
    Super.T.putData (self, data, pos);
  END PutData;

  
PROCEDURE GetData	(         self		:T;
			 VAR      data		:PageData.Part;
			          pos		:= FIRST (PageData.Index)) =
  BEGIN
    IF self.getPage () = NIL THEN
      InternalPageCache.LoadPage (self);
    ELSE
      InternalPageCache.RecognizeAccess (self);
    END;
    Super.T.getData (self, data, pos);
  END GetData;


PROCEDURE GetAll	(         self		:T) :PageData.T =
  BEGIN
    IF self.getPage () = NIL THEN
      InternalPageCache.LoadPage (self);
    ELSE
      InternalPageCache.RecognizeAccess (self);
    END;
    RETURN Super.T.getAll (self);
  END GetAll;


PROCEDURE CopyData	(         self		:T;
                                  source	:PageData.Index;
                                  destination	:PageData.Index;
                                  length	:PageData.Index) =
  BEGIN
    IF self.getPage () = NIL THEN
      InternalPageCache.LoadPage (self);
    ELSE
      InternalPageCache.RecognizeAccess (self);
    END;
    Super.T.copyData (self, source, destination, length);
  END CopyData;
  

PROCEDURE Fmt		(         self		:T) :TEXT =
  BEGIN
    RETURN Super.T.fmt (self) & ", pageNo = " & StdFmt.Int (self.pageNo);
  END Fmt;
  

BEGIN
END PageHandle.
