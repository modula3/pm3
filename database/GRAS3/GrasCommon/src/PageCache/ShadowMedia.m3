MODULE ShadowMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/06/17 17:00:50  roland
    ShadowMedia has moved from PageClient to GrasCommon, because server
    uses it also.

    Revision 1.2  1996/09/17 13:01:04  roland
    Adapted to new Basics and missing CarindalCollections

    Revision 1.1  1996/02/09 16:47:04  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

IMPORT SimpleMedia AS Super;
IMPORT
  CardStack,
  PageFile;


REVEAL
  T			= Public BRANDED OBJECT
      lastPage		:INTEGER;
      freePages		:CardStack.T;

    OVERRIDES
      (* SimpleMedia *)
      init		:= Init;

      obtainPageNo	:= ObtainPageNo;
      freePageNo	:= FreePageNo;
    END;


PROCEDURE Init		(         self		:T;
                                  file		:PageFile.T) :T =
  BEGIN
    self.lastPage := -1;
    self.freePages := NEW (CardStack.T).init();

    RETURN Super.T.init (self, file);
  END Init;


PROCEDURE ObtainPageNo	(         self		:T) :CARDINAL =
  <* FATAL CardStack.Undefined, CardStack.Empty *>
  BEGIN
    IF self.freePages.isEmpty () THEN
      INC (self.lastPage);
      RETURN self.lastPage;

    ELSE
      RETURN self.freePages.pop ()
    END
  END ObtainPageNo;


PROCEDURE FreePageNo	(         self		:T;
                                  pageNo	:CARDINAL) =
  <* FATAL CardStack.Undefined, CardStack.Full *>
  BEGIN
    self.freePages.push (pageNo);
  END FreePageNo;
  

BEGIN
END ShadowMedia.
