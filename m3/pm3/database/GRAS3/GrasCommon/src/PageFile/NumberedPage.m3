MODULE NumberedPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1996/10/07 13:31:44  rbnix
    	New module for numbered pages.

*)
(***************************************************************************)
IMPORT
  PageData;


REVEAL
  T			= Public BRANDED OBJECT
      number		:CARDINAL;

    OVERRIDES
      init		:= Init;
      getNumber		:= GetNumber;
    END;


PROCEDURE Init		(         self		:T;
                                  number	:CARDINAL;
                         READONLY data		:PageData.T)
			:T =
  BEGIN
    self.number := number;
    self.putData (data);

    RETURN self;
  END Init;


PROCEDURE GetNumber	(         self		:T)
			:CARDINAL =
  BEGIN
    RETURN self.number;
  END GetNumber;


BEGIN
END NumberedPage.
