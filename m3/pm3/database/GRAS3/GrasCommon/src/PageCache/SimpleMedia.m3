MODULE SimpleMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.2  1996/03/08 10:31:38  rbnix
    	In procedure DropData the usage of a local variable data is
    	replaced by the function getAll.

    Revision 1.1  1996/02/09 16:39:31  rbnix
    	First version of a new PageMedia added.

    	SimpleMedia represents a media transfering pages between the
    	cache and a simple flat file.

*)
(***************************************************************************)

IMPORT
  PageHandle,
  PageFile;


REVEAL
  T			= Public BRANDED OBJECT
      file		:PageFile.T;

    OVERRIDES
      (* SimpleMedia *)
      init		:= Init;
      getFile		:= GetFile;

      (* PageMedia *)
      loadData		:= LoadData;
      dropData		:= DropData;
    END;


PROCEDURE Init		(         self		:T;
                                  file		:PageFile.T) : T =
  BEGIN
    self.file := file;

    RETURN self;
  END Init;


PROCEDURE GetFile	(         self		:T) : PageFile.T =

  BEGIN
    RETURN self.file;
  END GetFile;


PROCEDURE LoadData	(         self		:T;
                                  handle	:PageHandle.T) =
  BEGIN
    handle.putData (self.file.getData (handle.getPageNo ()));
  END LoadData;


PROCEDURE DropData	(         self		:T;
                                  handle	:PageHandle.T) =
  BEGIN
    IF handle.isChanged () THEN
      self.file.putData (handle.getPageNo (), handle.getAll ());
    END;
  END DropData;


BEGIN
END SimpleMedia.
