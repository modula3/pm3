MODULE SimpleMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:28  hosking
    Import of GRAS3 1.1

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
  PageData,
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
                                  handle        :PageHandle.T;
                         VAR      data          :PageData.T) =
  BEGIN
    self.file.getData (handle.getPageNo (), data);
  END LoadData;


PROCEDURE DropData 	(         self	 	:T;
                                  handle        :PageHandle.T;
                         READONLY data          :PageData.T) =
  BEGIN
    IF handle.isChanged () THEN
      self.file.putData (handle.getPageNo (), data);
    END;
  END DropData; 


BEGIN
END SimpleMedia.
