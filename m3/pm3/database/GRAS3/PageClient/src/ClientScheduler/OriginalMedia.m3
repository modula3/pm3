MODULE OriginalMedia;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.2  1998/09/14 08:15:19  roland
    Modified code to remove compiler warnings.

    Revision 1.1  1996/02/09 16:46:52  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- OriginalMedia ------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)

IMPORT
  PageHandle,
  RemoteFile,
  ScheduledClientFile, InternalScheduledClientFile;


REVEAL
  T			= Public BRANDED OBJECT
      scheduledFile	:ScheduledClientFile.T;
      remoteFile	:RemoteFile.T;

    OVERRIDES
      (* OriginalMedia *)
      init		:= Init;
      getFile		:= GetFile;

      (* PageMedia *)
      loadData		:= LoadData;
      dropData		:= DropData;
    END;


PROCEDURE Init		(         self		:T;
                                  scheduledFile	:ScheduledClientFile.T;
                                  remoteFile	:RemoteFile.T) :T =
  BEGIN
    self.scheduledFile := scheduledFile;
    self.remoteFile := remoteFile;

    RETURN self;
  END Init;


PROCEDURE GetFile	(         self		:T) : RemoteFile.T =

  BEGIN
    RETURN self.remoteFile;
  END GetFile;


PROCEDURE LoadData	(         <* UNUSED *>
				  self            :T;
                                  <* UNUSED *>
				  handle	:PageHandle.T) =
  BEGIN
    <* ASSERT (FALSE) *>
    (*
      Data of original files may not loaded automatic by the cache!
    *)
  END LoadData;


PROCEDURE DropData	(         self		:T;
                                  handle	:PageHandle.T) =
  BEGIN
    <* ASSERT (NOT (handle.isChanged())) *>
    (*
      Data of original files may not be stored automatic by the cache!
    *)

    (* This potentially raises ScheduledClientFile.FatalError. We cannot
       put anything in the RAISES list, since it is inherited from
       PageMedia.T. So we simply ignore the compiler warning. *)
    self.scheduledFile.dropData (handle); <* NOWARN *>
  END DropData;
  

BEGIN
END OriginalMedia.
