INTERFACE InternalScheduledClientPage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.4  1996/11/18 17:51:43  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.3  1996/10/29 15:01:43  rbnix
    	New parameter for page age added.

    Revision 1.2  1996/03/11 17:18:32  rbnix
    	Method close added to return cached locks.

    Revision 1.1  1996/02/09 16:46:47  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- InternalScheduledClientPage ----------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Page,
  PageHandle,
  PageLock, Access,
  ScheduledClientPage, BaseScheduledClientFile,
  CallbackPort;

REVEAL
  ScheduledClientPage.T	<: Internal;

TYPE
  Internal		= ScheduledClientPage.Public OBJECT
    METHODS
      init		(         scheduledFile	:BaseScheduledClientFile.T;
                                  pageNo	:CARDINAL)
			:ScheduledClientPage.T;

      close		() RAISES {ScheduledClientPage.FatalError};
      commitTransaction	() RAISES {ScheduledClientPage.FatalError};
      abortTransaction	() RAISES {ScheduledClientPage.FatalError};

      releaseCallback	(         pageAge	:CARDINAL;
                                  lock		:PageLock.CallbackMode)
			RAISES {Access.Locked, CallbackPort.FatalError};
      propagateCallback	(         pageAge	:CARDINAL;
                                  lock		:PageLock.CallbackMode;
                                  page		:Page.T)
      RAISES {CallbackPort.FatalError};

      dropData		(         handle	:PageHandle.T)
      RAISES {ScheduledClientPage.FatalError};
    END;


END InternalScheduledClientPage.
