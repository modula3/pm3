INTERFACE InternalScheduledClientFile;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:47  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:36  hosking
    Import of GRAS3 1.1

    Revision 1.3  1996/11/18 17:51:42  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.2  1996/10/29 15:01:41  rbnix
    	New parameter for page age added.

    Revision 1.1  1996/02/09 16:46:46  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- InternalScheduledClientFile ----------------------------------------
  
 | ------------------------------------------------------------------------
 *)

IMPORT
  PageHandle,
  PageLock, Access,
  CommunicationSeq,
  ScheduledClientFile,
  CallbackPort;

REVEAL
  ScheduledClientFile.T	<: Internal;


TYPE
  Internal		= ScheduledClientFile.Public OBJECT
    METHODS
      dropData		(         handle         :PageHandle.T)
      RAISES {ScheduledClientFile.FatalError};

      commitTransaction	() RAISES {ScheduledClientFile.FatalError};
      chainTransaction	() RAISES {ScheduledClientFile.FatalError};
      abortTransaction	() RAISES {ScheduledClientFile.FatalError};

      releaseCallback	(         pageNo	:CARDINAL;
                                  pageAge	:CARDINAL;
                                  lock		:PageLock.CallbackMode) 
			RAISES {Access.Locked, CallbackPort.FatalError};
      propagateCallback	(         entries	:CommunicationSeq.T)
      RAISES {CallbackPort.FatalError};
    END;


END InternalScheduledClientFile.
