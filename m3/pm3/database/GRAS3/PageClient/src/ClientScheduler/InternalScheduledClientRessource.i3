INTERFACE InternalScheduledClientRessource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:36  hosking
    Import of GRAS3 1.1

    Revision 1.3  1996/11/18 17:51:44  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.2  1996/10/29 15:01:45  rbnix
    	New parameter for page age added.

    Revision 1.1  1996/02/09 16:46:48  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)

(*
 | --- InternalScheduledClientRessource -----------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageLock, Access, Txn,
  RemoteFile, CommunicationSeq,
  ScheduledClientRessource,
  CallbackPort;


REVEAL
  ScheduledClientRessource.T	<: Internal;

TYPE
  Internal		= ScheduledClientRessource.Public OBJECT
    METHODS
      releaseCallback	(         file		:RemoteFile.T;
                                  pageNo	:CARDINAL;
                                  pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode)
			RAISES {Access.Locked, CallbackPort.FatalError};

      propagateCallback	(         end		:Txn.End;
                                  entries	:CommunicationSeq.T)
      RAISES {CallbackPort.FatalError};
    END;

  
END InternalScheduledClientRessource.
