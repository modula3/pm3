INTERFACE CallbackPort;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:43  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

    Revision 1.4  1996/11/20 12:19:36  roland
    Improved exception handling. ASSERTs and FATALs (mostly) replaced by
    exceptions.

    Revision 1.3  1996/10/29 14:06:22  rbnix
    	New variable pageAge added.

    Revision 1.2  1996/03/06 07:25:38  rbnix
    	Specification of method ping added.

    Revision 1.1  1996/02/09 16:42:54  rbnix
    	First version of specification layer for network objects
    	added.

*)
(***************************************************************************)

(*
 | --- CallbackPort -------------------------------------------------------
  The abstract data type CallbackPort specifies the communication interface
  to be implemented by a client.

  The call releaseData is invoked by the server if the lock for a cached
  page (old lock = PageLock.Mode.C) should be revoked and the page is not
  longer be accessable at client side (new lock = PageLock.Mode.P). If the
  client still holds the current lock the exception Access.Locked is raised
  to inform the server.

  On the other hand the method propagateData will be invoked by the server
  if pending data may become accessable or out-of-date due to an other
  client ends a transaction. 
 | ------------------------------------------------------------------------
 *)
IMPORT
  Thread, NetObj, AtomList,
  PageLock, Access, Txn,
  RemoteFile, CommunicationSeq;


TYPE
  T                     = NetObj.T BRANDED "CallbackPort" OBJECT
    METHODS
      releaseData       (         file          :RemoteFile.T;
                                  pageNo	:CARDINAL;
                                  pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode)
			RAISES {Thread.Alerted, NetObj.Error,
                                Access.Locked, FatalError};

      propagateData     (         end		:Txn.End;
                                  entries       :CommunicationSeq.T)
			RAISES {Thread.Alerted, NetObj.Error, FatalError};

      ping		()
			RAISES {Thread.Alerted, NetObj.Error};
    END;

EXCEPTION
  FatalError(AtomList.T);
  
END CallbackPort.
