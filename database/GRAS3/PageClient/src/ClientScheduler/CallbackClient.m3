MODULE CallbackClient;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.6  1997/03/25 12:06:32  rbnix
    	Bug fixed: full pair of PageCache.BeginAccess and
    	PageCache.EndAccess must be executed wether exceptions are
    	raised or not.

    Revision 1.5  1996/11/18 17:51:38  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.4  1996/10/29 15:01:33  rbnix
    	New parameter for page age added.

    Revision 1.3  1996/03/12 17:41:58  rbnix
    	Bug fixed: call to end critical section moved into TRY FINALLY
    	statement. If the releaseCallback was revoked this statement
    	was not executed resulting in a deadlock.

    Revision 1.2  1996/03/06 07:26:09  rbnix
    	Implementation for method ping added.

    Revision 1.1  1996/02/09 16:46:37  rbnix
    	First version of client scheduler added.

*)
(***************************************************************************)
(*
 | --- CallbackClient -----------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageCache,
  PageLock, Access, Transaction,
  CommunicationSeq, RemoteFile,
  InternalBaseScheduledClientRessource,
  ScheduledClientRessource, InternalScheduledClientRessource,
  CallbackPort;


REVEAL
  T			= Public BRANDED OBJECT
      ressource		: ScheduledClientRessource.T;

    OVERRIDES
      init		:= Init;
      releaseData	:= ReleaseData;
      propagateData	:= PropagateData;
      ping		:= Ping;
    END;


PROCEDURE Init		(         self		:T;
                                  ressource	:ScheduledClientRessource.T) :T =
  BEGIN
    self.ressource := ressource;

    RETURN self;
  END Init;


PROCEDURE ReleaseData   (         self		:T;
                                  file          :RemoteFile.T;
                                  pageNo	:CARDINAL;
                                  pageAge	:CARDINAL;
                                  lock		:PageLock.ServerMode)
			RAISES  {Access.Locked, CallbackPort.FatalError} =
  BEGIN
    PageCache.BeginAccess ();
    TRY
      self.ressource.releaseCallback (file, pageNo, pageAge, lock);
    FINALLY
      PageCache.EndAccess ();
    END
  END ReleaseData;


PROCEDURE PropagateData	(         self		:T;
                                  end		:Transaction.End;
                                  entries       :CommunicationSeq.T)
  RAISES {CallbackPort.FatalError} =
  BEGIN
    PageCache.BeginAccess ();
    TRY
      self.ressource.propagateCallback (end, entries);
      self.ressource.signalAccess ();
    FINALLY
      PageCache.EndAccess ();
    END
  END PropagateData;


PROCEDURE Ping		(         <* UNUSED *>
				  self            :T) =
  BEGIN
    (* doing nothing but saying I'm still alive.*)
  END Ping;


BEGIN
END CallbackClient.
