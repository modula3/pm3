INTERFACE ServerWaitEntry;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:49  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:39  hosking
    Import of GRAS3 1.1

    Revision 1.2  1996/08/06 16:33:00  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/11 12:13:32  rbnix
    	First version of module providing a data type holding
    	information about waiting clients for data request.

*)
(***************************************************************************)

(*
 | --- ServerWaitEntry ----------------------------------------------------
 This data type contains all information to build the wait for graph.
 | ------------------------------------------------------------------------
 *)
IMPORT
  Thread, Pathname,
  PageLock,
  ServedClient,
  ServerLockTable;


CONST
  Brand			= "ServerWaitEntry";


TYPE
  T			= RECORD
    waitingClient	:ServedClient.T;
    otherClients	:ServerLockTable.T;
    waitingThread	:Thread.T;
    age			:CARDINAL;
    <*TRANSIENT*> file	:Pathname.T;
    pageNo		:CARDINAL;
    lock		:PageLock.ServerMode;
  END;


PROCEDURE Fmt		(         entry		:T) :TEXT;
  

END ServerWaitEntry.
