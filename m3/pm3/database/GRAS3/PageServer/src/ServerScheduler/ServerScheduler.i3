INTERFACE ServerScheduler;

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

    Revision 1.7  1996/11/21 07:56:08  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.6  1996/11/14 14:13:49  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.5  1996/11/08 14:46:05  roland
    GetResources in ServerScheduler handles PageFile.NoAccess correct now.

    Revision 1.4  1996/10/29 14:43:28  rbnix
    	Procedure SignalAccess based on BaseServerScheduler.SignalAccess
    	added.

    Revision 1.3  1996/08/06 16:32:57  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.1  1996/08/01 18:02:50  rbnix
    	New administration functions DeleteResource, CopyResource,
    	RenameResource, ExistsResource, ResourceInUse and GetResources
    	added.

    Revision 1.2  1996/03/06 08:16:04  rbnix
    	Function KillClient working over all resources added.

    Revision 1.1  1996/02/26 17:59:59  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- ServerScheduler ----------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname, TextTransientSeq AS TextSeq,
  Access, PageFile, Termination,
  ServedClient,
  ScheduledServerResource,
  ClientInfoSeq;


(* resource administration *)
PROCEDURE OpenResource	(         client	:ServedClient.T;
                                  baseName      :Pathname.T;
                                  access	:Access.Mode;
                                  new		:BOOLEAN)
			:ScheduledServerResource.T
			RAISES {PageFile.NoAccess, Access.Denied,
                                Access.Invalid};

PROCEDURE DeleteResource(         baseName	:Pathname.T)
			RAISES {PageFile.NoAccess};

PROCEDURE CopyResource	(         sourceName	:Pathname.T;
                                  destName	:Pathname.T)
			RAISES {PageFile.NoAccess};

PROCEDURE RenameResource(         oldName	:Pathname.T;
                                  newName	:Pathname.T)
			RAISES {PageFile.NoAccess};

PROCEDURE ExistsResource(baseName	:Pathname.T)
			:BOOLEAN;

PROCEDURE ResourceInUse	(baseName	:Pathname.T)
			:BOOLEAN;

PROCEDURE GetResourceUser(baseName	:Pathname.T)
			 :ClientInfoSeq.T;

PROCEDURE GetResources	()
			:TextSeq.T
                        RAISES{PageFile.NoAccess};

  
(* client administration *)
PROCEDURE KillClient	(         client	:ServedClient.T;
                                  why		:TEXT := NIL);


(* scheduler control *)
PROCEDURE Shutdown	(         termination	:Termination.Mode)
			RAISES {Termination.StillInUse};

PROCEDURE WaitShutdown	();
  
  
PROCEDURE SignalAccess	();

END ServerScheduler.
