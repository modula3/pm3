INTERFACE InternalScheduledServerResource;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.5  1996/11/21 07:56:01  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.4  1996/11/14 14:13:46  roland
    New exception Access.Denied flagging conflicting access modes when
    opening resources.

    Resource names will now be collected without the root path name.

    Revision 1.3  1996/08/06 16:32:49  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.1  1996/08/01 17:58:47  rbnix
    	Method inUse added.

    Revision 1.2  1996/03/06 08:13:36  rbnix
    	Method killClient is now exported.

    Revision 1.1  1996/02/26 17:59:43  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)

(*
 | --- InternalScheduledServerResource ------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Access, Termination,
  ServedClient,
  ScheduledServerResource,
  ClientInfoSeq;

REVEAL
  ScheduledServerResource.T <: Internal;

TYPE
  Internal		= ScheduledServerResource.Public OBJECT
    METHODS
      open		(         client	:ServedClient.T;
                                  access	:Access.Mode)
			:ScheduledServerResource.T
			RAISES {Access.Denied, Access.Invalid};

      killClient	(         client	:ServedClient.T;
                                  why		:TEXT := NIL);

      shutdown		(         termination	:Termination.Mode)
			RAISES {Termination.StillInUse};

      inUse		(         client	:ServedClient.T)
			:BOOLEAN;

      user              ()
			:ClientInfoSeq.T;
    END;

END InternalScheduledServerResource.
