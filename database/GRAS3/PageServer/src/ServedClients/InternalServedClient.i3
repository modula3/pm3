INTERFACE InternalServedClient;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.3  1996/11/21 07:55:11  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.2  1996/08/06 16:32:08  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/24 12:54:11  rbnix
    	New parameter/method to access info text of client added.

    Revision 1.1  1996/02/23 15:02:46  rbnix
    	First version of subsystem ServedClient added.

*)
(***************************************************************************)

(*
 | --- InternalServedClient -----------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  CallbackPort,
  ServedClient,
  ClientInfo;


REVEAL
  ServedClient.T		<: Internal;

TYPE
  Internal			= ServedClient.Public OBJECT
    METHODS
      init			(         resourceName	:Pathname.T;
				          callbackPort	:CallbackPort.T;
                                          info		:ClientInfo.T)
				:ServedClient.T;
  END;    
  

END InternalServedClient.
