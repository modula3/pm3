INTERFACE ServedClientTable;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.4  1996/11/21 07:55:16  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.3  1996/08/06 16:32:12  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.1  1996/07/24 12:54:19  rbnix
    	New parameter/method to access info text of client added.

    Revision 1.2  1996/03/06 07:33:27  rbnix
    	Function Delete added to remove a client entry out of the
    	collection.

    Revision 1.1  1996/02/23 15:02:52  rbnix
    	First version of subsystem ServedClient added.

*)
(***************************************************************************)

(*
 | --- ServedClientTable --------------------------------------------------
 This abstract data object modules is the collection of all created served
 clients. 
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  CallbackPort,
  ServedClient,
  ClientInfo;


PROCEDURE New		(         resourceName	:Pathname.T;
			          callbackPort	:CallbackPort.T;
                                  info		:ClientInfo.T)
			:ServedClient.T;

PROCEDURE Delete	(         client	:ServedClient.T);
  

TYPE
  Iterator		<: PublicIterator;

  PublicIterator	= OBJECT
    METHODS
      init		() :Iterator;
      
      next		(VAR      client	:ServedClient.T) :BOOLEAN;
    END;


END ServedClientTable.
