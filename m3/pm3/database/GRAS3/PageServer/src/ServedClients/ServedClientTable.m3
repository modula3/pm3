MODULE ServedClientTable;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.4  1996/11/21 07:55:17  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.3  1996/08/06 16:32:13  roland
    Merge of PAGESERVER and main branch.

    Revision 1.2.2.1  1996/07/24 12:54:21  rbnix
    	New parameter/method to access info text of client added.

    Revision 1.2  1996/03/06 07:33:28  rbnix
    	Function Delete added to remove a client entry out of the
    	collection.

    Revision 1.1  1996/02/23 15:02:55  rbnix
    	First version of subsystem ServedClient added.

*)
(***************************************************************************)
(*
 | --- ServedClientTable -------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Pathname,
  CallbackPort, ClientInfo,
  ServedClient, InternalServedClient, ServedClientTbl;


VAR
  table			:ServedClientTbl.T
			:= NEW (ServedClientTbl.Default). init ();

  
PROCEDURE New		(         resourceName	:Pathname.T;
			          callbackPort	:CallbackPort.T;
                                  info		:ClientInfo.T)
			:ServedClient.T =
  VAR
    client		:ServedClient.T;
  BEGIN
    client := NEW (ServedClient.T). init (resourceName, callbackPort, info);
    EVAL table.put (client.getID (), client);

    RETURN client;
  END New;


PROCEDURE Delete	(         client	:ServedClient.T) =
  BEGIN
    <* ASSERT (client.isKilled ()) *>

    EVAL table.delete (client.getID (), client);
  END Delete;


(*
 | --- Iterator -----------------------------------------------------------
 *)
REVEAL
  Iterator		= PublicIterator BRANDED OBJECT
      tblIterator	:ServedClientTbl.Iterator;

    OVERRIDES
      init		:= Init;
      next		:= Next;
    END;


PROCEDURE Init		(         self		:Iterator) :Iterator =
  BEGIN
    self.tblIterator := table.iterate ();

    RETURN self;
  END Init;


PROCEDURE Next		(         self		:Iterator;
                         VAR      client	:ServedClient.T) :BOOLEAN =
  VAR
    id			:TEXT;
  BEGIN
    RETURN self.tblIterator.next (id, client);
  END Next;


BEGIN
END ServedClientTable.
