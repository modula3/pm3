INTERFACE ServedClient;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.5  1996/11/21 07:55:13  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.4  1996/08/06 16:32:09  roland
    Merge of PAGESERVER and main branch.

    Revision 1.3.2.2  1996/07/24 12:54:14  rbnix
    	New parameter/method to access info text of client added.

    Revision 1.3.2.1  1996/07/11 10:48:34  rbnix
    	Method getTransactionNumber added. Transaction are numbered at
    	start of transaction to determine its age.

    Revision 1.3  1996/03/06 07:32:08  rbnix
    	In method kill: additional parameter describing why this
    	client was killed added.

    	New method whyKilled added to ask kill reason.

    Revision 1.2  1996/02/26 17:57:23  rbnix
    	Procedures Hash and Equal added to use with generics.

    Revision 1.1  1996/02/23 15:02:47  rbnix
    	First version of subsystem ServedClient added.

*)
(***************************************************************************)

(*
 | --- ServedClientEntry --------------------------------------------------
 This abstract data type module holds common used information about served
 clients.
 | ------------------------------------------------------------------------
 *)
IMPORT
  Word,
  Pathname,
  CallbackPort, CommunicationSeq, ClientInfo;


CONST
  Brand				= "ServedClient";

TYPE
  T				<: Public;

  Public			= OBJECT
    METHODS
      getID			() :TEXT;
      getInfo			() :ClientInfo.T;
      getResourceName		() :Pathname.T;
      getCallbackPort		() :CallbackPort.T;

      
      incXLockCount		();
      decXLockCount		();
      getXLockCount		() :INTEGER;

      isKilled			() :BOOLEAN;
      kill			(         why		:TEXT
							:= NIL);
      whyKilled			() :TEXT;

      inTransaction		() :BOOLEAN;
      setTransaction		(         on		:BOOLEAN);
      getTransactionNumber	() :CARDINAL;

      getPropagationData	() :CommunicationSeq.T;
      clearPropagationData	();
    END;


PROCEDURE Equal			(         t1, t2	:T) :BOOLEAN;

PROCEDURE Hash			(         t		:T) :Word.T;


END ServedClient.
