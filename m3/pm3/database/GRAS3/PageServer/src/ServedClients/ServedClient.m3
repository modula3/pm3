MODULE ServedClient
EXPORTS ServedClient, InternalServedClient;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.8  1996/11/21 07:55:14  roland
    New resources getResourceUser, getFileUser, and getGraphUser
    implemented. These resources compute sequences of information about
    clients that use the Graph/Resource/File.

    Revision 1.7  1996/08/06 16:32:11  roland
    Merge of PAGESERVER and main branch.

    Revision 1.6.2.2  1996/07/24 12:54:16  rbnix
    	New parameter/method to access info text of client added.

    Revision 1.6.2.1  1996/07/11 10:48:36  rbnix
    	Method getTransactionNumber added. Transaction are numbered at
    	start of transaction to determine its age.

    Revision 1.6  1996/03/15 14:23:45  rbnix
    	Client id is reduced to unique number.

    Revision 1.5  1996/03/06 08:39:45  rbnix
    	Default kill reason added.

    Revision 1.4  1996/03/06 07:32:10  rbnix
    	In method kill: additional parameter describing why this
    	client was killed added.

    	New method whyKilled added to ask kill reason.

    Revision 1.3  1996/03/05 15:11:38  rbnix
    	Procedure Kill improved: the callback port is released now
    	when the client is killed. (This allows freeing the network object).

    Revision 1.2  1996/02/26 17:57:24  rbnix
    	Procedures Hash and Equal added to use with generics.

    Revision 1.1  1996/02/23 15:02:50  rbnix
    	First version of subsystem ServedClient added.

*)
(***************************************************************************)
(*
 | --- ServedClient -------------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Word, Text,
  Fmt, Pathname,
  CallbackPort, CommunicationSeq, ClientInfo;


REVEAL
  T				= Internal BRANDED OBJECT
      id			:TEXT;
      info			:ClientInfo.T;
      resourceName		:TEXT;
      callbackPort		:CallbackPort.T;
      xLockCount		:INTEGER;
      killed			:BOOLEAN;
      killReason		:TEXT;
      transactionActive		:BOOLEAN;
      transactionNumber		:CARDINAL;
      propagationData		:CommunicationSeq.T;

    OVERRIDES
      init			:= Init;
	
      getID			:= GetID;
      getInfo			:= GetInfo;
      getResourceName		:= GetResourceName;
      getCallbackPort		:= GetCallbackPort;

      incXLockCount		:= IncXLockCount;
      decXLockCount		:= DecXLockCount;
      getXLockCount		:= GetXLockCount;

      isKilled			:= IsKilled;
      kill			:= Kill;
      whyKilled			:= WhyKilled;

      inTransaction		:= InTransaction;
      setTransaction		:= SetTransaction;
      getTransactionNumber	:= GetTransactionNumber;

      getPropagationData	:= GetPropagationData;
      clearPropagationData	:= ClearPropagationData;
    END;


VAR
  clientNumber			:CARDINAL
				:= 0;
  transactionNumber		: CARDINAL
				:= 0;

  
PROCEDURE Init			(         self		:T;
                                          resourceName	:Pathname.T;
                                          callbackPort	:CallbackPort.T;
                                          info		:ClientInfo.T) :T =
  BEGIN
    INC (clientNumber);
    self.id := Fmt.Int (clientNumber);
    self.info := info;
    self.resourceName := resourceName;
    self.callbackPort := callbackPort;
    self.xLockCount := 0;
    self.killed := FALSE;
    self.killReason := "Unkown";
    self.transactionActive := FALSE;
    self.propagationData := NEW (CommunicationSeq.T).init ();

    RETURN self;
  END Init;


PROCEDURE GetID			(         self		:T) :TEXT =
  BEGIN
    RETURN self.id;
  END GetID;


PROCEDURE GetInfo		(         self		:T) :ClientInfo.T =
  BEGIN
    RETURN self.info;
  END GetInfo;


PROCEDURE GetResourceName	(         self		:T) :TEXT =
  BEGIN
    RETURN self.resourceName;
  END GetResourceName;


PROCEDURE GetCallbackPort	(         self		:T) :CallbackPort.T =
  BEGIN
    RETURN self.callbackPort;
  END GetCallbackPort;


PROCEDURE IncXLockCount		(         self		:T) =
  BEGIN
    INC (self.xLockCount);
  END IncXLockCount;


PROCEDURE DecXLockCount		(         self		:T) =
  BEGIN
    DEC (self.xLockCount);
  END DecXLockCount;


PROCEDURE GetXLockCount		(         self		:T) :INTEGER =
  BEGIN
    RETURN self.xLockCount;
  END GetXLockCount;


PROCEDURE IsKilled		(         self		:T) :BOOLEAN =
  BEGIN
    RETURN self.killed;
  END IsKilled;
  
  
PROCEDURE Kill			(         self		:T;
                                          why		:TEXT
							:= NIL) =
  BEGIN
    IF why # NIL THEN
      self.killReason := why;
    END;
    
    self.killed := TRUE;
    self.callbackPort := NIL;
  END Kill;


PROCEDURE WhyKilled		(         self		:T)
				:TEXT =
  BEGIN
    RETURN self.killReason;
  END WhyKilled;


PROCEDURE InTransaction		(         self		:T) :BOOLEAN =
  BEGIN
    RETURN self.transactionActive;
  END InTransaction;


PROCEDURE SetTransaction	(         self		:T;
                                          on		:BOOLEAN) =
  BEGIN
    self.transactionActive := on;

    IF on THEN
      INC (transactionNumber);
      self.transactionNumber := transactionNumber;
    END;
  END SetTransaction;


PROCEDURE GetTransactionNumber	(         self		:T)
				:CARDINAL =
  BEGIN
    RETURN self.transactionNumber;
  END GetTransactionNumber;


PROCEDURE GetPropagationData	(         self		:T)
				:CommunicationSeq.T =
  BEGIN
    RETURN self.propagationData;
  END GetPropagationData;


PROCEDURE ClearPropagationData	(         self		:T) =
  BEGIN
    self.propagationData := NEW (CommunicationSeq.T).init ();
  END ClearPropagationData;


PROCEDURE Equal			(         t1, t2	:T) :BOOLEAN =
  BEGIN
    RETURN (t1.id = t2.id);
  END Equal;

  
PROCEDURE Hash			(         t		:T) :Word.T =
  BEGIN
    RETURN Text.Hash (t.id)
  END Hash;


BEGIN
END ServedClient.
