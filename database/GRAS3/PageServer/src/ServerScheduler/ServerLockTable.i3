INTERFACE ServerLockTable;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.2  1996/03/06 16:17:04  rbnix
    	Method/Procedure Fmt added.

    Revision 1.1  1996/02/26 17:59:56  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- ServerLockTable ----------------------------------------------------
 Abstract data type module holding server locks related to clients.
 Initially the table defaults to t(c) --> PageLock.Mode.O for all
 clients c. These mode is also ignored by the iterator.
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageLock,
  ServedClient;

TYPE
  T			<: Public;

  Public		= Private OBJECT
    METHODS
      init		() :T;
      
      get		(         client	:ServedClient.T)
			:PageLock.ServerMode;

      put		(         client	:ServedClient.T;
                                  lock		:PageLock.ServerMode);

      
      iterate		() :Iterator;

      
      fmt		() :TEXT;
    END;

  Private		<: ROOT;


  Iterator		<: PublicIterator;
  
  PublicIterator	= OBJECT
    METHODS
      next		(VAR      client	:ServedClient.T;
                         VAR      lock		:PageLock.ServerMode)
			:BOOLEAN;
    END;

END ServerLockTable.
