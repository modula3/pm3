MODULE ServerLockTable;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:39  hosking
    Initial revision

    Revision 1.2  1996/03/06 16:17:05  rbnix
    	Method/Procedure Fmt added.

    Revision 1.1  1996/02/26 17:59:57  rbnix
    	First version of subsystem ServerScheduler.

*)
(***************************************************************************)
(*
 | --- ServerLockTable ----------------------------------------------------
 Entries with lock = PageLock.Mode.O are assumed as default values and
 therefore not stored in the table.
 | ------------------------------------------------------------------------
 *)
IMPORT BaseServerLockTbl AS Super;
IMPORT
  PageLock,
  ServedClient,
  ServerLockEntry;

REVEAL
  Private		= Super.Default BRANDED OBJECT
      (* empty *)
    END;

  T			= Public BRANDED OBJECT
    OVERRIDES
      init		:= Init;

      get		:= Get;
      put		:= Put;
      
      iterate		:= Iterate;

      fmt		:= Fmt;
  END;

  Iterator		= PublicIterator BRANDED OBJECT
      i			: Super.Iterator;
    OVERRIDES
      next		:= Next;
    END;


PROCEDURE Init		(         self		:T) :T =
  BEGIN
    RETURN NARROW (self, Super.Default).init ();
  END Init;

PROCEDURE Get		(         self		:T;
                                  client	:ServedClient.T)
			:PageLock.ServerMode =
  VAR
    entry		:ServerLockEntry.T;
  BEGIN
    IF NARROW (self, Super.T).get (client, entry) THEN
      RETURN entry.lock
    ELSE
      RETURN PageLock.Mode.O
    END
  END Get;

PROCEDURE Put		(         self		:T;
                                  client	:ServedClient.T;
                                  lock		:PageLock.ServerMode) =
  VAR
    entry		:ServerLockEntry.T;
  BEGIN
    IF lock # PageLock.Mode.O THEN
      EVAL NARROW (self, Super.T).put (client, ServerLockEntry.T {lock});
    ELSE
      EVAL NARROW (self, Super.T).delete (client, entry);
    END
  END Put;

PROCEDURE Fmt		(         self		:T) :TEXT =
  VAR
    client		:ServedClient.T;
    entry		:ServerLockEntry.T;
    text		:TEXT;
    i			:Super.Iterator;
  BEGIN
    IF self.size () = 0 THEN
      text := "[<empty>]";
    ELSE
      i := NARROW (self, Super.T).iterate ();
      EVAL i.next (client, entry);
      text := "[\n (client = " &
                  client.getID () & ", " & ServerLockEntry.Fmt (entry) & ")";
      WHILE i.next (client, entry) DO
        text := text & ", \n (client = " &
                    client.getID () & ", " & ServerLockEntry.Fmt (entry) & ")";
      END;
      text := text & "]";
    END;

    RETURN text;
  END Fmt;

PROCEDURE Iterate	(         self		:T) :Iterator =
  BEGIN
    WITH i = NEW (Iterator) DO
      i.i := NARROW (self, Super.T).iterate ();
      
      RETURN i;
    END
  END Iterate;

PROCEDURE Next		(         self		:Iterator;
                         VAR      client	:ServedClient.T;
                         VAR      lock		:PageLock.ServerMode)
			:BOOLEAN =
  VAR
    entry		:ServerLockEntry.T;
    found		:BOOLEAN;
  BEGIN
    found := self.i.next (client, entry);
    lock := entry.lock;

    RETURN found;
  END Next;

BEGIN
END ServerLockTable.
