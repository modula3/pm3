MODULE VirtualRemotePage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:48  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:37  hosking
    Import of GRAS3 1.1

    Revision 1.2  1996/11/18 17:52:24  roland
    ASSERTs and FATALs (mostly) replaced by exception handling.

    Revision 1.1  1996/02/29 17:44:30  rbnix
    	First version of subsystem VirtualPages giving transparent
    	access to local/remote files/pages.

*)
(***************************************************************************)
(*
 | --- VirtualRemotePage --------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  PageHandle,
  Access,
  ScheduledClientPage,
  InternalVirtualPage, VirtualPage,
  ErrorSupport;


REVEAL
  T                     = Public BRANDED OBJECT
      scheduledPage	:ScheduledClientPage.T;

    OVERRIDES
      init		:= Init;
      peekAccess        := PeekAccess;
      readAccess	:= ReadAccess;
      writeAccess	:= WriteAccess;
    END;


PROCEDURE Init		(         self		:T;
                                  scheduledPage	:ScheduledClientPage.T)
			:T =
  BEGIN
    self.scheduledPage := scheduledPage;

    RETURN self;
  END Init;


PROCEDURE PeekAccess	(         self		:T)
			:PageHandle.T 
			RAISES {VirtualPage.FatalError} =
  BEGIN
    TRY
      RETURN self.scheduledPage.peekAccess ();
    EXCEPT
      ScheduledClientPage.FatalError(info) =>
      RAISE VirtualPage.FatalError(ErrorSupport.Propagate(
                           "VirtualRemotePage.PeekAccess",
                           "ScheduledClientPage.FatalError", info));
    END;
  END PeekAccess;
  

PROCEDURE ReadAccess	(         self		:T;
                              VAR pageAge       :CARDINAL)
			:PageHandle.T 
			RAISES {Access.Locked, VirtualPage.FatalError} =
  BEGIN
    TRY
      RETURN self.scheduledPage.readAccess (pageAge);
    EXCEPT
      ScheduledClientPage.FatalError(info) =>
      RAISE VirtualPage.FatalError(ErrorSupport.Propagate(
                           "VirtualRemotePage.ReadAccess",
                           "ScheduledClientPage.FatalError", info));
    END;
  END ReadAccess;
  

PROCEDURE WriteAccess	(         self		:T;
                              VAR pageAge       :CARDINAL)
			:PageHandle.T 
			RAISES {Access.Locked, VirtualPage.FatalError} =
  BEGIN
    TRY
      RETURN self.scheduledPage.writeAccess (pageAge);
    EXCEPT
      ScheduledClientPage.FatalError(info) =>
      RAISE VirtualPage.FatalError(ErrorSupport.Propagate(
                           "VirtualRemotePage.WriteAccess",
                           "ScheduledClientPage.FatalError", info));
    END;
  END WriteAccess;


BEGIN
END VirtualRemotePage.
