MODULE VirtualRemotePage;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

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


PROCEDURE ReadAccess	(         self		:T)
			:PageHandle.T 
			RAISES {Access.Locked, VirtualPage.FatalError} =
  BEGIN
    TRY
      RETURN self.scheduledPage.readAccess ();
    EXCEPT
      ScheduledClientPage.FatalError(info) =>
      RAISE VirtualPage.FatalError(ErrorSupport.Propagate(
                           "VirtualRemotePage.ReadAccess",
                           "ScheduledClientPage.FatalError", info));
    END;
  END ReadAccess;
  

PROCEDURE WriteAccess	(         self		:T)
			:PageHandle.T 
			RAISES {Access.Locked, VirtualPage.FatalError} =
  BEGIN
    TRY
      RETURN self.scheduledPage.writeAccess ();
    EXCEPT
      ScheduledClientPage.FatalError(info) =>
      RAISE VirtualPage.FatalError(ErrorSupport.Propagate(
                           "VirtualRemotePage.WriteAccess",
                           "ScheduledClientPage.FatalError", info));
    END;
  END WriteAccess;
  

BEGIN
END VirtualRemotePage.
