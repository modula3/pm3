INTERFACE Variant;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.8  1998/08/27 12:30:44  roland
    Included simple mechanism to monitor log size.

    Revision 1.7  1997/10/31 14:10:27  roland
    New variant for rule engine test.

    Revision 1.6  1997/07/21 10:33:24  roland
    Varaint for free memory list logging introduced. Values > 0: Free
    memory lists size is printed every time it reaches a new maximum which
    is a multiple of Variant.FreeMemoryListLog.

    Revision 1.5  1997/02/20 16:11:17  roland
    New variant to trace split/merge of data pages.

    Revision 1.4  1997/02/06 13:42:33  roland
    New variant for attribute cache statistics.

    Revision 1.3  1996/09/20 14:01:35  roland
    New Variant for test of change management.

    Revision 1.2  1996/03/06 09:15:56  rbnix
    	New variant definitions RegularClientJournal, TestClientScheduler,
    	TestClientCommunication added.

    Revision 1.1  1996/02/28 14:10:04  rbnix
    	First version of interface determining program variants.

*)
(***************************************************************************)

(*
 | --- Variant ------------------------------------------------------------
 This module exports boolean constants provided to make several variants of
 a program.

 The constants should be initialized for normal use.
 | ------------------------------------------------------------------------
 *)

CONST
(*
 | --- server variants ----------------------------------------------------
 *)
  RegularServerJournal		= TRUE;

  TestServerScheduler		= FALSE;
  TestServerCommunication	= FALSE;
  TestRuleServer                = FALSE;


(*
 | --- client variants ----------------------------------------------------
 *)
  RegularClientJournal		= TRUE;

  TestClientScheduler		= FALSE;
  TestClientCommunication	= FALSE;

  LogSplitAndMerge              = FALSE;

  TestChgMgmt                   = FALSE;
  LogCount                      = FALSE;
  TestAttributeCache            = FALSE;

  FreeMemoryListLog             = 0;
END Variant.
