INTERFACE InternalPageCache;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:44  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

    Revision 1.1  1996/01/31 10:04:38  rbnix
    	Initial version for subsystem PageCache.

*)
(***************************************************************************)

(*
 * --- InternalPageCache --------------------------------------------------
 * This interface extends the public one for subsystem internal use only.
 *
 * It gives access to the page replacement strategy and should therefore
 * used only by PageHandle to view transparent swapping behaviour outside.
 *
 * The function AtomicAccess may be used to assert non atomic cache use.
 * ------------------------------------------------------------------------
 *)

IMPORT
  PageHandle;


<* PRAGMA SPEC *>

PROCEDURE LoadPage	(        handle		:PageHandle.T);
  <*
    SPEC
    REQUIRES cacheUser = CURRENT
  *>

PROCEDURE AssignPage	(        handle		:PageHandle.T);
  <*
    SPEC
    REQUIRES cacheUser = CURRENT
  *>

PROCEDURE RecognizeAccess (      handle		:PageHandle.T);
  <*
    SPEC
    REQUIRES cacheUser = CURRENT
  *>
    

END InternalPageCache.
