MODULE CommunicationEntry;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.3  1996/10/29 14:06:28  rbnix
    	New variable pageAge added.

    Revision 1.2  1996/03/06 16:39:13  rbnix
    	Bug fixed: in procedure Fmt.

    Revision 1.1  1996/03/01 12:57:56  rbnix
    	Format function added.

*)
(***************************************************************************)
(*
 | --- CommunicationEntry -------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT Fmt AS StdFmt;
IMPORT
  Page,
  PageLock;


PROCEDURE Fmt		(         entry		:T;
                                  getFileName	:GetFileName) :TEXT =

  PROCEDURE IsNIL	(         page		:Page.T) :TEXT =
    BEGIN
      IF page = NIL THEN
        RETURN "page = NIL"
      ELSE
        RETURN "page # NIL"
      END
    END IsNIL;

  (* Fmt *)
  BEGIN
    RETURN
      "(" & getFileName (entry.file) & ", " & StdFmt.Int (entry.pageNo) & ", " &
        StdFmt.Int (entry.pageAge) & ", " & PageLock.FmtMode (entry.lock) & ", " & 
        IsNIL (entry.page) & ")";
  END Fmt;


BEGIN
END CommunicationEntry.
