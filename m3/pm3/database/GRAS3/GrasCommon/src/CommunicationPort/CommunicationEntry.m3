MODULE CommunicationEntry;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:43  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

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
  Word,
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


PROCEDURE Equal		(	  a, b		:T) :BOOLEAN =
  BEGIN
    RETURN (a.file = b.file) AND (a.pageNo = b.pageNo);
  END Equal;


PROCEDURE Hash		(	  k		:T) :Word.T =
  BEGIN
    RETURN k.pageNo;
  END Hash;


BEGIN
END CommunicationEntry.
