INTERFACE CommunicationEntry;

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

    Revision 1.3  1996/10/29 14:06:25  rbnix
    	New variable pageAge added.

    Revision 1.2  1996/03/01 12:57:55  rbnix
    	Format function added.

    Revision 1.1  1996/02/09 16:42:56  rbnix
    	First version of specification layer for network objects
    	added.

*)
(***************************************************************************)

(*
 | --- CommunicationEntry -------------------------------------------------
 This type collection module presents a CommunicationEntry to describe the
 communication between server and client about a page. The elements file
 and pageNo determines the page to be manipulated. The element lock marks
 the future lock mode. Page data is transfered only if neccessary otherwise
 page is NIL.
 | ------------------------------------------------------------------------
 *)
IMPORT
  Page,
  PageLock,
  RemoteFile,
  Word;


CONST
  Brand			= "CommunicationEntry";


TYPE
  T			= RECORD
    file		:RemoteFile.T;
    pageNo		:CARDINAL;
    pageAge		:CARDINAL;
    lock		:PageLock.ServerMode;
    page		:Page.T;
  END;

  GetFileName		=PROCEDURE (file :RemoteFile.T) :TEXT;
  

PROCEDURE Fmt		(         entry		:T;
                                  getFileName	:GetFileName) :TEXT;

PROCEDURE Equal		(	  k1, k2	:T) :BOOLEAN;

PROCEDURE Hash		(	  k		:T) :Word.T;


END CommunicationEntry.
