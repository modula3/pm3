MODULE CommunicationSeqSupport;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.2  1996/03/08 12:55:56  rbnix
    	Bug fixed: index of formated element was constant.

    Revision 1.1  1996/03/01 12:59:00  rbnix
    	New module to provide some support functions on communication
    	sequences added.

*)
(***************************************************************************)
(*
 | --- CommunicationSeqSupport --------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  CommunicationEntry, CommunicationSeq;


PROCEDURE Fmt		(         sequence	:CommunicationSeq.T;
                                  getFileName	:CommunicationEntry.GetFileName) 
			:TEXT =
  VAR
    text		:TEXT;
  BEGIN
    IF sequence.size () = 0 THEN
      text := "[<empty>]";

    ELSE
      text := "[";
      FOR i := 0 TO sequence.size ()-1 DO
        text :=
            text & "\n" & CommunicationEntry.Fmt (sequence.get (i), getFileName);
      END;
      text := text & "\n]";
    END;

    RETURN text;
  END Fmt;


BEGIN
END CommunicationSeqSupport.
