INTERFACE CommunicationSeqSupport;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/03/01 12:58:58  rbnix
    	New module to provide some support functions on communication
    	sequences added.

*)
(***************************************************************************)
(*
 | --- CommunicationSeqSupport --------------------------------------------
 This functional module provide some support functions on communication
 sequences.
 | ------------------------------------------------------------------------
 *)
IMPORT
  CommunicationEntry, CommunicationSeq;


PROCEDURE Fmt		(         sequence	:CommunicationSeq.T;
                                  getFileName	:CommunicationEntry.GetFileName) 
			:TEXT;


END CommunicationSeqSupport.
