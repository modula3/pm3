MODULE Journal;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:27  hosking
    Initial revision

    Revision 1.1  1996/02/28 14:12:09  rbnix
    	First version of message journal (protocol) storage.

*)
(***************************************************************************)
(*
 | --- Journal -----------------------------------------------------------
 Currently journal messages are directed to the standard output.
 | ------------------------------------------------------------------------
 *)
IMPORT
  Thread, Stdio, Wr;


PROCEDURE Add			(         message	:TEXT) =
  <* FATAL Thread.Alerted, Wr.Failure *>
  BEGIN
    Wr.PutText (Stdio.stdout, message & "\n");
    Wr.Flush (Stdio.stdout);
  END Add; 


BEGIN
END Journal.
