INTERFACE BenchmarkSupport;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.1  1996/10/08 10:37:33  roland
    Tools for benchmarking. Currently only used by OO1.

*)
(***************************************************************************)

IMPORT Time, Date;

(*
 | --- conversion routines ------------------------------------------------
 *)
PROCEDURE TimeToString (t: Time.T): TEXT RAISES {};

PROCEDURE DateToString (d: Date.T): TEXT;
(*
 | --- support routines ---------------------------------------------------
 *)
PROCEDURE Interval (t: Time.T): Time.T RAISES {};

  
END BenchmarkSupport.
