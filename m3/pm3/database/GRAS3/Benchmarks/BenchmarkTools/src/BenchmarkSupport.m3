MODULE BenchmarkSupport;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.1  1996/10/08 10:37:34  roland
    Tools for benchmarking. Currently only used by OO1.

*)
(***************************************************************************)

IMPORT Fmt, Date, Time;

(*
 | --- conversion routines ------------------------------------------------
 *)
PROCEDURE TimeToString (t: Time.T): TEXT RAISES {} =
  BEGIN
    RETURN Fmt.LongReal(t, Fmt.Style.Fix, prec := 2);
  END TimeToString;

PROCEDURE DateToString (d: Date.T): TEXT =
  BEGIN
    RETURN
      Fmt.Int(d.year) & "." & Fmt.Pad(Fmt.Int(ORD(d.month) + 1), 2, '0')
        & "." & Fmt.Pad(Fmt.Int(d.day), 2, '0') & "."
        & Fmt.Pad(Fmt.Int(d.hour), 2, '0') & "."
        & Fmt.Pad(Fmt.Int(d.minute), 2, '0')
  END DateToString;

(*
 | --- support routines ---------------------------------------------------
 *)
PROCEDURE Interval (t: Time.T): Time.T RAISES {} =
  VAR nt: Time.T := Time.Now();
  BEGIN
    nt := nt - t;
    RETURN nt;
  END Interval;

BEGIN
END BenchmarkSupport.
