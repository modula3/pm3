INTERFACE Perfmeter;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.1  1996/10/08 10:37:35  roland
    Tools for benchmarking. Currently only used by OO1.

*)
(***************************************************************************)

IMPORT Process, OSError, Pathname;

TYPE
  Quantity = {Cpu,               (* Percent of CPU being utilized.*)
              Pkts,              (* Ethernet packets per second.*)
              Page,              (* Paging activity in pages per second.*)
              Swap,              (* Jobs swapped per second.*)
              Intr,              (* Number of device interrupts per
                                    second.*)
              Disk,              (* Disk traffic in transfers per second.*)
              Cntxt,             (* Number of context switches per
                                    second.*)
              Load,              (* Average number of runnable processes
                                    over the last minute. *)
              Colls,             (* Collisions per second detected on the
                                    ethernet.*)
              Errs               (* Errors per second on receiving
                                    packets.*)
             };

  QuantitySet = SET OF Quantity;


CONST
  DefaultQuantities = QuantitySet{
                        Quantity.Cpu, Quantity.Swap, Quantity.Page,
                        Quantity.Intr, Quantity.Disk};

  NullParams = ARRAY [0 .. -1] OF TEXT{};

PROCEDURE Start (         what     : QuantitySet   := DefaultQuantities;
                          logToFile: BOOLEAN       := FALSE;
                          fileName : Pathname.T    := NIL;
                 READONLY params   : ARRAY OF TEXT := NullParams         ):
  Process.T RAISES {OSError.E};
  (* Starts a perfmeter process that measures the quantities given by
     parameter 'what'.  If logToFile is TRUE, the process will write its
     measures to a file named 'fileName' or a default name chosen by
     perfmeter.  Additional parameters might be supplied with 'params'.
     (see man-page for perfmeter for additional parameters. *)

PROCEDURE Stop (p: Process.T);
  (* Sends a Quit-Signal to p and waits until p terminates. *)

END Perfmeter.
