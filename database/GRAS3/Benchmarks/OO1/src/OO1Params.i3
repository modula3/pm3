INTERFACE OO1Params;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.2  1998/03/17 14:13:33  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.1  1997/02/20 16:08:52  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT OO1, Pathname;

CONST
  MinParts = 500;
  MaxParts = 2000000;

  (* The parameters that determine what OO1 has to do *)
VAR
  (* On which of the GRAS gras types *)
  graphtype: OO1.GraphType := OO1.GraphType.Persistent;

  (*--- define default open mode ---*)
  local : BOOLEAN := FALSE;

  (* How many parts *)
  N        : CARDINAL      := MinParts * 10; (* Size of database *)

  (* Attributed or simple connections *)
  SimpleConnects: BOOLEAN := FALSE;

  (* Many small transactions (slow) or few large (quicker) *)
  Quick         : BOOLEAN := FALSE;

  (* which parts of the original OO1 benchmark must be executed *)
  Performing := OO1.Suite{OO1.Benchmark.Load.. OO1.Benchmark.Insert};

  (* Use a performance meter *)
  PerfMeter: BOOLEAN       := FALSE;

  (* file name for benchmark log *)
  logToFile: BOOLEAN := FALSE;
  logfile: Pathname.T := NIL;

END OO1Params.
