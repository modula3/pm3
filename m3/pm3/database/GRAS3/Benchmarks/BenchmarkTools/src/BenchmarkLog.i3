INTERFACE BenchmarkLog;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.2  1997/02/20 16:08:15  roland
    OO1 rewritten with graphical user interface.

    Revision 1.1  1996/10/08 10:37:31  roland
    Tools for benchmarking. Currently only used by OO1.

*)
(***************************************************************************)

IMPORT Wr, Thread;

TYPE
  T <: Public;

  Public = OBJECT
    METHODS
      open(wr: Wr.T): T;
      openForN(READONLY wrs: ARRAY OF Wr.T): T;
      addWriter(wr: Wr.T);
      removeWriter(wr: Wr.T);
      close();
      
      write (t: TEXT) RAISES {Thread.Alerted};
      writeTime () RAISES {Thread.Alerted};
      writeEvent (descr: TEXT) RAISES {Thread.Alerted};
    END;
  
END BenchmarkLog.
