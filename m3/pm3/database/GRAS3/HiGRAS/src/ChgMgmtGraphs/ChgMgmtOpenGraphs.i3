INTERFACE ChgMgmtOpenGraphs;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:29  hosking
    Initial revision

    Revision 1.1  1998/05/19 10:30:00  roland
    Support for log-groups implemented.

*)
(***************************************************************************)

IMPORT PersistentGraph, Pathname;

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init(): T;
      createEntry (number : CARDINAL;
                   graph  : PersistentGraph.T;
                   vname  : Pathname.T;
                   wname  : Pathname.T;
                   counter: CARDINAL           );
      free           (graph: CARDINAL);
      exists         (graph: CARDINAL): BOOLEAN;
      putLogHandle   (graph: CARDINAL; log: CARDINAL);
      putOpenCounter (graph: CARDINAL; count: CARDINAL);
      getLogHandle   (graph: CARDINAL): CARDINAL;
      getVisibleName (graph: CARDINAL): Pathname.T;
      getGraph       (graph: CARDINAL): PersistentGraph.T RAISES {NotOpen};
      getWorkingName (graph: CARDINAL): Pathname.T;
      getOpenCounter (graph: CARDINAL): CARDINAL;
    END;

EXCEPTION NotOpen;
  
END ChgMgmtOpenGraphs.
