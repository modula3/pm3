INTERFACE DaVinciGraph;

(***************************************************************************)
(** Created by:  Markus Kluck						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.1  1998/09/03 11:07:31  kluck
    Further modules used by PoolView to implement selection.

*)
(***************************************************************************)

IMPORT VirtualResource, Names;
IMPORT DaVinci;


PROCEDURE IconSelection (<*UNUSED*> handler: DaVinci.EventHandler;
                         <*UNUSED*> type   : DaVinci.MsgType;
                         <*UNUSED*> msg    : TEXT                  );

PROCEDURE MsgPrinter (<* UNUSED *> handler: DaVinci.EventHandler;
                      <* UNUSED *> type   : DaVinci.MsgType;
                                   msg    : TEXT                  );

PROCEDURE View (res   : VirtualResource.T;
                names : Names.T;
                viewer: DaVinci.T;
                local : BOOLEAN             := FALSE;
                debug : BOOLEAN             := FALSE  )
  RAISES {DaVinci.Error};

END DaVinciGraph.
