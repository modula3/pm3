INTERFACE DaVinci;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1998/08/06 10:48:30  roland
    A Modula-3 interface to the graph display tool daVinci.

*)
(***************************************************************************)

(* A DaVinci.T is connected to a daVinci process and is able to communicate
   with the daVinci process via pipes (this is the daVinci API). *)

TYPE
  MsgType = {Ok, Quit, Close, Error, NodeSelection, NodeDoubleClick,
             EdgeSelction, EdgeDoubleCick, MenuSelection, IconSelection,
             Context, TclAnswer, BrowserAnswer, Disconnect, Unknown};

  EventHandler = OBJECT METHODS notify (type: MsgType; msg: TEXT); END;
    (* An EventHandler can receive events sent from the daVinci process.
       The msgType corresponds to the msg text sent from daVinci.  The text
       is left unchanged by this module. *)


  T <: Public;

  Public =
    OBJECT
    METHODS
      init (dvhome: TEXT := NIL): T RAISES {Error};
            (* Fork a daVinci process.  If dvhome is non-nil, it will be
               used as the value for the environment variable DAVINCIHOME,
               which must be set for running daVinci. *)

      quit ();
            (* Shutdown communication and kill the daVinci process. *)

      send (msg: TEXT) RAISES {Error};
            (* Send msg to the daVinci process.  Answers from the process
               can be received only with an EventHandler. *)

      registerHandler   (type: MsgType; handler: EventHandler);
      unregisterHandler (type: MsgType; handler: EventHandler);
    END;


EXCEPTION Error(TEXT);

END DaVinci.
