(*!  DATA TYPE MODULE *)
INTERFACE Action;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:09  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

IMPORT Event, ContextSet;

TYPE
  Procedure = PROCEDURE (e       : Event.T;
                         context : ContextSet.T;
                         local   : BOOLEAN;
                         userdata: REFANY        );

TYPE
  T = OBJECT METHODS client (): CARDINAL END;

  Local <: T OBJECT
           METHODS
             init (proc: Procedure): Local;
             proc (): Procedure;
           END;

  Remote <: T OBJECT
            METHODS
              init (client, trig: CARDINAL): Remote;
              trig (): CARDINAL;
            END;
END Action.
