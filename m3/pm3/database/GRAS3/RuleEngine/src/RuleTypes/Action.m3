MODULE Action;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:06:12  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling. RuleTypes and EventTypes
    subsystems implement basic types of the rule engine.

*)
(***************************************************************************)

TYPE
  LocalP = T OBJECT
           METHODS
             init (proc: Procedure): Local;
             proc (): Procedure;
           END;

REVEAL
  Local = LocalP BRANDED OBJECT
            procedure: Procedure;
          OVERRIDES
            init   := LocalInit;
            proc   := LocalProc;
            client := LocalClient;
          END;

PROCEDURE LocalInit (l: Local; proc: Procedure): Local =
  BEGIN
    l.procedure := proc;
    RETURN l;
  END LocalInit;

PROCEDURE LocalProc (l: Local): Procedure =
  BEGIN
    RETURN l.procedure;
  END LocalProc;


PROCEDURE LocalClient (<* UNUSED *> l: Local): CARDINAL =
  BEGIN
    RETURN 0;
  END LocalClient;

TYPE
  RemoteP = T OBJECT
            METHODS
              init (client, trig: CARDINAL): Remote;
              trig (): CARDINAL;
            END;

REVEAL
  Remote = RemoteP BRANDED OBJECT
             clientno, trigger: CARDINAL;
           OVERRIDES
             init   := RemoteInit;
             client := RemoteClient;
             trig   := RemoteTrig;
           END;

PROCEDURE RemoteInit (r: Remote; client, trig: CARDINAL): Remote =
  BEGIN
    r.clientno := client;
    r.trigger := trig;
    RETURN r;
  END RemoteInit;

PROCEDURE RemoteClient (r: Remote): CARDINAL =
  BEGIN
    RETURN r.clientno;
  END RemoteClient;

PROCEDURE RemoteTrig (r: Remote): CARDINAL =
  BEGIN
    RETURN r.trigger;
  END RemoteTrig; 

BEGIN
END Action.
