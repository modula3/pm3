INTERFACE RuleServer;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:37  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:16:40  roland
    RuleServer implements communication between different rule engines.

*)
(***************************************************************************)

PROCEDURE Setup(host, id: TEXT) RAISES {Failure};
PROCEDURE Shutdown();

EXCEPTION Failure(TEXT);
  
END RuleServer.
