MODULE RuleEngineServer;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:28  hosking
    Initial revision

    Revision 1.1  1997/10/31 14:09:28  roland
    Communication interfaces for the rule engine.

*)
(***************************************************************************)

PROCEDURE ComposeServerId(id: TEXT): TEXT =
  BEGIN
    RETURN "RuleServer " & id;
  END ComposeServerId;

BEGIN
END RuleEngineServer.
