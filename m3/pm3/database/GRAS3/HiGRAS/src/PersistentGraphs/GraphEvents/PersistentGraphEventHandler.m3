MODULE PersistentGraphEventHandler;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.1  1997/11/12 15:24:05  roland
    Specialized event handler subsystem for PersistentGraphs
    introduced. Events on edges and attributes depend on their nodes, so
    that events might not be delivered, when their nodes are deleted in
    the meantime.

*)
(***************************************************************************)

IMPORT RuleEngine, GraphEventHandler, GraphTriggerStorage,
       GraphEvents, CardSeq, PrivateGraphEvents;

IMPORT Node, Transaction;

VAR Handler: GraphEventHandler.T := NIL;

PROCEDURE Install () =
  VAR graphEvents: CardSeq.T;
      ts: GraphTriggerStorage.T;
  BEGIN
    IF Handler = NIL THEN
      graphEvents := NEW(CardSeq.T).init();
      FOR o := FIRST(GraphEvents.Operation) TO LAST(GraphEvents.Operation) DO
	graphEvents.addhi(PrivateGraphEvents.TypeNumber[o]);
      END;
      ts := NEW(GraphTriggerStorage.T).init();
      Handler := NEW(GraphEventHandler.T).init(ts);
      RuleEngine.RegisterEventHandler(Handler, graphEvents);
    END;
  END Install;

PROCEDURE NotifyNodeDeletion (unit: CARDINAL;
                              level: Transaction.Level;
                              node: Node.T) =
  BEGIN
    Handler.notifyNodeDeletion(unit, level, node);
  END NotifyNodeDeletion;

BEGIN
END PersistentGraphEventHandler.
