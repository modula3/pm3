MODULE DeadlockDetection;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:38  hosking
    Initial revision

    Revision 1.6  1996/10/17 11:25:40  rbnix
    	Deadlock test repeated to check for multiple cycles at the
    	same time added.

    Revision 1.5  1996/10/02 14:51:16  rbnix
    	Handling of killed clients improved: they are now ignored
    	building the wait for graph.

    Revision 1.4  1996/09/26 14:30:55  rbnix
    	Bug fixed in BuildGraph: dependencies check for waiting
    	clients adjusted.

    	Bug fixed in DetectDeadlock: to distuingish between allready
    	visited nodes at all and nodes still to be visited to get the
    	closure there a new set visitedNodes is used.

    	Sleep interval for deadlock test shrinked.

    Revision 1.3  1996/08/22 14:13:02  rbnix
    	Bug fixed: deadlock test extended to recognize conflicting
    	X locks.

    	Bug fixed: current start node of component wasn't set.

    	Additional internal tests added.

    Revision 1.2  1996/08/06 16:32:44  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/11 12:30:51  rbnix
    	First version of functional module to detect deadlocks. The
    	main function of this module works in a separate thread.

*)
(***************************************************************************)
(*
 | --- DeadlockDetection---------------------------------------------------
  
 | ------------------------------------------------------------------------
 *)
IMPORT
  Thread, 
  Variant, Journal,
  PageCache,
  PageLock,
  ServedClient,
  ServerLockTable, ServerWaitEntry, ServerWaitTbl, WaitForGraph;
IMPORT RefSetList AS RefSet;  


TYPE
  WaitClosure		= Thread.Closure OBJECT
    waitingClients	:ServerWaitTbl.T
  END;

  
(*
 | --- DetectDeadlock------------------------------------------------------
 This procedure never ends. It performs periodically a deadlock test
 building a wait-for-graph and checking for cycles. If a deadlock is
 detected the youngest client receives an alert to abort his request.
 Further the test will be repeatet on the resting clients to check for
 multiple deadlocks at the same time. This prevents continous aborts on the
 youngest transaction while still unresolved deadlocks on older
 transactions exists. This makes the algorithm more fair.
 | ------------------------------------------------------------------------
 *)
PROCEDURE DetectDeadlock(       self		:WaitClosure) :REFANY =
  VAR
    waitForGraph	:WaitForGraph.T;
    hangingClient	:ServedClient.T;
    hangingEntry	:ServerWaitEntry.T;


  PROCEDURE BuildGraph  (	waitingClients	:ServerWaitTbl.T)
			:WaitForGraph.T =
    VAR
      graph		:WaitForGraph.T;
      i			:ServerWaitTbl.Iterator;
      client		:ServedClient.T;
      entry		:ServerWaitEntry.T;
      j			:ServerLockTable.Iterator;
      lock		:PageLock.ServerMode;
      source, target	:WaitForGraph.Node;
    BEGIN
      graph := NEW (WaitForGraph.T).init ();

      IF Variant.TestServerScheduler THEN
        Journal.Add ("DetectDeadlock.BuildGraph: ");
        i := waitingClients.iterate ();
        WHILE i.next (client, entry) DO
          IF NOT client.isKilled () THEN
            Journal.Add ("  " & ServerWaitEntry.Fmt (entry));
          END;
        END;
      END;

      i := waitingClients.iterate ();
      WHILE i.next (client, entry) DO
        IF NOT client.isKilled () THEN
          source := graph.getNode (client);
          IF source = NIL THEN
            source := graph.insertNode (client);
          END;
          source.setWeight (entry.age);

          j := entry.otherClients.iterate ();
          WHILE j.next (client, lock) DO
            IF (entry.waitingClient # client) AND
               (((entry.lock = PageLock.Mode.C) AND (lock = PageLock.Mode.X)) OR
               ((entry.lock = PageLock.Mode.X) AND
              ((lock = PageLock.Mode.C) OR (lock = PageLock.Mode.X)))) THEN
              target := graph.getNode (client);
              IF target = NIL THEN
                target := graph.insertNode (client);
              END;
              graph.insertEdge (source, target);
            END;
          END;
        END;
      END;

      RETURN graph;
    END BuildGraph;

    
  (*
    closureNodes = nodes in a closure that still must be visited
    visitedNode  = nodes allready used as starting nodes
  *)
  PROCEDURE CycleTest	(	graph		:WaitForGraph.T)
			:ServedClient.T =
    VAR
      startNodes,
      closureNodes,
      visitedNodes	:RefSet.T;
      i			:WaitForGraph.NodeIterator;
      client		:ServedClient.T;
      heavyNode,
      start, node	:WaitForGraph.Node;
      j			:RefSet.Iterator;
      ref		:REFANY;
      found		:BOOLEAN;
    BEGIN
      (* collect all nodes as potentially starting points *)
      startNodes := NEW (RefSet.T);
      i := graph.iterateNodes ();
      WHILE i.next (node, client) DO
        EVAL startNodes.insert (node);
      END;

      WHILE NOT startNodes.isEmpty () DO
        (* select one node to start *)
        j := startNodes.iterate ();
        found := j.next (ref);
        <* ASSERT (found) *>
        start := ref;

        found := startNodes.delete (start);
        <* ASSERT (found) *>

        heavyNode := start;
        closureNodes := NEW (RefSet.T);
        visitedNodes := NEW (RefSet.T);
        
        REPEAT
          (* visit all immediate neighbours *)
          i := graph.iterateNeighbours (start);
          start := NIL;
          WHILE i.next (node, client) DO
            IF visitedNodes.member (node) THEN
              RETURN heavyNode.getClient ();
            ELSE
              IF heavyNode.getWeight () < node.getWeight () THEN
                heavyNode := node;
              END;

              found := visitedNodes.insert (node);
              <* ASSERT (NOT found) *>
              found := closureNodes.insert (node);
              <* ASSERT (NOT found) *>
              EVAL startNodes.delete (node);
            END;
          END;

          IF (0 < closureNodes.size ()) THEN
            j := closureNodes.iterate ();
            found := j.next (ref);
            <* ASSERT (found) *>
            start := ref;
            EVAL closureNodes.delete (start);
          END;
        UNTIL (start = NIL);
      END;

      (* no deadlock found *)
      RETURN NIL;
    END CycleTest;

  (* DetectDeadlock *)
  BEGIN
    LOOP
      PageCache.BeginAccess ();
      waitForGraph := BuildGraph (self.waitingClients);

      REPEAT
        hangingClient := CycleTest (waitForGraph);
        IF hangingClient # NIL THEN
          EVAL self.waitingClients.get (hangingClient, hangingEntry);

          IF Variant.TestServerScheduler THEN
            Journal.Add ("Deadlock detected. Aborting request: " &
                         ServerWaitEntry.Fmt (hangingEntry));
          END;

          (* alert this client to reject access *)
          Thread.Alert (hangingEntry.waitingThread);

          waitForGraph.deleteNode (hangingClient);
        END;
      UNTIL hangingClient = NIL;
      PageCache.EndAccess ();

      (* wait for a while and check again *)
      Thread.Pause(5.0d0);
    END;
  END DetectDeadlock;


PROCEDURE StartChecks	(         waitingClients :ServerWaitTbl.T) =
  BEGIN
    IF Variant.RegularServerJournal THEN
      Journal.Add ("Starting thread performing deadlock detection");
    END;

    EVAL Thread.Fork (NEW (WaitClosure,
                           apply := DetectDeadlock,
                           waitingClients := waitingClients));
  END StartChecks;  


BEGIN
END DeadlockDetection.
