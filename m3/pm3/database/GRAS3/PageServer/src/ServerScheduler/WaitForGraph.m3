MODULE WaitForGraph;

(***************************************************************************)
(** Created by:  Reiner Nix						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:49  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:39  hosking
    Import of GRAS3 1.1

    Revision 1.3  1996/10/17 11:23:51  rbnix
    	New method deleteNode added.

    Revision 1.2  1996/08/06 16:33:05  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/11 12:16:58  rbnix
    	First version of abstract data type module representing a wait
    	for graph.

*)
(***************************************************************************)
(*
 | --- WaitForGraph -------------------------------------------------------
 
 | ------------------------------------------------------------------------
 *)
IMPORT
  ServedClient, ServedClientRefTransientTbl AS ServedClientRefTbl;


REVEAL
  T			= Public BRANDED OBJECT
      nodes		:ServedClientRefTbl.T;

    OVERRIDES
      init		:= Init;
      getNode		:= GetNode;
      insertNode	:= InsertNode;
      deleteNode	:= DeleteNode;
      insertEdge	:= InsertEdge;
      iterateNodes	:= IterateNodes;
      iterateNeighbours	:= IterateNeighbours;
    END;


  Node                  = PublicNode BRANDED OBJECT
      client		:ServedClient.T;
      weight		:CARDINAL;
      neighbours	:ServedClientRefTbl.T;

    METHODS
      init		(         client	:ServedClient.T) :Node
			:= InitNode;

    OVERRIDES
      setWeight		:= SetWeight;
      getWeight		:= GetWeight;
      getClient		:= GetClient;
    END;


  NodeIterator          = PublicNodeIterator BRANDED OBJECT
      i			:ServedClientRefTbl.Iterator;

    OVERRIDES
      next		:= Next;
    END;


PROCEDURE Init		(         self		:T)
			:T =
  BEGIN
    self.nodes := NEW (ServedClientRefTbl.Default).init ();

    RETURN self
  END Init;


PROCEDURE GetNode	(         self		:T;
                                  client	:ServedClient.T)
			:Node =
  VAR
    node		:<*TRANSIENT*> REFANY;
  BEGIN
    IF self.nodes.get (client, node) THEN
      RETURN node;
    ELSE
      RETURN NIL;
    END;
  END GetNode;


PROCEDURE InsertNode	(         self		:T;
                                  client	:ServedClient.T)
			:Node =
  VAR
    node		:Node;
  BEGIN
    node := NEW (Node).init (client);
    EVAL self.nodes.put (client, node);

    RETURN node;
  END InsertNode;


PROCEDURE DeleteNode	(         self		:T;
                                  client	:ServedClient.T) =
  VAR
    i			:NodeIterator;
    node		:<*TRANSIENT*> REFANY;
    found		:BOOLEAN;
    otherNode		:Node;
    otherClient		:ServedClient.T;
  BEGIN
    (* delete node itself *)
    found := self.nodes.delete (client, node);
    <* ASSERT (found) *>

    (* delete all references to node *)
    i := self.iterateNodes ();
    WHILE i.next (otherNode, otherClient) DO
      (* try to delete reference, it may not exists *)
      EVAL otherNode.neighbours.delete (client, node);
    END;
  END DeleteNode;


PROCEDURE InsertEdge	(         <* UNUSED *>
				  self          :T;
                                  source,
                                  target	:Node) =
  BEGIN
    EVAL source.neighbours.put (target.getClient (), target);
  END InsertEdge;


PROCEDURE IterateNodes	(        self		:T)
			:NodeIterator =
  VAR
    i			:NodeIterator;
  BEGIN
    i := NEW (NodeIterator);
    i.i := self.nodes.iterate ();

    RETURN i;
  END IterateNodes;
                                 

PROCEDURE IterateNeighbours (    <* UNUSED *>
				 self		:T;
                                 source		:Node)
			    :NodeIterator =
  VAR
    i			:NodeIterator;
  BEGIN
    i := NEW (NodeIterator);
    i.i := source.neighbours.iterate ();

    RETURN i;
  END IterateNeighbours;


PROCEDURE InitNode	(         self		:Node;
                                  client	:ServedClient.T)
			:Node =
  BEGIN
    self.client := client;
    self.weight := 0;
    self.neighbours := NEW (ServedClientRefTbl.Default).init ();

    RETURN self;
  END InitNode;


PROCEDURE SetWeight	(         self		:Node;
                                  weight	:CARDINAL) =
  BEGIN
    self.weight := weight;
  END SetWeight;


PROCEDURE GetWeight	(         self		:Node)
			:CARDINAL =
  BEGIN
    RETURN self.weight;
  END GetWeight;


PROCEDURE GetClient	(         self		:Node)
			:ServedClient.T =
  BEGIN
    RETURN self.client;
  END GetClient;


PROCEDURE Next		(         self		:NodeIterator;
                         VAR      node		:Node;
                         VAR      client	:ServedClient.T)
			:BOOLEAN =
  VAR
    ref			:<*TRANSIENT*> REFANY;
    found		:BOOLEAN;
  BEGIN
    found := self.i.next (client, ref);
    node := NARROW (ref, Node);

    RETURN found;
  END Next;


BEGIN
END WaitForGraph.
