INTERFACE WaitForGraph;

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

    Revision 1.3  1996/10/17 11:23:50  rbnix
    	New method deleteNode added.

    Revision 1.2  1996/08/06 16:33:04  roland
    Merge of PAGESERVER and main branch.

    Revision 1.1.2.1  1996/07/11 12:16:56  rbnix
    	First version of abstract data type module representing a wait
    	for graph.

*)
(***************************************************************************)
(*
 | --- WaitForGraph -------------------------------------------------------
 
 | ------------------------------------------------------------------------
 *)
IMPORT
  ServedClient;


TYPE
  T			<: Public;
  
  Public		= OBJECT
    METHODS
      init		() :T;

      getNode		(         client	:ServedClient.T)
			:Node;

      insertNode	(         client	:ServedClient.T)
			:Node;

      deleteNode	(         client	:ServedClient.T);

      insertEdge	(         source,
                                  target	:Node);

      iterateNodes	()
			:NodeIterator;
      
      iterateNeighbours	(         source	:Node)
			:NodeIterator;
    END;

  
  Node			<: PublicNode;

  PublicNode		= <*TRANSIENT*> ROOT OBJECT
    METHODS
      setWeight		(         weight	:CARDINAL);
      getWeight		() :CARDINAL;
      getClient		() :ServedClient.T;
    END;


  NodeIterator		<: PublicNodeIterator;

  PublicNodeIterator	= OBJECT
    METHODS
      next		(VAR      node		:Node;
                         VAR      client	:ServedClient.T)
			:BOOLEAN;
    END;


END WaitForGraph.
