(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Tue May 24 09:09:59 PDT 1994 by najork   *)
(*      modified on Sat Jul 25 02:03:20 1992 by karsenty     *)

INTERFACE MFGraph;

(***************************************************************************)
(*   Author:  Solange Karsenty						   *)
(***************************************************************************)
(* A graph is an object represented as two lists of vertices and edges.
  Vertices and edges are both objects. They both carry some data (a
  REFANY) that can be used by other modules.

  From a vertex, one can access all the edges connected to it and also
  apply a procedure to all vertex neighbors.
  From an edge, one can access the vertices it connects (from, to). Edges
  are not oriented despite the identifiers.

  Apply methods allow browsing thought the vertices, the edges, or the neighbors
  of a given vertex.
  If the procedure that is passed to apply methods does deletions, it will do
  what you think, IF you delete only through the provided methods.

  Deletions methods are provided at the two levels: graph objects
  and vertices/edges objects. They are exactly the same.

  See test.m3 for as an example.
*)

(* $Revision$ *)
    
EXCEPTION
  ExitApply;
  MissingVertex;
  NoSuchVertexInGraph;
  NoSuchEdgeInGraph;

TYPE

  T <: TPublic;
  TPublic = OBJECT
    vertices: VertexList;
    edges: EdgeList
  METHODS
    init (): T;
	(* creates a vertex in the graph with data = u *)
    createVertex  (u: REFANY): Vertex ;
	(* delete first all edges connected to that vertex, then deletes
	   the vertex itself *)
    deleteVertex  (n: Vertex) RAISES {NoSuchVertexInGraph} ;
	(* create an edge connecting from and to, with data = u *)
    createEdge (from, to: Vertex; u: REFANY): Edge RAISES {MissingVertex};
	(* delete the edge from the graph. The edge is therefore deleted
	from the vertices' list of edges it belongs to *)
    deleteEdge  (edge: Edge)  RAISES {NoSuchEdgeInGraph};
	(* apply procedure p(arg) to each edge of the graph.
	The procedure can raise an exception ExitApply in order to
	exit from the loop. It returns the last edge that was visited, and
	NIL if all of them were visited *)
    edgeApply (p: Proc; arg: REFANY): Edge ;
	(* same for vertices *)
    vertexApply (p: Proc; arg: REFANY): Vertex ;
  END;

  Proc = PROCEDURE (obj: REFANY; arg: REFANY := NIL);

  Vertex <: TVertexPublic;
  TVertexPublic = OBJECT
    data: REFANY;
    edges: EdgeList;
    myGraph: T;
  METHODS
    init (graph: T): Vertex;
    delete () ;
    applyToNeighbors (p: Proc; arg: REFANY): Vertex;
  END;

  Edge <: TEdgePublic;
  TEdgePublic = OBJECT
    data: REFANY;
    from, to: Vertex;
    myGraph: T;
  METHODS
    init (graph: T): Edge;
	(* given one edge and one of the two edges it connects, returns
	the other vertex *)
    otherVertex (n: Vertex): Vertex ;
    delete () ;
  END;

  VertexList = REF VertexEl;
  VertexEl =  RECORD
    vertex: Vertex;
    next, prev: VertexList
  END;

  EdgeList = REF EdgeEl;
  EdgeEl = RECORD
    edge: Edge;
    next, prev: EdgeList 
  END;


PROCEDURE EdgeDelete(self: Edge) RAISES {};
(* same as DeleteEdge, but applied to an Edge *)

PROCEDURE VertexDelete(self: Vertex) RAISES {};
(* same as DeleteVertex, but applied to a vertex *)

PROCEDURE ApplyToNeighbors(self: Vertex; p: Proc; arg: REFANY): Vertex RAISES {};
(* apply p to all the vertex neighbors of self. p can raise ExitApply to exit 
   returns the last vertex that was visited, NIL if all of them were visited *)

END MFGraph.
