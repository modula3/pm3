(************************************************************************)
(**									*)
(**   GRAS - A Graph-Oriented Database System for SE Applications	*)
(**   Copyright (C) 1987-1992  Lehrstuhl Informatik III, RWTH Aachen	*)
(**                                                                     *)
(**   This library is free software; you can redistribute it and/or	*)
(**   modify it under the terms of the GNU Library General Public	*)
(**   License as published by the Free Software Foundation; either	*)
(**   version 2 of the License, or (at your option) any later version.	*)
(**    								        *)
(**   This library is distributed in the hope that it will be useful,	*)
(**   but WITHOUT ANY WARRANTY; without even the implied warranty of	*)
(**   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(**   Library General Public License for more details.			*)
(**    								        *)
(**   You should have received a copy of the GNU Library General Public *)
(**   License along with this library; if not, write to the Free	*)
(**   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.*)
(**    								        *)
(**   Contact Adresses:						        *)
(**    								        *)
(**    Roland Baumann,                                                  *)
(**    Lehrstuhl f"ur Informatik III,					*)
(**    University of Technology Aachen (RWTH Aachen),			*)
(**    Ahornstr. 55,							*)
(**    D-52074 Aachen							*)
(**    								        *)
(**   Email to								*)
(**    							 	        *)
(**	roland@i3.informatik.rwth-aachen.de				*)
(**									*)
(************************************************************************)

MODULE Simple1 EXPORTS Main;

IMPORT PersistentGraph, PersistentGraphPool, PersistentGraphSystem;
IMPORT ErrorSupport, Access, PageFile, Node;
IMPORT IO, Process;


VAR
  pool                 : PersistentGraphPool.T;
  graph                : PersistentGraph.T;
  firstnode, secondnode: Node.T;
  attr                 : TEXT;

BEGIN
  TRY

    (* Initialize GRAS. *)
    PersistentGraphSystem.Login("/tmp");

    (* Create a graphpool with name 'ExamplePool' *)
    IF PersistentGraphSystem.ExistsPool("ExamplePool") THEN
      IO.Put("Graphpool 'ExamplePool' already exists! \n");
      IO.Put("Use 'Simple3' to delete the graphpool. \n");
    ELSE
      TRY
        pool :=
          NEW(PersistentGraphPool.T).open(
            "ExamplePool", Access.Mode.ReadWriteExclusive, new := TRUE);
        IO.Put("Graphpool 'ExamplePool' created. \n");
      EXCEPT
        PageFile.NoAccess (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      | Access.Denied (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      END;

      TRY
        (* Create a graph with name 'ExampleGraph'. *)
        graph := NEW(PersistentGraph.T).open(
                   pool, "ExampleGraph",
                   PersistentGraph.AccessMode.ReadWriteExclusive,
                   new := TRUE, local := FALSE, errorChecks := FALSE);
        IO.Put("Graph 'ExampleGraph' created. \n");
      EXCEPT
        (* Exceptions raised by PersistentGraphPool.open *)
        PageFile.NoAccess (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      | Access.Denied (msg) =>
          IO.Put("Unable to open pool: " & msg & "\n");
          Process.Exit(1);
      | PersistentGraph.NotExistent =>
          IO.Put("Graph not existent!\n");
          Process.Exit(1);
      | PersistentGraph.InUse =>
          IO.Put("Graph in use!\n");
          Process.Exit(1);
      END;


      pool.beginTransaction();
      (* Now you can perform operations on the graph. *)

      (* Create a node with label '3', without an external name, near node
         with number '923' (if existent). *)
      firstnode := graph.createNodeNumber(Node.T{graph.number(), 923});
      firstnode := graph.createNode(3);

      (* Create a node with label '7' and an edge with label '83' from
         first_node to the new node. *)
      secondnode := graph.createNodeNumber(firstnode);
      secondnode := graph.createNode(7);
      graph.createEdge(firstnode, secondnode, 83);

      (* Append an index attribute to secondnode with tagfield 11 *)
      graph.putIndex(secondnode, 11, "sec_node");

      (* Store an attribute at secondnode starting at positions 4 of the
         attribute number 0. *)
      graph.putAttribute(secondnode, 0, 4, "Hi folks!");

      (* Read the attribute. *)
      attr := graph.getAttribute(secondnode, 0, 4, LAST(CARDINAL));
      (* Now commit the transaction, close the graph and pool. *)
      pool.commitTransaction();
      graph.close();
      pool.close();
    END;
  EXCEPT
    PersistentGraphPool.InternalError (e) =>
      IO.Put("Pool: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
  | PersistentGraph.InternalError (e) =>
      IO.Put("Graph: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
  | PersistentGraphSystem.InternalError (e) =>
      IO.Put("System: Internal Error\n" & ErrorSupport.ToText(e) & "\n");
  | Access.Locked => IO.Put("Deadlock!\n");
  | PersistentGraph.NotOwner => IO.Put("Graph is not owner of node!\n");
  | PersistentGraph.NodeNotFound => IO.Put("Node not found in graph!\n");
  | PersistentGraph.IndexUsed => IO.Put("Index in use!\n");
  | PersistentGraphPool.NotInTransaction =>
      IO.Put("Commit: Not in transaction!\n");
  END;

END Simple1.
