MODULE Node;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:31  hosking
    Initial revision

    Revision 1.2  1997/04/24 14:32:37  roland
    Adapted to access mode parameter for VirtualRemoteFile.T.open. Access
    modes for graphs are now supported.

    Revision 1.1  1997/03/26 11:39:26  roland
    Subsystem PersistentGraph adapted to handle graph boundary crossing
    edges. This has consequences on the architecture of the subsystem as
    well as on the graph model and interface.

    Graphs are organized in pools. Every graph has a number in the
    pool. Pools are the units of transaction management. Two graphs might
    be related by one external relation storage storing the edges between
    nodes of them. Nodes are identified by pairs (graph, entity), where
    graph is the number of the graph in the pool and entity the node
    number within the graph. Graphs and external relation storages are
    administered by the pool in a separate graph.

*)
(***************************************************************************)

IMPORT Word;

PROCEDURE Equal (READONLY n1, n2: T): BOOLEAN =
  BEGIN
    RETURN n1 = n2;
  END Equal;

PROCEDURE Compare (READONLY n1, n2: T): [-1 .. 1] =
  BEGIN
    IF n1.graph < n2.graph THEN
      RETURN -1
    ELSIF n1.graph > n2.graph THEN
      RETURN 1
    ELSE
      IF n1.entity < n2.entity THEN
        RETURN -1
      ELSIF n1.entity > n2.entity THEN
        RETURN 1
      ELSE
        RETURN 0;
      END;
    END;
  END Compare;

PROCEDURE Hash (READONLY n: T): Word.T =
  CONST bits = 2 * BITSIZE(CARDINAL) DIV 3;
  (* Graph number should be smaller than node number in general.  Therefor,
     we rotate the graph number to the higher order bits of the hash key. *)
  BEGIN
    RETURN Word.Xor(Word.LeftRotate(n.graph, bits), n.entity);
  END Hash;

BEGIN
END Node.
