MODULE TaggedName;

(***************************************************************************)
(** Created by:  Peter Klein						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:32  hosking
    Initial revision

    Revision 1.1  1997/03/26 11:39:48  roland
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

    Revision 1.3  1996/08/06 16:26:55  roland
    Merge of PAGESERVER and main branch.

# Revision 1.2  1994/03/30  18:30:37  pk
# New shape for 3.1 required.
#
# Revision 1.1  1994/01/20  18:42:14  pk
# Initial revision
#
*)
(***************************************************************************)

IMPORT Text;


PROCEDURE Equal (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN ((a.tag = b.tag) AND Text.Equal(a.name, b.name));
  END Equal;


PROCEDURE Compare (READONLY a, b: T): [-1 .. 1] =
  BEGIN
    IF (a.tag < b.tag) THEN
      RETURN -1;
    ELSIF (a.tag > b.tag) THEN
      RETURN 1;
    END;

    RETURN Text.Compare(a.name, b.name);
  END Compare;

BEGIN
END TaggedName.
