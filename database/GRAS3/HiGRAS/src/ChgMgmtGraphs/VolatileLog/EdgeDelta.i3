INTERFACE EdgeDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/05/30 07:51:42  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

(* Each EdgeDelta.T stores and combines all commands of a delta that add or
   delete edges. *)

IMPORT GraphCommand, Node;

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (): T;
            (* Initialize as empty delta *)

      create (READONLY source, target: Node.T; label: CARDINAL);
      delete (READONLY source, target: Node.T; label: CARDINAL);
              (* Two graph-operations affect edges: createEdge and
                 deleteEdge. *)

      deleteNode (node: Node.T);
                  (* When a node is deleted all edge operations concerning
                     this node have to be deleted, too. *)

      costs(): CARDINAL;

      loop ();
      getNext (VAR com: GraphCommand.T): BOOLEAN;
               (* At the end of a delta, the combined edge operations can
                  be acquired with

                  ad.loop(); WHILE ad.getNext(com) DO ...  END; *)
    END;


(* All parts of an optimizing delta manage their types in free memory
   lists.  This reduces overhead for allocation and garbage collection.
   Therefore you should use the following procedures to acquire and release
   memeory for EdgeDeltas. *)
PROCEDURE New (): T;
PROCEDURE Dispose (t: T);

END EdgeDelta.
