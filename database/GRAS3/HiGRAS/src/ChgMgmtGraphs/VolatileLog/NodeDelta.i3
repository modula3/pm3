INTERFACE NodeDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/05/30 07:51:46  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

(* A NodeDelta.T stores and combines all commands of a delta that add or
   delete nodes and modify node attributes. *)

IMPORT GraphCommand, Node;

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init (forward: BOOLEAN): T;
            (* Initialize as empty delta *)

      create (READONLY node: Node.T; label: CARDINAL);
      delete (READONLY node: Node.T; VAR deleteEdgeOps: BOOLEAN);
              (* Notify automaton of edge createion / deletion*)


      putLabel (node: Node.T; label: CARDINAL);
      putAttribute (node : Node.T;
                    attr : CARDINAL;
                    start: CARDINAL;
                    value: TEXT      );
      truncateAttribute (node: Node.T; attr: CARDINAL; length: CARDINAL);

      putIndex    (READONLY node: Node.T; ind: CARDINAL; value: TEXT);
      deleteIndex (READONLY node: Node.T; ind: CARDINAL; value: TEXT);

      notifyEdgeOp (source, target: Node.T): BOOLEAN;
                    (* Notifies automaton of modification.  If return value
                       is TRUE, edge operation has to be logged otherwise
                       it needs not. *)

      costs (): CARDINAL;

      loop ();
      getNext (VAR com: GraphCommand.T): BOOLEAN;
               (* At the end of a delta, the combined node operations can
                  be acquired with

                  ad.loop(); WHILE ad.getNext(com) DO ...  END; *)

      reverseLoop ();
      getPrev (VAR com: GraphCommand.T): BOOLEAN;
               (* At the end of a delta, the combined node operations can
                  be acquired in reverse order with

                  ad.reverseLoop(); WHILE ad.getPrev(com) DO ...  END; *)
END;


(* All parts of an optimizing delta manage their types in free memory
   lists.  This reduces overhead for allocation and garbage collection.
   Therefore you should use the following procedures to acquire and release
   memeory for EdgeDeltas. *)
PROCEDURE New (): T;
PROCEDURE Dispose (t: T);


END NodeDelta.
