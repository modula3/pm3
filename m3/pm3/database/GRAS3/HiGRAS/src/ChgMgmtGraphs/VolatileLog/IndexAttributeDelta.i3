INTERFACE IndexAttributeDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1998/01/21 12:33:33  roland
    Distinction between forward and backward delta necessary.

    Revision 1.1  1997/05/30 07:51:44  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

(* Each AttributeDelta.T is associated with one node and one attribute
   number.  It stores and combines all commands of a delta that change this
   attribute. *)

IMPORT GraphCommand, Node;

CONST
  Brand = "IndexAttributeDelta";

TYPE
  T <: Public;

  Public = OBJECT
           METHODS
             init (node: Node.T; indexno: CARDINAL; forward: BOOLEAN): T;
                   (* Initialize as empty delta for (node, indexno) *)

             put (value: TEXT);
             delete (value: TEXT);
                     (* Two graph-operations change the state of an
                        index-attribute: put stores a new value and delete
                        removes an old. *)

             costs(): CARDINAL;

             loop ();
             getNext (VAR com: GraphCommand.T): BOOLEAN;
                      (* At the end of a delta, the combined attribute
                         operations can be acquired with

                         ad.loop(); WHILE ad.getNext(com) DO ...  END;

                         In case of index attributes this reduces to
                         [delete].[put] .*)

             reverseLoop ();
             getPrev (VAR com: GraphCommand.T): BOOLEAN;
                      (* At the end of a delta, the combined attribute
                         operations can be acquired in reverse order with

                         ad.reverseLoop(); WHILE ad.getPrev(com) DO ...  END;

                         In case of index attributes this reduces to
                         [put].[delete] .*)
           END;

(* All parts of an optimizing delta manage their types in free memory
   lists.  This reduces overhead for allocation and garbage collection.
   Therefore you should use the following procedures to acquire and release
   memeory for IndexAttributeDeltas. *)
PROCEDURE New (): T;
PROCEDURE Dispose (t: T);


END IndexAttributeDelta.
