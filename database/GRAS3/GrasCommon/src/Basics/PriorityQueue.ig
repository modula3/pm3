GENERIC INTERFACE PriorityQueue(Element);

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.2  2003/04/08 21:56:43  hosking
    Merge of PM3 with Persistent M3 and CM3 release 5.1.8

    Revision 1.1.1.1  2003/03/27 15:25:27  hosking
    Import of GRAS3 1.1

    Revision 1.1  1997/10/31 14:08:24  roland
    New generic implementations for PriorityQueues. New templates and a new
    order function for Cardinal.

*)
(***************************************************************************)

(** Defines a type to efficiently store and retrieve Element.T sorted by
    priority.  Element must define

    - A type Element.T which describes the elements to be stored
    - a type Element.PriorityType that is used to store the priority of
      an element
    - a procedure Element.Priority(e: Element.T): Element.PriorityType;
      that returns the priority of an element
    - a procedure Element.PrioLess(p1, p2: Element.PriorityType): BOOLEAN:
      that defines an order on ELement.PriorityType.

   *)

CONST Brand = "(" & Element.Brand & " PriorityQueue )";

TYPE
  T <: Public;

  Public = <*TRANSIENT*> ROOT OBJECT
           METHODS
             init    (): T;
             isEmpty (): BOOLEAN;
             highest (): Element.PriorityType;
             insert  (e: Element.T);
             get     (): Element.T;
           END;

(* It is a checked runtime error to call highest or get when a queue is
   empty *)

END PriorityQueue.
