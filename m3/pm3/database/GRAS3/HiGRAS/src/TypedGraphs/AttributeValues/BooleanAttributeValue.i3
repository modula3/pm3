INTERFACE BooleanAttributeValue;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:36  hosking
    Initial revision

    Revision 1.1  1997/05/01 13:23:52  roland
    TypedGraph layer adapted to graph boundary crossing edges.

    Revision 1.1  1997/01/31 10:34:59  roland
    AttributeValues ease typed access to attributes.

*)
(***************************************************************************)

IMPORT AttributeValue;

TYPE
  T <: Public;

  Public = AttributeValue.T OBJECT
             METHODS
               set (val: BOOLEAN);
               get (): BOOLEAN;
             END;

END BooleanAttributeValue.
