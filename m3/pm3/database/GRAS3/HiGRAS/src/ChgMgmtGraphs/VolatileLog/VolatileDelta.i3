INTERFACE VolatileDelta;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.2  1997/05/30 07:51:49  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

(* A OptVolatileDelta.T minimizes the sequence of commands it stores.  It
   removes all unnecessary operations from the sequence (e.g.  operations
   concerning a node which is later deleted). *)

IMPORT Delta AS Super;

TYPE
  T <: Public;

  Public =
    Super.T OBJECT
    METHODS
      init (forward: BOOLEAN): T RAISES {};
            (* Open a delta on a list.  The delta will be a forward delta
               if forward is TRUE, else a backward delta.  The delta is
               initialized as empty with costs 0.  Costs of the delta will
               be incremented with every addCommand by one.*)

    END;

PROCEDURE New (): T;
  (* For reasons of efficiency, OptVolatileDelta holds a list of used and
     free deltas, so that not every graph command issues memory allocation
     for a delta.  To be effective, unused deltas should be freed using
     OptVolatileDelta.Free. *)

PROCEDURE Free (delta: T);
  (* Insert delta in a list of allocated but no longer used deltas. *)

END VolatileDelta.
