INTERFACE NodeDeltaInfo;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:30  hosking
    Initial revision

    Revision 1.1  1997/05/30 07:51:48  roland
    VolitleDeltas now optimize their command sequences to contain only the
    commands producing the net effect of all applied commands.

*)
(***************************************************************************)

IMPORT IndexAttributeDeltaTbl, AttributeDeltaTbl;

CONST Brand = "NodeDeltaInfo";

TYPE
  State = {Undefined,            (* no operations performed so far *)
           OldExistent,          (* operations on a node that wasn't
                                    created or deleted in the delta so
                                    far *)
           OldDeleted,           (* node existed when delta started and was
                                    deleted *)
           OldNew,               (* node (number) was in OldDeleted and was
                                    created again *)
           NewCreated,           (* node did not exist when delta started
                                    and was created *)
           NewDeleted            (* node was in NewDeleted and was
                                    deleted *)
          };

CONST
  DeletedStates = SET OF State{State.OldDeleted, State.OldNew};
  CreatedStates = SET OF State{State.NewCreated, State.OldNew};
  ExistingStates = SET OF State{State.NewCreated, State.OldNew, State.OldExistent};
  
TYPE
  T = RECORD
        state          : State;
        label          : CARDINAL;
        changedLabel   : BOOLEAN;
        attributeDeltas: AttributeDeltaTbl.T;
        indexDeltas    : IndexAttributeDeltaTbl.T;
      END;

END NodeDeltaInfo.
