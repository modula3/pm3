INTERFACE TreeScheme;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:26  hosking
    Initial revision

    Revision 1.1  1998/01/21 14:23:39  roland
    Tree example demonstrates derived attributes, triggers, and user-recovery.

*)
(***************************************************************************)

IMPORT Scheme;

(* names of schema components *)
CONST
  TreeNodeClass   = "NODE";
  TreeNodeType    = "Node";
  TreeEdge        = "ToSon";
  LevelAttribute  = "Level";
  DegreeAttribute = "MaxDegree";
  IsRootAttribute = "IsRoot";
  LevelEval       = "ComputeLevel";
  DegreeEval      = "ComputeMaxDegree";

(* id's of schema components *)
VAR type, level, maxDegree, toSon, isRoot: Scheme.ID;

END TreeScheme.
