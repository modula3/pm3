INTERFACE TypedOO1Graph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.4  1998/03/17 14:13:36  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.3  1997/10/14 09:16:44  roland
    Merge of HiGRAS and Main branch.

    Revision 1.1.2.1  1997/06/19 14:38:32  roland
    Branch HiGRAS implements OO1 for HiGRAS. TypegGraph-Implementation of
    OO1Graph added.

*)
(***************************************************************************)

IMPORT OO1Graph, Pathname, Access, Thread;

TYPE
  T <: Public;

  Public = OO1Graph.T OBJECT
           METHODS
             open ( name           : Pathname.T;
                    local          : BOOLEAN;
                    access         : Access.Mode;
                    new            : BOOLEAN;
                    simpleConnects : BOOLEAN      ): T
                   RAISES {OO1Graph.Failure, Thread.Alerted};
           END;

END TypedOO1Graph.
