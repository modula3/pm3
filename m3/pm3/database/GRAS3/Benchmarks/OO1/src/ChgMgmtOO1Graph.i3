INTERFACE ChgMgmtOO1Graph;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:24  hosking
    Initial revision

    Revision 1.3  1998/03/18 09:26:53  kluck
    When closing a graph there is no local parameter needed.
    Furthermore graphs are handled as remote by default.

    Revision 1.2  1998/03/17 14:13:28  kluck
    Necessary adaptions to use local graphs. (MK)

    Revision 1.1  1997/02/20 16:08:38  roland
    OO1 rewritten with graphical user interface.

*)
(***************************************************************************)

IMPORT OO1Graph, Pathname, Access, Thread;

TYPE
  T <: Public;

  Public = OO1Graph.T OBJECT
           METHODS
             open (name          : Pathname.T;
                   local         : BOOLEAN;
                   access        : Access.Mode;
                   new           : BOOLEAN;
                   simpleConnects: BOOLEAN      ): T
                   RAISES {OO1Graph.Failure, Thread.Alerted};
           END;

END ChgMgmtOO1Graph.
