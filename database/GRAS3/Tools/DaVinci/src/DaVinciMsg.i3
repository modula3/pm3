INTERFACE DaVinciMsg;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.2  1998/09/14 08:16:55  roland
    Methods for menu modification.

    Revision 1.1  1998/08/06 10:48:32  roland
    A Modula-3 interface to the graph display tool daVinci.

*)
(***************************************************************************)

TYPE
  EdgePattern = {Solid, Dotted, Dashed, Thick, Double};
  EdgeDirection = {Normal, Inverse, Both, None};
  NodeShape = {Box, Circle, Ellipse, Rhombus, Text, Icon};
  Border = {Single, Double};
  FontFamily = {Lucida, Times, Helvetica, Courier};
  FontStyle = {Normal, Bold, Italic, BoldItalic};

TYPE
  T <: Public;

  Public =
    OBJECT
    METHODS
      init   (): T;
      toText (): TEXT;

      beginNewGraph ();
      endNewGraph   ();

      declareNode (id        : TEXT;
                   type      : TEXT       := NIL;
                   label     : TEXT       := NIL;
                   color     : TEXT       := NIL;
                   shape     : NodeShape  := NodeShape.Box;
                   iconfile  : TEXT       := NIL;
                   fontfamily: FontFamily := FontFamily.Lucida;
                   fontstyle : FontStyle  := FontStyle.Bold;
                   hidden    : BOOLEAN    := FALSE;
                   border    : Border     := Border.Single;
                   first     : BOOLEAN    := FALSE              );
                   (* a node must be declared with 'declareNode'.  To get a
                      syntactically correct term, a call to declareNode
                      must be followed by an arbitrary number of edge
                      declareations, finally followed by endDeclareNode. *)

      declareEdge (id       : TEXT;
                   target   : TEXT;
                   type     : TEXT          := NIL;
                   direction: EdgeDirection := EdgeDirection.Normal;
                   color    : TEXT          := NIL;
                   pattern  : EdgePattern   := EdgePattern.Solid;
                   label    : TEXT          := NIL;
                   first    : BOOLEAN       := FALSE                 );
      endDeclareNode ();


      (*---------------------------*)
      (*----- Menu operations -----*)
      (*---------------------------*)

      addMenu (id: TEXT; label: TEXT): TEXT;
               (* adds a single menu entry into DaVinci's root menu *)

      insertIcon (id: TEXT; fn: TEXT; description: TEXT): TEXT;
                  (* inserts a new icon into icon bar of DaVinci's view *)

      activateIcon (id: TEXT): TEXT;
                    (* activates ONE icon, deactivating all others, which
                       are created by our application *)
    END;

END DaVinciMsg.
