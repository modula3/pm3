MODULE DaVinciMsg;

(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:41  hosking
    Initial revision

    Revision 1.2  1998/09/14 08:16:56  roland
    Methods for menu modification.

    Revision 1.1  1998/08/06 10:48:33  roland
    A Modula-3 interface to the graph display tool daVinci.

*)
(***************************************************************************)

IMPORT TextWr, Wr, Thread, Fmt;

<* FATAL Thread.Alerted, Wr.Failure *>

REVEAL
  T = Public BRANDED OBJECT
        wr   : TextWr.T;
        count: CARDINAL;
      OVERRIDES
        init           := Init;
        toText         := ToText;
        beginNewGraph  := BeginNewGraph;
        endNewGraph    := EndNewGraph;
        declareNode    := DeclareNode;
        endDeclareNode := EndDeclareNode;
        declareEdge    := DeclareEdge;
        addMenu        := AddMenu;
        insertIcon     := InsertIcon;
        activateIcon   := ActivateIcon;
      END;

PROCEDURE Init (msg: T): T =
  BEGIN
    msg.count := 0;
    msg.wr := TextWr.New();
    RETURN msg;
  END Init;


PROCEDURE ToText (msg: T): TEXT =
  BEGIN
    Wr.PutText(msg.wr, "\n");
    RETURN TextWr.ToText(msg.wr);
  END ToText;


PROCEDURE BeginNewGraph (msg: T) =
  BEGIN
    Wr.PutText(msg.wr, "graph(new([");
  END BeginNewGraph;


PROCEDURE EndNewGraph (msg: T) =
  BEGIN
    Wr.PutText(msg.wr, "]))");
  END EndNewGraph;


PROCEDURE DeclareNode (msg       : T;
                       id        : TEXT;
                       type      : TEXT       := NIL;
                       label     : TEXT       := NIL;
                       color     : TEXT       := NIL;
                       shape     : NodeShape  := NodeShape.Box;
                       iconfile  : TEXT       := NIL;
                       fontfamily: FontFamily := FontFamily.Lucida;
                       fontstyle : FontStyle  := FontStyle.Bold;
                       hidden    : BOOLEAN    := FALSE;
                       border    : Border     := Border.Single;
                       first     : BOOLEAN    := FALSE              ) =
  BEGIN
    IF type = NIL THEN type := "" END;
    IF NOT first THEN Wr.PutText(msg.wr, ","); END;
    Wr.PutText(msg.wr, "l(\"");
    Wr.PutText(msg.wr, id);
    Wr.PutText(msg.wr, "\",n(\"");
    Wr.PutText(msg.wr, type);
    Wr.PutText(msg.wr, "\",[");
    (* start of attribute list for node *)
    IF label # NIL THEN
      Wr.PutText(msg.wr, "a(\"OBJECT\",\"");
      Wr.PutText(msg.wr, label);
      Wr.PutText(msg.wr, "\"),");
    END;
    IF color # NIL THEN
      Wr.PutText(msg.wr, "a(\"COLOR\",\"");
      Wr.PutText(msg.wr, color);
      Wr.PutText(msg.wr, "\"),");
    END;
    IF iconfile = NIL AND shape = NodeShape.Icon THEN
      shape := NodeShape.Box;
    END;
    CASE shape OF
    | NodeShape.Box =>           (* Wr.PutText("a(\"_GO\",\"box\"),"); *)
    | NodeShape.Circle => Wr.PutText(msg.wr, "a(\"_GO\",\"circle\"),");
    | NodeShape.Ellipse => Wr.PutText(msg.wr, "a(\"_GO\",\"ellipse\"),");
    | NodeShape.Rhombus => Wr.PutText(msg.wr, "a(\"_GO\",\"rhombus\"),");
    | NodeShape.Text => Wr.PutText(msg.wr, "a(\"_GO\",\"text\"),");
    | NodeShape.Icon => Wr.PutText(msg.wr, "a(\"_GO\",\"icon\"),");
    END;
    IF shape = NodeShape.Icon AND iconfile # NIL THEN
      Wr.PutText(msg.wr, "a(\"ICONFILE\",\"");
      Wr.PutText(msg.wr, iconfile);
      Wr.PutText(msg.wr, "\"),");
    END;
    CASE fontfamily OF
    | FontFamily.Lucida =>       (* Wr.PutText(msg.wr,
                                    "a(\"FONTFAMILY\",\"lucida\"),"); *)
    | FontFamily.Times =>
        Wr.PutText(msg.wr, "a(\"FONTFAMILY\",\"times\"),");
    | FontFamily.Helvetica =>
        Wr.PutText(msg.wr, "a(\"FONTFAMILY\",\"helvetica\"),");
    | FontFamily.Courier =>
        Wr.PutText(msg.wr, "a(\"FONTFAMILY\",\"courier\"),");
    END;
    CASE fontstyle OF
    | FontStyle.Normal =>
        Wr.PutText(msg.wr, "a(\"FONTSTYLE\",\"normal\"),");
    | FontStyle.Bold =>          (* Wr.PutText(msg.wr,
                                    "a(\"FONTSTYLE\",\"bold\"),"); *)
    | FontStyle.Italic =>
        Wr.PutText(msg.wr, "a(\"FONTSTYLE\",\"italic\"),");
    | FontStyle.BoldItalic =>
        Wr.PutText(msg.wr, "a(\"FONTSTYLE\",\"bold_italic\"),");
    END;
    IF hidden THEN Wr.PutText(msg.wr, "a(\"HIDDEN\",\"true\"),"); END;
    CASE border OF
      (* in contrast the attributes above, we even print the default value
         here, to ensure no ',' is at the end of the attribute list. *)
      Border.Single => Wr.PutText(msg.wr, "a(\"BORDER\",\"single\")");
    | Border.Double => Wr.PutText(msg.wr, "a(\"BORDER\",\"double\")");
    END;
    Wr.PutText(msg.wr, "],[");
  END DeclareNode;


PROCEDURE EndDeclareNode (msg: T) =
  BEGIN
    Wr.PutText(msg.wr, "]))");
  END EndDeclareNode;


PROCEDURE DeclareEdge (msg      : T;
                       id       : TEXT;
                       target   : TEXT;
                       type     : TEXT          := NIL;
                       direction: EdgeDirection := EdgeDirection.Normal;
                       color    : TEXT          := NIL;
                       pattern  : EdgePattern   := EdgePattern.Solid;
                       label    : TEXT          := NIL;
                       first    : BOOLEAN       := FALSE                 ) =
  BEGIN
    IF NOT first THEN Wr.PutText(msg.wr, ",") END;
    IF type = NIL THEN type := "" END;
    Wr.PutText(msg.wr, "l(\"");
    Wr.PutText(msg.wr, id);
    Wr.PutText(msg.wr, "\",e(\"");
    Wr.PutText(msg.wr, type);
    (* attribute list for (first half of) edge *)
    Wr.PutText(msg.wr, "\",[");
    IF color # NIL THEN
      Wr.PutText(msg.wr, "a(\"EDGECOLOR\",\"");
      Wr.PutText(msg.wr, color);
      Wr.PutText(msg.wr, "\"),");
    END;
    CASE direction OF
      EdgeDirection.Normal =>    (* Wr.PutText(msg.wr,
                                    "a(\"_DIR\",\"normal\"),"); *)
    | EdgeDirection.Inverse =>
        Wr.PutText(msg.wr, "a(\"_DIR\",\"inverse\"),");
    | EdgeDirection.Both => Wr.PutText(msg.wr, "a(\"_DIR\",\"both\"),");
    | EdgeDirection.None => Wr.PutText(msg.wr, "a(\"_DIR\",\"none\"),");
    END;
    Wr.PutText(msg.wr, "a(\"EDGEPATTERN\",\"");
    CASE pattern OF
    | EdgePattern.Solid => Wr.PutText(msg.wr, "solid");
    | EdgePattern.Dotted => Wr.PutText(msg.wr, "dotted");
    | EdgePattern.Dashed => Wr.PutText(msg.wr, "dashed");
    | EdgePattern.Thick => Wr.PutText(msg.wr, "thick");
    | EdgePattern.Double => Wr.PutText(msg.wr, "double");
    END;
    Wr.PutText(msg.wr, "\")");
    IF label # NIL THEN
      (* edge labels are simulated by edge-node->edge, so there should be
         no arrow for the first half of the labelled edge. *)
      Wr.PutText(msg.wr, ",a(\"_DIR\",\"none\")");
    END;
    (* end of attribute list for (first half of) edge *)
    Wr.PutText(msg.wr, "],");

    IF label # NIL THEN
      (* when the edge is labelled we introduce an auxilliary node and
         edge.  The node has no shape (text) and the edge points from the
         auxilliary node to the target. *)
      Wr.PutText(msg.wr, "l(\"");
      Wr.PutText(msg.wr, NewAuxNode(msg));
      Wr.PutText(msg.wr, "\",n(\"\",[a(\"_GO\",\"text\"),a(\"OBJECT\",\"");
      Wr.PutText(msg.wr, label);
      Wr.PutText(msg.wr, "\")],[l(\"");
      Wr.PutText(msg.wr, NewAuxEdge(msg));
      Wr.PutText(msg.wr, "\",e(\"\",[");
      IF color # NIL THEN
        Wr.PutText(msg.wr, "a(\"EDGECOLOR\",\"");
        Wr.PutText(msg.wr, color);
        Wr.PutText(msg.wr, "\"),");
      END;
      Wr.PutText(msg.wr, "a(\"EDGEPATTERN\",\"");
      CASE pattern OF
      | EdgePattern.Solid => Wr.PutText(msg.wr, "solid");
      | EdgePattern.Dotted => Wr.PutText(msg.wr, "dotted");
      | EdgePattern.Dashed => Wr.PutText(msg.wr, "dashed");
      | EdgePattern.Thick => Wr.PutText(msg.wr, "thick");
      | EdgePattern.Double => Wr.PutText(msg.wr, "double");
      END;
      Wr.PutText(msg.wr, "\")],r(\"");
      Wr.PutText(msg.wr, target);
      Wr.PutText(msg.wr, "\")))]))");
    ELSE
      Wr.PutText(msg.wr, "r(\"");
      Wr.PutText(msg.wr, target);
      Wr.PutText(msg.wr, "\")");
    END;
    Wr.PutText(msg.wr, "))");
  END DeclareEdge;


PROCEDURE NewAuxEdge (msg: T): TEXT =
  BEGIN
    INC(msg.count);
    RETURN "e" & Fmt.Int(msg.count);
  END NewAuxEdge;


PROCEDURE NewAuxNode (msg: T): TEXT =
  BEGIN
    INC(msg.count);
    RETURN "n" & Fmt.Int(msg.count);
  END NewAuxNode;


(*-------------------------------*)
(*--- DaVinci menu operations ---*)
(*-------------------------------*)

PROCEDURE AddMenu (msg: T; id: TEXT; label: TEXT): TEXT =
  BEGIN
    Wr.PutText(msg.wr, "app_menu(create_menus(menu_entry_mne([" & id & ","
                         & label & ",\"none\",\"\"])))");
    RETURN msg.toText();
  END AddMenu;


PROCEDURE InsertIcon (msg: T; id: TEXT; fn: TEXT; description: TEXT):
  TEXT =
  BEGIN
    Wr.PutText(
      msg.wr, "app_menu(create_icons([icon_entry(\"" & id & "\",\"" & fn
                & "\",\"" & description & "\")]))\n");
    RETURN msg.toText();
  END InsertIcon;


PROCEDURE ActivateIcon (msg: T; id: TEXT): TEXT =
  BEGIN
    Wr.PutText(msg.wr, "app_menu(activate_icons([\"" & id & "\"]))\n");
    RETURN msg.toText();
  END ActivateIcon;

BEGIN
END DaVinciMsg.
