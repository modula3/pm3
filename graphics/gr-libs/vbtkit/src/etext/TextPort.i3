(* Copyright (C) 1989-1992, Digital Equipment Corporation       *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* Last modified on Fri May 17 22:06:53 PDT 1996 by mhb             *)
(*      modified on Wed Jun 16 10:20:38 PDT 1993 by meehan          *)
(*      modified on Fri Mar 20 22:34:09 1992 by steveg          *)
(*      modified on Fri Feb 8 15:36:58 PST 1991 by brooks       *)
(*      modified on Wed May 17 17:06:31 PDT 1989 by gidi        *)
<* PRAGMA LL *>

(* A textport is a "VBT" that allows the user to type and edit text.

   The methods and procedures in this interface fall into several categories,
   each dealing with different aspects of the text-editor. 

   <DL>
   
   <DT>Appearance<DD> The client can choose the font, colors, margins,
   and whether long lines should be clipped or wrapped. The fonts and
   colors can be changed dynamically.

   <DT>Access to the text<DD> There are procedures to read and write
   subsequences of the text, to read and set the current ``type-in''
   point (cursor position), to get the length of the text, and to make
   the text read-only.

   <DT>Keybindings and Text-Selections<DD> A textport is initialized
   with a <EM>model</EM>, an object (defined in the "TextPortClass"
   interface) that establishes the connection between keystrokes and
   editing operations, and the connection between mouse-gestures, the
   cursor position, local selections (including highlighted regions),
   and global selections such as the ``clipboard'' ("VBT.Source").
   Four such models are implemented---Emacs, Ivy, Xterm, and
   Mac---corresponding to different editing paradigms. The choice of
   model can be changed dynamically. The client may override the
   "filter" method to intercept keystrokes.

   <DT>Feedback<DD> A textport has callback-methods that are invoked
   when the text changes, when the user types Return or Tab, when the
   textport gains or loses the keyboard focus, when the visible
   region changes, and when errors are detected. All these methods
   have defaults.

   </DL>

   The locking level for all procedures is "LL &lt;= VBT.mu" except as noted. *)

INTERFACE TextPort;

IMPORT Font, PaintOp, VBT, VText;

TYPE
  T <: Public;
  Public = VBT.Leaf OBJECT
           METHODS
             init (hMargin, vMargin := 0.5;
                   font             := Font.BuiltIn;
                   colorScheme: PaintOp.ColorScheme := NIL;
                   wrap                             := TRUE;
                   readOnly                         := FALSE;
                   turnMargin                       := 0.5;
                   model := Model.Default): T;

             <* LL.sup = VBT.mu *>
             filter         (cd: VBT.KeyRec);
             getFont        (): Font.T;
             setFont        (font: Font.T);
             getColorScheme (): PaintOp.ColorScheme;
             setColorScheme (c: PaintOp.ColorScheme);
             getModel       (): SpecificModel;
             setModel       (model: Model);
             getReadOnly    (): BOOLEAN;
             setReadOnly    (flag: BOOLEAN);

             (* callbacks *)
             modified     ();
             returnAction (READONLY cd: VBT.KeyRec);
             tabAction    (READONLY cd: VBT.KeyRec);
             focus (gaining: BOOLEAN; time: VBT.TimeStamp);
             error        (msg: TEXT);

           END;

(* The call "v.init(...)" initializes "v" as a "TextPort.T" and
   returns it.

   The parameters "hMargin" and "vMargin" indicate how much whitespace
   to leave around the text, expressed in millimeters.

   "colorScheme" is used for painting the text.  If the parameter is
   "NIL", then "PaintOp.bgFg" will be used.

   If "wrap" is "TRUE", then text will be wrapped across line
   boundaries; otherwise it will be clipped.  If it is wrapped, then
   "turnMargin" specifies the width (in millimeters) of the gray bar
   placed at the end of the first line and the beginning of the
   second, indicating that the text has been wrapped.

   If "readOnly" is "TRUE", then the text cannot be changed through
   the user interface (keyboard and mouse).  The procedures "Replace",
   "Insert", "SetText", and "PutText" bypass the read-only protection,
   but these are not called by internal routines.  In all other
   descriptions in this interface, the words <I>replace</I>, <I>insert</I>,
   <I>delete</I>, and so on should be understood as having the
   restriction that "v" is not read-only.

   If "model" is "Model.Default", then the default model (see below) 
   will be used.

   "v.getModel()" returns the name of the current model; note that the
   return value cannot be "Model.Default".  The call "v.setModel(...)"
   changes the current model; its parameter may be "Model.Default", in
   which case the default model (see below) will be used.

   The call "v.setFont(font)" changes the font used for displaying
   the text.

   The call "v.setColorScheme(c)" changes the colors used for
   displaying the text.

   The implementation calls "v.focus(gaining, time)" whenever
   "v" gains or loses the keyboard focus.  If "gaining" is "TRUE",
   then "v" is about to gain the keyboard focus (and "time" is a
   valid event-time); i.e., this method is called <EM>before</EM> the
   selection feedback is established, so it is reasonable to call
   "Select" (below) or put up some other indication.  If "gaining" is
   "FALSE", then "v" has just lost the keyboard focus (and "time"
   is <EM>not</EM> valid), so it reasonable to take down whatever
   indicated that the focus had been acquired.  It is not within the
   power of the "focus" method to prevent "v" from gaining or
   losing the focus.  The default for this method is a no-op.

   The implementation calls "v.error(msg)" whenever an exception is
   raised for which there is no particular remedy, such as an
   "Rd.Failure".  The value of "msg" will be a short description of
   the error, typically the name of the procedure where the exception
   was raised.  No method or procedure defined in this interface
   raises exceptions, but the client may wish to override this method
   in order to report the error in a popup window, for example. The
   default for this method is a procedure that tests whether the
   environment-variable named "TEXTPORTDEBUG"<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>TEXTPORTDEBUG</SPAN>
</SPAN>
 is
   set (to any value); if so, it writes the message to
   "Stdio.stderr".

*)


(* <H3> Access to the text </H3>
   
   The textport's initial read-only status depends on the "readOnly"
   parameter to the "init" method. The "getReadOnly" method returns
   it; the "setReadOnly" method sets it.

 *)

PROCEDURE GetText (v    : T;
                   begin: CARDINAL := 0;
                   end  : CARDINAL := LAST (CARDINAL)): TEXT;
<* LL.sup = VBT.mu *>
(* Returns a sub-sequence of the text in "v".  The result will be
   empty if
| begin >= Length(v)
   Otherwise the range of indexes of the subsequence is
| [begin .. MIN (end, Length (v)) - 1]
   *)

PROCEDURE SetText (v: T; t: TEXT);
(* Replace the current contents of "v" with "t". This procedure does
   not test the read-only status of "v". *)

PROCEDURE PutText (v: T; t: TEXT);
(* Append "t" to the current contents of "v".  This procedure does not
   test the read-only status of "v".*)

PROCEDURE Replace (v: T; begin, end: CARDINAL; newText: TEXT);
(* Replace the text between positions "begin" and "end" in "v" with
   "newText".  If "begin" and "end" are beyond the end of the text,
   they are taken to refer to the end of the text.  This procedure
   does not test the read-only status of "v".*)

PROCEDURE Insert (v: T; text: TEXT);
(* If there is a replace-mode selection (see
   Section&nbsp;<A REL=REF.NUMBER HREF="ReplaceMode"> [ReplaceMode] </A>, page&nbsp;<A REL=REF.PAGE HREF="ReplaceMode"> [ReplaceMode] </A>), replace it
   with "text"; otherwise insert "text" at the type-in point. In
   either case, this is a no-op if "text" is the empty string.  This
   procedure does not test the read-only status of "v".*)

PROCEDURE Index (v: T): CARDINAL;
(* Return the current ``type-in'' position. *)

PROCEDURE Seek (v: T; n: CARDINAL);
(* Set the ``type-in'' position to "n". *)

PROCEDURE Length (v: T): CARDINAL;
(* Return the number of characters in "v"'s text. *)

PROCEDURE Newline (v: T);
(* Insert a newline character at the type-in point. *)

PROCEDURE NewlineAndIndent (v: T);
(* Insert a newline character and enough spaces to match the
   indentation of the previous line.  As it leaves a blank line,
   it will delete all spaces from that line so as to leave it
   truly empty. *)

PROCEDURE IsVisible (v: T; pos: CARDINAL): BOOLEAN;
(* Test whether the character at position "pos" is visible. *)

(* <H3> Models </H3><SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>Model</SPAN>
</SPAN>
 *)

TYPE
  Model = {Default, Ivy, Emacs, Mac, Xterm};
  SpecificModel = [Model.Ivy .. Model.Xterm];

VAR
   DefaultModel: SpecificModel; 

(* The default editing model, "DefaultModel", is initialized to the
   environment variable named "TEXTPORTMODEL";<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>TEXTPORTMODEL</SPAN>
</SPAN>
 if that
   variable is not set, or set to something other than "emacs", "ivy",
   "mac", or "xterm" at startup time, then "Model.Emacs" will be used.  See
   the "EmacsModel", "IvyModel", "XtermModel", and "MacModel" interfaces in
   Appendices <A REL=REF.NUMBER HREF="EmacsModel"> [EmacsModel] </A>--<A REL=REF.NUMBER HREF="XtermModel"> [XtermModel] </A> for details on
   keybindings, mouse-clicks, and selections.*)

PROCEDURE ChangeAllTextPorts (v: VBT.T; newModel := Model.Default);
(* For each textport "p" that is a descendent of VBT "v", call
   "p.setModel(newModel)". *)

(* <H3 ID="TextPortKeybindings"> Keybindings </H3>

   The "TextPort" interface allows clients a great deal of flexibility
   in handling keystrokes.  "v.key(cd)" proceeds in
   three steps:

   In step 1, it tests whether "cd.wentDown" is true, whether "v"
   has the keyboard focus, and whether "v"'s domain is non-empty.
   If all three conditions are true, it proceeds to step 2.

   In step 2, it passes "cd" to the model's "keyfilter" object, which
   handles low-level tasks such as converting ``Escape + character''
   into ``meta-character'' (in Emacs mode), 8-bit ``compose
   character'' operations, and so on.  The model may actually contain
   a <EM>chain</EM> of keyfilters (see the "KeyFilter" interface), each
   implementing some translation.

   In step 3, the model passes "cd" (possibly changed by the
   keyfilters) to the textport's "filter" method. Clients who wish
   to intercept keystrokes usually do so at this point, by overriding
   the "filter" method, rather than by overriding the "key" method, so
   that they can take advantage of the low-level conversions.

   In the default "filter" method, there are several mutually
   exclusive possibilities, tested in this order:

   <UL>

   <LI>If the key is Return, then if the "shift" modifier is on, we
   insert a newline; if the "option" modifier is on, we insert a
   newline but leave the cursor in place; otherwise, we invoke
   "v.returnAction(cd)", another callback method. Its default
   method calls "NewlineAndIndent(v, cd)".

   <LI>If the key is Tab, we invoke "v.tabAction(cd)".  The
   default method inserts 4 spaces.

   <LI>If the key is an ``arrow'' key, we call the model's
   "arrowKey" method, which moves the cursor one character forward,
   one character backward, one line up, or one line down, as
   appropriate.

   <LI>If the "control" modifier is on, we call the model's
   "controlChord" method.

   <LI>If the "option" modifier is on, we call the model's
   "optionChord" method.

   <LI>If the key is Backspace or Delete, we delete the previous
   character, or the current primary selection, if that is non-empty and
   in replace-mode.

   <LI>If the key is an ISO Latin-1 graphic character, we insert it into
   the text.

   <LI>Otherwise, we ignore it.

   </UL>

   Finally, we call "Normalize(v)", except in the "controlChord" and
   "optionChord" cases.

   Clients can specialize the handling of keys, therefore, by
   overriding the textport's "key", "filter", "returnAction", or
   "tabAction" methods, and by overriding the model's "controlChord",
   "optionChord", or "arrowKey" methods.

   The following procedures give the client access to the keyboard
   focus:
*)

PROCEDURE TryFocus (v: T; t: VBT.TimeStamp): BOOLEAN;
(* Try to acquire the keyboard focus and the primary selection, and
   report whether it succeeded. *)

PROCEDURE HasFocus (v: T): BOOLEAN; <* LL.sup = VBT.mu *>
(* Test whether "v" has the keyboard focus. *)


(* <H3 ID="TextPortSelections"> Selections </H3>

   With various keyboard and mouse-gestures, the user may delimit a
   range of text, known as a <EM>local selection</EM>.  The "TextPort"
   interface defines two local selections, called <EM>primary</EM> and
   <EM>secondary</EM>. The mechanism for doing this depends entirely on
   the textport's model. (In fact, only the Ivy model implements
   secondary selection.) The type-in point is always at one end or the
   other of the primary selection.

   Primary selections in non-readonly textports may be in <EM>
   replace mode</EM>, also called <EM>pending-delete mode</EM>. This means
   that any text that is inserted will replace the primary selection,
   and that the Backspace and Delete keys will delete it.

   Independent of the local selections are the two <EM>global
   selections</EM> defined by Trestle: "VBT.Source" and "VBT.Target".  On
   X window systems, these are defined by the X server, and are shared
   across applications. The Source selection, for example, is
   effectively the ``clipboard.'' Globals selections are ``owned'' by
   one program at a time; in Trestle programs, they are owned by one
   "VBT" at a time. While every textport may have a primary and
   secondary local selection, at most one can own Source, and at most
   one can own Target. The <EM>contents</EM> of a global selection are
   controlled by its owner.

   The correspondence between local and global selections also depends
   entirely on the model. Every model implements an operation called
   <B>Copy</B><SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>Copy</SPAN>
</SPAN>
, which is defined as follows: the textport
   acquires ownership of Source, and copies the Primary selection so
   that it is the contents of Source.

   Some models establish an <EM>alias</EM><SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>alias</SPAN>
</SPAN>
 between a local
   and a global selection, which means that when that textport owns
   the global selection, the contents of the global selection are
   <EM>identical with</EM> the contents of the local selection.

   In the Ivy model, for example, Primary is an alias for Target, and
   Secondary is an alias for Source. In the Xterm model, Primary is an
   alias for Source. The other models do not use aliasing at all; they
   implement <B>Copy</B> by making a separate copy of the local
   selection. In those models, the contents of the global selection
   are not visible; i.e., they are not displayed in the textport.

   Local selections are usually highlighted in some way. The
   highlighting obeys the following conventions, applied in this
   order:

   <OL><SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>TextPortHighlighting</SPAN>
</SPAN>

   <LI ID="TextPortHighlighting">A replace-mode Primary 
   selection is highlighted with black
   text on a light red background.  (On monochrome screens, it is
   highlighted with ``inverse video'': white text on a dark
   background.)

   <LI>If a Source selection is visible (i.e., if it is aliased with
   a local selection), it is highlighted with a thin, green underline.
   (On monochrome screens, it is a thin, black underline.)

   <LI>A Primary selection that is neither a replace-mode selection
   nor a Source selection (e.g., a selection in the Emacs model), is
   underlined with a thick line.  On color screens, there is a further
   distinction: in a read-only text, the underline is blue; otherwise,
   the underline is red.

   </OL>

   A selection is represented by a pair of inclusive indexes ("begin"
   and "end") into the text.  The current selection-indices can be
   retrieved via the "GetSelection" procedure.
 *)

TYPE SelectionType = {Primary, Secondary};

PROCEDURE Select (v    : T;
                  time : VBT.TimeStamp;
                  begin: CARDINAL        := 0;
                  end  : CARDINAL        := LAST (CARDINAL);
                  sel         := SelectionType.Primary;
                  replaceMode := FALSE;
                  caretEnd    := VText.WhichEnd.Right   );

(* Make a selection in "v", at event-time "time".  If "begin" and/or
   "end" are beyond the end of the text, they will be clipped to the
   end of the text.  Acquire ownership of the corresponding
   "VBT.Selection"; if "sel" is "SelectionType.Primary", acquire
   ownership of the keyboard focus as well.

   The parameters "replaceMode" and "caretEnd" are relevant only if
   the value of "sel" is "SelectionType.Primary".  If "replaceMode" is
   "TRUE" and the entire selection is writable, then "Insert" and
   "VBT.Write" will <EM>replace</EM> the selected text; otherwise, they
   cause the new text to be <EM>inserted</EM> at whichever end of the
   primary selection is specified by "caretEnd". *)

PROCEDURE IsReplaceMode (v: T): BOOLEAN;
(* Return "TRUE" if the primary selection is in replace mode. *)

TYPE Extent = RECORD l, r: CARDINAL END;

CONST NotFound = Extent {LAST (CARDINAL), LAST (CARDINAL)};

PROCEDURE GetSelection (v: T; sel := SelectionType.Primary):
  Extent;
(* Return the extent of the most recent selection in "v". If there is no
   such selection, return "NotFound". *)

PROCEDURE GetSelectedText (v: T; sel := SelectionType.Primary):
  TEXT;
<* LL.sup = VBT.mu *>
(* Return the text of the most recent selection in "v" if there is one, or
   the empty string otherwise. *)

PROCEDURE PutSelectedText (v: T;
                           t: TEXT;
                           sel := SelectionType.Primary);
<* LL.sup = VBT.mu *>
(* Replace the text of the most recent selection in "v", if there is
   one, with "t".  If there is no such selection, this is a no-op. *)


(* <H3> Feedback </H3>

   A textport maintains a ``modified'' flag.  Any operation that
   changes the text will cause this flag to be set to "TRUE".  If it
   was previously "FALSE", then the implementation calls
   "v.modified()" <I>after</I> the change has already happened to "v".
   The default is a no-op.  The "IsModified" and "SetModified"
   procedures set and test this flag, respectively. *)

PROCEDURE IsModified (v: T): BOOLEAN;
(* Return the value of the ``modified'' flag for "v". Any change to
   the text will cause the flag to be set to "TRUE". *)

PROCEDURE SetModified (v: T; value: BOOLEAN);
(* Set the value of the ``modified'' flag for "v". This will not
   invoke "v.modified", even if "value" is "TRUE". *)

(* A textport also maintains a scrollbar (optional).  See the
   "TextEditVBT" interface in Section&nbsp;<A REL=REF.NUMBER HREF="TextEditVBTSection"> [TextEditVBTSection] </A>. *)

PROCEDURE Normalize (v: T; to := -1);
(* Scroll "v" if necessary to ensure that position "to" is visible.
   If "to &lt; 0", it refers to the current type-in point.  If "to" is
   larger than the length of the text, normalizes to the end of the
   text. *)

(* <H3> Direct access to the text </H3> *)

PROCEDURE GetVText (v: T): VText.T;
(* For wizards only: extract the underlying "VText".  It is legal to
   create and manipulate highlighting intervals on it.  It is legal to
   run readers on it, provided you can be sure that you are locking
   out concurrent change (for example, by holding "VBT.mu").  It is
   not legal to modify it directly.  It is not legal to scroll it
   directly either, because that will leave the scrollbar incorrect.
   *)

END TextPort.

