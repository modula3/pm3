(* Copyright (C) 1992 Digital Equipment Corporation                          *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 16 10:22:26 PDT 1993 by meehan                   *)
<* PRAGMA LL *>

INTERFACE MacModel;

IMPORT TextPortClass;

TYPE T <: TextPortClass.Model;

END MacModel.

(* The Mac model supports only a single selection, Primary. Is it not
   an alias for either Source or Target. A Primary selection in a
   non-readonly textport is always in replace-mode.

   The conventions for the Mac model are taken from Apple's <I>Human
   Interface Guidelines</I> [pages 106-114]AppleUI.

   The first unmodified downclick establishes the <I>anchor point</I>.
   If the user then drags the mouse, the upclick establishes the <I>
   active end</I>; the range between the anchor point and the active end
   is the Primary selection, and it is highlighted. If the user
   releases the mouse without dragging, that establishes the <I>
   type-in point</I>, and there is no selection or highlighting.

   Shift-downclick extends (or reduces) the primary selection and
   establishes the new active end.

   Double-clicking selects a word; dragging after a double-click
   extends the selection in word-size increments.

   The Mac model implements the following Apple guidelines:

   <BQ>

   When a Shift-arrow key combination is pressed, the active end of
   the selection moves and the range over which it moves becomes
   selected. ...  Option-Shift-Left Arrow selects the whole word that
   contains the character to the left of the insertion point (just
   like double-clicking on a word).

   In a text application, pressing Shift and either Left Arrow or
   Right Arrow selects a single character.  Assuming that the Left
   Arrow key was used, the anchor point of the selection is on the
   right side of the selection, the active end on the left.  Each
   subsequent Shift-Left Arrow adds another character to the left side
   of the selection.  A Shift-Right Arrow at this point shrinks the
   selection.

   Pressing Option-Shift and either Left Arrow or Right Arrow ...
   selects the entire word containing the character to the left of the
   insertion point.  Assuming Left Arrow was pressed, the anchor point
   is at the right end of the word, the active end at the left.  Each
   subsequent Option-Shift-Left Arrow adds another word to the left
   end of the selection...

   When a block of text is selected, either with a pointing device or
   with cursor keys, pressing either Left Arrow or Right Arrow
   deselects the range.  If Left Arrow is pressed, the insertion point
   goes to the beginning of what had been the selection.  If Right
   Arrow is pressed, the insertion point goes to the end of what had
   been the selection.

   [From page 83] When the user chooses Cut, ... the place where the
   selection used to be becomes the new selection. ... In text, the
   new selection is an insertion point [and the highlighting is
   removed].

   Paste ... inserts the contents of the Clipboard [Source] into the
   document, replacing the current selection [i.e., Primary selections
   are always replace-mode].  If there is no current selection, it's
   inserted at the insertion point.... After a Paste, the new
   selection is ... an insertion point immediately after the pasted
   text. [In either case, there is no highlighting.]

   </BQ>

   In documentation from Apple, Mac keybindings are typically
   described in terms of ``command'' and ``option'' modifiers. DEC
   keyboards and the X server do not use those terms, but a
   correspondence can be established. The Mac model uses the value of
   environment variable "MacCommandModifier"<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>MacCommandModifier</SPAN>
</SPAN>

   to name the X-modifier that the user would like to behave as if it
   were the ``command'' key. The choices are:
| lock, control, mod1, mod2, mod3, mod4, `and` mod5 
   (Case is not significant in these names.) The default is "control".
   Consult the manpage for "xmodmap(1)" for more information on these 
   modifiers.

   Similarly, the Mac model uses the environment variable
   "MacOptionModifier"<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>MacOptionModifier</SPAN>
</SPAN>
 to name the X-modifier
   that the user would like to behave as if it were the ``option''
   key. The choices are the same as in the list above.  The default is
   "mod1".

   The following commands are implemented in the Mac model:

<PRE>
LongCommandKeyName  This is just a tab-setting line.
command-c  <B>Copy</B>
command-v  <B>Paste</B>
command-x  <B>Cut</B>
command-z  <B>Undo</B>
command-shift-z  <B>Redo</B>
</PRE>

   The Mac model supports the Apple standards for typing extended
   characters, insofar as the resulting characters are defined for ISO
   Latin-1. For example, option-g produces the copyright symbol,
   &copy;, but option-shift-7, which produces a double dagger,
   on the Macintosh, produces no key in the Mac model, since
   the double-dagger is not in ISO Latin-1. The Mac model supports all
   the two-character sequences, such as option-e followed by ``a'' to
   produce ``a'' with an acute accent, a. The complete table appears
   on page&nbsp;<A REL=REF.PAGE HREF="MacExtendedCharacters"> [MacExtendedCharacters] </A>. *)
