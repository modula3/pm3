(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 14 18:51:55 PDT 1993 by meehan *)
(*      modified on Sun Jan 31 21:58:53 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:34 PDT 1992 by muller *)
<* PRAGMA LL *>

(* A "ScrollerVBT" is a scrollbar with an orientation along an <EM>
   axis</EM>.  For the sake of brevity in this interface, we'll only talk
   about vertical scrollers.  For horizontal scrollers, replace
   phrases like <I>top and bottom edges</I> by <I>left and right
   edges</I>, and so on.

   Like a "NumericVBT", a "ScrollerVBT" provides a <EM>
   bounded-value</EM> abstraction.  That is, a "ScrollerVBT" has a <I>
   value</I> associated with it, and that value is guaranteed to stay
   within some bounds.  Various user gestures change the value and
   invoke a "callback" method on the "ScrollerVBT".  The callback
   method can inquire the value of the scrollbar, and can change the
   value and bounds.

   Visually, a scrollbar contains a <EM>stripe</EM> that spans some
   fraction of the height of the scrollbar and is slightly
   narrower than the scrollbar.  The stripe represents the value
   of the scrollbar.  Various user-gestures cause the stripe to
   move.

   More specifically, the state of a "ScrollerVBT" consists of
   five integer quantities: "min", "max", "thumb", "step", and
   "value".  The "value" is guaranteed to stay in the range "[min
   ..  max-thumb]".  Visually, the "value" is represented by the
   position (top edge) of a stripe in the scroller, and "thumb"
   by the length of the stripe.  The amount that "value" should
   change when continuous scrolling is given by "step", the <EM>
   stepping</EM> amount.

   Although each "VBT" class that uses a "ScrollerVBT" is free to
   associate any meaning with the length of the stripe, the
   following convention is suggested for using scrollbars to view
   an object:

   <BQ>

   The ratio of the height of the stripe to the height of the
   scrollbar should be the same as the ratio of the amount of the
   object visible vertically to its entire height.  The position
   of top of the stripe reflects the position of top of the view
   of the object within the entire object.

   </BQ>

   Here is some terminology and the user-interface provided by a
   "ScrollerVBT":

   <UL> <LI>To <EM>scroll</EM><SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>scrolling</SPAN>
</SPAN>
 means
   to left-click or right-click in the scrollbar.

   <LI>You need to release the button relatively quickly, or else
   you'll start <EM>continuous scrolling</EM>.  You stop continuous
   scrolling by releasing the button, by chord-cancelling<SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>chord</SPAN>
</SPAN>

   or by moving the mouse.

   <LI>When you move the mouse, you are then using <EM>proportional
   scrolling</EM>.  This means that the more that you move the mouse
   vertically, the more the stripe will be moved in the direction of
   the mouse movement.  You stop proportional scrolling by upclicking
   or chord-cancelling.

   <LI>The left and right buttons are inverses: the left button
   moves the stripe downward and the right button moves the stripe
   upward.

   <LI>You <EM>thumb</EM><SPAN CLASS=INDEX.MARK>
<SPAN CLASS=INDEX.KEY>thumb</SPAN>
</SPAN>
 with a middle-click.  The top of
   the stripe moves to the position of the cursor.  Thus, middle-click
   above the top of the stripe moves the stripe up, and middle-click
   below the top moves the stripe down.

   <LI>Middle-drag causes <EM>continuous thumbing</EM>.  As you drag to
   a new position, the top of the stripe moves to match the current
   cursor position.  You stop continuous thumbing by middle-upclicking
   or chord-canceling.

   </UL>

   If you want a different user interface, you need to subclass
   various methods (e.g., a "thumb", "scroll", "autoscroll") of the
   scrollbar.  These methods are defined in the "ScrollerVBTClass"
   interface. *)

INTERFACE ScrollerVBT;

IMPORT Axis, PaintOp, VBT;

TYPE
  T <: Public;
  Private <: VBT.T;
  Public = Private OBJECT
           METHODS
             <* LL.sup = VBT.mu *>
             init (axis  : Axis.T;
                   min   : INTEGER;
                   max   : INTEGER;
                   colors: PaintOp.ColorQuad;
                   step  : CARDINAL            := 1;
                   thumb : CARDINAL            := 0  ): T;
             <* LL = VBT.mu *>
             callback (READONLY cd: VBT.MouseRec);
           END;

(* The call to "v.init(...)" initializes "v" as a
   "ScrollerVBT" in the "axis" orientation.  It is
   displayed using "colors".

   The implementation calls "v.callback(cd)" after "v"'s value
   has been changed by the user; it is not called when the value
   is changed as the result of calls to "Put" or "PutBounds".
   The default "callback" method is a no-op. *)

PROCEDURE Put (v: T; n: INTEGER);
<* LL.sup = VBT.mu *>
(* Change the value of "v", projected to "[min .. max-thumb]", and
   mark "v" for redisplay. *)

PROCEDURE PutBounds (v    : T;
                     min  : INTEGER;
                     max  : INTEGER;
                     thumb: CARDINAL  := 0);
<* LL.sup = VBT.mu *>
(* Set the bounds, project "v"'s value into "[min .. max-thumb]", and
   mark "v" for redisplay. *)

PROCEDURE PutStep (v: T; step: CARDINAL);
<* LL.sup = VBT.mu *>
(* Change the amount that "v"'s value should change while
   continuous scrolling to "step".  If "step = 0", scrolling will
   be disabled. *)

PROCEDURE Get      (v: T): INTEGER;  <* LL.sup = VBT.mu *>
PROCEDURE GetMin   (v: T): INTEGER;  <* LL.sup = VBT.mu *>
PROCEDURE GetMax   (v: T): INTEGER;  <* LL.sup = VBT.mu *>
PROCEDURE GetThumb (v: T): CARDINAL; <* LL.sup = VBT.mu *>
PROCEDURE GetStep  (v: T): CARDINAL; <* LL.sup = VBT.mu *>
(* Return the current "value", "min", "max", "thumb", and
   "step". *)

END ScrollerVBT.

