(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* BatchRep.def, Fri May 11 9:34:18 PDT 1990 by steveg *)
<*PRAGMA LL*>

(* Last modified on Mon Oct  4 11:35:45 PDT 1993 by sfreeman *)
(* modified on Mon Feb 24 13:56:36 PST 1992 by muller *)
(* modified on Thu Jun 6 0:22:49 PDT 1991 by gnelson *)
(* modified on Fri May 11 15:17:53 PDT 1990 by steveg *)

INTERFACE BatchRep;

(* Unsafe representation of batches of Trestle painting commands.

   Index: VBT *)

IMPORT Batch, Rect, Word, BatchUtil, Point;

REVEAL Batch.T = UNTRACED BRANDED REF Rec;

TYPE
  T = Batch.T;
  Array = UNTRACED REF ARRAY OF Word.T;

  Rec =
    RECORD
      b   : Array;               (* never nil *)
      next: ADDRESS;
      (* Address of next free word in b; never nil.  The batch is empty iff
         next = ADDR(b^[0]).  The total length of the painting commands in
         the batch, in words, is (next - ADR(b^[0])) DIV
         BYTESIZE(Word.T). *)
      clip: Rect.T;
      (* All of the paint commands in b will have their clipping rectangles
         intersected with clip before painting.  (But see firstSingle
         below.) *)
      clipped: BatchUtil.ClipState;
      (* (clipped # Unclipped) => The clip of every paint command in b is a
         subset of clip; therefore the intersection refered to in the last
         comment is unnecessary.  (clipped = Tight) => The clipping
         rectangle cannot be made smaller without changing the meaning of
         the batch. *)
      scrollSource: Rect.T;
      (* This rectangle equals the join of the source of the scrolling
         commands in the batch. *)
      link: T;
      (* used for the free list of batches and by PipelineSplit. *)
      excessBegins: INTEGER;
      (* used while the batch is initially being created by individual
         paint commands; represents the excess of BeginGroup's over
         EndGroup's in the batch. *)
      firstSingle: ADDRESS;
      (* used while the batch is initially being created by individual
         paint commands; ADDR(b^[0]) <= firstSingle <= next.  The first
         interval contains painting operations to which clip and clipped
         apply.  The second interval contains painting operations generated
         by individual painting commands, which need to be clipped by the
         domain of the VBT containing this batch. *)
      containsPicture: BOOLEAN;
      (* picture commands are associated with an PictureRep.Completion
         object which keeps a reference count so callers can find out when
         the picture data may be changed.  Batch.Free decrements the count
         of any pictures in the batch.  This flag is TRUE when the Batch
         contains any pictures. *)
    END;

PROCEDURE ClipSub (READONLY clip   : Rect.T;
                   VAR      batch  : ARRAY OF Word.T;
                            st, len: INTEGER          );
(* Like BatchUtil.Clip, but uses clip instead of the batch's clip and
   doesn't affect the clip or clipstate of the batch and affects only the
   painting operations in batch starting with batch[st] and proceeding for
   len words. *)

PROCEDURE ClipSubAndTighten (READONLY clip        : Rect.T;
                             VAR      batch       : ARRAY OF Word.T;
                                      st, len     : INTEGER;
                             VAR      scrollSource: Rect.T           ):
  Rect.T;
(* Like ClipSub, but also returns the join of the clipping rectangles and
   the join of the scroll sources that it computes. *)

PROCEDURE TranslateSub (VAR      btch   : ARRAY OF Word.T;
                                 st, len: INTEGER;
                        READONLY delta  : Point.T          );
(* Like "BatchUtil.Translate", but doesn't affect the clip of the batch and
   affects only the painting operations in batch starting with batch[st]
   and proceeding for len words. *)

END BatchRep.

