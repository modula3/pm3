(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Sun Mar 21 16:29:22 PST 1993 by meehan *)
(*      modified On Tue Jun 16 13:12:45 PDT 1992 by muller *)
(*      modified On Fri Mar 20 10:06:05 PST 1992 by jdd    *)
(*      modified On Tue May 15 17:02:37 PDT 1990 by mcjones *)

MODULE VTBase;

IMPORT Point, Rd, Thread;
IMPORT VTDef, VTRd;

(* The routines in this module deal with the division of the buffer into
   screen lines.

   ComputeLine, given the purported beginning of a screen line, computes
   its end and other of its characteristics. Up, given an arbitrary buffer
   position, computes the beginning of the line "n" lines above the line
   that the position lies on. (If "n" is 0, this is the beginning of the
   line that the position lies on.) Down, given the purported beginning of
   a line, computes the beginning of the line "n" lines below that line.
   (If "n" is 0, Down is the identity function.)

   All of these procedures check whether they can solve or simplify the
   problem quickly using the information in the view's "virtual" structure;
   this unfortunately makes them less readable. They also special-case on
   simple cases before performing expensive set-up operations necessary for
   more complicated cases (e.g., setting up a reader); this further
   complicates their logic.

   Routine UnsafeLocateLine, given an index into the buffer, returns its
   line number in the view (or -1 if it is above the view, or -2 if it is
   below). UnsafeLocateLine assumes that the view's virtual structures are
   not dirty (hence its name). *)

TYPE VirtualLine = VTDef.VirtualLine;

(* ComputeLine computes the characteristics of a screen line starting at
   "from"; it returns the index after the end. ("From" is believed to be at
   the beginning of a screen line.) "Max" is set to the index after the
   last character examined to make the decision (the first was "from").
   "Turned" is set to whether the end of the screen line is turned (if the
   screen line does not end in a new-line and is not at the end of the
   buffer). "Width" is set to the width in pixels needed to display the
   text. *)

EXCEPTION Overflow; (* raised when the line gets too long *)
  (* local to ComputeLine *)

PROCEDURE ComputeLine (             view  : View;
                                    avail : Pixels;
                                    from  : I;
                       VAR (* OUT*) max   : I;
                       VAR (* OUT*) turned: BOOLEAN;
                       VAR (* OUT*) width : Pixels    ): I
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR length: I := view.vt.length;
  BEGIN
    <* ASSERT 0 <= from AND from <= length + 1 *>

    (* try the very easy case first *)
    IF from >= length THEN
      max := MAX(from, length + 1);
      turned := FALSE;
      IF from = length THEN width := 1 ELSE width := 0 END;
      RETURN max
    END;

    (* try to solve the problem using the virtual structure *)
    IF avail = view.lineWidth AND NOT view.virtual.bodyDirty
         AND from >= view.virtual.line[0].virtualLine.from
         AND from < view.virtual.line[view.virtual.lines].virtualLine.from
      THEN
      WITH
        lineNo = UnsafeLocateLine(view, from),
        vl = view.virtual.line[lineNo].virtualLine DO
        IF vl.from = from THEN
          max := vl.max;
          turned := vl.turned;
          width := vl.width;
          RETURN vl.to
        END
      END
    END;

    (* solve the problem the hard way *)
    VTRd.InitReaderIx(view.vt, from);
    VAR
      i    : I     := from;     (* the reader index *)
      w    : Pixels := 0;        (* the width so far *)
      ow   : Pixels := 0;        (* the previous value of w *)
      white        := FALSE;    (* true after a whitespace *)
      okI  : I     := 0;        (* index of a possible break *)
      okw  : Pixels := 0;        (* the width up to okI *)
      c    : CHAR;
      widthMap: ARRAY CHAR OF Pixels := view.vScreenFont.vScreenFont.width;
      printable: SET OF CHAR := view.vScreenFont.vScreenFont.vFont.vFont.printable;
    BEGIN
      TRY
        LOOP
          (* can we read a character? *)
          IF i >= length THEN   (* no; we're at eof *)
            <* ASSERT i <= length + 1 *>
            IF white THEN okI := i; okw := w END;
            turned := i # from;
            w := w + 1;         (* width of the caret *)
            i := length + 1;
            IF w > avail THEN RAISE Overflow END;
            max := i;
            width := w;
            RETURN i
          END;

          (* read a character *)
          c := Rd.GetChar(view.vt.rd);
          i := i + 1;
          (* handle the cases *)
          IF c = ' ' THEN
            w := w + widthMap[' '];
            IF w > avail THEN RAISE Overflow END;
            white := TRUE
          ELSIF c = '\n' THEN
            w := w + widthMap['\n'];
            IF white THEN okI := i - 1; okw := ow END;
            IF w > avail THEN RAISE Overflow END;
            max := i;
            turned := FALSE;
            width := w;
            RETURN i
          ELSIF c = '\t' AND '\t' IN printable THEN
            IF w + widthMap[' '] > avail THEN RAISE Overflow END;
            w := w + widthMap[' '] + widthMap['\t'] - 1;
            w := MIN(w - w MOD widthMap['\t'], avail);
            white := TRUE
          ELSE
            w := w + widthMap[c];
            IF white THEN okI := i - 1; okw := ow END;
            IF w > avail THEN RAISE Overflow END;
            white := FALSE
          END;
          ow := w
        END
      EXCEPT
      | Overflow =>             (* line got too long; break *)
          max := i;
          turned := TRUE;
          IF okI > 0 THEN
            width := okw;
            RETURN okI
          ELSE
            width := ow;
            RETURN MAX(i - 1, from + 1)
          END
      END
    END
  END ComputeLine;

(* Up computes the beginning of the screen line "n" lines above the screen
   line that "place" is on. ("Place" is not believed to be at the beginning
   of a screen line.) "Min" and "max" are set to a half-open interval that
   includes a set of buffer positions that imply this result. "Turned" is
   set to whether the beginning of that screen line is turned (if the
   screen line is not preceded by a new-line and is not at the beginning of
   the buffer). If fewer than "n" screen lines exist, the beginning of the
   buffer is returned. We repeatedly reduce the problem by searching
   backwards for a position that is known to be the beginning of a line,
   then computing lines forward from it until we reach the point of
   interest. We remember the indices of the lines thus computed in a table;
   we index into this table to solve or to simplify the problem. *)

PROCEDURE Up (             view : View;
                           avail: Pixels;
                           place: I;
                           n    : CARDINAL;
              VAR (* OUT*) start: VirtualStart)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  CONST
    BufSize   = 512;
    ChunkSize = 100;
  VAR
    at, bI, rI, rrI, mI: I;
    l, nB, nn, lineNo  : CARDINAL;
    rBuf               : ARRAY [0 .. BufSize - 1] OF CHAR;
    ll                 : ARRAY [0 .. ChunkSize - 1] OF VirtualLine;
  BEGIN
    <* ASSERT ((place >= 0) AND (place <= view.vt.length)) *>
    start.turned := FALSE;      (* default *)
    rrI := place;
    nB := 0;
    LOOP                        (* reduce problem *)
      (* try an easy solution *)
      IF place = 0 THEN
        start.at := 0;
        start.min := -1;
        start.max := 0;
        RETURN;
      END;
      (* try to reduce the problem using the virtual structure *)
      IF avail = view.lineWidth AND NOT view.virtual.dirty
           AND (place >= view.virtual.line[0].virtualLine.from)
           AND (place
                  < view.virtual.line[view.virtual.lines].virtualLine.from)
        THEN
        lineNo := UnsafeLocateLine(view, place);
        l := MIN(lineNo, n);    (* we can look "l" lines back *)
        (* reduce the problem *)
        place := view.virtual.line[lineNo - l].virtualLine.from;
        n := n - l;
        IF n = 0 THEN
          (* we've solved the problem *)
          start.at := place;
          start.min := view.virtual.start.min; (* conservative *)
          IF lineNo - l > 0 THEN
            start.turned :=
              view.virtual.line[lineNo - l - 1].virtualLine.turned;
            start.max := view.virtual.line[lineNo - l - 1].virtualLine.max
          ELSE
            start.turned := view.virtual.start.turned;
            start.max := view.virtual.start.max
          END;
          RETURN;
        END;
      ELSE
        (* find some beginning of (buffer) line at or before place *)
        rI := place;            (* next reverse read is at rI - 1 *)
        mI := place;            (* after the last place we've looked *)
        IF rI - rrI > nB THEN rrI := rI END;
        LOOP
          rI := rI - 1;
          IF rI < rrI THEN
            IF rI < 0 THEN rI := -1; bI := 0; EXIT END;
            nn := rI MOD BufSize;
            rrI := rI - nn;
            VTRd.InitReaderIx(view.vt, rrI);
            nB := nn + 1;
            WITH n = Rd.GetSub(view.vt.rd, SUBARRAY(rBuf, 0, nB)) DO
              <* ASSERT n = nB *>
            END
          END;
          IF rBuf[rI - rrI] = '\n' THEN
            bI := rI + 1;       (* beginning of a buffer line *)
            EXIT
          END
        END;
        at := bI;               (* the latest beginning of buffer line not
                                   after place *)
        l := 0;                 (* index of lines computing forward *)
        LOOP                    (* find recent line breaks up to place *)
          WITH z_4 = ll[l MOD ChunkSize] DO
            z_4.from := at;
            IF at = place THEN EXIT END;
            z_4.to :=
              ComputeLine(view, avail, at, z_4.max, z_4.turned, z_4.width);
            z_4.valid := TRUE;
            IF z_4.max > mI THEN mI := z_4.max END;
            IF z_4.to = at THEN
              (* roadblock in breaking; give arbitrary answer *)
              start.at := place;
              start.min := rI;
              start.max := mI;
              start.turned := TRUE;
              RETURN
            END;
            IF z_4.to > place THEN EXIT END;
            at := z_4.to
          END;
          l := l + 1
        END;
        nn := MIN(n, MIN(l, ChunkSize - 1));
        (* how far back we can look *)
        place := ll[(l - nn + ChunkSize) MOD ChunkSize].from;
        (* set place to bol *)
        n := n - nn;            (* maybe reduce n *)
        IF n = 0 THEN
          start.at := place;
          start.min := rI;
          start.max := mI;
          start.turned := nn # l;
          RETURN
        END
      END;
      (* place is now at the beginning of a line; try an easy solution *)
      IF place = 0 THEN
        start.at := 0;
        start.min := -1;
        start.max := 0;
        RETURN
      END;
      (* iterate *)
      place := place - 1;
      n := n - 1;
    END;
  END Up;

EXCEPTION Iterate; (* local to Down *)

PROCEDURE Down (view: View; from: I; n: CARDINAL): I
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  (* Down computes the beginning of the screen line "n" lines below the
     screen line that "from" is on. ("From" is believed to be at the
     beginning of a screen line.) If fewer than "n" screen lines exist, the
     end of the buffer is returned. *)
  VAR
    m        : I;
    t        : BOOLEAN;
    w        : Pixels;
    l, lineNo: INTEGER;
  BEGIN
    WHILE n > 0 DO
      (* try to reduce problem using virtual structure *)
      TRY
        IF NOT view.virtual.bodyDirty
             AND (from >= view.virtual.line[0].virtualLine.from)
             AND (from < view.virtual.line[
                           view.virtual.lines].virtualLine.from) THEN
          lineNo := UnsafeLocateLine(view, from);
          IF view.virtual.line[lineNo].virtualLine.from = from THEN
            l := MIN(view.virtual.lines - lineNo, n);
            IF l > 0 THEN
              n := n - l;
              from := view.virtual.line[lineNo + l].virtualLine.from;
              RAISE Iterate
            END
          END
        END;
        (* do it the hard way *)
        from := ComputeLine(view, view.lineWidth, from, m, t, w);
        n := n - 1
      EXCEPT
      | Iterate =>
      END
    END;
    RETURN from;
  END Down;

PROCEDURE UnsafeLocateLine (view: View; place: I): INTEGER RAISES {} =
  VAR i, j, k: INTEGER;
  BEGIN
    IF place < view.virtual.line[0].virtualLine.from THEN
      RETURN -1;
    ELSIF place >= view.virtual.line[view.virtual.lines].virtualLine.from
      THEN
      RETURN -2;
    ELSE
      i := 0;
      j := view.virtual.lines - 1;
      WHILE i < j DO
        k := (i + j + 1) DIV 2;
        IF view.virtual.line[k].virtualLine.from <= place THEN
          i := k
        ELSE
          j := k - 1
        END
      END;
      RETURN i;
    END;
  END UnsafeLocateLine;

PROCEDURE UnsafeLocatePoint (             view : View;
                                          place: I;
                             VAR (* OUT*) p    : Point.T;
                                          off  : CARDINAL  := 1)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    i    : INTEGER;
    w    : Pixels;
    index: I;
    c    : CHAR;
  BEGIN
    i := UnsafeLocateLine (view, place);
    IF i < 0 THEN
      p.v := i
    ELSE
      WITH z_7 = view^ DO
        WITH z_8 = z_7.virtual.line [i] DO
          p.v := z_7.rect.text.north + i * z_7.lineSpacing;
          VTRd.InitReaderIx (z_7.vt, z_8.virtualLine.from);
          WITH z_9 = z_7.vScreenFont^ DO
            WITH z_10 = z_9.vScreenFont.vFont^ DO
              w := 0;
              FOR z_11 := z_8.virtualLine.from TO place - off DO
                index := z_11;
                c := Rd.GetChar (z_7.vt.rd);
                IF c = '\n' THEN
                  w := z_7.lineWidth
                ELSE
                  w := w + z_9.vScreenFont.width [c];
                  IF (c = '\t') AND ('\t' IN z_10.vFont.printable) THEN
                    w := w - 1 + z_9.vScreenFont.width [' '];
                    w := w - w MOD z_9.vScreenFont.width ['\t']
                  END
                END
              END
            END
          END
        END;
        p.h := MIN (z_7.rect.text.west + w, z_7.rect.text.east)
      END
    END
  END UnsafeLocatePoint;

BEGIN
END VTBase.
