(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun May 30 10:12:44 PDT 1993 by meehan    *)
(*      modified on Tue Jun 16 13:08:41 PDT 1992 by muller    *)
(*      modified on Wed Aug 16 10:23:23 PDT 1989 by brooks    *)
(*      modified on Thu Aug 3 00:41:52 1989 by chan           *)
(*      modified on Wed May 24 15:47:44 PDT 1989 by mbrown    *)
(*      modified on Fri Jun 3 12:21:14 PDT 1988 by mcvl       *)
<* PRAGMA LL *>

MODULE MTextUnit;

IMPORT ISOChar, MText, MTextRd, Rd, Thread;

<* FATAL Rd.EndOfFile, Rd.Failure, Thread.Alerted *>

VAR
  mu      := NEW (MUTEX);
  rd, rrd := NEW (MTextRd.T);    <* LL = mu *>

PROCEDURE RunExtent (t: T; index: INTEGER; READONLY includedChars := WordRun):
  Extent =
  VAR e: Extent;
  BEGIN
    WITH len   = MText.Length (t),
         index = MIN (MAX (index, 0), len) DO
      e.left := index;
      e.right := index;
      IF index = len THEN e.inside := FALSE; RETURN e END;
      LOCK mu DO
        EVAL rd.init (t, index);
        EVAL rrd.init (t, index, reverse := TRUE);
        e.inside := Rd.GetChar (rd) IN includedChars;
        INC (e.right);
        IF e.inside THEN
          e.right := FindChar (rd, ISOChar.All - includedChars, e.right);
          e.left := FindChar (rrd, ISOChar.All - includedChars, e.left, -1)
        ELSE
          e.right := FindChar (rd, includedChars, e.right);
          e.left := FindChar (rrd, includedChars, e.left, -1)
        END;
        RETURN e
      END
    END
  END RunExtent;

PROCEDURE StartOfRun (t: T; index: INTEGER; READONLY includedChars := WordRun):
  INTEGER =
  VAR e := RunExtent (t, index, includedChars);
  BEGIN
    IF e.inside THEN RETURN e.left END;
    e := RunExtent (t, e.left - 1, includedChars);
    IF e.inside THEN RETURN e.left ELSE RETURN -1 END
  END StartOfRun;

PROCEDURE IsStartOfRun (         t            : T;
                                 index        : INTEGER;
                        READONLY includedChars            := WordRun):
  BOOLEAN =
  VAR e := RunExtent (t, index, includedChars);
  BEGIN
    RETURN e.inside AND index = e.left
  END IsStartOfRun;

PROCEDURE EndOfRun (t: T; index: INTEGER; READONLY includedChars := WordRun):
  INTEGER =
  VAR e := RunExtent (t, index, includedChars);
  BEGIN
    IF e.inside THEN RETURN e.right END;
    e := RunExtent (t, e.right, includedChars);
    IF e.inside THEN RETURN -1 ELSE RETURN e.left END
  END EndOfRun;

PROCEDURE IsEndOfRun (t: T; index: INTEGER; READONLY includedChars := WordRun):
  BOOLEAN =
  VAR e := RunExtent (t, index, includedChars);
  BEGIN
    RETURN NOT e.inside AND index = e.left
  END IsEndOfRun;

(* Lines *)

PROCEDURE LineExtent (t: T; index: INTEGER): Extent =
  VAR
    len         := MText.Length (t);
    e  : Extent;
  BEGIN
    e.inside := TRUE;
    index := MIN (MAX (index, 0), len);
    LOCK mu DO
      EVAL rd.init (t, index);
      EVAL rrd.init (t, index, reverse := TRUE);
      e.left := FindChar (rrd, SET OF CHAR {'\n'}, index, -1);
      e.right := FindChar (rd, SET OF CHAR {'\n'}, index);
      IF e.right < len THEN INC (e.right) END
    END;
    RETURN e
  END LineExtent;

PROCEDURE LineInfo (t: T; index: INTEGER): LineRec =
  VAR z: LineRec;
  BEGIN
    LineFacts (
      t, index, z.left, z.leftMargin, z.rightMargin, z.rightEnd, z.right);
    RETURN z
  END LineInfo;

PROCEDURE LineFacts (             t          : T;
                                  index      : INTEGER;
                     VAR (* out*) left       : INTEGER;
                     VAR (* out*) leftMargin : INTEGER;
                     VAR (* out*) rightMargin: INTEGER;
                     VAR (* out*) rightEnd   : INTEGER;
                     VAR (* out*) right      : INTEGER  ) =
  VAR len: INTEGER;
  BEGIN
    len := MText.Length (t);
    index := MIN (MAX (index, 0), len);
    LOCK mu DO
      EVAL rd.init (t, index);
      EVAL rrd.init (t, index, reverse := TRUE);
      left := FindChar (rrd, SET OF CHAR {'\n'}, index, -1);
      Rd.Seek (rd, left);
      leftMargin := FindChar (rd, SET OF CHAR {'\n'} + NonBlankRun, left);
      Rd.Seek (rd, leftMargin);
      rightEnd := FindChar (rd, SET OF CHAR {'\n'}, leftMargin);
      right := rightEnd;
      IF right < len THEN INC (right) END;
      IF rightEnd = leftMargin THEN
        rightMargin := rightEnd
      ELSE
        Rd.Seek (rrd, len - rightEnd); (* don't forget to reverse the
                                          index. *)
        rightMargin := FindChar (rrd, NonBlankRun, rightEnd, -1)
      END
    END
  END LineFacts;

PROCEDURE StartOfLine (t         : T;
                       index     : INTEGER;
                       leftOption            := LineOption.IncludeBlanks):
  INTEGER =
  BEGIN
    WITH r = LineInfo (t, index) DO
      IF leftOption = LineOption.ExcludeBlanks THEN
        RETURN r.leftMargin
      ELSE
        RETURN r.left
      END
    END
  END StartOfLine;

PROCEDURE IsStartOfLine (t         : T;
                         index     : INTEGER;
                         leftOption            := LineOption.IncludeBlanks):
  BOOLEAN =
  BEGIN
    RETURN index = StartOfLine (t, index, leftOption)
  END IsStartOfLine;

PROCEDURE EndOfLine (t          : T;
                     index      : INTEGER;
                     rightOption            := LineOption.IncludeNewline):
  INTEGER =
  BEGIN
    WITH r = LineInfo (t, index) DO
      IF rightOption = LineOption.ExcludeBlanks THEN
        RETURN r.rightMargin
      ELSIF rightOption = LineOption.IncludeBlanks THEN
        RETURN r.rightEnd
      ELSE
        RETURN r.right
      END
    END
  END EndOfLine;

PROCEDURE IsEndOfLine (t          : T;
                       index      : INTEGER;
                       rightOption            := LineOption.IncludeNewline):
  BOOLEAN =
  BEGIN
    RETURN index = EndOfLine (t, index, rightOption)
  END IsEndOfLine;

PROCEDURE IsBlankLine (t: T; i: INTEGER): BOOLEAN =
  PROCEDURE f (rd: Rd.T): BOOLEAN =
    (* Is the first non-whitespace character a newline? *)
    BEGIN
      TRY
        LOOP
          CASE Rd.GetChar (rd) OF
          | '\n' => RETURN TRUE
          | ' ', '\t', '\f' =>
          ELSE
            RETURN FALSE
          END
        END
      EXCEPT
      | Rd.EndOfFile => RETURN TRUE
      END
    END f;
  BEGIN
    LOCK mu DO
      EVAL rd.init (t, i);
      EVAL rrd.init (t, i, reverse := TRUE);
      RETURN f (rrd) AND f (rd)
    END
  END IsBlankLine;

PROCEDURE BlankLinesExtent (t: T; index: INTEGER): Extent =
  VAR
    j  : INTEGER;
    len          := MText.Length (t);
    e            := Extent {index, index, TRUE};
  BEGIN
    j := index;
    LOOP
      IF NOT IsBlankLine (t, j) THEN
        IF j = index THEN e.inside := FALSE END;
        EXIT
      END;
      e.left := j;
      j := StartOfLine (t, j - 1)
    END;
    j := index;
    LOOP
      IF NOT IsBlankLine (t, j) THEN EXIT END;
      IF j >= len THEN e.right := len; EXIT END;
      e.right := j + 1;
      j := EndOfLine (t, e.right)
    END;
    RETURN e
  END BlankLinesExtent;

(* Paragraphs *)

PROCEDURE ParagraphExtent (t: T; index: INTEGER): Extent =
  VAR
    e    : Extent;
    r, rr: NewlineRec;
    len               := MText.Length (t);
  BEGIN
    index := MIN (MAX (index, 0), len);
    e.left := index;
    e.right := index;
    IF index = len THEN e.inside := FALSE; RETURN e END;
    LOCK mu DO
      EVAL rd.init (t, index);
      EVAL rrd.init (t, index, reverse := TRUE);
      r := ToNewline (rd, e.right);
      rr := ToNewline (rrd, e.left, -1, FALSE);
      e.right := r.index;
      e.left := rr.index;
      e.inside := NOT r.allBlanks OR NOT rr.allBlanks;
      LOOP
        IF r.eof THEN EXIT END;
        r := ToNewline (rd, e.right);
        IF r.allBlanks = e.inside THEN EXIT END;
        e.right := r.index
      END;
      LOOP
        IF rr.eof THEN EXIT END;
        WITH c = Rd.GetChar (rrd) DO <* ASSERT c = '\n' *> END;
        rr := ToNewline (rrd, e.left - 1, -1, FALSE);
        IF rr.allBlanks = e.inside THEN EXIT END;
        e.left := rr.index
      END;
      RETURN e
    END
  END ParagraphExtent;

PROCEDURE IsStartOfParagraph (t: T; index: INTEGER): BOOLEAN =
  VAR e := ParagraphExtent (t, index);
  BEGIN
    RETURN e.inside AND index = e.left
  END IsStartOfParagraph;

PROCEDURE IsEndOfParagraph (t: T; index: INTEGER): BOOLEAN =
  VAR e := ParagraphExtent (t, index);
  BEGIN
    RETURN NOT e.inside AND index = e.left
  END IsEndOfParagraph;

PROCEDURE StartOfParagraph (t: T; index: INTEGER): INTEGER =
  VAR e := ParagraphExtent (t, index);
  BEGIN
    IF e.inside THEN RETURN e.left END;
    e := ParagraphExtent (t, e.left - 1);
    IF e.inside THEN RETURN e.left ELSE RETURN -1 END
  END StartOfParagraph;

PROCEDURE EndOfParagraph (t: T; index: INTEGER): INTEGER =
  VAR e := ParagraphExtent (t, index);
  BEGIN
    IF e.inside THEN RETURN e.right END;
    e := ParagraphExtent (t, e.right);
    IF e.inside THEN RETURN -1 ELSE RETURN e.left END
  END EndOfParagraph;

(*************)
(* Utilities *)
(*************)

TYPE
  NewlineRec = RECORD
                 allBlanks, eof: BOOLEAN;
                 index         : INTEGER
               END;

PROCEDURE ToNewline (rd: Rd.T; i: INTEGER; inc := +1; includeNewline := TRUE):
  NewlineRec =
  <* LL = mu *>
  VAR
    ch: CHAR;
    r        := NewlineRec {allBlanks := TRUE, eof := FALSE, index := i};
  BEGIN
    TRY
      LOOP
        ch := Rd.GetChar (rd);
        INC (r.index, inc);
        IF ch = '\n' THEN
          IF NOT includeNewline THEN
            Rd.UnGetChar (rd);
            DEC (r.index, inc)
          END;
          EXIT
        END;
        IF ch IN NonBlankRun THEN r.allBlanks := FALSE END
      END
    EXCEPT
    | Rd.EndOfFile => r.eof := TRUE
    END;
    RETURN r
  END ToNewline;

PROCEDURE FindChar (rd: Rd.T; cs: SET OF CHAR; i: INTEGER; inc: INTEGER := 1):
  INTEGER =
  <* LL = mu *>
  (* Read characters until EOF or a character in cs is found.  Return the
     index of the character. *)
  BEGIN
    TRY
      LOOP IF Rd.GetChar (rd) IN cs THEN EXIT END; INC (i, inc) END
    EXCEPT
      Rd.EndOfFile =>
    END;
    RETURN i
  END FindChar;

BEGIN
END MTextUnit.

