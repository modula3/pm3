(* Copyright (C) 1989, Digital Equipment Corporation              *)
(* All rights reserved.                                           *)
(* See the file COPYRIGHT for a full description.                 *)

(* Written in Modula-2+ by John Ellis before the dawn of history  *)
(* Converted to Modula-3 by Bill Kalsow on Oct 1989               *)
(* Last modified on Mon Dec 12 10:25:31 PST 1994 by kalsow    *)
(*      modified on Sun Feb 23 14:45:05 PST 1992 by meehan    *)
(*      modified on Thu Jul 25 19:30:30 PDT 1991 by stolfi        *)
(*      modified on Thu Jul 25 20:44:22 1991 by muller            *)
(*      modified on Mon Apr 22 17:34:37 1991 by nichols@xerox.com *)

INTERFACE XFormat;

(* This version was hacked to use Wx.T instead of Wr.T, drops
   the exceptions, and allows zero-width markup to be passed
   through the formatter.  -- Bill Kalsow 12/12/94 *)

(* 
  A Formatter.T is a basic tool for "pretty printing," the printing
  of structured objects with appropriate line breaks and indentation.

  Index: formatted streams; printing, structured data; pretty printers
  *)

IMPORT Text, Wx;

TYPE T <: T_;
  (*
    A Formatter.T is a filter which takes a sequence of /expressions/
    and converts them into a sequence of characters, which are sent to
    an underlying Wx.T.
    
    The input expressions consist of character strings intermixed with
    /formatting operators/ that specify a set of possible line breaks,
    and the relative alignment of parts in multi-line expressions.  The
    SxFormatter.T chooses a "good" subset of the given line breaks,
    inserts newlines and padding whitespaces at those points, and writes
    the result to the underlying writer. *)

PROCEDURE New (wr: Wx.T;  width: CARDINAL := 75): T;
  (* 
    Creates a new Formatter.T  whose output will go to "wr".
    The /width/ parameter specifies the nominal maximum line width. 
    *)

TYPE BreakType = {NonOptimal, OptimalBreak, OptimalNoBreak};

TYPE
  T_ = OBJECT METHODS

    underlyingWr (): Wx.T;
    (* 
      Returns the writer that is connected to the output of /t/. *)

    close ();
    (* 
      Flushes all buffered expressions to the underlying writer,    
      with proper line breaks, and releases internal resources.
      The formatter /t/ should not be used afterwards. *)

    flush ();
    (* 
      Flushes all buffered expressions to the underlying writer, with
      proper line breaks.  Automatically supplies /End/s for all unmatched
      /Group/s, /Begin/s, and /Align/s (see below).  Returns only after
      the output has made it to the writer.  *)

(**********************************************************)
(* TEXTS AND CHARACTERS                                   *)
(**********************************************************)

(*
  The following procedures insert characters and strings into the
  formatter's input stream: *)

    putText (text: Text.T;  raw:= FALSE);
    (*
      If /raw=TRUE/, sends an expression consisting of the given text,
      irrespective of its content.
    
      If /raw=FALSE/ (the default), breaks the text into substrings
      before and after each blank or newline, and sends each piece as
      a separate expression.
      *)

    putChar (c: CHAR);
    (*
      If c='\n', sends a NewLine(t) operator to the formatter; otherwise
      sends the character /c/ itself.
    
      Successive calls to PutChar whose arguments are neither ' ' nor
      '\n' are compacted into a single text string.
      *)

    putMarkup (text: Text.T;  width := 0);
    (*
      Sends an expression consisting of the given text with the
      assumption that it displays in /width/ characters of output.
      *)

(**********************************************************)
(* BREAKS                                                 *)
(**********************************************************)

(* 
  The following procedures send line breaking operators to the formatter.
  There are four kinds of line breaks:  

      Break
      BreakOpt
      PartialBreak
      NewLine
      UnitedBreak

  Precedence decreases from top to bottom.  That is, the input expresion
  stream is parsed as a sequence of "terms" separated by /UnitedBreak/s;
  each "term" is parsed as a sequence of "factors" separated by
  /NewLine/s; and so on.  These precedences can be overriden
  by enclosing expressions between /Group/ and /End/ operators (see
  below). *)
  
    break (offset := 0;  type := BreakType.OptimalBreak;  freshLine := TRUE);
    (*
      Specifies an optional line break.
    
      Breaks come in three types:

      NonOptimal:  If the expressions /e1, e2, .../ following the /Break/
      (up to the next line break or /End/) can be printed on the current
      line without exceeding the line width, they are so printed.
      Otherwise, a NewLine(t, offset, freshLine) is performed and the
      expressions are then printed.

      OptimalBreak, OptimalNoBreak: Two possibilities are compared:

        1. Printing the expressions /e1, e2, .../ without any 
           preceeding newline.

        2. Doing a NewLine(t, offset, freshLine) and then printing
           those expressions.

      The option that uses fewer lines without exceeding the line width
      is chosen.  Ties are broken in favor of either 1 (OptimalNoBreak)
      or 2 (OptimalBreak).

      In general, optimal breaks are more expensive than 
      non-optimal ones. *)

    partialBreak (offset := 0;  freshLine := TRUE);
    (*
      Performs a Newline(t, offset, freshLine) if more than one line
      has been used up to this point to print the current innermost
      object delimited by Begin/End. *)

    newLine (offset := 0;  freshLine := TRUE);
    (* 
      Starts a new line indented /offset/ spaces from the current left
      margin (see /Begin/).   If /freshline/ is true, starts a newline only if
      /leftMargin + offset < c/, where /c/ is the current column of the
      current position. *)

    unitedBreak (offset := 0;  freshLine := TRUE);
    (*
      Inserts a new member of an all-or-none group of line breaks.
    
      If all the expressions in the innermost /Group/ or /Begin/ can be
      printed so that they all fit on the current line, then they are
      so printed, and every top-level /UnitedBreak/ in that group is ignored.
      Otherwise, every top-level /UnitedBreak/ in that group is
      equivalent to a NewLine(t, offset, freshLine). *)

(**********************************************************)
(* GROUPING AND ALIGNMENT                                 *)
(**********************************************************)

    group ();
    (* 
      Logical parenthesis: the expressions between a /Group/ and the matching
      /End/ will be treated as a single expression. *)

    begin (offset := 0;  width: CARDINAL := LAST(CARDINAL));
    (* 
      The operators /Begin/ and /End/ delimit a structured object,
      consisting of the enclosed expressions and formatting operators.
    
      /Begin/ establishes a new left margin "offset" spaces to the right
      of the first character of the object.  Any line breaks occuring
      in the printing of the enclosed expressions will be relative to
      this new margin.  If "offset" = LAST( INTEGER ), then the current
      left margin is retained.

      If /width/ < LAST(INTEGER), the enclosed expressions are printed
      assuming this new nominal line width.

      The old left margin and line width are restored after the object
      has been printed.  *)

    align (
      columns:      CARDINAL;
      tryOneLine:   BOOLEAN := TRUE;
      offset:       INTEGER := 0;
      alignPred:    AlignPred := NIL;
    );
    (*
      The /Align/ operator produces a table with multiple
      columns, aligned if possible.

      The expressions between the /Align/ and the matching /End/ describe
      the rows of the table.  Each row should be either a list of
      expressions delimited by /Group/ and /End/, or a non-aligned row
      (see NoAlign).  The /offset/ parameter specifies the left-margin
      offset for each row.

      Each expression in a row will be in a separate column.  It is an
      error if a row has more than /columns/ entries, but a row may have
      fewer.

      IF /tryOneLine/ is true and the initial output position is in the
      first line of the enclosing Begin/End object, then Align will
      print all the rows on that one line (if they fit).

      Align will align the maximal set of leading rows such that each row
      fits on a single line, each row's columns satisfy the /alignPred/
      predicate, and the row is not a NoAlign row.  Then it will
      recursively align the rest of the rows without regard to the
      alignment of the leading rows.  *)

    noAlign ();
    (*
      A /NoAlign/ operator, valid only as a top-level member in an
      Align/End group, specifies that the following expression /e/ should
      not be aligned with the previous or succeeding rows, and should
      not affect their alignment.
    
      The expression /e/ is printed without any leading newline.
      Typically, /e/ will be a group or a raw text containing its own
      leading newline, but not a trailing newline.  *)

    col (column: INTEGER; relative := FALSE; space: CARDINAL := 0);
    (*
      If the current position of the formatter is less than "column"
      (zero-based), emit enough spaces to go to that column.  Otherwise, emit
      "space" spaces.  If "relative" is TRUE, interpret "column" as being
      relative to the current left margin; in this case, it is meaningful
      for "column" to be negative. *)
    
    end ();
    (*
      Delineates the end of /Group/, /Begin/, and /Align/ operators.
    
      The /Flush/ procedure automatically supplies /End/s 
      for all unmatched /Group/s, /Begin/s, and /Align/s. *)

  END; (* OBJECT T_ *)

TYPE 
  AlignPred = OBJECT METHODS 

      pred (
        column:   CARDINAL;
        maxWidth: CARDINAL;
        width:    CARDINAL
      ): BOOLEAN;

    END;
  (*
    If the /alignPred/ parameter of /Align/ is non-nil, then

        alignPred.pred(arg, column, maxWidth, width)

    is called for each column in each row, where /column/ is the 0-based
    column number, /maxWidth/ is the maximum width of that column in
    the previous rows being aligned, and /width/ is the width of column
    in the current row.
    
    If any of these calls returns FALSE, the entire row is printed
    without any column alignment, as if it were preceded by a /NoAlign/
    operator. *)

END XFormat.
